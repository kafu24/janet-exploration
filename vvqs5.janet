(import ./helper :prefix "" :exit true)
 
# ExprC
(defn numC [n]
  {:n n})
 
(defn stringC [s]
  {:str s})
 
(defn idC [sym]
  {:id sym})
 
(defn appC [fun &opt args]
  (default args @[])
  {:fun fun :args args})
 
(defn lamC [args body]
  {:args args :body body})
 
(defn ifC [then test else]
  {:then then :test test :else else})
 
# Values
(defn NumV [n]
  {:n n})
 
(defn StringV [s]
  {:s s})
 
(defn BoolV [bool]
  {:bool bool})
 
(defn ClosV [args body env]
  {:args args :body body :env env})
 
(defn PrimopV [op]
  {:op op})
 
(defn ErrorV [any]
  {:any any})

# environment
(defn Binding [sym val]
  {:sym sym :val val})

(def empty-env {})

(def top-env @[(Binding "+" (PrimopV '+))
               (Binding "-" (PrimopV '-))
               (Binding "*" (PrimopV '*))
               (Binding "/" (PrimopV '/))
               (Binding "<=" (PrimopV '<=))
               (Binding "equal?" (PrimopV 'equal?))])

(defn parser [text]
  "Not really a parser, can just recognise some VVQS5 source code."
  (peg/match
    (peg/compile
      ~{:ws (set " \t\r\f\n\0\v")
        :wheresym "where"
        :equalsym "="
        :ifsym "if"
        :elsesym "else"
        :assignsym "=>"
        :forbidden (+ :wheresym :equalsym :ifsym :elsesym :assignsym)
        :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
        :token (some :symchars)
        :symbol (/ (* (not :forbidden) ':token) ,idC)
        :hex (range "09" "af" "AF")
        :escape (* "\\" (+ (set "ntrvzf0e\"\\")
                        (* "x" :hex :hex)
                        (error (constant "bad hex escape"))))
        :number (/ (number (some (+ :d (set ".-+")))) ,numC)
        :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
        :string (/ ':bytes ,stringC)
        :raw-value (+ :number :string :symbol :lam :if :app)
        :value (* (any :ws) :raw-value (any :ws))
        :id (* (any :ws) :symbol (any :ws))
        :root (any :value)
        :root2 (any :id)
        :if (/ (* "{" :value "if" :value "else" :value (+ "}" (error "if"))) ,ifC)
        :lam (/ (* "{" "{" (group (any :root2)) (+ "}" (error "lam")) (any :ws) "=>" :value (+ "}" (error "lam"))) ,lamC)
        #:where (/ (* "{" :value "where" "{" (group (any "[" ""]"}"))
        :app (/ (* "{" :value (group (any :root)) (+ "}" (error "app"))) ,appC)
        :main :root})
    text))

(defn lookup [id env]
  "Lookup value for given id"
  (var result nil)
  (each binding env
    (match binding
      {:sym sym :val val} (if (= sym id) (set result val))))
  (cond result
    nil (error "symbol not found"))
  result)

(defn binder [args funs]
  "Used in ClosV interp match case. Return an array with bindings
   from args to funs sequentially."
  )

(defn interp [exp env]
  "Takes an ExprC and an Env to evalute it to a Value"
    (match exp
      {:n n} (NumV n) 
      {:id sym} (lookup sym env)
      {:str s} (StringV s) 
      {:args args :body body} (ClosV args body env)
      # currently not using lookup this is just straight matching
      {:fun fun :args args}
        (do 
          (def func (interp fun env))
          (match func
           {:op op} # PrimV
            (match op
              (@ '+) (NumV (+ (get (get args 0) :n) (get (get args 1) :n)))
              (@ '-) (NumV (- (get (get args 0) :n) (get (get args 1) :n)))
              (@ '*) (NumV (* (get (get args 0) :n) (get (get args 1) :n)))
              (@ '/) (NumV (/ (get (get args 0) :n) (get (get args 1) :n)))
              (@ '<=) (BoolV (<= (get (get args 0) :n) (get (get args 1) :n)))
              (@ 'equal?) (BoolV (= (get (get args 0) :n) (get (get args 1) :n))))
          #{:args a :body b :env e} # CloV
          #  (do)
           ))
      {:then then :test test :else else}
        (do
          (def condition (interp test env))
          (match condition
            {:bool bool} (if (= bool true) (interp then env) (interp else env))
            _ (error "interp: VVQS IfC failed")))
      _ (error "interp: VVQS unknown input")))
 
(defn serialize [value]
  "takes a VVQS5 value and returns it as a string"
  (match value
    {:n n}  (string n)
    {:s s} (string s)
    {:bool bool} (string bool)
    {:op op} (string "#<primop>") # not done
    {:args args :body body :env env} (string "#<procedure>") # not done
 
    )
  )

(defn top-interp [text]
  (serialize (interp ((parser text) 0) top-env)))

# Top-interp tests
(assert (= (top-interp "{+ 6 12}") "18"))
(assert (= (top-interp "{- 6 12}") "-6"))
(assert (= (top-interp "{/ 12 12}") "1"))
(assert (= (top-interp "{* 6 12}") "72"))
(assert (= (top-interp "{<= 2 12}") "true"))
(assert (= (top-interp "{<= 22 12}") "false"))
(assert (= (top-interp "{equal? 12 12}") "true"))

# Parse tests
(assert (= ((parser "5") 0) {:n 5}))
(assert (= ((parser "\"abc\"") 0) {:str "\"abc\""}))
(assert (= ((parser "test") 0) {:id "test"}))
(assert (= ((parser "{10 if 15 else 100}") 0) {:test {:n 15} :then {:n 10} :else {:n 100}}))
(assert (deep= ((parser "{5}") 0) {:fun {:n 5} :args @[]}))
(assert (deep= ((parser "{f 1 2 3}") 0) {:fun {:id "f"} :args @[{:n 1} {:n 2} {:n 3}]}))
(assert (deep= ((parser "{{x y z} => {+ x 5}}") 0)
  {:args @[{:id "x"} {:id "y"} {:id "z"}] :body {:fun {:id "+"} :args @[{:id "x"} {:n 5}]}}))