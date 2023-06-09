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

# Environment
(defn Binding [sym val]
  {:sym sym :val val})

(def empty-env @[])

(def top-env @[(Binding (idC "+") (PrimopV '+))
               (Binding (idC "-") (PrimopV '-))
               (Binding (idC "*") (PrimopV '*))
               (Binding (idC "/") (PrimopV '/))
               (Binding (idC "<=") (PrimopV '<=))
               (Binding (idC "equal?") (PrimopV 'equal?))
               (Binding (idC "true") (BoolV true))
               (Binding (idC "false") (BoolV false))])

(defn where [fun combined]
  "Convert where into AppC and LamC. fun is the body for LamC. For combined,
  all the even index combined is the args for LamC. All the
  odd index is the args for AppC."
  (def lamC-args @[])
  (def appC-args @[])
  (var n 0)
  (each i combined
    (if (= n 0)
      (do
        (set n 1)
        (array/push lamC-args i))
      (do
        (set n 0)
        (array/push appC-args i))))
  (appC (lamC lamC-args fun) appC-args))

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
        :raw-value (+ :number :string :symbol :lam :if :where :app)
        :value (* (any :ws) :raw-value (any :ws))
        :id (* (any :ws) :symbol (any :ws))
        :assign (* (any :ws) "[" :id ":=" :value (+ "]" (error "assign")) (any :ws))
        :root (any :value)
        :root2 (any :id)
        :if (/ (* "{" :value "if" :value "else" :value (+ "}" (error "if"))) ,ifC)
        :lam (/ (* "{" "{" (group (any :root2)) (+ "}" (error "lam")) (any :ws) "=>" :value (+ "}" (error "lam"))) ,lamC)
        :where (/ (* "{" :value "where" (any :ws) "{" (group (some :assign)) (+ "}" (error "lam")) (any :ws) (+ "}" (error "lam"))) ,where)
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

(defn interp [exp env]
  "Takes an ExprC and an Env to evalute it to a Value"
    (match exp
      {:n n} (NumV n) 
      {:id sym} (lookup exp env)
      {:str s} (StringV s) 
      {:args args :body body} (ClosV args body env)
      # currently not using lookup this is just straight matching
      {:fun fun :args args}
        (do 
          (def func (interp fun env))
          (match func
           {:op op} # PrimV
            (match op
              (@ '+) (NumV (+ (get (interp (args 0) env) :n) (get (interp (args 1) env) :n)))
              (@ '-) (NumV (- (get (interp (args 0) env) :n) (get (interp (args 1) env) :n)))
              (@ '*) (NumV (* (get (interp (args 0) env) :n) (get (interp (args 1) env) :n)))
              (@ '/) (NumV (/ (get (interp (args 0) env) :n) (get (interp (args 1) env) :n)))
              (@ '<=) (BoolV (<= (get (interp (args 0) env) :n) (get (interp (args 1) env) :n)))
              (@ 'equal?) (BoolV (= (get (interp (args 0) env) :n) (get (interp (get args 1) env) :n))))
          {:args cargs :body cbody :env cenv} # CloV
            (do
              (def expected-len (length cargs))
              (def actual-len (length args))
              (if (= false (= expected-len actual-len))
                (error "interp: VVQS unexpected number of arguments found"))
              (interp cbody (array/concat (map Binding cargs (map (fn [x] (interp x env)) args)) cenv)))))
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
    {:op op} "#<primop>"
    {:args args :body body :env env} "#<procedure>"))

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
(assert (= (top-interp 
            "{{{fir sec} => {{{f a} => {{fir f} {{sec f} a}}} {{x} => {+ x x}} 5}}
              {{f} => {{a} => {f a}}}
              {{g} => {{a} => {g {g a}}}}}")
           "40"))
(assert-error "interp: VVQS unexpected number of arguments found"
              (top-interp "{{{x y} => x} 1}"))
(assert (= "{{fact fact 12}
             where
             {[fact := {{self x} => {1 if {<= x 0} else {* x {self self {- x 1}}}}}]}}")
           "479001600")

# Parse tests
(assert (= ((parser "5") 0) {:n 5}))
(assert (= ((parser "\"abc\"") 0) {:str "\"abc\""}))
(assert (= ((parser "test") 0) {:id "test"}))
(assert (= ((parser "{10 if 15 else 100}") 0) {:test {:n 15} :then {:n 10} :else {:n 100}}))
(assert (deep= ((parser "{5}") 0) {:fun {:n 5} :args @[]}))
(assert (deep= ((parser "{f 1 2 3}") 0) {:fun {:id "f"} :args @[{:n 1} {:n 2} {:n 3}]}))
(assert (deep= ((parser "{{x y z} => {+ x 5}}") 0)
  {:args @[{:id "x"} {:id "y"} {:id "z"}] :body {:fun {:id "+"} :args @[{:id "x"} {:n 5}]}}))
(assert (deep= ((parser "{{+ x y} where {[x := 7] [y := 4]}}") 0)
  {:args @[{:n 7} {:n 4}] :fun {:args @[{:id "x"} {:id "y"}] :body {:args @[{:id "x"} {:id "y"}] :fun {:id "+"}}}}))