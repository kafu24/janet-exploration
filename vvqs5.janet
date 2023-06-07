(import ./helper :prefix "" :exit false)

# ExprC
(defn numC [n]
  @{:n n})

(defn stringC [s]
  @{:s s})

(defn idC [sym]
  @{:s sym})

(defn appC [fun &opt args]
  (default args {})
  {:fun fun :args args})

(defn LamC [args body]
  @{:args args :body body})

(defn ifC [then test else]
  {:then then :test test :else else})

(defn ifC [test then else]
  @{:test test :then then :else else})

# Values
(defn NumV [n]
  @{:n n})

(defn StringV [s]
  @{:s s})

(defn BoolV [bool]
  @{:bool bool})

(defn ClosV [args body env]
  @{:args args :body body :env env})

(defn PrimopV [op]
  @{:op op})

(defn ErrorV [any]
  @{:any any})

# Takes an ExprC and an Env to evaluate it to a Value
(defn interp [exp]
    (match exp 
        {:n n} (NumV (get exp :n)) # change later
        {:stringC s} (printf "It's a string! %d" s) # change later
        _ (printf "error cant interp"))
    )

# NOT DONE have to use getters for table 
(defn serialize [value]
    (match value
        {:n n}  (printf "%d" n)
        {:s s} (printf "%s" s)
        {:BoolV n} (printf "idk for now")
        {:op op} (printf "#<primop>") # not done
        {:args args :body body :env env} (printf "#<procedure>") # not done
        )
    )

(defn top-interp [exp]
    (serialize (interp exp)))

# "TEST CASES" print statements
(def test-num-c (numC 5))

(top-interp test-num-c)



(defn parser [text]
  "Peg for compiling VVQS5 into a Janet source ast"
  (peg/match
    (peg/compile
      ~{:ws (set " \t\r\f\n\0\v")
        :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
        :token (some :symchars)
        :symbol (/ ':token ,idC)
        :hex (range "09" "af" "AF")
        :escape (* "\\" (+ (set "ntrvzf0e\"\\")
                        (* "x" :hex :hex)
                        (error (constant "bad hex escape"))))
        :number (/ (number (some (+ :d (set ".-+")))) ,numC)
        :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
        :string (/ ':bytes ,stringC)
        :raw-value (+ :number :string :symbol :if :app)
        :value (* (any :ws) :raw-value (any :ws))
        :root (any :value)
        :if (/ (* "{" :value "if" :value "else" :value (+ "}" (error ""))) ,ifC)
        :app (/ (* "{" :value (group (any :root)) (+ "}" (error ""))) ,appC)
        :main :root})
    text))

(assert (= ((parser "5") 0) {:n 5}))
(assert (= ((parser "\"abc\"") 0) {:s "\"abc\""}))
(assert (= ((parser "test") 0) {:id "test"}))
(assert (= ((parser "{10 if 15 else 100}") 0) {:test {:n 15} :then {:n 10} :else {:n 100}}))
(assert (deep= ((parser "{5}") 0) {:fun {:n 5} :args @[]}))
(assert (deep= ((parser "{f 1 2 3}") 0) {:fun {:id "f"} :args @[{:n 1} {:n 2} {:n 3}]}))