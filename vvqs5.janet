(import ./helper :prefix "" :exit false)

# ExprC
(defn numC [n]
  {:n n})

(defn stringC [s]
  {:s s})

(defn idC [sym]
  {:id sym})

(defn appC [fun args]
  {:fun fun :args args})

(defn LamC [args body]
  {:args args :body body})

(defn ifC [test then else]
  {:test test :then then :else else})

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

# Takes an ExprC and an Env to evaluate it to a Value
(defn interp [exp]
    (match exp 
        {:n n} (printf "its a numC" n) # change later
        {:stringC s} (printf "It's a string! %d" s) # change later
        _ (printf "error cant interp"))
    )

# NOT DONE have to use getters for table 
(defn serialize [value]
    (match value
        {:n n} (printf "%d" n)
        {:s s} (printf "%s" s)
        {:BoolV n} (printf "test") # not done 
        {:PrimopV n} (printf "#<primop>") # not done
        {:args args :body body :env env} (printf "#<procedure>") # not done
        )
    )

# "TEST CASES" print statements
(def test-num-c (numC 5))
(interp test-num-c)
(printf "testing serialize ill make top later")
(serialize (interp test-num-c))

(defn parser [str]
  (peg/match
    (peg/compile
      ~{:ws (set " \t\r\f\n\0\v")
        :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
        :token (some :symchars)
        :symbol (/ ':token ,idC)
        :string (/ (* `"` (<- (any (if-not `"` 1))) `"`) ,stringC)
        :number (/ (number (some (+ :d (set ".-+")))) ,numC)
        :raw-value (+ :number :string :symbol :app)
        :value (* (any :ws) :raw-value (any :ws))
        :root (any :value)
        :app (* "{" :root (+ "}" (error "")))
        :main :root})
    str))

(assert (= (get (parser "5") 0) {:n 5}))
(assert (= (get (parser "\"abc\"") 0) {:s "abc"}))
(assert (= (get (parser "test") 0) {:id "test"}))
