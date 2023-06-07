(import ./helper :prefix "" :exit true)
(start-suite 0)

# ExprC
(defn numC [n]
  {:n n})

(defn stringC [s]
  {:s s})

(defn idC [sym]
  {:s sym})

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
        :hex (range "09" "af" "AF")
        :escape (* "\\" (+ (set "ntrzfev0\"\\")
                       (* "x" :hex :hex)
                       (* "u" [4 :hex])
                       (* "U" [6 :hex])
                       (error (constant "bad escape"))))
        :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
        #:string (/ :bytes ,string)
        :string (/ (* `"` (<- (any (if-not `"` 1))) `"`) ,stringC)
        :number (/ (number (some (+ :d (set ".-+")))) ,numC)
        :raw-value (+ :number :string :app)
        :value (* (any :ws) :raw-value (any :ws))
        :root (any :value)
        :app (* "{" :root (+ "}" (error "")))
        :main :root})
    str))

(assert (parser "\"abc\"") @[{:s "abc"}])

(end-suite)