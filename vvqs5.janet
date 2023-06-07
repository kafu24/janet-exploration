# restricted ids that should never be overwritten
(def used_ids '(where := if else =>))

# # Takes an ExprC and an Env to evaluate it to a Value
# (defn interp [exp]
#     (match exp 
#         [:numC n] (printf "its a numC" n) # change later
#         [:stringC s] (printf "It's a string! %d" s) # change later
#         _ (printf "error"))
#     )
# (printf "trying to interp numExpr")
# (interp numExpr )
# (interp [:numC 4])
# (printf "interp number 3")
# (interp 3 )
# (interp [:number 3])

# (defn serialize [value]
#     (match value
#         [:NumV n] (printf "%d" n)
#         [:StringV n] (printf "%s" n)
#         [:BoolV n] # match 
#         [:PrimopV n] (printf "#<primop>")
#         [:ClosV argument body env] (printf "#<procedure>")
#         )
#     )
