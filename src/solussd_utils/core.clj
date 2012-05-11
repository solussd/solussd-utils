(ns solussd-utils.core)

(defmacro sub-if
  "If (pred expr) returns true, returns subexpr, otherwise expr
Optionally, you can provide an anaphor to access the result of expr in subexpr"
  ([expr pred subexpr anaphor]
     `(let [~anaphor ~expr]
        (if (~pred ~anaphor)
          ~subexpr
          ~anaphor)))
  ([expr pred subexpr]
     (let [sym (gensym)]
       `(sub-if ~expr ~pred ~subexpr ~sym))))
