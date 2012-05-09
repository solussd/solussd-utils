(ns solussd-utils.core)

(defmacro sub-if
  "If (pred expr) returns true, returns subexpr, otherwise expr"
  [expr pred subexpr]
  `(let [expr# ~expr]
     (if (~pred expr#)
       ~subexpr
       expr#)))
