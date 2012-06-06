(ns solussd-utils.string
  (:require [clojure.string :as string])
  (:import [java.util.regex Pattern]))

;; sanitize
(defn ^String trim-chars
  "Removes characters in coll chars from the left and right of string"
  [chars-coll ^CharSequence string]
  (if string
    (let [trim-pattern (apply str chars-coll)]
      (second (re-find (re-pattern (format "[%s]*([^%s].*[^%s])[%s]*" trim-pattern trim-pattern trim-pattern trim-pattern)) string)))))

;; test
(defn has-suffix?
  "Checks a string for a suffix"
  [suffix s]
  (not (nil? (re-find (re-pattern (str (Pattern/quote suffix) "$")) s))))

(defn has-prefix?
  "Checks a string for a prefix"
  [prefix s]
  (not (nil? (re-find (re-pattern (str "^" (Pattern/quote prefix))) s))))







;; xml
(defn xml-escape
  "Takes a string and returns a string with all XML entities escaped"
  [^CharSequence s]
  (string/escape s {\' "&apos;", \" "&quot;", \< "&lt;", \> "&gt;", \& "&amp;"}))

(defn xml-unescape
  "Takes a string and returns a string with all XML entities decoded"
  [^CharSequence s]
  (let [m (re-matcher #"&.*?;" s)
        replace-fn {"&apos;" "'", "&quot;" "\"", "&lt;" "<", "&gt;" ">", "&amp;" "&"}
        buffer (StringBuffer. (.length s))]
    (loop []
      (if (.find m)
        (do (.appendReplacement m buffer (let [r (replace-fn (re-groups m))] (if r r "")))
            (recur))
        (do (.appendTail m buffer)
            (.toString buffer))))))
