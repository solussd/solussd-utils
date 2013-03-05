(ns solussd-utils.string
  (:require [clojure.string :as string])
  (:import [java.util.regex Pattern]))

;; regex
(defn regex-escape
  "Takes a string and escapes all PCRE regular expression reserve characters
 which are:"
  [s]
  (let [escape-map {\. "\\."
                    \^ "\\^"
                    \$ "\\$"
                    \* "\\*"
                    \+ "\\+"
                    \? "\\?"
                    \( "\\("
                    \) "\\)"
                    \[ "\\["
                    \] "\\]"
                    \- "\\-"
                    \\ "\\\\"
                    \{ "\\{"
                    \} "\\}"
                    \  "\\ "}]
    (string/escape s escape-map)))

;; sanitize
(defn ^String trim-chars
  "Removes characters in coll chars from the left and right of string"
  [chars-coll ^CharSequence string]
  (if string
    (let [trim-pattern (apply str chars-coll)]
      (second (re-find (re-pattern (format "[%s]*([^%s].*[^%s])[%s]*" trim-pattern trim-pattern trim-pattern trim-pattern)) string)))))

(defn ^String trim-chars-right
  "Removes characters in coll chars from the right of string"
  [chars-coll ^CharSequence string]
  (if string
    (let [trim-pattern (apply str chars-coll)]
      (second (re-find (re-pattern (format "^(.*[^%s])[%s]*" trim-pattern trim-pattern)) string)))))

(defn ^String trim-chars-left
  "Removes characters in coll chars from the left of string"
  [chars-coll ^CharSequence string]
  (if string
    (let [trim-pattern (apply str chars-coll)]
      (second (re-find (re-pattern (format "[%s]*([^%s].*)$" trim-pattern trim-pattern trim-pattern trim-pattern)) string)))))

;; test
(defn has-suffix?
  "Checks a string for a suffix"
  [suffix s]
  (not (nil? (re-find (re-pattern (str (Pattern/quote suffix) "$")) s))))

(defn has-prefix?
  "Checks a string for a prefix"
  [prefix s]
  (not (nil? (re-find (re-pattern (str "^" (Pattern/quote prefix))) s))))


;; format
(defn- replace-by
  [^CharSequence s re f]
  (let [m (re-matcher re s)]
    (let [buffer (StringBuffer. (.length s))]
      (loop []
        (if (.find m)
          (do (.appendReplacement m buffer (f (re-groups m)))
              (recur))
          (do (.appendTail m buffer)
              (.toString buffer)))))))

(defn to-camel
  "Converts a string separated by any of separators to ACamelCaseString.
If no separator characters are provided, defaults to - and _."
  ([s separators & {:keys [titleize] :or {titleize true}}]
     (replace-by (cond-> s titleize string/capitalize)
                 (re-pattern (format "[%s]+(.)" (regex-escape (apply str separators))))
                 (comp string/upper-case second)))
  ([s]
     (to-camel s "-_")))

(defn dashes-to-camel
  "Converts a dash-separated-string to a camelCaseString"
  [s]
  (to-camel s "-"))

(defn camel-to-dash
  "Converts a camelCaseString to a dash-separated-string"
  [s & {:keys [lower-case] :or {lower-case true}}]
  (cond-> (replace-by s #"(.)([A-Z])" (fn [m] (str (second m) \- (string/lower-case (nth m 2)))))
          lower-case string/lower-case))

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
