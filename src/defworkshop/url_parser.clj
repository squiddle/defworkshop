(ns defworkshop.url-parser
  (:require [clojure.string :as string]
            [workshoplib.tdd :refer […]]))

;; Ok, enough basics for us! Let's move to something more interesting now.
;; Let's implement a URL parser that will call certain functions depending on what's coming into it.
;;
;; Of course, it won't be a full-blown url parser, since there're way too many options to cover in such
;; a short time, but be able to match url, extract params from them and run custom handlers based on which
;; url was called.
;;
;; We'll have an interface definition for you. You can check tests to see which arguments your functions
;; will be called with.

(defn get-parts
  "Gets parts of the URL, for example, for `\"/a/b/c\"`  it would return

     [\"a\", \"b\", \"c\"].

   You can use `clojure.string/split` for splitting the sequence.
   To create a regular expression from string, you can use `#\"\"` shortcut."
  [s]
  (clojure.string/split s #"/"))

(defn named-argument?
  "Checks wether the given route part is a named argument.

   Basically, checks wether the string starts with `:`."
  [part]
  (= \: (first part)))

(defn extract-named-argument
  "Converts url part (string) into key for the argument hash (keyword), stripping leading `:`.

     * You can use `subs` to get a part of string.
     * You can use `keyword` to create a keyword."
  [part]
  (keyword (subs part 1)))

(defn uri-matches?
  "Checks wether URI matches given pattern.

   Given you have a url `'/people/123'` and pattern `'/people/:id'`, their parts would look like:

      [\"people\", \"123\"]

   and

      [\"people\", \":id\"]

   Correspondingly.

   When the problem looks like one you would use `reduce` for, but you have more than 1 sequence,
   you can use 3 approaches:

   * stack-generating recursion
   * loop/recur
   * partition/interleave."
  [uri pattern]
  (let [uri-parts   (get-parts uri)
        pattern-parts (get-parts pattern)]
    (and (= (count uri-parts)
            (count pattern-parts))
         (loop [[curr-uri-part & rest-uri] uri-parts
                [curr-pat-part & rest-pat] pattern-parts
                result true]
            (if (nil? curr-uri-part) 
              result
              (recur 
                rest-uri 
                rest-pat 
                (and 
                  result 
                  (or 
                    (= curr-uri-part curr-pat-part) 
                    (named-argument? curr-pat-part)))))))))

(defn extract-arguments
  "Extracts named arguments from the string, based on a pattern."
  [uri pattern]
  (let [uri-parts   (get-parts uri)
        pattern-parts (get-parts pattern)]
    (and (= (count uri-parts)
            (count pattern-parts))
         (let [uri-pat (interleave uri-parts pattern-parts)
               uri-pat-pairs (partition 2 uri-pat)]
          (reduce 
            (fn [result [uri pat]] 
              (if (named-argument? pat)
                (assoc result (extract-named-argument pat) uri)
                result))
            {}
            uri-pat-pairs)))))

(defn dispatch
  "Dispatches the route based on route map"
  [uri route-map]
  (if-let [matches (seq (filter (fn [[pat handler]] (uri-matches? uri pat)) route-map))]
    (let [first-match (first matches)
          pat (nth first-match 0)
          handler (nth first-match 1)
          arguments (extract-arguments uri pat)]
      (handler arguments))))
