(ns parseltongue.core)

(require '[clojure.data.csv :as csv]
         '[clojure.string :as str]
         '[clojure.java.io :as io])

;; Read in args from the commandline
(def template-file (first *command-line-args*))
(def target-file (second *command-line-args*))
(def allowed-commands #{'col 'row 'cel 'column 'cell})
(def comparators #{'< '= '<= '> '>= '<=> '! 'NOT 'BETWEEN})
(def aggregates #{'sum 'count 'length})

;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers            ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn enparen
  "wrap a string in parentheses
  presumably to turn it into code"
  [string]
  (str "(" string ")"))

(defn enquote
  "wrap a symbol in quote"
  [symbol]
  (list 'quote symbol))

;; from clojure.data.csv documentation
(defn read-column [reader column-index]
  (let [data (csv/read-csv reader)]
    (map #(nth % column-index) data)))

;; from https://gist.github.com/AlexBaranosky/4134522
(defmacro doseq-indexed [index-sym [item-sym coll] & body]
  `(doseq [[~index-sym ~item-sym] (map list (range) ~coll)]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Verb Vocabulary    ;;
;;;;;;;;;;;;;;;;;;;;;;;;
 
;; SUM (total value of all numbers in the ROW/COL)
(defn sum [coll]
  (reduce + (map read-string coll)))

;; LENGTH (to get the count of cells with values in them)
;; using COUNT directly should achieve the same result
(defn length [coll]
  (count coll))

;; <, =, <=, >, >=, <=>, !, NOT, BETWEEN for value comparison
;; here there be dangerous overrides
;; this seems like a great candidate for a macro...

(defn < [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/< a))
    (clojure.core/< b a)))

(defn > [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/> a))
    (clojure.core/> b a)))

(defn <= [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/<= a))
    (clojure.core/<= b a)))

(defn >= [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/>= a))
    (clojure.core/>= b a)))

(defn = [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/= a))
    (clojure.core/= b a)))

;; TYPE (ie. is it an Integer? a float?)

;; IN (ie. is the value in a provided  set of values)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Vocabulary    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; COL, ROW, CEL
;; And synonymous COLUMN and CELL

(defn col [expr x y reader]
  (let [column (read-column reader y)]
    (if (some (zipmap (vec expr) (repeat true)) aggregates)
      (let [agg (eval (list (first expr) (vec column)))]
        (concat (rest expr) [agg]))
      (concat expr (vec [column])))))

(defn row [expr x y reader]
  (let [row (nth (csv/read-csv reader) x)]
    (if (some (zipmap (vec expr) (repeat true)) aggregates)
      (let [agg (eval (list (first expr) (vec row)))]
        (concat (rest expr) [agg]))
      (concat expr (vec [row])))))

(defn cel [expr x y reader]
  (let [val (nth (read-column reader y) x)]
    (concat expr (vec val))))

(defmacro cell [expr x y reader]
  `(concat (list ~cel '~expr) [~x ~y ~reader]))

;  I get it to work later
; (defmacro column [expr x y reader]
;   (eval (concat (list 'col expr) [x y reader])))

;; LEFT RIGHT UP DOWN (to fetch the value of a neighbor)
; this one is real hard right now, bonus to get it no stress if no
; consider left-col, right-col, row-above, row-below


;;;;;;;;;;;;;;;;;;;;;;;;
;; Translators        ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn string->parseltongue
  "digest a string into parseltongue"
  [string]
  (let [cmd (read-string (enparen string))]
    (list (first cmd) (enquote (rest cmd)))))

(defn parseltongue->string
  "transform a parseltongue expression to a string"
  [expr]
  (-> expr
      (str/replace #"[\(\)]" "")
      (str/replace "quote " "") ; space after quote is deliberate
  ))

;; '(col '(sum < 1)) x y reader
(defn eval-parseltongue
  "evaluate an expression if it's in the vocab
  optional arg is a grid coordinate for tabular data (csv)
  if the coordinate is provided, the expression is unread
  and compared to the value found at that coordinate in the target file"
  [expr x y reader]
  (let [data (csv/read-csv reader)]  
  (if (contains? allowed-commands (first expr))
    (concat 'expr [x y reader]) 
    (concat `(cel (= ~(parseltongue->string expr))) [x y reader]))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Loop          ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Loop through the TEMPLATE to exercise its rules on the TARGET
; FIXME main-loop is not a great function name
(defn main-loop!
  "read in the template file, parse and eval each item"
  [template-file target-file]
  (with-open [template-reader (io/reader template-file)
              target-reader (io/reader target-file)]
    (let [template-data (csv/read-csv template-reader)]
    (doseq-indexed curr-row [r template-data]
           (doseq-indexed curr-col [c r]
              (eval-parseltongue (string->parseltongue c) curr-row curr-col target-reader))))))