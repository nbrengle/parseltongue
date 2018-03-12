(ns parseltongue.core)

(require '[clojure.data.csv :as csv]
         '[clojure.string :as str]
         '[clojure.java.io :as io])

;; Read in args from the commandline
(def template-file (first *command-line-args*))
(def target-file (second *command-line-args*))
(def allowed-commands #{'col 'row 'cel 'column 'cell})

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

(def comparators #{'< '= '<= '> '>= '<=> '! 'NOT 'BETWEEN})


;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Vocabulary    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; COL, ROW, CEL
;; And synonymous COLUMN and CELL

;; Examples of the macro expansion
; (col '(sum < 1) x y reader) ==> ((sum < 1) (column-from-target))
; (col '(type int) x y reader) ==> ((type int) (column-from-target))
; (col '(in ["foo" "bar"]) x y reader) ==> ((in ["foo" "bar"]) (column-from-target))

; (< 1 (sum (column-from-target)))
; (type int (column-from-target))
; (in ["foo" "bar"] (column-from-target))

; (column-from-target) is a list of strings!


; (defmacro col [expr x y reader]
;   `(let [[verb# & pred#] ~expr
;         column# (read-column ~reader ~y)]
;     (if (some (zipmap (vec ~expr) (repeat true)) comparators)
;       (eval (pred# (verb# column#)))
;       (eval (concat (mapcat identity ~expr) [(vec column#)])))))

; (defn col [expr x y reader]
;   (let [[verb & pred] expr
;         column (read-column reader y)]
;     (if (some (zipmap (vec expr) (repeat true)) comparators)
;       (eval (pred (verb column))) ; FIXME I'm not sure this works
;       (eval (concat (mapcat identity expr) [(vec column)])))))

(def aggregates #{'sum 'count 'length})

;(agg comp args) ==> (comp (agg col) args)
;(comp args) ==> (comp col args)
(defn col [expr x y reader]
  (let [column (read-column reader y)]
    (if (some (zipmap (vec expr) (repeat true)) aggregates)
      (eval (concat (rest expr) ((first expr) column)))
      (eval (concat (rest expr) [column])))))

;(agg comp args) ==> (comp (agg col) args)
;(comp args) ==> (comp col args)
(defn col [expr x y reader]
  (let [column (read-column reader y)]
    (if (some (zipmap (vec expr) (repeat true)) aggregates)
      (prn (list (second expr) ((first expr) column) (nth expr 2)))
      (prn (list (first expr) column (nth expr 1 nil))))))

; (defn col [expr x y reader]
;   (let [agg? #(if (some (zipmap (vec expr) (repeat true)) aggregates)
;                 ((first expr) %)
;                 %)
;         f     (rest expr)]
;   (prn f)
;   (as-> (read-column reader y) column
;       (agg? column)
;       (eval (con(f )))))

(defn cel [expr x y reader]
  (let [val (nth (read-column reader y) x)]
    (eval (concat expr (vec val)))))

(defn row [expr x y reader]
  (let [[verb & pred] expr
        row (nth (csv/read-csv reader) x)]
    (if (some (zipmap (vec expr) (repeat true)) comparators)
      (eval (pred (verb row))) ; FIXME I'm not sure this works
      (eval (concat (mapcat identity expr) [(vec row)])))))

(defmacro cell [expr x y reader]
  (eval (concat (list 'cel expr) [x y reader])))

; bouns points when I get it to work later
; (defmacro column [expr x y reader]
;   (eval (concat (list 'col expr) [x y reader])))

;; LEFT RIGHT UP DOWN (to fetch the value of a neighbor)
; this one is real hard right now, bonus to get it no stress if no
; consider left-col, right-col, row-above, row-below

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
    (clojure.core/< (read-string b) a)))

(defn > [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/> a))
    (clojure.core/> (read-string b) a)))

(defn <= [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/<= a))
    (clojure.core/<= (read-string b) a)))

(defn >= [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/>= a))
    (clojure.core/>= (read-string b) a)))

(defn = [a b]
  (if (coll? b)
    (-> (map read-string b)
        (map clojure.core/= a))
    (clojure.core/= (read-string b) a)))

;; TYPE (ie. is it an Integer? a float?)

;; IN (ie. is the value in a provided  set of values)

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
;; consider swapping out this concat stuff for apply
(defn eval-parseltongue
  "evaluate an expression if it's in the vocab
  optional arg is a grid coordinate for tabular data (csv)
  if the coordinate is provided, the expression is unread
  and compared to the value found at that coordinate in the target file"
  [expr x y reader]
  (let [data (csv/read-csv reader)]  
  (if (contains? allowed-commands (first expr))
    (eval (concat 'expr [x y reader])) ; FIXME this use of concat comes from unidiomatic Clojure
    (eval (concat `(cel (= ~(parseltongue->string expr))) [x y reader]))))) ; FIXME how do we expose errors? Print may be sufficient

;; from https://gist.github.com/AlexBaranosky/4134522
(defmacro doseq-indexed [index-sym [item-sym coll] & body]
  `(doseq [[~index-sym ~item-sym] (map list (range) ~coll)]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Loop          ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Loop through the TEMPLATE to exercise its rules on the TARGET
; FIXME main-loop is a rubbish function name
(defn main-loop!
  "read in the template file, parse and eval each item"
  [template-file target-file]
  (with-open [template-reader (io/reader template-file)
              target-reader (io/reader target-file)]
    (let [template-data (csv/read-csv template-reader)]
    (doseq-indexed curr-row [r template-data]
           (doseq-indexed curr-col [c r]
              (eval-parseltongue (string->parseltongue c) curr-row curr-col target-reader))))))

;; from: https://stackoverflow.com/questions/8087115/clojure-index-of-a-value-in-a-list-or-other-collection
;; not using them now, but I don't want to lose them
; (defn indexes-of [e coll] (keep-indexed #(if (= e %2) %1) coll)) 
; (first (indexes-of "a" (list "a" "a" "b"))) ;; => 0