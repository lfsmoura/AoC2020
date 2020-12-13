(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn tap [val]
  (do (println val) val))

(def input (with-open [rdr (reader "input.txt")]
    (doall (line-seq rdr))))

; forms is a list of strings from blank-line-separated list
; the order of elements is reverse but that doesn't matter for this problem
; '("abc" "" "a" "b" "" "b")
; ->
; '("b" "ab" "abc")
(def forms (reduce 
    (fn [result line] (if (= line "")
        (cons "" result)
        (cons (apply str (first result) line) (rest result))
    ))
    '("")
    input
))

(def count-answers
    (reduce
        +
        (map
            count
            (map
                set
                forms
            )
        )
    )
)

(println "Solution1:" count-answers)

; for part two we need to read the input in a different way
; '("abc" "" "a" "b" "" "b")
; ->
; '('("b") '("a" "b") '("abc"))
(def forms2 (reduce 
    (fn [result line] (if (= line "")
        (cons '() result)
        (cons (cons line (first result)) (rest result))
    ))
    '()
    input
))

(defn everyone-count
    {:test #(do
        (is (= 3 (everyone-count '("abc"))))
        (is (= 0 (everyone-count '("a" "b" "c"))))
        (is (= 1 (everyone-count '("ab" "ac"))))
        (is (= 1 (everyone-count '("a" "a" "a" "a"))))
        (is (= 1 (everyone-count '("b"))))
    )}
    [ls]
    (count 
        (reduce
            set/intersection
            (map
                set
                ls
            )
        )
    )

)

(test #'everyone-count)

(println "Solution2" (reduce + (map everyone-count forms2)))