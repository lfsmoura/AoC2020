(use 'clojure.java.io)
(use 'clojure.test)

(defn tap [val] 
    (do (println val) val))

(defn parse-int [x] (Integer/parseInt x))

(def example1 "1-3 a: abcde")
(def example2 "1-3 b: cdefg")
(def example3 "4-18 r: rrrdrrrrrrrrrkblrr")
(def example4 "2-9 c: ccccccccc")

(defn parse-input
    {:test #(do
        (is (= (parse-input example1) (list 1 3 "a" "abcdefeeeeeeeee")))
        (is (= (parse-input example3) (list 4 18 "r" "rrrdrrrrrrrrrkblrr")))
    )}
    [str]
    (rest (re-find #"(\d+)\-(\d+) ([a-z])\: ([a-z]*)" str))
)

(test #'parse-input)

(defn count-occurrences
    {:test #(do
        (is (= (count-occurrences \a "bbabba") 2))
    )}
    [letter input]
        (reduce
            (fn [sum x] (if (= x letter) (+ sum 1) sum))
            0
            (seq input)
        )
    )

(test #'count-occurrences)

(defn valid
    {:test #(do
        (is (= (valid (parse-input example1)) 1))
        (is (= (valid (parse-input example2)) 0))
    )}
    [password]
        (let [[min max letter input] password
              char-letter (first (seq letter))
              count (count-occurrences char-letter input)
              res (list count input)]
            (if (and (>= count (parse-int min)) (<= count (parse-int max))) 1 0)
        )
    )

(test #'valid)

(def input (with-open [rdr (reader "input.txt")]
    (map parse-input (doall (line-seq rdr)))))

(println "Solution 1:" (reduce + (map valid input)))

(defn xor
    [a b]
    (or (and a (not b))
        (and (not a) b)
    ))

(defn valid2
    {:test #(do
        (is (= (valid2 (parse-input example1)) 1))
        (is (= (valid2 (parse-input example2)) 0))
        (is (= (valid2 (parse-input example4)) 0))
    )}
        [password]
        (let [[i j letter input] password
            char-letter (first (seq letter))
            ]
            (if (xor 
                    (= char-letter (nth input (- (parse-int i) 1)))
                    (= char-letter (nth input (- (parse-int j) 1)))
                )
                1
                0
            )
        )
    )

(test #'valid2)

(println "Solution 2:" (reduce + (map valid2 input)))