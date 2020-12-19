(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (if (integer? x)
        x
        (Integer/parseInt x)))

(def example0 "2 * 3 + (4 * 5)")
(def example1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
(def example2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(def example3 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(defn break-on-matching-paren
    {:test #(do 
        (is (= ["4 * 5" ""] (break-on-matching-paren "(4 * 5)")))
        (is (= ["4 * (5 + 3)" " + 2"] (break-on-matching-paren "(4 * (5 + 3)) + 2")))
    )}
    [expr]
        (let [res (reduce 
                (fn [acc c]
                    (if (= 0 (acc :open))
                        (assoc acc :rest (str (acc :rest) c))
                        (case c
                            \(  (assoc acc :open (inc (acc :open)) :inside (str (acc :inside) c))
                            \)  (assoc acc :open (dec (acc :open)) 
                                           :inside (if (= 1 (acc :open)) (acc :inside) (str (acc :inside) c)))
                            (assoc acc :inside (str (acc :inside) c))
                        )
                    )
                )
                { :inside "" :rest "" :open 1 }
                ; ignore open paren
                (rest expr)
            )]
        [(res :inside) (res :rest)]
    )
)

(test #'break-on-matching-paren)

(defn ev
    {:test #(do
        (is (= 26 (ev example0)))
        (is (= 437 (ev example1)))
        (is (= 12240 (ev example2)))
        (is (= 13632 (ev example3)))
    )}
    [expr]
    (let [
        tokens (loop [ tokens [] input expr ]
            (if (empty? input) 
                tokens
                (case (first input)
                    \(      (let [[inside the-rest] (break-on-matching-paren input)] 
                        (recur (conj tokens (ev inside)) the-rest)
                    )
                    \space  (recur tokens (rest input))
                    (recur (conj tokens (str (first input))) (rest input))
                )
            )
        )
    ]
        (->> tokens
            (reduce
                (fn [acc token]
                    (case token
                        "*" (assoc acc :op :mul)
                        "+" (assoc acc :op :sum)
                        ; otherwise
                        (case (acc :op)
                            :mul (assoc acc :res (* (parse-int token) (acc :res)))
                            :sum (assoc acc :res (+ (parse-int token) (acc :res)))
                            ; no op yet
                            (assoc acc :res (parse-int token))
                        )
                    )
                )
                { :res 0 }
            )
            (:res)
        )
    )
)

(test #'ev)

(def input (with-open [rdr (reader "input.txt")]
   (doall (line-seq rdr))))

(println "Solution1:" (apply + (map ev input)))