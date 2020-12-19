(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn tap [val]
  (do (println val) val))

(defn parse-int [x]
    (if (integer? x)
        x
        (Integer/parseInt x)))

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

; Run in two steps first step runs all sums and the second step just multiply
(defn ev
    {:test #(do
        (is (= 46 (ev "2 * 3 + (4 * 5)")))
        (is (= 23340 (ev "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))
        (is (= 51 (ev "1 + (2 * 3) + (4 * (5 + 6))")))
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
        ; if there is a sum to be done do it otherwise just keep the tokens as they are
        sum-step (reduce
                (fn [acc token]
                    (case token
                        "*" (assoc acc :tokens (into (acc :tokens) ["*"]))
                        "+" (assoc acc :op :sum)
                        ; otherwise
                        (case (acc :op)
                            :sum (assoc acc :op nil 
                                            :tokens (into (pop (acc :tokens))
                                                          [(+ (parse-int token) (parse-int (last (acc :tokens))))]))
                            ; no op yet
                            (assoc acc :tokens (into (acc :tokens) [token]))
                        )
                    )
                )
                { :tokens [] }
                tokens
            )
    ]
        (->> (sum-step :tokens)
            ; this could be more concise but I just quickly modified solution for part 1 
            (reduce
                (fn [acc token]
                    (case token
                        "*" (assoc acc :op :mul)
                        ; otherwise
                        (case (acc :op)
                            :mul (assoc acc :res (* (parse-int token) (acc :res)))
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

(println "Solution2:" (apply + (map ev input)))