(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn tap [val]
  (do (println val) val))

(def ex0 "FBFBBFFRLR")
(def ex1 "BFFFBBFRRR")
(def ex2 "FFFBBBFRRR")
(def ex3 "BBFFBBFRLL")

(defn seat-id 
    {:test #(do 
        (is (= 357 (seat-id [44 5])))
    )}
    [seat]
    (let [[row col] seat]
        (+ (* row 8) col)
    )
)

(test #'seat-id)

(defn get-id
    {:test
        #(do
            (is (= 357 (get-id ex0)))
            (is (= 567 (get-id ex1)))
            (is (= 119 (get-id ex2)))
            (is (= 820 (get-id ex3)))
        )
    }
    [coords]
        (let [
            bynary-partitioning (fn [acc dir]
                (let [[row col] acc]
                    (case dir
                        \F (let [[i e] row diff (quot (- e i) 2)] [[i (+ i diff)] col])
                        \B (let [[i e] row diff (quot (- e i) 2)] [[(+ i diff 1) e] col])
                        \L (let [[i e] col diff (quot (- e i) 2)] [row [i (+ i diff)]])
                        \R (let [[i e] col diff (quot (- e i) 2)] [row [(+ i diff 1) e]])
                    )
                )
            )
            intervals (reduce
                bynary-partitioning
                ; initial intervals
                [[0 127] [0 7]]
                coords
            )
            seat-pos (map first intervals)
        ]
            (seat-id seat-pos)
        )
    )

(test #'get-id)

(def input (with-open [rdr (reader "input.txt")]
    (doall (line-seq rdr))))

(def ids (map get-id input))

(println "Solution 1:" (reduce max ids))

(defn missing-nb
    {:test #(do
        (is (= 3 (missing-nb '(1 2 4 5))))
    )}
    [ids]
    (let [
        reducer (fn [acc current]
                    (let [[last solution] acc]
                        (if (= current (+ last 2))
                            [-1 (- current 1)]
                            [current solution]
                        )
                    ))
        sorted-ids (sort ids)]
        (second (reduce reducer [-1 nil] sorted-ids))
    )
)

(test #'missing-nb)

(println "Solution 2:" (missing-nb ids))