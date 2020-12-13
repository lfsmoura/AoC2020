(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (Long/parseLong x))

(defn tap [lab val]
  (do (println lab val) val))

(def example ["L.LL.LL.LL"
"LLLLLLL.LL"
"L.L.L..L.."
"LLLL.LL.LL"
"L.LL.LL.LL"
"L.LLLLL.LL"
"..L.L....."
"LLLLLLLLLL"
"L.LLLLLL.L"
"L.LLLLL.LL"])

(def example2 ["L##...."
               "L.#...."
               "LLLLLL#"
               "......."
               "......."
               "......."
               "......."])

(def example3 ["#.##.##.##" "#######.##"])

(defn count-seated
    {:test #(do 
        (is (= 0 (count-seated example)))
        (is (= 4 (count-seated example2)))
    )}
    [ls]
    (apply + 
        (flatten
            (map
                (fn [line] 
                    (map
                        #(-> (if (= \# %) 1 0))
                        (seq line)
                    )
                )
                ls
            )))
)

(test #'count-seated)

(defn get-pos [array column line] (or (get (or (get array line) []) column) \.))

(defn neighbors
    {:test #(do 
        (is (= 2 (neighbors example2 2 1)))
        (is (= 1 (neighbors example2 0 0)))
        (is (= 0 (neighbors example2 5 0)))
        (is (= 2 (neighbors example3 0 0)))
    )}
    [config x y]
    (apply
        +
        (for [neighbor [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1] ]]
            (let [[i j] neighbor line (+ i y) column (+ j x)]
                (if (= \# (get-pos config column line)) 1 0)
            )
        )
    )
)

(test #'neighbors)

; Otherwise, the seat's state does not change.
(defn seats
    {:test #(do (is (= 37 (seats example))))}
    [lines]
    (loop [config lines nb-seated -1] 
        (let [seated-now (count-seated config)]
            (if (= nb-seated seated-now)
                seated-now
                ; otherwise apply rule and recur
                (recur
                    (apply vector (for [y (range 0 (count config))]
                        (apply vector (for [x (range 0 (count (first config)))]
                            (case (get-pos config x y)
                                ; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
                                \L (if (= 0 (neighbors config x y)) \# \L)
                                ; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
                                \# (if (>= (neighbors config x y) 4) \L \#)
                                \. \.
                            )
                        ))
                    ))
                    seated-now)
            )
        )
    )
)

(test #'seats)

(def input (with-open [rdr (reader "input.txt")]
   (apply vector (doall (line-seq rdr)))))

(println "Solution1:" (seats input))

(defn get-pos-no-floor [array column line] 
    (let [seattype (get (or (get array line) []) column)]
        (if (= seattype \.) nil seattype)))

(defn visible-neighbors
    {:test #(do
        (is (= 2 (visible-neighbors example2 6 0)))
    )}
    [config x y]
    (apply
        +
        (for [neighbor [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]]
            (let [[j i] neighbor]
                (if (= \# (some identity (for [radius (range 1 (count config))] 
                    (get-pos-no-floor config (+ x (* radius j)) (+ y (* radius i)))))) 1 0)
            )
        )
    )
)

(test #'visible-neighbors)

(defn seats2
    {:test #(do (is (= 26 (seats2 example))))}
    [lines]
    (loop [config lines nb-seated -1] 
        (let [seated-now (count-seated config)]
            (if (= nb-seated seated-now)
                seated-now
                ; otherwise apply rule and recur
                (recur
                    (apply vector (for [y (range 0 (count config))]
                        (apply vector (for [x (range 0 (count (first config)))]
                            (case (get-pos config x y)
                                ; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
                                \L (if (= 0 (visible-neighbors config x y)) \# \L)
                                ; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
                                \# (if (>= (visible-neighbors config x y) 5) \L \#)
                                \. \.
                            )
                        ))
                    ))
                    seated-now)
            )
        )
    )
)

(test #'seats2)

(println "Solution2:" (seats2 input))
