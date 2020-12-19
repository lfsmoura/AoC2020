(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn tap [val]
  (do (println val) val))

(def example [".#." "..#" "###"])

(defn parse-lines
    [lines]
    (apply set/union
        (->> lines
            (zipmap (range))
            (map (fn [[i line]]
                (->> line
                    (zipmap (range))
                    (seq)
                    (filter (fn [[j c]] (= c \#)))
                    (map (fn [[j c]] #{ [i j 0 0] }))
                )
            ))
            (flatten)
        )
    )
)

(defn neighborhood
    {:test #(do
        (is (= 81 (count (neighborhood [0 0 0 0]))))
    )}
    [[x0 y0 z0 w0]]
        (for [x (range -1 2) y (range -1 2) z (range -1 2) w (range -1 2)] [(+ x0 x) (+ y0 y) (+ z0 z) (+ w0 w)])
)

(test #'neighborhood)

(defn neighbors
    {:test #(do
        (is (= 80 (count (neighbors [0 0 0 0]))))
    )}
    [coords]
        (filterv
            (partial not= coords) 
            (neighborhood coords))
)

(test #'neighbors)

(defn conway
    {:test #(do
        (is (= 848 (conway example 6)))
    )}
    [lines cycles]
    (count
    (loop [cycles-left cycles cells (parse-lines lines)]
        (if (= cycles-left 0)
            cells
            (let [next-cells (reduce
                (fn [acc cell]
                    (let [alive-neigbors (count (filter (partial contains? cells) (neighbors cell)))]
                        (if (contains? cells cell)
                        ; active
                            (if (contains? #{2 3} alive-neigbors)
                                (conj acc cell)
                                acc
                                ;(assoc acc :dead (conj (:dead acc) cell))
                            )
                        ; inactive
                            (if (= 3 alive-neigbors)
                                (conj acc cell)
                                acc
                                ;(assoc acc :dead (conj (:dead acc) cell))
                            )
                        )
                    )
                )
                #{}
                ; get all posible neighbors
                (reduce
                    set/union
                    (map
                        (comp set neighborhood)
                        cells
                    )
                )
            )]
            (recur (dec cycles-left) next-cells)
            )
        )
    )
    )
)

(test #'conway)

(def input (with-open [rdr (reader "input.txt")]
   (doall (line-seq rdr))))

(println "Solution1:" (conway input 6))