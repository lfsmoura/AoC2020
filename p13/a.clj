(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x] (try 
    (Long/parseLong x)
    (catch Exception e nil)
))

(def input (with-open [rdr (reader "input.txt")]
   (doall (line-seq rdr))))

(def timestamp (parse-int (first input)))

(def ids-str (string/split (second input) #","))

(def ids (->> ids-str
            (map parse-int)
            ; remove nil's
            (filter identity)))

(defn bus-time
    {:test #(do
        (is (= [59 5] (bus-time 939 [7 13 59 31 19])))
    )}
    [time ls]
    (let [
            next-bus (fn [id] [id (* (+ 1 (quot time id)) id)])
            [best-id best-time] (apply min-key second (map next-bus ls))
        ]
       [best-id (- best-time time)]
    )
)

(test #'bus-time)

(println "Solution1:" (apply * (bus-time timestamp ids)))

(defn find-t
    {:test #(do
        (is (= 1068781 (find-t [[0 7] [1 13] [4 59] [6 31] [7 19]])))
        (is (= 3417 (find-t [[0 17] [2 13] [3 19]])))
    )}
    [ls]
    (first
        (reduce
            (fn [acc bus] (let [[x inc] acc [remainder id] bus target-remainder (mod (- id remainder) id)]
                    (if (= remainder 0) [id id]
                        (loop [x1 x]
                            (if (or (= (rem x1 id) target-remainder))
                                [x1 (* id inc)]
                                (recur (+ x1 inc))
                            )
                        )
                    )
                )
            )
            [0 1]
            ls
        )
    )
)

(test #'find-t)

(def indexed-ids (->> ids-str
            (map parse-int)
            (zipmap (range (count ids-str)))
            ; remove nil's
            (filter #(-> (some? (second %))))))

(println "Solution2:" (find-t indexed-ids))