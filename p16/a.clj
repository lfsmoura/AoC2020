(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (Integer/parseInt x))

(def example-input (string/split "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12" #"\n"))

(def example2 (string/split "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9" #"\n"))

(defn parse-line
    {:test #(do
        (is (= { :type :restriction, :name "class" :rule [1 3 5 7]} (parse-line "class: 1-3 or 5-7")))
        (is (= { :type :ticket, :vals '(7 1 14)} (parse-line "7,1,14")))
        (is (nil? (parse-line "nearby tickets:")))
    )}
    [line]
    (let [
            restriction (re-find #"([ a-z]*)\: (\d*)\-(\d*) or (\d*)-(\d*)" line)
            ticket (string/split line #",")
        ]
        (cond (some? restriction) { :type :restriction :name (first (rest restriction)) :rule (apply vector (map parse-int (rest (rest restriction))))}
            (>= (count ticket) 2) { :type :ticket :vals (map parse-int ticket) }
        )
    )
)

(test #'parse-line)

(defn parse-input
    [lines]
    (group-by :type (filter some? (map parse-line lines)))
)

(defn valid
    {:test #(do
        (is (valid 2 [1 3 5 7]))
        (is (valid 7 [1 3 5 7]))
        (is (not (valid 4 [1 3 5 7])))
    )}
    [nb restriction]
    (or
        (and (>= nb (restriction 0)) (<= nb (restriction 1)))
        (and (>= nb (restriction 2)) (<= nb (restriction 3)))
    )
)
(test #'valid)

(defn error-rate
    {:test #(do
        (is (= 71 (error-rate (parse-input example-input))))
    )}
    [input]
    (->> (map
        (fn [ticket]
            ; gets all the values in a ticket
            (->> (map
                    (fn [val]
                        val
                    )
                    (:vals ticket)
                )
                ; filter the ones that are not valid for any restriction
                (filter (fn [val]
                        (not-any? (partial valid val) (map :rule (:restriction input)))
                    )
                )
            )
        )
        ; ignore your ticket
        (rest (:ticket input))
    )
        (flatten)
        (reduce +)
    )
)

(test #'error-rate)

(def input (with-open [rdr (reader "input.txt")]
   (doall (line-seq rdr))))

(println "Solution1:" (error-rate (parse-input input)))

(defn exclude-invalid
    [input]
    (filter
        (fn [ticket]
            ; gets all the values in a ticket
            (->> (map
                    (fn [val]
                        val
                    )
                    (:vals ticket)
                )
                ; filter the ones that are not valid for any restriction
                (filter (fn [val]
                        (not-any? (partial valid val) (map :rule (:restriction input)))
                    )
                )
                ; true for tickets that do not have invalid values
                (empty?)
            )
        )
        ; ignore your ticket
        (rest (:ticket input))
    )
)

; thanks Stack Overflow
(defn transpose [& xs]
  (apply map list xs))

(defn find-fields
    {:test #(do
        (is (= {"row" 11, "class" 12, "seat" 13} (find-fields (parse-input example2))))
    )}
    [input]
    (let [
            my-ticket (first (:ticket input))
            tickets (exclude-invalid input)
            tickets-possible-names (->> tickets
                (map :vals)
                (map (fn [vals]
                    (map (fn [val]
                        (->> (:restriction input)
                            (filter #(-> (valid val (:rule %))))
                            (map :name)
                            (set)
                        )
                        )
                        vals
                    )
                ))
            )
            columns-possible-names (map
                (partial apply set/intersection)
                ; with transpose we get a list of possible restrictions for each field
                (apply transpose tickets-possible-names)
            )
            ; there must be at least one column that has a single possible name
            ; while there are columns that have more than one name we exclude the one-name column
            column-names (map first (loop [possible columns-possible-names used #{}]
                (if (every? (comp (partial = 1 ) count) possible)
                    possible
                    ; get first that is not used and has only one possibility
                    (let [solved (first (filter (fn [column] (and (not (contains? used column)) (= 1 (count column)))) possible))]
                        (recur (map 
                                ;(partial apply set/intersection solved)
                                (fn [column] (if (= column solved) solved (set/difference column solved)))
                                possible
                            )
                            (conj used solved)
                        )
                    )
                )
            ))
        ]
        (zipmap column-names (:vals my-ticket))
    )
)

(test #'find-fields)

(println "Solution2:"
    (apply *
        (map 
            (fn [[k v]] (if (string/starts-with? k "departure") v 1))
            (find-fields (parse-input input)))
        )
    )