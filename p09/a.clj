(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (Long/parseLong x))

(defn tap [val]
  (do (println val) val))

(def example (string/split "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576" #"\n"))

; from day 1 (modified)
(defn find-two-that-sum
    [ls num]
    (let [hashset (if (set? ls) ls (set ls))] 
        (reduce 
            (fn [sol elem] (cond
                (not (nil? sol)) sol  
                (and (not= (- num elem) elem) (contains? hashset (- num elem))) (list elem (- num elem))
                :else nil))
            nil ls)
    )
)

(defn find-nb 
    {:test #(do
        (is (= 127 (find-nb 5 (vec (map parse-int example)))))
    )}
    [preamble-size ls]
    (loop [curr preamble-size]
        (let [
                window (subvec ls (- curr preamble-size) curr)
                two-that-sum (find-two-that-sum window (get ls curr))
            ]
            (if (nil? two-that-sum)
                (get ls curr)
                (recur (+ 1 curr))
            )
        )
    )
)

(test #'find-nb)

(def input (with-open [rdr (reader "input.txt")]
    (doall (line-seq rdr))))

(def invalid-number (find-nb 25 (vec (map parse-int input))))

(println "Solution1:" invalid-number)

(defn cont-list
    "searches a contiguos sublist of ls that sum up to invalid"
        {:test #(do
        (is (= [15 25 47 40] (cont-list 127 (vec (map parse-int example)))))
    )}
    [invalid ls]
    (loop [i 0 e 1]
        (let [subls (subvec ls i e) sum (reduce + subls)]
            (cond
                (= sum invalid) subls
                (<= sum invalid) (recur i (+ e 1))
                :else (recur (+ i 1) (+ i 2))
            )
        )
    )
)

(test #'cont-list)

(def sol2 (cont-list invalid-number (vec (map parse-int input))))
(println "Solution2:" (+ (apply min sol2) (apply max sol2)))