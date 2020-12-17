(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (Long/parseLong x))

(defn tap [lab val]
  (do (println lab val) val))

(def example (string/split-lines "F10
N3
F7
R90
F11"))

(defn parse-line
    {:test #(do
        (is (= { :action "F" :val 10 } (parse-line (first example))))
    )}
    [line]
    (let [res (rest (re-find #"([A-Z])(\d*)" line))]
        { :action (first res) :val (parse-int (second res))}
    )
)

(test #'parse-line)

(defn move
    {:test #(do 
        (is (= 25 (move (map parse-line example))))
    )}
    [movs]
    (let [get-forward-dir (fn [dir]
        (get [[1 0] [0 1] [-1 0] [0 -1]] (/ dir 90))
    )
        abs #(-> (max (* % -1) %))
        manhattan (fn [acc] 
            (+ (abs (:x acc)) (abs (:y acc)))
        )
    ]
        (manhattan
            (reduce
                (fn [acc mov]
                    (case (get mov :action)
                        "N" (assoc acc :y (+ (acc :y) (mov :val)))
                        "S" (assoc acc :y (- (acc :y) (mov :val)))
                        "E" (assoc acc :x (+ (acc :x) (mov :val)))
                        "W" (assoc acc :x (- (acc :x) (mov :val)))
                        "R" (assoc acc :dir (mod (- (acc :dir) (mov :val)) 360))
                        "L" (assoc acc :dir (mod (+ (acc :dir) (mov :val)) 360))
                        "F" (let [[xdir ydir] (get-forward-dir (acc :dir))]
                                { :x (+ (acc :x) (* (mov :val) xdir)) :y (+ (acc :y) (* (mov :val) ydir)) :dir (acc :dir) })
                    )
                )
                ; EAST = 0
                { :x 0 :y 0 :dir 0 } 
                movs
            )
        )
    )
)
; it's not 409
(test #'move)

(def input (with-open [rdr (reader "input.txt")]
   (map parse-line (doall (line-seq rdr)))))

(println "Solution1:" (move input))

; rotate clockwisely
(defn rotate-waypoint
    [acc degrees]
    (if (= degrees 0) 
        acc
        (rotate-waypoint (assoc (assoc acc :xway (acc :yway)) :yway (- 0 (acc :xway))) (- degrees 90))
    )
)

(defn move-waypoint
    {:test #(do 
        (is (= 286 (move-waypoint (map parse-line example))))
    )}
    [movs]
    (let [
        abs #(-> (max (* % -1) %))
        manhattan (fn [acc] 
            (+ (abs (:x acc)) (abs (:y acc)))
        )
    ]
        (manhattan
            (reduce
                (fn [acc mov]
                    (case (get mov :action)
                        "N" (assoc acc :yway (+ (acc :yway) (mov :val)))
                        "S" (assoc acc :yway (- (acc :yway) (mov :val)))
                        "E" (assoc acc :xway (+ (acc :xway) (mov :val)))
                        "W" (assoc acc :xway (- (acc :xway) (mov :val)))
                        "R" (rotate-waypoint acc (mov :val))
                        "L" (rotate-waypoint acc (- 360 (mov :val)))
                        "F" {
                                :x (+ (acc :x) (* (acc :xway) (mov :val)))
                                :y (+ (acc :y) (* (acc :yway) (mov :val)))
                                :xway (acc :xway)
                                :yway (acc :yway)
                            }
                    )
                )
                ; EAST = 0
                { :x 0 :y 0 :xway 10 :yway 1 } 
                movs
            )
        )
    )
)

(test #'move-waypoint)

(println "Solution1:" (move-waypoint input))