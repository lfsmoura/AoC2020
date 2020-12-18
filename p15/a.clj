(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(def example1 [0 3 6])
(def example2 [1 3 2])
(def example3 [1 2 3])
(def input [6 3 15 13 1 0])

(defn tap [val]
  (do (println val) val))

(def second-threshold 30000000)

(defn game
    {:test #(do
        (is (= 0 (game example1 4)))
        (is (= 3 (game example1 5)))
        (is (= 3 (game example1 6)))
        (is (= 0 (game example1 10)))
        (is (= 436 (game example1 2020)))
        (is (= 1 (game example2 2020)))
        (is (= 27 (game example3 2020)))
        (is (= 175594 (game [0 3 6] second-threshold)))
    )}
    [ls nth]
    (loop [
            i (+ 1 (count ls))
            last (last ls)
            ; don't add the last number to the Map
            last-table (zipmap ls (range 1 (count ls)))
        ]
        (let [current (if (nil? (last-table last)) 
                    0
                    (- i (last-table last) 1)
                )]
                ;(do (println ">" i "last" last last-table)
                (if (= i nth)
                    current
                    (recur 
                        (+ 1 i)
                        current
                        (assoc last-table last (- i 1))
                    )
                )
                ;)
        )
    )
)

(test #'game)

(println "Solution1:" (game input 2020))
(println "Solution2:" (game input second-threshold))