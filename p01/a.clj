(use 'clojure.java.io)
(use 'clojure.test)

(defn tap [val] 
    (do (println val) val))

(defn parse-int [x] (Integer/parseInt x))

(def example (list 1721 979 366 299 675 1456))

(defn find-two-that-sum
    {:test #(do
        (is (= (set (find-two-that-sum example 2020)) (set '(1721 299))))
    )}
    [ls num]
    (let [hashset (if (set? ls) ls (set ls))] 
        (reduce 
            (fn [sol elem] (cond
                (not (nil? sol)) sol  
                (contains? hashset (- num elem)) (list elem (- num elem))
                :else nil))
            nil ls)
    )
)

(test #'find-two-that-sum)

(defn find-three-that-sum
    {:test #(do 
        (is (= (set (find-three-that-sum example 2020)) (set (list 979 366 675))))
    )}
    [ls num]
    (let [hashset (if (set? ls) ls (set ls))] 
        (reduce
            (fn [sol elem] 
                (if (nil? sol)
                    (let [two-that-sum (find-two-that-sum ls (- num elem))]
                        (if (nil? two-that-sum)
                        nil
                        (cons elem two-that-sum))
                    )
                    sol
                )
            )
            nil
            ls
        )
    )
)

(test #'find-three-that-sum)

(def input (with-open [rdr (reader "input.txt")]
    (map parse-int (doall (line-seq rdr) ))))

(println "Solution 1:" (apply * (find-two-that-sum input 2020)))
(println "Solution 2:" (apply * (find-three-that-sum input 2020)))