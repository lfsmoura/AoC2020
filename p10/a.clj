(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (Long/parseLong x))

(defn tap [val]
  (do (println val) val))

(def example1 '(16
10
15
5
1
11
7
19
6
12
4))

(def example2 '(28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3))

(defn difference
    {:test #(do
        (is (= '(7 5) (difference example1)))
        (is (= '(22 10) (difference example2)))
    )}
    [ls]
    (let [sorted-ls (sort ls)]
        (rest (reduce
            (fn [acc jol] 
                (let [[last one three] acc]
                    [jol (+ one (if (= 1 (- jol last)) 1 0)) (+ three (if (= 3 (- jol last)) 1 0))]
                )
            )
            ; the last adapter is the build in always 3 difference
            [0 0 1]
            sorted-ls
        ))
    )
)

(test #'difference)

(def input (with-open [rdr (reader "input.txt")]
    (map parse-int (doall (line-seq rdr)))))

(println "Solution1:" (apply * (difference input)))

(defn ways
    {:test #(do
        (is (= 8 (ways example1)))
        (is (= 19208 (ways example2)))
    )}
    [ls]
    (let [sorted-ls (sort ls)]
        (get
            (reduce
                (fn [acc jol]
                    (assoc acc jol (apply + (for [x [1 2 3]] (or (get acc (- jol x)) 0))))
                )
                {0 1}
                sorted-ls
            )
            (last sorted-ls)
        )
    )
)

(test #'ways)

(println "Solution2:" (ways input))