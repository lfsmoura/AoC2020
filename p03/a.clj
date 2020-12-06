(use 'clojure.java.io)
(use 'clojure.test)
(require '[clojure.string :as str])

(defn tap [val]
  (do (println val) val))

(def example "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def example2 ".."
"..")

(defn is-tree
  "returns a function that returns 1 if position y, x is a tree, 0 otherwise"
  {:test
   #(do
      (is (= ((is-tree example) 0 0) 0))
      (is (= ((is-tree example) 0 2) 1))
      ;;(is (= ((is-tree example) 11 2) 1))
      ;;(is (= ((is-tree example) 11 12) 1))
      )}
  [tree-map]
  (fn [y x]
    (let [lines (str/split tree-map #"\n")
          nbColumns (count (first lines))
          nbLines (count lines)
          map-x (mod x nbColumns)]
      (if (>= y nbLines)
        0
        (case (nth (nth lines y) map-x)
          \# 1
          \. 0
          :else 0
        )
      )
    )
  )
)

(test #'is-tree)

(defn how-many-trees
  "How many trees you pass on tree-map "
  {:test
    #(do
      (is (= (how-many-trees example 1 3) 7))
    )
  }
  [tree-map down right]
  (let [is-tree-fn (is-tree tree-map)
    lines (str/split tree-map #"\n")
    nbLines (count lines)
  ]
    (reduce
      +
      (for [i (range 1 nbLines)] 
        (is-tree-fn (* i down) (* i right))
      )
    )
  )
)

(test #'how-many-trees)

(def input (with-open [rdr (reader "input.txt")]
    (doall (line-seq rdr))))

(def concat-input (str/join "\n" input))

(println "Solution 1:" (how-many-trees concat-input 1 3))
(println "Solution 2:" 
  (reduce
    *
    (list (how-many-trees concat-input 1 1)
      (how-many-trees concat-input 1 3)
      (how-many-trees concat-input 1 5)
      (how-many-trees concat-input 1 7)
      (how-many-trees concat-input 2 1)
    )
  )
)