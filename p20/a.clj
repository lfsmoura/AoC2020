(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (if (integer? x)
        x
        (Integer/parseInt x)))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn cart
    [xs ys]
    (mapcat (fn [a] (map (fn [b] [a b]) ys)) xs)
)

(defn transpose [xs]
  (apply mapv vector xs))

(defn first-f [f ls]
    (if (empty? ls)
        nil
        (or (f (first ls)) (first-f f (rest ls))))
)

(def example (string/split "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..." #"\n"))

(def input (with-open [rdr (reader "input.txt")]
   (doall (line-seq rdr))))

(defn parse-input
    [lines]
    (dissoc (reduce 
        (fn [acc line]
            (let [label (re-find #"Tile (\d*)\:" line)]
                (if label
                    (assoc acc :current (parse-int (get label 1)))
                    (if (empty? line)
                        acc
                        (assoc acc (acc :current) (apply vector (conj (acc (acc :current)) (apply vector line))))
                    )
                )
            )
        )
        {}
        lines
    ) :current)
)

(defn fits
    {:test #(do
        (is (fits {[0 0] { :grid [[0 1] [0 2]]}} [[1 0] [2 0]] 0 1 ))
        (is (fits {[0 0] { :grid [[0 1] [0 2]]}} [[0 2] [4 4]] 1 0 ))
    )}
    [result c i j]
    (and
        ; above
        (if (= i 0)
            true
            (= (first c) (last (get (get result [(dec i) j]) :grid)))
        )
        ; left
        (if (= j 0)
            true
            (= (map first c) (map last (get (get result [i (dec j)]) :grid)))
        )
    )
)

(test #'fits)

(defn get-array
    {:test #(do
        (is (= [[3 2 1] [6 5 4]] (get-array :flip-x [[1 2 3] [4 5 6]])))
        (is (= [[4 5 6] [1 2 3]] (get-array :flip-y [[1 2 3] [4 5 6]])))
        (is (= [[3 1] [4 2]] (get-array [:rotate 1] [[1 2] [3 4]])))
        (is (= [[4 3] [2 1]] (get-array [:rotate 2] [[1 2] [3 4]])))
    )}
    [op grid]
    (case op
        :flip-x (map reverse grid)
        :flip-y (reverse grid)
        (let [[_ rounds] op]
                    (case rounds
                        0 grid
                        1 (map reverse (transpose grid))
                        (get-array [:rotate (dec rounds)] (get-array [:rotate 1] grid))
                    )
                )
    )
)

(test #'get-array)

(defn find-square
    ([config]
    (let [size (int (Math/sqrt (count (keys config))))]
        (find-square size 0 0 config {})
    ))
    ([size i j config-left result]
        (if (>= i size)
            result
            (if (>= j size)
                (find-square size (+ i 1) 0 config-left result)
                (let [candidates (cart '([:rotate 0] [:rotate 1] [:rotate 2] [:rotate 3]) (keys config-left))]
                    ; get first candidate that fits and return a valid result
                    (first-f (fn [[op id]] 
                        (let [m (get-array op (get config-left id))
                            m-flipped (get-array :flip-x m)
                            ]
                            ; candidates are rotations
                            (if (fits result m i j)
                                (find-square size i (inc j) (dissoc config-left id) (assoc result [i j] { :id id :grid m }))
                                ; if the rotation fail we try with a flip
                                (if (fits result m-flipped i j)
                                    (find-square size i (inc j) (dissoc config-left id) (assoc result [i j] { :id id :grid m-flipped }))
                                    nil
                                )
                            )
                        )
                    ) candidates)
                )
            )
        )
    )
)

(defn get-corners
    {:test #(do
        (is (= 20899048083289 (get-corners (parse-input example))))
    )}
    [config]
    (let [size-1 (dec (int (Math/sqrt (count (keys config)))))
        result (find-square config)
        corners (map :id (vals (select-keys result [[0 0] [0 size-1] [size-1 0] [size-1 size-1]])))
        ]
        (apply * corners)
    )
)

(test #'get-corners)

(println "Solution1:" (get-corners (parse-input input)))

(defn get-image-pixel
    [result x y]
    (let [i (quot y 8) j (quot x 8) px (inc (rem x 8)) py (inc (rem y 8))]
        (nth (nth (:grid (get result [i j])) py) px)
    )
)

(defn get-image
    [config]
    (let [size (int (Math/sqrt (count (keys config))))
        result (find-square config)
        ]
        (for [y (range (* size 8))]
            (for [x (range (* size 8))]
                (get-image-pixel result x y)
            )
        )
    )
)

(def example-image (get-image (parse-input example)))

; the image is n x n (12 x 12) pieces
; so there are (n x 8) lines and (n x 8 columns)
; we are searching for pattern
; ..................#.
; #....##....##....###
; .#..#..#..#..#..#...
; so we should search all 3 x 20 blocks

(defn is-monster-block
    [image x y]
    (let [block
            (for [i (range y (+ y 3))]
                (subs (apply str (nth image i)) x (+ x 20))
            )]
        (and 
            (re-find #"..................\#." (first block))
            (re-find #"\#....\#\#....\#\#....\#\#\#" (second block))
            (re-find #".\#..\#..\#..\#..\#..\#..." (nth block 2))
        )
    )
)

(defn count-monster-blocks
    [image]
    (let [size (count image)]
        (count (filter some? (flatten (for [y (range (- size 2))]
            (for [x (range (- size 19))]
                (is-monster-block image x y)
            )
        ))))
    )
)

(defn count-pixels
    [image]
    (count (filter (partial = \#) (apply str (flatten [image]))))
)

(def pixels-in-a-monster (count-pixels "..................#.#....##....##....###.#..#..#..#..#..#..."))

(defn roughness
    {:test #(do
        (is (= 273 (roughness example-image)))
    )}
    [image]
    (let [
            pixels (count-pixels image)
            monster-blocks (count-monster-blocks image)
            ops (cart '([:rotate 0] [:rotate 1] [:rotate 2] [:rotate 3]) '([:rotate 0] :flip-x))
            monsters (first (filter (partial < 0) (map (fn [[op1 op2]] (count-monster-blocks (get-array op2 (get-array op1 image)))) ops)))
        ]
        (- pixels (* monsters pixels-in-a-monster))
    )
)
(test #'roughness)

(println "Solution2:" (roughness (get-image (parse-input input))))