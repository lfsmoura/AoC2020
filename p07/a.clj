(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x] (Integer/parseInt x))

(def input (with-open [rdr (reader "input.txt")]
    (doall (line-seq rdr))))

(defn read-bag
    {:test #(do
        (is 
            (=  [ "light red" (list '("1", "bright white") '("2" "muted yellow")) ]
                (read-bag "light red bags contain 1 bright white bag, 2 muted yellow bags.")))
            (= ["drab salmon" '()]
                (read-bag "drab salmon bags contain no other bags."))
    )}
    [s]
    (let [[bag-color contents-str] (rest (re-find #"([^ ]* [^ ]*) bags contain (.*)\." s))
          contents (map rest (re-seq #"(\d*) ([^ ]* [^ ]*) bag[ s,]*" contents-str))
        ]
        [bag-color contents])
)

(test #'read-bag)

(def bag-graph (into {} (map
        read-bag
        input)))

(println "Solution1:"
    (count (filter
        (fn [bag] 
            ; Random First Search (RFS) searching for "shiny gold"
            (loop [to-visit #{bag}
                visited #{}
                ]
                (when (not (empty? to-visit))
                    (let [
                        current (first to-visit)
                        neighbors (map
                                    second
                                    (get bag-graph current))
                        neighbors-not-visited (set/difference (set neighbors) visited)
                    ]
                        (if (= current "shiny gold")
                            true
                            (recur (set/union (rest to-visit) neighbors-not-visited) (conj visited current))
                        ))
                )
            )
        )
        (filter
            #(-> (not= % "shiny gold"))
            (keys bag-graph))
    )))

(def example '("shiny gold bags contain 2 dark red bags."
"dark red bags contain 2 dark orange bags."
"dark orange bags contain 2 dark yellow bags."
"dark yellow bags contain 2 dark green bags."
"dark green bags contain 2 dark blue bags."
"dark blue bags contain 2 dark violet bags."
"dark violet bags contain no other bags."))

(def example-bag-graph (into {} (map
        read-bag
        example)))

(defn how-many-bags
    {:test #(do
        (is (= 126 (how-many-bags example-bag-graph)))
    )}
    [bag-graph]
    (let [how-many-bags-aux (fn how-many-bags-aux [bag]
        (let [neighbors (get bag-graph bag)]
            (if (empty? neighbors)
                1
                (+ 1
                    (apply
                        +
                        (map
                            (fn [neighbor] (let [[qty bag-color] neighbor] (* (parse-int qty) (how-many-bags-aux bag-color))))
                            neighbors
                        ))
                    ))))]
        (- (how-many-bags-aux "shiny gold") 1))
)

(test #'how-many-bags)

(println "Solution2:" (how-many-bags bag-graph))