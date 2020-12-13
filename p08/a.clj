(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (Integer/parseInt x))

(defn tap [val]
  (do (println val) val))

(def example '("nop +0" "acc +1" "jmp +4" "acc +3" "jmp -3" "acc -99" "acc +1" "jmp -4" "acc +6"))

(defn read-instr
    {:test #(do
        (is (= ["nop" 0] (read-instr "nop +0")))
        (is (= ["acc" 1] (read-instr "acc +1")))
        (is (= ["jmp" -99] (read-instr "jmp -99")))
    )}
    [line]
    (let [[_ op nb] (re-find #"([a-z]{3}) ((\+|\-)\d*)" line)]
        [op (parse-int nb)])
)

(test #'read-instr)

(defn run
    {:test #(do
        (is (= '(:loop 5) (run (vec (map read-instr example)))))
    )}
    [instructions]
    (loop [curr 0 acc 0 visited #{}]
        (cond
            (get visited curr) (list :loop acc)
            (>= curr (count instructions)) (list :finish acc)
            :else (let [[curr-instr nb] (get instructions curr)
                    new-visited (conj visited curr)
                    ]
                    (case curr-instr
                        "nop" (recur (+ 1 curr) acc new-visited)
                        "acc" (recur (+ 1 curr) (+ acc nb) new-visited)
                        "jmp" (recur (+ curr nb) acc new-visited)
                    )
                )
        )
    )
)

(test #'run)

(def input (with-open [rdr (reader "input.txt")]
    (doall (line-seq rdr))))

(println "Solution1:" (run (vec (map read-instr input))))

(defn swap-jmp-nop
    {:test #(do
        (is (= ['("acc" 1) '("nop" 2)] (swap-jmp-nop 1 ['("acc" 1) '("jmp" 2)])))
        (is (= ['("acc" 1) '("acc" 2)] (swap-jmp-nop 1 ['("acc" 1) '("acc" 2)])))
        (is (= ['("jmp" 1) '("jmp" 2)] (swap-jmp-nop 0 ['("nop" 1) '("jmp" 2)])))
    )}
    [nb instructions]
    (update instructions nb #(-> (list (case (first %) "jmp" "nop" "nop" "jmp" "acc") (second %))))
)

(test #'swap-jmp-nop)

; switch nop for jmp or jmp for nop
(defn find-corrupted
    {:test #(do
        (is (= 8 (find-corrupted (vec (map read-instr example)))))
    )}
    [instructions]
    (loop [swap-nb 0]
        (let [result (run (swap-jmp-nop swap-nb instructions))]
            (if (= :finish (first result))
                (second result)
                (recur (+ swap-nb 1)))
        )
    )
)

(test #'find-corrupted)

(println "Solution2:" (find-corrupted (vec (map read-instr input))))