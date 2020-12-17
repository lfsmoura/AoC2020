(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-long 
    ([x] (try 
    (Long/parseLong x)
    (catch Exception e nil)))
    ([x default-val] (try 
    (Long/parseLong x)
    (catch Exception e default-val)))
)

(defn tap [lab val]
  (do (println lab val) val))

(def example ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
"mem[8] = 11"
"mem[7] = 101"
"mem[8] = 0"])

(defn parse-line 
    {:test #(do
        (is (= [:mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"] (parse-line (first example))))
        (is (= [:mem 8 11] (parse-line (second example))))
    )}
    [line]
    (let [
            mask (re-find #"mask = ([01X]*)" line)
            mem (re-find #"mem\[(\d*)\] = (\d*)" line)
        ]
        (if mask
            [:mask (second mask)]
            [:mem (parse-long (mem 1)) (parse-long (mem 2))]
        )
    )
)

(test #'parse-line)

(def example-mask (second (parse-line (first example))))

(defn apply-mask
    {:test #(do
        (is (= 73 (apply-mask 11 example-mask)))
    )}
    [nb mask]
    (let
        [
            nb-str (Integer/toBinaryString nb)
            zero-padded-nb-str (concat (repeat (- 36 (count nb-str)) \0) nb-str)
        ]
        (Long/parseLong
            (apply str (map
                    (fn [a b] (if (= \X a) b a))
                    mask
                    zero-padded-nb-str
                )
            )
            2 )
    )
)

(test #'apply-mask)

(def input (with-open [rdr (reader "input.txt")]
   (doall (line-seq rdr))))

(defn sim
    {:test #(do
        (is (= 165 (sim (map parse-line example))))
    )}
    [instructions]
    (let [final-mem (reduce 
        (fn [mem inst]
            (if (= :mask (first inst))
                (assoc mem :mask (second inst))
                (assoc mem (inst 1) (apply-mask (inst 2) (mem :mask))))
        )
        {}
        instructions
    )]
        (apply + (vals (dissoc final-mem :mask)))
    )
)

(test #'sim)

(println "Solution1:" (sim (map parse-line input)))

(defn add-char-to-all 
    [ls char]
    (if (empty? ls)
        (list (str char))
        (map 
            str
            ls
            (repeat char)
        )
    )
)

(def example2-mask "000000000000000000000000000000X1001X")

(defn apply-mask-floating
    {:test #(do
        (is (= #{26 27 58 59} (set (apply-mask-floating 42 example2-mask))))
    )}
    [nb mask]
    (let
        [
            nb-str (Integer/toBinaryString nb)
            zero-padded-nb-str (concat (repeat (- 36 (count nb-str)) \0) nb-str)
            masked (apply str (map
                    (fn [a b] (case a
                        \0 b
                        \1 \1
                        \X \X
                    ))
                    mask
                    zero-padded-nb-str
                )
            )
        ]
        (map
            #(-> (Long/parseLong % 2))
            (reduce
                (fn [ls char] 
                    (if (= char \X)
                        (concat (add-char-to-all ls \0) (add-char-to-all ls \1))
                        (add-char-to-all ls char)
                    )
                )
                '()
                masked
            )
        )
    )
)

(test #'apply-mask-floating)

(def example2 ["mask = 000000000000000000000000000000X1001X"
"mem[42] = 100"
"mask = 00000000000000000000000000000000X0XX"
"mem[26] = 1"])

(defn sim2
    {:test #(do
        (is (= 208 (sim2 (map parse-line example2))))
    )}
    [instructions]
    (let [final-mem (reduce 
        (fn [mem inst]
            (if (= :mask (first inst))
                (assoc mem :mask (second inst))
                (merge mem
                    (let [positions (apply-mask-floating (inst 1) (mem :mask))]
                        (apply conj (map 
                            #(-> { % (inst 2) })
                        positions))
                    )
                )
            )
        )
        {}
        instructions
    )]
        (apply + (vals (dissoc final-mem :mask)))
    )
)

(test #'sim2)

(println "Solution2:" (sim2 (map parse-line input)))