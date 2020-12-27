(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn parse-int [x]
    (if (integer? x)
        x
        (Integer/parseInt x)))

(def example-input ["0: 4 1 5"
"1: 2 3 | 3 2"
"2: 4 4 | 5 5"
"4: \"a\""
"3: 4 5 | 5 4"
"5: \"b\""
""
"ababbb"
"bababa"
"abbbab"
"aaabbb"
"aaaabbb"
])

(defn parse-input
    [lines]
    (let [parsed (group-by :type
        (map
            (fn [line]
                (let [rule (re-find #"(\d*)\: (.*)" line)]
                    (if rule
                        { :type :rules :key (parse-int (first (rest rule))) :val (first (rest (rest rule)))}
                        { :type :msg :val line }
                    )
                )
            )
            lines
        )
    )]
        
        [
            (reduce-kv (fn [m k v] (assoc m k (:val (first v)))) {} (group-by :key (get parsed :rules)))
            ; discards empty line
            (rest (map :val (get parsed :msg)))
        ]
    )
)

(def example-rules (first (parse-input example-input)))

(defn build-regex
    [rule rules]
    (let [leaf (re-find #"\"(a|b)\"" rule)]
        (if leaf
            (second leaf)
            (let [or-clause (re-find #"(.*) \| (.*)" rule)]
                (if or-clause
                    (str "(?:(?:" (build-regex (first (rest or-clause)) rules) ")|(?:" (build-regex (second (rest or-clause)) rules) "))")
                    (apply str (map #(-> (build-regex % rules)) (map (partial get rules) (map parse-int (string/split rule #" ")))))
                )
            )
        )
    )
)

(defn cmatch
    {:test #(do
        (is (= 2 (cmatch example-input)))
    )}
    [input]
    (let [
        [rules lines] (parse-input input)
        sregex (str "^" (build-regex (get rules 0) rules) "$")
    ]
        (count
            (filter
                (partial re-matches (re-pattern sregex))
                lines
            )
        )
    )
)

(test #'cmatch)

(def input (with-open [rdr (reader "input.txt")]
   (doall (line-seq rdr))))

(println "Solution1:" (cmatch input))

(def example2 (string/split "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba" #"\n"))

(defn cmatch2
    {:test #(do
        (is (= 12 (cmatch2 example2)))
    )}
    [input]
    (let [
        [rules lines] (parse-input input)
        mod-rules (loop [a 8 b 11 x 0 mod-rules rules]
            (if (>= x 50)
                (assoc mod-rules a "42" b "42 31")
                (let [next-a (+ a 1000 x) next-b (+ b 1000 x 1)] 
                    (recur next-a next-b (+ x 1) (assoc mod-rules a (str "42 | 42 " next-a) b (str "42 31 | 42 " next-b " 31")))
                )
            )
        )
        sregex (str "^" (build-regex (get mod-rules 0) mod-rules) "$")
    ]
        (count
            (filter
                (partial re-matches (re-pattern sregex))
                lines
            )
        )
    )
)

(test #'cmatch2)

(println "Solution2:" (cmatch2 input))