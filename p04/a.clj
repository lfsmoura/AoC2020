(use 'clojure.java.io)
(use 'clojure.test)
(require ['clojure.string :as 'string])
(require ['clojure.set :as 'set])

(defn tap [val]
  (do (println val) val))

(defn parse-int [x] (try 
    (Integer/parseInt x)
    (catch Exception e -1)
))

(def example (list "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd" "byr:1937 iyr:2017 cid:147 hgt:183cm" "" "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884" "hcl:x byr:1929"))

(defn break-passports
    {:test #(do
        (is (= (count (break-passports example)) 2))
    )}
    [lines]
    (let [lines2 (map
            (fn [line] (if (= (count line) 0) "$" line))
           lines 
        )
        joined (string/join " " lines2)
        ]
        (string/split joined #"\$")
    )
)

(test #'break-passports)

(defn read-passport-keys
    {:test #(do
        (is (= (read-passport-keys (first (break-passports example))) #{"ecl" "pid" "eyr" "hcl" "byr" "iyr" "cid" "hgt"}))
    )}
    [passport-str-ls]
    (let
        [key-value-ls (string/split passport-str-ls #" ")]
        (set (map 
            (fn [elem] (first (string/split elem #":")))
            key-value-ls
        ))
    )
)

(test #'read-passport-keys)

(def valid-keyset #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn required-fields
    {:test #(do
        (is (required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}))
        (is (required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"}))
        (is (not (required-fields #{"byr" "iyr" "eyr" "hcl" "ecl" "pid"})))
    )}
    [keyset]
    (set/subset? valid-keyset keyset)
)

(test #'required-fields)

(def input (with-open [rdr (reader "input.txt")]
    (doall (line-seq rdr))))

(println "solution1:" 
    (count
        (filter
            required-fields
            (map
                read-passport-keys
                (break-passports input)))))

(def example2 "hcl:#623a2f")

(defn read-passport
    {:test #(do
        (is (= (read-passport (first (break-passports example))) {"hgt" "183cm", "pid" "860033327", "byr" "1937", "eyr" "2020", "iyr" "2017", "ecl" "gry", "cid" "147", "hcl" "#fffffd"}))
        (is (= (read-passport example2) {"hcl" "#623a2f"}))

    )}
    [passport-str-ls]
    (let
        [key-value-ls (string/split (string/trim passport-str-ls) #" ")]
        (apply 
            hash-map
            (flatten
                (map
                    (fn [elem] (string/split elem #":"))
                    key-value-ls
                )))
    )
)

(test #'read-passport)

(defn valid-field
    [key-value]
    (let [[key value] key-value]
        (case key
            ;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
            "byr" (let [v (parse-int value)] (and (>= v 1920) (<= v 2002)))
            ; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
            "iyr" (let [v (parse-int value)] (and (>= v 2010) (<= v 2020)))
            ; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
            "eyr" (let [v (parse-int value)] (and (>= v 2020) (<= v 2030)))
            ; hgt (Height) - a number followed by either cm or in:
            ; If cm, the number must be at least 150 and at most 193.
            ; If in, the number must be at least 59 and at most 76.
            "hgt" (let [[_ v unity] (re-find #"^(\d*)((in)|(cm))$" value) nb (parse-int v)]
                    (case unity
                        "cm" (and (>= nb 150) (<= nb 193))
                        "in" (and (>= nb 59) (<= nb 76))
                        false
                    ))
            ; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
            "hcl" (not (nil? (re-find #"^\#[a-f0-9]{6}$" value)))
            ; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
            "ecl" (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value)
            ; pid (Passport ID) - a nine-digit number, including leading zeroes.
            "pid" (not (nil? (re-find #"^\d{9}$" value)))
            ; cid (Country ID) - ignored, missing or not.
            true
        )
    )
)

(def invalid1 (read-passport "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"))
(def invalid2 (read-passport "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"))
(def valid1 (read-passport "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"))
(def valid2 (read-passport "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))

(defn valid-passport
    {:test #(do
        (is (valid-passport valid1 ))
        (is (valid-passport valid2 ))
        (is (not (valid-passport invalid1 )))
        (is (not (valid-passport invalid2 )))
    )}
    [passport]
    (and
        (required-fields passport)
        (every?
            valid-field
            passport)
    )
)

(test #'valid-passport)

(println "solution2:" 
    (count
        (filter
           valid-passport
            (map
                read-passport
                (break-passports input)))))