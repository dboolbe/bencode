(ns bencode.core)

(defn bstring
  [mystring]
  (str (count mystring) ":" mystring))

(defn bint
  [myint]
  (if (number? myint)
     (str "i" (int myint) "e")
    ""))

(defn bdic [proxi])
(defn bddic [proxi])
(defn bddic-cnt [proxi])

(defn blist
  ([mylist] (blist mylist 0 ""))
  ([mylist cnt nlist]
    (cond
      (> cnt (count mylist)) nlist
      (= cnt (count mylist)) (recur mylist (inc cnt) (str nlist "e"))
      (= cnt 0) (recur mylist (inc cnt) (cond
                                          (map? (nth mylist cnt)) (str nlist "l" (bdic (nth mylist cnt)))
                                          (list? (nth mylist cnt)) (str nlist "l" (blist (nth mylist cnt)))
                                          (number? (nth mylist cnt)) (str nlist "l" (bint (nth mylist cnt)))
                                          :else (str nlist "l" (bstring (nth mylist cnt)))))
      :else (recur mylist (inc cnt) (cond
                                      (map? (nth mylist cnt)) (str nlist (bdic (nth mylist cnt)))
                                      (list? (nth mylist cnt)) (str nlist (blist (nth mylist cnt)))
                                      (number? (nth mylist cnt)) (str nlist (bint (nth mylist cnt)))
                                      :else (str nlist (bstring (nth mylist cnt))))))))

(defn bdic
  ([mydic] (bdic mydic 0 ""))
  ([mydic cnt ndic]
    (cond
      (> cnt (count (keys mydic))) ndic
      (= cnt (count (keys mydic))) (recur mydic (inc cnt) (str ndic "e"))
      (= cnt 0) (recur mydic (inc cnt) (cond
                                         (map? (mydic (nth (keys mydic) cnt))) (str ndic "d" (bstring (nth (keys mydic) cnt)) (bdic (mydic (nth (keys mydic) cnt))))
                                         (list? (mydic (nth (keys mydic) cnt))) (str ndic "d" (bstring (nth (keys mydic) cnt)) (blist (mydic (nth (keys mydic) cnt))))
                                         (number? (mydic (nth (keys mydic) cnt))) (str ndic "d" (bstring (nth (keys mydic) cnt)) (bint (mydic (nth (keys mydic) cnt))))
                                         :else  (str ndic "d" (bstring (nth (keys mydic) cnt)) (bstring (mydic (nth (keys mydic) cnt))))))
      :else (recur mydic (inc cnt) (cond
                                     (map? (mydic (nth (keys mydic) cnt))) (str ndic (bstring (nth (keys mydic) cnt)) (bdic (mydic (nth (keys mydic) cnt))))
                                     (list? (mydic (nth (keys mydic) cnt))) (str ndic (bstring (nth (keys mydic) cnt)) (blist (mydic (nth (keys mydic) cnt))))
                                     (number? (mydic (nth (keys mydic) cnt))) (str ndic (bstring (nth (keys mydic) cnt)) (bint (mydic (nth (keys mydic) cnt))))
                                     :else  (str ndic (bstring (nth (keys mydic) cnt)) (bstring (mydic (nth (keys mydic) cnt)))))))))

(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn bdstring
  ([mystring] (bdstring mystring 0 false))
  ([mystring cnt stat]
    (cond
      stat (let [mylen (parse-number (subs mystring 0 cnt))]
             (subs mystring (inc cnt) (+ (inc cnt) mylen)))
      (= (subs mystring cnt (inc cnt)) ":") (recur mystring cnt (not stat))
      :else (recur mystring (inc cnt) stat))))

(defn bdstring-cnt
  ([mystring] (bdstring-cnt mystring 0 false))
  ([mystring cnt stat]
    (cond
      stat (inc (+ cnt (parse-number (subs mystring 0 cnt))))
      (= (subs mystring cnt (inc cnt)) ":") (recur mystring cnt (not stat))
      :else (recur mystring (inc cnt) stat))))

(defn bdint
  ([myint] (bdint myint 0))
  ([myint cnt]
    (cond
      (= (subs myint cnt (inc cnt)) "e") (parse-number (subs myint 1 cnt))
      :else (recur myint (inc cnt)))))

(defn bdint-cnt
  ([myint] (bdint-cnt myint 0))
  ([myint cnt]
    (cond
      (= (subs myint cnt (inc cnt)) "e") (inc cnt)
      :else (recur myint (inc cnt)))))

(defn bdlist-cnt
  ([mylist] (bdlist-cnt mylist 0))
  ([mylist cnt]
    (cond
      (= (subs mylist cnt (inc cnt)) "e") (inc cnt)
      (= cnt 0) (recur mylist (inc cnt))
      (= (subs mylist cnt (inc cnt)) "d") (let [substr (subs mylist cnt (count mylist))]
                                            ;;(println mylist (+ cnt (bdlist-cnt substr)))
                                            (recur mylist (+ cnt (bddic-cnt substr))))
      (= (subs mylist cnt (inc cnt)) "l") (let [substr (subs mylist cnt (count mylist))]
                                            ;;(println mylist (+ cnt (bdlist-cnt substr)))
                                            (recur mylist (+ cnt (bdlist-cnt substr))))
      (= (subs mylist cnt (inc cnt)) "i") (let [substr (subs mylist cnt (count mylist))]
                                            ;;(println mylist (+ cnt (bdint-cnt substr)))
                                            (recur mylist (+ cnt (bdint-cnt substr))))
      :else (let [substr (subs mylist cnt (count mylist))]
              ;;(println mylist (+ cnt (bdstring-cnt substr)))
              (recur mylist (+ cnt (bdstring-cnt substr)))))))

(defn bddic-cnt
  ([mydic] (bddic-cnt mydic 0))
  ([mydic cnt]
    (cond
      (= (subs mydic cnt (inc cnt)) "e") (inc cnt)
      (= cnt 0) (recur mydic (inc cnt))
      (= (subs mydic cnt (inc cnt)) "d") (let [substr (subs mydic cnt (count mydic))]
                                            ;;(println mydic (+ cnt (bddic-cnt substr)))
                                            (recur mydic (+ cnt (bddic-cnt substr))))
      (= (subs mydic cnt (inc cnt)) "l") (let [substr (subs mydic cnt (count mydic))]
                                            ;;(println mydic (+ cnt (bdlist-cnt substr)))
                                            (recur mydic (+ cnt (bdlist-cnt substr))))
      (= (subs mydic cnt (inc cnt)) "i") (let [substr (subs mydic cnt (count mydic))]
                                            ;;(println mydic (+ cnt (bdint-cnt substr)))
                                            (recur mydic (+ cnt (bdint-cnt substr))))
      :else (let [substr (subs mydic cnt (count mydic))]
              ;;(println mydic (+ cnt (bdstring-cnt substr)))
              (recur mydic (+ cnt (bdstring-cnt substr)))))))

(defn bdlist
  ([mylist] (bdlist mylist 0 (list)))
  ([mylist cnt nlist]
    (cond
      (= (subs mylist cnt (inc cnt)) "e") nlist
      (= cnt 0) (recur mylist (inc cnt) nlist)
      (= (subs mylist cnt (inc cnt)) "d") (let [substr (subs mylist cnt (count mylist))]
                                            (recur mylist (+ cnt (bddic-cnt substr)) (concat nlist (list (bddic substr)))))
      (= (subs mylist cnt (inc cnt)) "l") (let [substr (subs mylist cnt (count mylist))]
                                            (recur mylist (+ cnt (bdlist-cnt substr)) (concat nlist (list (bdlist substr)))))
      (= (subs mylist cnt (inc cnt)) "i") (let [substr (subs mylist cnt (count mylist))]
                                            (recur mylist (+ cnt (bdint-cnt substr)) (concat nlist (list (bdint substr)))))
      :else (let [substr (subs mylist cnt (count mylist))]
              (recur mylist (+ cnt (bdstring-cnt substr)) (concat nlist (list (bdstring substr))))))))

(defn bddic
  ([mydic] (bddic mydic 1 (sorted-map)))
  ([mydic cnt nmap]
    ;;(print "Params:" mydic cnt nmap)
    (if (< cnt (dec (count mydic)))
    (if (= (subs mydic cnt (inc cnt)) "e") nmap
    (let [keyindex (bdstring-cnt (subs mydic cnt (count mydic)))]
      ;;(println "" keyindex (subs mydic (+ cnt keyindex) (+ (inc cnt) keyindex)))
    (cond
      ;;(= (subs mydic cnt (inc cnt)) "e") nmap
      (= cnt 0) (recur mydic (inc cnt) nmap)
      (= (subs mydic (+ cnt keyindex) (+ (inc cnt) keyindex)) "d") (do ;;(println "DEBUG: Parsing Dictionary")
                                                                       (let [substr0 (subs mydic cnt (count mydic))
                                                                             substr1 (subs mydic (+ cnt (bdstring-cnt substr0)) (count mydic))]
                                                                         ;;(println mydic (+ cnt (bddic-cnt substr1)) (bdstring substr0) substr1 );;(bddic substr1) (+ cnt (bdstring-cnt substr0) (bddic-cnt substr1)))
                                                                         (recur mydic (+ cnt (bdstring-cnt substr0) (bddic-cnt substr1)) (assoc nmap (bdstring substr0) (bddic substr1)))))
      (= (subs mydic (+ cnt keyindex) (+ (inc cnt) keyindex)) "l") (do ;;(println "DEBUG: Parsing List")
                                                                       (let [substr0 (subs mydic cnt (count mydic))
                                                                             substr1 (subs mydic (+ cnt (bdstring-cnt substr0)) (count mydic))]
                                                                         ;;(println "String:" mydic (+ cnt (bdlist-cnt substr1)) (bdstring substr0) (bdlist substr1) (+ cnt (bdstring-cnt substr0) (bdlist-cnt substr1)))
                                                                         (recur mydic (+ cnt (bdstring-cnt substr0) (bdlist-cnt substr1)) (assoc nmap (bdstring substr0) (bdlist substr1)))))
      (= (subs mydic (+ cnt keyindex) (+ (inc cnt) keyindex)) "i") (do ;;(println "DEBUG: Parsing Integer")
                                                                       (let [substr0 (subs mydic cnt (count mydic))
                                                                             substr1 (subs mydic (+ cnt (bdstring-cnt substr0)) (count mydic))]
                                                                         ;;(println "String:" mydic (+ cnt (bdint-cnt substr1)) (bdstring substr0) (bdint substr1) (+ cnt (bdstring-cnt substr0) (bdint-cnt substr1)))
                                                                         (recur mydic (+ cnt (bdstring-cnt substr0) (bdint-cnt substr1)) (assoc nmap (bdstring substr0) (bdint substr1)))))
      :else (do ;;(println "DEBUG: Parsing String")
                (let [substr0 (subs mydic cnt (count mydic))
                      substr1 (subs mydic (+ cnt (bdstring-cnt substr0)) (count mydic))]
                  ;;(println "String:" mydic (+ cnt (bdint-cnt substr1)) (bdstring substr0) (bdstring substr1) (+ cnt (bdstring-cnt substr0) (bdint-cnt substr1)))
                  (recur mydic (+ cnt (bdstring-cnt substr0) (bdstring-cnt substr1)) (assoc nmap (bdstring substr0) (bdstring substr1)))))))) nmap)))



