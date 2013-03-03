(ns bencode.core-test
  (:use clojure.test
        bencode.core))

(deftest int-encode-test
  (testing "Testing Type INTeger encoding:"
    (is (= (bencode.core/bint 256) "i256e"))))

(deftest int-decode-test
  (testing "Testing Type INTeger decoding:"
    (is (= (bencode.core/bdint "i3e") 3))))

(deftest string-encode-test
  (testing "Testing Type STRING encoding:"
    (is (= (bencode.core/bstring "spam") "4:spam"))))

(deftest string-decode-test
  (testing "Testing Type STRING decoding:"
    (is (= (bencode.core/bdstring "0:") ""))))

(deftest list-encode-test
  (testing "Testing Type LIST encoding:"
    (is (= (bencode.core/blist (list "spam" "eggs"))
           "l4:spam4:eggse"))))

(deftest list-decode-test
  (testing "Testing Type LIST decoding:"
    (is (= (bencode.core/bdlist "li4e12:leaf cloverse")
           (list 4 "leaf clovers")))))

(deftest dic-encode-test
  (testing "Testing Type DICtionary encoding:"
    (is (= (bencode.core/bdic (sorted-map "cow" "moo", "spam" "eggs"))
           "d3:cow3:moo4:spam4:eggse"))))

(deftest dic-decode-test
  (testing "Testing Type DICtionary decoding:"
    (is (= (bencode.core/bddic "d4:spaml1:a1:bee")
           (sorted-map "spam" (list "a" "b"))))))

(deftest complex-encode-test
  (testing "Testing Type LIST embedded in Types DICtionary and LIST and vice-versa encoding:"
    (is (= (bencode.core/blist (list "i" 8 (sorted-map "fast" "zombies", "ha" (list 101 (hash-map "you" "suck")))))
           "l1:ii8ed4:fast7:zombies2:hali101ed3:you4:suckeeee"))))

(deftest complex-decode-test
  (testing "Testing Type LIST embedded in Types DICtionary and LIST and vice-versa decoding:"
    (is (= (bencode.core/bdlist "l1:ii8ed4:fast7:zombies2:hali101ed3:you4:suckeeee")
           (list "i" 8 (sorted-map "fast" "zombies", "ha" (list 101 (hash-map "you" "suck"))))))))

