(ns swhid.qualifier-test
  (:require [clojure.test :refer [deftest is testing]]
            [swhid.qualifier :as q]))

(def ^:private percent-encode #'q/percent-encode)
(def ^:private percent-decode #'q/percent-decode)

(deftest percent-round-trip
  (testing "special chars"
    (is (= "%3B" (percent-encode ";")))
    (is (= "%25" (percent-encode "%")))
    (is (= ";" (percent-decode "%3B")))
    (is (= "/a;b%c" (percent-decode (percent-encode "/a;b%c")))))
  (testing "utf-8 byte level"
    (is (= "café" (percent-decode "caf%C3%A9"))))
  (testing "code points outside the BMP survive"
    (is (= "/a🚀b" (percent-decode "/a🚀b")))
    (is (= "/a🚀b" (percent-decode (percent-encode "/a🚀b"))))
    (is (= "🚀" (percent-decode "%F0%9F%9A%80")))))

(deftest percent-decode-malformed
  (is (thrown? clojure.lang.ExceptionInfo (percent-decode "%3")))
  (is (thrown? clojure.lang.ExceptionInfo (percent-decode "%zz")))
  (testing "invalid UTF-8 throws instead of yielding U+FFFD"
    (is (thrown? clojure.lang.ExceptionInfo (percent-decode "%FF")))
    (is (thrown? clojure.lang.ExceptionInfo (percent-decode "caf%C3")))))

(deftest whitespace-is-escaped
  (testing "an escaped space in origin survives the round trip"
    (let [s "origin=https://ex.org/a%20b"]
      (is (= "https://ex.org/a b" (:origin (q/parse s))))
      (is (= s (q/render (q/parse s))))))
  (testing "control and non-ASCII whitespace characters are escaped too"
    (is (= "a%09b%C2%A0c" (percent-encode "a\tb c"))))
  (testing "raw whitespace is rejected"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "origin=https://ex.org/a b")))
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "path=/a b")))))

(deftest path-is-escaped-as-swh-model-does
  (is (= "path=/a%20b/caf%C3%A9/x%3Dy_.-~"
         (q/render {:path "/a b/café/x=y_.-~"})))
  (is (= "/a b/café/x=y_.-~"
         (:path (q/parse "path=/a%20b/caf%C3%A9/x%3Dy_.-~"))))
  (is (= "origin=https://exämple.org/a=b"
         (q/render {:origin "https://exämple.org/a=b"}))
      "origin escapes only what it must"))

(deftest parse-and-render
  (let [s (str "origin=https://example.org/r"
               ";visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9"
               ";anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0"
               ";path=/src/main.clj"
               ";lines=9-15"
               ";bytes=4")
        m (q/parse s)]
    (is (= "https://example.org/r" (:origin m)))
    (is (= "/src/main.clj" (:path m)))
    (is (= [9 15] (:lines m)))
    (is (= [4 nil] (:bytes m)))
    (is (= s (q/render m)))))

(deftest render-canonical-order
  (is (= "origin=x;path=/p;lines=1-2"
         (q/render {:lines [1 2] :path "/p" :origin "x"}))))

(deftest single-line-range
  (is (= [9 nil] (:lines (q/parse "lines=9"))))
  (is (= "lines=9" (q/render {:lines [9 nil]}))))

(deftest path-with-special-chars
  (let [m (q/parse "path=/a%3Bb%25c")]
    (is (= "/a;b%c" (:path m)))
    (is (= "path=/a%3Bb%25c" (q/render m)))))

(deftest rejects-bad-qualifiers
  (testing "unknown keys are ignored (SWHID 1.2, 6.1), even repeated"
    (is (= {} (q/parse "nope=1")))
    (is (= {:lines [1 nil]} (q/parse "nope=1;lines=1;nope=2"))))
  (testing "a key without = is still malformed"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "nope"))))
  (testing "duplicate key"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "lines=1;lines=2"))))
  (testing "missing '='"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "origin"))))
  (testing "empty segment"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "origin=x;"))))
  (testing "a blank origin is rejected, not kept with its visit"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "origin=")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (q/parse "origin=%20;visit=swh:1:snp:94a9ed024d3859793618152ea559a168bbcbb5e2"))))
  (testing "visit must be a core SWHID"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "visit=not-a-swhid"))))
  (testing "range past Long/MAX_VALUE"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "lines=99999999999999999999")))
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "lines=1-99999999999999999999"))))
  (testing "lines are numbered from 1, bytes from 0, a range ends after it starts"
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "lines=0")))
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "lines=0-3")))
    (is (thrown? clojure.lang.ExceptionInfo (q/parse "lines=5-3")))
    (is (= [0 nil] (:bytes (q/parse "bytes=0"))))
    (is (= [3 3] (:lines (q/parse "lines=3-3"))))))

(deftest validate!-cases
  (is (q/validate! {:lines [1 2] :origin "x"}))
  (is (q/validate! {:bytes [0 nil]}))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:lines ["a"]})))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:lines [0 2]})))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:lines [2 1]})))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:lines [1 2 3]})))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:visit "nope"})))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:origin ""})))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:origin " "})))
  (is (thrown? clojure.lang.ExceptionInfo (q/validate! {:unknown "x"}))))
