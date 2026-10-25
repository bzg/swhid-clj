(ns swhid.core-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [are deftest is testing]]
            [swhid.core :as swhid]))

(def core-examples
  ["swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
   "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505"
   "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d"
   "swh:1:rel:22ece559cc7cc2364edc5e5593d63ae8bd229f9f"
   "swh:1:snp:c7c108084bc0bf3d81436bf980b46e98bd338453"])

(def qualified-example
  (str "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b"
       ";origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git"
       ";visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9"
       ";anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0"
       ";path=/Examples/SimpleFarm/simplefarm.ml"
       ";lines=9-15"))

(deftest parse-core
  (let [m (swhid/parse "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2")]
    (is (= {:type :content :hash "94a9ed024d3859793618152ea559a168bbcbb5e2"}
           m))
    (is (not (contains? m :qualifiers)))))

(deftest round-trip-core
  (doseq [s core-examples]
    (is (= s (swhid/swhid->string (swhid/parse s))) s)))

(deftest round-trip-qualified
  (is (= qualified-example
         (swhid/swhid->string (swhid/parse qualified-example)))))

(deftest parse-qualified-values
  (let [m (swhid/parse qualified-example)]
    (is (= :content (swhid/object-type m)))
    (is (= "https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git"
           (get-in m [:qualifiers :origin])))
    (is (= "/Examples/SimpleFarm/simplefarm.ml" (get-in m [:qualifiers :path])))
    (is (= [9 15] (get-in m [:qualifiers :lines])))))

(deftest valid?-cases
  (testing "valid"
    (doseq [s (conj core-examples qualified-example)]
      (is (swhid/valid? s) s)))
  (testing "invalid"
    (doseq [s ["swh:1:xxx:94a9ed024d3859793618152ea559a168bbcbb5e2"   ; bad type
               "swh:1:cnt:94A9ED024D3859793618152EA559A168BBCBB5E2"   ; uppercase hex
               "swh:1:cnt:94a9ed024d385979"                           ; too short
               "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"   ; bad version
               "not-a-swhid"
               ""]]
      (is (not (swhid/valid? s)) s))))

(deftest make-and-render
  (is (= "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505"
         (swhid/swhid->string
          (swhid/make :directory "d198bc9d7a6bcf6db04f476d29314f157507d505"))))
  (testing "bad type / hash throw"
    (is (thrown? clojure.lang.ExceptionInfo (swhid/make :nope "d198bc9d7a6bcf6db04f476d29314f157507d505")))
    (is (thrown? clojure.lang.ExceptionInfo (swhid/make :directory "zzz"))))
  (testing "qualifiers that are not a map throw ex-info"
    (doseq [bad ["x" 5 [[:origin "x"]]]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (swhid/make :directory "d198bc9d7a6bcf6db04f476d29314f157507d505" bad))
          (pr-str bad))))
  (testing "swhid->string rejects unknown types"
    (is (thrown? clojure.lang.ExceptionInfo
                 (swhid/swhid->string {:type :dir :hash "d198bc9d7a6bcf6db04f476d29314f157507d505"})))))

(deftest core-swhid-strips-qualifiers
  (let [m (swhid/parse qualified-example)]
    (is (= "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b"
           (swhid/swhid->string (swhid/core-swhid m))))))

(deftest predicates
  (is (swhid/content? (swhid/parse (first core-examples))))
  (is (swhid/snapshot? (swhid/parse (last core-examples))))
  (is (not (swhid/revision? (swhid/parse (first core-examples))))))

(deftest qualifier-algebra
  (testing "composition rebuilds the qualified example"
    (is (= qualified-example
           (-> (swhid/make :content "4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b")
               (swhid/with-origin "https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git")
               (swhid/with-visit "swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9")
               (swhid/with-anchor (swhid/make :revision "2db189928c94d62a3b4757b3eec68f0a4d4113f0"))
               (swhid/with-path "/Examples/SimpleFarm/simplefarm.ml")
               (swhid/with-lines 9 15)
               swhid/swhid->string))))
  (testing "with-visit accepts a SWHID map and drops its qualifiers"
    (let [snp (-> (swhid/make :snapshot "d7f1b9eb7ccb596c2622c4780febaa02549830f9")
                  (swhid/with-origin "https://example.org"))]
      (is (= "swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9"
             (-> (swhid/parse (first core-examples))
                 (swhid/with-visit snp)
                 :qualifiers :visit)))))
  (testing "single-line range renders without dash"
    (is (= "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2;lines=9"
           (-> (swhid/parse (first core-examples))
               (swhid/with-lines 9)
               swhid/swhid->string))))
  (testing "without-qualifier removes the key and the map when empty"
    (let [m (-> (swhid/parse (first core-examples)) (swhid/with-lines 9 15))]
      (is (= (swhid/parse (first core-examples))
             (swhid/without-qualifier m :lines)))))
  (testing "invalid values throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (swhid/with-visit (swhid/parse (first core-examples)) "not-a-swhid")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (swhid/with-lines (swhid/parse (first core-examples)) -1)))))

(deftest spec-qualifier-rules
  ;; SWHID 1.2, clause 6: ignore invalid qualifiers when parsing, never
  ;; generate them
  (let [cnt "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
        dir "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505"
        snp "swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9"
        rev "swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0"
        reparse (comp swhid/swhid->string swhid/parse)]
    (testing "parsing ignores what the spec says to ignore"
      (are [in out] (= out (reparse in))
        (str cnt ";nope=1") cnt
        (str cnt ";nope=x;lines=1") (str cnt ";lines=1")
        (str dir ";lines=1-2") dir
        (str dir ";origin=https://e.org;bytes=0") (str dir ";origin=https://e.org")
        (str cnt ";lines=1;bytes=0") (str cnt ";bytes=0")
        (str cnt ";visit=" snp) cnt
        (str cnt ";origin=https://e.org;visit=" rev) (str cnt ";origin=https://e.org")
        (str dir ";anchor=" rev) dir
        (str dir ";anchor=" cnt ";path=/a") (str dir ";path=/a"))
      (is (not (contains? (swhid/parse (str cnt ";visit=" snp)) :qualifiers))))
    (testing "what is valid is kept, path on a content included"
      (are [s] (= s (reparse s))
        (str cnt ";origin=https://e.org;visit=" snp)
        (str dir ";anchor=" rev ";path=/a")
        (str cnt ";path=/a;lines=1-2")
        qualified-example))
    (testing "path must be absolute"
      (doseq [p ["a/b" "//a" ""]]
        (is (not (swhid/valid? (str dir ";path=" p))) p)
        (is (thrown? clojure.lang.ExceptionInfo
                     (swhid/with-path (swhid/parse dir) p)) p))
      (is (swhid/valid? (str dir ";path=/"))))
    (testing "building throws on qualifiers that can never be valid"
      (are [f] (thrown? clojure.lang.ExceptionInfo (f))
        #(swhid/with-lines (swhid/parse dir) 1 2)
        #(swhid/with-bytes (swhid/with-lines (swhid/parse cnt) 1 2) 0 3)
        #(swhid/with-visit (swhid/parse cnt) rev)
        #(swhid/with-anchor (swhid/parse dir) cnt)
        #(swhid/make :directory (subs dir 10) {:lines [1 2]})))
    (testing "visit and anchor may come before origin and path, and are
              left out of the rendering until then"
      (let [m (-> (swhid/parse dir) (swhid/with-visit snp) (swhid/with-anchor rev))]
        (is (= dir (swhid/swhid->string m)))
        (is (= (str dir ";origin=https://e.org;visit=" snp ";anchor=" rev ";path=/a")
               (-> m (swhid/with-origin "https://e.org") (swhid/with-path "/a")
                   swhid/swhid->string)))))))

(deftest tagged-literal-test
  (testing "#swh/id in code"
    (is (= (swhid/parse qualified-example)
           #swh/id "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=9-15")))
  (testing "pr-str prints the tagged literal, which reads back"
    (doseq [m [(swhid/parse qualified-example)
               #swh/id "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
               (swhid/make :directory "d198bc9d7a6bcf6db04f476d29314f157507d505")
               (-> (swhid/parse qualified-example) (swhid/with-lines 1 2) swhid/core-swhid)]]
      (is (= (str "#swh/id \"" (swhid/swhid->string m) "\"") (pr-str m)))
      (is (= m (edn/read-string {:readers swhid/edn-readers} (pr-str m))))))
  (testing "a SWHID nested in data prints as a literal too"
    (is (= "{:dep #swh/id \"swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2\"}"
           (pr-str {:dep (swhid/parse (first core-examples))}))))
  (testing "a map without the metadata, or that does not render, prints as a map"
    (let [m (swhid/parse (first core-examples))]
      (is (= "{:type :content, :hash \"94a9ed024d3859793618152ea559a168bbcbb5e2\"}"
             (pr-str (into {} m))))
      (is (= "{:type :nope, :hash \"94a9ed024d3859793618152ea559a168bbcbb5e2\"}"
             (pr-str (assoc m :type :nope))))))
  (testing "clojure.edn with edn-readers"
    (is (= (swhid/parse (first core-examples))
           (edn/read-string {:readers swhid/edn-readers}
                            "#swh/id \"swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2\"")))))
