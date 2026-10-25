(ns swhid.hash-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [swhid.core :as swhid]
            [swhid.hash :as h])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(deftest content-swhid-test
  (testing "known Git blob hashes"
    (is (= "swh:1:cnt:e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
           (swhid/swhid->string (h/content-swhid ""))))
    (is (= "swh:1:cnt:3b18e512dba79e4c8300dd08aeb37f8e728b8dad"
           (swhid/swhid->string (h/content-swhid "hello world\n")))))
  (testing "byte array and string agree"
    (is (= (h/content-swhid "hello world\n")
           (h/content-swhid (.getBytes "hello world\n" "UTF-8"))))))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "swhid-test" (into-array FileAttribute []))))

(defn- delete-recursively [f]
  (doseq [child (reverse (file-seq f))]
    (io/delete-file child true)))

(defn- make-fixture
  "hello.txt, run.sh (executable), link -> hello.txt, sub.txt, sub/a.txt.
  The expected tree hash comes from git write-tree."
  [dir]
  (spit (io/file dir "hello.txt") "hello world\n")
  (let [sh (io/file dir "run.sh")]
    (spit sh "#!/bin/sh\n")
    (.setExecutable sh true))
  (spit (io/file dir "sub.txt") "x\n")
  (.mkdir (io/file dir "sub"))
  (spit (io/file dir "sub" "a.txt") "a\n")
  (Files/createSymbolicLink (.toPath (io/file dir "link"))
                            (.toPath (io/file "hello.txt"))
                            (into-array FileAttribute [])))

(deftest path-swhid-test
  (let [dir (temp-dir)]
    (try
      (make-fixture dir)
      (testing "directory tree hash matches git write-tree"
        (is (= "swh:1:dir:f8c7fb941951368c36cf7980a7f5312f58aaeed5"
               (swhid/swhid->string (h/path-swhid dir)))))
      (testing "subdirectory"
        (is (= "swh:1:dir:08585692ce06452da6f82ae66b90d98b55536fca"
               (swhid/swhid->string (h/path-swhid (io/file dir "sub"))))))
      (testing "regular file"
        (is (= "swh:1:cnt:3b18e512dba79e4c8300dd08aeb37f8e728b8dad"
               (swhid/swhid->string (h/path-swhid (io/file dir "hello.txt"))))))
      (testing "symlink hashes its target path"
        (is (= "swh:1:cnt:a5162f80d4a6782b7cb2a0a197f834e683cb9eb1"
               (swhid/swhid->string (h/path-swhid (io/file dir "link"))))))
      (testing "tree-swhids returns every node in one pass"
        (let [m (h/tree-swhids dir)]
          (is (= #{"." "hello.txt" "run.sh" "sub.txt" "sub" "sub/a.txt" "link"}
                 (set (keys m))))
          (is (= (h/path-swhid dir) (m ".")))
          (is (= (h/path-swhid (io/file dir "sub")) (m "sub")))
          (is (= (h/path-swhid (io/file dir "hello.txt")) (m "hello.txt")))
          (is (= (h/path-swhid (io/file dir "link")) (m "link")))))
      (testing ":exclude removes entries from the tree"
        (let [without (h/path-swhid dir)]
          (spit (io/file dir "extra.txt") "extra\n")
          (is (not= without (h/path-swhid dir)))
          (is (= without (h/path-swhid dir {:exclude #(= "extra.txt" (.getName %))})))))
      (finally
        (delete-recursively dir)))))

(deftest special-names-test
  ;; Semicolons, percents, spaces, NFC vs NFD, emoji. Expected hash from
  ;; git write-tree; the same cases pass against the SWHID conformance
  ;; test-suite payloads (validated 25/25 during development).
  ;; Nota: the two "fileé.txt" keys below are distinct byte sequences,
  ;; precomposed é (NFC) then e + combining acute (NFD).
  (let [dir (temp-dir)]
    (try
      (doseq [[name content] {"file;with;semicolon.txt" "semicolon\n"
                              "file with spaces.txt" "spaces\n"
                              "file%with%percent.txt" "percent\n"
                              "fileé.txt" "nfc\n"
                              "fileé.txt" "nfd\n"
                              "🚀.txt" "emoji\n"}]
        (spit (io/file dir name) content))
      (is (= "swh:1:dir:292a3aca12ed1f073fc6c37eaaf5c08096147b24"
             (swhid/swhid->string (h/path-swhid dir))))
      (finally
        (delete-recursively dir)))))

(deftest special-files-are-skipped-test
  ;; Git does not track fifos (nor sockets or devices): the tree hash
  ;; must be the same with and without one.
  (let [dir (temp-dir)]
    (try
      (make-fixture dir)
      (let [before (h/path-swhid dir)
            {:keys [exit]} (sh/sh "mkfifo" (str (io/file dir "pipe")))]
        (when (zero? exit)
          (is (= before (h/path-swhid dir)))
          (is (not (contains? (h/tree-swhids dir) "pipe")))
          (testing "asked for directly, a fifo is still an error"
            (is (thrown? clojure.lang.ExceptionInfo (h/path-swhid (io/file dir "pipe")))))))
      (finally
        (delete-recursively dir)))))

(deftest executable-bit-test
  ;; Git reads the owner execute bit only: a file executable by its group
  ;; alone is 100644, even for root (for whom canExecute is true).
  (let [dir (temp-dir)
        f (io/file dir "group-only.sh")]
    (try
      (spit f "#!/bin/sh\n")
      (Files/setPosixFilePermissions (.toPath f) (PosixFilePermissions/fromString "rw-r-xr--"))
      (is (not (h/executable? f)))
      (is (= ((h/tree-swhids-from-entries
               {"group-only.sh" {:mode "100644" :hash (:hash (h/path-swhid f))}}) ".")
             (h/path-swhid dir)))
      (Files/setPosixFilePermissions (.toPath f) (PosixFilePermissions/fromString "rwxr--r--"))
      (is (h/executable? f))
      (finally
        (delete-recursively dir)))))

(deftest undecodable-name-test
  ;; A name that is not valid UTF-8 decodes to U+FFFD: rather than being
  ;; silently dropped or mis-hashed, the walk throws.
  (let [dir (temp-dir)]
    (try
      (spit (io/file dir "ok.txt") "ok\n")
      (let [{:keys [exit]} (sh/sh "sh" "-c" "touch \"$(printf 'x\\377')\"" :dir (str dir))]
        (when (zero? exit)
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Cannot decode file name"
                                (h/path-swhid dir)))
          (is (thrown? clojure.lang.ExceptionInfo (h/tree-swhids dir)))
          (testing "an excluded entry is not checked"
            (is (= (h/tree-swhids-from-entries
                    {"ok.txt" {:mode "100644" :hash (:hash (h/content-swhid "ok\n"))}})
                   (h/tree-swhids dir {:exclude #(not= "ok.txt" (.getName %))}))))))
      (finally
        (delete-recursively dir)))))

(deftest entry-mode-test
  (is (= ["120000" "40000" "100755" "100644"]
         (map #(apply #'h/entry-mode %)
              [[:symlink false] [:directory false] [:file true] [:file false]]))))

(deftest fold-tree-test
  (let [blob {:rel "sub/a" :name (.getBytes "a" "UTF-8") :mode "100644"
              :digest (byte-array 20) :swhids nil}
        [digest swhids] (#'h/fold-tree [blob] true)]
    (testing "record? false skips the SWHIDs, not the digest"
      (is (nil? (second (#'h/fold-tree [blob] false))))
      (is (= (seq digest) (seq (first (#'h/fold-tree [blob] false))))))
    (testing "a child's own SWHIDs are carried up"
      (is (= #{"sub/a" "sub" "sub/a/deep"}
             (set (keys (second (#'h/fold-tree
                                 [{:rel "sub" :name (.getBytes "sub" "UTF-8") :mode "40000"
                                   :digest digest
                                   :swhids (assoc swhids "sub/a/deep" (swhids "sub/a"))}]
                                 true)))))))))

(deftest empty-directory-test
  (let [dir (temp-dir)]
    (try
      (is (= "swh:1:dir:4b825dc642cb6eb9a060e54bf8d69288fbee4904"
             (swhid/swhid->string (h/path-swhid dir))))
      (finally
        (delete-recursively dir)))))

(deftest missing-path-test
  (is (thrown? Exception (h/path-swhid "/nonexistent/swhid-test"))))

;; Expected values come from the SWHID conformance test-suite
;; (https://github.com/swhid/test-suite, payloads/git/*) except the
;; dangling case, cross-checked against swh.model (the suite's value
;; for that payload is stale).
(deftest snapshot-swhid-test
  (testing "branch ordering (snapshot_branch_order payload)"
    (is (= "swh:1:snp:8f0d48de532ad98671b25f6b069ee3003f46a505"
           (swhid/swhid->string
            (h/snapshot-swhid
             {"HEAD" {:target-type :alias :target "refs/heads/main"}
              "refs/heads/alpha" {:target-type :revision :target "cc0ef1d04d1dbcc7647c66ae625982a05e2a505c"}
              "refs/heads/beta" {:target-type :revision :target "8e5c928382ed0ddba58509d6c23b42ca34f9136f"}
              "refs/heads/gamma" {:target-type :revision :target "fac1f5aca8aeb1ac4f5da75f7d0023afc4622c1b"}
              "refs/heads/main" {:target-type :revision :target "7911b70479ea613a2fa7804ff47fe2a04b717286"}
              "refs/heads/zeta" {:target-type :revision :target "c881b167047f03e91574b86ee6d6e19a9a01a01b"}})))))
  (testing "aliases (alias_branches payload)"
    (is (= "swh:1:snp:9985c2da7ec2950ae93a4bc81d09bbe21ac3d423"
           (swhid/swhid->string
            (h/snapshot-swhid
             {"HEAD" {:target-type :alias :target "refs/heads/main"}
              "refs/heads/alias-feature" {:target-type :revision :target "4afd42e0cb71d7f0776b9dd7dcfb1d8096f554cb"}
              "refs/heads/feature" {:target-type :revision :target "4afd42e0cb71d7f0776b9dd7dcfb1d8096f554cb"}
              "refs/heads/main" {:target-type :revision :target "4afd42e0cb71d7f0776b9dd7dcfb1d8096f554cb"}})))))
  (testing "releases (with_tags payload)"
    (is (= "swh:1:snp:9497c331aac82899611d1c2e9a0eef1d3c161c8d"
           (swhid/swhid->string
            (h/snapshot-swhid
             {"HEAD" {:target-type :alias :target "refs/heads/main"}
              "refs/heads/main" {:target-type :revision :target "d3f10ba4eb9ca2101a437cd54aab53e414af4d91"}
              "refs/heads/release" {:target-type :revision :target "6c43c9a42fbfca5348de247f23bb2db7f25ad3d1"}
              "refs/tags/v1.0" {:target-type :release :target "976993709ac2245f5128a5205653b26eab703fe1"}
              "refs/tags/v2.0" {:target-type :release :target "a7c9921fab18efe11882532bdf751f44a704917a"}})))))
  (testing "dangling branch (cross-checked against swh.model)"
    (is (= "swh:1:snp:946e7036ac56d6706f8c77995edc27335e8bc86a"
           (swhid/swhid->string
            (h/snapshot-swhid
             {"HEAD" {:target-type :alias :target "refs/heads/main"}
              "refs/heads/main" {:target-type :revision :target "9238e09faa058799bd7e180a4da7106978a6e56b"}
              "refs/heads/orphan" {:target-type :dangling :target nil}})))))
  (testing "empty snapshot"
    (is (= "swh:1:snp:1a8893e6a86f444e8be8e7bda6cb34fb1735a00e"
           (swhid/swhid->string (h/snapshot-swhid {})))))
  (testing "invalid branches throw"
    (is (thrown? clojure.lang.ExceptionInfo   ; typo'd target type
                 (h/snapshot-swhid {"HEAD" {:target-type :revison :target "00"}})))
    (is (thrown? clojure.lang.ExceptionInfo   ; target not 40 hex
                 (h/snapshot-swhid {"x" {:target-type :revision :target "00ff"}})))
    (is (thrown? clojure.lang.ExceptionInfo   ; alias without a target
                 (h/snapshot-swhid {"HEAD" {:target-type :alias :target nil}})))))

(deftest tree-swhids-from-entries-test
  (let [dir (temp-dir)]
    (try
      (make-fixture dir)
      (testing "agrees with the directory walk"
        (let [walked (h/tree-swhids dir)
              entry (fn [path mode] [path {:mode mode :hash (:hash (walked path))}])]
          (is (= walked
                 (h/tree-swhids-from-entries
                  (into {} [(entry "hello.txt" "100644") (entry "run.sh" "100755")
                            (entry "sub.txt" "100644") (entry "sub/a.txt" "100644")
                            (entry "link" "120000")]))))))
      (testing "no entry is the empty tree"
        (is (= "swh:1:dir:4b825dc642cb6eb9a060e54bf8d69288fbee4904"
               (swhid/swhid->string ((h/tree-swhids-from-entries {}) ".")))))
      (testing "a submodule is a revision entry"
        (let [m (h/tree-swhids-from-entries
                 {"lib" {:mode "160000" :hash "497b00c6b08cef93c8eef6797eb159653f2cb92b"}})]
          (is (= (swhid/make :revision "497b00c6b08cef93c8eef6797eb159653f2cb92b")
                 (m "lib")))))
      (testing "invalid entries throw"
        (is (thrown? clojure.lang.ExceptionInfo
                     (h/tree-swhids-from-entries {"x" {:mode "100644" :hash "zz"}})))
        (let [blob {:mode "100644" :hash "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"}]
          (doseq [bad ["/x" "x/" "a//b" "./x" "a/../x"]]
            (is (thrown? clojure.lang.ExceptionInfo (h/tree-swhids-from-entries {bad blob})) bad))
          (is (thrown? clojure.lang.ExceptionInfo
                       (h/tree-swhids-from-entries {"a" blob "a/b" blob}))
              "a file cannot also be a directory"))
        (is (thrown? clojure.lang.ExceptionInfo
                     (h/tree-swhids-from-entries
                      {"x" {:mode "40000" :hash "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"}}))))
      (finally
        (delete-recursively dir)))))
