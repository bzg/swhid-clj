(ns swhid.git-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [swhid.core :as swhid]
            [swhid.git :as g]
            [swhid.hash :as h])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private git-env
  "Fixed identity and dates so the fixture repository is deterministic."
  {"GIT_AUTHOR_NAME" "Test" "GIT_AUTHOR_EMAIL" "test@example.org"
   "GIT_COMMITTER_NAME" "Test" "GIT_COMMITTER_EMAIL" "test@example.org"
   "GIT_AUTHOR_DATE" "1700000000 +0000" "GIT_COMMITTER_DATE" "1700000000 +0000"})

(defn- git! [dir & args]
  (let [{:keys [exit err]} (apply sh/sh "git" "-C" (str dir)
                                  (concat args [:env (merge (into {} (System/getenv))
                                                            git-env)]))]
    (assert (zero? exit) err)))

(defn- make-repo
  "main + feature on one commit, annotated tag v1, lightweight tag lw."
  []
  (let [dir (.toFile (Files/createTempDirectory "swhid-git-test" (into-array FileAttribute [])))]
    (git! dir "init" "-q" "-b" "main" ".")
    (spit (io/file dir "a.txt") "hello world\n")
    (git! dir "add" "a.txt")
    (git! dir "commit" "-q" "-m" "one")
    (git! dir "branch" "feature")
    (git! dir "tag" "-a" "-m" "release v1" "v1")
    (git! dir "tag" "lw")
    dir))

(defn- delete-recursively [f]
  (doseq [child (reverse (file-seq f))]
    (io/delete-file child true)))

(defn- rev
  "Object id `r` resolves to, unpeeled."
  [dir r]
  (str/trim (:out (sh/sh "git" "-C" (str dir) "rev-parse" r))))

;; Pure helpers: no repository needed.

(def ^:private id1 "497b00c6b08cef93c8eef6797eb159653f2cb92b")
(def ^:private id2 "1ac172c2020e5a34b8ebf71bf11ec619689467ba")

(deftest parse-config-flags-test
  (let [parse #'g/parse-config-flags]
    (is (= {:filemode? true :symlinks? true} (parse "")))
    (is (= {:filemode? false :symlinks? true}
           (parse "core.filemode true\ncore.filemode false\n"))
        "the last value wins")
    (is (= {:filemode? true :symlinks? false} (parse "core.symlinks false\n")))))

(deftest refs->branches-test
  (let [refs (#'g/parse-refs (str id1 "\trefs/heads/main\t\n"
                                  id2 "\trefs/tags/v1\t\n"
                                  id1 "\trefs/remotes/origin/HEAD\trefs/remotes/origin/main\n"
                                  "\n"))
        types (#'g/parse-object-types [id1 id2] (str "commit\n" id2 " missing\n"))]
    (is (= [[id1 "refs/heads/main" ""] [id2 "refs/tags/v1" ""]
            [id1 "refs/remotes/origin/HEAD" "refs/remotes/origin/main"]]
           refs))
    (is (= {id1 :revision id2 :dangling} types))
    (is (= {"HEAD" {:target-type :alias :target "refs/heads/main"}
            "refs/heads/main" {:target-type :revision :target id1}
            "refs/tags/v1" {:target-type :dangling :target nil}
            "refs/remotes/origin/HEAD" {:target-type :alias :target "refs/remotes/origin/main"}}
           (#'g/refs->branches refs types {:target-type :alias :target "refs/heads/main"})))
    (is (thrown? clojure.lang.ExceptionInfo (#'g/parse-object-types [id1] "weird\n")))))

(deftest parse-index-test
  (is (= {"a.txt" {:mode "100644" :hash id1}
          "dir/b\tc" {:mode "100755" :hash id2}}
         (#'g/parse-index [(str "100644 " id1 " 0\ta.txt")
                           (str "100755 " id2 " 0\tdir/b\tc")
                           (str "100644 " id1 " 2\tconflict.txt")]))
      "tabs in paths survive, unmerged entries are left out"))

(deftest worktree-entry-test
  (let [entry #'g/worktree-entry
        default {:filemode? true :symlinks? true}
        file {:mode "100644" :hash id1}]
    (testing "a regular file takes its mode from disk and gets no hash"
      (is (= {:mode "100755"} (entry default file {:kind :file :executable? true})))
      (is (= {:mode "100644"} (entry default nil {:kind :file :executable? false}))))
    (testing "core.fileMode=false keeps the index mode"
      (let [flags (assoc default :filemode? false)]
        (is (= {:mode "100644"} (entry flags file {:kind :file :executable? true})))
        (is (= {:mode "100755"} (entry flags {:mode "100755"} {:kind :file :executable? false})))
        (is (= {:mode "100644"} (entry flags {:mode "120000"} {:kind :file :executable? true})))))
    (testing "core.symlinks=false keeps a symlink checked out as a file"
      (is (= {:mode "120000"} (entry (assoc default :symlinks? false) {:mode "120000"}
                                     {:kind :file :executable? false}))))
    (testing "a symlink carries its own hash"
      (is (= {:mode "120000" :hash id2} (entry default file {:kind :symlink :hash id2}))))
    (testing "submodules"
      (let [sub {:mode "160000" :hash id1}]
        (is (= {:mode "160000" :hash id2} (entry default sub {:kind :directory :hash id2})))
        (is (= sub (entry default sub {:kind :directory :hash nil}))
            "an unreadable checkout keeps its commit")
        (is (nil? (entry default sub nil)) "a deleted checkout leaves the tree")))
    (testing "a directory replacing a tracked file, or nothing at all, removes it"
      (is (nil? (entry default file {:kind :directory})))
      (is (nil? (entry default file nil))))))

(deftest apply-changes-test
  (is (= {"a" {:mode "100755" :hash id2}
          "l" {:mode "120000" :hash id1}}
         (#'g/apply-changes {"a" {:mode "100644" :hash id1}
                             "b" {:mode "100644" :hash id1}}
                            {"a" {:mode "100755"}
                             "b" nil
                             "l" {:mode "120000" :hash id1}}
                            {"a" id2}))))

;; Expected values cross-checked against swh.model (`swh identify`).
(deftest git-repo-test
  (let [repo (make-repo)]
    (try
      (testing "snapshot"
        (is (= "swh:1:snp:a525d9aa2ad1fdd1e045cb6abf4369f0eb6850db"
               (swhid/swhid->string (g/snapshot-swhid repo)))))
      (testing "revision"
        (is (= "swh:1:rev:497b00c6b08cef93c8eef6797eb159653f2cb92b"
               (swhid/swhid->string (g/revision-swhid repo))))
        (is (= (g/revision-swhid repo)
               (g/revision-swhid repo "main")
               ;; an annotated tag peels to its commit
               (g/revision-swhid repo "v1"))))
      (testing "release"
        (is (= "swh:1:rel:1ac172c2020e5a34b8ebf71bf11ec619689467ba"
               (swhid/swhid->string (g/release-swhid repo "v1"))))
        (testing "a lightweight tag is not a release"
          (is (thrown? Exception (g/release-swhid repo "lw")))))
      (finally
        (delete-recursively repo)))))

(deftest detached-head-test
  (let [repo (make-repo)]
    (try
      (git! repo "checkout" "-q" "--detach")
      (testing "a detached HEAD is a revision branch, not an alias"
        (let [commit (rev repo "HEAD")]
          (is (= (h/snapshot-swhid
                  {"HEAD" {:target-type :revision :target commit}
                   "refs/heads/main" {:target-type :revision :target commit}
                   "refs/heads/feature" {:target-type :revision :target commit}
                   "refs/tags/lw" {:target-type :revision :target commit}
                   "refs/tags/v1" {:target-type :release :target (rev repo "v1")}})
                 (g/snapshot-swhid repo)))))
      (finally
        (delete-recursively repo)))))

(deftest unborn-head-test
  (let [dir (.toFile (Files/createTempDirectory "swhid-git-test"
                                                (into-array FileAttribute [])))]
    (try
      (git! dir "init" "-q" "-b" "main" ".")
      (testing "a fresh repo snapshots HEAD as an alias to the unborn branch"
        (is (= (h/snapshot-swhid
                {"HEAD" {:target-type :alias :target "refs/heads/main"}})
               (g/snapshot-swhid dir))))
      (testing "its tracked working tree is empty"
        (is (= {"." (swhid/make :directory "4b825dc642cb6eb9a060e54bf8d69288fbee4904")}
               (g/worktree-swhids dir))))
      (finally
        (delete-recursively dir)))))

(def ^:private no-junk
  "Walk exclusion matching what Git leaves out of the fixture below."
  {:exclude #(contains? #{".git" "target" "untracked.txt"} (.getName %))})

(deftest worktree-swhids-test
  (let [repo (make-repo)
        head-tree (fn [] (swhid/make :directory (rev repo "HEAD^{tree}")))]
    (try
      (testing "a clean tree is the tree of HEAD"
        (let [m (g/worktree-swhids repo)]
          (is (= (head-tree) (m ".")))
          (is (= #{"." "a.txt"} (set (keys m))))))
      (testing "ignored and untracked files do not count"
        (spit (io/file repo ".gitignore") "target/\n")
        (git! repo "add" ".gitignore")
        (git! repo "commit" "-q" "-m" "ignore")
        (.mkdir (io/file repo "target"))
        (spit (io/file repo "target" "junk") "junk\n")
        (spit (io/file repo "untracked.txt") "new\n")
        (let [m (g/worktree-swhids repo)]
          (is (= (head-tree) (m ".")))
          (is (not (contains? m "untracked.txt")))
          (is (not (contains? m "target")))))
      (testing "a modified tracked file is hashed from the working tree"
        (spit (io/file repo "a.txt") "changed\n")
        (let [m (g/worktree-swhids repo)]
          (is (not= (head-tree) (m ".")))
          (is (= (h/path-swhid (io/file repo "a.txt")) (m "a.txt")))
          (is (= (h/path-swhid repo no-junk) (m ".")))))
      (testing "a deleted tracked file leaves the tree"
        (io/delete-file (io/file repo "a.txt"))
        (let [m (g/worktree-swhids repo)]
          (is (not (contains? m "a.txt")))
          (is (= (h/path-swhid repo no-junk) (m ".")))))
      (testing "staged content counts, subdirectories get dir SWHIDs"
        (git! repo "checkout" "-q" "a.txt")
        (.mkdir (io/file repo "sub"))
        (spit (io/file repo "sub" "b.txt") "b\n")
        (git! repo "add" "sub/b.txt")
        (let [m (g/worktree-swhids repo)]
          (is (= (h/path-swhid (io/file repo "sub")) (m "sub")))
          (is (= (h/path-swhid (io/file repo "sub" "b.txt")) (m "sub/b.txt")))
          (is (= (h/path-swhid repo no-junk) (m ".")))))
      (finally
        (delete-recursively repo)))))

(defn- tree-of
  "Root SWHID of the tree made of `entries` ({path [mode content]})."
  [entries]
  ((h/tree-swhids-from-entries
    (update-vals entries (fn [[mode content]]
                           {:mode mode :hash (:hash (h/content-swhid content))})))
   "."))

(deftest worktree-core-config-test
  (let [repo (make-repo)
        a (io/file repo "a.txt")]
    (try
      (spit a "changed\n")
      (.setExecutable a true)
      (testing "the executable bit counts by default"
        (is (= (tree-of {"a.txt" ["100755" "changed\n"]})
               ((g/worktree-swhids repo) "."))))
      (testing "with core.fileMode=false, the index's mode is kept"
        (git! repo "config" "core.fileMode" "false")
        (is (= (tree-of {"a.txt" ["100644" "changed\n"]})
               ((g/worktree-swhids repo) "."))))
      (testing "with core.symlinks=false, a symlink checked out as a file stays one"
        (git! repo "config" "core.symlinks" "false")
        (git! repo "checkout" "-q" "a.txt")
        (let [link-hash (str/trim (:out (sh/sh "git" "-C" (str repo) "hash-object" "-w"
                                               "--stdin" :in "a.txt")))]
          (git! repo "update-index" "--add" "--cacheinfo" (str "120000," link-hash ",link"))
          ;; what checkout writes for a symlink when core.symlinks=false
          (spit (io/file repo "link") "a.txt")
          (is (= (tree-of {"a.txt" ["100644" "hello world\n"] "link" ["120000" "a.txt"]})
                 ((g/worktree-swhids repo) ".")))))
      (finally
        (delete-recursively repo)))))

(deftest worktree-hashing-paths-test
  (let [repo (make-repo)]
    (try
      (spit (io/file repo ".gitattributes") "*.txt text=auto\n")
      (doseq [p ["x\ny.txt" "\"quoted.txt" "crlf.txt"]]
        (spit (io/file repo p) "v1\n"))
      (git! repo "add" ".")
      (doseq [p ["x\ny.txt" "\"quoted.txt" "crlf.txt" "a.txt"]]
        (spit (io/file repo p) "v2\r\n"))
      (testing "every changed path is hashed, with its clean filters"
        (let [m (g/worktree-swhids repo)]
          (doseq [p ["x\ny.txt" "\"quoted.txt" "crlf.txt" "a.txt"]]
            (is (= (h/content-swhid "v2\n") (m p)) p))))
      (finally
        (delete-recursively repo)))))

(deftest sha256-repo-test
  (let [dir (.toFile (Files/createTempDirectory "swhid-git-test"
                                                (into-array FileAttribute [])))]
    (try
      (when (zero? (:exit (sh/sh "git" "-C" (str dir) "init" "-q"
                                 "--object-format=sha256" ".")))
        (doseq [f [g/snapshot-swhid g/worktree-swhids g/revision-swhid]]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"SHA-1 repository"
                                (f dir)))))
      (finally
        (delete-recursively dir)))))

(deftest rev-parse-and-blob-test
  (let [repo (make-repo)]
    (try
      (is (= (rev repo "HEAD") (#'g/rev-parse repo "HEAD^{commit}")))
      (is (= (rev repo "HEAD:a.txt") (:hash (g/blob-swhid repo "a.txt"))))
      (is (nil? (#'g/rev-parse repo "HEAD:nope.txt")))
      (testing "clean filters apply, as the archive would see the file"
        (spit (io/file repo ".gitattributes") "* text=auto\n")
        (spit (io/file repo "crlf.txt") "a\r\nb\r\n")
        (is (= "swh:1:cnt:422c2b7ab3b3c668038da977e4e93a5fc623169c"
               (swhid/swhid->string (h/content-swhid "a\nb\n")))
            "the LF form of the content")
        (is (= (h/content-swhid "a\nb\n") (g/blob-swhid repo "crlf.txt")))
        (is (not= (h/path-swhid (io/file repo "crlf.txt")) (g/blob-swhid repo "crlf.txt"))))
      (finally
        (delete-recursively repo)))))

(deftest worktree-submodule-test
  (let [sub (make-repo)
        repo (make-repo)]
    (try
      (git! repo "-c" "protocol.file.allow=always" "submodule" "add" "-q" (str sub) "lib")
      (git! repo "commit" "-q" "-m" "submodule")
      (testing "a submodule is a revision entry and the tree matches HEAD"
        (let [m (g/worktree-swhids repo)]
          (is (= (swhid/make :directory (rev repo "HEAD^{tree}")) (m ".")))
          (is (= (swhid/make :revision (rev sub "HEAD")) (m "lib")))))
      (testing "a submodule checkout that moved counts with its new HEAD"
        (let [lib (io/file repo "lib")]
          (git! lib "commit" "-q" "--allow-empty" "-m" "two")
          (let [m (g/worktree-swhids repo)]
            (is (= (swhid/make :revision (rev lib "HEAD")) (m "lib")))
            (is (not= (rev sub "HEAD") (rev lib "HEAD")))
            (is (not= (swhid/make :directory (rev repo "HEAD^{tree}")) (m "."))))))
      (testing "a submodule whose checkout was deleted leaves the tree"
        (delete-recursively (io/file repo "lib"))
        (let [m (g/worktree-swhids repo)]
          (is (not (contains? m "lib")))
          (is (= (h/path-swhid repo no-junk) (m ".")))))
      (finally
        (delete-recursively sub)
        (delete-recursively repo)))))
