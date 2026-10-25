(ns swhid.git
  "SWHIDs from a local Git repository: rev / rel / snp, and the dir / cnt
  SWHIDs of its tracked working tree.

  Shells out to the `git` binary to read refs, object types and the
  index; the hashing itself is done by swhid.hash.

  The snapshot covers all refs of the repository (including remotes),
  plus HEAD and other symbolic refs as aliases, matching what
  `swh identify` (swh.model) computes for a local repository."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [swhid.core :as core]
            [swhid.hash :as h]))

(defn- git [repo & args]
  (let [{:keys [exit out err]} (apply sh/sh "git" "-C" (str repo) args)]
    (when-not (zero? exit)
      (throw (ex-info "git command failed"
                      {:repo (str repo) :args (vec args) :err err})))
    out))

(defn- check-object-format!
  "SWHIDs are SHA-1 based: refuse a SHA-256 repository up front rather
  than failing later on a 64-hex object id."
  [repo]
  (let [{:keys [exit out]} (sh/sh "git" "-C" (str repo) "config" "--get"
                                  "extensions.objectFormat")
        fmt (str/lower-case (str/trim out))]
    (when (and (zero? exit) (not= "sha1" fmt))
      (throw (ex-info "Unsupported object format: SWHIDs need a SHA-1 repository"
                      {:repo (str repo) :object-format fmt})))))

(defn- parse-config-flags
  "core.filemode and core.symlinks, both true by default, from the
  output of `git config --bool --get-regexp`. Later lines (local config)
  override earlier ones (system)."
  [out]
  (let [m (into {}
                (comp (remove str/blank?) (map #(str/split % #" " 2)))
                (str/split-lines out))]
    {:filemode? (not= "false" (m "core.filemode"))
     :symlinks? (not= "false" (m "core.symlinks"))}))

(defn- config-flags
  "Effective core.filemode and core.symlinks of `repo`. When false, Git
  keeps the index's mode rather than trust the executable bit or the
  file type found on disk."
  [repo]
  (parse-config-flags
   (:out (sh/sh "git" "-C" (str repo) "config" "--bool"
                "--get-regexp" "^core\\.(filemode|symlinks)$"))))

(def ^:private git-type->swh
  {"blob" :content "tree" :directory "commit" :revision "tag" :release})

(defn- parse-object-types
  "Map of id -> :content/:directory/:revision/:release/:dangling, from
  the output of `git cat-file --batch-check=%(objecttype)` for `ids`."
  [ids out]
  (zipmap ids
          (map (fn [line]
                 (or (git-type->swh line)
                     (when (str/ends-with? line " missing") :dangling)
                     (throw (ex-info "Unexpected object type" {:line line}))))
               (str/split-lines out))))

(defn- object-types
  "Map of object id -> its type in `repo`, as parse-object-types."
  [repo ids]
  (if (empty? ids)
    {}
    (parse-object-types ids (git repo "cat-file" "--batch-check=%(objecttype)"
                                 :in (str (str/join "\n" ids) "\n")))))

(defn- parse-refs
  "[id refname symref] records from the output of `git for-each-ref
  --format=%(objectname)%09%(refname)%09%(symref)`; symref is blank
  unless the ref is symbolic."
  [out]
  (->> (str/split-lines out)
       (remove str/blank?)
       (map #(let [[id refname symref] (str/split % #"\t" 3)]
               [id refname (str symref)]))))

(defn- refs->branches
  "Branches map for swhid.hash/snapshot-swhid from parse-refs records,
  the `types` of their targets (as parse-object-types) and the `head`
  branch."
  [refs types head]
  (into {"HEAD" head}
        (map (fn [[id refname symref]]
               [refname
                (if-not (str/blank? symref)
                  {:target-type :alias :target symref}
                  (let [t (types id)]
                    {:target-type t
                     :target (when-not (= :dangling t) id)}))]))
        refs))

(defn- head-branch
  "The HEAD branch of `repo`."
  [repo]
  (let [{:keys [exit out]} (sh/sh "git" "-C" (str repo) "symbolic-ref" "-q" "HEAD")]
    (if (zero? exit)
      ;; On an unborn HEAD (fresh `git init`), this yields an alias to a
      ;; branch absent from the snapshot; SWH snapshots do contain such
      ;; dangling aliases.
      {:target-type :alias :target (str/trim out)}
      ;; Detached HEAD points directly at a commit.
      {:target-type :revision
       :target (str/trim (git repo "rev-parse" "HEAD"))})))

(defn- branches
  "Branches map of `repo` for swhid.hash/snapshot-swhid."
  [repo]
  (let [refs (parse-refs (git repo "for-each-ref"
                              "--format=%(objectname)%09%(refname)%09%(symref)"))
        direct-ids (->> refs (filter #(str/blank? (nth % 2))) (map first) distinct)]
    (refs->branches refs (object-types repo direct-ids) (head-branch repo))))

(defn snapshot-swhid
  "SWHID map of the snapshot of the local Git repository at `repo-path`."
  [repo-path]
  (check-object-format! repo-path)
  (h/snapshot-swhid (branches repo-path)))

(defn revision-swhid
  "SWHID map of a revision of the local Git repository at `repo-path`.
  `rev` is anything git rev-parse accepts (defaults to \"HEAD\")."
  ([repo-path] (revision-swhid repo-path "HEAD"))
  ([repo-path rev]
   (check-object-format! repo-path)
   (core/make :revision
              (str/trim (git repo-path "rev-parse" "--verify"
                             (str rev "^{commit}"))))))

(defn release-swhid
  "SWHID map of a release (annotated tag) `tag` of the local Git
  repository at `repo-path`."
  [repo-path tag]
  (check-object-format! repo-path)
  (core/make :release
             (str/trim (git repo-path "rev-parse" "--verify"
                            (str tag "^{tag}")))))

(defn- rev-parse
  "The object id `spec` resolves to in the repository at `repo-path`
  (anything `git rev-parse --verify` accepts, e.g. \"HEAD:src/a.clj\");
  nil when it does not resolve."
  [repo-path spec]
  (try (str/trim (git repo-path "rev-parse" "--verify" "-q" spec))
       (catch clojure.lang.ExceptionInfo _ nil)))

(defn blob-swhid
  "SWHID map of the file at `path` (relative) in the repository at
  `repo-path`, hashed as `git add` would store it: its clean filters and
  text conversion apply, so this is what the archive would hold."
  [repo-path path]
  (check-object-format! repo-path)
  (core/make :content
             (str/trim (git repo-path "hash-object" "--path" path "--" path))))

(defn- git-z
  "Records of a NUL-separated git output (a record may be a blank path)."
  [repo & args]
  (remove empty? (str/split (apply git repo args) #"\u0000")))

(defn- stdin-path?
  "Whether `git hash-object --stdin-paths` reads `path` back unchanged: it
  splits lines, drops a trailing CR and unquotes a leading double quote."
  [path]
  (not (or (str/includes? path "\n")
           (str/includes? path "\r")
           (str/starts-with? path "\""))))

(defn- hash-objects
  "Map of path -> blob hash for `paths` (relative to `repo`), hashed as
  `git add` would store them, in a single git call; a path that
  --stdin-paths cannot carry gets a call of its own."
  [repo paths]
  (let [{batch true single false} (group-by stdin-path? paths)]
    (merge (when (seq batch)
             (zipmap batch
                     (str/split-lines
                      (git repo "hash-object" "--stdin-paths"
                           :in (str (str/join "\n" batch) "\n")))))
           (into {}
                 (map (fn [p] [p (str/trim (git repo "hash-object" "--path" p "--" p))]))
                 single))))

(defn- parse-index
  "Map of path -> {:mode m :hash h} from the records of `git ls-files -z
  --stage`, \"<mode> <hash> <stage>\\t<path>\". Unmerged entries (stage
  other than 0) are left out."
  [records]
  (into {}
        (keep (fn [record]
                (let [[meta path] (str/split record #"\t" 2)
                      [mode hash stage] (str/split meta #" ")]
                  (when (= "0" stage)
                    [path {:mode mode :hash hash}]))))
        records))

(defn- observe
  "What the working tree of `root` holds at `path`: {:kind k} with k
  :symlink (and its :hash), :file (and :executable?), :directory (and,
  for a submodule, the :hash of its HEAD, if any) or nil when missing
  or of a kind Git leaves out."
  [^java.io.File root path index-mode]
  (let [f (io/file root path)]
    (cond
      (java.nio.file.Files/isSymbolicLink (.toPath f))
      {:kind :symlink :hash (:hash (h/path-swhid f))}
      (.isFile f)
      {:kind :file :executable? (h/executable? f)}
      (.isDirectory f)
      (cond-> {:kind :directory}
        (= "160000" index-mode) (assoc :hash (rev-parse f "HEAD"))))))

(defn- worktree-entry
  "The entry `git add` would record for a path whose index entry is
  `indexed` (nil when unmerged) and whose working tree holds `observed`
  (as observe), under the `flags` of config-flags; nil to leave it out.
  A regular file gets no :hash: its blob goes through clean filters."
  [{:keys [filemode? symlinks?]} indexed {:keys [kind executable? hash]}]
  (let [mode (:mode indexed)]
    (case kind
      :symlink {:mode "120000" :hash hash}
      :file {:mode (cond
                     ;; a symlink checked out as a plain file
                     (and (not symlinks?) (= "120000" mode)) mode
                     (not filemode?) (if (#{"100644" "100755"} mode) mode "100644")
                     executable? "100755"
                     :else "100644")}
      ;; a submodule counts with the HEAD of its checkout, or keeps its
      ;; commit when that cannot be read; any other directory replaces
      ;; the tracked file
      :directory (when (= "160000" mode)
                   (if hash {:mode mode :hash hash} indexed))
      nil)))

(defn- apply-changes
  "`indexed` updated with the `entries` of changed paths (nil when
  deleted), regular files taking their blob hash from `hashes`."
  [indexed entries hashes]
  (reduce-kv (fn [acc p e]
               (if e
                 (assoc acc p (update e :hash #(or % (hashes p))))
                 (dissoc acc p)))
             indexed
             entries))

(defn worktree-swhids
  "Map of relative path -> SWHID map for the tracked working tree of the
  Git repository at `repo-path`, the root under \".\": every file in the
  index, no untracked or ignored file, and no \".git\". Files modified in
  the working tree are hashed as `git add` would store them (clean
  filters, autocrlf, core.fileMode, core.symlinks), deleted ones are
  left out, a submodule counts with the HEAD of its checkout. Directory
  SWHIDs are those the archive computes from a commit of this tree;
  submodules are :revision entries, as in the archive."
  [repo-path]
  (check-object-format! repo-path)
  (let [root (io/file (str repo-path))
        flags (config-flags root)
        indexed (parse-index (git-z root "ls-files" "-z" "--stage"))
        ;; paths whose working tree differs from the index
        changed (distinct (git-z root "diff-files" "-z" "--name-only"))
        entries (into {}
                      (map (fn [p]
                             (let [i (indexed p)]
                               [p (worktree-entry flags i (observe root p (:mode i)))])))
                      changed)
        hashes (hash-objects root (keep (fn [[p e]] (when (and e (not (:hash e))) p))
                                        entries))]
    (h/tree-swhids-from-entries (apply-changes indexed entries hashes))))
