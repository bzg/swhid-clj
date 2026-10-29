(ns swhid.hash
  "Compute SWHIDs from local artifacts: content (cnt) and directory (dir)
  from files, directories or explicit tree entries, snapshot (snp) from a
  branches map.

  Hashing follows the SWHID v1.2 specification and is Git-compatible: a
  content SWHID is the Git blob hash of the data, a directory SWHID the
  Git tree hash of the directory. Only JDK classes are used; runs on the
  JVM and Babashka."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [swhid.core :as core])
  (:import (java.io ByteArrayOutputStream File FileInputStream InputStream)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute PosixFilePermission)
           (java.security MessageDigest)))

(def ^:private hex-digits "0123456789abcdef")

(defn- hex
  "Lowercase hexadecimal of `bs`."
  [^bytes bs]
  (let [sb (StringBuilder. (* 2 (alength bs)))]
    (doseq [b bs]
      (let [u (bit-and b 0xff)]
        (.append sb (.charAt hex-digits (bit-shift-right u 4)))
        (.append sb (.charAt hex-digits (bit-and u 0xf)))))
    (str sb)))

(defn- object-header ^bytes [obj-type len]
  (.getBytes (str obj-type " " len "\u0000") "UTF-8"))

(defn- blob-digest ^bytes [^bytes data]
  (let [md (MessageDigest/getInstance "SHA-1")]
    (.update md (object-header "blob" (alength data)))
    (.update md data)
    (.digest md)))

(defn- file-blob-digest
  "Blob digest of `f`, streamed. The header carries the length read
  beforehand: if the file changes size meanwhile, the digest would be
  of no object at all, so throw instead."
  ^bytes [^File f]
  (let [md (MessageDigest/getInstance "SHA-1")
        buf (byte-array 8192)
        len (.length f)]
    (.update md (object-header "blob" len))
    (with-open [^InputStream in (FileInputStream. f)]
      (loop [total 0]
        (let [n (.read in buf)]
          (if (pos? n)
            (do (.update md buf 0 n)
                (recur (+ total n)))
            (when-not (= total len)
              (throw (ex-info "File changed while being hashed"
                              {:path (str f) :expected len :read total})))))))
    (.digest md)))

(defn- symlink-blob-digest ^bytes [^File f]
  (blob-digest (.getBytes (str (Files/readSymbolicLink (.toPath f))) "UTF-8")))

;; Entries are sorted as Git sorts trees: byte order of the UTF-8 name,
;; with "/" appended to directory names for comparison only.
(defn- dir-sort-key ^bytes [^bytes name-bytes]
  (let [n (alength name-bytes)
        out (byte-array (inc n))]
    (System/arraycopy name-bytes 0 out 0 n)
    (aset-byte out n 47)
    out))

(defn- compare-unsigned
  "Not java.util.Arrays/compareUnsigned: Babashka has no reflection
  entry for it."
  [^bytes a ^bytes b]
  (let [la (alength a) lb (alength b) n (min la lb)]
    (loop [i 0]
      (if (< i n)
        (let [d (- (bit-and (aget a i) 0xff) (bit-and (aget b i) 0xff))]
          (if (zero? d) (recur (inc i)) d))
        (- la lb)))))

(defn- entries-digest
  "Git tree digest of `entries`, each {:mode \"100644\"|\"100755\"|
  \"120000\"|\"40000\"|\"160000\" :name <UTF-8 bytes> :digest <20 bytes>},
  sorted here as Git sorts them."
  ^bytes [entries]
  (let [payload (ByteArrayOutputStream.)]
    (doseq [{:keys [mode name digest]}
            (sort-by (fn [{:keys [mode ^bytes name]}]
                       (if (= "40000" mode) (dir-sort-key name) name))
                     compare-unsigned
                     entries)]
      (.write payload (.getBytes ^String mode "UTF-8"))
      (.write payload 32)
      (.write payload ^bytes name)
      (.write payload 0)
      (.write payload ^bytes digest))
    (let [bs (.toByteArray payload)
          md (MessageDigest/getInstance "SHA-1")]
      (.update md (object-header "tree" (alength bs)))
      (.update md bs)
      (.digest md))))

(defn ^:no-doc executable?
  "True when the owner execute bit of `f` is set, the bit Git reads to
  choose mode 100755. Unlike File.canExecute, this depends neither on
  the current user (root) nor on a noexec mount. Falls back to
  canExecute on file systems without POSIX permissions."
  [^File f]
  (try
    (contains? (Files/getPosixFilePermissions
                (.toPath f) (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
               PosixFilePermission/OWNER_EXECUTE)
    (catch UnsupportedOperationException _ (.canExecute f))))

(defn- check-name!
  "Bytes of a file name that do not decode in the platform encoding come
  back as U+FFFD: such a name cannot be hashed as Git stores it, and the
  file itself may not even be reachable."
  [^File f]
  (when (str/includes? (.getName f) "\uFFFD")
    (throw (ex-info (str "Cannot decode file name; on the JVM, run with a"
                         " UTF-8 locale (e.g. LC_ALL=C.UTF-8)")
                    {:path (str f)
                     :encoding (System/getProperty "sun.jnu.encoding")}))))

(def ^:private mode->type
  {"100644" :content "100755" :content "120000" :content "160000" :revision})

(defn- entry-mode
  "Git mode of a tree entry of `kind` (:symlink, :directory or :file).
  Git serializes the directory mode as \"40000\", without the leading
  zero of the octal notation."
  [kind executable?]
  (case kind
    :symlink "120000"
    :directory "40000"
    :file (if executable? "100755" "100644")))

(defn- fold-tree
  "[digest swhids] of a tree node from its `children`, each {:rel path
  :name <UTF-8 bytes> :mode m :digest <20 bytes> :swhids <map or nil>}
  where :swhids holds the SWHIDs below that child. When `record?`,
  swhids maps the relative path of every child and of everything below
  it to its SWHID; otherwise it is nil."
  [children record?]
  [(entries-digest children)
   (when record?
     (into {}
           (mapcat (fn [{:keys [rel mode digest swhids]}]
                     (cons [rel (core/make (if (= "40000" mode) :directory (mode->type mode))
                                           (hex digest))]
                           swhids)))
           children))])

(defn ^:no-doc file-kind
  "Kind of `f` as Git sees it, nil for those Git leaves out of its trees
  (sockets, fifos, devices)."
  [^File f]
  (cond (Files/isSymbolicLink (.toPath f)) :symlink
        (.isDirectory f) :directory
        (.isFile f) :file))

(defn- walk-tree
  "[digest swhids] of the directory `dir`, as fold-tree, its entries
  keyed under the `rel` prefix."
  [^File dir exclude record? rel]
  (fold-tree
   (for [^File f (or (.listFiles dir)
                     (throw (ex-info "Cannot list directory" {:path (str dir)})))
         :when (not (and exclude (exclude f)))
         :let [_ (check-name! f)
               kind (file-kind f)]
         :when kind
         :let [child-rel (str rel (.getName f))
               [digest swhids] (case kind
                                 :symlink [(symlink-blob-digest f)]
                                 :file [(file-blob-digest f)]
                                 :directory (walk-tree f exclude record?
                                                       (str child-rel "/")))]]
     {:rel child-rel
      :name (.getBytes (.getName f) "UTF-8")
      :mode (entry-mode kind (and (= :file kind) (executable? f)))
      :digest digest
      :swhids swhids})
   record?))

(defn- hex->bytes ^bytes [^String s]
  (let [n (quot (count s) 2)
        out (byte-array n)]
    (dotimes [i n]
      (aset-byte out i
                 (unchecked-byte
                  (Integer/parseInt (subs s (* 2 i) (+ (* 2 i) 2)) 16))))
    out))

(defn snapshot-swhid
  "SWHID map of a snapshot, from a branches map: branch name (string) ->
  {:target-type <kw> :target <value>} where :target-type is :content,
  :directory, :revision, :release, :snapshot (:target is a 40-hex id),
  :alias (:target is a branch name) or :dangling (:target is nil)."
  [branches]
  (let [payload (ByteArrayOutputStream.)]
    (doseq [[name-bytes {:keys [target-type target]}]
            (->> branches
                 (map (fn [[k v]] [(.getBytes ^String k "UTF-8") v]))
                 (sort-by first compare-unsigned))]
      (let [target-bytes (case target-type
                           :dangling (byte-array 0)
                           :alias (if (string? target)
                                    (.getBytes ^String target "UTF-8")
                                    (throw (ex-info "Alias target must be a branch name"
                                                    {:target target})))
                           (:content :directory :revision :release :snapshot)
                           (if (and (string? target) (re-matches #"[0-9a-f]{40}" target))
                             (hex->bytes target)
                             (throw (ex-info "Target must be 40 hex digits"
                                             {:target target})))
                           (throw (ex-info "Unknown target type"
                                           {:target-type target-type})))]
        (.write payload (.getBytes (name target-type) "UTF-8"))
        (.write payload 32)
        (.write payload ^bytes name-bytes)
        (.write payload 0)
        (.write payload (.getBytes (str (alength target-bytes) ":") "UTF-8"))
        (.write payload target-bytes)))
    (let [bs (.toByteArray payload)
          md (MessageDigest/getInstance "SHA-1")]
      (.update md (object-header "snapshot" (alength bs)))
      (.update md bs)
      (core/make :snapshot (hex (.digest md))))))

(defn content-swhid
  "SWHID map of raw content: a byte array, or a string hashed as UTF-8."
  [data]
  (core/make :content
             (hex (blob-digest
                   (if (string? data) (.getBytes ^String data "UTF-8") data)))))

(defn path-swhid
  "SWHID map of a local artifact. A regular file or a symbolic link yields
  a :content SWHID, a directory a :directory SWHID. Inside a directory,
  entries of any other kind (sockets, fifos, devices) are skipped, as Git
  skips them; asked for directly, they are an error.

  opts: {:exclude pred} -- a predicate on java.io.File; matching entries
  (and their subtrees) are omitted from directory hashing, e.g.
  #(= \".git\" (.getName %))."
  ([path] (path-swhid path nil))
  ([path {:keys [exclude]}]
   (let [f (io/file path)]
     (case (file-kind f)
       :symlink (core/make :content (hex (symlink-blob-digest f)))
       :directory (core/make :directory (hex (first (walk-tree f exclude false ""))))
       :file (core/make :content (hex (file-blob-digest f)))
       (throw (ex-info "No such file or unsupported file type"
                       {:path (str f)}))))))

(defn tree-swhids
  "Map of relative path -> SWHID map for the directory at `path` and every
  entry under it, in a single pass. The root directory is keyed by \".\";
  entries use \"/\"-separated paths relative to the root. Files and
  symbolic links map to :content SWHIDs, directories to :directory ones.
  Same opts as path-swhid."
  ([path] (tree-swhids path nil))
  ([path {:keys [exclude]}]
   (let [[digest swhids] (walk-tree (io/file path) exclude true "")]
     (assoc swhids "." (core/make :directory (hex digest))))))

(defn- nest
  "Nested map of `entries` ({relpath entry}): a directory is a map of
  name -> child, a leaf is its entry. Rejects ill-formed paths and a path
  that is both a leaf and a directory."
  [entries]
  (reduce (fn [tree [path entry]]
            (let [segs (str/split path #"/" -1)]
              (when (some #(contains? #{"" "." ".."} %) segs)
                (throw (ex-info "Invalid entry path" {:path path})))
              (when (or (map? (get-in tree segs))
                        (some #(contains? (get-in tree (take % segs)) :mode)
                              (range 1 (count segs))))
                (throw (ex-info "Entry path is both a file and a directory"
                                {:path path})))
              (assoc-in tree segs entry)))
          {}
          entries))

(defn- nested-tree
  "[digest swhids] of the nested `node`, as fold-tree, its entries keyed
  under the `rel` prefix."
  [node rel]
  (fold-tree
   (for [[name child] node
         :let [child-rel (str rel name)
               dir? (not (contains? child :mode))
               [digest swhids] (if dir?
                                 (nested-tree child (str child-rel "/"))
                                 [(hex->bytes (:hash child))])]]
     {:rel child-rel
      :name (.getBytes ^String name "UTF-8")
      :mode (if dir? "40000" (:mode child))
      :digest digest
      :swhids swhids})
   true))

(defn tree-swhids-from-entries
  "Like tree-swhids, but from an explicit list of leaves instead of a
  directory walk: `entries` maps a \"/\"-separated relative path to
  {:mode m :hash h}, with m the Git mode (\"100644\", \"100755\",
  \"120000\" or \"160000\" for a submodule) and h the 40-hex Git hash of
  the blob (or of the submodule's commit). Returns relpath -> SWHID map
  for every leaf and every intermediate directory, the root under \".\"."
  [entries]
  (doseq [[path {:keys [mode hash]}] entries]
    (when-not (and (mode->type mode) (string? hash) (re-matches #"[0-9a-f]{40}" hash))
      (throw (ex-info "Invalid entry" {:path path :mode mode :hash hash}))))
  (let [[digest swhids] (nested-tree (nest entries) "")]
    (assoc swhids "." (core/make :directory (hex digest)))))
