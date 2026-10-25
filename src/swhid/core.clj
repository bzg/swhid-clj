(ns swhid.core
  "Parse, validate, build and render SWHIDs (Software Hash Identifiers, ISO 18670).

  A SWHID is represented as a plain map:
    {:type :content :hash \"<40 hex>\"
     :qualifiers {...}}   ; :qualifiers present only when non-empty
  `pr` prints it as the tagged literal #swh/id \"swh:1:...\", which reads
  it back (see data_readers.clj).

  This namespace covers *manipulation* of SWHIDs (parse / validate / render /
  compare). Hash computation from local artifacts lives in swhid.hash.

  Both the JVM and Babashka are supported, with no dependency."
  (:require [clojure.string :as str]
            [swhid.qualifier :as q]))

(def ^:private tag->type
  {"cnt" :content
   "dir" :directory
   "rev" :revision
   "rel" :release
   "snp" :snapshot})

(def ^:private type->tag
  {:content "cnt"
   :directory "dir"
   :revision "rev"
   :release "rel"
   :snapshot "snp"})

(def ^:private core-re
  #"swh:1:(cnt|dir|rev|rel|snp):([0-9a-f]{40})")

(def ^:private hash-re #"[0-9a-f]{40}")

(defn- swhid-map
  "The SWHID map of validated parts, tagged with the :type metadata that
  selects its print-method."
  [type hash qualifiers]
  (with-meta (cond-> {:type type :hash hash}
               (seq qualifiers) (assoc :qualifiers qualifiers))
    {:type ::swhid}))

(defn parse
  "Parse a SWHID string into a SWHID map. Throws ex-info if invalid.
  Qualifiers that the spec says to ignore (lines on a directory, visit
  without origin, …) are left out."
  [s]
  (when-not (string? s)
    (throw (ex-info "SWHID must be a string" {:input s})))
  (let [idx (str/index-of s ";")
        core-str (if idx (subs s 0 idx) s)
        qual-str (when idx (subs s (inc idx)))
        m (re-matches core-re core-str)]
    (when-not m
      (throw (ex-info "Invalid core SWHID" {:input s :core core-str})))
    (let [[_ tag hash] m]
      (swhid-map (tag->type tag) hash
                 (some->> qual-str q/parse (q/effective (tag->type tag)))))))

(defn valid?
  "Return true if `s` is a syntactically valid SWHID string."
  [s]
  (try
    (parse s)
    true
    (catch Exception _ false)))

(defn make
  "Build a SWHID map from an object type keyword, a 40-hex object id, and an
  optional qualifiers map. Validates its arguments, and throws on
  qualifiers that cannot be valid on this type (lines on a directory, …)."
  ([type hash] (make type hash nil))
  ([type hash qualifiers]
   (when-not (type->tag type)
     (throw (ex-info "Unknown object type" {:type type})))
   (when-not (and (string? hash) (re-matches hash-re hash))
     (throw (ex-info "Object id must be 40 lowercase hex digits" {:hash hash})))
   (when (some? qualifiers)
     (q/check-target! type (q/validate! qualifiers)))
   (swhid-map type hash qualifiers)))

(defn swhid->string
  "Render a SWHID map to its canonical string form, leaving out the
  qualifiers the spec says to ignore (visit without origin, …). Trusts
  :hash and :qualifiers: build hand-made maps with `make`, which
  validates them."
  [{:keys [type hash qualifiers]}]
  (let [tag (or (type->tag type)
                (throw (ex-info "Unknown object type" {:type type})))
        base (str "swh:1:" tag ":" hash)]
    (if-let [qualifiers (q/effective type qualifiers)]
      (str base ";" (q/render qualifiers))
      base)))

;; The :type metadata survives assoc, dissoc and update, so the maps
;; built by this library keep printing as #swh/id; into {} or
;; select-keys drop it and the map prints as a map. A map that does not
;; render (e.g. an unknown :type) also prints as a map: printing never
;; throws.
(defmethod print-method ::swhid [m ^java.io.Writer w]
  (if-let [s (try (swhid->string m) (catch Exception _ nil))]
    (.write w (str "#swh/id " (pr-str s)))
    (print-method (vary-meta m dissoc :type) w)))

;; Accessors

(defn object-type [swhid] (:type swhid))
(defn object-id [swhid] (:hash swhid))

(defn core-swhid
  "Return the SWHID with its qualifiers removed (the bare core identifier)."
  [swhid]
  (dissoc swhid :qualifiers))

;; Predicates

(defn content? [swhid] (= :content (:type swhid)))
(defn directory? [swhid] (= :directory (:type swhid)))
(defn revision? [swhid] (= :revision (:type swhid)))
(defn release? [swhid] (= :release (:type swhid)))
(defn snapshot? [swhid] (= :snapshot (:type swhid)))

;; Qualifier algebra: build qualified SWHIDs by composition.

(defn with-qualifier
  "Return `swhid` with qualifier `k` set to `v`. Validates the result."
  [swhid k v]
  (let [qualifiers (assoc (:qualifiers swhid) k v)]
    (q/check-target! (:type swhid) (q/validate! qualifiers))
    (assoc swhid :qualifiers qualifiers)))

(defn without-qualifier
  "Return `swhid` with qualifier `k` removed."
  [swhid k]
  (let [qualifiers (not-empty (dissoc (:qualifiers swhid) k))]
    (if qualifiers
      (assoc swhid :qualifiers qualifiers)
      (dissoc swhid :qualifiers))))

(defn- core-string
  "A map is rendered without its qualifiers; a string is taken as is
  (validate! then rejects a qualified one)."
  [x]
  (if (map? x) (swhid->string (core-swhid x)) x))

(defn with-origin [swhid url] (with-qualifier swhid :origin url))
(defn with-path [swhid path] (with-qualifier swhid :path path))

(defn with-visit
  "`snp` is a snapshot SWHID map or core SWHID string."
  [swhid snp]
  (with-qualifier swhid :visit (core-string snp)))

(defn with-anchor
  "`anchor` is a SWHID map or core SWHID string."
  [swhid anchor]
  (with-qualifier swhid :anchor (core-string anchor)))

(defn with-lines
  ([swhid from] (with-qualifier swhid :lines [from nil]))
  ([swhid from to] (with-qualifier swhid :lines [from to])))

(defn with-bytes
  ([swhid from] (with-qualifier swhid :bytes [from nil]))
  ([swhid from to] (with-qualifier swhid :bytes [from to])))

;; Tagged literal: #swh/id "swh:1:cnt:…" reads as a SWHID map (see
;; data_readers.clj). For clojure.edn, pass {:readers edn-readers}.
(def edn-readers {'swh/id parse})
