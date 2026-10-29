(ns swhid.qualifier
  "Parsing, rendering and validation of SWHID qualifiers.

  A qualifiers map uses native values:
    {:origin \"https://example.org/repo\"   ; decoded string
     :visit  \"swh:1:snp:...\"               ; a core SWHID string
     :anchor \"swh:1:rev:...\"               ; a core SWHID string
     :path   \"/src/main.clj\"               ; decoded string
     :lines  [1 18]                          ; [from to], to may be nil
     :bytes  [4 20]}                         ; [from to], to may be nil

  Percent-encoding is applied only at render time, as swh.model does:
  `origin` escapes `%`, `;` and whitespace or control characters, `path`
  everything but ASCII letters, digits and `_.-~/`. Parsing returns
  decoded values."
  (:require [clojure.string :as str])
  (:import (java.nio ByteBuffer)
           (java.nio.charset CharacterCodingException CodingErrorAction
                             StandardCharsets)))

;; A core SWHID, used to validate the `visit` and `anchor` qualifiers.
;; Kept local to avoid a dependency cycle with swhid.core.
(def ^:private core-swhid-re
  #"swh:1:(?:cnt|dir|rev|rel|snp):[0-9a-f]{40}")

;; Canonical ordering: context qualifiers first, then fragment qualifiers.
(def ^:private qualifier-order [:origin :visit :anchor :path :lines :bytes])

(def ^:private known-keys (set qualifier-order))

(defn- hex-digit? [c]
  (or (<= (int \0) (int c) (int \9))
      (<= (int \a) (int c) (int \f))
      (<= (int \A) (int c) (int \F))))

(defn- utf-8-decode
  "Decode `bs` as UTF-8, throwing on invalid input instead of inserting
  U+FFFD."
  ^String [^bytes bs s]
  (try
    (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))
                  (ByteBuffer/wrap bs)))
    (catch CharacterCodingException _
      (throw (ex-info "Percent-encoding is not valid UTF-8" {:input s})))))

(defn- percent-decode
  "Decode %XX sequences (UTF-8 byte level). Throws on a malformed escape
  or on bytes that are not valid UTF-8."
  [s]
  (let [n (count s)
        out (java.io.ByteArrayOutputStream.)]
    (loop [i 0]
      (if (< i n)
        (let [c (.charAt ^String s i)]
          (if (= c \%)
            (if (and (< (+ i 2) n)
                     (hex-digit? (.charAt ^String s (+ i 1)))
                     (hex-digit? (.charAt ^String s (+ i 2))))
              (do (.write out (Integer/parseInt (subs s (+ i 1) (+ i 3)) 16))
                  (recur (+ i 3)))
              (throw (ex-info "Malformed percent-encoding" {:input s :index i})))
            ;; Iterate by code point: chars outside the BMP are surrogate
            ;; pairs, and encoding a lone surrogate would yield "?".
            (let [cp (.codePointAt ^String s i)
                  bs (.getBytes (String. (Character/toChars cp)) "UTF-8")]
              (.write out bs 0 (alength bs))
              (recur (+ i (Character/charCount cp))))))
        (utf-8-decode (.toByteArray out) s)))))

(defn- encode-with
  "Percent-encode, as UTF-8 bytes, every code point of `s` not `safe?`."
  [^String s safe?]
  (let [sb (StringBuilder.)]
    (loop [i 0]
      (if (< i (count s))
        (let [cp (.codePointAt s i)
              ch (String. (Character/toChars cp))]
          (if (safe? cp)
            (.append sb ch)
            (doseq [b (.getBytes ch "UTF-8")]
              (.append sb (format "%%%02X" (bit-and b 0xff)))))
          (recur (+ i (Character/charCount cp))))
        (str sb)))))

(defn- percent-encode
  "Encode the characters that must be escaped in a qualifier value: `%`,
  `;`, and whitespace or control characters, which cannot appear raw in
  a SWHID."
  [s]
  (encode-with s (fn [cp]
                   (not (or (= cp (int \%)) (= cp (int \;))
                            (Character/isWhitespace (int cp))
                            (Character/isSpaceChar (int cp))
                            (Character/isISOControl (int cp)))))))

(defn- path-encode
  "Encode a path as swh.model does (urllib.parse.quote_from_bytes): only
  ASCII letters, digits, `_.-~` and `/` are left as is."
  [s]
  (encode-with s (fn [cp]
                   (or (<= (int \a) cp (int \z))
                       (<= (int \A) cp (int \Z))
                       (<= (int \0) cp (int \9))
                       (contains? #{(int \_) (int \.) (int \-) (int \~) (int \/)} cp)))))

(defn- range-start-ok?
  "Lines are numbered from 1, bytes from 0."
  [k from]
  (if (= :lines k) (pos-int? from) (nat-int? from)))

(defn- check-range!
  "A range is [from to?] with to >= from, from >= 1 for lines, >= 0 for bytes."
  [k v]
  (when-not (and (vector? v)
                 (<= 1 (count v) 2)
                 (int? (first v))
                 (range-start-ok? k (first v))
                 (or (nil? (second v))
                     (and (int? (second v)) (<= (first v) (second v)))))
    (throw (ex-info "Range qualifier must be [from to?], to >= from, from >= 1 for lines"
                    {:key k :value v})))
  v)

(defn- parse-range [k v]
  (if-let [[_ a b] (re-matches #"(\d+)(?:-(\d+))?" v)]
    (let [from (parse-long a)
          to (some-> b parse-long)]
      ;; parse-long returns nil past Long/MAX_VALUE
      (when (or (nil? from) (and b (nil? to)))
        (throw (ex-info "Range out of bounds" {:key k :value v})))
      (check-range! k [from to]))
    (throw (ex-info "Invalid range qualifier" {:key k :value v}))))

(defn- render-range [[a b]]
  (if b (str a "-" b) (str a)))

(defn- core-swhid? [v]
  (some? (re-matches core-swhid-re v)))

(defn- check-path!
  "A path is an RFC 3987 ipath-absolute: it starts with a single `/`."
  [v]
  (when-not (re-matches #"(?s)/(?:[^/].*)?" v)
    (throw (ex-info "Path qualifier must be absolute (start with a single /)"
                    {:key :path :value v})))
  v)

(defn- check-origin!
  "Throw ex-info on a blank origin: it would say nothing, yet keep a
  visit that an absent origin drops (see drop-orphans)."
  [v]
  (if (str/blank? v)
    (throw (ex-info "Origin must not be blank" {:key :origin :value v}))
    v))

(defn- parse-value [k v]
  (case k
    :origin (check-origin! (percent-decode v))
    :path (check-path! (percent-decode v))
    (:visit :anchor) (if (core-swhid? v)
                       v
                       (throw (ex-info "Qualifier must be a core SWHID"
                                       {:key k :value v})))
    (:lines :bytes) (parse-range k v)))

(defn ^:no-doc parse
  "Parse the qualifier substring (everything after the first `;`) into a
  qualifiers map. Unknown qualifiers are left out, as SWHID 1.2 (6.1)
  says to ignore invalid ones. Throws ex-info on any malformed qualifier,
  or on raw whitespace (it must be percent-encoded)."
  [s]
  (when (re-find #"(?U)\s" s)
    (throw (ex-info "Unescaped whitespace in qualifiers" {:input s})))
  (reduce
   (fn [acc seg]
     (when (str/blank? seg)
       (throw (ex-info "Empty qualifier segment" {:input s})))
     (let [eq (str/index-of seg "=")]
       (when-not eq
         (throw (ex-info "Qualifier missing '='" {:segment seg})))
       (let [k (keyword (subs seg 0 eq))
             v (subs seg (inc eq))]
         (cond
           (not (known-keys k)) acc
           (contains? acc k) (throw (ex-info "Duplicate qualifier" {:key k}))
           :else (assoc acc k (parse-value k v))))))
   {}
   (str/split s #";" -1)))

(defn- render-value [k v]
  (case k
    :origin (percent-encode v)
    :path (path-encode v)
    (:visit :anchor) v
    (:lines :bytes) (render-range v)))

(defn ^:no-doc render
  "Render a qualifiers map into its canonical string form (without leading
  `;`). Trusts the map (see validate!); unknown keys are left out."
  [qualifiers]
  (->> qualifier-order
       (keep (fn [k]
               (when (contains? qualifiers k)
                 (str (name k) "=" (render-value k (get qualifiers k))))))
       (str/join ";")))

(defn ^:no-doc validate!
  "Throw ex-info if the qualifiers map is not well-formed; return it otherwise.
  Validation is syntactic only: see check-target! for the rules that
  depend on the object type and on the qualified objects."
  [qualifiers]
  (when-not (map? qualifiers)
    (throw (ex-info "Qualifiers must be a map" {:qualifiers qualifiers})))
  (doseq [[k v] qualifiers]
    (case k
      (:origin :path) (if (string? v)
                        (if (= :path k) (check-path! v) (check-origin! v))
                        (throw (ex-info "Qualifier value must be a string" {:key k :value v})))
      (:visit :anchor) (when-not (and (string? v) (core-swhid? v))
                         (throw (ex-info "Qualifier must be a core SWHID" {:key k :value v})))
      (:lines :bytes) (check-range! k v)
      (throw (ex-info "Unknown qualifier" {:key k}))))
  qualifiers)

;; Rules of SWHID 1.2, clause 6: "A conformant implementation shall not
;; generate invalid qualifiers or qualifier combinations, and shall ignore
;; them if present". Not applied: "[path] is only valid when the object
;; type is not content", which the spec's own examples (a cnt with a
;; path) and swh.model contradict.

(defn- tag [core-swhid] (subs core-swhid 6 9))

(defn- drop-invalid
  "`qualifiers` without those that cannot be valid on an object of `type`."
  [type {:keys [visit anchor bytes] :as qualifiers}]
  (cond-> qualifiers
    ;; 6.2.1: fragments are for contents, lines is ignored next to bytes
    (not= :content type) (dissoc :lines :bytes)
    bytes (dissoc :lines)
    ;; 6.3.3, 6.3.5: a snapshot; a directory, revision, release or snapshot
    (and visit (not= "snp" (tag visit))) (dissoc :visit)
    (and anchor (= "cnt" (tag anchor))) (dissoc :anchor)))

(defn- drop-orphans
  "`qualifiers` without visit when origin is absent, nor anchor when path
  is (6.3.3, 6.3.5)."
  [qualifiers]
  (cond-> qualifiers
    (not (:origin qualifiers)) (dissoc :visit)
    (not (:path qualifiers)) (dissoc :anchor)))

(defn ^:no-doc effective
  "The qualifiers of an object of `type` that the spec does not ignore,
  nil when none is left."
  [type qualifiers]
  (not-empty (drop-orphans (drop-invalid type qualifiers))))

(defn ^:no-doc check-target!
  "Throw ex-info if some `qualifiers` can never be valid on an object of
  `type`; return them otherwise. visit without origin and anchor without
  path are allowed: the missing qualifier can be added later."
  [type qualifiers]
  (let [kept (drop-invalid type qualifiers)]
    (when (not= kept qualifiers)
      (throw (ex-info "Qualifiers not valid on this SWHID"
                      {:type type
                       :invalid (apply dissoc qualifiers (keys kept))}))))
  qualifiers)

;; Accessors taking a SWHID map.
(defn qualifiers [swhid] (:qualifiers swhid))
(defn origin [swhid] (get-in swhid [:qualifiers :origin]))
(defn visit [swhid] (get-in swhid [:qualifiers :visit]))
(defn anchor [swhid] (get-in swhid [:qualifiers :anchor]))
(defn path [swhid] (get-in swhid [:qualifiers :path]))
(defn line-range [swhid] (get-in swhid [:qualifiers :lines]))
(defn byte-range [swhid] (get-in swhid [:qualifiers :bytes]))
