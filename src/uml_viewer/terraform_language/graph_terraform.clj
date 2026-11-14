(ns uml-viewer.terraform-language.graph-terraform
  "Terraform LanguageGraph: one class per `.tf` file, directories are the tree.
  A file depends on another when it references something the other declares
  (a resource, data source, module or local). Declarations are the class
  `:ops`. A resource type `aws_ecs_service` also links its file to the
  foreign `aws.ecs`. `var.*` references are omitted unless the source entry
  sets `:vars true`, because `variables.tf` would otherwise touch everything."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.graph :as graph]))

;; ---------------------------------------------------------------- files

(defn- skip-dir? [name]
  (or (str/starts-with? name ".") (= name "node_modules")))

(defn- tf-files [root]
  (let [root (.getCanonicalFile (io/file root))]
    (->> (tree-seq (fn [f] (and (.isDirectory f)
                                (or (= f root) (not (skip-dir? (.getName f))))))
                   (fn [d] (vec (.listFiles d)))
                   root)
         (filter #(and (.isFile ^java.io.File %) (str/ends-with? (.getName ^java.io.File %) ".tf")))
         (sort-by #(.getPath ^java.io.File %)))))

(defn- module-relative [root file]
  (let [rel (str (.relativize (.toPath (.getCanonicalFile (io/file root)))
                              (.toPath (.getCanonicalFile ^java.io.File file))))]
    (-> rel
        (str/replace #"\\" "/")
        (str/replace #"\.tf$" "")
        (str/replace "/" "."))))

;; ---------------------------------------------------------------- masking

(defn mask-comments
  "`source` without `#`, `//` and block comments. Strings are kept, because
  references live inside `${...}`."
  [^String source]
  (let [n (count source)
        sb (StringBuilder.)]
    (loop [i 0 in-string? false]
      (when (< i n)
        (let [c (.charAt source i)
              nx (graph/next-char source i)]
          (cond
            in-string?
            (cond
              (= c \\) (do (.append sb c)
                           (when nx (.append sb nx))
                           (recur (+ i 2) true))
              (= c \") (do (.append sb c) (recur (inc i) false))
              :else (do (.append sb c) (recur (inc i) true)))

            (= c \") (do (.append sb c) (recur (inc i) true))

            (or (= c \#) (graph/line-comment? c nx))
            (recur (let [j (or (str/index-of source "\n" i) n)] j) false)

            (graph/block-comment? c nx)
            (recur (graph/mask-block-comment source sb i n) false)

            :else (do (.append sb c) (recur (inc i) false))))))
    (str sb)))

;; ---------------------------------------------------------------- declarations

(def ^:private block-re
  #"(?m)^\s*(resource|data)\s+\"([^\"]+)\"\s+\"([^\"]+)\"|^\s*(module|variable|output|provider)\s+\"([^\"]+)\"")

(defn- matching-brace
  "Index of the `}` that closes the `{` at `open`."
  [^String s open]
  (let [n (count s)]
    (loop [i open depth 0 in-string? false]
      (cond
        (>= i n) n
        :else
        (let [c (.charAt s i)]
          (cond
            (and in-string? (= c \\)) (recur (+ i 2) depth true)
            (= c \") (recur (inc i) depth (not in-string?))
            in-string? (recur (inc i) depth true)
            (= c \{) (recur (inc i) (inc depth) false)
            (= c \}) (if (= depth 1) i (recur (inc i) (dec depth) false))
            :else (recur (inc i) depth false)))))))

(defn- locals-in
  "Names declared in every `locals { ... }` block of `masked`."
  [^String masked]
  (let [m (re-matcher #"(?m)^\s*locals\s*\{" masked)]
    (loop [acc []]
      (if (.find m)
        (let [open (dec (.end m))
              close (matching-brace masked open)
              body (subs masked (inc open) (min close (count masked)))
              ;; top-level keys only: depth 0 inside the block
              keys (loop [i 0 depth 0 line-start 0 out []]
                     (if (>= i (count body))
                       out
                       (let [c (.charAt ^String body i)]
                         (cond
                           (= c \newline)
                           (let [line (subs body line-start i)
                                 k (when (zero? depth)
                                     (second (re-find #"^\s*([A-Za-z_][\w-]*)\s*=" line)))]
                             (recur (inc i) depth (inc i) (cond-> out k (conj k))))
                           (#{\{ \[ \(} c) (recur (inc i) (inc depth) line-start out)
                           (#{\} \] \)} c) (recur (inc i) (dec depth) line-start out)
                           :else (recur (inc i) depth line-start out)))))]
          (recur (into acc keys)))
        acc))))

(defn declarations
  "Addresses a Terraform file declares: `aws_x.y`, `data.aws_x.y`,
  `module.m`, `local.k` (plus variable/output/provider, which are not link
  targets but are listed as ops)."
  [masked]
  (let [blocks (for [[_ kind1 type1 name1 kind2 name2] (re-seq block-re masked)]
                 (cond
                   (= kind1 "resource") {:addr (str type1 "." name1) :type type1 :kind :resource}
                   (= kind1 "data") {:addr (str "data." type1 "." name1) :type type1 :kind :data}
                   (= kind2 "module") {:addr (str "module." name2) :kind :module}
                   (= kind2 "variable") {:addr (str "var." name2) :kind :variable}
                   (= kind2 "output") {:addr (str "output." name2) :kind :output}
                   :else {:addr (str "provider." name2) :kind :provider}))
        locals (map (fn [k] {:addr (str "local." k) :kind :local}) (locals-in masked))]
    (vec (concat blocks locals))))

(defn- service-of
  "`aws_ecs_service` -> `aws.ecs`; a type without a provider prefix is nil."
  [type]
  (when-let [[_ provider service] (re-find #"^([a-z0-9]+)_([a-z0-9]+)" (str type))]
    (str provider "." service)))

;; ---------------------------------------------------------------- references

(def ^:private ref-re
  #"(?<![\w.\-])((?:data\.[a-z][a-z0-9_]*\.[A-Za-z][\w-]*)|(?:module\.[A-Za-z][\w-]*)|(?:var\.[A-Za-z][\w-]*)|(?:local\.[A-Za-z][\w-]*)|(?:[a-z][a-z0-9]*_[a-z0-9_]+\.[A-Za-z][\w-]*))")

(defn references
  "Distinct addresses referenced in `masked`."
  [masked]
  (->> (re-seq ref-re (str/replace masked #"(?m)^\s*description\s*=.*$" ""))
       (map second) distinct vec))

;; ---------------------------------------------------------------- scan

(defrecord TerraformGraph []
  graph/LanguageGraph
  (scan [_ root opts]
    (let [prefix (or (:prefix opts) "infra")
          ns-prefix (or (:ns-prefix opts) prefix)
          vars? (boolean (:vars opts))
          files (tf-files root)
          parsed (mapv (fn [f]
                         (let [masked (mask-comments (slurp f))
                               rel (module-relative root f)
                               id (graph/id-of (graph/ns-join ns-prefix rel) prefix)]
                           {:id id
                            :ns (graph/ns-join ns-prefix rel)
                            :file f
                            :masked masked
                            :decls (declarations masked)}))
                       files)
          owner (into {}
                      (for [p parsed d (:decls p)
                            :when (not (#{:output :provider} (:kind d)))]
                        [(:addr d) (:id p)]))
          edges (vec
                  (for [p parsed
                        addr (references (:masked p))
                        :let [to (owner addr)]
                        :when (and to (not= to (:id p))
                                   (or vars? (not (str/starts-with? addr "var."))))]
                    {:from (:id p) :to to :kind :dependency}))
          foreign-edges (vec
                          (for [p parsed
                                d (:decls p)
                                :when (#{:resource :data} (:kind d))
                                :let [svc (service-of (:type d))]
                                :when svc]
                            {:from (:id p) :to (keyword svc) :kind :dependency}))
          foreign-ids (distinct (map :to foreign-edges))
          classes (into (mapv (fn [p]
                                (let [ops (->> (:decls p)
                                               (remove #(#{:variable :provider} (:kind %)))
                                               (map :addr)
                                               distinct
                                               (mapv (fn [a] {:name a :text a})))]
                                  (cond-> {:id (:id p)
                                           :name (graph/module-name (:id p))
                                           :ns (:ns p)
                                           :lang :terraform
                                           :file (graph/relative-path (:file p))}
                                    (seq ops) (assoc :ops ops))))
                              parsed)
                        (map graph/foreign-class foreign-ids))]
      {:classes classes
       :edges (vec (distinct (concat edges foreign-edges)))})))

(def impl (->TerraformGraph))

(graph/register! :terraform impl)
