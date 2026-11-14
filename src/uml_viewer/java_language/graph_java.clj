(ns uml-viewer.java-language.graph-java
  "Java LanguageGraph: one class per top-level type (one per .java file).
  Imports, same-package references and fully qualified names are
  dependencies; `extends` is :inheritance, `implements` is :implements.
  The package declaration, not the directory, names the tree."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.graph :as graph]))

;; ---------------------------------------------------------------- files

(defn- skip-dir? [name]
  (or (str/starts-with? name ".")
      (#{"build" "target" "out" "node_modules" "generated"} name)))

(defn- java-file? [^java.io.File f]
  (let [n (.getName f)]
    (and (.isFile f)
         (str/ends-with? n ".java")
         (not (#{"package-info.java" "module-info.java"} n)))))

(defn- source-files
  "`.java` files under `root` (a path or a collection of paths)."
  [root]
  (->> (if (coll? root) root [root])
       (mapcat (fn [r]
                 (let [r (.getCanonicalFile (io/file r))]
                   (tree-seq (fn [f] (and (.isDirectory f)
                                          (or (= f r) (not (skip-dir? (.getName f))))))
                             (fn [d] (vec (.listFiles d)))
                             r))))
       (filter java-file?)
       (sort-by #(.getPath ^java.io.File %))
       distinct))

;; ---------------------------------------------------------------- masking

(defn- blank [^StringBuilder sb ^String s i end]
  (doseq [k (range i end)]
    (.append sb (if (= \newline (.charAt s k)) \newline \space))))

(defn mask
  "`source` with comments, string literals and char literals blanked
  (newlines kept), so that code patterns cannot match inside them."
  [^String source]
  (let [n (count source)
        sb (StringBuilder.)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt source i)
              nx (graph/next-char source i)]
          (cond
            (graph/line-comment? c nx)
            (recur (let [j (or (str/index-of source "\n" i) n)]
                     (blank sb source i j) j))

            (graph/block-comment? c nx)
            (recur (let [e (graph/mask-block-comment source sb i n)] e))

            (str/starts-with? (subs source i (min n (+ i 3))) "\"\"\"")
            (recur (let [j (str/index-of source "\"\"\"" (+ i 3))
                         e (if j (+ j 3) n)]
                     (blank sb source i e) e))

            (or (= c \") (= c \'))
            (recur (let [e (graph/scan-quoted source i c n)]
                     (blank sb source i e) e))

            :else (do (.append sb c) (recur (inc i)))))))
    (str sb)))

;; ---------------------------------------------------------------- reading one file

(def ^:private package-re #"(?m)^\s*package\s+([\w.]+)\s*;")
(def ^:private import-re #"(?m)^\s*import\s+(static\s+)?([\w.]+?)(\.\*)?\s*;")
(def ^:private type-re
  #"(?:^|[\s;}])(?:(?:public|protected|private|abstract|final|static|sealed|non-sealed|strictfp)\s+)*(@interface|class|interface|enum|record)\s+([A-Za-z_]\w*)")

(defn- strip-nested
  "`s` without any `open ... close` groups (nesting aware)."
  [s open close]
  (let [sb (StringBuilder.)]
    (reduce (fn [depth c]
              (cond
                (= c open) (inc depth)
                (and (= c close) (pos? depth)) (dec depth)
                (zero? depth) (do (.append sb c) depth)
                :else depth))
            0 s)
    (str sb)))

(defn- type-names
  "Type names in a comma list: `A<B>, p.C` -> [\"A\" \"p.C\"]."
  [s]
  (->> (str/split (strip-nested s \< \>) #",")
       (map str/trim)
       (keep #(re-find #"^[\w.]+" %))
       vec))

(defn- header-clause [header kw other-kws]
  (let [stops (str/join "|" (map #(str "\\b" % "\\b") other-kws))
        m (re-find (re-pattern (str "(?s)\\b" kw "\\b\\s*(.+?)(?=" stops "|$)")) header)]
    (when m (type-names (second m)))))

(defn read-source
  "Surface of one Java file: package, imports, top-level type and header
  names, plus the masked text after the imports. `file-name` picks the
  primary type when a file declares several."
  [source file-name]
  (let [masked (mask source)
        pkg (second (re-find package-re masked))
        imports (vec (for [m (re-seq import-re masked)]
                       {:static? (boolean (nth m 1))
                        :wildcard? (boolean (nth m 3))
                        :name (nth m 2)}))
        body-start (let [ends (fn [re] (let [m (re-matcher re masked)]
                                         (loop [e 0]
                                           (if (.find m) (recur (.end m)) e))))]
                     (max (ends package-re) (ends import-re)))
        body (subs masked body-start)
        types (let [m (re-matcher type-re body)]
                (loop [acc []]
                  (if (.find m)
                    (recur (conj acc {:kind (.group m 1) :name (.group m 2) :end (.end m)}))
                    acc)))
        base (str/replace (or file-name "") #"\.java$" "")
        t (or (some #(when (= base (:name %)) %) types) (first types))]
    (when t
      (let [after (subs body (:end t))
            header (let [h (subs after 0 (or (str/index-of after "{") (count after)))]
                     (if (= "record" (:kind t))
                       (strip-nested h \( \))
                       h))
            header (strip-nested header \< \>)
            interface? (#{"interface" "@interface"} (:kind t))]
        {:package pkg
         :imports imports
         :name (:name t)
         :kind (:kind t)
         :interface? (boolean interface?)
         :extends (vec (header-clause header "extends" ["implements" "permits"]))
         :implements (vec (header-clause header "implements" ["extends" "permits"]))
         :body body}))))

;; ---------------------------------------------------------------- resolution

(defn- project-index
  "{:by-fqn fqn->parsed, :by-pkg pkg->{simple parsed}}."
  [parsed]
  {:by-fqn (into {} (map (juxt :fqn identity)) parsed)
   :by-pkg (reduce (fn [m p] (assoc-in m [(:package p) (:name p)] p)) {} parsed)})

(defn- resolve-fqn
  "Project class for dotted name `fqn`, trying shorter prefixes so that a
  nested type `a.B.Inner` resolves to `a.B`."
  [index fqn]
  (loop [segs (str/split fqn #"\.")]
    (when (seq segs)
      (or (get-in index [:by-fqn (str/join "." segs)])
          (recur (butlast segs))))))

(defn- foreign-package
  "Package part of import `fqn`: the leading lowercase segments."
  [fqn]
  (let [segs (take-while #(not (Character/isUpperCase ^char (first %))) (str/split fqn #"\."))]
    (when (seq segs) (str/join "." segs))))

(defn- file-resolver
  "Function from a (simple or dotted) type name used in `p` to a project class."
  [index p]
  (let [simple->fqn (into {}
                          (keep (fn [{:keys [name wildcard? static?]}]
                                  (when-not wildcard?
                                    (let [segs (str/split name #"\.")
                                          cls (if static?
                                                (last (butlast segs))
                                                (last segs))]
                                      (when cls [cls (if static?
                                                       (str/join "." (butlast segs))
                                                       name)])))))
                          (:imports p))
        wild (keep #(when (:wildcard? %) (:name %)) (:imports p))]
    (fn [type-name]
      (let [head (first (str/split type-name #"\."))]
        (or (some->> (simple->fqn head) (resolve-fqn index))
            (get-in index [:by-pkg (:package p) head])
            (some #(or (get-in index [:by-pkg % head])
                       (resolve-fqn index (str % "." head)))
                  wild)
            (when (str/includes? type-name ".") (resolve-fqn index type-name)))))))

(def ^:private ident-re #"[A-Za-z_$][\w$]*")
(def ^:private qualified-re #"\b(?:[a-z_]\w*\.)+[A-Z]\w*")

(defn- referenced
  "Project classes referenced from `p`, as {fqn parsed}."
  [index p resolve]
  (let [from-imports (keep (fn [{:keys [name wildcard?]}]
                             (when-not wildcard? (resolve-fqn index name)))
                           (:imports p))
        from-idents (keep #(or (get-in index [:by-pkg (:package p) %])
                               (resolve (str %)))
                          (set (re-seq ident-re (:body p))))
        from-qualified (keep #(resolve-fqn index %) (set (re-seq qualified-re (:body p))))]
    (->> (concat from-imports from-idents from-qualified)
         (remove #(= (:fqn %) (:fqn p))))))

;; ---------------------------------------------------------------- strip prefix

(defn- common-package
  "Longest dotted package prefix shared by all `packages`."
  [packages]
  (let [segs (map #(str/split % #"\.") (remove str/blank? packages))]
    (if (empty? segs)
      ""
      (str/join "." (reduce (fn [acc s]
                              (vec (map first (take-while (fn [[a b]] (= a b))
                                                          (map vector acc s)))))
                            (first segs) (rest segs))))))

(defn- strip-package [pkg strip]
  (cond
    (str/blank? strip) pkg
    (= pkg strip) ""
    (str/starts-with? pkg (str strip ".")) (subs pkg (inc (count strip)))
    :else pkg))

;; ---------------------------------------------------------------- scan

(defn- read-file [file]
  (let [source (slurp file)
        parsed (read-source source (.getName ^java.io.File file))]
    (when (and parsed (:package parsed))
      (assoc parsed
        :fqn (str (:package parsed) "." (:name parsed))
        :file file))))

(defn- class-id [p strip ns-prefix prefix]
  (let [rel (strip-package (:package p) strip)
        dotted (if (str/blank? rel) (:name p) (str rel "." (:name p)))]
    (graph/id-of (graph/ns-join ns-prefix dotted) prefix)))

(defn- edges-of [index id-of p]
  (let [resolve (file-resolver index p)
        owner (id-of p)
        strong (fn [names kind]
                 (for [n names :let [t (resolve n)] :when t]
                   {:from owner :to (id-of t) :kind kind}))
        class-kind (if (:interface? p) :implements :inheritance)
        inh (concat (strong (:extends p) class-kind)
                    (strong (:implements p) :implements))
        strong-targets (set (map :to inh))
        deps (for [t (referenced index p resolve)
                   :let [to (id-of t)]
                   :when (not (strong-targets to))]
               {:from owner :to to :kind :dependency})
        foreign (for [{:keys [name wildcard?] :as imp} (:imports p)
                      :when (and (not (resolve-fqn index name))
                                 (not (and wildcard? (get-in index [:by-pkg name]))))
                      :let [pkg (foreign-package name)]
                      :when pkg]
                  {:from owner :to (keyword pkg) :kind :dependency :foreign pkg})]
    (concat inh deps foreign)))

(defrecord JavaGraph []
  graph/LanguageGraph
  (scan [_ root opts]
    (let [prefix (or (:prefix opts) "app")
          ns-prefix (or (:ns-prefix opts) prefix)
          parsed (vec (keep read-file (source-files root)))
          strip (if (contains? opts :strip)
                  (:strip opts)
                  (common-package (map :package parsed)))
          index (project-index parsed)
          id-of #(class-id % strip ns-prefix prefix)
          edges (vec (mapcat #(edges-of index id-of %) parsed))
          foreign-ids (->> edges (keep :foreign) distinct sort)
          classes (into (mapv (fn [p]
                                (cond-> {:id (id-of p)
                                         :name (:name p)
                                         :ns (graph/ns-join ns-prefix
                                                            (let [rel (strip-package (:package p) strip)]
                                                              (if (str/blank? rel)
                                                                (:name p)
                                                                (str rel "." (:name p)))))
                                         :lang :java
                                         :file (graph/relative-path (:file p))}
                                  (:interface? p) (assoc :stereotype :interface)))
                              parsed)
                        (map #(graph/foreign-class (keyword %)) foreign-ids))]
      {:classes classes
       :edges (->> edges
                   (map #(dissoc % :foreign))
                   (remove #(= (:from %) (:to %)))
                   distinct
                   vec)})))

(def impl (->JavaGraph))

(graph/register! :java impl)
