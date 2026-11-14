(ns uml-viewer.application.web-export
  "Export a hierarchical IR as one self-contained HTML page (Cytoscape + ELK
  inlined). The page holds the whole graph and does collapsing, search and
  tracing client-side, so large dependency graphs stay explorable."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.domain.policy :as policy]))

(defn- id-str [x] (name x))

(defn- node [c]
  (cond-> {:id (id-str (:id c))
           :name (or (:name c) (id-str (:id c)))
           :level (or (:level c) 0)
           :foreign (boolean (:foreign c))}
    (:file c) (assoc :file (:file c))
    (:ns c) (assoc :ns (:ns c))))

(defn graph-data
  "Plain-data view of a hierarchical IR `doc` for the page: dotted string ids,
  levels, and edges with `:violating` set from the document's level ranks."
  [doc]
  (when-not (:hierarchical doc)
    (throw (ex-info "web export needs a hierarchical IR (policy :hierarchical true)"
                    {:title (:title doc)})))
  (let [ranks (policy/level-ranks doc)
        edges (policy/mark-violations (:edges doc) ranks)]
    {:title (or (:title doc) "UML")
     :prefix (some-> (:prefix doc) name)
     :levels (mapv (fn [g] (mapv id-str g)) (policy/level-groups doc))
     :nodes (mapv node (:classes doc))
     :edges (mapv (fn [e] {:from (id-str (:from e))
                           :to (id-str (:to e))
                           :kind (id-str (or (:kind e) :dependency))
                           :violating (boolean (:violating e))})
                  edges)}))

(def ^:private vendor-files
  ["web/vendor/cytoscape.min.js" "web/vendor/elk.bundled.js" "web/vendor/cytoscape-elk.js"])

(defn- resource-text [path]
  (if-let [r (io/resource path)]
    (slurp r)
    (throw (ex-info (str "missing web asset on classpath: " path) {:path path}))))

(defn- escape-html-text [s]
  (-> s (str/replace "&" "&amp;") (str/replace "<" "&lt;")))

(defn render-html
  "The page for `data` (see `graph-data`) as a string, with every script and
  style inlined so it opens from file:// with no network."
  [data]
  (let [subs-map {"TITLE" (escape-html-text (:title data))
                  "STYLE" (resource-text "web/style.css")
                  "VENDOR" (str/join "\n;\n" (map resource-text vendor-files))
                  "GRAPH_JS" (resource-text "web/graph.js")
                  "APP_JS" (resource-text "web/app.js")
                  ;; data.json escapes "/" so "</script>" cannot appear in the payload
                  "DATA" (json/write-str data)}]
    (str/replace (resource-text "web/index.template.html")
                 #"\{\{([A-Z_]+)\}\}"
                 (fn [[whole k]] (get subs-map k whole)))))

(defn write-html!
  "Read the IR at `ir-path`, write the page to `out-path`, return `out-path`."
  [ir-path out-path]
  (let [doc (edn/read-string (slurp ir-path))]
    (spit out-path (render-html (graph-data doc)))
    out-path))
