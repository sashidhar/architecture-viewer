(ns uml-viewer.application.components
  "Component view: roll the class graph up into named components and the
  systems around them (databases, queues, third-party services), then render
  it as a Mermaid flowchart.

  Policy keys:
    :components [{:id :billing :name \"Billing\" :group \"Backend\"
                  :match [\"backend.projects.service.billing.*\"]} ...]
      A class belongs to the first component whose `:match` fits. A pattern is
      an id, or `prefix.*` for everything under that prefix.
    :systems [{:id :postgres :name \"PostgreSQL\" :kind :database
               :via [\"jakarta.persistence\" \"org.hibernate\"]
               :group \"Data\"} ...]
      A component uses a system when one of its classes has a dependency on an
      id that a `:via` pattern fits: a project class or a foreign package
      (`jakarta.persistence`, `github.com.jackc.pgx`). A project class that a
      `:via` pattern fits belongs to the system, not to a component, so an
      adapter package (a Paddle client) shows as the system it talks to.
      `:kind` is :database, :queue or :external and only changes the shape.
    :min-weight n  drop component-to-component links backed by fewer than n
      class dependencies (system links always stay). Default 1."
  (:require [clojure.string :as str]
            [uml-viewer.application.ir-generator :as ir-generator]))

(defn- id-str [x] (name (keyword x)))

(defn matches?
  "True when `id` is `pattern` or sits under it. `prefix.*` is the same as
  `prefix`, so a package name matches every class and package below it."
  [pattern id]
  (let [p (str/replace (id-str pattern) #"\.\*$" "") i (id-str id)]
    (or (= i p) (str/starts-with? i (str p ".")))))

(defn- first-match [specs key id]
  (some (fn [spec] (when (some #(matches? % id) (get spec key)) spec)) specs))

(defn- project-ids [graph]
  (into {} (comp (remove :foreign) (map (juxt :id identity))) (:classes graph)))

(defn roll-up
  "Component model of `graph` under `policy`:
  {:components [{:id :name :group :classes n}] :systems [...]
   :edges [{:from :to :weight n}] :unmatched [class-ids]}.
  Edges join components, and components to the systems they use."
  [policy graph]
  (let [comps (vec (:components policy))
        systems (vec (:systems policy))
        classes (project-ids graph)
        absorbed? (fn [id] (some (fn [s] (some #(matches? % id) (:via s))) systems))
        owner (into {} (keep (fn [id]
                               (when-not (absorbed? id)
                                 (when-let [c (first-match comps :match id)]
                                   [id (:id c)]))))
                    (keys classes))
        class-edges (for [{:keys [from to]} (:edges graph)
                          :let [a (owner from)]
                          :when a]
                      [a to])
        comp-edges (for [[a to] class-edges
                         :let [b (owner to)]
                         :when (and b (not= a b))]
                     [a b])
        sys-edges (for [[a to] class-edges
                        s systems
                        :when (some #(matches? % to) (:via s))]
                    [a (:id s)])
        min-weight (or (:min-weight policy) 1)
        comp-weights (into {} (filter (fn [[_ w]] (>= w min-weight))) (frequencies comp-edges))
        weights (merge comp-weights (frequencies sys-edges))
        counts (frequencies (vals owner))]
    {:title (:title policy)
     :components (mapv #(assoc (select-keys % [:id :name :group])
                          :classes (get counts (:id %) 0))
                       comps)
     :systems (mapv #(select-keys % [:id :name :kind :group]) systems)
     :edges (->> weights
                 (map (fn [[[a b] w]] {:from a :to b :weight w}))
                 (sort-by (juxt (comp name :from) (comp name :to)))
                 vec)
     :unmatched (vec (sort (remove #(or (owner %) (absorbed? %)) (keys classes))))}))

;; ---------------------------------------------------------------- Mermaid

(defn- node-id [prefix id]
  (str prefix "_" (str/replace (id-str id) #"[^A-Za-z0-9_]" "_")))

(defn- label [s]
  (-> (str s) (str/replace "\"" "'")))

(defn- system-node [s]
  (let [id (node-id "s" (:id s))
        text (str "\"" (label (:name s)) "\"")]
    (case (:kind s)
      :database (str id "[(" text ")]")
      :queue (str id ">" text "]")
      (str id "([" text "])"))))

(defn- component-node [c]
  (str (node-id "c" (:id c)) "[\"" (label (:name c))
       (when (pos? (:classes c)) (str "<br/><small>" (:classes c) " classes</small>"))
       "\"]"))

(defn- subgraph [index title nodes]
  (str "  subgraph g" index "[\"" (label title) "\"]\n"
       (str/join "" (map #(str "    " % "\n") nodes))
       "  end\n"))

(defn mermaid
  "Mermaid flowchart text for a `roll-up` model. A component or system is drawn
  inside the subgraph named by its `:group`; ungrouped ones sit at the top."
  [{:keys [components systems edges]}]
  (let [entries (concat (map (fn [c] [(:group c) (component-node c)]) components)
                        (map (fn [s] [(:group s) (system-node s)]) systems))
        groups (distinct (keep first entries))
        by-group (group-by first entries)
        ids (into {} (map-indexed (fn [i g] [g i]) groups))
        kinds (into {} (concat (map (fn [c] [(:id c) "c"]) components)
                               (map (fn [s] [(:id s) "s"]) systems)))]
    (str "flowchart LR\n"
         (str/join "" (for [g groups]
                        (subgraph (ids g) g (map second (by-group g)))))
         (str/join "" (map #(str "  " (second %) "\n") (get by-group nil)))
         (str/join "" (for [{:keys [from to weight]} edges
                            :when (and (kinds from) (kinds to))]
                        (str "  " (node-id (kinds from) from) " -->|" weight "| "
                             (node-id (kinds to) to) "\n"))))))

(defn page
  "Standalone HTML that draws `mermaid-text` (Mermaid is loaded from a CDN)."
  [title mermaid-text]
  (str "<!doctype html><html><head><meta charset=\"utf-8\"><title>" (label title)
       "</title><style>body{font-family:system-ui,sans-serif;margin:1rem}"
       "pre.mermaid{overflow:auto}</style></head><body><h1>" (label title)
       "</h1><pre class=\"mermaid\">\n" mermaid-text "</pre>"
       "<script type=\"module\">import mermaid from "
       "\"https://cdn.jsdelivr.net/npm/mermaid@11.4.0/dist/mermaid.esm.min.mjs\";"
       "mermaid.initialize({startOnLoad:true,theme:\"neutral\",maxEdges:5000,"
       "securityLevel:\"strict\",flowchart:{htmlLabels:true}});</script></body></html>\n"))

(defn generate
  "Scan `policy-path`, roll up, and return {:model :mermaid}."
  [graph-impl policy-path]
  (let [policy (ir-generator/read-policy policy-path)
        model (roll-up policy (ir-generator/scan-policy graph-impl policy))]
    {:model model :mermaid (mermaid model)}))
