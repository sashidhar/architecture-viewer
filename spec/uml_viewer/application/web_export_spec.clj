(ns uml-viewer.application.web-export-spec
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.web-export :as web]))

(def doc
  {:hierarchical true
   :title "Demo"
   :prefix "demo"
   :levels [[:domain] [:adapters]]
   :classes [{:id :domain.model :ns "demo.domain.model" :name "Model" :level 0 :file "src/domain/model.clj"}
             {:id :adapters.http :ns "demo.adapters.http" :name "Http" :level 1 :file "src/adapters/http.clj"}
             {:id :ring :name "ring" :foreign true}]
   :edges [{:from :adapters.http :to :domain.model :kind :dependency}
           {:from :domain.model :to :adapters.http :kind :dependency}
           {:from :adapters.http :to :ring :kind :dependency}]})

(defn- edge [data from to]
  (first (filter #(and (= from (:from %)) (= to (:to %))) (:edges data))))

(describe "web-export"
  (context "graph-data"
    (it "carries title, prefix and levels as plain strings"
      (let [d (web/graph-data doc)]
        (should= "Demo" (:title d))
        (should= "demo" (:prefix d))
        (should= [["domain"] ["adapters"]] (:levels d))))

    (it "emits dotted string ids with level, foreign flag and file"
      (let [d (web/graph-data doc)
            by-id (into {} (map (juxt :id identity) (:nodes d)))]
        (should= #{"domain.model" "adapters.http" "ring"} (set (keys by-id)))
        (should= 1 (:level (by-id "adapters.http")))
        (should= "src/domain/model.clj" (:file (by-id "domain.model")))
        (should (:foreign (by-id "ring")))
        (should-not (:foreign (by-id "domain.model")))))

    (it "marks only inner-to-outer dependencies as violating"
      (let [d (web/graph-data doc)]
        (should-not (:violating (edge d "adapters.http" "domain.model")))
        (should (:violating (edge d "domain.model" "adapters.http")))
        (should-not (:violating (edge d "adapters.http" "ring")))))

    (it "rejects a document that is not hierarchical"
      (should-throw clojure.lang.ExceptionInfo
                    (web/graph-data {:title "x" :diagrams []}))))

  (context "render-html"
    (it "is one self-contained page: data, graph logic and vendored libs inlined"
      (let [html (web/render-html (web/graph-data doc))]
        (should-contain "<title>Demo" html)
        (should-contain "UmlGraph" html)
        (should-contain "cytoscape" html)
        (should-contain "ELK" (str/upper-case html))
        (should-not (re-find #"<script[^>]+src=" html))
        (should-not (re-find #"\{\{[A-Z_]+\}\}" (subs html 0 2000)))))

    (it "round-trips the graph as JSON and cannot be broken out of the script tag"
      (let [evil (assoc-in doc [:classes 0 :file] "</script><script>alert(1)</script>")
            html (web/render-html (web/graph-data evil))
            m (re-find #"(?s)<script id=\"graph-data\" type=\"application/json\">(.*?)</script>" html)
            parsed (json/read-str (second m) :key-fn keyword)]
        (should-not-be-nil m)
        (should= 3 (count (:nodes parsed)))
        (should-contain "</script><script>alert(1)</script>"
                        (map :file (:nodes parsed))))))

  (context "write-html!"
    (it "reads an IR file and writes the page"
      (let [ir (doto (java.io.File/createTempFile "web-ir" ".edn") (spit (pr-str doc)))
            out (java.io.File/createTempFile "web-out" ".html")]
        (should= (.getPath out) (web/write-html! (.getPath ir) (.getPath out)))
        (should-contain "Demo" (slurp out))))))
