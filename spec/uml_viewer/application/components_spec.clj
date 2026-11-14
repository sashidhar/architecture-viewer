(ns uml-viewer.application.components-spec
  (:require [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.components :as components]))

(def graph
  {:classes [{:id :web.Ctl} {:id :svc.Svc} {:id :repo.Repo} {:id :stray.X}
             {:id :jakarta.persistence :foreign true}]
   :edges [{:from :web.Ctl :to :svc.Svc :kind :dependency}
           {:from :web.Ctl :to :svc.Svc :kind :implements}
           {:from :svc.Svc :to :repo.Repo :kind :dependency}
           {:from :repo.Repo :to :jakarta.persistence :kind :dependency}
           {:from :repo.Repo :to :repo.Repo :kind :dependency}]})

(def policy
  {:title "T"
   :components [{:id :api :name "API" :group "Backend" :match ["web.*"]}
                {:id :core :name "Core" :group "Backend" :match ["svc.*" "repo.*"]}]
   :systems [{:id :pg :name "Postgres" :kind :database :via ["jakarta.persistence"] :group "Data"}]})

(describe "matches?"
  (it "fits ids and prefix patterns"
    (should (components/matches? "a.b.*" :a.b.C))
    (should-not (components/matches? "a.b.*" :a.bc.C))
    (should (components/matches? "a.b" :a.b.C))
    (should (components/matches? "a.b.*" :a.b))
    (should (components/matches? "a.b.C" :a.b.C))))

(describe "roll-up"
  (it "groups classes, counts class edges and finds systems used"
    (let [m (components/roll-up policy graph)]
      (should= [["api" 1] ["core" 2]] (map (juxt (comp name :id) :classes) (:components m)))
      (should= #{[:api :core 2] [:core :pg 1]}
               (set (map (juxt :from :to :weight) (:edges m))))
      (should= [:stray.X] (:unmatched m)))))

(describe "min-weight"
  (it "drops weak component links but keeps system links"
    (let [m (components/roll-up (assoc policy :min-weight 3) graph)]
      (should= #{[:core :pg 1]} (set (map (juxt :from :to :weight) (:edges m)))))))

(describe "adapter classes"
  (it "belong to the system they match, not to a component"
    (let [g {:classes [{:id :web.Ctl} {:id :paddle.Client}]
             :edges [{:from :web.Ctl :to :paddle.Client :kind :dependency}
                     {:from :paddle.Client :to :web.Ctl :kind :dependency}]}
          p {:components [{:id :api :name "API" :match ["web.*" "paddle.*"]}]
             :systems [{:id :paddle :name "Paddle" :kind :external :via ["paddle.*"]}]}
          m (components/roll-up p g)]
      (should= #{[:api :paddle 1]} (set (map (juxt :from :to :weight) (:edges m))))
      (should= [] (:unmatched m)))))

(describe "mermaid"
  (it "draws groups, a cylinder for a database and weighted links"
    (let [text (components/mermaid (components/roll-up policy graph))]
      (should (str/starts-with? text "flowchart LR"))
      (should (str/includes? text "subgraph g0[\"Backend\"]"))
      (should (str/includes? text "s_pg[(\"Postgres\")]"))
      (should (str/includes? text "c_api -->|2| c_core"))
      (should (str/includes? text "c_core -->|1| s_pg"))))
  (it "wraps the diagram in a page"
    (should (str/includes? (components/page "T" "flowchart LR\n") "mermaid"))))
