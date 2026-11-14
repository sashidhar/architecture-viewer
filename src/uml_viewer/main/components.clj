(ns uml-viewer.main.components
  "Component diagram: policy -> Mermaid.
   clojure -M -m uml-viewer.main.components POLICY.edn [--out FILE.mmd] [--html FILE.html]"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.application.components :as components]
            [uml-viewer.clojure-language.graph-clojure]
            [uml-viewer.go-language.graph-go]
            [uml-viewer.graph :as graph]
            [uml-viewer.java-language.graph-java]
            [uml-viewer.python-language.graph-python]
            [uml-viewer.rust-language.graph-rust]
            [uml-viewer.terraform-language.graph-terraform]
            [uml-viewer.typescript-language.graph-typescript])
  (:gen-class))

(defn parse-args
  "{:policy :out :html} from CLI args."
  [args]
  (loop [[a & more :as xs] args acc {}]
    (cond
      (empty? xs) (if (:policy acc)
                    acc
                    (throw (ex-info "usage: components POLICY.edn [--out F.mmd] [--html F.html]" {})))
      (= a "--out") (recur (rest more) (assoc acc :out (first more)))
      (= a "--html") (recur (rest more) (assoc acc :html (first more)))
      (:policy acc) (throw (ex-info (str "unexpected argument: " a) {:arg a}))
      :else (recur more (assoc acc :policy a)))))

(defn- write! [path text]
  (io/make-parents path)
  (spit path text)
  (println "Wrote" path))

(defn -main [& args]
  (let [{:keys [policy out html]} (parse-args args)
        {:keys [model mermaid]} (components/generate (graph/lookup :clojure) policy)]
    (write! (or out (str/replace policy #"\.edn$" ".components.mmd")) mermaid)
    (when html (write! html (components/page (or (:title model) "Components") mermaid)))
    (when (seq (:unmatched model))
      (binding [*out* *err*]
        (println (count (:unmatched model)) "classes match no component, e.g."
                 (str/join ", " (map name (take 8 (:unmatched model)))))))))
