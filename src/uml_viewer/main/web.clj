(ns uml-viewer.main.web
  "Browser viewer: write the IR as one HTML page and open it.
   clojure -M:web path/to/ir.edn [--out page.html] [--no-open]"
  (:require [clojure.java.browse :as browse]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.application.web-export :as web-export])
  (:gen-class))

(defn parse-args
  "{:ir :out :open?} from CLI args. :out defaults next to the IR as .html."
  [args]
  (loop [[a & more :as xs] args
         acc {:open? true}]
    (cond
      (empty? xs) (let [ir (:ir acc)]
                    (when-not ir
                      (throw (ex-info "usage: uml-viewer.main.web IR.edn [--out PAGE.html] [--no-open]" {})))
                    (assoc acc :out (or (:out acc) (str (str/replace ir #"\.edn$" "") ".html"))))
      (= a "--no-open") (recur more (assoc acc :open? false))
      (= a "--out") (recur (rest more) (assoc acc :out (first more)))
      (:ir acc) (throw (ex-info (str "unexpected argument: " a) {:arg a}))
      :else (recur more (assoc acc :ir a)))))

(defn -main [& args]
  (let [{:keys [ir out open?]} (parse-args args)
        path (web-export/write-html! ir out)]
    (println "Wrote" path)
    (when open?
      (browse/browse-url (.toURI (io/file path))))
    (shutdown-agents)))
