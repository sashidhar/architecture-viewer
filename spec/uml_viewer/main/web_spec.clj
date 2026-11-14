(ns uml-viewer.main.web-spec
  (:require [speclj.core :refer :all]
            [uml-viewer.main.web :as web]))

(describe "main.web parse-args"
  (it "defaults the page next to the IR and opens it"
    (should= {:ir "a/b.edn" :out "a/b.html" :open? true} (web/parse-args ["a/b.edn"])))
  (it "honours --out and --no-open in any order"
    (should= {:ir "x.edn" :out "p.html" :open? false}
             (web/parse-args ["--no-open" "x.edn" "--out" "p.html"])))
  (it "requires an IR path"
    (should-throw clojure.lang.ExceptionInfo (web/parse-args ["--no-open"])))
  (it "rejects a second positional argument"
    (should-throw clojure.lang.ExceptionInfo (web/parse-args ["a.edn" "b.edn"]))))
