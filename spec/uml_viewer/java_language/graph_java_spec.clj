(ns uml-viewer.java-language.graph-java-spec
  (:require [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.graph :as graph]
            [uml-viewer.java-language.graph-java :as java]))

(defn- spit-file [dir rel content]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- temp-root []
  (io/file (System/getProperty "java.io.tmpdir") (str "uml-java-" (System/nanoTime))))

(defn- edge-set [g] (set (map (juxt :from :to :kind) (:edges g))))

(describe "java source surface"
  (it "reads package, imports, type, extends and implements"
    (let [s (java/read-source
              (str "package com.acme.web;\n"
                   "import com.acme.svc.Service;\n"
                   "import static com.acme.util.Strings.trim;\n"
                   "import org.springframework.web.bind.annotation.*;\n"
                   "// import com.acme.nope.Nope;\n"
                   "public class Ctl<T> extends Base<T> implements Runnable, com.acme.svc.Port {\n"
                   "  String s = \"import com.acme.nope.Nope;\";\n"
                   "}\n")
              "Ctl.java")]
      (should= "com.acme.web" (:package s))
      (should= "Ctl" (:name s))
      (should= ["Base"] (:extends s))
      (should= ["Runnable" "com.acme.svc.Port"] (:implements s))
      (should= [["com.acme.svc.Service" false false]
                ["com.acme.util.Strings.trim" true false]
                ["org.springframework.web.bind.annotation" false true]]
               (map (juxt :name :static? :wildcard?) (:imports s)))
      (should-not (re-find #"Nope" (:body s)))))

  (it "treats records, enums and interfaces"
    (should (:interface? (java/read-source "package a;\npublic interface P extends Q {}" "P.java")))
    (should= ["Q"] (:extends (java/read-source "package a;\npublic interface P extends Q {}" "P.java")))
    (should= "R" (:name (java/read-source "package a;\npublic record R(int x, List<String> y) implements Z {}" "R.java")))
    (should= ["Z"] (:implements (java/read-source "package a;\npublic record R(int x, List<String> y) implements Z {}" "R.java")))
    (should= "E" (:name (java/read-source "package a;\nenum E { A, B }" "E.java"))))

  (it "ignores text blocks and `.class` literals"
    (let [s (java/read-source "package a;\nclass A {\n String t = \"\"\"\n class Fake {}\n \"\"\";\n Object o = B.class;\n}" "A.java")]
      (should= "A" (:name s)))))

(describe "java graph"
  (it "links imports, same-package use, inheritance and foreign packages"
    (let [root (temp-root)
          _ (spit-file root "com/acme/web/Ctl.java"
                       (str "package com.acme.web;\n"
                            "import com.acme.svc.Service;\n"
                            "import org.springframework.stereotype.Component;\n"
                            "public class Ctl extends Base implements com.acme.svc.Port {\n"
                            "  Helper h; Service s;\n}\n"))
          _ (spit-file root "com/acme/web/Base.java" "package com.acme.web;\nclass Base {}\n")
          _ (spit-file root "com/acme/web/Helper.java" "package com.acme.web;\nclass Helper {}\n")
          _ (spit-file root "com/acme/svc/Service.java" "package com.acme.svc;\npublic class Service {}\n")
          _ (spit-file root "com/acme/svc/Port.java" "package com.acme.svc;\npublic interface Port {}\n")
          g (graph/scan java/impl root {:prefix "app" :ns-prefix "app"})
          ids (set (map :id (:classes g)))
          edges (edge-set g)]
      (should= #{:web.Ctl :web.Base :web.Helper :svc.Service :svc.Port
                 :org.springframework.stereotype}
               ids)
      (should (contains? edges [:web.Ctl :web.Base :inheritance]))
      (should (contains? edges [:web.Ctl :svc.Port :implements]))
      (should (contains? edges [:web.Ctl :web.Helper :dependency]))
      (should (contains? edges [:web.Ctl :svc.Service :dependency]))
      (should (contains? edges [:web.Ctl :org.springframework.stereotype :dependency]))
      (should-not (contains? edges [:web.Ctl :web.Base :dependency]))
      (should= :interface (:stereotype (first (filter #(= :svc.Port (:id %)) (:classes g)))))))

  (it "accepts several roots and an explicit strip"
    (let [a (temp-root) b (temp-root)
          _ (spit-file a "com/acme/a/A.java" "package com.acme.a;\nimport com.acme.b.B;\nclass A { B b; }\n")
          _ (spit-file b "com/acme/b/B.java" "package com.acme.b;\nclass B {}\n")
          g (graph/scan java/impl [a b] {:prefix "app" :ns-prefix "app" :strip "com"})]
      (should (contains? (edge-set g) [:acme.a.A :acme.b.B :dependency])))))

(describe "spring routes"
  (it "joins class and method mappings and normalises variables"
    (should= ["/api/projects/{}" "/api/projects/{}/stop" "/api/projects"]
             (java/routes-of
               (str "package a;\n@RestController\n@RequestMapping(\"/api/projects\")\nclass C {\n"
                    "  @GetMapping(\"/{id}\") void a() {}\n"
                    "  @PostMapping(value = \"/{projectId}/stop\", produces = \"application/json\") void b() {}\n"
                    "  @GetMapping void c() {}\n}\n"))))

  (it "has no routes without method mappings"
    (should= [] (java/routes-of "package a;\n@RequestMapping(\"/x\")\nclass C {}\n")))

  (it "links a controller to a TypeScript caller through merge-scans"
    (let [root (temp-root)
          _ (spit-file root "a/Ctl.java" "package a;\n@RequestMapping(\"/api\")\nclass Ctl { @GetMapping(\"/p/{id}\") void x() {} }\n")
          g (graph/scan java/impl root {:prefix "app" :ns-prefix "app" :strip ""})
          ts {:classes [{:id :ui.page}] :edges []
              :invokes {:ui.page [(graph/normalize-route "/api/p/${id}?x=1")]}}
          merged (graph/merge-scans [g ts])]
      (should (contains? (edge-set merged) [:ui.page :a.Ctl :dependency])))))
