(ns uml-viewer.terraform-language.graph-terraform-spec
  (:require [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.graph :as graph]
            [uml-viewer.terraform-language.graph-terraform :as tf]))

(defn- spit-file [dir rel content]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- temp-root []
  (io/file (System/getProperty "java.io.tmpdir") (str "uml-tf-" (System/nanoTime))))

(describe "terraform declarations"
  (it "finds resources, data, modules, locals and ignores comments"
    (let [m (tf/mask-comments (str "# resource \"aws_x\" \"no\" {}\n"
                                   "resource \"aws_ecs_cluster\" \"main\" {\n  name = \"a#b\" // c\n}\n"
                                   "data \"aws_caller_identity\" \"current\" {}\n"
                                   "module \"cdn\" { source = \"./cdn\" }\n"
                                   "locals {\n  tags = { a = 1 }\n  name = \"x\"\n}\n"))]
      (should= ["aws_ecs_cluster.main" "data.aws_caller_identity.current" "module.cdn"
                "local.tags" "local.name"]
               (map :addr (tf/declarations m)))))

  (it "reads references but not attributes of unknown things"
    (should= ["aws_ecs_cluster.main" "local.tags" "var.region" "data.aws_caller_identity.current"]
             (tf/references "x = \"${aws_ecs_cluster.main.arn}\" y = local.tags z = var.region w = data.aws_caller_identity.current.account_id q = each.value"))))

(describe "terraform graph"
  (it "links a file to the file declaring what it references"
    (let [root (temp-root)
          _ (spit-file root "ecs.tf" "resource \"aws_ecs_cluster\" \"main\" {}\n")
          _ (spit-file root "svc.tf" (str "resource \"aws_ecs_service\" \"api\" {\n"
                                          "  cluster = aws_ecs_cluster.main.id\n  r = var.region\n}\n"))
          _ (spit-file root "variables.tf" "variable \"region\" {}\n")
          g (graph/scan tf/impl root {:prefix "infra" :ns-prefix "infra"})
          edges (set (map (juxt :from :to) (:edges g)))]
      (should (contains? edges [:svc :ecs]))
      (should (contains? edges [:svc :aws.ecs]))
      (should-not (contains? edges [:svc :variables]))
      (should= ["aws_ecs_cluster.main"]
               (map :name (:ops (first (filter #(= :ecs (:id %)) (:classes g)))))))
    )

  (it "can include variable links"
    (let [root (temp-root)
          _ (spit-file root "a.tf" "x = var.region\n")
          _ (spit-file root "variables.tf" "variable \"region\" {}\n")
          g (graph/scan tf/impl root {:prefix "infra" :ns-prefix "infra" :vars true})]
      (should (contains? (set (map (juxt :from :to) (:edges g))) [:a :variables])))))
