(ns build
  "Build script for this project"
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as cb]))

(defn project
  "apply project default to `opts`"
  [opts]
  (let [version (format "1.1.%s" (b/git-count-revs nil))
        defaults {:lib     'io.github.robertluo/rich-comment-test
                  :version version}]
    (merge defaults opts)))

(defn pom
  [opts]
  (let [pom-data
        [[:description "Rich comment test"]
         [:url "https://github.com/robertluo/rich-comment-tests"]
         [:licenses
          [:license
           [:name "MIT"]
           [:url "https://github.com/robertluo/rich-comment-tests/blob/main/LICENSE.md"]]]]
        opts (merge opts
                    {:basis (b/create-basis {})
                     :target "target"
                     :pom-data pom-data})]
    (b/write-pom opts)
    opts))

(defn tests
  "run all tests, for clj and cljs."
  [opts]
  (-> opts (cb/run-task [:test])))

(defn ci
  [opts]
  (-> opts
      (project)
      (cb/clean)
      (tests)
      (pom)
      (cb/jar)))

(defn deploy
  [opts]
  (-> opts
      (project)
      (cb/deploy)))
