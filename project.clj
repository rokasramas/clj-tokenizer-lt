(defproject tokenizer "0.1.0-SNAPSHOT"
  :description "Simple tokenizer for Lithuanian language based on Conditional Random Fields"
  :url "https://github.com/rokasramas/clj-tokenizer-lt"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-crfsuite "0.5.2"]]
  :main ^:skip-aot tokenizer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
