(defproject mulesoft-clj "0.1.0-SNAPSHOT"
  :description "MuleSoft API client"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [csv-map "0.1.2"]
                 [org.clojure/tools.cli "0.3.5"]
                 [clj-http "3.4.1"]
                 [cheshire "5.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [log4j "1.2.17"]
                 [slingshot "0.12.2"]]
  :main ^:skip-aot post-migration-deploy-proxies.core
  :uberjar-name "mulesoft-api.jar"
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
