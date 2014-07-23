(defproject conway "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.0.3"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2268"]]
  :cljsbuild 
  {:builds 
   [{:source-paths ["src-cljs"]
     :compiler 
     {:output-to "target/js/main.js"
      :pretty-print true}}]})
