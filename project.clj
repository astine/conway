(defproject conway "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.0.3"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2268"]
                 [reagent "0.4.2"]
                 [org.clojure/core.async "0.1.319.0-6b1aca-alpha"]]
  :cljsbuild 
  {:builds 
   [{:source-paths ["src-cljs"]
     :compiler 
     {:output-to "www/js/main.js"
      :optimizations :whitespace
      :preamble ["reagent/react.js"]
      :pretty-print true}}]})
