(defproject beatha "0.0.1-SNAPSHOT"
  :description "Cellular automata experiments."
  :url "http://github.com/gsnewmark/beatha"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}

  :min-lein-version "2.3.4"

  :source-paths ["src/clj" "src/cljs"]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2156"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [om "0.5.0"]
                 [com.facebook/react "0.9.0"]
                 [org.webjars/bootstrap "3.1.1"]]

  :plugins [[lein-cljsbuild "1.0.2"]]

  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds {:beatha
            {:source-paths ["src/cljs"]
             :compiler
             {:output-to "dev-resources/public/js/beatha.js"
              :optimizations :advanced
              :pretty-print false}}}})
