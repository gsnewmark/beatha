{:shared {:clean-targets ["out" :target-path]

          :test-paths ["test/cljs"]
          :resources-paths ["dev-resources"]

          :dependencies [[com.cemerick/double-check "0.5.7-SNAPSHOT"]]
          :plugins [[com.cemerick/clojurescript.test "0.3.0"]]

          :cljsbuild
          {:builds
           {:beatha
            {:source-paths ["test/cljs"]
             :compiler {:libs [""]}}}
           :test-commands
           {"phantomjs"
            ["phantomjs" :runner
             "dev-resources/public/js/beatha.js"]}}}

 :tdd [:shared
       {:cljsbuild
        {:builds {:beatha
                  {:compiler
                   {:optimizations :whitespace
                    :pretty-print true}}}}}]

 :dev [:shared
       {:source-paths ["dev-resources/tools/http" "dev-resources/tools/repl"]

        :dependencies [[ring "1.2.1"]
                       [compojure "1.1.6"]
                       [enlive "1.1.5"]]
        :plugins [[com.cemerick/austin "0.1.4"]]

        :cljsbuild
        {:builds {:beatha
                  {:source-paths ["dev-resources/tools/repl"]
                   :compiler
                   {:optimizations :whitespace
                    :pretty-print true}}}}

        :injections
        [(require '[ring.server :as http :refer [run]]
                  'cemerick.austin.repls)
         (defn browser-repl-env []
           (reset! cemerick.austin.repls/browser-repl-env
                   (cemerick.austin/repl-env)))
         (defn browser-repl []
           (cemerick.austin.repls/cljs-repl
            (browser-repl-env)))]}]}
