(ns beatha.automaton-test
  (:require-macros [simple-check.clojure-test :refer [defspec]]
                   [simple-check.properties :refer [for-all]])
  (:require [cemerick.cljs.test]
            [simple-check.core]
            [simple-check.clojure-test.runtime]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [beatha.automaton :as a]))

(defn gen-2d-cell [] (gen/vector gen/int 2))

(defspec eight-neighbours-are-generated 100
  (for-all [cell (gen-2d-cell)] (= 8 (count (a/neighbours cell)))))

(defspec neighbours-does-not-contain-cell-itself 100
  (for-all [cell (gen-2d-cell)]
    (not (contains? (into #{} (a/neighbours cell)) cell))))
