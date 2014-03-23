(ns beatha.automaton-test
  (:require-macros [cemerick.double-check.clojure-test :refer [defspec]]
                   [cemerick.double-check.properties :refer [for-all]])
  (:require [cemerick.cljs.test]
            [cemerick.double-check]
            [cemerick.double-check.clojure-test.runtime]
            [cemerick.double-check.generators :as gen]
            [cemerick.double-check.properties :as prop]
            [beatha.automaton :as a]))

(defn gen-2d-cell [] (gen/vector gen/int 2))

(defspec eight-neighbours-are-generated 100
  (for-all [cell (gen-2d-cell)] (= 8 (count (a/neighbours cell)))))

(defspec neighbours-does-not-contain-cell-itself 100
  (for-all [cell (gen-2d-cell)]
    (not (contains? (into #{} (a/neighbours cell)) cell))))
