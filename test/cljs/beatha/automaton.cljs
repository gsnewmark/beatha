(ns beatha.automaton-test
  (:require [cemerick.cljs.test]
            [cemerick.double-check]
            [cemerick.double-check.clojure-test :include-macros true
                                                :refer [defspec]]
            [cemerick.double-check.clojure-test.runtime]
            [cemerick.double-check.generators :as gen]
            [cemerick.double-check.properties :include-macros true
                                              :refer [for-all]]
            [beatha.automaton :as a]))

(defn gen-2d-cell [] (gen/vector gen/nat 2))

(defn gen-nat-more-than [n] (gen/such-that #(> % n) gen/nat))

(defspec eight-neighbours-are-generated 100
  (for-all [cell (gen-2d-cell)] (= 8 (count (a/neighbours cell)))))

(defspec neighbours-does-not-contain-cell-itself 100
  (for-all [cell (gen-2d-cell)]
           (not (contains? (into #{} (a/neighbours cell)) cell))))

(defspec cells-on-top-row-should-have-neighbours-from-the-bottom-one 100
  (for-all [x (gen-nat-more-than 0)
            max-height (gen-nat-more-than 2)]
           (let [cell [x 0]
                 max-width (+ x 2)
                 neighbour-y (dec max-height)]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     (map (fn [x] [x neighbour-y]) [x (dec x) (inc x)])))))

(defspec cells-on-bottom-row-should-have-neighbours-from-the-top-one 100
  (for-all [x (gen-nat-more-than 0)
            max-height (gen-nat-more-than 2)]
           (let [cell [x (dec max-height)]
                 max-width (+ x 2)]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     (map (fn [x] [x 0]) [x (dec x) (inc x)])))))

(defspec cells-along-right-wall-should-have-neighbours-on-the-left-one 100
  (for-all [y (gen-nat-more-than 0)
            max-width (gen-nat-more-than 2)]
           (let [cell [(dec max-width) y]
                 max-height (+ y 2)]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     (map (fn [y] [0 y]) [y (dec y) (inc y)])))))

(defspec cells-along-left-wall-should-have-neighbours-on-the-right-one 100
  (for-all [y (gen-nat-more-than 0)
            max-width (gen-nat-more-than 2)]
           (let [cell [0 y]
                 max-height (+ y 2)
                 neighbour-x (dec max-width)]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     (map (fn [y] [neighbour-x y]) [y (dec y) (inc y)])))))

(defspec cell-in-top-left-corner-should-have-correct-neigbours 100
  (for-all [max-height (gen-nat-more-than 2)
            max-width (gen-nat-more-than 2)]
           (let [cell [0 0]
                 neighbour-y (dec max-height)
                 neighbour-x (dec max-width)]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     [[0 neighbour-y] [neighbour-x 0] [1 neighbour-y]
                      [neighbour-x 1] [neighbour-x neighbour-y]]))))

(defspec cell-in-top-right-corner-should-have-correct-neigbours 100
  (for-all [max-height (gen-nat-more-than 2)
            max-width (gen-nat-more-than 2)]
           (let [x (dec max-width)
                 cell [x 0]
                 neighbour-y (dec max-height)]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     [[(dec x) neighbour-y] [x neighbour-y] [0 0] [0 1]
                      [0 neighbour-y]]))))

(defspec cell-in-bottom-left-corner-should-have-correct-neigbours 100
  (for-all [max-height (gen-nat-more-than 2)
            max-width (gen-nat-more-than 2)]
           (let [y (dec max-height)
                 cell [0 y]
                 neighbour-x (dec max-width)]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     [[neighbour-x y] [neighbour-x 0] [0 0] [1 0]
                      [neighbour-x (dec y)]]))))

(defspec cell-in-bottom-right-corner-should-have-correct-neigbours 100
  (for-all [max-height (gen-nat-more-than 2)
            max-width (gen-nat-more-than 2)]
           (let [x (dec max-width)
                 y (dec max-height)
                 cell [x y]]
             (every? (into #{} (a/neighbours max-width max-height cell))
                     [[0 (dec y)] [0 y] [0 0] [x 0] [(dec x) 0]]))))
