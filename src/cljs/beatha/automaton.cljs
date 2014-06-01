(ns beatha.automaton
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! <!]]
            [clojure.set :as set]))

(defprotocol AutomatonSpecification
  "Describes basic interactions with particular set of cellular automata
  rules."
  (default-cell [this] "Default cell of the given automata.")
  (next-initial-state [this state]
    "Returns next possible initial state after the given one.")
  (next-grid [this grid]
    "Transforms given grid according to automata's rules."))

(defprotocol InformationChannelsSpecification
  "Describes interactions with information channels which augment the regular
  cellular automata:

    - command channel is a source of external commands which augment rules of
      automata;
    - output channel is filled by the automata itself with aggregate
      information about current state."
  (process-command-channel [this ic]
    "Handles messages from the command information channel.")
  (fill-output-info-channel [this oc grid]
    "Sends a message about the current automata's state."))

;;; State of world is described as a set of living cells, where each cell is
;;; vector of coordinates [x y].

(defn neighbours
  "Generates seq with neighbour cells of the given cell.

  Intended to work with a two-dimensional grid representation where each cell
  is encoded as pair of it's coordinates in a world."
  ([[x y]]
     {:pre [(not (neg? x)) (not (neg? y))]}
     (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
       [(+ x dx) (+ y dy)]))
  ([width height coords]
     {:pre [(pos? width) (pos? height)]}
     (map (fn [[x y]]
            (let [x (if (< x 0) (+ width x) x)
                  y (if (< y 0) (+ height y) y)]
              [(rem x width) (rem y height)]))
          (neighbours coords))))

(defn- transition
  [this grid dead-state tfn]
  (let [{:keys [width height cells]} grid]
    (->>
     (range height)
     (mapcat
      (fn [y] (map (fn [x] (tfn this width height cells x y)) (range width))))
     (remove #(let [[cell] (vals %)] (nil? cell)))
     (remove #(let [[cell] (vals %)] (= (:state cell) dead-state)))
     (reduce merge {})
     (assoc grid :cells))))


(def default-automata
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-initial-state
      [_ state]
      ({:dead :alive :alive :speaking :speaking :dead} state))
    (next-grid [_ grid] grid)

    InformationChannelsSpecification
    (process-command-channel [this ic])
    (fill-output-info-channel [this oc grid])))

(def game-of-life
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-initial-state
      [_ state]
      ({:dead :alive :alive :dead} state))
    (next-grid [this grid]
      (transition
       this grid :dead
       (fn [this width height cells x y]
         (let [get-cell (fn [coords] (get cells coords (default-cell this)))
               n (->> (neighbours width height [x y])
                      (map get-cell)
                      (filter #(= (:state %) :alive))
                      count)
               cell (get-cell [x y])
               alive? (= (:state cell) :alive)]
           {[x y]
            (cond
             (and alive? (or (< n 2) (> n 3))) {:state :dead}
             (and alive? (or (= n 2) (= n 3))) {:state :alive}
             (and (not alive?) (= n 3))        {:state :alive}
             :else                             cell)}))))

    InformationChannelsSpecification
    (process-command-channel [this ic])
    (fill-output-info-channel [this grid oc])))

(defn- get-cell [this cells coords] (get cells coords (default-cell this)))

(defn- ordered-neighbour-states
  [get-cell width height x y]
  (->> (neighbours width height [x y])
       (map (comp :state get-cell))
       (interleave [:top-left :left :bottom-left
                    :top :bottom :top-right
                    :right :bottom-right])
       (apply hash-map)))

(def unrestricted-language-parser
  "Cell automaton that is able to detect whether the given word belongs to the
  unrestricted language L = { ww : w є {a, b}* }."
  (let [ext-command (atom {})]
    (reify
      AutomatonSpecification
      (default-cell [_] {:state :dead})
      (next-initial-state [_ state]
        (or ({:dead :a :a :b :b :dead} state) :dead))
      (next-grid [this grid]
        (transition
         this grid :dead
         (fn [this width height cells x y]
           (let [get-cell (partial get-cell this cells)
                 n-states (ordered-neighbour-states get-cell width height x y)
                 {:keys [top-left top top-right left right
                         bottom-left bottom bottom-right]} n-states
                 cell (get-cell [x y])
                 s (:state cell)
                 letter? #{:a :b}
                 dead? #(= :dead %)
                 alive-only
                 (fn [dir pred]
                   (and (pred (dir n-states))
                        (every? dead? (vals (dissoc n-states dir)))))

                 between-l-r
                 (fn [left-states right-states]
                   (and (left-states (:left n-states))
                        (right-states (:right n-states))))

                 command (get @ext-command :command (fn [_ _] false))]

             {[x y]
              (or (command n-states cell)
                  {:state
                   (cond
                    (and (dead? s) (alive-only :right letter?))    :lc
                    (and (dead? s) (alive-only :left letter?))     :rc
                    (and (dead? s)
                         (or (alive-only :right #{:lc})
                             (alive-only :left #{:rc})))           :x
                    (and (= :lc s)
                         (between-l-r #{:x :a :b :dead} letter?))  right
                    (and (= :rc s)
                         (between-l-r letter? #{:x :a :b :dead}))  left
                    (and (letter? s) (between-l-r #{:lc} letter?)) :lc
                    (and (letter? s) (between-l-r letter? #{:rc})) :rc
                    (between-l-r #{:lc} #{:rc})                    :f
                    (or (and (= :lc s) (= :rc right))
                        (and (= :lc left) (= :rc s)))              :m
                    (and (dead? s) (= :m top))                     :n
                    (and (dead? s) (letter? top) (= :n right))     :n
                    (and (dead? s) (letter? top)
                         (#{:n :a :b} left))                       top
                    (and (= :n s) (letter? right))                 right
                    (and (letter? s) (= :n left))                  :n
                    (and (letter? s) (= :x left) (= s bottom))     :x
                    (and (letter? s) (= :x left)
                         (letter? bottom) (not (= s bottom)))      :f
                    (and (= :m s) (= :x left))                     :s
                    :else                                          s)})}))))

      InformationChannelsSpecification
      (process-command-channel [this ic]
        (go (while true (let [cmd (<! ic)] (reset! ext-command cmd)))))
      (fill-output-info-channel [this oc grid]
        (put! oc (->> grid
                      :cells
                      vals
                      (group-by :state)
                      (#(select-keys % [:s :f]))
                      (map (fn [[k v]] [k (count v)]))
                      (into {})
                      (merge {:s 0 :f 0})))))))


(defn- weighted
  [m]
  (when-not (empty? m)
    (let [w (reductions #(+ % %2) (vals m))
          r (rand (last w))]
      (nth (keys m) (count (take-while #(<= % r) w))))))

;;; from https://groups.google.com/d/msg/clojure/UdFLYjLvNRs/NqlA7wnLCE0J
(defn- deep-merge
  "Recursively merges maps. If vals are not maps, the last value wins."
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(defn corp-keyword [i] (keyword (str "corp-" i)))

(defn- corp-params
  [n pfn]
  (into {}
        (map (fn [i]
               (let [corp (corp-keyword i)]
                 [corp (pfn corp i)]))
             (range 1 (inc n)))))

(def market-model-default-params
  {:taxation-type :rate
   :tax-rate 0.05
   :fixed-tax 20
   :expenditures-per-cell
   {:government 1 :corp 20}
   :depreciation 0.03
   :utility-params
   {:a 1 :p -1 :q 0}
   :utility
   (fn [a p price global-share local-share]
     (* global-share
        (Math/pow local-share a)
        (Math/pow price p)))})

(defn market-model-default-state
  [n]
  (merge market-model-default-params
         {:corp-quantity n
          :iteration 0
          :prices
          (corp-params n (constantly 100))
          :capital
          (merge {:government 1000} (corp-params n (constantly 0)))
          :capital-diff
          (corp-params n (constantly 0))}))

(defn conservative-corp
  [[competitor-count share capital capital-diff price]]
  (cond
   (< capital-diff 0)                        {:type :change-price
                                              :cmd (* price 1.5)}
   (and (< share (/ 1.0 competitor-count))
        (> price 100))                       {:type :change-price
                                              :cmd (/ price 3)}
   (and (< share (/ 1.0 competitor-count))
        (> price 10))                        {:type :change-price
                                              :cmd (/ price 1.5)}
   (and (< share (/ 1.0 competitor-count 2))
        (> price 100))                       {:type :change-price
                                              :cmd (/ price 4)}
   (and (< share (/ 1.0 competitor-count 2))
        (> price 10))                        {:type :change-price
                                              :cmd (/ price 2)}
   (>= share 0.33)                           {:type :change-price
                                              :cmd (* price 1.2)}
   (>= share 0.5)                            {:type :change-price
                                              :cmd (* price 1.5)}
   :else                                     {:type :do-nothing}))

(def market-model
  (let [env (atom (market-model-default-state 4))

        compute-global-share
        (fn [corp-quantity grid]
          (let [cell-count (* (:width grid) (:height grid))
                cells-with-good (vals (:cells grid))
                without-good-count (- cell-count (count cells-with-good))]
            (->> cells-with-good
                 (group-by :state)
                 (merge {:without-good
                         (repeat without-good-count {:state :without-good})})
                 (map (fn [[k v]] [k (/ (count v) cell-count)]))
                 (into (corp-params corp-quantity (constantly 0))))))

        user-preferences
        (fn [env global-share n-states]
          (let [{:keys [utility utility-params prices capital]} env
                available-goods (filter #(>= (get capital % -1)  0)
                                        (keys prices))
                neighbour-count (count n-states)
                corp-quantity (:corp-quantity env)
                local-share
                (->> n-states
                     (group-by second)
                     (map (fn [[k v]] [k (/ (count v) neighbour-count)]))
                     (into (corp-params corp-quantity (constantly 0))))
                utility
                (partial utility
                         (:a utility-params)
                         (:p utility-params))]
            (->> available-goods
                 (map
                  (fn [g]
                    [g (utility (prices g)
                                (global-share g)
                                (local-share g))]))
                 ((fn [p]
                    (let [total-pref (apply + (map second p))]
                      (map (fn [[k v]]
                             [k (if (not (zero? total-pref))
                                  (/ v total-pref)
                                  (/ 1 (count available-goods)))])
                           p))))
                 (into {}))))]
    (reify
      AutomatonSpecification
      (default-cell [_] {:state :without-good})
      (next-initial-state [_ state]
        (let [corp-quantity (:corp-quantity @env)
              corp-states-progression
              (merge {:without-good
                      (if (> corp-quantity 0) :corp-1 :without-good)}
                     (corp-params corp-quantity
                                  (fn [corp i]
                                    (if (< i corp-quantity)
                                      (corp-keyword (inc i))
                                      :without-good))))]
          (get corp-states-progression state :without-good)))
      (next-grid [this grid]
        (let [env-atom env
              env @env
              corp-quantity (:corp-quantity env)
              cell-quantity (* (:width grid) (:height grid))
              global-share (compute-global-share corp-quantity grid)

              update-capitals
              (fn [env key]
                (let [cell-quantity (* cell-quantity (get global-share key 0))
                      income (* cell-quantity (get-in env [:prices key] 0))
                      exp (* cell-quantity
                             (get-in env [:expenditures-per-cell :corp] 0))
                      tax (if (> income 0)
                            (condp = (get env :taxation-type :rate)
                              :rate (* (+ income exp) (get env :tax-rate 0))
                              :income-rate (* income (get env :tax-rate 0))
                              :fixed (get env :fixed-tax 0))
                            0)
                      diff (- income tax exp)]
                  (-> env
                      (update-in [:capital key] + diff)
                      (update-in [:capital :government] + tax)
                      (update-in [:capital-diff key] + diff))))]
          (swap! env-atom
                 (fn [e]
                   (-> (reduce update-capitals e (keys (:prices e)))
                       (update-in [:iteration] inc)
                       (update-in
                        [:capital :government]
                        (fn [capital]
                          (- capital
                             (* cell-quantity
                                (get-in e [:expenditures-per-cell :government]
                                        0))))))))
          (transition
           this grid :without-good
           (fn [this width height cells x y]
             (let [get-cell (partial get-cell this cells)
                   n-states
                   (ordered-neighbour-states get-cell width height x y)

                   {:keys [top-left top top-right left right
                           bottom-left bottom bottom-right]}
                   n-states

                   cell (get-cell [x y])
                   s (:state cell)
                   without-good? (fn [s] (= :without-good s))
                   avg-price (/ (apply + (vals (:prices env)))
                                (:corp-quantity env))
                   current-price (get-in env [:prices s])
                   bankrupt? (fn [c] (and
                                     (not (without-good? s))
                                     (< (get-in env [:capital c] -1) 0)))]
               {[x y]
                {:state
                 (cond
                  (or (< (rand) (* (:depreciation env)
                                   (/ current-price avg-price)))
                      (bankrupt? s))
                  :without-good

                  (without-good? s)
                  (if-let [c (weighted
                              (user-preferences env global-share n-states))]
                    c
                    :without-good)

                  :else s)}})))))

      InformationChannelsSpecification
      (process-command-channel [this ic]
        (go
          (while true
            (let [msg (<! ic)
                  {:keys [type cmd]} msg]
              (condp = type
                :reset
                (reset! env
                        (market-model-default-state (:corp-quantity @env)))

                :change-corp-quantity
                (reset! env (market-model-default-state cmd))

                :change-price
                (let [[corp price] cmd]
                  (swap! env #(-> %
                                  (assoc-in [:prices corp] price)
                                  (assoc-in [:capital-diff corp] 0))))

                :do-nothing
                nil

                :config
                (swap! env deep-merge cmd))))))
      (fill-output-info-channel [this oc grid]
        (let [env @env]
          (put! oc (-> env
                       (dissoc :utility)
                       (assoc :global-user-share
                         (compute-global-share
                          (:corp-quantity env) grid)))))))))
