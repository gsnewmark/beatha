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

    - input channel is a source of external commands which augment rules of
      automata;
    - output channel is filled by the automata itself with aggregate
      information about current state."
  (process-info-channel [this ic]
    "Handles messages from the input information channel.")
  (fill-output-info-channel [this grid oc]
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
  ([this grid tfn] (transition this grid :dead tfn))
  ([this grid dead-state tfn]
     (let [{:keys [width height cells]} grid]
       (->>
        (range height)
        (mapcat
         (fn [y] (map (fn [x] (tfn this width height cells x y))
                     (range width))))
        (remove #(let [[cell] (vals %)] (nil? cell)))
        (remove #(let [[cell] (vals %)] (= (:state cell) dead-state)))
        (reduce merge {})
        (assoc grid :cells)))))


(def default-automata
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-initial-state
      [_ state]
      ({:dead :alive :alive :speaking :speaking :dead} state))
    (next-grid [_ grid] grid)

    InformationChannelsSpecification
    (process-info-channel [this ic])
    (fill-output-info-channel [this grid oc])))

(def game-of-life
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-initial-state
      [_ state]
      ({:dead :alive :alive :dead} state))
    (next-grid [this grid]
      (transition
       this grid
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
             (and alive? (< n 2))              {:state :dead}
             (and alive? (or (= n 2) (= n 3))) {:state :alive}
             (and alive? (> n 3))              {:state :dead}
             (and (not alive?) (= n 3))        {:state :alive}
             :else                             cell)}))))

    InformationChannelsSpecification
    (process-info-channel [this ic])
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
         this grid
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
      (process-info-channel [this ic]
        (go (while true (let [cmd (<! ic)] (reset! ext-command cmd)))))
      (fill-output-info-channel [this grid oc]
        (put! oc (->> grid
                      :cells
                      vals
                      (group-by :state)
                      (#(select-keys % [:s :f]))
                      (map (fn [[k v]] [k (count v)]))
                      (into {})
                      (merge {:s 0 :f 0})))))))

(def labour-market-model
  (let [state (atom {:government {:welfare-rate 0.2
                                  :max-welfare 10
                                  :tax-rate 0.95
                                  :capital 1000}
                     :job-offer-chance 0.2
                     :companies
                     {:corp-1 {:capital 100
                               :greediness 0.5
                               :productivity (+ 15 (rand 10))}
                      :corp-2 {:capital 100
                               :greediness 0.5
                               :productivity (+ 15 (rand 10))}
                      :corp-3 {:capital 100
                               :greediness 0.5
                               :productivity (+ 15 (rand 10))}
                      :corp-4 {:capital 100
                               :greediness 0.5
                               :productivity (+ 15 (rand 10))}}})
        company-worker {:corp-1 :worker-1 :corp-2 :worker-2
                        :corp-3 :worker-3 :corp-4 :worker-4}
        worker? #((into #{:worker} (vals company-worker)) %)
        company-of-worker (fn [s] (or ((set/map-invert company-worker) s) s))
        worker-of-company (fn [s] (or (company-worker s) s))]
    (reify
      AutomatonSpecification
      (default-cell [_] {:state :worker})
      (next-initial-state [_ state]
        (or ({:worker :corp-1 :corp-1 :corp-2 :corp-2 :corp-3
              :corp-3 :corp-4 :corp-4 :worker} state) :worker))
      (next-grid [this grid]
        (let [sstate @state
              {:keys [width height cells]} grid
              workers-count (- (* width height) (count cells))
              wage (fn [state c]
                     (if-let [corp (get (:companies state) c)]
                       (let [{:keys [capital productivity greediness]} corp]
                         (->> (get-in state [:government :tax-rate])
                              (- 1)
                              (* productivity)
                              (* (- 1 greediness))))
                       (let [{:keys [welfare-rate max-welfare capital]}
                             (:government state)]
                         (min max-welfare
                              (/ (* capital welfare-rate) workers-count)))))]
          (transition
           this grid :worker
           (fn [this width height cells x y]
             (let [get-cell (partial get-cell this cells)
                   n-states
                   (ordered-neighbour-states get-cell width height x y)

                   {:keys [top-left top top-right left right
                           bottom-left bottom bottom-right]}
                   n-states

                   cell (get-cell [x y])
                   s (:state cell)
                   current-company (company-of-worker s)

                   company-income
                   (if current-company
                     (get-in sstate [:companies current-company :productivity])
                     0)
                   current-wage (wage sstate current-company)
                   current-tax
                   (* company-income (get-in sstate [:government :tax-rate]))]
               (swap! state
                      (fn [s]
                        (-> s
                            (update-in [:government :capital]
                                       #(+ % current-tax))
                            ((fn [s]
                               (if (worker? current-company)
                                 (update-in s [:government :capital]
                                            #(- % current-wage))
                                 (update-in s [:companies current-company :capital]
                                            #(- (+ % company-income)
                                                current-wage current-tax))))))))
               {[x y]
                {:state (if-not (worker? s)
                          s
                          (->> (vals n-states)
                               (map company-of-worker)
                               (filter #(not (#{(company-of-worker s)} %)))
                               (into #{})
                               (filter #(<= (rand) (:job-offer-chance sstate)))
                               (map (fn [o] [o (wage sstate o)]))
                               (reduce (fn [[b-s b-w] [c-s c-w]]
                                         (if (>= c-w b-w)
                                           [(worker-of-company c-s) c-w]
                                           [b-s b-w]))
                                       [s current-wage])
                               first))}})))))

      InformationChannelsSpecification
      (process-info-channel [this ic])
      (fill-output-info-channel [this grid oc]))))
