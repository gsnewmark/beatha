(ns beatha.automaton)

(enable-console-print!)

(defprotocol AutomatonSpecification
  "Describes basic interactions with particular set of cellular automata
  rules."
  (default-cell [this] "Default cell of the given automata.")
  (next-initial-state [this state]
    "Returns next possible initial state after the given one.")
  (next-grid [this grid]
    "Transforms given grid according to automata's rules."))

;;; State of world is described as a set of living cells, where each cell is
;;; vector of coordinates [x y].

(defn neighbours
  "Generates seq with neighbour cells of the given cell.

  Intended to work with a two-dimensional grid representation where each cell
  is encoded as pair of it's coordinates in a world."
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))

(def default-automata
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-initial-state
      [_ state]
      ({:dead :alive :alive :speaking :speaking :dead} state))
    (next-grid [_ grid] grid)))

(def game-of-life
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-initial-state
      [_ state]
      ({:dead :alive :alive :dead} state))
    (next-grid [this grid]
      (let [{:keys [width height cells]} grid]
        (->> (mapcat
               (fn [y]
                 (map
                  (fn [x]
                    (let [get-cell (fn [coords]
                                     (get cells coords (default-cell this)))
                          n (->> (neighbours [x y])
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
                        :else                             cell)}))
                  (range width)))
               (range height))
             (remove #(let [[cell] (vals %)] (nil? cell)))
             (remove #(let [[cell] (vals %)] (= (:state cell) :dead)))
             (reduce merge {})
             (assoc grid :cells))))))

(def unrestricted-language-parser
  "Cell automaton that is able to detect whether the given word belongs to the
  unrestricted language L = { ww : w є {a, b}* }."
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-initial-state
      [_ state]
      ({:dead :a :a :b :b :dead} state))
    (next-grid [this grid]
      (let [{:keys [width height cells]} grid]
        (->> (mapcat
               (fn [y]
                 (map
                  (fn [x]
                    (let [get-cell (fn [coords]
                                     (get cells coords (default-cell this)))
                          n-states (->> (neighbours [x y])
                                       (map (comp :state get-cell))
                                       (interleave [:top-left :left
                                                    :bottom-left :top
                                                    :bottom :top-right
                                                    :right :bottom-right])
                                       (apply hash-map))
                          {:keys [top-left top top-right left right
                                  bottom-left bottom bottom-right]} n-states
                          cell (get-cell [x y])
                          state (:state cell)
                          all-dead-except
                          (fn [chosen-n chosen-states]
                            (and
                             (chosen-states (chosen-n n-states))
                             (every? #{:dead}
                                     (vals (dissoc n-states chosen-n)))))

                          between-l-r
                          (fn [left-states right-states]
                            (and (left-states (:left n-states))
                                 (right-states (:right n-states))))]
                      {[x y]
                       (cond
                        (and (= :dead state)
                             (all-dead-except :right #{:a :b}))
                        {:state :lc}

                        (and (= :dead state)
                             (all-dead-except :left #{:a :b}))
                        {:state :rc}

                        (and (= :dead state)
                             (or (all-dead-except :right #{:lc})
                                 (all-dead-except :left #{:rc})))
                        {:state :x}

                        (and (= :lc state)
                             (between-l-r #{:x :a :b :dead} #{:a :b}))
                        {:state right}

                        (and (= :rc state)
                             (between-l-r #{:a :b} #{:x :a :b :dead}))
                        {:state left}

                        (and (#{:a :b} state)
                             (between-l-r #{:lc} #{:a :b}))
                        {:state :lc}

                        (and (#{:a :b} state)
                             (between-l-r #{:a :b} #{:rc}))
                        {:state :rc}

                        (between-l-r #{:lc} #{:rc})
                        {:state :f}

                        (or (and (= :lc state) (= :rc right))
                            (and (= :lc left) (= :rc state)))
                        {:state :m}

                        :else cell)}))
                  (range width)))
               (range height))
             (remove #(let [[cell] (vals %)] (nil? cell)))
             (remove #(let [[cell] (vals %)] (= (:state cell) :dead)))
             (reduce merge {})
             (assoc grid :cells))))))
