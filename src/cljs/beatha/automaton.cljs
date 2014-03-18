(ns beatha.automaton)

(defprotocol AutomatonSpecification
  "Describes basic interactions with particular set of cellular automata
  rules."
  (default-cell [this] "Default cell of the given automata.")
  (next-state [this state] "Returns next possible state after the given one.")
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
    (next-state
      [_ state]
      ({:dead :alive :alive :speaking :speaking :dead} state))
    (next-grid [_ grid] grid)))

(def game-of-life
  (reify
    AutomatonSpecification
    (default-cell [_] {:state :dead})
    (next-state
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
                          state (:state (get-cell [x y]))
                          alive? (= state :alive)]
                      {[x y]
                       (cond
                        (and alive? (< n 2)) {:state :dead}
                        (and alive? (or (= n 2) (= n 3))) {:state :alive}
                        (and alive? (> n 3)) {:state :dead}
                        (and (not alive?) (= n 3)) {:state :alive})}))
                  (range width)))
               (range height))
             (remove #(let [[cell] (vals %)] (nil? cell)))
             (remove #(let [[cell] (vals %)] (= (:state cell) :dead)))
             (reduce merge {})
             (assoc grid :cells))))))
