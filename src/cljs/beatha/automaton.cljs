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
