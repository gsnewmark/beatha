(ns beatha.core.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(def app-state (atom {:grid {:width 10 :height 10}}))


(defn alive-cell-view
  [cell owner]
  (reify
    om/IRender
    (render [_]
      (dom/span #js {:className "cell alive"} "x"))))

(defn dead-cell-view
  [cell owner]
  (reify
    om/IRender
    (render [_]
      (dom/span #js {:className "cell dead"} "_"))))

(defmulti cell-view (fn [cell _] (:state cell)))

(defmethod cell-view :alive
  [cell owner] (alive-cell-view cell owner))

(defmethod cell-view :dead
  [cell owner] (dead-cell-view cell owner))


(defn grid-view
  [data owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div #js {:className "automaton-grid"}
             (mapv (fn [y]
                     (apply dom/div #js {:className "automaton-row"}
                            (mapv (fn [x] (+ x y))
                                  (range (get-in data [:grid :width])))))
                   (range (get-in data [:grid :height])))))))


(om/root grid-view
         app-state
         {:target (. js/document (getElementById "app"))})
