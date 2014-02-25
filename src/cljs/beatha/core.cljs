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


(defn handle-config-change
  [e owner state key]
  (let [value (.. e -target -value)]
    (if (re-matches #"[0-9]+" value)
      (om/set-state! owner key value)
      (om/set-state! owner key (get state key)))))

(defn grid-config-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [width height change-grid-dimensions] :as state}]
      (dom/div
       #js {:role "form" :className "automaton-grid-control"}
       (dom/input
        #js {:type "text" :className "form-control" :value width
             :onChange #(handle-config-change % owner state :width)})
       (dom/input
        #js {:type "text" :className "form-control" :value height
             :onChange #(handle-config-change % owner state :height)})
       (dom/button
        #js {:type "submit" :className "btn btn-primary btn-lg btn-block"
             :onClick
             #(put! change-grid-dimensions [width height])} "Reset")))))


(defn app-view
  [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:change-grid-dimensions (chan)})
    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :change-grid-dimensions)]
        (go (loop []
              (let [[width height] (<! c)]
                (om/transact! data :grid
                  (fn [grid] (assoc grid :width width :height height)))
                (recur))))))
    om/IRenderState
    (render-state [_ state]
      (dom/div
       #js {:className "container-liquid"}
       (dom/div
        #js {:className "row"}
        (dom/div #js {:className "col-sm-2"}
                 (om/build grid-config-view
                           data
                           {:init-state
                            (merge state
                                   {:width (get-in data [:grid :width])
                                    :height (get-in data [:grid :height])})}))
        (dom/div #js {:className "col-sm-10"}
                 (om/build grid-view data)))))))


(om/root app-view
         app-state
         {:target (. js/document (getElementById "app"))})
