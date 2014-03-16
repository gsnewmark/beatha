(ns beatha.core.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [beatha.automaton :as a]))

(enable-console-print!)

(def app-state (atom {:grid {:width 10 :height 10 :cells {}}
                      :display {:width 600 :height 600}}))


(defn cell-view
  [cell owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [cell-state-changed x y]}]
      (let [{:keys [width height state]} cell
            st #js {:width width :height height}]
        (dom/div #js {:style st
                      :className
                      (apply str
                             (interpose " " ["automaton-cell" (name state)]))
                      :onClick #(put! cell-state-changed [x y])})))))


(defn grid-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (let [grid-width (get-in data [:grid :width])
            grid-height (get-in data [:grid :height])
            cell-width (/ (get-in data [:display :width]) grid-width)
            cell-height (/ (get-in data [:display :height]) grid-height)
            cells (get-in data [:grid :cells])]
        (apply dom/div #js {:className "automaton-grid"}
               (mapv (fn [y]
                       (apply dom/div #js {:className "automaton-row row"}
                              (mapv
                               (fn [x]
                                 (om/build
                                  cell-view
                                  (merge (get-in data [:grid :cells [x y]]
                                                 a/default-cell)
                                         {:width cell-width
                                          :height cell-height})
                                  {:init-state
                                   {:cell-state-changed
                                    (:cell-state-changed state)

                                    :x x :y y}}))
                               (range grid-width))))
                     (range grid-height)))))))


(defn handle-numeric-config-change
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
       (dom/label nil "Grid width")
       (dom/input
        #js {:type "text" :className "form-control" :value width
             :onChange #(handle-numeric-config-change % owner state :width)})
       (dom/label nil "Grid height")
       (dom/input
        #js {:type "text" :className "form-control" :value height
             :onChange #(handle-numeric-config-change % owner state :height)})
       (dom/button
        #js {:type "submit" :className "btn btn-primary btn-lg btn-block"
             :onClick #(put! change-grid-dimensions [width height])}
        "Reset grid")))))


(defn display-config-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [width height change-display-dimensions]
                      :as state}]
      (dom/div
       #js {:role "form" :className "automaton-display-control"}
       (dom/label nil "Display width")
       (dom/input
        #js {:type "text" :className "form-control" :value width
             :onChange #(handle-numeric-config-change % owner state :width)})
       (dom/label nil "Display height")
       (dom/input
        #js {:type "text" :className "form-control" :value height
             :onChange #(handle-numeric-config-change % owner state :height)})
       (dom/button
        #js {:type "submit" :className "btn btn-primary btn-lg btn-block"
             :onClick #(put! change-display-dimensions [width height])}
        "Reset display")))))


(defn app-view
  [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:change-grid-dimensions (chan)
       :change-display-dimensions (chan)
       :cell-state-changed (chan)})
    om/IWillMount
    (will-mount [_]
      (let [grid-c (om/get-state owner :change-grid-dimensions)
            display-c (om/get-state owner :change-display-dimensions)
            cell-state-c (om/get-state owner :cell-state-changed)]
        (go (loop []
              (let [[width height] (<! grid-c)]
                (om/transact! data :grid
                  (fn [grid] (assoc grid :width width :height height :cells {})))
                (recur))))
        (go (loop []
              (let [[width height] (<! display-c)]
                (om/transact! data :display
                  (fn [display] (assoc display :width width :height height)))
                (recur))))
        (go (loop []
              (let [[x y] (<! cell-state-c)]
                (om/transact!
                 data [:grid :cells]
                 (fn [grid]
                   (let [cell (get grid [x y] a/default-cell)
                         state (:state cell)
                         cell (assoc cell :state (a/next-state state))]
                     (assoc grid [x y] cell))))
                (recur))))))
    om/IRenderState
    (render-state [_ state]
      (dom/div
       #js {:className "container-liquid"}
       (dom/div
        #js {:className "row"}
        (dom/div
         #js {:className "col-sm-2"}
         (dom/div
          #js {:className "row"}
          (om/build grid-config-view
                    data
                    {:init-state
                     (merge state
                            {:width (get-in data [:grid :width])
                             :height (get-in data [:grid :height])})}))
         (dom/div
          #js {:className "row"}
          (om/build display-config-view
                    data
                    {:init-state
                     (merge state
                            {:width (get-in data [:display :width])
                             :height (get-in data [:display :height])})})))
        (dom/div #js {:className "col-sm-10"}
                 (om/build grid-view data {:init-state state})))))))


(om/root app-view app-state {:target (. js/document (getElementById "app"))})
