(ns beatha.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [beatha.automaton :as a]))

(enable-console-print!)

(def app-state {:automaton {:grid {:width 10 :height 10 :cells {}}
                            :display {:width 600 :height 600}
                            :util {:started false}}
                :command {}
                :output {}})

(defn automaton-view [data owner]
  (reify
    om/IRender
    (render [_]
      (println "render automaton")
      (dom/b nil (get-in data [:grid :width])))))

(defn command-view [data owner]
  (reify
    om/IRender
    (render [_]
      (println "render command")
      (dom/input #js {:type "text"}))))

(defn root-view [data owner]
  (reify
    om/IRender
    (render [_]
      (println "render parent")
      (dom/div #js {:className "row"}
               (dom/h2 nil "Hello")
               (om/build command-view (:command data))
               (om/build automaton-view (:automaton data))))))


(defn handle-numeric-config-change
  [e owner state key]
  (let [value (.. e -target -value)]
    (if (re-matches #"[0-9]+" value)
      (om/set-state! owner key (js/parseInt value))
      (om/set-state! owner key (get state key)))))

(defn navigation-button
  [text f]
  (dom/button
   #js {:type "button"
        :className "btn btn-success btn-lg btn-block"
        :onClick f}
   text))


(defn grid-config-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [width height change-grid-dimensions] :as state}]
      (println "render grid config")
      (let [started (:started data)]
        (dom/div
         #js {:role "form" :className "automaton-grid-control"}
         (dom/label nil "Grid width")
         (dom/input
          #js {:type "text" :className "form-control" :value width
               :disabled started
               :onChange
               #(handle-numeric-config-change % owner state :width)})
         (dom/label nil "Grid height")
         (dom/input
          #js {:type "text" :className "form-control" :value height
               :disabled started
               :onChange
               #(handle-numeric-config-change % owner state :height)})
         (dom/button
          #js {:type "button" :className "btn btn-danger btn-lg btn-block"
               :disabled started
               :onClick #(put! change-grid-dimensions [width height])}
          "Reset grid"))))))


(defn display-config-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [width height change-display-dimensions]
                      :as state}]
      (let [started (:started data)]
        (dom/div
         #js {:role "form" :className "automaton-display-control"}
         (dom/label nil "Display width")
         (dom/input
          #js {:type "text" :className "form-control" :value width
               :disabled started
               :onChange
               #(handle-numeric-config-change % owner state :width)})
         (dom/label nil "Display height")
         (dom/input
          #js {:type "text" :className "form-control" :value height
               :disabled started
               :onChange
               #(handle-numeric-config-change % owner state :height)})
         (dom/button
          #js {:type "button" :className "btn btn-warning btn-lg btn-block"
               :disabled started
               :onClick #(put! change-display-dimensions [width height])}
          "Reset display"))))))

(defn cell-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [cell-state-changed x y]}]
      (let [{:keys [width height state started]} data
            st #js {:width width :height height}]
        (dom/div #js {:style st
                      :className
                      (apply str
                             (interpose " " ["automaton-cell" (name state)]))
                      :onClick #(when-not started
                                  (put! cell-state-changed [x y]))})))))


(defn grid-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (let [grid-width (get-in data [:grid :width])
            grid-height (get-in data [:grid :height])
            cell-width (/ (get-in data [:display :width]) grid-width)
            cell-height (/ (get-in data [:display :height]) grid-height)
            default-cell (:default-cell state)]
        (println "render grid")
        (apply dom/div #js {:className "automaton-grid"}
               (mapv (fn [y]
                       (apply dom/div #js {:className "automaton-row row"}
                              (mapv
                               (fn [x]
                                 (om/build
                                  cell-view
                                  (merge (get-in data [:grid :cells [x y]]
                                                 default-cell)
                                         {:started (:started data)
                                          :width cell-width
                                          :height cell-height})
                                  {:init-state
                                   {:cell-state-changed
                                    (:cell-state-changed state)

                                    :x x :y y}}))
                               (range grid-width))))
                     (range grid-height)))))))


(declare render-menu-view)

(defn gen-app-view
  [automaton-spec]
  (fn [data owner]
    (reify
      om/IInitState
      (init-state [_]
        {:change-grid-dimensions (chan)
         :change-display-dimensions (chan)
         :cell-state-changed (chan)
         :started (chan)
         :output-info-channel (chan)
         :input-info-channel (chan)})
      om/IWillMount
      (will-mount [_]
        (let [grid-c (om/get-state owner :change-grid-dimensions)
              display-c (om/get-state owner :change-display-dimensions)
              cell-state-c (om/get-state owner :cell-state-changed)
              started-c (om/get-state owner :started)
              output-info-c (om/get-state owner :output-info-channel)
              input-info-c (om/get-state owner :input-info-channel)]
          (go (while true
                (alt!
                  grid-c
                  ([[width height]]
                     #_(automaton-input-reset customization input-info-c)
                     #_(automaton-output-reset customization data owner)
                     (om/transact!
                      data [:automaton :grid]
                      (fn [grid]
                        (assoc grid
                          :width width :height height :cells {}))))

                  display-c
                  ([[width height]]
                     (om/transact!
                      data [:automato :display]
                      (fn [display]
                        (assoc display :width width :height height))))

                  cell-state-c
                  ([[x y]]
                     (om/transact!
                      data [:automaton :grid :cells]
                      (fn [grid]
                        (let [cell (get grid [x y]
                                        (a/default-cell automaton-spec))
                              state (:state cell)
                              cell (assoc cell
                                     :state
                                     (a/next-initial-state automaton-spec
                                                           state))]
                          (assoc grid [x y] cell)))))

                  started-c
                  ([started]
                     (om/update! data [:automaton :util :started] started)
                     (if started
                       (om/set-state!
                        owner :update-loop-id
                        (js/setInterval
                         (fn []
                           ;(om/transact! data [:automaton :grid :width] inc)
                           (om/transact! data [:automaton :grid]
                              (partial a/next-grid automaton-spec)))
                         (or (om/get-state owner :animation-step) 1000)))
                       (when-let [id (om/get-state owner :update-loop-id)]
                         (js/clearTimeout id)))))))))
      om/IRenderState
      (render-state [_ state]
        (println "render app-view")
        (dom/div
         #js {:className "container-liquid"}
         (dom/div
          #js {:className "row"}
          (navigation-button "Menu" render-menu-view)
          (dom/hr nil))
         (dom/div
          #js {:className "row"}
          (dom/div
           #js {:className "col-sm-2"}
           (let [started (get-in data [:automaton :util :started])]
             (dom/div
              #js {:className "row"}
              (dom/button
               #js {:type "button"
                    :className "btn btn-primary btn-lg btn-block"
                    :onClick #(put! (:started state) (not started))}
               (if started "Stop" "Start"))))
           (dom/hr nil)
           (dom/div
            #js {:className "row"}
            (om/build grid-config-view
                      (get-in data [:automaton :util])
                      {:init-state
                       {:change-grid-dimensions
                        (:change-grid-dimensions state)
                        :width (get-in data [:grid :width])
                        :height (get-in data [:grid :height])}}))
           (dom/hr nil)
           (dom/div
            #js {:className "row"}
            (om/build display-config-view
                      (get-in data [:automaton :util])
                      {:init-state
                       {:change-display-dimensions
                        (:change-display-dimensions state)
                        :width (get-in data [:display :width])
                        :height (get-in data [:display :height])}})))
          (dom/div
           #js {:className "col-sm-10"}
           (dom/div
            #js {:className "row"}
            (om/build grid-view
                      (:automaton data)
                      {:init-state
                       {:cell-state-changed (:cell-state-changed state)
                        :default-cell
                        (a/default-cell automaton-spec)}})))))))))


(defn render-cellular-automaton
  [view]
  (om/root
   view
   app-state
   {:target (. js/document (getElementById "app"))
    :init-state {:animation-step 500}}))


(defn menu-view
  [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "app-menu"}
        (dom/div #js {:className "page-header"}
          (dom/h1 nil "Cellular automata experiments"))
        (navigation-button
         "Game of Life"
         (partial render-cellular-automaton (gen-app-view a/game-of-life)))
        #_(navigation-button
         "Unrestricted language parser"
         (partial render-cellular-automaton
                  (gen-app-view
                   a/unrestricted-language-parser
                   unrestricted-language-parser-customization)))
        #_(navigation-button
         "Economic model"
         (partial render-cellular-automaton
                  (gen-app-view
                   a/market-model market-model-customization)))))))

(defn render-menu-view
  []
  (om/root
   menu-view
   app-state
   {:target (. js/document (getElementById "app"))}))

(render-cellular-automaton (gen-app-view a/game-of-life))
