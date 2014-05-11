(ns beatha.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [beatha.automaton :as a]))

(enable-console-print!)

(def app-state {:grid {:width 10 :height 10 :cells {}}
                :display {:width 600 :height 600}
                :started false})


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
        (apply dom/div #js {:className "automaton-grid"}
               (mapv (fn [y]
                       (apply dom/div #js {:className "automaton-row row"}
                              (mapv
                               (fn [x]
                                 (om/build
                                  cell-view
                                  (merge (get-in data [:grid :cells [x y]]
                                                 default-cell)
                                         {:started (:started data)}
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

(defn navigation-button
  [text f]
  (dom/button #js {:type "button"
                   :className "btn btn-success btn-lg btn-block"
                   :onClick f}
    text))


(defprotocol CellularAutomatonAppCustomization
  (automaton-input-view [this]
    "Generates Om component that renders block for sending commands to
    automaton.")
  (automaton-input-reset [this input-info-channel]
    "Resets any changes created by the command sent to the automata.")
  (automaton-output-handler [this data owner o]
    "Handles messages posted by the cellular automaton.")
  (automaton-output-view [this]
    "Generates Om component that renders any changes produced by
    the handler.")
  (automaton-output-reset [this data owner]
    "Resets changes produced by the handler."))

(def ^:private empty-view
  (fn [data owner] (reify om/IRender (render [_] (dom/span nil "")))))

(declare render-menu-view)

(defn gen-app-view
  ([automaton-spec]
     (gen-app-view automaton-spec
                   (reify
                     CellularAutomatonAppCustomization
                     (automaton-input-view [_] empty-view)
                     (automaton-input-reset [_ _])
                     (automaton-output-handler [_ _ _ _])
                     (automaton-output-view [_] empty-view)
                     (automaton-output-reset [_ _ _]))))
  ([automaton-spec customization]
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
             (a/process-info-channel automaton-spec input-info-c)
             (go (while true
                   (alt!
                     grid-c
                     ([[width height]]
                        (automaton-input-reset customization input-info-c)
                        (automaton-output-reset customization data owner)
                        (om/transact!
                         data :grid
                         (fn [grid]
                           (assoc grid
                             :width width :height height :cells {}))))

                     display-c
                     ([[width height]]
                        (om/transact!
                         data :display
                         (fn [display]
                           (assoc display :width width :height height))))

                     cell-state-c
                     ([[x y]]
                        (om/transact!
                         data [:grid :cells]
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
                        (om/update! data :started started)
                        (if started
                          (om/set-state!
                           owner :update-loop-id
                           (js/setInterval
                            (fn []
                              (om/transact!
                               data :grid
                               (partial a/next-grid automaton-spec))
                              (a/fill-output-info-channel
                               automaton-spec (:grid @data) output-info-c))
                            (or (om/get-state owner :animation-step) 1000)))
                          (when-let [id (om/get-state owner :update-loop-id)]
                            (js/clearTimeout id))))

                     output-info-c
                     ([o] (automaton-output-handler
                           customization data owner o)))))))
         om/IRenderState
         (render-state [_ state]
           (dom/div
            #js {:className "container-liquid"}
            (dom/div #js {:className "row"}
                     (navigation-button "Menu" render-menu-view)
                     (dom/hr nil))
            (dom/div
             #js {:className "row"}
             (dom/div
              #js {:className "col-sm-2"}
              (let [started (:started data)]
                (dom/div
                 #js {:className "row"}
                 (dom/button
                  #js {:type "button"
                       :className "btn btn-primary btn-lg btn-block"
                       :onClick #(put! (:started state) (not started))}
                  (if-not started "Start" "Stop"))))
              (dom/div #js {:className "row"}
                       (om/build (automaton-output-view customization) data))
              (dom/hr nil)
              (dom/div #js {:className "row"}
                       (om/build
                        (automaton-input-view customization)
                        data
                        {:init-state
                         (select-keys state [:input-info-channel])}))
              (dom/div
               #js {:className "row"}
               (om/build grid-config-view
                         data
                         {:init-state
                          (merge state
                                 {:width (get-in data [:grid :width])
                                  :height (get-in data [:grid :height])})}))
              (dom/hr nil)
              (dom/div
               #js {:className "row"}
               (om/build display-config-view
                         data
                         {:init-state
                          (assoc state
                            :width (get-in data [:display :width])
                            :height (get-in data [:display :height]))})))
             (dom/div #js {:className "col-sm-10"}
                      (dom/div #js {:className "row"})
                      (om/build grid-view
                                data
                                {:init-state
                                 (assoc state
                                   :default-cell
                                   (a/default-cell automaton-spec))})))))))))


(def unrestricted-language-parser-customization
  (reify
    CellularAutomatonAppCustomization
    (automaton-input-view [this]
      (fn [data owner]
        (reify
          om/IRenderState
          (render-state [_ state]
            (let [c (:input-info-channel state)
                  a-wildcard (get-in data [:command :a-wildcard])
                  b-wildcard (get-in data [:command :b-wildcard])

                  wildcard-fn
                  (fn [wildcard neighbour-states cell]
                    (let [{:keys [top-left top top-right left right
                                  bottom-left bottom bottom-right]}
                          neighbour-states]
                      (if (and (#{:a :b} (:state cell)) (= :x left)
                               (= wildcard bottom))
                        {:state :x}
                        false)))
                  a-wildcard-fn (partial wildcard-fn :a)
                  b-wildcard-fn (partial wildcard-fn :b)]
              (dom/div nil
               (dom/label nil "a as a wildcard")
               (dom/input
                #js {:type "checkbox"
                     :checked a-wildcard
                     :onClick
                     (fn [] (om/transact! data [:command :a-wildcard] not))})
               (dom/label nil "b as a wildcard")
               (dom/input
                #js {:type "checkbox"
                     :checked b-wildcard
                     :onClick
                     (fn [] (om/transact! data [:command :b-wildcard] not))})
               (dom/button
                #js {:type "button"
                     :className "btn btn-success btn-lg btn-block"
                     :onClick
                     #(cond
                       (and b-wildcard (not a-wildcard))
                       (put! c {:command b-wildcard-fn})

                       (and a-wildcard (not b-wildcard))
                       (put! c {:command a-wildcard-fn})

                       (and a-wildcard b-wildcard)
                       (put! c {:command
                                (fn [neigbours s]
                                  (if-let [a-res (a-wildcard-fn neigbours s)]
                                    a-res
                                    (b-wildcard-fn neigbours s)))})

                       :else (fn [_ _] false))}
                "Send command")
               (dom/button
                #js {:type "button"
                     :className "btn btn-info btn-lg btn-block"
                     :onClick #(automaton-input-reset this c)}
                "Reset command")
               (dom/hr nil)))))))
    (automaton-input-reset [_ input-channel] (put! input-channel {}))
    (automaton-output-handler [_ data owner {:keys [s f]}]
      (when (or (> s 0) (> f 0))
        (om/transact!
         data (fn [d] (assoc d :result (if (> s 0) :success :failure))))
        (put! (om/get-state owner :started) false)))
    (automaton-output-view [_]
      (fn [data _]
        (reify
          om/IRender
          (render [_]
            (dom/div #js {:className "row"
                          :hidden (not (#{:success :failure} (:result data)))}
                     (dom/h2 nil
                             "Result: "
                             (if (= :success (:result data))
                               "word is parsed"
                               "word is not parsed")))))))
    (automaton-output-reset [_ data _]
      (om/transact! data (fn [d] (assoc d :result :none))))))


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
        (navigation-button
         "Unrestricted language parser"
         (partial render-cellular-automaton
                  (gen-app-view
                   a/unrestricted-language-parser
                   unrestricted-language-parser-customization)))
        (navigation-button
         "Economic model"
         (partial render-cellular-automaton
                  (gen-app-view a/labour-market-model)))))))

(defn render-menu-view
  []
  (om/root
   menu-view
   app-state
   {:target (. js/document (getElementById "app"))}))


(render-menu-view)
