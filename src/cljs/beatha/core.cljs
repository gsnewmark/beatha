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
                :command {}})


(defn handle-int-config-change
  [e owner state key]
  (let [value (.. e -target -value)]
    (if (re-matches #"-?[0-9]+" value)
      (om/set-state! owner key (js/parseInt value))
      (om/set-state! owner key (get state key)))))

(defn handle-float-config-change
  [e owner state key]
  (let [value (.. e -target -value)]
    (if (re-matches #"-?[0-9]+(\\.[0-9]+)?" value)
      (om/set-state! owner key (js/parseFloat value))
      (om/set-state! owner key (get state key)))))

(defn navigation-button
  [text f]
  (dom/button
   #js {:type "button"
        :className "btn btn-success btn-lg btn-block"
        :onClick f}
   text))

(def ^:private empty-view
  (fn [data owner] (reify om/IRender (render [_] (dom/span nil "")))))

(defn num->percent [n] (str (Math/floor (* n 100)) "%"))

(defn normalize
  [n min max]
  (cond
   (< n min) min
   (> n max) max
   :else n))

(defn is-number? [v] (not (js/isNaN v)))

(defn keyword->str
  [k]
  (when (keyword? k)
    (.substr (str k) 1)))


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
               #(handle-int-config-change % owner state :width)})
         (dom/label nil "Grid height")
         (dom/input
          #js {:type "text" :className "form-control" :value height
               :disabled started
               :onChange
               #(handle-int-config-change % owner state :height)})
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
               #(handle-int-config-change % owner state :width)})
         (dom/label nil "Display height")
         (dom/input
          #js {:type "text" :className "form-control" :value height
               :disabled started
               :onChange
               #(handle-int-config-change % owner state :height)})
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


(defprotocol CellularAutomatonAppCustomization
  (automaton-command-view [this]
    "Generates Om component that renders block for sending commands to
    automaton. Please ensure the same component is returned on each call and
    the new one is generated (i. e., anonymous function).")
  (automaton-command-initial-state [this]
    "Returns the default stated for the command view.")
  (automaton-command-reset [this command-info-channel]
    "Resets any changes created by the command sent to the automata.")
  (automaton-output-handler [this data owner msg]
    "Handles messages posted by the cellular automaton.")
  (automaton-output-view [this]
    "Generates Om component that renders any changes produced by
    the handler. Please ensure the same component is returned on each call and
    the new one is generated (i. e., anonymous function).")
  (automaton-output-reset [this data owner]
    "Resets changes produced by the handler."))


(declare render-menu-view)

(defn gen-app-view
  ([automaton-spec]
     (gen-app-view automaton-spec
                   (reify
                     CellularAutomatonAppCustomization
                     (automaton-command-view [_] empty-view)
                     (automaton-command-initial-state [_] {})
                     (automaton-command-reset [_ _])
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
            :command-info-channel (chan)})
         om/IWillMount
         (will-mount [_]
           (let [grid-c (om/get-state owner :change-grid-dimensions)
                 display-c (om/get-state owner :change-display-dimensions)
                 cell-state-c (om/get-state owner :cell-state-changed)
                 started-c (om/get-state owner :started)
                 output-info-c (om/get-state owner :output-info-channel)
                 command-info-c (om/get-state owner :command-info-channel)]
             (a/process-command-channel automaton-spec command-info-c)
             (go (while true
                   (alt!
                     grid-c
                     ([[width height]]
                        (automaton-command-reset customization command-info-c)
                        (automaton-output-reset customization data owner)
                        (om/transact!
                         data [:automaton :grid]
                         (fn [grid]
                           (assoc grid
                             :width width :height height :cells {}))))

                     display-c
                     ([[width height]]
                        (om/transact!
                         data [:automaton :display]
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
                              (om/transact!
                               data
                               [:automaton :grid]
                               (partial a/next-grid automaton-spec))
                              (a/fill-output-info-channel
                               automaton-spec
                               output-info-c
                               (get-in @data [:automaton :grid])))
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
              (dom/div
               #js {:className "row"}
               (om/build (automaton-output-view customization) data))
              (dom/hr nil)
              (dom/div
               #js {:className "row"}
               (om/build
                (automaton-command-view customization)
                (:command data)
                {:init-state
                 (merge
                  (select-keys state [:command-info-channel])
                  (automaton-command-initial-state customization))}))
              (dom/div
               #js {:className "row"}
               (om/build grid-config-view
                         (get-in data [:automaton :util])
                         {:init-state
                          {:change-grid-dimensions
                           (:change-grid-dimensions state)
                           :width (get-in data [:automaton :grid :width])
                           :height
                           (get-in data [:automaton :grid :height])}}))
              (dom/hr nil)
              (dom/div
               #js {:className "row"}
               (om/build display-config-view
                         (get-in data [:automaton :util])
                         {:init-state
                          {:change-display-dimensions
                           (:change-display-dimensions state)
                           :width (get-in data [:automaton :display :width])
                           :height
                           (get-in data [:automaton :display :height])}})))
             (dom/div
              #js {:className "col-sm-10"}
              (dom/div
               #js {:className "row"}
               (om/build grid-view
                         (:automaton data)
                         {:init-state
                          {:cell-state-changed (:cell-state-changed state)
                           :default-cell
                           (a/default-cell automaton-spec)}}))))))))))


(declare unrestricted-language-parser-customization)

(defn unrestricted-language-parser-command-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (let [c (:command-info-channel state)
            a-wildcard (:a-wildcard data)
            b-wildcard (:b-wildcard data)

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
        (dom/div
         nil
         (dom/label nil "a as a wildcard")
         (dom/input
          #js {:type "checkbox"
               :checked a-wildcard
               :onClick
               (fn [] (om/transact! data [:a-wildcard] not))})
         (dom/label nil "b as a wildcard")
         (dom/input
          #js {:type "checkbox"
               :checked b-wildcard
               :onClick
               (fn [] (om/transact! data [:b-wildcard] not))})
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
               :onClick
               #(automaton-command-reset
                 unrestricted-language-parser-customization c)}
          "Reset command")
         (dom/hr nil))))))

(defn unrestricted-language-parser-output-view
  [data _]
  (reify
    om/IRender
    (render [_]
      (dom/div
       #js {:className "row"
            :hidden (not (#{:success :failure} (:result data)))}
       (dom/h2
        nil
        "Result: " (if (= :success (:result data))
                     "word is parsed"
                     "word is not parsed"))))))

(def unrestricted-language-parser-customization
  (reify
    CellularAutomatonAppCustomization
    (automaton-command-view [this] unrestricted-language-parser-command-view)
    (automaton-command-initial-state [_] {})
    (automaton-command-reset [_ input-channel] (put! input-channel {}))
    (automaton-output-handler [_ data owner {:keys [s f]}]
      (when (or (> s 0) (> f 0))
        (om/transact!
         data (fn [d] (assoc d :result (if (> s 0) :success :failure))))
        (put! (om/get-state owner :started) false)))
    (automaton-output-view [_] unrestricted-language-parser-output-view)
    (automaton-output-reset [_ data _]
      (om/transact! data (fn [d] (assoc d :result :none))))))


(defn market-command-view
  [data owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [tax-rate fixed-tax depreciation
                             a p q command-info-channel
                             taxation-type]
                      :as state}]
      (dom/div
       #js {:role "form" :className "economic-model-control"}

       (dom/label nil "Taxation type")
       (dom/div
        #js {:className "radio"}
        (dom/input
         #js {:type "radio" :name "taxation-type" :value "rate"
              :checked (= :rate taxation-type)
              :onChange #(om/set-state! owner :taxation-type :rate)}
         "Rate"))
       (dom/div
        #js {:className "radio"}
        (dom/input
         #js {:type "radio" :name "taxation-type" :value "fixed"
              :checked (= :fixed taxation-type)
              :onChange #(om/set-state! owner :taxation-type :fixed)}
         "Fixed"))

       (dom/span
        #js {:hidden (not= :rate taxation-type)}
        (dom/label nil "Tax rate (%)")
        (dom/input
         #js {:type "text" :className "form-control" :value tax-rate
              :onChange
              #(handle-int-config-change % owner state :tax-rate)}))

       (dom/span
        #js {:hidden (not= :fixed taxation-type)}
        (dom/label nil "Fixed tax")
        (dom/input
         #js {:type "text" :className "form-control" :value fixed-tax
              :onChange
              #(handle-int-config-change % owner state :fixed-tax)}))

       (dom/label nil "Depreciation rate (%)")
       (dom/input
        #js {:type "text" :className "form-control" :value depreciation
             :onChange
             #(handle-int-config-change % owner state :depreciation)})

       (dom/label nil "Utilty param a")
       (dom/input
        #js {:type "text" :className "form-control" :value a
             :onChange #(handle-float-config-change % owner state :a)})

       (dom/label nil "Utilty param p")
       (dom/input
        #js {:type "text" :className "form-control" :value p
             :onChange #(handle-float-config-change % owner state :p)})

       (dom/label nil "Utilty param q")
       (dom/input
        #js {:type "text" :className "form-control" :value q
             :onChange #(handle-float-config-change % owner state :q)})

       (dom/button
        #js {:type "button"
             :className "btn btn-success btn-lg btn-block"
             :onClick
             #(->> {:tax-rate
                    (normalize (/ (js/parseFloat tax-rate) 100) 0 1.0)
                    :depreciation
                    (normalize (/ (js/parseFloat depreciation) 100) 0 1.0)
                    :fixed-tax (js/parseInt fixed-tax)}
                   (filter (comp is-number? second))
                   (into {:taxation-type taxation-type})
                   (merge
                    {:utility-params (->> {:a (js/parseFloat a)
                                           :p (js/parseFloat p)
                                           :q (js/parseFloat q)}
                                          (filter (comp is-number? second))
                                          (into {}))})
                   (put! command-info-channel))}
        "Send command")

       (dom/button
        #js {:type "button"
             :className "btn btn-info btn-lg btn-block"
             :onClick
             #(put! command-info-channel
                    (a/market-model-default-params
                     (count (:prices a/market-model-default-state))))}
        "Reset command")

       (dom/hr nil)))))

(defn market-output-view
  [data owner]
  (let [get-market-info (fn [path] (get-in data (cons :market-state path)))]
    (reify
      om/IRender
      (render [_]
        (let [taxation-type (get-market-info [:taxation-type])]
          (dom/div
           #js {:className "row"
                :hidden (not (contains? data :market-state))}
           (dom/b nil "Taxation type: ")
           (dom/span nil (keyword->str taxation-type))
           (dom/div nil)

           (dom/span
            #js {:hidden (not= :rate taxation-type)}
            (dom/b nil "Tax rate: ")
            (dom/span
             nil
             (num->percent (get-market-info [:tax-rate])))
            (dom/div nil))

           (dom/span
            #js {:hidden (not= :fixed taxation-type)}
            (dom/b nil "Fixed tax: ")
            (dom/span nil (get-market-info [:fixed-tax]))
            (dom/div nil))

           (dom/b nil "Depreciation rate: ")
           (dom/span
            nil
            (num->percent (get-market-info [:depreciation])))
           (dom/div nil)

           (dom/b nil "Utility function params: ")
           (dom/div
            nil
            (dom/span nil "a: " (get-market-info [:utility-params :a]))
            (dom/span nil " p: " (get-market-info [:utility-params :p]))
            (dom/span nil " q: " (get-market-info [:utility-params :q])))

           (dom/b nil "Capital")
           (dom/div
            nil
            "Government: " (get-market-info [:capital :government]))
           (dom/div
            #js {:className "corp-1"} "Corporation 1: "
            (get-market-info [:capital :corp-1]))
           (dom/div
            #js {:className "corp-2"} "Corporation 2: "
            (get-market-info [:capital :corp-2]))
           (dom/div
            #js {:className "corp-3"} "Corporation 3: "
            (get-market-info [:capital :corp-3]))
           (dom/div
            #js {:className "corp-4"} "Corporation 4: "
            (get-market-info [:capital :corp-4]))

           (dom/b nil "Market share")
           (dom/div
            #js {:className "without-good"}
            "Without good: "
            (num->percent
             (get-market-info [:global-user-share :without-good])))
           (dom/div
            #js {:className "corp-1"} "Corporation 1: "
            (num->percent (get-market-info [:global-user-share :corp-1])))
           (dom/div
            #js {:className "corp-2"} "Corporation 2: "
            (num->percent (get-market-info [:global-user-share :corp-2])))
           (dom/div
            #js {:className "corp-3"} "Corporation 3: "
            (num->percent (get-market-info [:global-user-share :corp-3])))
           (dom/div
            #js {:className "corp-4"} "Corporation 4: "
            (num->percent (get-market-info [:global-user-share :corp-4])))

           (dom/b nil "Good prices")
           (dom/div
            #js {:className "corp-1"} "Corporation 1: "
            (get-market-info [:prices :corp-1]))
           (dom/div
            #js {:className "corp-2"} "Corporation 2: "
            (get-market-info [:prices :corp-2]))
           (dom/div
            #js {:className "corp-3"} "Corporation 3: "
            (get-market-info [:prices :corp-3]))
           (dom/div
            #js {:className "corp-4"} "Corporation 4: "
            (get-market-info [:prices :corp-4]))

           (dom/b nil "Good quality")
           (dom/div
            #js {:className "corp-1"} "Corporation 1: "
            (get-market-info [:quality :corp-1]))
           (dom/div
            #js {:className "corp-2"} "Corporation 2: "
            (get-market-info [:quality :corp-2]))
           (dom/div
            #js {:className "corp-3"} "Corporation 3: "
            (get-market-info [:quality :corp-3]))
           (dom/div
            #js {:className "corp-4"} "Corporation 4: "
            (get-market-info [:quality :corp-4]))))))))

(def market-model-customization
  (reify
    CellularAutomatonAppCustomization
    (automaton-command-view [_] market-command-view)
    (automaton-command-initial-state [_]
      (let [params (a/market-model-default-params
                    (count (:prices a/market-model-default-state)))]
        (merge (select-keys params [:fixed-tax :taxation-type])
               (:utility-params params)
               {:tax-rate
                (apply str (butlast (num->percent (:tax-rate params))))
                :depreciation
                (apply str
                       (butlast (num->percent (:depreciation params))))})))
    (automaton-command-reset [_ command-channel]
      (put! command-channel a/market-model-default-state))
    (automaton-output-handler [_ data owner msg]
      (om/transact! data (fn [d] (assoc d :market-state msg))))
    (automaton-output-view [_] market-output-view)
    (automaton-output-reset [_ data _]
      (om/transact! data (fn [d] (dissoc d :market-state))))))


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
                  (gen-app-view
                   a/market-model market-model-customization)))))))

(defn render-menu-view
  []
  (om/root
   menu-view
   app-state
   {:target (. js/document (getElementById "app"))}))

(render-menu-view)
