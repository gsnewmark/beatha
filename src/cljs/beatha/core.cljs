(ns beatha.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [beatha.automaton :as a]))

(enable-console-print!)

(def app-state {:automaton {:grid {:width 10 :height 10 :cells {}}
                            :display {:width 600 :height 600}
                            :started false}
                :command {}})

(defn automaton-view [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (om/set-state!
       owner :update-loop-id
       (js/setInterval
        (fn []
          (om/transact! data [:grid :width] inc))
        (or (om/get-state owner :animation-step) 1000))))
    om/IRender
    (render [_]
      (println "automaton")
      (dom/b nil (get-in data [:grid :width])))))

(defn command-view [data owner]
  (reify
    om/IRender
    (render [_]
      (println "command")
      (dom/input #js {:type "text"}))))

(defn root-view [data owner]
  (reify
    om/IRender
    (render [_]
      (println "parent")
      (dom/div #js {:className "row"}
               (dom/h2 nil "Hello")
               (om/build command-view (:command data))
               (om/build automaton-view (:automaton data))))))


(om/root
   root-view
   app-state
   {:target (. js/document (getElementById "app"))})
