(ns muck.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [muck.version_control :as vc]
            [muck.drawing_area :as drawing-area]
            [muck.keypress :as keypress]))

(defonce app-state (atom {:history {}
                          :active-commit :start}))

(comment (reset! app-state {:history {}
                            :active-commit :start}))



(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [this]
          (dom/div nil
                   (om/build drawing-area/view app)
                   (om/build keypress/key-listener app)))))
    app-state
    {:target (. js/document (getElementById "app"))}))


 
