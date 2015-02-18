(ns muck.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [muck.version_control :as vc]
            [muck.drawing_area :as drawing-area]
            [muck.keypress :as keypress]
            [muck.commit :as commit]))

;;If a have an active branch do I need an active commit?

(defonce app-state (atom {:history {:start {:location :start :state []}}
                          :active-commit :start
                          :branches {:master :start}
                          :active-branch :master}))

(comment (reset! app-state {:history {}
                            :active-commit :start}))

@app-state

(defn branches-to-commits [branches-map history]
  (vec (map (fn [[branch-name most-recent-commit]]
         {:display-name (name branch-name)
          :commit-state (:state (most-recent-commit history))}) branches-map)))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [this]
          (dom/div nil
                   (om/build drawing-area/view app)
                   (apply dom/div nil (om/build-all commit/commit-view (branches-to-commits (:branches app) (:history app))))
                   (om/build keypress/key-listener app)))))
    app-state
    {:target (. js/document (getElementById "app"))}))
