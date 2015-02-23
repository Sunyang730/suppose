(ns muck.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [muck.version_control :as vc]
            [muck.drawing_area :as drawing-area]
            [muck.keypress :as keypress]
            [muck.commit :as commit]
            [cljs.core.async :refer [put! chan <!]]
            [muck.color-picker :as cp]))

;;If a have an active branch do I need an active commit?

(defonce app-state (atom {:history {:start {:location :start :state []}}
                          :active-commit :start
                          :branches {:master :start}
                          :active-branch :master
                          :canvas-width 500
                          :canvas-height 500}))


(defn branches-to-commits [branches-map history commit-chan [canvas-width canvas-height]]
  (vec (map (fn [[branch-name most-recent-commit]]
         {:display-name (name branch-name)
          :commit-state (:state (most-recent-commit history))
          :commit-chan commit-chan
          :canvas-width canvas-width
          :canvas-height canvas-height}) branches-map)))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IInitState
        (init-state [_]
          {:click-chan (chan)})
        om/IWillMount
        (will-mount [_]
         (let [click-chan (om/get-state owner :click-chan)]
          (go (loop []
            (let [event-map (<! click-chan)
                  branch-name (:branch-name (:data event-map))]
              (om/transact! app (fn [app-state]
                                  (assoc app-state :active-branch branch-name
                                                   :active-commit (branch-name (:branches app-state)))))
            (recur))))))
        om/IRenderState
        (render-state [this {:keys [click-chan]}]
          (dom/div #js {:className "container"}
              (dom/div #js {:className "row"}
                   (dom/div #js {:className "ten columns"}
                           (om/build drawing-area/view app)
                   (apply dom/div #js {:className "branches"}
                          (dom/div #js {:className "tree"}
                                   "TREE")
                          (om/build-all commit/commit-view (branches-to-commits (:branches app)
                                                                                (:history app)
                                                                                click-chan
                                                                                [(:canvas-width app) (:canvas-height app)]))))
                   (dom/div #js {:className "two columns"}
                           (dom/p nil "sidepanel")
                           (om/build cp/color-picker {:size 500})))
                   (om/build keypress/key-listener app)))))
    app-state
    {:target (. js/document (getElementById "app"))}))
