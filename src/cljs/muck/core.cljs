(ns muck.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [muck.version_control :as vc]
            [muck.keypress :as keypress]
            [muck.commit :as commit]
            [cljs.core.async :refer [put! chan <!]]
            [muck.color-picker :as cp]
            [muck.drawing_area :as drawing-area]))

;;If a have an active branch do I need an active commit?

(defonce app-state (atom {:history {:start {:location :start :state []}}
                          :active-commit :start
                          :branches {:master :start}
                          :active-branch :master
                          :canvas-width 500
                          :canvas-height 500
                          :rgb {:r 50 :g 50 :b 50}}))


;;(:hash1 (:history @app-state))
;;(:canvas-width @app-state)
;;State
;;[ [ [1 2][2 2][2 3] ]  ]
;;I do this function on every mouse drag.
(defn branches-to-commits [{:keys [active-commit branches history active-branch canvas-width canvas-height]} commit-chan]
  (vec (map (fn [[branch-name most-recent-commit]]
         {:display-name (name branch-name)
          :commit-state (:state (most-recent-commit history))
          :commit-chan commit-chan
          :canvas-width canvas-width
          :canvas-height canvas-height
          :active-commit active-commit
          :most-recent-commit most-recent-commit}) branches)))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IInitState
        (init-state [_]
          {:click-chan (chan)
           :color-channel (chan)})
        om/IWillMount
        (will-mount [_]
         (let [click-chan (om/get-state owner :click-chan)
               color-chan (om/get-state owner :color-channel)]
          (go (loop []
            (let [event-map (<! click-chan)
                  branch-name (:branch-name (:data event-map))]
              (om/transact! app (fn [app-state]
                                  (assoc app-state :active-branch branch-name
                                                   :active-commit (branch-name (:branches app-state)))))
            (recur))))
          (go (loop []
            (let [{:keys [r g b] :as rgb} (<! color-chan)]
              (om/transact! app (fn [app-state]
                                  (assoc app-state :rgb rgb)))
            (recur))))))
        om/IRenderState
        (render-state [this {:keys [click-chan color-channel]}]
          (dom/div #js {:className "container"}
              (dom/div #js {:className "drawspace row"}
                   (dom/div #js {:className "ten columns"}
                           (om/build drawing-area/view app))
                   (dom/div #js {:className "two columns"}
                           (om/build cp/color-picker {:size 100 :color-channel color-channel})
                           ;;(apply dom/div nil (om/build-all ))
                            ))
              (dom/div #js {:className "row"}
                   (apply dom/div #js {:className "branches twelve columns"}
                          (dom/div #js {:className "tree"}
                                   "TREE")
                          (om/build-all commit/commit-view (branches-to-commits app click-chan))))
                   (om/build keypress/key-listener app)))))
    app-state
    {:target (. js/document (getElementById "app"))}))
