(ns muck.commit
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [monet.canvas :as canvas]))

(defn commit-view [{:keys [display-name commit]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:length 50})
    om/IDidMount
    (did-mount [_]
      (om/set-state! owner :monet-canvas (canvas/init (om/get-node owner) "2d")))
    om/IRenderState
    (render-state [this {:keys [monet-canvas length]}]
      (dom/div nil
               (dom/canvas #js {:width (str length "px")  :height (str length "px")} nil)
               (dom/p nil display-name)))))

