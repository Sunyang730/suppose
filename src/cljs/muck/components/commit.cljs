(ns muck.commit
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [monet.canvas :as canvas]
            [muck.drawing_area :as drawing-area]))

;;define canvas size global vars.
(defn drawn-canvas [state owner]
 (reify
  om/IInitState
    (init-state [_]
      {:length 50})
  om/IDidMount
    (did-mount [_]
      (om/set-state! owner :monet-canvas
                     (canvas/init (om/get-node owner) "2d")))
  om/IRenderState
  (render-state [this {:keys [monet-canvas length]}]
                (dom/canvas #js {:width (str length "px")  :height (str length "px")}
                           (if (not (nil? monet-canvas))
                            (do
                             (canvas/clear-rect (:ctx monet-canvas) {:x 0 :y 0 :w 500 :h 500})
                             (canvas/save (:ctx monet-canvas))
                             (canvas/scale (:ctx monet-canvas)  .1 .1)
                             (drawing-area/draw-all-lines monet-canvas state)
                             (canvas/restore (:ctx monet-canvas))
                             nil)
                            nil)))))





;;You can transact on cursors within the app state I believe
(defn commit-view [{:keys [display-name commit-state]} owner]
  (reify
    om/IRender
    (render [this]
      (dom/div {:onClick (fn [e]
                           (om/transact! app-state (fn [as]
                                                     (merge as {:active-commit 1
                                                                :active-branch 1}))))}
               (om/build drawn-canvas commit-state)
               (dom/p nil display-name)))))
