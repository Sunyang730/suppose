(ns muck.commit
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [monet.canvas :as canvas]
            [muck.drawing_area :as drawing-area]
            [cljs.core.async :refer [put! chan <!]]))

;;define canvas size global vars.
(defn drawn-canvas [{:keys [canvas-width canvas-height state]}  owner]
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
                (.log js/console canvas-width canvas-height)
                (dom/canvas #js {:width (str length "px")  :height (str length "px")}
                           (if (not (nil? monet-canvas))
                            (do
                             (canvas/save (:ctx monet-canvas))
                             (canvas/scale (:ctx monet-canvas) (/ length canvas-width) (/ length canvas-height))
                             (drawing-area/draw-all-lines monet-canvas state)
                             (canvas/restore (:ctx monet-canvas))
                             nil)
                            nil)))))


;;You can transact on cursors within the app state I believe
(defn commit-view [{:keys [display-name commit-state commit-chan canvas-width canvas-height]} owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:onClick (fn [e]
                           (put! commit-chan {:action-type :commit
                                              :event-type :onClick
                                              :event e
                                              :data {:branch-name (keyword display-name)}}))}
               (om/build drawn-canvas {:state commit-state :canvas-width canvas-width :canvas-height canvas-height} )
               (dom/p nil display-name)))))
