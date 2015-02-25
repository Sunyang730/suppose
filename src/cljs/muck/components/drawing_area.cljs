(ns muck.drawing_area
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [monet.canvas :as canvas]
            [cljs.core.async :refer [put! chan <!]]
            [muck.version_control :as vc]))

;;TD:
;;draw to make line
;;list branches
;;make new branch
;;Go bewteen branches

(defn log [thing]
  (.log js/console (pr-str thing)))

(defn console-log [thing]
  (.log js/console thing))

(defn to-rgb [{:keys [r g b]}]
  (str "rgb(" r "," g "," b ")"))

(defn draw-line [ctx {:keys [pos-attrs style-attrs]}]
     (if (not (nil? ctx))
       (do
         (-> ctx
             (canvas/begin-path)
             (canvas/stroke-style (to-rgb (:stroke style-attrs)))
             (canvas/stroke-width 2)
             ((fn [ctx]
               (reduce (fn [ctx [x y]]
                         (do
                           (canvas/line-to ctx x y)
                           ctx)) ctx pos-attrs)))
             (canvas/stroke)
             (canvas/close-path))
         nil)))

(defn draw-all-lines [monet-canvas paths]
  (if (not (nil? monet-canvas))
    (do
    (log "render")
    (canvas/clear-rect (:ctx monet-canvas) {:x 0 :y 0 :w 500 :h 500})
    (doseq [path paths]
               (draw-line (:ctx monet-canvas) path)))))


(defn last-ind [v]
 (- (count v) 1))

(defn make-action [e-type event]
  {:type e-type
   :x (.-offsetX (.-nativeEvent event))
   :y (.-offsetY (.-nativeEvent event))})

;;Variable amounts of arguments to funcitons?

;;When something is drawn it must fire on the canvas and be added to history.

(defn start-path [ctx style-attrs]
  (-> ctx
     (canvas/begin-path)
     (canvas/stroke-style (to-rgb (:stroke style-attrs)))
     (canvas/stroke-width 1)))

(defn extend-path [ctx [x y]]
  (-> ctx
    (canvas/line-to x y)
    (canvas/stroke)))

(defn draw-circle [ctx [x y] style-attrs]
  (log [ctx (last pos-attrs)])
  (-> ctx
   (canvas/fill-style (to-rgb (:stroke style-attrs)))
   (canvas/circle {:x x :y y :r 4})
   (canvas/fill)))


(def event-callbacks
 {:mouseDown (fn [app-state event ctx]
              (om/transact! app-state
                            (fn [{:keys [history active-commit mousePosition branches active-branch rgb] :as app-state}]
                                   (let [new-commit (vc/create-commit {:shape-type :line
                                                                        :pos-attrs [[(:x event) (:y event)]]
                                                                        :style-attrs {:stroke rgb}} (active-commit history))
                                         new-history (vc/add-commit history new-commit active-commit)
                                         last-state (last (:state new-commit))]
                                    (do
                                     (start-path ctx (:style-attrs last-state))
                                     (extend-path ctx (last (:pos-attrs last-state)))
                                     (assoc app-state :mouseDown? true
                                                      :history new-history
                                                      :active-commit (:location new-commit)
                                                      :branches (assoc branches active-branch (:location new-commit)))) ))))

 :mouseUp (fn [app-state _ _]
            (om/transact! app-state :mouseDown? (fn [_] false)))

 :mouseMove (fn [app-state event ctx]
              (if (:mouseDown? event)
                 (om/transact! app-state
                   (fn [{:keys [history active-commit] :as app-state}]
                     (do
                      (extend-path ctx [(:x event) (:y event)])
                     (update-in app-state [:history active-commit :state]
                                (fn [state-vec]
                                 (update-in state-vec [(last-ind state-vec) :pos-attrs]
                                            (fn [step]
                                              (conj step [(:x event) (:y event)]))))))))))})

(defn view [{:keys [history active-commit mouseDown? canvas-width canvas-height rgb] :as app-state} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:canvas-events (chan)})
    om/IWillMount
    (will-mount [_]
      (let [canvas-events (om/get-state owner :canvas-events)]
        (go (loop []
          (let [event (<! canvas-events)
                ctx (:ctx (om/get-state owner :monet-canvas))]
            (((:type event) event-callbacks) app-state event ctx)
            (recur))))))
    om/IDidMount
    (did-mount [_]
      (let [dom-element (om/get-node owner)
            resize-func (fn []
                          (om/update! app-state :canvas-width (.-offsetWidth (.-parentNode dom-element))))]
      (resize-func)
      (om/set-state! owner :monet-canvas (canvas/init dom-element "2d"))
      (aset js/window "onresize" resize-func)))
    om/IRenderState
    (render-state [this {:keys [monet-canvas canvas-events]}]
      (dom/canvas #js {
                       :width (str canvas-width "px")
                       :height (str canvas-height "px")
                       :onMouseMove (fn [e]
                                      (put! canvas-events (merge (make-action :mouseMove e) {:mouseDown? mouseDown?})))
                       :onMouseDown (fn [e]
                                      (put! canvas-events (make-action :mouseDown e)))
                       :onMouseUp (fn [e]
                                    (put! canvas-events (make-action :mouseUp e)))
                      }
             ;;(draw-all-lines monet-canvas (:state (active-commit history)))
                  ))))



;;Can put canvas stuff inside as long as it evals to nil?
;;SVG will go the auto diffing for me for changes... (would be easier...)
;;Could write both.... branches...
