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

;;Next prevent the re-renders on the commit windows.
;;the ctx's for the main window may need to be in the application state.
;;Maybe just the bottom canvas.
;;After that fix clicking on a commit window.
;;How will the main canvas know what commit to update?
;;What is an elegant solution for this?

;;Questiona about whether to re-render: is it the right branch? Is the active-commit new?
;;Could have a draw in progress atom?

(def line-width 2)

(defn draw-line [ctx {:keys [pos-attrs style-attrs]}]
     (if (not (nil? ctx))
       (do
         (-> ctx
             (canvas/begin-path)
             (canvas/stroke-style (to-rgb (:stroke style-attrs)))
             (canvas/stroke-width line-width)
             ((fn [ctx]
               (reduce (fn [ctx [x y]]
                         (do
                           (canvas/line-to ctx x y)
                           ctx)) ctx pos-attrs)))
             (canvas/stroke)
             (canvas/close-path))
         nil)))

(defn draw-all-lines [monet-canvas paths [canvas-width canvas-height]]
  (if (not (nil? monet-canvas))
    (do
      (canvas/clear-rect (:ctx monet-canvas) {:x 0 :y 0 :w canvas-width :h canvas-height})
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
     (canvas/stroke-width line-width)))

(defn extend-path [ctx [x y]]
  (-> ctx
    (canvas/line-to x y)
    (canvas/stroke)))

(defn draw-circle [ctx [x y] style-attrs]
  (-> ctx
   (canvas/fill-style (to-rgb (:stroke style-attrs)))
   (canvas/circle {:x x :y y :r 4})
   (canvas/fill)))

(defn get-ctx [canvas-element]
  (.getContext canvas-element "2d"))

;;Maybe its snowing song really good 21 min

;;Mouse down: init line on top canvas
;;Mouse move: extend line on top canvas. Clearing along the way.
;;Mouse up: clear top canvas - kick top line to the back canvas

(defn get-active-shape-state [{:keys [active-commit history] :as app-state}]
  (:state (active-commit history)))

(defn get-most-recent-shape [{:keys [active-commit history] :as app-state}]
  (last (get-active-shape-state app-state)))


(def event-callbacks
 {:mouseDown (fn [app-state event [top-ctx bottom-ctx] [width height]]
              ;;(.log js/console "pen down")
              (om/transact! app-state
                            (fn [{:keys [history active-commit mousePosition branches active-branch rgb] :as app-state}]
                                   (let [new-commit (vc/create-commit {:shape-type :line
                                                                        :pos-attrs [[(:x event) (:y event)]]
                                                                        :style-attrs {:stroke rgb}} (active-commit history))
                                         new-history (vc/add-commit history new-commit active-commit)
                                         last-state (last (:state new-commit))]
                                    (do
                                     (start-path top-ctx (:style-attrs last-state))
                                     (extend-path top-ctx (last (:pos-attrs last-state)))
                                     (assoc app-state :mouseDown? true
                                                      :history new-history
                                                      :active-commit (:location new-commit)
                                                      :branches (assoc branches active-branch (:location new-commit)))) ))))

 :mouseUp (fn [app-state event [top-ctx bottom-ctx] [width height]]
            (do
              (canvas/clear-rect top-ctx {:x 0 :y 0 :w width :h height})
              ;;This is a hack. There is a case in my code where mouse down has not updated yey, but the move event fires.
              ;;I need to fix that.
              (canvas/begin-path top-ctx)
              (draw-line bottom-ctx (get-most-recent-shape @app-state))
              (om/transact! app-state :mouseDown? (fn [_] false))))

 :mouseMove (fn [app-state event [top-ctx bottom-ctx] [width height]]
              ;;(.log js/console (str "movin " (:mouseDown? event)) )
              (if (:mouseDown? event)
                 (om/transact! app-state
                   (fn [{:keys [history active-commit] :as app-state}]
                     (do
                      (canvas/clear-rect top-ctx {:x 0 :y 0 :w width :h height})
                      (extend-path top-ctx [(:x event) (:y event)])
                     (update-in app-state [:history active-commit :state]
                                (fn [state-vec]
                                 (update-in state-vec [(last-ind state-vec) :pos-attrs]
                                            (fn [step]
                                              (let [[last-x last-y] (last step)]
                                               (if (and (= last-x (:x event)) (= last-y (:y event)))
                                                step
                                                (conj step [(:x event) (:y event)]))))))))))))})



(defn view [{:keys [history active-commit mouseDown? canvas-width canvas-height rgb] :as app-state} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:canvas-events (chan)
       })
    om/IWillMount
    (will-mount [_]
      (let [canvas-events (om/get-state owner :canvas-events)]
        (go (loop []
          (let [event (<! canvas-events)
                top-ctx (om/get-state owner :top-ctx)
                bottom-ctx (om/get-state owner :bottom-ctx)]
            (((:type event) event-callbacks) app-state event [top-ctx bottom-ctx] [(:canvas-width event) (:canvas-height event)])
            (recur))))))
    om/IDidMount
    (did-mount [_]
      (let [dom-element (om/get-node owner)
            dom-children (.-children dom-element)
            topLayerDom (aget dom-children 0)
            bottomLayerDom (aget dom-children 1)
            resize-func (fn []
                          (om/update! app-state :canvas-width (.-offsetWidth (.-parentNode dom-element))))]
      (resize-func)
      (om/update-state! owner (fn [state]
                                (assoc state
                                  :top-ctx (get-ctx topLayerDom)
                                  :bottom-ctx (get-ctx bottomLayerDom))))
      (aset js/window "onresize" resize-func)))
    om/IWillReceiveProps
      (will-receive-props [this next-props]
                  (let [width (:canvas-width next-props)
                        height (:canvas-height next-props)]
                   (if (not (= (:active-commit next-props) (:active-commit (om/get-props owner))))
                     (do
                      (canvas/clear-rect (om/get-state owner :top-ctx) {:x 0 :y 0 :w width :h height})
                      (draw-all-lines {:ctx (om/get-state owner :bottom-ctx)} (get-active-shape-state next-props) [width height])
                      (om/set-state! owner :active-branch (:active-branch next-props))))))
    om/IRenderState
    (render-state [this {:keys [top-ctx bottom-ctx canvas-events]}]
      (dom/div #js {:className "canvasContainer"}
        (dom/canvas #js {
                         :className "topLayer"
                         :width (str canvas-width "px")
                         :height (str canvas-height "px")
                         :onMouseMove (fn [e]
                                        (put! canvas-events (merge (make-action :mouseMove e) {:mouseDown? mouseDown?
                                                                                               :canvas-width canvas-width
                                                                                               :canvas-height canvas-height})))
                         :onMouseDown (fn [e]
                                        (put! canvas-events (merge (make-action :mouseDown e) {:canvas-width canvas-width
                                                                                               :canvas-height canvas-height}) ))
                         :onMouseUp (fn [e]
                                      (put! canvas-events (merge (make-action :mouseUp e) {:canvas-width canvas-width
                                                                                           :canvas-height canvas-height}) ))
                        })
        (dom/canvas #js {:className "bottomLayer"
                        :width (str canvas-width "px")
                        :height (str canvas-height "px")}))  )))



;;Can put canvas stuff inside as long as it evals to nil?
;;SVG will go the auto diffing for me for changes... (would be easier...)
;;Could write both.... branches...
