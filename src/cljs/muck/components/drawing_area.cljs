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

(defn draw-line [ctx line-vec]
     (if (not (nil? ctx))
       (do
         (-> ctx
             (canvas/begin-path)
             (canvas/stroke-style "#191d21")
             (canvas/stroke-width 4)
             ((fn [ctx]
               (reduce (fn [ctx [x y]]
                         (do
                           (canvas/line-to ctx x y)
                           ctx)) ctx line-vec)))
             (canvas/stroke)
             (canvas/close-path))
         nil)))

(defn last-ind [v]
 (- (count v) 1))

(defn make-action [e-type event]
  {:type e-type
   :x (.-offsetX (.-nativeEvent event))
   :y (.-offsetY (.-nativeEvent event))})

;;Variable amounts of arguments to funcitons?

;;Why handle such simple logic with core async?? Why not just do a callback?
(defn view [{:keys [history active-commit mouseDown?] :as app-state} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:canvas-events (chan)})
    om/IWillMount
    (will-mount [_]
      (let [canvas-events (om/get-state owner :canvas-events)]
        (go (loop []
          (let [event (<! canvas-events)]
            ;;Seperate the recording of events from
            (cond
             (= (:type event) "mouseDown") (om/transact! app-state (fn [{:keys [history active-commit mousePosition] :as app-state}]
                                                                (let [new-commit (vc/create-commit [[(:x event) (:y event)]] (active-commit history))
                                                                      new-history (vc/add-commit history new-commit active-commit)]
                                                                  (assoc app-state :mouseDown? true
                                                                                   :history new-history
                                                                                   :active-commit (:location new-commit)))))
             (= (:type event) "mouseUp") (om/transact! app-state :mouseDown? (fn [_] false))
             (= (:type event) "mouseMove")
               (if (:mouseDown? event)
                 (om/transact! app-state
                   (fn [{:keys [history active-commit] :as app-state}]
                     (update-in app-state [:history active-commit :state]
                                (fn [state-vec]
                                 (update-in state-vec [(last-ind state-vec)]
                                            (fn [step]
                                              (conj step [(:x event) (:y event)])))))))
                 nil))

            (recur))))))
    om/IDidMount
    (did-mount [_]
      (om/set-state! owner :monet-canvas (canvas/init (om/get-node owner) "2d")))
    om/IRenderState
    (render-state [this {:keys [monet-canvas canvas-events]}]
      ;;Not having the right paremteters causes this to silently fail.
      ;;Bad bug.
      (dom/canvas #js {
                       :width "500px"
                       :height "500px"
                       :onMouseMove (fn [e]
                                      (put! canvas-events (merge (make-action "mouseMove" e) {:mouseDown? mouseDown?})))
                       :onMouseDown (fn [e]
                                      (put! canvas-events (make-action "mouseDown" e)))
                       :onMouseUp (fn [e]
                                    (put! canvas-events (make-action "mouseUp" e)))
                      }
             (doseq [path (:state (active-commit history))]
               (draw-line (:ctx monet-canvas) path)) ))))



;;Can put canvas stuff inside as long as it evals to nil?
;;SVG will go the auto diffing for me for changes... (would be easier...)
;;Could write both.... branches...
