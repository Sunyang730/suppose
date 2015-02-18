(ns muck.keypress
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [goog.events :as events]
            [muck.version_control :as vc]))

;; This might make a good general component for handling key events.

(def key-codes
  {88 :x
   65 :a
   82 :r
   67 :c
   85 :u
   80 :p
   86 :v
   83 :s
   69 :e
   68 :d
   76 :l
   73 :i
   71 :g
   75 :k
   90 :z
   38 :up
   40 :down
   37 :left
   39 :right
   8 :delete})


(def key-event
  {
   :keyup {:c (fn [app]
                (om/transact! app (fn [app-state]
                                            (vc/new-branch app-state))))}
   :keydown {}
  })


(defn handle-key-event [app [press-type event]]
  (let [keyCode (.-keyCode event)
        metaKey (.-metaKey event)
        shiftKey (.-shiftKey event)
        ctrlKey (.-ctrlKey event)
        event-map (press-type key-event)
        key-keyword (get key-codes keyCode)]
      (if (contains? event-map key-keyword)
        ((key-keyword event-map) app))))


(defn key-listener [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:keyboard-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [keychan (om/get-state owner :keyboard-chan)]
        (go
          (while true
            (let [event (<! keychan)]
              (do
                (handle-key-event app event)))))))
    om/IDidMount
    (did-mount [_]
      (let [keychan (om/get-state owner :keyboard-chan)]
        (events/listen js/document "keydown" (fn [e]
                                    (do
                                      (.preventDefault e)
                                      (put! keychan [:keydown e]))))
        (events/listen js/document "keyup" (fn [e]
                                    (do
                                      (.preventDefault e)
                                      (put! keychan [:keyup e]))))))
    om/IWillUnmount
    (will-unmount [_]
                  (do
                    (events/removeAll js/document "keydown")
                    (events/removeAll js/document "keyup")))
    om/IRenderState
    (render-state [this {:keys [keyboard-chan]}]
      (dom/div nil ""))))
