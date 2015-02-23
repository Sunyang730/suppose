(ns muck.color-picker
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [cljs.core.async :refer [put! chan <!]]))




;;HSL

;;Hue Picker

(defn hue-picker [{:keys [size channel]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:stops [["0%"   "#FF0000" ]
               ["13%"  "#FF00FF" ]
               ["25%"  "#8000FF" ]
               ["38%"  "#0040FF" ]
               ["50%"  "#00FFFF" ]
               ["63%"  "#00FF40" ]
               ["75%"  "#0BED00" ]
               ["88%"  "#FFFF00" ]
               ["100%" "#FF0000" ]]})
    om/IRenderState
    (render-state [this {:keys [stops]}]
       (dom/svg #js {:width (str size "px")
                     :height (str (* size .2) "px")}
                (dom/defs nil
                  (apply dom/linearGradient #js {:id "colorPicker"
                                               ;;Make horizontal
                                               ;;:x1 "0%" :y1 "100%"
                                               ;;:x2 "0%" :y2 "0%"
                                                 }
                    (map (fn [[offset color]]
                           (dom/stop #js {:offset offset :stopColor color})) stops)))
                (dom/rect #js {:fill "url(#colorPicker)"
                               :x 0 :y 0
                               :width "100%" :height "100%"})))))


;;Saturation, Lightness Picker

(defn saturation-lightness-picker [{:keys [size backgroundColor channel percentage-x percentage-y]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:dragging? false})
    om/IDidMount
    (did-mount [_]
      "")
    om/IRenderState
    (render-state [this _]
                (dom/div #js {:style #js {:backgroundColor backgroundColor
                                          :width (str size "px")
                                          :height (str size "px")}}
                 (dom/svg #js {:width (str size "px")
                               :height (str size "px")}
                  (dom/defs nil
                    (dom/linearGradient #js {:id "gradient-black" :x1 "0%" :y1 "100%" :x2 "0%" :y2 "0%"}
                                      (dom/stop #js {:offset "0%" :stopColor "#000000" :stopOpacity "1"})
                                      (dom/stop #js {:offset "100%" :stopColor "#CC9A81" :stopOpacity "0"}))
                    (dom/linearGradient #js {:id "gradient-white" :x1 "0%" :y1 "100%" :x2 "100%" :y2 "100%"}
                                      (dom/stop #js {:offset "0%" :stopColor "#FFFFFF" :stopOpacity "1"})
                                      (dom/stop #js {:offset "100%" :stopColor "#CC9A81" :stopOpacity "0"})))
                          (dom/rect #js {:x 0 :y 0 :width "100%" :height "100%" :fill "url(#gradient-white)"})
                          (dom/rect #js {:x 0 :y 0 :width "100%" :height "100%" :fill "url(#gradient-black)"})
                          (dom/circle #js {:cx (* size percentage-x) :cy (* size percentage-x) :r 5}))))))



(defn color-picker [{:keys [size]}  owner]
 (reify
  om/IInitState
    (init-state [_]
      {:backgroundColor "#0040FF"
       :channel (chan)
       :hue-percentage .2
       :sl-x-percentage .2
       :sl-y-percentage .2})
  om/IWillMount
    (will-mount [_]
     (let [channel (om/get-state owner :channel)]
      (go (loop []
        (let [event-map (<! channel)]
          (.log js/console "loggg")
        (recur))))))
  om/IRenderState
  (render-state [this {:keys [backgroundColor channel hue-percentage
                              sl-x-percentage sl-y-percentage]}]
                (dom/div nil
                  (om/build saturation-lightness-picker {:size 100
                                                         :backgroundColor backgroundColor
                                                         :channel channel
                                                         :percentage-x sl-x-percentage
                                                         :percentage-y sl-y-percentage})
                  (om/build hue-picker {:size 100
                                        :channel channel
                                        :percentage hue-percentage})))))
