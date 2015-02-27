(ns muck.color-picker
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))


(enable-console-print!)

(defn make-action [e-type event]
  {:type e-type
   :x (.-offsetX (.-nativeEvent event))
   :y (.-offsetY (.-nativeEvent event))})


;;HSL
;;function hsv2rgb(hsv) {
;;        var R, G, B, X, C;
;;        var h = (hsv.h % 360) / 60;
;;
;;        C = hsv.v * hsv.s;
;;        X = C * (1 - Math.abs(h % 2 - 1));
;;        R = G = B = hsv.v - C;
;;
;;        h = ~~h;
;;        R += [C, X, 0, 0, X, C][h];
;;        G += [X, C, C, X, 0, 0][h];
;;        B += [0, 0, X, C, C, X][h];
;;
;;        var r = Math.floor(R * 255);
;;        var g = Math.floor(G * 255);
;;        var b = Math.floor(B * 255);
;;        return { r: r, g: g, b: b, hex: "#" + (16777216 | b | (g << 8) | (r << 16)).toString(16).slice(1) };
;;}
;;Hue Picker

;;Reference.
;;http://www.rapidtables.com/convert/color/hsv-to-rgb.htm

(defn floor [n]
  (.floor js/Math n))

(defn abs [n]
  (.abs js/Math n))

(defn % [n1 n2]
  (mod n1 n2))

(defn hsv2rgb [{:keys [h s v]}]
  (let [index (floor (/ (% h 360) 60))
        C (* s v)
        X (* C (- 1 (abs (- (% h 2) 1))))
        m (- v C)
        color-cutoffs [[C, X, 0, 0, X, C]
                       [X, C, C, X, 0, 0]
                       [0, 0, X, C, C, X]]]
    (zipmap
     [:r :g :b]
     (map (fn [cutoff]
             (floor (* (+ m (cutoff index)) 255))) color-cutoffs))))



(defn hue-picker [{:keys [size channel percentage]} owner]
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
               ["100%" "#FF0000" ]]
       :dragging? false})
    om/IRenderState
    (render-state [this {:keys [stops dragging?]}]
       (dom/svg #js {:width (str size "px")
                     :height (str (* size .2) "px")
                     :onMouseMove (fn [e]
                                    (if dragging?
                                     (put! channel (merge {:picker :hue} (make-action :mouseMove e)))))
                     :onMouseUp (fn []
                                  (om/set-state! owner :dragging? false))
                     :onMouseDown (fn []
                                    (om/set-state! owner :dragging? true))}
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
                               :width "100%" :height "100%"})
                (dom/rect #js {:fillOpacity 0
                               :stroke "gray"
                               :strokeWidth 1
                               :x (* (- 1 percentage) size)
                               :y 0
                               :width "3%" :height "100%"})))))


(defn to-rgb-str [{:keys [r g b]}]
  (str "rgb(" r "," g "," b ")"))

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
    (render-state [this {:keys [dragging?]}]
                (print backgroundColor)
                (dom/div #js {:style #js {:backgroundColor (to-rgb-str backgroundColor)
                                          :width (str size "px")
                                          :height (str size "px")}}
                 (dom/svg #js {:width (str size "px")
                               :height (str size "px")
                               :onMouseMove (fn [e]
                                              (if dragging?
                                               (put! channel (merge {:picker :saturation-lightness} (make-action :mouseMove e)))))
                               :onMouseUp (fn []
                                            (om/set-state! owner :dragging? false))
                               :onMouseDown (fn []
                                              (om/set-state! owner :dragging? true))}
                  (dom/defs nil
                    (dom/linearGradient #js {:id "gradient-black" :x1 "0%" :y1 "100%" :x2 "0%" :y2 "0%"}
                                      (dom/stop #js {:offset "0%" :stopColor "#000000" :stopOpacity "1"})
                                      (dom/stop #js {:offset "100%" :stopColor "#CC9A81" :stopOpacity "0"}))
                    (dom/linearGradient #js {:id "gradient-white" :x1 "0%" :y1 "100%" :x2 "100%" :y2 "100%"}
                                      (dom/stop #js {:offset "0%" :stopColor "#FFFFFF" :stopOpacity "1"})
                                      (dom/stop #js {:offset "100%" :stopColor "#CC9A81" :stopOpacity "0"})))
                          (dom/rect #js {:x 0 :y 0 :width "100%" :height "100%" :fill "url(#gradient-white)"})
                          (dom/rect #js {:x 0 :y 0 :width "100%" :height "100%" :fill "url(#gradient-black)"})
                          (dom/circle #js {:cx (* size percentage-x)
                                           :cy (* size (- 1 percentage-y))
                                           :r 3
                                           :fillOpacity 0
                                           :stroke "gray"
                                           :strokeWidth 1
                                           :onMouseUp (fn []
                                                        (om/set-state! owner :dragging? false))}))))))

;;I think hsv to rgb needs the constant.
(defn make-hsv [{:keys [hue-percentage
                        sl-x-percentage
                        sl-y-percentage]}]
  (let [color-constant 0]
    {:h (+ (* hue-percentage 360) color-constant)
     :s sl-x-percentage
     :v sl-y-percentage}))

(defn state-to-rgb [owner]
  (-> (om/get-state owner) make-hsv hsv2rgb))

(defn color-picker [{:keys [size rgb]}  owner]
 (reify
  om/IInitState
    (init-state [_]
      {:backgroundColor {:r 51, :g 48, :b 40}
       :channel (chan)
       :hue-percentage .2
       :sl-x-percentage .2
       :sl-y-percentage .2})
  om/IWillMount
    (will-mount [_]
     (let [channel (om/get-state owner :channel)]
      (go (loop []
        (let [{:keys [x y picker]} (<! channel)]
          (cond (= picker :saturation-lightness)
                    (om/update-state! owner
                              (fn [state]
                                (merge state {:sl-x-percentage (/ x size)
                                             :sl-y-percentage (/ (- size y) size)})))
                (= picker :hue)
                  (om/update-state! owner
                            (fn [state]
                              (let [new-hue-state (merge state {:hue-percentage (/ (- size x) size)})]
                                  (merge new-hue-state {:backgroundColor (-> new-hue-state make-hsv hsv2rgb)})))))
        (om/transact! rgb (fn [rgb]
                           (-> (om/get-state owner) make-hsv hsv2rgb)))
        (recur))))))
  om/IRenderState
  (render-state [this {:keys [backgroundColor channel hue-percentage
                              sl-x-percentage sl-y-percentage]}]
                (dom/div nil
                  (om/build saturation-lightness-picker {:size size
                                                         :backgroundColor backgroundColor
                                                         :channel channel
                                                         :percentage-x sl-x-percentage
                                                         :percentage-y sl-y-percentage})
                  (om/build hue-picker {:size size
                                        :channel channel
                                        :percentage hue-percentage})))))
