(ns main
  (:require [cljs.reader :refer [read-string]]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :refer [<! chan onto-chan]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;;; Conway engine ;;;

(def ^:dynamic *world-width* 50)
(def ^:dynamic *world-height* 40)

(defn adjacent-life [index life]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        [x y] index
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (+ (if (contains? life [decx decy])
         1 0)
       (if (contains? life [incx incy])
         1 0)
       (if (contains? life [decx incy])
         1 0)
       (if (contains? life [incx decy])
         1 0)
       (if (contains? life [decx y])
         1 0)
       (if (contains? life [incx y])
         1 0)
       (if (contains? life [x decy])
         1 0)
       (if (contains? life [x incy])
         1 0))))

(defn set-active [index active]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        [x y] index
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (reduce conj! active
            [[decx decy]
             [incx incy]
             [decx incy]
             [incx decy]
             [decx y]
             [incx y]
             [x decy]
             [x incy]])))

(defn pass-generation [world-adj]
  (let [world (first world-adj)
        active (second world-adj)]
    (loop [indexes active
           nworld (transient (first world-adj))
           nactv (transient #{})]
      (if (not-empty indexes)
        (let [index (first indexes)]
          (let [adjacencies (adjacent-life index world)]
            (if (contains? world index)
              (if (or (= 2 adjacencies)
                      (= 3 adjacencies))
                (recur (rest indexes) nworld nactv)
                (recur (rest indexes)
                       (disj! nworld index)
                       (set-active index nactv)))
              (if (= 3 adjacencies)
                (recur (rest indexes)
                       (conj! nworld index)
                       (set-active index nactv))
                (recur (rest indexes) nworld nactv)))))
        (vector (persistent! nworld)
                (persistent! nactv))))))

(defn toggle-cell [world-adj index]
  (let [world (first world-adj)
        adjacencies (transient (second world-adj))
        active (transient (second world-adj))]
    (if (get world index false)
      (vector (disj world index)
              (persistent! (set-active index active)))
      (vector (conj world index)
              (persistent! (set-active index active))))))

(defn life-to-point-array [life]
  (into-array life))

(defn world-to-point-array [world]
  (life-to-point-array (first world)))

;;; Interface ;;;

(defn blank-world []
  [#{} #{}])

(defn random-world []
  (let [life (set (repeatedly (/ (* *world-width* *world-height*) 5) 
                              #(vector (rand-int *world-width*) 
                                       (rand-int *world-height*))))]
    [life 
     (set (for [x (range *world-width*) y (range *world-height*)] 
            [x y]))]))

(defn life-sequence [world]
  (iterate pass-generation world))

(def playing (atom false))

(defn draw-life [life duration callback]
  (let [selection (-> (d3/select "svg#field")
                      (.selectAll "rect")
                      (.data life))
        tile-height (-> (js/jQuery "svg#field")
                        (.first)
                        (.height)
                        (/ *world-height*))
        tile-width (-> (js/jQuery "svg#field")
                       (.first)
                       (.width)
                       (/ *world-width*))]
    (-> selection
        (.attr "y" #(str (* tile-height (second %))))
        (.attr "x" #(str (* tile-width (first %))))
        (.transition)
        (.delay duration)
        (.duration 1)
        (.each "end" callback))
    (-> selection
        (.enter)
        (.append "rect")
        (.attr "height" (str tile-height))
        (.attr "width" (str tile-width))
        (.attr "y" #(str (* tile-height (second %))))
        (.attr "x" #(str (* tile-width (first %))))
        (.style "fill-opacity" "1")
        (.style "fill" "black")
        (.transition)
        (.delay duration)
        (.duration 1)
        (.each "end" callback))
    (-> selection
        (.exit)
        (.remove))))

(def duration (atom 40))

(declare stop)

(defn draw-life-sequence [life-sequence]
  (if (not-empty life-sequence)
    (draw-life (first life-sequence)
               @duration
               #(if (and (zero? %2) @playing)
                  (draw-life-sequence (rest life-sequence))))
    (stop)))

(def world-future (atom nil))

(def world-history (atom nil))

(def world (atom nil))

(defn sequence-with-history 
  ([future world history]
     (sequence-with-history @future future world history))
  ([sequence future world history]
     (when (not-empty sequence)
       (lazy-seq
        (cons (first sequence)
              (sequence-with-history 
               (do 
                 (swap! history conj @world)
                 (reset! world (first sequence))
                 (reset! future (rest sequence))
                 (rest sequence))
               future world history))))))

(defn initialize-world [& [type]]
  (reset! world-history nil)
  (case (or type :random)
    :random (reset! world-future (life-sequence (random-world)))
    :blank (reset! world-future (life-sequence (blank-world))))
  (reset! world (first @world-future))
  (swap! world-future rest)
  (draw-life (world-to-point-array @world) 0 (constantly nil)))

(defn alter-world [function & args]
  (swap! world-history conj @world)
  (swap! world #(apply function (cons % args)))
  (reset! world-future (rest (life-sequence @world)))
  (draw-life (world-to-point-array @world) 0 (constantly nil)))

(defn play []
  (reset! playing true)
  (.prop (js/jQuery "#pause") "disabled" false)
  (draw-life-sequence (map world-to-point-array (sequence-with-history world-future world world-history))))

(defn stop []
  (reset! playing false)
  (.prop (js/jQuery "#pause") "disabled" true))

(defn rewind []
  (reset! playing true)
  (.prop (js/jQuery "#pause") "disabled" false)
  (draw-life-sequence (map world-to-point-array (sequence-with-history world-history world world-future))))

(defn step-forward []
  (draw-life (first (map world-to-point-array (sequence-with-history world-future world world-history)))
             0
             (constantly nil)))

(defn step-backward []
  (draw-life (first (map world-to-point-array (sequence-with-history world-history world world-future)))
             0
             (constantly nil)))

(defn reset []
  (when (not-empty @world-history)
    (reset! world-future (concat (rest (reverse @world-history)) (list @world) @world-future))
    (reset! world (last @world-history))
    (reset! world-history '())
    (draw-life (world-to-point-array @world) 0 (constantly nil))))

(def play-button (-> (d3/select "#play")
                     (.on "click" play)))

(def stop-button (-> (d3/select "#pause")
                     (.on "click" stop)))

(def rewind-button (-> (d3/select "#rewind")
                       (.on "click" rewind)))

(def forward-button (-> (d3/select "#step-forward")
                        (.on "click" step-forward)))

(def backward-button (-> (d3/select "#step-backward")
                        (.on "click" step-backward)))

(def reset-button (-> (d3/select "#to-beginning")
                      (.on "click" reset)))

(def gen-random-button (-> (d3/select "#gen-random")
                           (.on "click" initialize-world)))

(def gen-blank-button (-> (d3/select "#gen-blank")
                          (.on "click" #(initialize-world :blank))))

(def svg-area (-> (js/jQuery "svg#field")
                  (.click #(let [svg (js/jQuery "div#fieldwrapper")
                                 offset (.offset svg)
                                 position [(int (* (/ (- (.-pageX %) 
                                                         (.-left offset)) 
                                                      (.width svg)) 
                                                   *world-width*))
                                           (int (* (/ (- (.-pageY %) 
                                                         (.-top offset)) 
                                                      (.height svg)) 
                                                   *world-height*))]]
                             (alter-world toggle-cell position)))))

(defn speed []
  [:span
   [:span "Speed: " (int (/ 1000 @duration)) " "]
   [:input {:id "foo" :value (/ 1000 @duration)
            :type "range" :min "1" :max "50"
            :on-change #(reset! duration (int (/ 1000 (-> % .-target .-value))))}]])

(defn mountit []
  (reagent/render-component [speed]
                            (aget (js/jQuery "span#speed") 0)))

(mountit)

(initialize-world :blank)
