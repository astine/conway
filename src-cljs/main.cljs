(ns main
  (:require [cljs.reader :refer [read-string]]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :refer [<! chan onto-chan]]
            ;[goog.object :refer [remove]]
)
  (:require-macros [cljs.core.async.macros :refer [go]]))

;;; Conway engine ;;;

(def ^:dynamic *world-width* 200)
(def ^:dynamic *world-height* 160)

(defn key-in-view? [key]
  (let [[x y] key]
    (and (<= 0 x *world-width*)
         (<= 0 y *world-height*))))

(defn to-key [x y]
  (+ (* (+ 5000 x) 10000) (+ 5000 y)))

(defn from-key [num]
  [(- (int (/ num 10000)) 5000)
   (- (int (mod num 10000)) 5000)])

(defn adjacent-life [index life]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        x (- (int (/ index 10000)) 5000)
        y (- (int (mod index 10000)) 5000)
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (+ (if (aget life (to-key decx decy))
         1 0)
       (if (aget life (to-key incx incy))
         1 0)
       (if (aget life (to-key decx incy))
         1 0)
       (if (aget life (to-key incx decy))
         1 0)
       (if (aget life (to-key decx y))
         1 0)
       (if (aget life (to-key incx y))
         1 0)
       (if (aget life (to-key x decy))
         1 0)
       (if (aget life (to-key x incy))
         1 0))))

(defn set-active [index active]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        x (- (int (/ index 10000)) 5000)
        y (- (int (mod index 10000)) 5000)
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (aset active (to-key decx decy) true)
    (aset active (to-key incx incy) true)
    (aset active (to-key decx incy) true)
    (aset active (to-key incx decy) true)
    (aset active (to-key decx y) true)
    (aset active (to-key incx y) true)
    (aset active (to-key x decy) true)
    (aset active (to-key x incy) true)
    (aset active (to-key x y) true)
    active))

(defn seq->js [seq]
  (let [obj (js-obj)]
    (doseq [item seq]
      (aset obj item true))
    obj))

(defn pass-generation [[world active]]
  (let [nworld (goog.object.clone world)
        nactv (js-obj)]
    (doseq [index (goog.object.getKeys active)]
      (let [adjacencies (adjacent-life index world)]
        (if (true? (aget world index))
          (when-not (or (= 2 adjacencies)
                        (= 3 adjacencies))
            (goog.object.remove nworld index)
            (set-active index nactv))
          (when (= 3 adjacencies)
            (aset nworld index true)
            (set-active index nactv)))))
    (vector nworld
            nactv)))

(defn toggle-cell [[world active] index]
  (let [index (apply to-key index)
        nworld (goog.object.clone world)
        nactv (goog.object.clone active)]
    (if (true? (aget world index))
      (vector (do (goog.object.remove nworld index) nworld)
              (set-active index nactv))
      (vector (do (aset nworld index true) nworld)
              (set-active index nactv)))))

(defn life-to-point-array [life]
  (goog.array.filter
   (goog.array.map (goog.object.getKeys life) from-key)
   key-in-view?))


(defn world-to-point-array [world]
  (life-to-point-array (first world)))

;;; Interface ;;;

(defn blank-world []
  [(js-obj) (js-obj)])

(defn random-world []
  (let [life (seq->js (repeatedly (/ (* *world-width* *world-height*) 5) 
                                  #(to-key (rand-int *world-width*) 
                                           (rand-int *world-height*))))]
    [life 
     (seq->js (for [x (range *world-width*) y (range *world-height*)] 
                (to-key x y)))]))

(defn end-at-cycle
  ([sequence pred]
     (end-at-cycle sequence (rest sequence) pred))
  ([tortoise hare pred]
     (if-not (pred (first tortoise) (first hare))
       (lazy-seq
        (cons (first tortoise)
              (end-at-cycle (rest tortoise) (drop 2 hare) pred)))
       (cons (first tortoise)
             (take-while #(not (pred (first hare) %)) (rest tortoise))))))

(defn life-sequence [world]
  (end-at-cycle
   (iterate pass-generation world)
   #(goog.array.equals (world-to-point-array %1)
                       (world-to-point-array %2)
                       =)))

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
        (.remove))
    (when (zero? (count life))
      (callback nil 0))))

(def duration (atom 40))

(declare stop)

(def world-future (atom nil))

(def world-history (atom nil))

(def world (atom nil))

(defn step-sequence
  ([future world history]
     (step-sequence @future future world history))
  ([sequence future world history]
     (when (not-empty sequence)
       (swap! history conj @world)
       (reset! world (first sequence))
       (reset! future (rest sequence))
       (first sequence))))

(defn animate-sequence
  ([future world history]
     (animate-sequence @future future world history))
  ([sequence future world history]
     (if (not-empty sequence)
       (draw-life (world-to-point-array (step-sequence sequence future world history))
                  @duration
                  #(if (and (zero? %2) @playing)
                     (animate-sequence (rest sequence) future world history)))
       (stop))))

(defn initialize-world [& [type]]
  (reset! world-history '())
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
  (animate-sequence world-future world world-history))

(defn stop []
  (reset! playing false))

(defn rewind []
  (reset! playing true)
  (animate-sequence world-history world world-future))

(defn step-forward []
  (-> (or (step-sequence world-future world world-history)
          (step-sequence (list (pass-generation @world)) world-future world world-history))
      world-to-point-array
      (draw-life 0 (constantly nil))))

(defn step-backward []
  (-> (or (step-sequence world-history world world-future)
          @world)
      world-to-point-array
      (draw-life 0 (constantly nil))))

(defn reset []
  (when (not-empty @world-history)
    (reset! world-future (concat (rest (reverse @world-history)) (list @world) @world-future))
    (reset! world (last @world-history))
    (reset! world-history '())
    (draw-life (world-to-point-array @world) 0 (constantly nil))))

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

(defn controls []
  [:span
   [:input {:type "button" :value "|◀◀"
            :id "reset"
            :disabled @playing
            :on-click reset}]
   [:input {:type "button" :value "|◀"
            :id "backward"
            :disabled @playing
            :on-click step-backward}]
   [:input {:type "button" :value "◀"
            :id "rewind"
            :disabled @playing
            :on-click rewind}]
   [:input {:type "button" :value "▮▮"
            :id "pause"
            :disabled (not @playing)
            :on-click stop}]
   [:input {:type "button" :value "▶"
            :id "play"
            :disabled @playing
            :on-click play}]
   [:input {:type "button" :value "▶|"
            :id "forward"
            :disabled @playing
            :on-click step-forward}]
   [:br]
   [:span
    [:span "Speed: " (int (/ 1000 @duration)) " "]
    [:input {:id "foo" :value (/ 1000 @duration)
             :type "range" :min "1" :max "50"
             :on-change #(reset! duration (int (/ 1000 (-> % .-target .-value))))}]]])

(defn right-controls []
  [:span
   [:input {:type "button" :value "Generate"
            :id "gen-random"
            :disabled @playing
            :on-click #(initialize-world :random)}]
   [:input {:type "button" :value "Clear"
            :id "gen-blank"
            :disabled @playing
            :on-click #(initialize-world :blank)}]])


(defn mountit []
  (reagent/render-component [controls]
                            (aget (js/jQuery "#controls") 0))
  (reagent/render-component [right-controls]
                            (aget (js/jQuery "#controls2") 0)))

(mountit)

(initialize-world :blank)
