(ns main
  (:require [cljs.reader :refer [read-string]]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :refer [<! chan onto-chan]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;;; Conway engine ;;;

(def ^:dynamic *world-width* 50)
(def ^:dynamic *world-height* 40)

(def blank-life (into-array (repeat (* *world-width* *world-height*) false)))

(defn get-index [position]
  (let [{x :x y :y} position]
    (+ x (* y *world-width*))))

(defn get-position [index]
  {:x (mod index *world-width*)
   :y (int (/ index *world-width*))})

(defn print-world [world]
  (doseq [row (partition *world-width* world)]
    (doseq [square (butlast row)]
      (if square (print 1) (print 0)))
    (if (last row) (println 1) (println 0))))

(defn adjacent-life [index life]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        {x :x y :y} (get-position index)
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (+ (if (and (<= 0 decx xmax)
                (<= 0 decy ymax)
                (nth life (get-index {:x decx :y decy})))
         1 0)
       (if (and (<= 0 incx xmax)
                (<= 0 incy ymax)
                (nth life (get-index {:x incx :y incy})))
         1 0)
       (if (and (<= 0 decx xmax)
                (<= 0 incy ymax)
                (nth life (get-index {:x decx :y incy})))
         1 0)
       (if (and (<= 0 incx xmax)
                (<= 0 decy ymax)
                (nth life (get-index {:x incx :y decy})))
         1 0)
       (if (and (<= 0 decx xmax)
                (nth life (get-index {:x decx :y y})))
         1 0)
       (if (and (<= 0 incx xmax)
                (nth life (get-index {:x incx :y y})))
         1 0)
       (if (and (<= 0 decy ymax)
                (nth life (get-index {:x x :y decy})))
         1 0)
       (if (and (<= 0 incy ymax)
                (nth life (get-index {:x x :y incy})))
         1 0))))

(defn generate-adjacencies [life]
  (into-array
   (for [index (range (count life))]
     (adjacent-life index life))))

(defn set-adjacencies [index adjacencies inc-or-dec]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        {x :x y :y} (get-position index)
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (as-> adjacencies adjacencies
          (if (and (<= 0 decx xmax)
                   (<= 0 decy ymax))
            (let [index (get-index {:x decx :y decy})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies)
          (if (and (<= 0 incx xmax)
                   (<= 0 incy ymax))
            (let [index (get-index {:x incx :y incy})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies)
          (if (and (<= 0 decx xmax)
                   (<= 0 incy ymax))
            (let [index (get-index {:x decx :y incy})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies)
          (if (and (<= 0 incx xmax)
                   (<= 0 decy ymax))
            (let [index (get-index {:x incx :y decy})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies)
          (if (<= 0 decx xmax)
            (let [index (get-index {:x decx :y y})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies)
          (if (<= 0 incx xmax)
            (let [index (get-index {:x incx :y y})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies)
          (if (<= 0 decy ymax)
            (let [index (get-index {:x x :y decy})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies)
          (if (<= 0 incy ymax)
            (let [index (get-index {:x x :y incy})]
              (assoc! adjacencies index (inc-or-dec (nth adjacencies index))))
            adjacencies))))

(defn set-active [index active]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        {x :x y :y} (get-position index)
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (as-> active active
          (if (and (<= 0 decx xmax)
                   (<= 0 decy ymax))
            (let [index (get-index {:x decx :y decy})]
              (assoc! active index true))
            active)
          (if (and (<= 0 incx xmax)
                   (<= 0 incy ymax))
            (let [index (get-index {:x incx :y incy})]
              (assoc! active index true))
            active)
          (if (and (<= 0 decx xmax)
                   (<= 0 incy ymax))
            (let [index (get-index {:x decx :y incy})]
              (assoc! active index true))
            active)
          (if (and (<= 0 incx xmax)
                   (<= 0 decy ymax))
            (let [index (get-index {:x incx :y decy})]
              (assoc! active index true))
            active)
          (if (<= 0 decx xmax)
            (let [index (get-index {:x decx :y y})]
              (assoc! active index true))
            active)
          (if (<= 0 incx xmax)
            (let [index (get-index {:x incx :y y})]
              (assoc! active index true))
            active)
          (if (<= 0 decy ymax)
            (let [index (get-index {:x x :y decy})]
              (assoc! active index true))
            active)
          (if (<= 0 incy ymax)
            (let [index (get-index {:x x :y incy})]
              (assoc! active index true))
            active))))

(defn pass-generation [world-adj]
  (loop [world (first world-adj)
         adjacencies (second world-adj)
         active (nth world-adj 2)
         index 0
         nworld (transient (vec (first world-adj)))
         nadjs (transient (vec (second world-adj)))
         nactv (transient (vec blank-life))]
    (if (not-empty world)
      (if (first active)
        (if (first world)
          (if (or (= 2 (first adjacencies))
                  (= 3 (first adjacencies)))
            (recur (rest world) (rest adjacencies) (rest active)
                   (inc index)
                   nworld nadjs nactv)
            (recur (rest world) (rest adjacencies) (rest active)
                   (inc index)
                   (assoc! nworld index false)
                   (set-adjacencies index nadjs dec)
                   (set-active index nactv)))
          (if (= 3 (first adjacencies))
            (recur (rest world) (rest adjacencies) (rest active)
                   (inc index)
                   (assoc! nworld index true)
                   (set-adjacencies index nadjs inc)
                   (set-active index nactv)) 
            (recur (rest world) (rest adjacencies) (rest active)
                   (inc index)
                   nworld nadjs nactv)))
        (recur (rest world) (rest adjacencies) (rest active)
               (inc index)
               nworld nadjs nactv))
      (vector (into-array (persistent! nworld))
              (into-array (persistent! nadjs))
              (into-array (persistent! nactv))))))

(defn toggle-cell [world-adj position]
  (let [index (get-index position)
        world (vec (first world-adj))
        adjacencies (transient (vec (second world-adj)))
        active (transient (vec (nth world-adj 2)))]
    (if (nth world index)
      (vector (into-array (assoc world index false))
              (into-array (persistent! (set-adjacencies index adjacencies dec)))
              (into-array (persistent! (set-active index active))))
      (vector (into-array (assoc world index true))
              (into-array (persistent! (set-adjacencies index adjacencies inc)))
              (into-array (persistent! (set-active index active)))))))

(defn life-to-point-array [life]
  (loop [life life indexes (range) point-array (transient [])]
    (cond (empty? life)
          (into-array (persistent! point-array))
          (first life)
          (recur (rest life) (rest indexes) (conj! point-array (get-position (first indexes))))
          :else
          (recur (rest life) (rest indexes) point-array))))

(defn world-to-point-array [world]
  (life-to-point-array (first world)))

;;; Interface ;;;

(defn blank-world []
  [blank-life (into-array (repeat (* *world-width* *world-height*) 0)) blank-life])

(defn random-world []
  (let [life (into-array (repeatedly (* *world-width* *world-height*) #(< (rand) 0.25)))]
    [life (into-array (generate-adjacencies life)) (into-array (repeat (* *world-width* *world-height*) true))]))

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
        (.attr "y" #(str (* tile-height (:y %))))
        (.attr "x" #(str (* tile-width (:x %))))
        (.transition)
        (.delay duration)
        (.duration 1)
        (.each "end" callback))
    (-> selection
        (.enter)
        (.append "rect")
        (.attr "height" (str tile-height))
        (.attr "width" (str tile-width))
        (.attr "y" #(str (* tile-height (:y %))))
        (.attr "x" #(str (* tile-width (:x %))))
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
                                 position {:x (int (* (/ (- (.-pageX %) 
                                                            (.-left offset)) 
                                                         (.width svg)) 
                                                      *world-width*))
                                           :y (int (* (/ (- (.-pageY %) 
                                                            (.-top offset)) 
                                                         (.height svg)) 
                                                      *world-height*))}]
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
