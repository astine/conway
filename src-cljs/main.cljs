(ns main
  (:require [cljs.reader :refer [read-string]]))

;;; Conway engine ;;;

(def ^:dynamic *world-width* 25)
(def ^:dynamic *world-height* 20)

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

(defn pass-generation [world-adj]
  (loop [world (first world-adj)
         adjacencies (second world-adj)
         index 0
         nworld (transient (vec (first world-adj)))
         nadjs (transient (vec (second world-adj)))]
    (if (not-empty world)
      (if (first world)
        (case (first adjacencies)
          (0 1 4 5 6 7 8)
          (recur (rest world) (rest adjacencies)
                 (inc index)
                 (assoc! nworld index false)
                 (set-adjacencies index nadjs dec))
          (2 3)
          (recur (rest world) (rest adjacencies)
                 (inc index)
                 nworld nadjs))
        (if (= 3 (first adjacencies))
          (recur (rest world) (rest adjacencies)
                 (inc index)
                 (assoc! nworld index true)
                 (set-adjacencies index nadjs inc)) 
          (recur (rest world) (rest adjacencies)
                 (inc index)
                 nworld nadjs)))
      (vector (into-array (persistent! nworld))
              (into-array (persistent! nadjs))))))
      
;;; Interface ;;;

(defn generate-world []
  (let [life (into-array (repeatedly (* *world-width* *world-height*) #(< (rand) 0.5)))]
    [life (into-array (generate-adjacencies life))]))

(def world (atom (generate-world)))

(def world-history (atom nil))

(def playing (atom false))

(defn life-sequence [world]
  (iterate pass-generation world))

(defn sequence-with-history [sequence]
  (for [iteration sequence]
    (do
      (when iteration
        (swap! world (constantly iteration)))
      (swap! world-history conj iteration)
      iteration)))

(def duration (atom 50))

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
        (.transition)
        (.duration duration)
        (.attr "height" (str tile-height))
        (.attr "width" (str tile-width))
        (.attr "y" #(str (* tile-height (:y (get-position %2)))))
        (.attr "x" #(str (* tile-width (:x (get-position %2)))))
        (.style "fill-opacity" "1")
        (.style "fill" #(if % "black" "white"))
        (.each "end" callback))
    (-> selection
        (.enter)
        (.append "rect")
        (.transition)
        (.duration duration)
        (.attr "height" (str tile-height))
        (.attr "width" (str tile-width))
        (.attr "y" #(str (* tile-height (:y (get-position %2)))))
        (.attr "x" #(str (* tile-width (:x (get-position %2)))))
        (.style "fill-opacity" "1")
        (.style "fill" #(if % "black" "white"))
        (.each "end" callback))
    (-> selection
        (.exit)
        (.remove))))

(defn draw-life-sequence [life-sequence]
  (when life-sequence
    (draw-life (first life-sequence)
               @duration
               #(if (and (zero? %2) @playing)
                  (draw-life-sequence (rest life-sequence))))))

(defn play []
  (swap! playing (constantly true))
  (draw-life-sequence (map first (sequence-with-history (life-sequence @world)))))

(defn stop []
  (swap! playing (constantly false)))

(defn rewind []
  (swap! playing (constantly true))
  (draw-life-sequence (map first @world-history)))
  
(def speed-box (d3/select "#speed"))

(defn set-speed []
  (swap! duration (constantly (/ 1000 (read-string (get (first (first speed-box)) "value"))))))

(.on speed-box "change" set-speed)

(def play-button (-> (d3/select "#play")
                     (.on "click" play)))

(def stop-button (-> (d3/select "#stop")
                     (.on "click" stop)))

(def rewind-button (-> (d3/select "#rewind")
                       (.on "click" rewind)))
