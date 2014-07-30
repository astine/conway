(ns main)

;;; Conway engine ;;;

(def ^:dynamic *world-width* 25)
(def ^:dynamic *world-height* 25)

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

(defn adjacent-life [index world]
  (let [xmax (dec *world-width*)
        ymax (dec *world-height*)
        {x :x y :y} (get-position index)
        decx (dec x)
        decy (dec y)
        incx (inc x)
        incy (inc y)]
    (+ (if (and (<= 0 decx xmax)
                (<= 0 decy ymax)
                (nth world (get-index {:x decx :y decy})))
         1 0)
       (if (and (<= 0 incx xmax)
                (<= 0 incy ymax)
                (nth world (get-index {:x incx :y incy})))
         1 0)
       (if (and (<= 0 decx xmax)
                (<= 0 incy ymax)
                (nth world (get-index {:x decx :y incy})))
         1 0)
       (if (and (<= 0 incx xmax)
                (<= 0 decy ymax)
                (nth world (get-index {:x incx :y decy})))
         1 0)
       (if (and (<= 0 decx xmax)
                (nth world (get-index {:x decx :y y})))
         1 0)
       (if (and (<= 0 incx xmax)
                (nth world (get-index {:x incx :y y})))
         1 0)
       (if (and (<= 0 decy ymax)
                (nth world (get-index {:x x :y decy})))
         1 0)
       (if (and (<= 0 incy ymax)
                (nth world (get-index {:x x :y incy})))
         1 0))))

(defn pass-generation [world]
  (into-array
   (for [index (range (count world))]
     (if (nth world index)
       (case (adjacent-life index world)
         (0 1 4 5 6 7 8) false
         (2 3) true)
       (= 3 (adjacent-life index world))))))
      
(defn life-sequence [world]
  (cons world
        (lazy-seq (life-sequence (pass-generation world)))))

;;; Interface ;;;

(defn draw-world [world]
  (this-as this
    (let [selection (-> (d3/select "svg#field")
                        (.selectAll "rect")
                        (.data world))]
      (-> selection
          (.transition)
          (.delay 10)
          (.duration 1000)
          (.attr "height" "15")
          (.attr "width" "15")
          (.attr "x" #(str (* 15 (:x (get-position %2)))))
          (.attr "y" #(str (* 15 (:y (get-position %2)))))
          (.style "fill-opacity" "1")
          (.style "fill" #(if % "black" "white"))
          (.each "end" #(if (zero? %2) (draw-world (pass-generation world)))))
          ;(.each "end" #(if (zero? %2) (js/alert "test"))))
      (-> selection
          (.enter)
          (.append "rect")
          (.transition)
          (.delay 10)
          (.duration 1000)
          (.attr "height" "15")
          (.attr "width" "15")
          (.attr "x" #(str (* 15 (:x (get-position %2)))))
          (.attr "y" #(str (* 15 (:y (get-position %2)))))
          (.style "fill-opacity" "1")
          (.style "fill" #(if % "black" "white"))
          (.each "end" #(if (zero? %2) (draw-world (pass-generation world)))))
          ;(.each "end" #(if (zero? %2) (js/alert "test"))))
      (-> selection
          (.exit)
          (.remove)))))

(def world (into-array (repeatedly (* *world-width* *world-height*) #(< (rand) 0.5))))

