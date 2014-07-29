(ns main)

;;; Conway engine ;;;

(def ^:dynamic *world-width* 25)

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
        ymax (int (/ (dec (count world)) *world-width*))
        {x :x y :y} (get-position index)]
    (reduce +
            (for [xo (range -1 2)
                  yo (range -1 2)
                  :let [i (get-index {:x (+ x xo) :y (+ y yo)})]
                  :when (and (not (= i index))
                             (<= 0 (+ x xo) xmax)
                             (<= 0 (+ y yo) ymax))]
              (if (nth world i) 1 0)))))

(defn pass-generation [world]
  (for [index (range (count world))]
    (if (nth world index)
      (case (adjacent-life index world)
        (0 1 4 5 6 7 8) false
        (2 3) true)
      (= 3 (adjacent-life index world)))))
      
(defn life-sequence [world]
  (cons world
        (lazy-seq (life-sequence (pass-generation world)))))

;;; Interface ;;;

(defn draw-row [row row-index]
  (this-as this
    (let [selection (-> (d3/select this)
                        (.selectAll "rect")
                        (.data (into-array row)))]
      (-> selection
          (.attr "height" "10")
          (.attr "width" "10")
          (.attr "x" #(str (* 10 %2)))
          (.attr "y" (str (* 10 row-index)))
          (.style "fill-opacity" "1")
          (.style "fill" #(if % "white" "black")))
      (-> selection
          (.enter)
          (.append "rect")
          (.attr "height" "10")
          (.attr "width" "10")
          (.attr "x" #(str (* 10 %2)))
          (.attr "y" (str (* 10 row-index)))
          (.style "fill-opacity" "1")
          (.style "fill" #(if % "white" "black")))
      (-> selection
          (.exit)
          (.remove)))))

(defn draw-world [world]
  (let [selection (-> (d3/select "svg#field")
                      (.selectAll "g.row")
                      (.data (into-array (partition *world-width* world))))]
    (-> selection
        (.each draw-row))
    (-> selection
        (.enter)
        (.append "g")
        (.attr "class" "row")
        (.each draw-row))
    (-> selection
        (.exit)
        (.remove))))

(def world (repeatedly 625 #(< (rand) 0.5)))
