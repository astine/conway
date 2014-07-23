(ns main)

(def ^:dynamic *world-width* 25)

(defn get-grid-index [position]
  (let [{x :x y :y} position]
    (+ x (* y *world-width*))))

(defn print-world [world]
  (doseq [row (partition *world-width* world)]
    (doseq [square (butlast row)]
      (if square (print 1) (print 0)))
    (if (last row) (println 1) (println 0))))

(defn adjacent-life [index world]
  (let [max (dec (count world))
        min 0]
    (reduce +
            (for [i [(dec (- index *world-width*))
                     (- index *world-width*)
                     (inc (- index *world-width*))
                     (dec index)
                     (inc index)
                     (dec (+ index *world-width*))
                     (+ index *world-width*)
                     (inc (+ index *world-width*))]
                  :when (and (>= i min)
                             (<= i max))]
              (if (nth world i) 1 0)))))

(defn pass-generation [world]
  (for [index (range (count world))]
    (if (nth world index)
      (case (adjacent-life index world)
        (0 1 4 5 6 7 8) false
        (2 3) true)
      (= 3 (adjacent-life index world)))))
      
