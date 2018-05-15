(ns setgol.game
  (:require [clojure.set :refer [select]]))

(defn stats [boardset]
  (let [mid (fn [min max]
              (- max (-> max (- min) (quot 2))))
        xs (map :x boardset)
        ys (map :y boardset)
        minx (apply min xs)
        miny (apply min ys)
        maxx (apply max xs)
        maxy (apply max ys)
        midx (mid minx maxx)
        midy (mid miny maxy)]
    {:min {:x minx :y miny}
     :max {:x maxx :y maxy}
     :mid {:x midx :y midy}}))

(defn neighbors? [cell1 cell2]
  (let [{:keys [x y]} cell1]
    (and (<= (dec x) (:x cell2) (inc x))
         (<= (dec y) (:y cell2) (inc y))
         (not (= cell2 cell1)))))

(defn count-live-neighbors [liveset cell]
  (->> liveset
       (select #(neighbors? cell %))
       (count)))

(defn breedset [liveset]
  (->> (for [xadd '(-1 0 1)
             yadd '(-1 0 1)
             cell liveset
             :let [{:keys [x y]} cell]]
         {:x (+ xadd x) :y (+ yadd y)})
       (into #{})))

(defn translate [boardset {:keys [x y]}]
  (->> boardset
       (map #(assoc % :x (x (:x %)) :y (y (:y %))))
       (into #{})))

(defn positive [boardset]
  (let [stats (stats boardset)
        minx (if (< (:x (:min stats)) 0) (:x (:min stats)) 0)
        miny (if (< (:y (:min stats)) 0) (:y (:min stats)) 0)]
    (translate
     boardset
     {:x #(- % minx)
      :y #(- % miny)})))

(defn select-range [boardset {:keys [width height x y]}]
  (select #(and (<= x (:x %) (+ width x))
                (<= y (:y %) (+ height y)))
          boardset))

(defn life-condition [liveset cell]
  (let [count (count-live-neighbors liveset cell)]
    (or (= 3 count)
        (and (= 2 count)
             (contains? liveset cell)))))

(defn next-turn [liveset]
  (select (partial life-condition liveset)
          (breedset liveset)))
