(ns setgol.rleparse
  (:require [clojure.string :as str]))

(defrecord Section [count symbol])

(defn build-section [str-section]
  (let [digits (or (re-find #"[0-9]*" str-section) 1)
        count (Integer. (if (empty? digits) 1 digits))
        symbol (re-find #"[a-z|$]" str-section)]
    (->Section count symbol)))

(defn build-state [rle]
  (let [sections
        (->> rle
             (re-seq #"[0-9]*[a-z|$]")
             (map build-section))
        dec-section #(assoc % :count (dec (:count %)))]
    (loop [section (first sections)
           sections (rest sections)
           x 0
           y 0
           result '()]
      (cond (= 0 (:count section))
            (recur (first sections) (rest sections) x y result)
            (= "b" (:symbol section))
            (recur (dec-section section)
                   sections
                   (inc x)
                   y
                   result)
            (= "o" (:symbol section))
            (recur (dec-section section)
                   sections
                   (inc x)
                   y
                   (conj result {:y y :x x}))
            (= "$" (:symbol section))
            (recur (dec-section section)
                   sections
                   0
                   (inc y)
                   result)
            :else result))))

(defn load-rle-file [name]
  (let [rle (slurp name)
        comment? #(str/starts-with? % "#")
        header? #(str/starts-with? % "x = ")
        rle
        (->> rle
             (str/split-lines)
             (filter #(not (or (comment? %)
                               (header? %))))
             (apply str))]
    (build-state rle)))
