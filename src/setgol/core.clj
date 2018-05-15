(ns setgol.core
  (:require [setgol.rleparse :refer [load-rle-file]]
            [setgol.game :refer [next-turn select-range stats translate positive]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :refer [join]]))

(defn clear-screen []
  (print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H"))) ; move cursor to the top left corner of the screen

(defn terminal-size []
  (let [out (:out (sh "/bin/sh" "-c" "stty -a < /dev/tty"))]
    {:width (quot (- (->> out
                          (re-find #"columns (\d+)")
                          (second)
                          (Integer.)) 4) 2)
     :height (- (->> out
                     (re-find #"rows (\d+)")
                     (second)
                     (Integer.)) 4)}))

(defn bar-line [width pos]
  (let [top? #(= pos :top)
        left (if (top?) \┏ \┗)
        right (if (top?) \┓ \┛)
        middle (join (repeat (+ 1 (* 2 width)) \━))]
    (str left middle right)))

(defn print-board [board {:keys [width height]}]
  (let [board (positive board)
        {:keys [x y]} (:mid (stats board))
        board (translate board {:x #(+ % (- (quot width 2) x))
                                :y #(+ % (- (quot height 2) y))})
        board (select-range board {:x 0 :y 0 :width width :height height})
        empty-game (->> '\ 
                        (repeat width)
                        (into [])
                        (repeat (inc height))
                        (into []))
        fill-cell (fn [game {:keys [x y]}]
                    (assoc-in game [y x] '●))
        game (reduce fill-cell empty-game board)]
    (clear-screen)
    (println (bar-line width :top))
    (doseq [row game]
      (apply println (into [\┃] (conj row \┃))))
    (println (bar-line width :bottom))))

(defn -main
  ([name]
   (-main name 0))
  ([name rate]
   (-main name rate 0))
  ([name rate count]
   (def gameboard (atom (into #{} (load-rle-file name))))
   (loop [n 0]
     (let [terminal-size (terminal-size)]
       (swap! gameboard next-turn)
       (print-board @gameboard terminal-size)
       (Thread/sleep (Integer. rate))
       (cond (zero? n)
             (recur 0)
             (< n (Integer. count))
             (recur (inc n)))))))

;; (time (-main "copperhead.rle" 0 1)) 
