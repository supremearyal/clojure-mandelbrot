(ns mandelbrot.core
  (:import (javax.swing JFrame JLabel ImageIcon)
           (java.awt.image BufferedImage)
           (java.io File)
           (javax.imageio ImageIO))
  (:gen-class))

(set! *unchecked-math* true)

(defn mandelbrot-function [[^float a ^float b] [^float c ^float d]]
  (let [as (* a a)
        bs (* b b)
        ab (* a b)]
    [(+ c (- as bs))
     (+ d (+ ab ab))]))

(defn mandelbrot-unbounded? [[^float a ^float b]]
  (> (+ (* a a)
        (* b b))
     4.0))

(def iterations 100)

(defn mandelbrot-escapes-count [[a b]]
  (loop [^int i      iterations
              [c d]  [0 0]]
    (if (or (zero? i)
            (mandelbrot-unbounded? [c d]))
      (- iterations i)
      (recur (dec i)
             (mandelbrot-function [c d] [a b])))))

(def width 1366)

(def height 768)

(defn gray-gradient [^long level ^long divisions]
  (let [scaled-level      (* (Math/sqrt divisions)
                             (Math/sqrt level))
        white             (int 0xff)
        ratio             (/ scaled-level divisions)
        gray              (int (* white ratio))
        alpha-mask        (unchecked-int 0xff000000)
        red-shift         (unchecked-int 16)
        green-shift       (unchecked-int 8)
        blue-shift        (unchecked-int 0)
        color-no-alpha    (bit-not (bit-or (bit-shift-left gray red-shift)
                                           (bit-shift-left gray green-shift)
                                           (bit-shift-left gray blue-shift)))]
    (bit-or alpha-mask color-no-alpha)))

(defn mandelbrot-pixels []
  (let [x-size 4
        y-size (/ x-size
                  (float (/ width height)))]
    (for [x (range width)
          :let [normalize-x  (float (/ x width))
                full-x       (* x-size normalize-x)
                s            (- full-x 3)]
          y (range height)
          :let [normalize-y   (float (/ y height))
                invert-y      (- 1 normalize-y)
                full-y        (* y-size invert-y)
                t             (- full-y (/ y-size 2))
                escape-count  (mandelbrot-escapes-count [s t])]]
      [x y (gray-gradient escape-count iterations)])))

(defn mandelbrot-buffer [pixels]
  (let [buffer (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
    (doseq [[x y color] pixels]
      (.setRGB buffer x y color))
    buffer))

(defn -main [& args]
  (let [buffer (mandelbrot-buffer (mandelbrot-pixels))
        file   (first args)
        output (if file
                 (File. (first args))
                 nil)]
    (doto (JFrame.)
      (.add (JLabel. (ImageIcon. buffer)))
      (.pack)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setVisible true))
    (if output
      (ImageIO/write buffer "png" output))))
