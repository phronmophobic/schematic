(ns com.phronemophobic.membrane.svg
  (:require [membrane.skia :as skia]
            [membrane.ui :as ui])
  (:import com.sun.jna.Pointer))

(def membraneskialib (try
                       (com.sun.jna.NativeLibrary/getInstance "membraneskia")
                       (catch java.lang.UnsatisfiedLinkError e
                         nil)))

(defmacro defc
  ([fn-name lib ret]
   `(defc ~fn-name ~lib ~ret []))
  ([fn-name lib ret args]
   (let [cfn-sym (with-meta (gensym "cfn") {:tag 'com.sun.jna.Function})]
     `(if ~lib
        (let [~cfn-sym (.getFunction ~(with-meta lib {:tag 'com.sun.jna.NativeLibrary})
                                     ~(name fn-name))]
          (defn- ~fn-name [~@args]
            (.invoke ~cfn-sym
                     ~ret (to-array [~@args]))))
        (defn- ~fn-name [~@args]
          (throw (Exception. (str ~(name fn-name) " not loaded."))))))))

(def void Void/TYPE)

(defc skia_delete_path membraneskialib void [path])
(defc skia_make_path membraneskialib Pointer [])

(defc skia_reset_path membraneskialib void [path])

(defc skia_skpath_moveto membraneskialib void [path, x, y])
(defn skia-skpath-moveto [path, x, y]
  (skia_skpath_moveto path, (double x), (double y)))

(defc skia_skpath_lineto membraneskialib void [path, x, y])
(defn skia-skpath-lineto [path, x, y]
  (skia_skpath_lineto path, (double x), (double y)))

(defc skia_skpath_arcto membraneskialib void [path, x1, y1, x2, y2, radius])
(defn skia-skpath-arcto [path, x1, y1, x2, y2, radius]
  (skia_skpath_arcto path, (double x1), (double y1), (double x2), (double y2), (double radius)))

(defc skia_skpath_cubicto membraneskialib void [path, x1, y1, x2, y2, x3, y3])
(defn skia-skpath-cubicto [path, x1, y1, x2, y2, x3, y3]
  (skia_skpath_cubicto path, (double x1), (double y1), (double x2), (double y2), (double x3), (double y3)))

(defc skia_skpath_conicto membraneskialib void [path, x1, y1, x2, y2, w])
(defn skia-skpath-conicto [path, x1, y1, x2, y2, w]
  (skia_skpath_conicto path, (double x1), (double y1), (double x2), (double y2), (double w)))

(defc skia_skpath_close membraneskialib void [path])

(defc skia_skpath_draw membraneskialib void [resource path])

(defmacro print-svg-call [[fname _ & args :as form]]
  `(do #_(prn ~(apply list
                 'list
                 (list 'quote fname) args))
       ~form
       ))

(defn process-path-commands [path commands]
  (reduce
   (fn [pos [command-type & args :as command]]
     (case command-type
       :move-to-absolute
       (let [[[x y]] args]
         (do (print-svg-call (skia-skpath-moveto path x y))
             [x y]))

       ;; :moveto
       ;; (let [[[dx dy]] args
       ;;       [x y] pos
       ;;       x' (+ x dx)
       ;;       y' (+ y dy)]
       ;;   (do (print-svg-call (skia-skpath-moveto path x' y'))
       ;;       [x' y']))

       ;; :lineto
       ;; (let [[[dx dy]] args
       ;;       [x y] pos
       ;;       x' (+ x dx)
       ;;       y' (+ y dy)]
       ;;   (do (print-svg-call (skia-skpath-lineto path x' y'))
       ;;       [x' y']))

       :line-to-absolute
       (let [[[x y]] args]
         (print-svg-call
          (skia-skpath-lineto path x y))
         [x y])

       :curve-to-absolute
       (let [[[x1 y1] [x2 y2] [x3 y3]] (first args)]
         (print-svg-call
          (skia-skpath-cubicto path x1 y1 x2 y2 x3 y3))
         [x3 y3])

       :close-path
       (do
         (print-svg-call
          (skia_skpath_close path))
         pos)))
   [0 0]
   commands)

  nil)

(defmacro with-path [path & body]
  `(let [~path (skia_make_path)]
     (try
       ~@body
       (finally
         (skia_delete_path ~path)))))

(defn draw-svg-commands [commands]
  (with-path path
    (process-path-commands path commands)
    (skia_skpath_draw skia/*skia-resource* path)))


(defrecord SvgPath [commands origin bounds]
  ui/IOrigin
  (-origin [_]
    origin)

  ui/IBounds
  (-bounds [_]
    bounds)


  skia/IDraw
  (draw [this]
    (draw-svg-commands commands)))

(defn svg-path
  ([commands origin bounds]
   (SvgPath. commands origin bounds))
  ([commands]
   (SvgPath. commands [0 0] [0 0]))) 
