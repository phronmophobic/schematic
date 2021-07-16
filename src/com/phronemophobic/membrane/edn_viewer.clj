(ns com.phronemophobic.membrane.edn-viewer
  (:require [membrane.ui :as ui]
            ;;[membrane.skija :as backend]
            [membrane.skia :as backend]
            [membrane.basic-components :as basic]
            [com.phronemophobic.membrane.scrollview :as sv]
            [membrane.component :refer [defui defeffect]]
            ))

(def test-data
  '(def employees
    {:employees [{:name 1
                  :case1 600
                  :case2 800
                  :case3 1000}
                 {:name 1
                  :case1 600
                  :case2 800
                  :case3 1000}
                 {:name 1
                  :case1 600
                  :case2 800
                  :case3 1000}
                 {:name 1
                  :case1 600
                  :case2 800
                  :case3 1000}
                 {:name 1
                  :case1 600
                  :case2 800
                  :case3 1000}]}))

(defn get-color [obj]
  (cond
    (keyword? obj) [0 0 1 0.8] ;;[0.46666666865348816 0.0 0.5333333611488342],
    (number? obj) [0.06666667014360428 0.4000000059604645 0.2666666805744171],
    (= obj 'def) [0.0 0.0 1.0],

    (string? obj) [0.6666666865348816 0.06666667014360428 0.06666667014360428],
    :else [0 0 0]))

(defn depth-draw [obj]
  (let [padding 8

        gray 0.9
        border-color [gray gray gray]]
    (cond
      (map? obj)
      (ui/with-color [0 1 0 0.1]
        (let [childs (ui/padding padding padding
                                 (ui/table-layout
                                  (for [[k v] obj]
                                    [(depth-draw k)
                                     (depth-draw v)])
                                  padding padding))
              [cw ch] (ui/bounds childs)]
          [(ui/rounded-rectangle
            (+ padding cw) (+ padding ch)
            8)
           (ui/with-style :membrane.ui/style-stroke
             (ui/with-color border-color
               (ui/rounded-rectangle
                (+ padding cw) (+ padding ch)
                8)))
           childs]))

      (vector? obj)
      (ui/with-color [1 1 1]
        (let [childs (ui/padding padding padding
                                 (apply
                                  ui/vertical-layout
                                  (interpose
                                   (ui/spacer (/ padding 2))
                                   (for [x obj]
                                     (depth-draw x)))))
              [cw ch] (ui/bounds childs)]
          [(ui/rounded-rectangle
            (+ padding cw) (+ padding ch)
            8)
           (ui/with-style :membrane.ui/style-stroke
             (ui/with-color border-color
               (ui/rounded-rectangle
                (+ padding cw) (+ padding ch)
                8)))
           childs]))

      (and (seqable? obj)
           (not (string? obj)))
      (ui/with-color [0.95 0.95 0.95]
        (let [childs (ui/padding padding padding
                                 (apply
                                  ui/vertical-layout
                                  (interpose
                                   (ui/spacer (/ padding 2))
                                   (for [x obj]
                                     (depth-draw x)))))
              [cw ch] (ui/bounds childs)]
          [(ui/rounded-rectangle
            (+ padding cw) (+ padding ch)
            8)
           (ui/with-style :membrane.ui/style-stroke
             (ui/with-color border-color
               (ui/rounded-rectangle
                (+ padding cw) (+ padding ch)
                8)))
           childs]))


      :else
      (ui/with-color (get-color obj)
        (ui/label (pr-str obj))))))





(comment
  (backend/run (constantly (depth-draw test-data)))

  (backend/draw-to-image! "boxes.png"
                          (depth-draw
                           (read-string
                            (slurp "https://raw.githubusercontent.com/seancorfield/dot-clojure/develop/deps.edn")))))
