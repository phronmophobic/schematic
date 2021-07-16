(ns com.phronemophobic.membrane.scrollview
  (:require [membrane.ui :as ui
             :refer [on
                     vertical-layout
                     horizontal-layout]]
            [membrane.skia :as backend]
            [liq.buffer :as buffer]
            [membrane.components.code-editor.code-editor
             :as code-editor]
            [membrane.component
             :refer [defeffect defui make-app]]
            [membrane.basic-components :as basic]
            [datascript.core :as d]
            
            ))




(defn scrollview* [offset zoom scroll-bounds body]
  (let [offset-x (nth offset 0)
        offset-y (nth offset 1)
        [width height] scroll-bounds
        
        scissor-body (if (= 1 zoom)
                       body
                       (ui/scale zoom zoom
                                 body))
        scissor-body (ui/translate offset-x offset-y
                                   scissor-body)
        
        scissor-elem (ui/scissor-view [0 0]
                                      scroll-bounds
                                      scissor-body)]
    scissor-elem))

(defn maybe-mouse-move [test on-mouse-move body]
  (if test
    (ui/wrap-on
     :mouse-move
     (fn [handler pt]
       (if-let [steps (seq (handler pt))]
         steps
         (on-mouse-move pt)))
     body)
    body))

(defn maybe-mouse-up [test on-mouse-up body]
  (if test
    (ui/wrap-on
     :mouse-up
     (fn [handler pt]
       (if-let [steps (seq (handler pt))]
         steps
         (on-mouse-up pt)))
     body)
    body))



(defui scrollview
  "Basic scrollview.

  scroll-bounds should be a two element vector of [width height] of the scrollview
  body should be an element.
"
  [{:keys [offset zoom zooming? panning? shift-down? mdown scroll-bounds body]
    :or {offset [0 0]
         zoom 1}}]
  (basic/on-mouse-out
   {:mouse-out (fn []
                 [[:set $zooming? false]
                  [:set $panning? false]])
    :body
    (maybe-mouse-up
     (or zooming? panning?)
     (fn [mpos]
       (when mdown
         (if panning?
           (let [ox (nth offset 0)
                 oy (nth offset 1)

                 mx1 (nth mdown 0)
                 my1 (nth mdown 1)

                 mx2 (nth mpos 0)
                 my2 (nth mpos 1)]
             [[:set $offset [(+ ox
                                (- mx2 mx1))
                             (+ oy
                                (- my2 my1))]]
              [:set $mdown nil]
              [:set $panning? false]])
           ;; zooming
           (let [mx1 (nth mdown 0)
                 my1 (nth mdown 1)

                 mx2 (nth mpos 0)
                 my2 (nth mpos 1)

                 zoom-dir (if (or (< mx2 mx1)
                                  (< my2 my1))
                            -
                            +)

                 zoom-delta (zoom-dir
                             (/ (Math/sqrt
                                 (+ (* (- mx2 mx1) (- mx2 mx1))
                                    (* (- my2 my1) (- my2 my1))))
                                100.0))]
             [[:set $zoom (+ zoom zoom-delta)]
              [:set $mdown nil]
              [:set $zooming? false]])
           )
         ))
     (maybe-mouse-move
      (or zooming? panning?)
      (fn [mpos]
        (when mdown
          (if panning?
            (let [ox (nth offset 0)
                  oy (nth offset 1)

                  mx1 (nth mdown 0)
                  my1 (nth mdown 1)

                  mx2 (nth mpos 0)
                  my2 (nth mpos 1)]
              [[:set $offset [(+ ox
                                 (- mx2 mx1))
                              (+ oy
                                 (- my2 my1))]]
               [:set $mdown mpos]])
            ;; zooming
            (let [mx1 (nth mdown 0)
                  my1 (nth mdown 1)

                  mx2 (nth mpos 0)
                  my2 (nth mpos 1)

                  zoom-dir (if (or (< mx2 mx1)
                                   (< my2 my1))
                             -
                             +)

                  zoom-delta (zoom-dir
                              (/ (Math/sqrt
                                  (+ (* (- mx2 mx1) (- mx2 mx1))
                                     (* (- my2 my1) (- my2 my1))))
                                 100.0))]
              [[:set $zoom (+ zoom zoom-delta)]
               [:set $mdown mpos]])
            
            )))
      (ui/wrap-on
       :key-event
       (fn [handler key scancode action mods]
         (if (#{344 340} key)
           [[:set $shift-down? (not= action :release)]]
           (handler key scancode action mods)))
       :mouse-down
       (fn [handler mpos]
         (let [ox (nth offset 0)
               oy (nth offset 1)
               mx (nth mpos 0)
               my (nth mpos 1)

               local-mpos [(* zoom (- mx ox))
                           (* zoom (- my oy))]]
           (if-let [steps (seq (handler mpos))]
             steps
             (let [$key (if shift-down?
                          $zooming?
                          $panning?)
                   $other-key (if (not shift-down?)
                                $zooming?
                                $panning?)]
               [[:set $key true]
                [:set $other-key false]
                [:set $mdown mpos]]))))
       (scrollview* offset zoom scroll-bounds body))))}))


(defui test-scrollview [{:keys [offset]}]
  (let [offset (or offset [0 0])]
    [(ui/with-style :membrane.ui/style-stroke
       (ui/rectangle 450 450))
     (scrollview {:offset offset
                  :body
                  (ui/filled-rectangle [1 0 0] 150 100)
                  :scroll-bounds [450 450]})]))

(comment
  (backend/run (make-app #'test-scrollview))
  ,)
