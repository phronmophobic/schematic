(ns com.phronemophobic.membrane.figma
  (:require [clojure.java.io :as io]
            ;;[clj-oauth2.client :as oauth2]
            [clojure.data.json :as json]
            [membrane.ui :as ui]
            [meander.epsilon :as m]
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;[membrane.skija :as backend]
            [membrane.skia :as backend]
            [membrane.skia :as skia]
            [com.phronemophobic.membrane.search :as search]
            [clojure.zip :as z]
            [clj-http.client :as http]
            [membrane.basic-components :as basic]
            [clojure.data.xml :as xml]
            [com.phronemophobic.clj-rtree :as rt]
            [com.phronemophobic.membrane.scrollview :as sv]
            [com.phronemophobic.membrane.schematic2 :as s2]
            [com.phronemophobic.membrane.svg :as svg]
            
            [com.phronemophobic.membrane.svg-path-parse :refer [parse-svg-path]]
            [membrane.component :refer [defui defeffect]]
            [clojure.data.codec.base64 :as b64])
  (:import java.time.Instant
           java.awt.font.FontRenderContext
           java.awt.Font
           [java.net URLEncoder])

  )


(defn hex->color [s]
  (->> s
       (partition 2)
       (into []
             (comp (map #(apply str %))
                   (map #(Integer/parseInt % 16))
                   (map #(/ % 255.0))))))

(defn ui-zip [obj]
  (z/zipper #(seq (ui/children %))
            ui/children
            (fn [node s]
              node)
            obj))

;; find-fills
(def fill-types #{"SOLID"
                  "GRADIENT_LINEAR"
                  "GRADIENT_RADIAL"
                  "GRADIENT_ANGULAR"
                  "GRADIENT_DIAMOND"
                  "IMAGE"
                  "EMOJI"})


;; You can now use the token to make requests to the Figma API. You can do so by passing the token to the API in the header of your request. We’ve shown you an example of how this would look.
;; The name of the token header is X-Figma-Token.
(def secrets (clojure.edn/read-string (slurp "secrets.edn")))
(def access-token (-> secrets :figma :access-token))


(defn get-files [fkey]
  (-> (http/get (str "https://api.figma.com/v1/files/" fkey)
                {:headers {"X-Figma-Token" access-token}
                 :query-params {"geometry" "paths"}
                 :throw-exceptions false})
      :body
      json/read-str))

(defn get-component-sets [fkey]
  
  (-> (http/get (format "https://api.figma.com/v1/files/%s/component_sets" fkey)
                {:headers {"X-Figma-Token" access-token}
                 :throw-exceptions false})
      :body
      json/read-str))


(defn get-components [fkey]
  
  (-> (http/get (format "https://api.figma.com/v1/files/%s/components" fkey)
                {:headers {"X-Figma-Token" access-token}
                 :throw-exceptions false})
      :body
      json/read-str))

(comment
  (get-component-sets "sAEkytxl4mv5COV7unNcjV")
  (get-components "sAEkytxl4mv5COV7unNcjV")
  ,
  )

#_(spit "/var/tmp/figma-body.edn" (pr-str body))


(defn get-styles [skey]
  (-> (http/get (str "https://api.figma.com/v1/styles/" skey)
             {:headers {"X-Figma-Token" access-token}
              :throw-exceptions false})
      :body
      json/read-str))



(defn get-svg [fkey ids]
  (-> (http/get (str "https://api.figma.com/v1/images/" fkey )
                {:headers {"X-Figma-Token" access-token}
                 :query-params {"format" "svg"
                                "svg_include_id" "true"
                                "ids" (clojure.string/join ","
                                                           ids)}
                 
                 :throw-exceptions false})
      :body
      json/read-str
      )
  )

(defui figma-viewer [{:keys [offset
                             zoom
                             view]
                      :or {zoom 1
                           offset [0 0] }}]
  (ui/vertical-layout
   (ui/horizontal-layout
    (basic/button {:text "reset"
                   :on-click (fn []
                               [[:set $offset [0 0]]
                                [:set $zoom 1]])})
    (basic/button {:text "reset zoom"
                   :on-click (fn []
                               [[:set $zoom 1]])}))
   (ui/label (pr-str offset))
   (ui/label zoom)
   (sv/scrollview {:offset offset
                   :zoom zoom
                   :scroll-bounds [1200 800]
                   :body view})))

(defonce figma-state (atom {}))


(defn figma->click-handler [doc]
  (let [rtree
        (-> doc
            (search/find-all #(and (map? %)
                                   (contains? % :absolute-bounding-box)))
            (->> (map (fn [z]
                        (let [m (z/node z)
                              {{:keys [x y width height]} :absolute-bounding-box} m]
                          {:x x
                           :y y
                           :w width
                           :h height
                           :zobj z
                           :obj m})))
                 (filter (fn [{:keys [w h]}]
                           (and w h
                                (pos? (* w h)))))
                 )
            (rt/rtree))]
    (fn [pt]
      (let [entries (rt/search rtree pt)]
        (when-let [entry (->> entries
                              (sort-by (fn [{:keys [w h]}]
                                         (* w h)))
                              first)]
          [[::select (:obj entry) (:zobj entry)]])))))

(defonce selection (atom nil))
(defonce zelection (atom nil))

(defeffect ::select [m z]
  (reset! selection m)
  (reset! zelection z)
  
  (tap> m))

(declare figma->membrane
         normalize-bounds)
(defn load-view []
  (def design-system-response
    (get-files
     ;; "LX4ucdBEjBu3Bpnw95cJ3E" ;; tutorial design system
     ;; "lGvV046zHH9xwj8OMX2xZB" ;;material design free trial junk
     ;; "ukH49eAeYCjrszVHQ2npTa" ;; bootstrap
     ;; "pIOYSSejwqteM3A68PlF2s" ;; more material design junk
     ;; "82wr5v8a7NKbnjuOGxrHRv" ;; dashboard
     "sAEkytxl4mv5COV7unNcjV" ;; Atlassian design system! :D
     ))

  
  (def styles (get-file-styles
                ;; Atlassian design system! :D
               "sAEkytxl4mv5COV7unNcjV"))

  (def body design-system-response)

  (def document (get body "document"))

  (def view (-> document
                (get "children")
                (nth 6)
                normalize-bounds
                figma->membrane))
  (def views (-> document
                 (get "children")
                 (->> (map normalize-bounds)
                      (map figma->membrane))))

  (print "loaded")
  ,)


(defn get-styles [obj]
  (->> (search/keep-all obj
                        (fn [m]
                          (seq
                           (keep #(when-let [x (get m %)]
                                    (when (string? x)
                                      x))
                                 ["fill" "fills" "stroke" "strokes" :fill :fills :stroke :strokes]))))
       
       (sequence cat)
       ;; (map ads-styles)
       ;; frequencies
       ))

(defn find-style [obj style-id]
  (let [
        zelem (search/find obj
                           (fn [m]
                             (some #(= style-id
                                       (get m %))
                                   ["fill" "fills" "stroke" "strokes" :fill :fills :stroke :strokes])))
        path (loop [path ()
                    zelem zelem]
               (if zelem
                 (let [name (get (z/node zelem) :name
                                 (get (z/node zelem) "name"))]
                   (if name
                     (recur (conj path name)
                            (z/up zelem))
                     (recur path
                            (z/up zelem))))
                 path))]
    path))

(comment

  (do
    
    (def rects (-> view
                   (search/find-all #(and (map? %)
                                          (contains? % :absolute-bounding-box)))
                   (->> (map z/node)
                        (filter (fn [m]
                                  (= "TEXT"
                                     (get m :type))))
                        (map (fn [m]
                               (let [{{:keys [x y width height]} :absolute-bounding-box} m]
                                 {:x x
                                  :y y
                                  :w width
                                  :h height
                                  :obj m})))
                        (filter (fn [{:keys [w h]}]
                                  (and w h
                                       (pos? (* w h)))))
                        (mapv (fn [{:keys [x y w h]}]
                                (ui/translate x y
                                              [#_(ui/with-style
                                                   :membrane.ui/style-fill
                                                   (ui/label [x y]))
                                               (ui/rectangle w h)]))))))

    

    (do
      (swap! figma-state
             assoc 
             :view
             [#_(ui/on
                 :mouse-down
                 (figma->click-handler view)
                 (ui/no-events view))
              
              (ui/no-events (ui/->Cached [view]))

              #_(ui/no-events
                 (ui/with-style :membrane.ui/style-stroke
                   rects))])
      nil))
  (backend/run (membrane.component/make-app #'figma-viewer figma-state))



  ,)


(defn figma-color->membrane [color]
  [(get color "r")
   (get color "g")
   (get color "b")
   (get color "a")])

(defn render-children [body m]
  (when (seq (:children m))
    [body (:children m)]))


"absoluteBoundingBox"
"fills"
"cornerRadius"
"rectangleCornerRadii"
;; property counts
;; ["gradientStops" 18]
;;   ["gradientHandlePositions" 18]
;;   ["scaleMode" 22]
;;   ["imageRef" 22]
;;   ["opacity" 59]
;;   ["visible" 240]
;;   ["color" 1069]
;;   ["type" 1109] ;; mostly SOLID, also image and gradient_linear
;;   ["blendMode" 1109] ;; always NORMAL



(comment
  (-> (search/find-all components (fn [x]
                                     (and (map? x)
                                          (= "COMPONENT" (get x "type"))
                                          (= "Textfield/Active" (get x "name")))))
      (->> (into []
                 (comp
                  (map z/node)
                  )))
       first

       (search/find-all (fn [m]
                          (m/find m {"effects" ?fills}
                            (seq ?fills))))
       (->> (map z/node)
            (map #(get % "effects"))))
  )



(defn render-fills [body m]
  (when (#{"COMPONENT"
           "FRAME"
           "INSTANCE"
           "RECTANGLE"
           "VECTOR"
           "STAR"
           "LINE"
           "ELLIPSE"
           "REGULAR_POLYGON"} (:type m))
    (let [{:keys [fills
                  absolute-bounding-box
                  corner-radius
                  rectangle-corner-radii]} m
          active-fills (->> fills
                            (filter (fn [fill]
                                      (get fill :visible true))))]
      (when (seq active-fills)
        (let [fill (first active-fills)
              we-can-do-this (and (= 1
                                     (count active-fills))
                                  (or (not (seq rectangle-corner-radii))
                                      (every? #(= (first rectangle-corner-radii)
                                                  %)
                                              (rest rectangle-corner-radii)))
                                  (= "SOLID"
                                     (:type fill)))
              corner-radius (if (seq rectangle-corner-radii)
                              (first rectangle-corner-radii)
                              corner-radius)]
          (if (not we-can-do-this)
            [body #_(ui/label "FILL :D")]
            (let [color (:color fill)
                  ;; element opacity
                  color (if-let [opacity (:opacity m)]
                          (update color 3 * opacity)
                          color)
                  ;; fill opacity
                  color (if-let [opacity (:opacity fill)]
                          (update color 3 * opacity)
                          color)
                  
                  {:keys [width height]} absolute-bounding-box]
              ;; (prn color (:opacity m) m)
              [body
               
               (ui/with-color color
                 (ui/with-style :membrane.ui/style-fill
                   (if corner-radius
                     (ui/rounded-rectangle width height corner-radius)
                     (if-let [geoms (:fill-geometry m)]
                       (into []
                             (comp (map :path)
                                   (map parse-svg-path)
                                   (map svg/svg-path))
                             geoms)
                       (ui/rectangle width height)))))])))))))

(defn render-transform [body m]
  #_(skia/transform (into []
                        cat
                        #_[[1 0] [0 1] [0 0]]
                        [[1 0 0]
                         [0 1 0]])
                  
                    body)
  (let [{:keys [x y]} (:absolute-bounding-box m)]
      (when (and x y)
        (ui/translate x y
                      body)))

  #_(if-let [transform (:relative-transform m)]
    (skia/transform (into []
                          cat
                          transform)
                    body)
    (let [{:keys [x y]} (:absolute-bounding-box m)]
      (when (and x y)
        (ui/translate x y
                      body)))))


(defn render-strokes [body m]
  (when (#{"COMPONENT"
           "FRAME"
           "INSTANCE"
           "RECTANGLE"
           "VECTOR"
           "STAR"
           "LINE"
           "ELLIPSE"
           "REGULAR_POLYGON"} (:type m))
    (let [{:keys [strokes
                  absolute-bounding-box
                  corner-radius
                  rectangle-corner-radii]} m
          active-strokes (->> strokes
                              (filter (fn [stroke]
                                        (get stroke :visible true))))]
      (when (seq active-strokes)
        (let [stroke (first active-strokes)
              we-can-do-this (and (= 1
                                     (count active-strokes))
                                  (or (not (seq rectangle-corner-radii))
                                      (every? #(= (first rectangle-corner-radii)
                                                  %)
                                              (rest rectangle-corner-radii)))
                                  (= "SOLID"
                                     (:type stroke)))
              corner-radius (if (seq rectangle-corner-radii)
                              (first rectangle-corner-radii)
                              corner-radius)]
          (if (not we-can-do-this)
            [body #_(ui/label "STROKE :D")]
            (let [color (:color stroke)
                  color (if-let [opacity (:opacity m)]
                          (update color 3 * opacity)
                          color)
                  {:keys [width height]} absolute-bounding-box]
              [body
               (ui/with-color color
                 (ui/with-style :membrane.ui/style-stroke
                   (if corner-radius
                     (ui/rounded-rectangle width height corner-radius)
                     (if-let [geoms (:stroke-geometry m)]
                       (into []
                             (comp (map :path)
                                   (map parse-svg-path)
                                   (map svg/svg-path))
                             geoms)
                       (ui/rectangle width height)))))])))))))


(defn render-stroke-weight [body m]
  (when-let [stroke-weight (:stroke-weight body)]
    (ui/with-stroke-width stroke-weight
      body)))

:clips-content
:absolute-bounding-box
(defn render-clips-content [body m]
  (when (:clips-content m)
    (let [{:keys [width height]} (:absolute-bounding-box m)]
      (ui/scissor-view [0 0]
                       [width height]
                       body))))


(defn render-effects [body m]
  #_(when (seq (:effects m))
    [body (ui/label "Effects!")]))



(defn subpath [folder fname]
  (let [parent (clojure.java.io/file folder)
        child (clojure.java.io/file parent fname)]
    (prn (.startsWith (.normalize (.toPath child))
                      (.toPath parent)))
    (assert (.startsWith (.normalize (.toPath child))
                         (.toPath parent)))
    (.getCanonicalPath child)))

(defn get-font-path [font-name weight]
  (case font-name
    ;; "Monaco" "/System/Library/Fonts/Monaco.dfont"

    "Work Sans"
    (case weight
      "Black"             "/Users/adrian/Library/Fonts/WorkSans-Black.ttf"
      "BlackItalic"       "/Users/adrian/Library/Fonts/WorkSans-BlackItalic.ttf"
      "Bold"              "/Users/adrian/Library/Fonts/WorkSans-Bold.ttf"
      "BoldItalic"        "/Users/adrian/Library/Fonts/WorkSans-BoldItalic.ttf"
      "ExtraBold"         "/Users/adrian/Library/Fonts/WorkSans-ExtraBold.ttf"
      "ExtraBoldItalic"   "/Users/adrian/Library/Fonts/WorkSans-ExtraBoldItalic.ttf"
      "ExtraLight"        "/Users/adrian/Library/Fonts/WorkSans-ExtraLight.ttf"
      "ExtraLightItalic"  "/Users/adrian/Library/Fonts/WorkSans-ExtraLightItalic.ttf"
      "Italic"            "/Users/adrian/Library/Fonts/WorkSans-Italic.ttf"
      "Light"             "/Users/adrian/Library/Fonts/WorkSans-Light.ttf"
      "LightItalic"       "/Users/adrian/Library/Fonts/WorkSans-LightItalic.ttf"
      "Medium"            "/Users/adrian/Library/Fonts/WorkSans-Medium.ttf"
      "MediumItalic"      "/Users/adrian/Library/Fonts/WorkSans-MediumItalic.ttf"
      "Regular"           "/Users/adrian/Library/Fonts/WorkSans-Regular.ttf"
      "SemiBold"          "/Users/adrian/Library/Fonts/WorkSans-SemiBold.ttf"
      "SemiBoldItalic"    "/Users/adrian/Library/Fonts/WorkSans-SemiBoldItalic.ttf"
      "Thin"              "/Users/adrian/Library/Fonts/WorkSans-Thin.ttf"
      "ThinItalic"        "/Users/adrian/Library/Fonts/WorkSans-ThinItalic.ttf"
      ;;else
      "/Users/adrian/Library/Fonts/WorkSans-Regular.ttf"
      ,)

    "Inter"
    (case weight
      "Black"       "/Users/adrian/Library/Fonts/Inter-Black.ttf"
      "Bold"        "/Users/adrian/Library/Fonts/Inter-Bold.ttf"
      "ExtraBold"   "/Users/adrian/Library/Fonts/Inter-ExtraBold.ttf"
      "ExtraLight"  "/Users/adrian/Library/Fonts/Inter-ExtraLight.ttf"
      "Light"       "/Users/adrian/Library/Fonts/Inter-Light.ttf"
      "Medium"      "/Users/adrian/Library/Fonts/Inter-Medium.ttf"
      "Regular"     "/Users/adrian/Library/Fonts/Inter-Regular.ttf"
      "SemiBold"    "/Users/adrian/Library/Fonts/Inter-SemiBold.ttf"
      "Thin"        "/Users/adrian/Library/Fonts/Inter-Thin.ttf"
      ;; else
      "/Users/adrian/Library/Fonts/Inter-Regular.ttf")

    ("SF UI Text" "SF Pro Text")
     (case weight
       "Black"      "/Library/Fonts/SF-Pro-Text-Black.otf"
       "Bold"       "/Library/Fonts/SF-Pro-Text-Bold.otf"
       "Heavy"      "/Library/Fonts/SF-Pro-Text-Heavy.otf"
       "Light"      "/Library/Fonts/SF-Pro-Text-Light.otf"
       "Medium"     "/Library/Fonts/SF-Pro-Text-Medium.otf"
       "Regular"    "/Library/Fonts/SF-Pro-Text-Regular.otf"
       "Semibold"   "/Library/Fonts/SF-Pro-Text-Semibold.otf"
       "Thin"       "/Library/Fonts/SF-Pro-Text-Thin.otf"
       "Ultralight" "/Library/Fonts/SF-Pro-Text-Ultralight.otf"

       ;;else
       "/Library/Fonts/SF-Pro-Text-Regular.otf")

     "SF Pro Display"

     (case weight
       "Black" "/Library/Fonts/SF-Pro-Display-Black.otf"
       "Bold" "/Library/Fonts/SF-Pro-Display-Bold.otf"
       "Heavy" "/Library/Fonts/SF-Pro-Display-Heavy.otf"
       "Light" "/Library/Fonts/SF-Pro-Display-Light.otf"
       "Medium" "/Library/Fonts/SF-Pro-Display-Medium.otf"
       "Regular" "/Library/Fonts/SF-Pro-Display-Regular.otf"
       "Semibold" "/Library/Fonts/SF-Pro-Display-Semibold.otf"
       "Thin" "/Library/Fonts/SF-Pro-Display-Thin.otf"
       "Ultralight" "/Library/Fonts/SF-Pro-Display-Ultralight.otf"

       ;; else
       "/Library/Fonts/SF-Pro-Display-Regular.otf")

     ;;else
     nil
    ))

(def font-weight-names
  {100 "Thin"
   200 "ExtraLight"
   300 "Light"
   400 "Regular"
   500 "Medium"
   600 "SemiBold"
   700 "Bold"
   800 "Extra-Bold"
   900 "Black"})

(defn center-vertically [elem height]
  (let [[_ eheight] (ui/bounds elem)]
    (ui/translate 0
               (int (- (/ height 2)
                       (/ eheight 2)))
               elem)))

(defn render-text [body m]
  (when (= "TEXT" (:type m))
    (let [font-style (:style m)
          
          font-size (:font-size font-style)
          font-name (:font-family font-style)
          font-weight (:font-weight font-style)
          font-weight-name (if (number? font-weight)
                             (font-weight-names font-weight "Regular")
                             font-weight)
          text-case (:text-case font-style)
          label-str (case text-case
                      "UPPER"
                      (clojure.string/upper-case (:characters m))

                      "LOWER"
                      (clojure.string/lower-case (:characters m))

                      ;;else
                      (:characters m)
                      )

          line-height (:line-height-px font-style)
          line-gap (- line-height font-size)

          fill (->> (:fills m)
                    (filter (fn [fill]
                              (get fill :visible true)))
                    first)
          color (or (:color fill)
                    [0 0 0 0])
          

          font-path (get-font-path font-name font-weight-name)]
      [body
       #_(ui/with-style :membrane.ui/style-stroke
           (ui/rectangle
            (-> m :absolute-bounding-box :width)
            (-> m :absolute-bounding-box :height)))
       (ui/with-color color
        (ui/translate 0 0 #_(- line-gap )
                      (ui/label label-str
                                (ui/font font-name font-size))))
       ])))


(defn render [m]
  (let [render-pipeline [render-strokes
                         render-fills
                         render-stroke-weight

                         
                         
                         render-effects
                         render-text
                         
                         ;; render-clips-content
                         render-transform
                         
                         render-children]]
    (when (get m :visible true)
      (reduce
       (fn [body f]
         (if-let [new-body (f body m)]
           new-body
           body))
       nil
       render-pipeline))))

(defmulti ->ast :type)

(defn ast-position [m]
  (if-let [{:keys [x y]} (:relative-bounding-box m)]
    {:element/position [x y]}
    (let [{:keys [x y]} (:absolute-bounding-box m)]
      (when (and x y)
        {:element/position [x y]}))))

(defn ast-background [m]
  (let [{:keys [strokes
                corner-radius
                rectangle-corner-radii]} m]
    
    (let [corner-radius (if corner-radius
                          corner-radius
                          (when (seq rectangle-corner-radii)
                            rectangle-corner-radii))
          strokes (mapv (fn [stroke]
                          (let [color (:color stroke)]
                            (merge
                             {:element/color color
                              :element/opacity (:opacity stroke)}
                             (when (contains? stroke :visible)
                               {:element/hidden (not (get stroke :visible))})
                             )))
                        (:strokes m))
          fills (mapv (fn [fill]
                        (let [color (:color fill)]
                          (merge
                           {:element/color color
                            :element/opacity (:opacity fill)
                            :element/fill-type (:type fill)}
                           
                           (when (contains? fill :visible)
                             {:element/hidden (not (get fill :visible))}))))
                      (:fills m))]
      (merge
       (when corner-radius
         {:element/corner-radius corner-radius})
       (when-let [opacity (:opacity m)]
         {:element/opacity (:opacity m)})
       (when (seq fills)
         {:element/fills fills})
       (when (seq strokes)
         {:element/strokes strokes})))))

(defmethod ->ast "TEXT" [m]
  (let [font-style (:style m)
        
        font-size (:font-size font-style)
        font-name (:font-family font-style)
        font-weight (:font-weight font-style)
        font-weight-name (if (number? font-weight)
                           (font-weight-names font-weight "Regular")
                           font-weight)
        text-case (:text-case font-style)
        label-str (case text-case
                    "UPPER"
                    (clojure.string/upper-case (:characters m))

                    "LOWER"
                    (clojure.string/lower-case (:characters m))

                    ;;else
                    (:characters m)
                    )

        line-height (:line-height-px font-style)
        line-gap (- line-height font-size)

        ;; fill (->> (:fills m)
        ;;           (filter (fn [fill]
        ;;                     (get fill :visible true)))
        ;;           first)
        ;; color (or (:color fill)
        ;;           [0 0 0 0])

        font (merge
              (when font-name
                {:font/name (get-font-path font-name font-weight-name)})
              (when font-size
                {:font/size font-size})
              (when font-weight
                {:font/weight font-weight}))
        

        font-path (get-font-path font-name font-weight-name)]
    (merge {:element/text label-str
            :element/type :element/label
            :element/id (:id m)}
           (ast-background m)
           (when-let [name (:name m)]
             {:element/name name})
           (when (contains? m :visible)
             {:element/hidden (not (:visible m))})
           (ast-position m)
           (when (seq font)
             {:element/font font}))))



(defn ast-frame [m]
  (let [auto? (and (= "AUTO" (:primary-axis-sizing-mode m))
                   (not= "NONE" (:layout-mode m)))
        layout (if auto?
                 {}
                 {:element/bounds ((juxt :width :height) (:absolute-bounding-box m))})

        padding (merge
                 (when-let [p (:padding-left m)]
                   {:padding/left p})
                 (when-let [p (:padding-right m)]
                   {:padding/right p})
                 (when-let [p (:padding-top m)]
                   {:padding/top p})
                 (when-let [p (:padding-bottom m) ]
                   {:padding/bottom p}))]
    (merge {:element/id (:id m)}
           (when-let [name (:name m)]
             {:element/name name})
           layout
           (when (contains? m :visible)
             {:element/hidden (not (:visible m))})
           (case (:layout-mode m)
             "VERTICAL" {:element/layout :vertical
                         :element/layout-spacing (:item-spacing m)}
             "HORIZONTAL" {:element/layout :horizontal
                           :element/layout-spacing (:item-spacing m)}
             nil)
           (when (seq padding)
             {:element/padding padding})
           (when-let [childs (seq (:children m))]
             {:element/children (into []
                                      (comp (map
                                             (let [{px :x py :y} (:absolute-bounding-box m {:x 0 :y 0})]
                                               (fn [child]
                                                 (let [{:keys [x y width height] :as bb} (:absolute-bounding-box child)
                                                       local-x (- x (or px 0))
                                                       local-y (- y (or py 0))]
                                                   (assoc child :relative-bounding-box
                                                          (assoc bb
                                                                 :x local-x
                                                                 :y local-y))))))
                                            (map ->ast)
                                            (map #(if auto?
                                                    (dissoc % :element/position)
                                                    %)))
                                      childs)})
           (ast-background m)
           (ast-position m))))

(defmethod ->ast "COMPONENT" [m]
  (merge (ast-frame m)
         {:element/type :element/component}))

(defmethod ->ast "FRAME" [m]
  (merge (ast-frame m)
         {:element/type :element/group}))

(defmethod ->ast "GROUP" [m]
  (merge (ast-frame m)
         {:element/type :element/group}))

(defmethod ->ast "INSTANCE" [m]
  (merge (ast-frame m)
         {:element/type :element/group}))

(defn ast-vector [m]
  (merge {:element/type :element/shape
          :element/id (:id m)}
         (when-let [name (:name m)]
           {:element/name name})
         (when (contains? m :visible)
           {:element/hidden (not (:visible m))})
         (when-let [transform (:relative-transform m)]
           {:element/transform transform})
         (when-let [{:keys [x y width height]} (:absolute-bounding-box m)]
           {:svg/origin [x y]
            :svg/bounds [width height]})
         (when-let [stroke-weight (:stroke-weight m)]
           {:element/stroke-weight stroke-weight})

         (when-let [geoms (:fill-geometry m)]
           {:element/fill-path (into []
                                     (comp (map :path)
                                           (map parse-svg-path))
                                     geoms)})
         (ast-background m)
         (when-let [geoms (:stroke-geometry m)]
           {:element/stroke-path (into []
                                       (comp (map :path)
                                             (map parse-svg-path))
                                       geoms)})))

(defmethod ->ast "LINE" [m]
  (ast-vector m))

(defmethod ->ast "VECTOR" [m]
  (ast-vector m))

(defmethod ->ast "ELLIPSE" [m]
  (ast-vector m))

(defmethod ->ast "BOOLEAN_OPERATION" [m]
 (ast-vector m))

(defmethod ->ast "RECTANGLE" [m]
  (merge {:element/id (:id m)
          :element/type :element/shape}
         (when-let [name (:name m)]
           {:element/name name})
         (when (contains? m :visible)
           {:element/hidden (not (:visible m))})
         (when-let [stroke-weight (:stroke-weight m)]
           {:element/stroke-weight stroke-weight})
         {:element/bounds ((juxt :width :height) (:absolute-bounding-box m))}
         (ast-background m)
         (ast-position m))
  )



(comment
  (-> (search/find-all components
                       #(and (map? %)
                             (contains? % "absoluteBoundingBox")))
      
      (->> (map z/node)
           (remove #(-> %
                        (get "absoluteBoundingBox")
                        (get "x")
                        some?)))
      
      )
  ,)



(defn figma-origin [m]
  (let [{{:keys [x y]} :absolute-bounding-box} m]
    (if (and x y)
      [x y]
      (ui/origin (:children m)))))

(defn figma-bounds [m]
  (let [{{:keys [width height]} :absolute-bounding-box} m]
    (if (and width height)
      [width height]
      (ui/bounds (:children m)))))

(defn props->record* [record-name props]
  (let [constructor (symbol (str "->" record-name))
        props (conj props
                    '[type]
                    '[name]
                    '[id]
                    '[visible true])]
    `(let [v# (defrecord ~(symbol record-name) ~(->> props
                                                     (map first)
                                                     (mapv csk/->kebab-case))

                ui/IOrigin
                (~'-origin [this#]
                 (figma-origin this#))

                ui/IBounds
                (~'-bounds [this#]
                 (figma-bounds this#))

                ui/IChildren
                (~'-children [this#]
                 (render this#)))]
       (defn ~(symbol (str "props->" record-name)) [~'m]
         (~constructor
          ~@(for [prop props]
              (let [pname (first prop)]
                `(parse-figma-prop
                  ~(name pname)
                  ~(if (>= (count prop) 2)
                     `(get ~'m ~(name pname) ~(second prop) )
                     `(get ~'m ~(name pname))))))))
       v#)))

(defmacro props->record [record-name props]
  (props->record* record-name (eval props)))

(defn normalize-bounds [doc]
  (let [minx (->> (search/find-all doc
                                   #(and (map? %)
                                         (contains? % "absoluteBoundingBox")))
                  (map z/node)
                  (map #(get % "absoluteBoundingBox"))
                  (map #(get % "x"))
                  (remove nil?)
                  (reduce min Double/POSITIVE_INFINITY))
        miny (->> (search/find-all doc
                                   #(and (map? %)
                                         (contains? % "absoluteBoundingBox")))
                  (map z/node)
                  (map #(get % "absoluteBoundingBox"))
                  (map #(get % "y"))
                  (remove nil?)
                  (reduce min Double/POSITIVE_INFINITY))]
    (search/zip-walk (search/clj-zip doc)
                     (fn [m]
                       (if (and (map? m)
                                (contains? m "absoluteBoundingBox"))
                         (let [{{:strs [x y]} "absoluteBoundingBox"} m]
                           (if (and x y)
                             (-> m
                                 (update-in ["absoluteBoundingBox" "x"] - minx)
                                 (update-in ["absoluteBoundingBox" "y"] - miny))
                             m))
                         m)))))


(defmulti figma->membrane (fn [obj]
                            (get obj "type")))

(defmulti parse-figma-prop (fn [prop val]
                             prop))

(defmethod parse-figma-prop "children" [_ childs]
  (mapv figma->membrane childs))


(defn figma-paint->membrane [m]
  (let [m (cske/transform-keys csk/->kebab-case-keyword  m)]
    (if-let [c (:color m)]
      (assoc m :color [(:r c)
                       (:g c)
                       (:b c)
                       (:a c)])
      m)))

(defmethod parse-figma-prop "fills" [_ fills]
  (mapv figma-paint->membrane fills))

(defmethod parse-figma-prop "strokes" [_ strokes]
  (mapv figma-paint->membrane strokes))

(defmethod parse-figma-prop "backgroundColor" [_ color]
  (figma-color->membrane color))

(defmethod parse-figma-prop "color" [_ color]
  (figma-color->membrane color))

(defmethod parse-figma-prop :default [_ x]
  (cske/transform-keys csk/->kebab-case-keyword  x))


(props->record Canvas '[[children]
                        [backgroundColor]])

#_(extend-protocol ui/IChildren
  Canvas
  (-children [this]
    (render this)))

(defmethod figma->membrane "CANVAS" [obj]
  (props->Canvas obj))



(def frame-props
  '[[children]
    [locked false]
    [fills []]
    [strokes []]
    [strokeWeight]
    [strokeAlign]
    [cornerRadius]
    [rectangleCornerRadii]
    [exportSettings []]
    [blendMode]
    [preserveRatio]
    [constraints]
    [layoutAlign]
    [transitionNodeID]
    [transitionDuration]
    [transitionEasing]
    [opacity 1]
    [absoluteBoundingBox]
    [size]
    [relativeTransform]
    [clipsContent]
    [layoutMode "NONE"]
    [primaryAxisSizingMode "AUTO"]
    [primaryAxisAlignItems "MIN"]
    [counterAxisAlignItems "MIN"]
    [paddingLeft 0]
    [paddingRight 0]
    [paddingTop 0]
    [paddingBottom 0]
    [horizontalPadding 0]
    [verticalPadding 0]
    [itemSpacing 0]
    [layoutGrids []]
    [overflowDirection "NONE"]
    [effects []]
    [isMask false]
    [isMaskOutline false]
    ])



(props->record Frame frame-props)

(defmethod figma->membrane "FRAME" [obj]
  (props->Frame obj))

#_(extend-type Frame
  ui/IChildren
  (-children [this]
    (:children this)))

(props->record Group frame-props)

(defmethod figma->membrane "GROUP" [obj]
  (props->Group obj))

#_(extend-type Group
  ui/IChildren
  (-children [this]
    (:children this)))

(def vector-props
  '[[locked false]
    [exportSettings []]
    [blendMode]
    [preserveRatio false]
    [layoutAlign]
    [layoutGrow 0]
    [constraints]
    [transitionNodeID]
    [transitionDuration]
    [transitionEasing]
    [opacity 1]
    [absoluteBoundingBox]
    [effects []]
    [size]
    [relativeTransform]
    [isMask false]
    [fills []]
    [fillGeometry]
    [strokes []]
    [strokeWeight]
    [strokeCap "NONE"]
    [strokeJoin "MITER"]
    [strokeDashes []]
    [strokeMiterAngle 28.96]
    [strokeGeometry]
    [strokeAlign]
    [styles]])

(props->record Vector vector-props)

(defmethod figma->membrane "VECTOR" [obj]
  (props->Vector obj))

(defn extend-vectorish* [atype]
  `(extend ~atype
     ui/IChildren
     {:-children (fn [this#]
                   (let [box# (:absolute-bounding-box this#)]
                     (into
                      [(ui/translate (:x box#) (:y box#)
                                     [(ui/label ~(str atype "!"))
                                      (ui/with-style :membrane.ui/style-stroke
                                        (ui/rectangle (:width box#)
                                                      (:height box#)))])
                       (:children this#)])))}))
(defmacro extend-vectorish [atype]
  (extend-vectorish* atype))

;; (extend-vectorish Vector)

(props->record BooleanOperation
               '[[children]
                 [booleanOperation]])
(defmethod figma->membrane "BOOLEAN_OPERATION" [obj]
  (props->BooleanOperation obj))

(props->record Star vector-props)
(defmethod figma->membrane "STAR" [obj]
  (props->Star obj))

;; (extend-vectorish Star)

(props->record Line vector-props)
(defmethod figma->membrane "LINE" [obj]
  (props->Line obj))

;; (extend-vectorish Line)


(props->record Ellipse vector-props)
(defmethod figma->membrane "ELLIPSE" [obj]
  (props->Ellipse obj))

;; (extend-vectorish Ellipse)

(props->record RegularPolygon vector-props)
(defmethod figma->membrane "REGULAR_POLYGON" [obj]
  (props->RegularPolygon obj))

;; (extend-vectorish RegularPolygon)

(props->record Rectangle (into vector-props
                               '[[cornerRadius]
                                 [rectangleCornerRadii]]))
(defmethod figma->membrane "RECTANGLE" [obj]
  (props->Rectangle obj))

;; (extend-vectorish Rectangle)

(props->record Text (into vector-props
                          '[[characters]
                            [style]
                            [characterStyleOverrides]
                            [styleOverrideTable]]))
(defmethod figma->membrane "TEXT" [obj]
  (props->Text obj))

#_(extend-type Text
  ui/IChildren
  (-children [this]
    (let [box (:absolute-bounding-box this)]
      [(ui/translate (:x box) (:y box)
                     [(ui/label (:characters this))
                      #_(ui/with-style :membrane.ui/style-stroke
                        (ui/rectangle (:width box)
                                      (:height box)))])])))

(props->record Slice '[[exportSettings]
                       [absoluteBoundingBox]
                       [size]
                       [relativeTransform]])
(defmethod figma->membrane "SLICE" [obj]
  (props->Slice obj))

(props->record Component frame-props)
(defmethod figma->membrane "COMPONENT" [obj]
  (props->Component obj))

;; (extend-vectorish Component)



(props->record ComponentSet frame-props)
(defmethod figma->membrane "COMPONENT_SET" [obj]
  (props->ComponentSet obj))

(props->record Instance
               (into frame-props
                     '[[componentId]]))
(defmethod figma->membrane "INSTANCE" [obj]
  (props->Instance obj))

(defn clj-zip [obj]
  (z/zipper #(and (seqable? %)
                  (not (string? %)))
            seq
            (fn [node children]
              (if (map-entry? node)
                (vec children)
                (into (empty node) children)))
            obj))

(defn zip-walk
  "Depth first walk of zip. edit each loc with f"
  [zip f]
  (loop [zip zip]
    (if (z/end? zip)
      (z/root zip)
      (recur (-> (z/edit zip f)
                 z/next)))))

(defn tree-zip [obj]
  (z/zipper list?
            identity
            (fn [node children]
              (into '() (remove #{::remove} children)))
            obj))

(defn remove-tree [pred tree]
  (zip-walk (tree-zip tree)
            (fn [obj]
              (if (pred obj)
                ::remove
                obj))))

(defn name->variant [name]
  (loop [name name
         m {}]
    (let [match (re-find #"([^=]+)=([^,]+),? *" name)]
      (if match
        (let [[s k v] match]
          (recur (subs name (count s))
                 (conj m [k v])))
        m))))


(defn name->style [name]
  (let [match (re-find #"\((.*)\)$" name)]
    (if match
      (let [[s style] match]
        style)
      "default")))

(defn preserve-meta [f]
  (fn [o]
    (with-meta (f o) (meta o))))

(defn ->view [elems]
  (ui/no-events
   (apply
    ui/vertical-layout
    (into []
          (comp
           (map (fn [m]
                  (with-meta m
                    {:id (:id m)})))
           (map (preserve-meta ->ast))
           (map (preserve-meta #(dissoc % :element/position)))
           (map (preserve-meta s2/compile))
           (map (preserve-meta eval))
           (map (fn [view]
                  (ui/try-draw 
                   view
                   (fn [draw e]
                     (draw (ui/label (str (:id (meta view))"!!!"))))))))
          elems))))

(comment

  (require '[com.phronemophobic.membrane.schematic2 :as s2])

  (def buttons
    (-> view
        (search/keep-all
         (fn [m]
           (when-let [name (:name m)]
             (let [variant (name->variant name)]
               (when (and (= "False" (get variant "Loading"))
                          (= "None" (get variant "Icon")))
                 m)))))))
  
  (def buttons2
    (->> (search/find-all
          view
          (fn [m]
            (= "COMPONENT_SET"
               (:type m))))
         (map z/node)
         (mapcat (fn [cset]
                   (let [style (name->style (:name cset))
                         variants (search/keep-all
                                   cset
                                   (fn [m]
                                     (when-let [name (:name m)]
                                       (let [variant (name->variant name)]
                                         (when (and (= "False" (get variant "Loading"))
                                                    (= "None" (get variant "Icon")))
                                           m)))))]
                     (->>
                      variants
                      (map (fn [m]
                             (letfn [(->keyword [s]
                                       (-> s
                                           csk/->kebab-case
                                           keyword))]
                               (let [params (-> (name->variant (:name m))
                                              (dissoc "Loading" "Icon")
                                              (->> (reduce-kv
                                                    (fn [m k v]
                                                      (assoc m
                                                             (->keyword k) (->keyword v)))
                                                    {}))
                                              (assoc :style (->keyword style)))]
                                 (assoc m :figma/parameters params)))))))))))

  (def schematic-button-component
    {:component/defaults {:state :default
                          :spacing :default
                          :style :default
                          :text "Go"}
     :element/type :element/component
     :element/name "figma-button"
     :element/children
     [{:element/type :flow-control/case
       :flow-control.case/expression '{:state state
                                       :spacing spacing
                                       :style style}
       :flow-control.case/clauses
       (into {}
             (comp (map (fn [m]
                          (assoc m :type "GROUP")))
                   (map #(dissoc % :name))
                   
                   (map (juxt :figma/parameters ->ast))
                   (map (fn [[k m]]
                          [k
                           (-> (search/find m :element/text)
                                
                               (z/edit assoc :element/text 'text)
                               z/root)]))
                   (map (fn [[k m]]
                          [k (dissoc m :element/position)])))
             buttons2)}]})

  (-> schematic-button-component
      com.phronemophobic.membrane.schematic2/export-component
      )
  (def button-table (into []
                          (comp
                           (map (fn [m]
                                  (with-meta m
                                    {:id (:id m)
                                     :figma m})))
                           (map (preserve-meta ->ast))
                           (map (preserve-meta #(dissoc % :element/position)))
                           (map (preserve-meta s2/compile))
                           (map (preserve-meta eval))
                           (map (fn [view]
                                  (let [[x y :as pos] (-> view meta :figma ->ast :element/position)]
                                    (ui/translate 
                                     x y
                                     (ui/on
                                      :mouse-down
                                      (fn [_]
                                        (tap> (-> view meta :figma))
                                        nil)
                                      (ui/try-draw 
                                       view
                                       (fn [draw e]
                                         (prn e)
                                         (draw (ui/label (str (:id (meta view))"!!!")))))))))))
                          buttons))
    (backend/run (membrane.component/make-app #'figma-viewer {:view button-table}))

    (def test-ast (->ast (->> buttons
                              (sort-by (fn [{{:keys [x y]} :absolute-bounding-box}]
                                         (+ x y)))
                              first)))





  ,)



(declare document)
(defn show-view [n]
  (def view (-> document
                (get "children")
                (nth n)
                normalize-bounds
                figma->membrane))
  (prn n (:name view))

  (backend/run (membrane.component/make-app #'figma-viewer {:view (->view
                                                                   (-> view
                                                                       (search/keep-all
                                                                        (fn [m]
                                                                          (when (= "COMPONENT" (:type m))
                                                                            m)))))})))


(comment
  (defui figma-button-with-hover [{:keys [spacing style text hover?]}]
    (basic/on-hover {:hover? hover?
                     :body
                     (figma-button {:state (if hover?
                                             :hover
                                             :default)
                                    :text text
                                    :spacing spacing
                                    :style style})}))

  (defui button-app [{:keys []}]
    (ui/table-layout
     (for [spacing [:default :compact]]
       (for [style [:default :primary :danger :subtle :link :subtle-link]]
         (figma-button-with-hover {:spacing spacing
                                   :style style
                                   :text (pr-str [spacing style])})))
     8 8))

  (skia/run
    (membrane.component/make-app #'button-app)))
