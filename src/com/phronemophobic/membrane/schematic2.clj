(ns com.phronemophobic.membrane.schematic2
  (:refer-clojure :exclude [compile])
  (:require [membrane.ui :as ui
             :refer [on
                     vertical-layout
                     horizontal-layout]]
            [membrane.skia :as backend]
            [liq.buffer :as buffer]
            [membrane.components.code-editor.code-editor
             :as code-editor]
            [membrane.component
             :refer [defeffect defui]]
            [membrane.basic-components :as basic]
            [datascript.core :as d]
            clojure.edn
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]

            [clojure.zip :as z]
            [zippo.core :as zippo]
            [com.phronemophobic.membrane.svg :as svg]
            [com.rpl.specter :as specter]

            ;; [autonormal.core :as auto]
            [com.phronemophobic.membrane.scrollview :as sv])
  (:gen-class))

(defn clj-zip [obj]
  (z/zipper #(and (seqable? %)
                  (not (string? %)))
            seq
            (fn [node children]
              (let [children (remove #{::delete} children)]
                (if (map-entry? node)
                  (vec children)
                  (into (empty node) children))))
            obj))

(defn search-keep-all [obj pred]
  (->> (zippo/loc-seq (clj-zip obj))
       (map z/node)
       (keep #(when-let [ret (pred %)]
                ret))))



(defn element-zip [root]
  (z/zipper #(seq (:element/children %))
            :element/children
            #(assoc %1 :element/children (vec %2))
            root))

(s/def :element/id pos-int?)

(s/def :geom/point (s/tuple number? number?))
;; (s/def :geom/position :geom/point)
(s/def :geom/path (s/and (s/coll-of :geom/position
                                    :into [])
                         #(>= (count %) 2)))
(s/def :geom/relative-to :element/id)
(s/def :geom/offset :geom/point)
(s/def :geom/relative-position (s/keys
                                :req [:geom/relative-to
                                      :geom/offset]))

(s/def :style/color-value (s/double-in :min 0 :max 1
                                       :infinite? false
                                       :NaN? false))
(s/def :style/color3d (s/tuple :style/color-value
                               :style/color-value
                               :style/color-value))
(s/def :style/color4d (s/tuple :style/color-value
                               :style/color-value
                               :style/color-value
                               :style/color-value))
(s/def :style/color (s/or :color3d :style/color3d
                          :color4d :style/color4d))

(s/def :element/type keyword?)
;; (s/def :element/position (s/or :position :geom/position
;;                                :relative :geom/relative-position))
(s/def :element/x number?)
(s/def :element/y number?)


(s/def :element/text string?)

(s/def :event/type keyword?)
(s/def :event/intent (s/and vector?
                            (s/cat :type :event/type
                                   :args (s/* any?))))
(s/def :event/intents (s/coll-of :event/intent))

(s/def :element/events (s/map-of :event/type
                                 :event/intents))

(s/def :element/path :geom/path)
(s/def :element/stroke #{:membrane.ui/style-fill
                         :membrane.ui/style-stroke
                         :membrane.ui/style-stroke-and-fill})
(s/def :element/stroke-color :style/color)
(s/def :element/fill-color :style/color)
(s/def :element/stroke-weight double?)
(s/def :element/hidden boolean?)

;; really not sure about these
(s/def :element/image string?)


(s/def :element/children (s/coll-of :element/element
                                    :into []))
(s/def :element/layout #{:vertical
                         :horizontal
                         :none})

;; from origin?
;; what about negative space?
;; this is almost certainly wrong
;; (s/def :element/bounds :geom/point)
(s/def :element/width number?)
(s/def :element/height number?)

(s/def :instance/fn any?)
(s/def :component/args-spec any?)
(s/def :component/args any?)
(s/def :component/defaults any?)


(s/def :component/binding (s/cat
                           :name symbol?
                           :value any?))
(s/def :component/bindings (s/and
                            vector?
                            (s/* :component/binding)))
(s/def :element/for-bindings (s/and
                              vector?
                              (s/* :component/binding)))

(s/def :instance/args (s/map-of any? any?))

(s/def :element/type #{:element/label
                       :element/shape
                       :element/component
                       :element/instance
                       :element/group})
(defmulti element-type :element/type)
(defmethod element-type :element/component [_]
  #_(s/keys :req [:event/type :event/timestamp :search/url]))

(defmethod element-type :element/group [_]
  (s/keys :req [:element/id
                :element/type]
          :opt [;;:element/position
                :element/x
                :element/y
                :element/width
                :element/height
                :element/children
                :element/hidden
                :element/layout
                ]))
(defmethod element-type :element/label [_]
  (s/keys :req [:element/id
                :element/type
                :element/text]
          :opt [:element/stroke-color
                :element/stroke
                ;; :element/position
                :element/x
                :element/y
                :element/fill-color
                :element/stroke-weight
                :element/hidden]))

(defmethod element-type :element/shape [_]
  (s/keys :req [:element/id
                :element/type]
          :opt [:element/stroke-color
                :element/stroke
                :element/path
                ;; :element/position
                :element/fill-color
                :element/stroke-weight
                :element/hidden]))

(defmethod element-type :element/component [_]
  (s/keys :req [:element/id
                :element/type]
          :opt [;;:component/args
                :component/args-spec
                :component/bindings
                :component/defaults
                ;; :element/position
                :element/x
                :element/y
                :element/width
                :element/height
                :element/children
                :element/hidden
                :element/layout
                ]))
(defmethod element-type :element/instance [_]
  (s/keys :req [:element/id
                :instance/fn
                :element/type]
          :opt [;;:component/args
                ;; :component/bindings
                :instance/args
                ;; :element/position
                :element/x
                :element/y
                :element/width
                :element/height
                :element/hidden
                :element/layout
                ]))

(s/def :element/element (s/multi-spec element-type :element/type))


;; todo example
(s/def :todo/description string?)
(s/def :todo/complete? boolean?)
(s/def ::todo (s/keys :req-un [:todo/description :todo/complete?]))
(s/def :ui/todo-item (s/keys :req-un [::todo]))

(s/def ::todos (s/coll-of ::todo :into []))
(s/def :ui/todo-list (s/keys :req-un [::todos]))

(s/def ::next-todo string?)
(s/def :ui/todo-app (s/keys :req-un [::todos ::next-todo]))





(def test-elem
  #:element{
            :children
            [#:element{:x 3.0
                       :y 1
                       :id 193,
                       :type :element/label
                       :text "asfd"}
             #:element{:x 3.0
                       :y 1
                       :id 192,
                       :path [[0 0]
                              [20 20]]
                       :type :element/shape
                       }
             ],
            ;; :layout :none
            :x 50
            :y 50
            :id 106,
            :type :element/group})

(def test-for
  #:element{
            :children
            [#:element{:x 3
                       :y 1
                       :id 193,
                       :type :element/label
                       :text '(str "counter: " x)}
             #:element{:x 3
                       :y 1
                       :id 193,
                       :path [[0 0]
                              [20 20]]
                       :type :element/shape
                       }
             ],
            :element/for-bindings '[x (range 5)]
            ;; :layout :none
            :x 50
            :y 50
            :id 106,
            :type :element/group})

(def test-flex
  #:element{
            :children
            [#:element{:x 3
                       :y 1
                       :id 193,
                       :fills [{}]
                       :type :element/label
                       :text '(str "counter: " y)}
             #:element{:x 3
                       :y 1
                       :id 193,
                       :strokes [{}]
                       :path '[[0 0]
                               [20 (* y 20)]]
                       :type :element/shape
                       }
             ],
            :element/for-bindings '[y (range 5)]
            :layout {;;:flex/align :flex.align/center
                     ;; :flex/justify-content :flex.justify-content/space-evenly
                     ;; :flex/column-gap 10
                     :flex/direction :flex.direction/column}
            :x 50
            :y 50
            :height 500
            :id 106,
            :type :element/group})


(comment
  (backend/run (constantly
                (eval (compile test-flex))))
  ,)


(def test-counter
  #:element{
            :children
            [#:element{:x 3
                       :y 1
                       :id 193,
                       :type :element/label
                       :text 'num}],
            ;; :layout :none
            :x 50
            :y 50
            ;; :component/args {:num 42}
            :component/defaults {:num 42}
            :component/args-spec nat-int?
            :component/bindings '[num (:num $)]

            :id 106,
            :type :element/component}
  )



(def test-textarea

  #:element{
            ;; :layout :none
            :x 50
            :y 50
            ;; :component/args {:num 42}
            :instance/args {:text "hello"}
            :instance/fn `basic/textarea

            :id 200,
            :type :element/instance}
  )
(def test-instance
  #:element{
            :children
            [(assoc test-textarea
                    :instance/args {:text 'num})],
            ;; :layout :none
            :x 50
            :y 50
            ;; :component/args {:num 42}
            :component/defaults {:num 42}
            :component/args-spec nat-int?
            :component/bindings '[num (:num $)]

            :id 201
            :type :element/component}
  )

(def test-instance2
  #:element{
            :instance/component #:element{:x 3
                                          :y 1
                                          :id 193,
                                          :path [[0 0]
                                                 [20 20]]
                                          :type :element/shape
                                          }
            :x 50
            :y 50
            :id 106,
            :type :element/instance}
  )



(defprotocol IElementStore
  (root-eid [es])
  (next-eid [es])
  (append-child [es eid child])
  (edit-elem [es eid f])
  (delete-elem [es eid])
  (->tree [es eid])
  (get-elem-shallow [es eid]))


(def schema
  {:element/element {:entitydb/id :element/id
                     :entitydb/relations
                     {:element/children
                      {:entitydb.relation/path [:element/children :*]
                       :entitydb.relation/type :element/element}

                      :instance/component :element/element}}})

;; (defn root-eid [es]
;;   ::root)
;; (defn next-eid [es]
;;   (:next-eid es))

(defn normalize
  "Given an entity, return a vector of normalized entities."
  [next-eid entity]
  (loop [queue [entity]
         next-eid next-eid
         normalized []]
    (if queue
      (let [entity (first queue)
            component (:instance/component entity)
            [next-eid component] (if component
                                   (if-let [cid (:element/id component)]
                                     [next-eid component]
                                     [(inc next-eid) (assoc component :element/id next-eid)])
                                   [next-eid nil])

            childs (:element/children entity)
            [next-eid childs] (reduce (fn [[next-eid childs] child]
                                        (if-let [cid (:element/id child)]
                                          [next-eid (conj childs child)]
                                          [(inc next-eid) (conj childs
                                                                (assoc child :element/id next-eid))]))
                                      [next-eid []]
                                      childs)

            norm (merge entity
                        (when component
                          (let []
                            {:instance/component
                             {:element/id (:element/id component)}}))
                        (when (seq childs)
                          {:element/children
                           (mapv (fn [child]
                                   {:element/id (:element/id child)})
                                 childs)}))

            next-queue (next queue)
            next-queue (if component
                         (conj next-queue component)
                         next-queue)
            next-queue (into next-queue childs)]
        (recur next-queue
               next-eid
               (conj normalized norm)))
      
      ;; else
      [next-eid normalized])))

(defn add-entity [es entity]
  (let [next-eid (:next-eid es)
        [next-eid entities] (normalize next-eid entity)]
    (assoc es
           :next-eid next-eid
           :db (reduce (fn [db entity]
                         (update db (:element/id entity)
                                 merge entity))
                       (:db es)
                       entities))))

(defn append-child* [es eid child]
  (let [[es child] (if (:element/id child)
                     [es child]
                     [(update es :next-eid inc)
                      (assoc child :element/id (:next-eid es))])

        child (if (:element/name child)
                child
                (let [ename (if-let [fsym (:instance/fn child)]
                              (str (name fsym)
                                   "-"
                                   (:element/id child))
                              (if-let [comp (:instance/component child)]
                                (let [component-entity (->tree es (:element/id comp))]
                                  (str (:element/name component-entity)
                                       "-"
                                       (:element/id child)))
                                (str (name (:element/type child))
                                     "-"
                                     (:element/id child))))]
                 (assoc child
                        :element/name ename)))

        {:keys [next-eid db]} es
        es (update-in es [:db eid :element/children]
                      (fn [childs]
                        (conj childs {:element/id (:element/id child)})))]
    (add-entity es child)))

(defn edit-elem* [es eid f]
  (update-in es
             [:db eid]
             f))
(defn delete-elem* [es eid]
  (->> es
         (specter/setval
          [(specter/keypath :db)
           (specter/keypath eid)]
          specter/NONE)

         (specter/setval
          [(specter/keypath :db)
           specter/MAP-VALS
           (specter/multi-path
            (specter/must :instance/component)
            [(specter/must :element/children)
             specter/ALL])
           (fn [m]
             (= eid (:element/id m)))]
          specter/NONE)))

(defn ->tree* [es eid]
  (let [entity (get-in es [:db eid])
        entity (if-let [childs (:element/children entity)]
                 (assoc entity
                        :element/children (mapv #(->tree es (:element/id %)) childs))
                 entity)
        entity (if-let [component (:instance/component entity)]
                 (assoc entity
                        :instance/component (->tree es (:element/id component)))
                 entity)]
    entity))

(defrecord ElementStore [db next-eid]
  IElementStore
  (next-eid [_]
    next-eid)
  (root-eid [_]
    ::root)

  (append-child [es eid child]
    (append-child* es eid child))

  (edit-elem [es eid f]
    ;; This doesn't properly handle relations
    ;; It's not even clear what properly handling relations would mean
    (edit-elem* es eid f))
  (delete-elem [es eid]
    (delete-elem* es eid))
  
  (->tree [es eid]
    (->tree* es eid))

  (get-elem-shallow [es eid]
    (get-in es [:db eid])))

(defn element-store []
  (-> (->ElementStore {} 1)
      (add-entity #:element {:id ::root
                             :children []})))



(comment
  (edbq/get-entity db2 :element/element 106
                   [(edbq/recur-on :element/children)
                    (edbq/recur-on :element/children)])

  (edbq/get-entity db2 :element/element 106
                   [(edbq/recur-on :element/children)
                    (edbq/recur-on :instance/component)
                    ]
                   ))



(defn query-db [db eid]
  (let [m (get-in db eid)
        m (if-let [xs (:element/children m)]
            (assoc m :element/children (mapv #(query-db db %) xs))
            m)
        m (if-let [cid (:instance/component m)]
            (assoc m :instance/component (query-db db cid))
            m)]
    m))

(def ->tree-memo (memoize ->tree))

(declare compile)
(defn render
  ([root]
   (try
     (ui/try-draw
      (-> root compile eval)
      (fn [draw e]
        (draw (ui/label "Exception!"))))
     (catch Exception e
       (ui/label "Compile EXception!")))))
#_(defn render-elem [elem]
  (first (render {0 elem} [0])))

(def render-memo (memoize render))

(defn render-store [store]
  (render-memo (->tree-memo store ::root)))


#_(defn append-child [es pid child]
    (append-child pid child)
    #_(update parent :element/children
              (fn [xs]
                (conj (or xs []) child))))


(defeffect ::add-element [$store elem]

  (dispatch! :update $store
             (fn [store]
               (append-child store
                             (root-eid store)
                             elem)))
  #_(dispatch! :update $root append-child elem))

(defui line-tool [{:keys [store scroll-offset scroll-bounds]}]
  (let [mpos (get extra :mpos)
        temp-pos (get extra :temp-pos)]
    (ui/scissor-view
     [0 0]
     scroll-bounds
     (ui/translate
      (nth scroll-offset 0) (nth scroll-offset 1)
      (ui/on
       :mouse-down
       (fn [[mx my :as pos]]
         [[:set $mpos pos]
          [:delete $temp-pos nil]])
       :mouse-move
       (fn [pos]
         (when mpos
           [[:set $temp-pos pos]]))
       :mouse-up
       (fn [pos]
         (when mpos
           [[::add-element $store
             (let [[x1 y1] (mapv int pos)
                   [x2 y2] (mapv int mpos)
                   x (min x1 x2)
                   y (min y1 y2)
                   start [(- x1 x) (- y1 y)]
                   end [(- x2 x) (- y2 y)]]
               #:element{:type :element/shape
                         :x x
                         :y y
                         :strokes [{}]
                         :path [start end]})]
            [:delete $temp-pos]
            [:delete $mpos]]))
       [(ui/with-style :membrane.ui/style-stroke
          (ui/rectangle (nth scroll-bounds 0)
                        (nth scroll-bounds 1)))
        (ui/no-events
         (render-store store))
        (when (and mpos temp-pos)
          (ui/with-style :membrane.ui/style-stroke
            (ui/path mpos temp-pos)))]
       )
      ))
    ))

(defui rect-tool [{:keys [store scroll-offset scroll-bounds]}]
  (let [mpos (get extra :mpos)
        temp-pos (get extra :temp-pos)]
    (ui/scissor-view
     [0 0]
     scroll-bounds
     (ui/translate
      (nth scroll-offset 0) (nth scroll-offset 1)
      (ui/on
       :mouse-down
       (fn [[mx my :as pos]]
         [[:set $mpos pos]
          [:delete $temp-pos nil]])
       :mouse-move
       (fn [pos]
         (when mpos
           [[:set $temp-pos pos]]))
       :mouse-up
       (fn [pos]
         (when mpos
           [[::add-element $store
             (let [[x1 y1] (mapv int pos)
                   [x2 y2] (mapv int mpos)
                   x (min x1 x2)
                   y (min y1 y2)
                   width (Math/abs (- x2 x1))
                   height (Math/abs (- y2 y1))]
               #:element{:type :element/shape
                         :x x
                         :y y
                         :strokes [{}]
                         :path [[0 0] [0 height]
                                [width height] [width 0]
                                [0 0]]})]
            [:delete $temp-pos]
            [:delete $mpos]]))
       [(ui/with-style :membrane.ui/style-stroke
          (ui/rectangle (nth scroll-bounds 0)
                        (nth scroll-bounds 1)))
        (ui/no-events
         (render-store store))
        (when (and mpos temp-pos)
          (ui/with-style :membrane.ui/style-stroke
            (let [[x1 y1] mpos
                  [x2 y2] temp-pos]
             (ui/translate (min x1 x2) (min y1 y2)
                           (ui/rectangle (Math/abs (- x2 x1))
                                         (Math/abs (- y2 y1)))))))]
       )
      ))
    ))

(defui pan-tool [{:keys [store scroll-offset scroll-bounds]}]
  (sv/scrollview {:offset scroll-offset
                  :scroll-bounds scroll-bounds
                  :body
                  (ui/no-events
                   [(ui/with-style :membrane.ui/style-stroke
                      (ui/rectangle (nth scroll-bounds 0)
                                    (nth scroll-bounds 1)))
                    (render-store store)])}))




(defeffect ::mouse-down-text-tool [$store $edit-text-eid $edit-text $focus [x y :as pos]]
  (let [store (dispatch! :get $store)
        root (->tree-memo store ::root)
        [dist eid]
        (reduce
         (fn [[dist id :as old] {eid :element/id :as elem}]
           (if-not (= :element/label (:element/type elem))
             old
             (let [;;[ex ey] (get elem :element/position [0 0])
                   ex (get elem :element/x 0)
                   ey (get elem :element/y 0)

                   xdist (- ex x)
                   ydist (- ey y)
                   edist (+ (* xdist xdist)
                            (* ydist ydist))]
               (if (< edist dist)
                 [edist eid]
                 old))))
         [Double/MAX_VALUE nil]
         (:element/children root))]
    (if (< dist (* 15 15))
      (let [elem (get-elem-shallow store eid)]
        (dispatch! :set $edit-text-eid eid)
        (dispatch! :set $edit-text (:element/text elem))
        (dispatch! :set $focus $edit-text))
      (if (dispatch! :get $edit-text-eid)
        (let [edit-text-eid (dispatch! :get $edit-text-eid)
              edit-text (dispatch! :get $edit-text)]
          (dispatch! :set $edit-text-eid nil)
          (dispatch! :update $store
                     (fn [store]
                       (edit-elem store edit-text-eid
                                  #(assoc % :element/text edit-text)))))
        (do
          (dispatch! :set $edit-text-eid nil)
          (dispatch! ::add-element $store
                     #:element{:x x
                               :y y
                               :type :element/label,
                               :fills [{}]
                               :text "asfd"}))))))

(defui text-tool [{:keys [store scroll-bounds scroll-offset]}]
  (let [focus (get context :focus)
        edit-text-eid (get extra ::edit-text-eid)
        temp-text (get extra [::edit-text-eid :text])]
    (ui/scissor-view
     [0 0]
     [800 800]
     (ui/translate
      (nth scroll-offset 0) (nth scroll-offset 1)
      (ui/on
       :mouse-down
       (fn [[mx my :as pos]]
         [[::mouse-down-text-tool $store $edit-text-eid $temp-text $focus pos]])
       [(ui/with-style :membrane.ui/style-stroke
          (ui/rectangle 800 800))
        (ui/no-events
         (render-store
          (if edit-text-eid
            (delete-elem store edit-text-eid)
            store)))
        (when edit-text-eid
          (let [text-elem (get-elem-shallow store edit-text-eid)
                x (:element/x text-elem)
                y (:element/y text-elem)
                ;;[x y] (:element/position text-elem)
                ]
            (ui/translate x y
                          (basic/textarea {:text temp-text}))))])))))


(defui instance-tool [{:keys [store elem scroll-bounds scroll-offset]}]
  (ui/scissor-view
   [0 0]
   scroll-bounds
   (ui/translate
    (nth scroll-offset 0) (nth scroll-offset 1)
    (ui/on
     :mouse-down
     (fn [[mx my :as pos]]
       [[::add-element $store
         ;;(assoc elem :element/position (mapv int pos))
         (assoc elem
                :element/x (int mx)
                :element/y (int my))]])

     [(ui/with-style :membrane.ui/style-stroke
        (ui/rectangle (nth scroll-bounds 0)
                      (nth scroll-bounds 1)))
      (ui/no-events
       (render-store store))]))))

(defeffect ::select-one [$selection store [x y :as pos]]
  (let [root (->tree-memo store ::root)
        [dist eid]
        (reduce
         (fn [[dist id :as old] {eid :element/id :as elem}]
           (let [;;[ex ey] (get elem :element/position [0 0])
                 ex (get elem :element/x 0)
                 ey (get elem :element/y 0)

                 xdist (- ex x)
                 ydist (- ey y)
                 edist (+ (* xdist xdist)
                          (* ydist ydist))]
             (if (< edist dist)
               [edist eid]
               old)))
         [Double/MAX_VALUE nil]
         (:element/children root))]
    (if (< dist (* 15 15))
      (dispatch! :set $selection [eid])
      (dispatch! :set $selection nil))))


(defeffect ::select-many [$selection store [x y :as pos]]
  (let [root (->tree-memo store ::root)
        [dist eid]
        (reduce
         (fn [[dist id :as old] {eid :element/id :as elem}]
           (let [;; [ex ey] (get elem :element/position [0 0])
                 ex (get elem :element/x 0)
                 ey (get elem :element/y 0)

                 xdist (- ex x)
                 ydist (- ey y)
                 edist (+ (* xdist xdist)
                          (* ydist ydist))]
             (if (< edist dist)
               [edist eid]
               old)))
         [Double/MAX_VALUE nil]
         (:element/children root))]
    (when (< dist (* 15 15))
      (dispatch! :update $selection
                 (fn [selection]
                   (if (some #{eid} selection)
                     selection
                     (conj selection eid)))))))

(def +zero (fnil + 0))

(defeffect ::move-selection [$store selection [ox oy]]
  (let [selected? (set selection)]
    (dispatch! :update
               $store
               (fn [store]
                 (reduce
                  (fn [es eid]
                    (edit-elem es eid
                               (fn [elem]
                                 (-> elem
                                     (update :element/x #(int (+zero % ox)))
                                     (update :element/y #(int (+zero % oy)))))))
                  store
                  selection)))
    #_(dispatch! :update
                 $root
                 (fn [root]
                   (specter/transform [:element/children
                                       specter/ALL
                                       (fn [x] (selected? (:element/id x)))
                                       :element/position]
                                      (fn [[x y]]
                                        [(+zero x (nth offset 0))
                                         (+zero y (nth offset 1))])
                                      root))))
  )





(defui move-tool [{:keys [store selection scroll-bounds scroll-offset]}]
  (let [temp-pos (get extra :temp-pos)
        mpos (get extra :mpos)

        root (->tree-memo store ::root)
        temp-elements (if (and temp-pos mpos selection)
                        (let [selected? (set selection)]
                          (specter/transform [:element/children
                                              specter/ALL
                                              (fn [x] (selected? (:element/id x)))
                                              ;;:element/position
                                              ]
                                             (fn [elem]
                                               (-> elem
                                                   (update :element/x #(int (+zero % (- (nth temp-pos 0)
                                                                                        (nth mpos 0)))))
                                                   (update :element/y #(int (+zero % (- (nth temp-pos 1)
                                                                                        (nth mpos 1)))))))
                                             root))
                        root)
        body (render-memo temp-elements)]
    (ui/scissor-view
     [0 0]
     scroll-bounds
     (ui/translate
      (nth scroll-offset 0) (nth scroll-offset 1)
      (ui/on
       :key-press
       (fn [c]
         (when-let [offset (case c
                             :left [-1 0]
                             :right [1 0]
                             :up [0 -1]
                             :down [0 1]
                             ;; else
                             nil)]
           [[::move-selection $store selection offset]]))
       :mouse-down
       (fn [pos]
         [[::select-one $selection store pos]
          [:set $mpos pos]])
       :mouse-move
       (fn [pos]
         [[:set $temp-pos pos]])
       :mouse-up
       (fn [pos]
         (into [[:delete $temp-pos]
                [:delete $mpos]]
               (when (and temp-pos mpos selection)
                 [
                  [::move-selection $store selection
                   [(- (nth temp-pos 0)
                       (nth mpos 0))
                    (- (nth temp-pos 1)
                       (nth mpos 1))]]]))
         )
       [(ui/with-style :membrane.ui/style-stroke
          (ui/rectangle (nth scroll-bounds 0)
                        (nth scroll-bounds 1)))
        (ui/no-events body)]
       )
      ))
    ))


(defeffect ::delete-one [$store [x y :as pos]]
  (let [store (dispatch! :get $store)
        root (->tree-memo store ::root)

        [dist eid]
        (reduce
         (fn [[dist id :as old] {eid :element/id :as elem}]
           (let [ex (get elem :element/x 0)
                 ey (get elem :element/y 0)

                 xdist (- ex x)
                 ydist (- ey y)
                 edist (+ (* xdist xdist)
                          (* ydist ydist))]
             (if (< edist dist)
               [edist eid]
               old)))
         [Double/MAX_VALUE nil]
         (:element/children root))]
    (when (< dist (* 15 15))
      (dispatch! :update $store
                 (fn [store]
                   (delete-elem store eid)))
      #_(dispatch! :update $root
                   (fn [root]
                     (specter/setval [:element/children
                                      specter/ALL
                                      (fn [x] (= eid (:element/id x)))]
                                     specter/NONE
                                     root))))))


(defeffect ::delete-selection [$store selection]
  (dispatch! :update $store
             (fn [store]
               (reduce (fn [store eid]
                         (delete-elem store eid))
                       store
                       selection))))

(defui delete-tool [{:keys [store selection scroll-bounds scroll-offset]}]
  (ui/scissor-view
   [0 0]
   scroll-bounds
   (ui/translate
    (nth scroll-offset 0) (nth scroll-offset 1)
    (ui/on
     :mouse-down
     (fn [pos]
       [[::delete-one $store pos]])

     [(ui/with-style :membrane.ui/style-stroke
        (ui/rectangle (nth scroll-bounds 0)
                      (nth scroll-bounds 1)))
      (ui/no-events
       (render-store store))]
     )
    ))
  )



(defeffect ::make-component [$store selection]
  (let [store (dispatch! :get $store)
        root (->tree-memo store ::root)
        selected? (set selection)

        children (->> root
                      :element/children
                      (filter #(selected? (:element/id %))))

        cx (transduce (comp (map :element/x)
                            (map #(or % 0)))
                      min
                      Double/POSITIVE_INFINITY
                      children)
        cy (transduce (comp (map :element/y)
                            (map #(or % 0)))
                      min
                      Double/POSITIVE_INFINITY
                      children)

        children (->> children
                      (mapv (fn [elem]
                              (-> elem
                                  (update :element/x #(int (- (or % 0) cx)))
                                  (update :element/y #(int (- (or % 1) cy)))))))]
    (dispatch! :update $store
               (fn [store]
                 (let [store (reduce #(delete-elem %1 %2)
                                     store
                                     (map :element/id children))
                       store (append-child store ::root
                                           #:element{
                                                     :children children
                                                     :x cx
                                                     :y cy
                                                     ;; :position [cx cy],
                                                     ;; :component/args {:num 42}
                                                     :component/defaults {}
                                                     ;; :component/args-spec nat-int?
                                                     :component/bindings '[]
                                                     :type :element/component})]
                   store)))))

(defeffect ::make-vertical [$store selection]
  (let [store (dispatch! :get $store)
        root (->tree-memo store ::root)
        selected? (set selection)

        children (->> root
                      :element/children
                      (filter #(selected? (:element/id %))))

        cx (transduce (comp (map :element/x)
                            (map #(or % 0)))
                      min
                      Double/POSITIVE_INFINITY
                      children)
        cy (transduce (comp (map :element/y)
                            (map #(or % 0)))
                      min
                      Double/POSITIVE_INFINITY
                      children
                      )

        children (->> children
                      (mapv (fn [elem]
                              (-> elem
                                  (update :element/x #(int (- (or % 0) cx)))
                                  (update :element/y #(int (- (or % 1) cy)))))))
        ]
    (dispatch! :update $store
               (fn [store]
                 (let [
                       
                       store (reduce #(delete-elem %1 %2)
                                     store
                                     (map :element/id children))
                       store (append-child store ::root
                                           #:element{:children children
                                                     :x cx
                                                     :y cy
                                                     :layout :vertical
                                                     :for-bindings '[x (range 3)]
                                                     :type :element/group})]
                   store)))))



(defui select-tool [{:keys [store selection scroll-bounds scroll-offset shift-down?]}]
  [
   (ui/scissor-view
    [0 0]
    scroll-bounds
    (ui/translate
     (nth scroll-offset 0) (nth scroll-offset 1)
     (ui/on
      :mouse-down
      (fn [pos]
        (if shift-down?
          [[::select-many $selection store pos]]
          [[::select-one $selection store pos]]))

      [(ui/with-style :membrane.ui/style-stroke
         (ui/rectangle (nth scroll-bounds 0)
                       (nth scroll-bounds 1)))
       (ui/no-events
        (render-store store))]
      )
     ))
   (when (seq selection)
     (horizontal-layout
      (basic/button {:text "make component"
                     :on-click
                     (fn []
                       [[::make-component $store selection]])})
      (basic/button {:text "vertical"
                     :on-click
                     (fn []
                       [[::make-vertical $store selection]])})))]
  )



(defui toolbar [{:keys [options selected]}]
  (apply
   horizontal-layout
   (for [option options]
     (let [hover? (get extra [$option :hover?])]
       (basic/button {:text (:text option)
                      :hover? (or (when-let [v (:value option)]
                                    (= selected v))
                                  hover?)
                      :$hover? $hover?
                      :on-click
                      (fn []
                        [[:set $selected (:value option)]])})))))


(defeffect ::detail-editor-toggle [$editing]
  (dispatch! :update $editing not))

(defeffect ::detail-editor-finish [$editing $obj s]
  (dispatch! :update $obj
             )
  (dispatch! :set $editing false))

(defui detail-editor [{:keys [obj editing tool]}]
  (if editing
    (let [buf (get extra [:buf $obj]
                   (buffer/buffer (with-out-str
                                    (clojure.pprint/pprint (dissoc obj
                                                                   :element/children
                                                                   :instance/component)))
                                  {:rows 40 :cols 15
                                   :mode :insert}))]
      (vertical-layout
       (basic/button {:text "done"
                      :on-click (fn []
                                  (let [s (buffer/text buf)]
                                   [ ;;[::detail-editor-finish $obj (buffer/text buf)]
                                    [:update $obj
                                     (fn [obj]
                                       (let [v (try
                                                 (clojure.edn/read-string s)
                                                 (catch Exception e
                                                   (prn e)
                                                   ::error))]
                                         (if (= v ::error)
                                           obj
                                           (let [v (if-let [children (get obj :element/children)]
                                                     (assoc v :element/children children)
                                                     v)
                                                 v (if-let [component (get obj :instance/component)]
                                                     (assoc v :instance/component component)
                                                     v)]
                                             v))))]
                                    [:set $editing false]
                                    [:delete $buf]]))})
       (code-editor/text-editor {:buf buf})))
    
    ;; else
    (vertical-layout
     (horizontal-layout
      (basic/button {:text "edit"
                     :on-click (fn []
                                 [[::detail-editor-toggle $editing]])})
      (when (= :element/component (:element/type obj))
        (basic/button {:text "tool"
                       :on-click
                       (fn []
                         (let [t instance-tool]
                          [[:set $tool
                            #(t (merge %
                                       {:elem
                                        #:element{:instance/component {:element/id (:element/id obj)}
                                                  :type :element/instance}}))]]))}))
      (when (and (= :element/component (:element/type obj))
                 (:component/args-spec obj))
        (basic/button {:text "regen"
                       :on-click
                       (fn []
                         [[:update $obj
                           (fn [obj]
                             (assoc obj :component/defaults
                                    (gen/generate (s/gen (:component/args-spec obj)))))]])})))
     (ui/label (with-out-str
                 (clojure.pprint/pprint
                  (dissoc obj
                          :instance/component
                          :element/children)))))))

(comment

  (backend/run (membrane.component/make-app  #'detail-editor {:obj {:a 1 :b 2
                                                                    :d {}
                                                                    :element/children (range 10)}}))

  ,)


(defmacro maybe-mouse-move [test handler body]
  `(if ~test
     (ui/on :mouse-move ~handler ~body)
     ~body))


(defui divider [{:keys [hover?]}]
  (let [brightness 0.9
        gray [brightness brightness brightness]
        gray2 (mapv #(* % 0.8) gray)]
   (basic/on-hover
    {:hover? hover?
     :body
     (ui/filled-rectangle (if hover?
                            gray2
                            gray)
                          75 8)})))

(defeffect ::reparent-at [path]
  (tap> [:reparent-at path]))



(defui textarea-double-click
  "Textarea component."
  [{:keys [text
           border?
           last-click
           font
           ^:membrane.component/contextual focus
           textarea-state]
    :or {border? true}}]
  (on
   ::basic/request-focus
   (fn []
     (let [time (.getTime ^java.util.Date (java.util.Date.))]
       (if (and last-click
                (< (- time last-click)
                   500))
         [[:set $focus $text]
          [:delete $last-click]]
         [[:set $last-click (.getTime ^java.util.Date (java.util.Date.))]
          [:set $focus nil]])))
   ::basic/text-double-click
   (fn [& args])

   (basic/textarea-view {:text text
                         :cursor (get textarea-state :cursor 0)
                         :focus? (= focus $text)
                         :font font
                         :down-pos (:down-pos textarea-state)
                         :mpos (:mpos textarea-state)
                         :border? (or border?
                                      (nil? border?))
                         :select-cursor (:select-cursor textarea-state)}))
  )

(defui element-tree-branch [{:keys [element expanded selection
                                    dragging?]
                             :or {expanded {}}}]
  (let [pid (:element/id element)]
    (apply
     vertical-layout
     (when dragging?
       (on
        :mouse-up
        (fn [_]
          [[::reparent-at pid 0]])
        (divider {:hover? (get extra [:hover? :pid 0])})))
     (for [[i element] (map-indexed vector (:element/children element))]
       (let [eid (get element :element/id)
             expanded? (get expanded eid)]
         (vertical-layout
          (horizontal-layout
           (if (:element/children element)
             (on
              ::basic/toggle
              (fn [$x]
                [[:update $expanded
                  (fn [m]
                    (if (get m eid)
                      (dissoc m eid)
                      (assoc m eid {})))]])
              (basic/checkbox {:checked? expanded?}))
             (ui/spacer 12))
           (ui/wrap-on
            :mouse-down
            (fn [handler pos]
              (into [[::select eid]
                     [::start-drag pid eid]]
                    (handler pos)))
            (let [lbl
                  (ui/on
                   :mouse-move (fn [_])
                   (textarea-double-click
                    {:border? false
                     :text (get element :element/name
                                eid)
                     :$text [(list 'keypath :store)
                             (list 'keypath :db)
                             (list 'keypath eid)
                             (list 'keypath :element/name)]}))
                  #_(ui/label (get element :element/name
                                   eid))
                  selected? (some #{eid} selection)]
              (if selected?
                [(ui/filled-rectangle [0.8 0.8 0.8]
                                      (ui/width lbl) (ui/height lbl))
                 lbl]
                lbl)
              )))
          (when expanded?
            (ui/translate 25 0
                          (element-tree-branch
                           {:element element
                            :expanded (get expanded eid {})
                            :dragging? dragging?
                            :selection selection})))
          (when dragging?
            (on
             :mouse-up
             (fn [_]
               [[::reparent-at pid (inc i)]])
             (divider {:hover? (get extra [:hover? :after eid])})))))))))


(def WALK-ELEM
  (specter/recursive-path [] p
                          (specter/stay-then-continue
                           (specter/if-path #(:element/children %)
		                            [(specter/keypath :element/children)
                                             specter/ALL
                                             p]
		                            specter/STAY))))


(defeffect ::reparent-elem [$store drag-source drag-destination]
  (dispatch! :update
             $store
             (fn [store]

               (let [;; remove from source
                     source-eid (:eid drag-source)
                     store
                     (specter/setval
                      [(specter/keypath :db)
                       (specter/keypath (:pid drag-source))
                       (specter/must :element/children)
                       specter/ALL
                       (fn [child]
                         (= source-eid (:element/id child)))]
                      specter/NONE
                      store)

                     ;; insert into destination
                     store
                     (specter/setval
                      [(specter/keypath :db)
                       (specter/keypath (:pid drag-destination))
                       (specter/must :element/children)
                       (specter/before-index (:idx drag-destination))]
                      {:element/id source-eid}
                      store)]
                 store))))

(defui element-tree [{:keys [store expanded selection

                             ;; drag and drop
                             mouse-down
                             drag-source
                             dragging?
                             ]
                      :or {expanded {}}}]

  (let [root (->tree-memo store ::root)
        temp-root (if (and drag-source dragging?)
                    (specter/setval
                     [WALK-ELEM #(= (:eid drag-source) (:element/id %))]
                     specter/NONE
                     root)
                    root)]
    (basic/on-mouse-out
     {:mouse-out
      (fn []
        [[:delete $mouse-down]
         [:delete $drag-source]
         [:delete $dragging?]])
      :body
      (vertical-layout
       (maybe-mouse-move
        mouse-down
        (fn [[mx my]]
          (let [[x y] mouse-down]
            (when (> (+ (* (- x mx) (- x mx))
                        (* (- y my) (- y my)))
                     (* 5 5))
              [[:delete $mouse-down]
               [:set $dragging? true]
             
               ;;[::remove-eid drag-eid]
               ])))
        (ui/wrap-on
         :mouse-down (fn [handler mpos]
                       (conj (handler mpos)
                             [:set $mouse-down mpos]))
         (ui/wrap-on
          :mouse-up
          (fn [handler pos]
            (concat
             (handler pos)
             [[:delete $mouse-down]
              [:delete $drag-source]
              [:delete $dragging?]]))
          (on
           ::start-drag
           (fn [pid eid]
             [[:set $drag-source {:pid pid
                                  :eid eid}]])
           ::reparent-at
           (fn [pid idx]
             [[::reparent-elem $store drag-source {:pid pid
                                                   :idx idx}]])
           (element-tree-branch
            {:element temp-root
             :expanded expanded
             :dragging? dragging?
             :selection selection}))))))}
     )))




(defonce et-test-state (atom nil))
(comment

  (reset!
   et-test-state
   {:root {:element/id :root
           :element/children
           [{:element/id 123}
            {:element/id 1234
             :element/children [{:element/id 10
                                 :element/children
                                 [{:element/id (gensym)}
                                  {:element/id (gensym)}]}
                                {:element/id 222}]}]}})

  (backend/run (membrane.component/make-app
                #'element-tree
                et-test-state))
  ,)


(def textarea-tool
  #(instance-tool (merge %
                         {:elem #:element{:instance/args {:text "hello"}
                                          :instance/fn `basic/textarea
                                          :type :element/instance}})))
(def checkbox-tool
  #(instance-tool (merge %
                         {:elem #:element{:instance/args {:checked? true}
                                          :instance/fn `basic/checkbox
                                          :type :element/instance}})))

(def button-tool
  #(instance-tool (merge %
                         {:elem #:element{:instance/args {:text "Go!"}
                                          :instance/fn `basic/button
                                          :type :element/instance}})))



(defn query-tree [elements-by-id eid]
  (let [m (get elements-by-id eid)
        m (if-let [xs (:element/children m)]
            (assoc m :element/children (mapv #(query-tree elements-by-id %) xs))
            m)
        m (if-let [cid (:instance/component m)]
            (assoc m :instance/component (query-tree elements-by-id cid))
            m)]
    m))

(let [tools {:line line-tool
             :rect rect-tool
             :pan pan-tool
             ;; :instance instance-tool
             :textarea textarea-tool
             :checkbox checkbox-tool
             :button button-tool
             :text text-tool
             :move move-tool
             :delete delete-tool
             :select select-tool}
      toolbar-options (mapv (fn [k]
                              {:text (name k)
                               :value k})
                            (keys tools))]
  (defui editor [{:keys [selection store tool
                         scroll-bounds
                         shift-down?
                         scroll-offset]}]
    (let [
          tool (or tool :line)
        
          tool-fn (if (keyword? tool)
                    (get tools tool)
                    tool)
          tool-extra (get extra [$extra :tools tool])
        
        
          tool-args
          {:selection selection
           :$selection $selection
           :store store :$store $store
           :extra tool-extra :$extra $tool-extra
           :context context :$context $context
           :shift-down? shift-down?

           :scroll-bounds scroll-bounds :$scroll-bounds $scroll-bounds
           :scroll-offset scroll-offset :$scroll-offset $scroll-offset}]
      (ui/wrap-on
       :key-event (fn [handler key scancode action mods]
                    
                    (let [new-shift-down? (pos? (bit-and mods 1))]
                      (if (not= new-shift-down? shift-down?)
                        (cons [:set $shift-down? new-shift-down?]
                              (handler key scancode action mods))
                        (handler key scancode action mods))))
       :key-press (fn [handler c]
                    (if (= :delete c)
                      [[::delete-selection $store selection]]
                      (handler c)))
       
       (vertical-layout
        (toolbar {:selected tool
                  :options toolbar-options})
        (horizontal-layout
         (on
          ::select
          (fn [eid]
            (if shift-down?
              [[:update $selection conj eid]]
              [[:set $selection [eid]]]))
          (element-tree {:store store
                         :selection selection}))
         (ui/wrap-on
          :mouse-down
          (fn [handler pos]
            (into [[:set '[(keypath :membrane.component/context) (keypath :focus)] nil]]
                  (handler pos)))
          (tool-fn tool-args))
         (when (seq selection)
           (let [$obj ::$obj
                 detail-elem (->tree-memo store (first selection))]
             (when detail-elem
               (on
                :update
                (fn [$ f & args]
                  (if (= $ ::$obj)
                    [[:update $store
                      (fn [store]
                        (edit-elem store (first selection) #(apply f % args)))]]
                    [(into [:update $ f]
                           args)]))
                (detail-editor {:obj detail-elem
                                :$obj $obj
                                :tool tool})))))))))))

(def initial-state
  {:selection []
   ;;:elements []
   :scroll-bounds [800 800]
   :scroll-offset [0 0]
   :tool :line
   ;; :elements-by-id {}
   :store (element-store)
   })

(defonce editor-state (atom initial-state))

(defn lookup-elem [eid]
  (->tree (:store @editor-state)
          eid))


(defn restart-editor []
  (reset! editor-state initial-state)
  (backend/run (membrane.component/make-app  #'editor editor-state))
  ,)

(defn run-editor []
  (backend/run (membrane.component/make-app  #'editor editor-state))
  )

(comment


  (reset! editor-state initial-state)

  (backend/run (membrane.component/make-app  #'line-tool editor-state))
  (backend/run (membrane.component/make-app  #'pan-tool editor-state))
  (backend/run (membrane.component/make-app  #'instance-tool editor-state))
  (backend/run (membrane.component/make-app  #'text-tool editor-state))
  (backend/run (membrane.component/make-app  #'move-tool editor-state))

  (backend/run (membrane.component/make-app  #'editor editor-state))

  ,)





(defn compile-hidden [body m]
  (when (:element/hidden m)
    (reduced nil)))

(defn compile-children [body m]
  (when (seq (:element/children m))
    (into (if body
            (if (vector? body)
              body
              [body])
            [])
          (map compile)
          (:element/children m))))

(defn compile-image [body m]
  (when-let [image (:element/image m)]
    (let [width (:element/width m)
          height (:element/height m)]
      [body
       (if (or width height)
         `(ui/image ~image [~width ~height])
         `(ui/image ~image))])))

(defn compile-form [body m]
  (when-let [form (:element/form m)]
    form))

(defn compile-for [body m]
  (when-let [for-bindings (:element/for-bindings m)]
    `(vec
      (for ~for-bindings
        ~body))))

(defn compile-flex-layout [body m]
  (let [{:flex/keys [direction
                     wrap
                     justify-content
                     align-items
                     row-gap
                     column-gap]
         :as layout}
        (:element/layout m)

        direction (get layout :flex/direction :flex.direction/row)
        wrap (get layout :flex/wrap :flex.wrap/nowrap)

        [ui-size get-size ui-cross-size get-cross-size get-gap make-spacer main-layout align ->alignment]
        (if (= direction :flex.direction/row)
          [`ui/width :element/width `ui/height :element/height :flex/row-gap #(list `ui/spacer % 0) `ui/horizontal-layout `ui/align-row
           {:flex.align/start :top
            :flex.align/end :bottom
            :flex.align/center :center}]
          [`ui/height :element/height `ui/width :element/width :flex/column-gap #(list `ui/spacer 0 %) `ui/vertical-layout `ui/align-column {:flex.align/start :left
                                                                                                                                             :flex.align/end :right
                                                                                                                                             :flex.align/center :center}])

        fixed-size? (some? (get-size m))]
    (if fixed-size?
      (let [ ;; only justify for fixed sizes
            gap (get-gap layout)
            justification (:flex/justify-content layout)

            body (if-let [gap (get-gap layout)]
                   (do
                     (assert (not (#{:flex.justify-content/space-around
                                     :flex.justify-content/space-between
                                     :flex.justify-content/space-evenly} justification))
                             (str get-gap "  Doesn't make sense with " justification))
                     `(interpose ~(make-spacer gap)
                                 ~body))
                   body)
            body (if-let [alignment (:flex/align layout)]
                   `(~align ~(->alignment alignment) ~(get-cross-size m) (vec ~body))
                   body)
            body (if-let [justification (get layout :flex/justify-content
                                             :flex.justify-content/start)]
                   (case justification
                     :flex.justify-content/start
                     `(apply ~main-layout ~body)

                     :flex.justify-content/end
                     (let [body `(apply ~main-layout ~body)
                           body# (gensym "body")
                           offset `(- ~(get-size m)
                                      (~ui-size ~body#))]
                       `(let [~body# ~body]
                          (ui/translate ~@(if (= direction :flex.direction/row)
                                            [offset 0]
                                            [0 offset])
                                        ~body#)))

                     :flex.justify-content/center
                     (let [body `(apply ~main-layout ~body)
                           body# (gensym "body")
                           offset `(/ (- ~(get-size m)
                                         (~ui-size ~body#))
                                      2)]
                       `(let [~body# ~body]
                          (ui/translate ~@(if (= direction :flex.direction/row)
                                            [offset 0]
                                            [0 offset])
                                        ~body#)))

                     (:flex.justify-content/space-between
                      :flex.justify-content/space-around
                      :flex.justify-content/space-evenly)
                     `(let [body# ~body]
                        (~(if (= direction :flex.direction/row)
                            `ui/justify-row-content
                            `ui/justify-column-content)

                         ~(get {:flex.justify-content/space-between :space-between
                                :flex.justify-content/space-around :space-around
                                :flex.justify-content/space-evenly :space-evenly}
                               justification)
                         ~(get-size m)
                         body#)))
                   ;; else, no justification
                   body)]
        body)
      (let [;; justify doesn't make sense for non-fixed-width
            ;; but gaps do.
            gap (get-gap layout)
            body (if gap
                   `(interpose ~(make-spacer gap)
                               ~body)
                   body)
            body `(apply ~main-layout ~body)
            body (if-let [alignment (:flex/align layout)]
                   `(~align ~(->alignment (:flex/align layout)) ~(get-cross-size m) ~body)
                   body)]
        body)))
  )

(defn compile-layout [body m]
  (when-let [layout (:element/layout m)]
    (if (map? layout)
      (compile-flex-layout body m)
      (case layout
        :vertical
        (let [spacing (:element/layout-spacing m)]
          (if (and spacing (pos? spacing))
            `(apply vertical-layout (interpose (ui/spacer 0 ~spacing)
                                               ~body))
            `(apply vertical-layout ~body)))
        :horizontal
        (let [spacing (:element/layout-spacing m)]
          (if (and spacing (pos? spacing))
            `(apply horizontal-layout (interpose (ui/spacer ~spacing 0)
                                                 ~body))
            `(apply horizontal-layout ~body)))

        ;;else
        (throw (Exception. (str "Unknown layout " layout))))))
  )

(defn compile-position [body m]
  (let [x (:element/x m)
        y (:element/y m)]
    (when (or x y)
      (assert (and x y) (str "Partially specified position " (pr-str m)))
      `(ui/translate ~x ~y
                     ~body))))

(defn compile-transform [body m]
  (when-let [transform (:element/transform m)]
    `(backend/transform ~(into []
                               cat
                               transform)
                        ~body)))

(defn compile-bounds [body m]
  (let [{:element/keys [width height]} m]
    (when (or width height)
      (assert (and width height))
      `(ui/scissor-view ~width ~height
                        ~body))))

(defn compile-text [body m]
  (when-let [text (:element/text m)]
    (let [body
          (if-let [font (:element/font m)]
            (let [font# (gensym)]
              `(ui/label ~text
                         (let [~font#
                               (ui/font ~(if-let [nm (:font/name font)]
                                           (str nm)
                                           nil)
                                        ~(:font/size font))
                               ~font# ~(if-let [weight (:font/weight font)]
                                         `(assoc ~font# :weight ~weight)
                                         font#)]
                           ~font#)))
            `(ui/label ~text))

          body (if-let [color (:element/color m)]
                 (let [color (if-let [element-opacity (:element/opacity m)]
                               (update color 3 * element-opacity)
                               color)]
                   `(ui/with-color ~color ~body))
                 ;; else
                 body)]
      body)))

(defn compile-padding [body m]
  (when-let [{:padding/keys [top right bottom left]} (:element/padding m)]
    `(ui/padding ~(or top 0)
                 ~(or right 0)
                 ~(or bottom 0)
                 ~(or left 0)
                 ~body)))

(defn compile-path [body m]
  (when-let [path (:element/path m)]
    `(apply ui/path ~path)))

(defn compile-stroke-color [body m]
  (when-let [stroke-color (:element/stroke-color m)]
    `(ui/with-color ~stroke-color
       ~body)))

;;           shape `(ui/with-style :membrane.ui/style-stroke
;;                    ~shape)

(defn compile-style [body m]
  (when (= :element/shape
           (:element/type m))
    `(ui/with-style :membrane.ui/style-stroke
       ~body)))


(defn compile-bindings [body m]
  (when-let [bindings (:component/bindings m)]
    `(let ~bindings
       ~body)))


(defn compile-defaults [body m]
  (when-let [defaults (:component/defaults m)]
    (let [ks (->> (:component/defaults m)
                  keys
                  (map symbol))]
      `(let [{:keys [~@ks]} ~defaults]
         ~body))))

(defn compile-instance-fn [body m]
  (when-let [cid (:instance/fn m)]
    (let [args (:instance/args m)]
      `(~cid ~args))))

(defn compile-instance [body m]
  (when-let [component (:instance/component m)]
    (reduced
     (compile (merge component (dissoc m :instance/component ))))))

(defn compile-instance-ref [body m]
  (when-let [component (:instance/component m)]
    (reduced
     `(~(symbol (:element/name component))
       ~(:component/defaults m))))
  )

(defn compile-events [body m]
  (when-let [events (:element/events m)]
    (let [handler-bindings
          (into
           []
           (comp (map (fn [[k handler]]
                        [k `(constantly ~handler)]))
                 cat)
           events)]
      `(on
        ~@handler-bindings
        ~body))))





(defn compile-paint [body m]
  (let [fills (->> (:element/fills m)
                   (remove :element/hidden))
        strokes (->> (:element/strokes m)
                     (remove :element/hidden))]
    (when (or (seq fills)
              (seq strokes))
      (let [element-opacity (:element/opacity m)

            width# (gensym "width-")
            height# (gensym "height-")

            rectangle-corner-radii (:element/corner-radius m)
            corner-radius (if (seqable? rectangle-corner-radii)
                            (first rectangle-corner-radii)
                            rectangle-corner-radii)


            with-color (fn [paint elem#]
                         (let [color (:element/color paint [0 0 0 1])
                               ;; paint opacity
                               color (if-let [opacity (:element/opacity paint)]
                                       (update color 3 * opacity)
                                       color)
                               ;; element opacity
                               color (if (and color element-opacity)
                                       (update color 3 * element-opacity)
                                       color)]
                           `(ui/with-color ~color
                              ~elem#)))

            fill-elems (when (seq fills)
                         (let [fill-body# (gensym "fill-body-")]
                           `(let [~fill-body# ~(if (:element/text m)
                                                 (compile-text nil m)
                                                 (if (:element/path m)
                                                   (compile-path nil m)
                                                   (if-let [fill-paths (seq (:element/fill-path m))]
                                                     (mapv (fn [commands]
                                                             `(svg/svg-path ~commands
                                                                            ~(or (:svg/origin m) [0 0])
                                                                            ~(or (:svg/bounds m) [0 0])))
                                                           fill-paths)
                                                     (if corner-radius
                                                       `(ui/rounded-rectangle ~width# ~height# ~corner-radius)
                                                       `(ui/rectangle ~width# ~height#)))))]
                              (ui/with-style :membrane.ui/style-fill
                                ~(mapv #(with-color % fill-body#) fills)))))
            stroke-elems (when (seq strokes)
                           (let [stroke-body# (gensym "stroke-body-")]
                             `(let [~stroke-body# ~(if (:element/text m)
                                                     (compile-text nil m)
                                                     (if (:element/path m)
                                                       (compile-path nil m)
                                                       (if-let [stroke-paths (seq (:element/stroke-path m))]
                                                         (mapv (fn [commands]
                                                                 `(svg/svg-path ~commands
                                                                                ~(or (:svg/origin m) [0 0])
                                                                                ~(or (:svg/bounds m) [0 0])))
                                                               stroke-paths)
                                                         (if corner-radius
                                                           `(ui/rounded-rectangle ~width# ~height# ~corner-radius)
                                                           `(ui/rectangle ~width# ~height#)))))

                                    ~stroke-body# ~(if-let [stroke-weight (:element/stroke-weight m)]
                                                     `(ui/with-stroke-width ~stroke-weight
                                                        ~stroke-body#)
                                                     stroke-body#)]
                                (ui/with-style :membrane.ui/style-stroke
                                  ~(mapv #(with-color % stroke-body#) strokes)))))
            body# (gensym "body-")
            ]
        `(let [~body# ~body
               [~width# ~height#] ~(let [{:element/keys [width height]} m]
                                     (if (and width height)
                                       [width height]
                                       `(ui/bounds ~body#)))]
           [~fill-elems
            ~stroke-elems
            ~body#])))))

(defn compile-case [body m]
  (when (= :flow-control/case
           (:element/type m))
    `(case ~(:flow-control.case/expression m)
       ~@(into []
               (comp (map (fn [[k v]]
                            [k (compile v)]))
                     cat)
               (:flow-control.case/clauses m)))))

;; experimenting with just some events for now
(defn compile-events [body m]
  (when-let [mouse-down (:element/mouse-down m)]
    `(ui/on
      :mouse-down ~mouse-down
      ~body)))

(def default-passes
  [compile-form
   compile-instance
   compile-image
   compile-hidden
   compile-children

   ;; compile-path
   ;; compile-style
   ;; compile-stroke-color
   
   ;; compile-text

   compile-bindings
   compile-defaults

   compile-instance-fn
   ;; compile-events


   compile-case
   compile-for
   compile-layout
   compile-padding
   compile-paint
   compile-transform

   compile-events
   compile-position
   ;;   compile-bounds
   ])

(def ^:dynamic *passes* nil)

(defn compile* [elem passes]
  (reduce
   (fn [body f]
     (if-let [new-body (f body elem)]
       new-body
       body))
   nil
   passes))

(do
  (defn compile
    ([elem]
     (if *passes*
       (compile* elem *passes*)
       (binding [*passes* default-passes]
         (compile* elem *passes*))))
    ([passes elem]
     (binding [*passes* passes]
       (compile elem))))
  (def compile-memo (memoize compile)))

(defn compile-defui [body m]
  (let [ks (->> (:component/defaults m)
                keys
                (map symbol))]
    (when (= :element/component
             (:element/type m))
      `(defui ~(symbol (:element/name m)) [{:keys [~@ks]}]
        ~body))))

(def export-passes
  [compile-form
   compile-instance-ref
   compile-image
   compile-hidden
   compile-children

   ;; compile-path
   ;; compile-style
   ;; compile-stroke-color
   
   ;; compile-text

   compile-bindings
   ;; compile-defaults
   

   compile-instance-fn
   compile-events

   compile-case
   compile-for
   compile-layout
   compile-padding
   compile-paint
   compile-transform
   compile-position
   ;; compile-bounds
   compile-defui])




(defn export-component [component]
  (binding [*passes* export-passes]
    (compile component)))

(defn export-store
  ([]
   (export-store (:store @editor-state)))
  ([store]
   (let [comps
         (for [eid (-> store :db keys)
               :let [elem (-> (->tree store eid)
                              (dissoc :element/x
                                      :element/y))]
               :when (= :element/component
                        (:element/type elem))]
           (binding [*passes* export-passes]
             (compile elem)))]
     `(do
        ~@comps))))




(defeffect :add-todo [$todos $next-todo]
  (let [next-todo (dispatch! :get $next-todo)]
    (when (seq next-todo)
      (dispatch! :update $todos conj {:description next-todo
                                      :complete? false})
      (dispatch! :set $next-todo "")))
  )

(comment

  (require '[com.phronemophobic.membrane.figma :as figma])

  ;; button
  (let [view (-> figma/document
                 (get "children")
                 (nth 6)
                 figma/normalize-bounds
                 figma/figma->membrane)
        buttons (-> view
                    (search-keep-all
                     (fn [m]
                       (when-let [name (:name m)]
                         (let [variant (figma/name->variant name)]
                           (when (and (= "False" (get variant "Loading"))
                                      (= "None" (get variant "Icon")))
                             m))))))
        button (->> buttons
                    (sort-by (fn [{{:keys [x y]} :absolute-bounding-box}]
                               (+ x y)))
                    first)]
    (def button-figma button)
    (def button-ast (figma/->ast button)))


  ;; checkbox
  (let [view (-> figma/document
                 (get "children")
                 (nth 8)
                 figma/normalize-bounds
                 figma/figma->membrane)
        cbs (-> view
                (search-keep-all
                 (fn [m]
                   (when (= (:type m)
                            "COMPONENT")
                     m))))
        cb (->> cbs
                (sort-by (fn [{{:keys [x y]} :absolute-bounding-box}]
                           (+ x y)))
                first)]
    (def checkbox-ast (figma/->ast cb)))

  ;; text field
  (let [view (-> figma/document
                 (get "children")
                 (->> (some #(when (= "Text field" (get % "name"))
                               %)))
                 figma/normalize-bounds
                 figma/figma->membrane)
        fields (-> view
                   (search-keep-all
                    (fn [m]
                      (when (= (:type m)
                               "COMPONENT")
                        m))))
        field (->> fields
                   (sort-by (fn [{{:keys [x y]} :absolute-bounding-box}]
                              (+ x y)))
                   ;; not the first!
                   second)]
    (def textfield-figma field)
    (def textfield-ast (figma/->ast field)))

  (restart-editor)
  (swap! editor-state update :store #(append-child % ::root
                                                   (assoc button-ast
                                                          :element/name "button")))
  (swap! editor-state update :store #(append-child % ::root
                                                   (assoc checkbox-ast
                                                          :element/name "checkbox")) )

  (swap! editor-state update :store #(append-child % ::root
                                                   (assoc textfield-ast
                                                          :element/name "textfield")))

  ,)


