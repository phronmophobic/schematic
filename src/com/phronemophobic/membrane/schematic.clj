(ns com.phronemophobic.membrane.schematic
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
            [datascript.core :as d])
  (:gen-class))


(defn uuid [] (java.util.UUID/randomUUID))

#_(defprotocol ICode
  :extend-via-metadata true
  (code [this]))

(defmulti component-code :type)

(defn with-translate [m view]
  (if-let [origin (:origin m)]
    `(let [[x# y#] ~origin]
       (ui/translate x# y#
                     ~view))
    view))

(defn with-events [m view]
  (let [events (:events m)]
    (if (and (seqable? events)
             (seq events))
      `(ui/on ~@(sequence cat
                          (for [[event handler] events]
                            [event (if (vector? handler)
                                     `(fn [& _#] ~handler)
                                     handler)]))
              ~view)
      view)))

(defn code [m]
  (->> (component-code m)
       (with-events m)
       (with-translate m)))

(defmethod component-code :line [{:keys [origin start end stroke-width color]}]
  `(ui/with-style :membrane.ui/style-stroke
    (ui/with-color ~color
      (ui/with-stroke-width ~stroke-width
        (ui/path ~start
                 ~end)))))


(defmethod component-code :rect [{:keys [origin bounds background-color corner-radius fill-style] :as m}]
  `(let [[w# h#] ~bounds]
     (ui/with-style (get #{:membrane.ui/style-fill
                           :membrane.ui/style-stroke
                           :membrane.ui/style-stroke-and-fill}
                         ~fill-style
                         :membrane.ui/style-fill)
       (ui/with-color ~background-color
         (if-let [corner-radius# ~corner-radius]
           (ui/rounded-rectangle w# h# corner-radius#)
           (ui/rectangle w# h#))))))

(defmethod component-code :checkbox [{:keys [origin checked?]}]
  `(ui/checkbox ~checked?))

(defmethod component-code :label [{:keys [origin text]}]
  `(ui/label ~text))

(defmethod component-code :textarea [{:keys [origin text]}]
  `(basic/textarea {:text ~text}))

(defmethod component-code :button [{:keys [origin text]}]
  `(ui/button ~text))

(defmethod component-code :defui [{:keys [origin] :as m}]
  `(~(:defui m) ~(dissoc m
                         :origin
                         :db/id
                         :type
                         :events
                         :defui)))


(defmethod component-code :for [{:keys [sym seq origin horizontal? padding] :as m}]
  (let [for-code
        `(for [~sym ~seq]
          ~(code (:component/child m)))]
   `(apply
     (if ~horizontal? horizontal-layout vertical-layout)
     (if-let [padding# ~padding]
       (interpose (ui/spacer padding# padding#)
                  ~for-code)
       ~for-code))))


;; (defmulti render :type)

;; (defmethod render :line [{:keys [start end stroke-width color]}]
;;   (ui/with-style :membrane.ui/style-stroke
;;     (ui/with-color color
;;      (ui/with-stroke-width stroke-width
;;        (ui/path start
;;                 end)))))


;; (defmethod render :rect [{:keys [origin bounds background-color]}]
;;   (ui/with-style :membrane.ui/style-stroke
;;     (let [[x y] origin
;;           [w h] bounds]
;;       (ui/translate x y
;;                     (ui/filled-rectangle background-color w h)))))


;; (defmethod render :textarea [{:keys [origin]}]
;;   (let [[x y] origin]
;;     (ui/translate x y
;;                   (basic/textarea {:text "hi"}))))


;; (defmethod render :checkbox [{:keys [origin checked?]}]
;;   (let [[x y] origin]
;;     (ui/translate x y
;;                   (ui/checkbox checked?))))

;; (defmethod render :button [{:keys [origin text]}]
;;   (let [[x y] origin]
;;     (ui/translate x y
;;                   (ui/button text))))


;; (defmethod render :label [{:keys [origin text]}]
;;   (let [[x y] origin]
;;     (ui/translate x y
;;                   (ui/label text))))


;; (defmethod render :defui [{:keys [origin] :as m}]
;;   (let [[x y] origin]
;;     (ui/translate x y
;;                   ((:defui m) (dissoc m :origin)))))

;; (defmethod render :for [{:keys [sym seq] :as m}]
  
;;   (let [[x y] origin]
;;     (ui/translate x y
;;                   ((:defui m) (dissoc m :origin)))))




(def schema {:component/layers {:db/cardinality :db.cardinality/many
                                :db/isComponent true
                                :db/valueType :db.type/ref}
             :component/child {:db/isComponent true
                               :db/valueType :db.type/ref}
             :component/args {:db/cardinality :db.cardinality/many
                              :db/isComponent true
                              :db/valueType :db.type/ref}})

(def hike-schema [{:db/cardinality :db.cardinality/many
                   :db/isComponent true
                   :db/valueType :db.type/ref
                   :db/ident :component/layers}
                  {:db/cardinality :db.cardinality/one
                   :db/isComponent true
                   :db/valueType :db.type/ref
                   :db/ident :component/child}
                  {:db/cardinality :db.cardinality/many
                   ;; :db/isComponent true
                   :db/valueType :db.type/ref
                   :db/ident :component/args}])

(defonce last-id (atom 0))
(defn gen-id []
  (swap! last-id inc))


;; (defrecord Component [id name])
;; (defrecord ComponentProp [cid name])


;; (defrecord Layer [id name])

;; (defrecord ComponentLayer [cid lid])

;; (defrecord DerivedComponent [id name props layers])

;; (defrule derive-component-props
  
;;   )

;; (defrule derive-component
;;   "Derives a component"
;;   [?component <- Component (= ?cid id)]
;;   [?props <- (acc/all) :from [ComponentProp (= ?cid cid)]]

;;   =>
;;   (insert! (->DerivedComponent ?cid (:name ?component) ?props nil)))


;; (defquery get-derived-components
;;   "Query to find promotions for the purchase."
;;   [] ;;; can also be specified using a keyword as :?type, but symbols are more idiomatic to specify bindings.
;;   [?c <- DerivedComponent])

;; (-> (c/mk-session [derive-component
;;                    get-derived-components])
;;     (c/insert (->Component 1 "Foo"))
;;     (c/insert (->Component 2 "Bar"))
;;     (c/insert (->ComponentProp 1 "littile foo"))
;;     (c/insert (->ComponentProp 1 "littile foo2"))

;;     (c/fire-rules)
;;     (c/query get-derived-components))




#_(defn Line [name start end]
    ^{`code
      (fn [_]
        `(ui/with-style :membrane.ui/style-stroke
           (ui/path ~start
                    ~end)))}
    {:name name
     :start start
     :end end})

#_(defn map->line [m]
  ^{`code
    (fn [_]
      `(ui/with-style :membrane.ui/style-stroke
         (ui/path ~start
                  ~end)))}
  m)

#_(defrecord Line [name start end]
  ICode
  (code [_]
    `(ui/with-style :membrane.ui/style-stroke
       (ui/path ~start
                ~end))))


(def conn (d/create-conn schema))
(d/transact! conn [{:name 'foo
                    :type :component}])

(defn save! [fname]
  (spit fname (pr-str @conn)))
#_(defn load! [fname]
  (read-string (slurp fname)))


;; Create a connection to the database
;; (def conn (d/connect db-uri))
#_(d/transact! conn
               [{:name "foobar"
                 :foo :hey
                 ;; :db/id -3
                 :props [{:name "a"}
                         {:name "b"}]
                 :component/layers [{:name (gensym)
                                     :start [4 5]
                                     :type :line
                                     ;; :db/id -2
                                     :end [10 14]}]}])










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

(defn delete-X* []
  (backend/->Cached
   (ui/with-style :membrane.ui/style-stroke
     (ui/with-color
       [1 0 0]
       (ui/with-stroke-width
         3
         [(ui/path [0 0]
                   [10 10])
          (ui/path [10 0]
                   [0 10])])))))
(def delete-X (memoize delete-X*))

(defui layers-editor [{:keys [layers]}]
  (apply
   vertical-layout
   (for [layer layers]
     (horizontal-layout
      (on
       :mouse-down
       (fn [_]
         [[::delete-entity (:db/id layer)]])
       (delete-X))
      (on
       :mouse-down
       (fn [_]
         [[::select-layer (:db/id layer)]])
       (ui/label (str (:db/id layer)
                      " "
                      (:type layer)
                      (when-let [name (:name layer)]
                        (str " "
                             name)))))))))

(defeffect ::update-layer-prop [layer-id prop s]
  (d/transact! conn [[:db/add layer-id prop (read-string s)]]))

(defeffect ::delete-layer-prop [layer-id prop]
  (d/transact! conn [[:db/retract layer-id prop]]))

(defn wrap-for [db layer-id]
  (let [eids (d/q '[:find ?e
                    :in $ ?lid
                    :where
                    
                    [?e :component/layers ?lid]]
                  db
                  layer-id)
        _ (assert (= (count eids) 1))
        eid (ffirst eids)

        layer-origin (ffirst
                      (d/q '[:find ?origin
                             :in $ ?lid
                             :where
                             [?lid :origin ?origin]]
                           db
                           layer-id))

        fid -1]
    [;; create new layer
     {:db/id fid
      :type :for
      :sym 'x
      :origin layer-origin
      :seq '(range 4)
      :component/child layer-id}
     [:db/add eid :component/layers fid]
     [:db/retract eid :component/layers layer-id]
     [:db/add layer-id :origin [0 0]]]))

(defeffect ::wrap-for [layer-id]
  (d/transact! conn [[:db.fn/call wrap-for layer-id]]))



(defui detail-editor [{:keys [layer bufs]}]
  (apply
   vertical-layout
   (let [prop-name (get extra [(:db/id layer) :prop-name] "")]
     (horizontal-layout
      (basic/button {:text "+"
                     :on-click
                     (fn []
                       [[::update-layer-prop (:db/id layer) (keyword prop-name) "42"]
                        [:set $prop-name ""]])})
      (basic/textarea {:text prop-name})))

   (ui/label (pr-str layer))
   (basic/button {:text "wrap for"
                  :on-click
                  (fn []
                    [[::wrap-for (:db/id layer)]])})
   (basic/button {:text "reset"
                  :on-click
                  (fn []
                    [[:delete $bufs nil]])})

   (for [[k v] layer
         :when (not (#{:db/id :type} k))]
     (if (= k :component/child)
       (horizontal-layout
        (ui/label k)
        (basic/button
         {:text "edit"
          :hover? (get extra [v :hover?])
          :on-click
          (fn []
            [[::select-layer (:db/id v)]])}))
       (let [buf (get bufs [(:db/id layer) k])
             buf (or buf
                     (buffer/buffer (pr-str v)
                                    {:rows 40 :cols 5
                                     :mode :insert}))]
       
         (horizontal-layout
          (on
           :mouse-down
           (fn [_]
             [[::delete-layer-prop (:db/id layer) k]])
           (delete-X))
          (basic/button {:text "set"
                         :hover? (get extra $buf)
                         :on-click
                         (fn []
                           [[::update-layer-prop (:db/id layer) k (buffer/text buf)]])})
          (ui/label k)
          (code-editor/text-editor {:buf buf})))))))



(defn construct* [component]
  (let [layer-bindings (for [layer (:component/layers component)]
                         [(symbol (str "x" (:db/id layer)))
                          (code layer)])
        bindings (into []
                       cat
                       (concat
                        (for [arg (:component/args component)]
                          [(symbol (:k arg))
                           (:v arg)])
                        layer-bindings))
        ret (into [] (map first) layer-bindings)
        body `(let [~'extra nil
                    ~'context nil]
                ~(membrane.component/path-replace
                    `(try
                       (let ~bindings
                         ~ret)
                       (catch Exception e#
                         nil))))]
    ;; (prn body)
    body))





(defn construct-defui* [component]
  (let [layer-bindings (for [layer (:component/layers component)]
                         [(symbol (str "x" (:db/id layer)))
                          (code layer)])
        bindings (into []
                       cat
                       layer-bindings)
        ret (into [] (map first) layer-bindings)
        body 
        `(defui ~(:name component) [{:keys [~@(for [arg (:component/args component)]
                                 (symbol (:k arg)))]}]
           (let ~bindings
             ~ret))]
    ;; (prn body)
    body))

(defn construct [component]
  (try
    (let [view (eval (construct* component))]
      (ui/bounds view)
      view)
    (catch Exception e
      (tap> e)
      (tap> (construct* component))
      nil)))

(def construct-memo (memoize construct))

(defn add-layer [c layer]
  (update c :component/layers
          conj layer))

(defeffect ::add-layer [cid layer]
  (d/transact! conn [{:component/layers [layer]
                      :db/id cid}])

  
  #_(dispatch! :update $component
               add-layer layer))


(defeffect ::delete-entity [eid]
  (d/transact! conn [[:db.fn/retractEntity eid]]))

(defn normalize-line [start end]
  (let [[x1 y1] (mapv int start)
        [x2 y2] (mapv int end)
        ox (min x1 x2)
        oy (min y1 y2)
        
        ]
    {:origin [ox oy]
     :start [(- x1 ox)
             (- y1 oy)]
     :end [(- x2 ox)
           (- y2 oy)]}))

(defn find-origins* [component]
  (let [layer-bindings (for [layer (:component/layers component)]
                         [(symbol (str "x" (:db/id layer)))
                          ;;(list 'quote (code layer))
                          `(ui/origin ~(code layer))])
        $prop-bindings (for [arg (:component/args component)]
                         [(symbol (str "$" (:k arg)))
                          nil])
        bindings (into []
                       cat
                       (concat
                        $prop-bindings
                        (for [arg (:component/args component)]
                          [(symbol (:k arg))
                           (:v arg)])
                        layer-bindings))
        ret (into [] (map first) layer-bindings)
        body `(try
                (let ~bindings
                  ~ret)
                (catch Exception e#
                  nil))]
    ;; (prn body)
    body))

(defn find-origins [component]
  (try
    (eval (find-origins* component))
    (catch Exception e
      (tap> e)
      nil)))

(def find-origins-memo (memoize find-origins))

(defui base-component-view [{:keys [view
                                    component
                                    show-origins?
                                    ^:membrane.component/contextual focus]}]
  (ui/maybe-key-event
   (nil? focus)
   (ui/on
    :key-event
    (fn [key scancode action mods]
      (when (= scancode 56)
        (if (= action :press)
          [[:set $show-origins? true]]
          [[:set $show-origins? false]])))
    (ui/bordered [1 1]
                 (basic/scrollview
                  {:scroll-bounds [600 600]
                   :body
                   (ui/no-events
                    (ui/try-draw
                     [view
                      (when show-origins?
                        (mapv #(let [[x y] %]
                                 (ui/translate
                                  x y
                                  (ui/filled-rectangle [0 0 0]
                                                       3 3)))
                              (find-origins-memo component)))]
                     nil))})))))


(defui line-tool [{:keys [component]}]
  (let [
        mdown? (get extra :mdown?)
        mpos (get extra :mpos)
        temp-pos (get extra :temp-pos)

        c (if (and mpos temp-pos)
            (add-layer component
                       (merge
                        (normalize-line mpos temp-pos)
                        {:type :line
                         :color [0 0 0]
                         :stroke-width 1})
                       )
            component)
        view (construct c)]
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
         [[::add-layer (:db/id component)
           (merge
            (normalize-line mpos pos)
            {:type :line
             :color [0 0 0]
             :stroke-width 1})]
          [:delete $temp-pos]
          [:delete $mpos]]))
     (base-component-view {:view view
                           :component component})
     )))


(defeffect ::set-layer-origin [layer-id pos]
  (d/transact! conn [[:db/add layer-id :origin (mapv int pos)]]))

(defui move-tool [{:keys [component]}]
  (let [
        selected-layer-id (get extra :selected-move-layer-id)

        view (construct component)]
    (ui/on
     :mouse-down
     (fn [[mx my :as pos]]
       (let [selected-origin
             (loop [origins (seq (find-origins-memo component))
                    i 0
                    selected-origin nil
                    selected-distance (* 10 10)]
               (if origins
                 (let [[ox oy] (first origins)
                       dist (+ (* (- mx ox) (- mx ox))
                               (* (- my oy) (- my oy)))]
                   (if (< dist selected-distance )
                     (recur (next origins) (inc i) i dist)
                     (recur (next origins) (inc i) selected-origin selected-distance)))
                 selected-origin))]
         (when selected-origin
           (let [layer-id (-> component :component/layers (nth selected-origin) :db/id)]
             [[:set $selected-layer-id layer-id]
              [::set-layer-origin layer-id pos]])))
       #_[[:set $mpos pos]
          [:delete $temp-pos nil]])
     :mouse-move
     (fn [pos]
       (when selected-layer-id
         [[::set-layer-origin selected-layer-id pos]]))
     :mouse-up
     (fn [pos]
       (when selected-layer-id
         [[::set-layer-origin selected-layer-id pos]
          [:delete $selected-layer-id]]))
     (let [show-origins? true]
       (on
        :set
        (fn [$ref v]
          (when-not (= $ref $show-origins?)
            [[:set $ref v]]))
        (base-component-view {:view view
                              :show-origins? show-origins?
                              :component component}))))))

(defn normalize-rect [start end]
  (let [[x1 y1] start
        [x2 y2] end]
    {:origin [(int (min x1 x2))
              (int (min y1 y2))]
     :bounds [(int (Math/abs (- x2 x1)))
              (int (Math/abs (- y2 y1)))]}))

(defui rect-tool [{:keys [component]}]
  (let [mdown? (get extra :mdown?)
        mpos (get extra :mpos)
        temp-pos (get extra :temp-pos)
        c (if (and mpos temp-pos)
            (add-layer component
                       (merge
                        {:type :rect
                         :background-color [0 0 0]}
                        (normalize-rect mpos temp-pos)))
            component)
        view (construct c)]
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
         [[::add-layer (:db/id component)
           (merge
            {:type :rect
             :background-color [0 0 0]}
            (normalize-rect mpos pos))]
          [:delete $temp-pos]
          [:delete $mpos]]))
     (base-component-view {:view view
                           :component component}))))



(defui defui-tool [{:keys [component defui defaults]}]
  (let [temp-pos (get extra :temp-pos)
        c (if temp-pos
            (add-layer component
                       (merge
                        {:origin temp-pos
                         :type :defui
                         :defui defui}
                        defaults))
            component)
        view (construct c)]
    (ui/on
     :mouse-move
     (fn [pos]
       [[:set $temp-pos pos]])
     :mouse-down
     (fn [[mx my :as pos]]
       [[:delete $temp-pos]
        [::add-layer (:db/id component)
         (merge
          {:origin [(int mx)
                    (int my)]
           :type :defui
           :defui defui}
          defaults)]])
     (base-component-view {:view view
                           :component component}))))

(defn make-defui-tool
  ([defui]
   (make-defui-tool defui {}))
  ([defui defaults]
   (fn [m]
     (defui-tool (merge m
                        {:defui defui
                         :defaults defaults})))))



(defui label-tool [{:keys [component]}]
  (let [temp-pos (get extra :temp-pos)
        c (if temp-pos
            (add-layer component
                       {:origin temp-pos
                        :type :label
                        :text "helvetica"})
            component)
        view (construct c)]
    (ui/on
     :mouse-move
     (fn [pos]
       [[:set $temp-pos pos]])
     :mouse-down
     (fn [[mx my :as pos]]
       [[:delete $temp-pos]
        [::add-layer (:db/id component)
         {:origin [(int mx)
                   (int my)]
          :type :label
          :text "helvetica"}]])
     (base-component-view {:view view
                           :component component}))))


(defui args-editor [{:keys [args]}]
  (let [temp (get extra :temp)]
    (apply
     vertical-layout
     (basic/button {:text "+"
                    :on-click
                    (fn []
                      [[::add-arg]])}
                   )
     (basic/button {:text "reset"
                    :on-click
                    (fn []
                      [[:delete $temp]])})
     (for [arg args]
       
       (let [arg-name (get temp [$arg :name])
             arg-name (or arg-name (pr-str (:k arg)))
             buf (get temp [$arg :buf])
             buf (or buf
                     (buffer/buffer (pr-str (:v arg))
                                    {:rows 40 :cols 5
                                     :mode :insert}))]
         (horizontal-layout
          (on
           :mouse-down
           (fn [_]
             [[::delete-entity (:db/id arg)]])
           (delete-X))
          (basic/button {:text "set"
                         :hover? (get extra [(:db/id arg) :hover?])
                         :on-click
                         (fn []
                           [[::update-arg (:db/id arg)
                             arg-name (buffer/text buf)]])})
          (vertical-layout
           (basic/textarea {:text arg-name})
           (code-editor/text-editor {:buf buf})))))))
  )

(defeffect ::update-arg [aid k v]
  (d/transact! conn [[:db/add aid :k (read-string k)]
                     [:db/add aid :v (read-string v)]]))

(defeffect ::add-arg [cid]
  (d/transact! conn [{:db/id cid
                      :component/args {:k (symbol (str "arg-" (gen-id)))
                                       :v 42}}]))

(defn clear-component [db cid]
  (let [eids (d/q '[:find ?e
                    :where
                    (or
                     [?c :component/args ?e]
                     [?c :component/layers ?e])]
                  db)]
    (prn "clearing eids" eids)
    (mapv (fn [[eid]]
            [:db.fn/retractEntity eid])
          eids)))

(defonce schematic-state
  (atom {}))
(reset!
 schematic-state
 {})

(swap! schematic-state
       assoc :tools {"Line" {:text "Line"
                             :value line-tool}
                     "Move" {:text "Move"
                             :value move-tool}
                     "Rect" {:text "Rect"
                             :value rect-tool}
                     "Checkbox" {:text "Checkbox"
                                 :value (make-defui-tool 'membrane.basic-components/checkbox
                                                         {:checked? true})}
                     "Button" {:text "Button"
                               :value (make-defui-tool 'membrane.basic-components/button
                                                       {:text "action!"})}
                     "Label" {:text "Label"
                              :value label-tool}
                     "Textarea2" {:text "Textarea2"
                                  :value (make-defui-tool 'membrane.basic-components/textarea
                                                          {:text "wassup?"})}})
(def schematic-dispatch! (membrane.component/default-handler schematic-state))

(defeffect ::clear-component [cid]
  (d/transact! conn [[:db.fn/call clear-component cid]]))
(defeffect ::update-component-name [cid name]
  (d/transact! conn [[:db/add cid :name (symbol name)]]))

(defeffect ::save-component [cid]
  (binding [*ns* (the-ns 'user)]
    (let [component (d/pull @conn '[* {:component/layers [*]
                                       :component/args [*]}]
                            cid)
          
          defaults (eval
                    (into {}
                          (for [{:keys [k v]} (:component/args component)]
                            [(keyword k) v])))
          component-code (construct-defui* component)
          uivar (try
                  (eval component-code)
                  (catch Exception e
                    (clojure.pprint/pprint component-code)
                    (throw e)))]
      (swap! schematic-state
             update :tools
             assoc (name (:name component)) {:text (name (:name component))
                                             :value (make-defui-tool (symbol uivar)
                                                                     defaults)}))))

(defeffect ::add-component []
  (d/transact! conn
               [{:type :component
                 :name (gensym)}]))



(defui component-editor [{:keys [component detail-layer tool]}]
  (on
   ::select-layer
   (fn [layer-id]
     [[:set $detail-layer layer-id]])
   (horizontal-layout
    (vertical-layout
     (on
      ::add-arg
      (fn []
        [[::add-arg (:db/id component)]])
      (args-editor {:args (:component/args component)}))
     (layers-editor {:layers (:component/layers component)}))
    (let [extra (get extra [$component :extra])]
      (tool {:component component
             :$component $component
             :extra extra
             :$extra $extra
             :context context
             :$context $context}))
    (when detail-layer
      (detail-editor {:layer detail-layer})))))

(defui schematic [{:keys [components selected-component selection tool tools detail-layer]}]
  (let [c (get components selected-component)
        tool (or tool line-tool)]
    
    (vertical-layout
     (horizontal-layout
      (basic/button {:text "+"
                     :on-click
                     (fn []
                       [[::add-component]])})
      (toolbar {:selected selected-component
                :options
                (for [component (vals components)]
                  {:text (:name component)
                   :value (:db/id component)})}))
     (horizontal-layout
      (basic/button {:text "clear"
                     :on-click (fn []
                                 [[::clear-component (:db/id c)]])})
      (toolbar {:selected tool
                :options (vals tools)}))
     (let [component-name (get extra [(:db/id c) :name] "")]
       (horizontal-layout
        (basic/button {:text "save"
                       :on-click
                       (fn []
                         [[::save-component (:db/id c)]])}
                      )
        (basic/button {:text "reset"
                       :on-click
                       (fn []
                         [[:set $component-name (str (:name c))]])})
        
        (basic/button {:text "set"
                       :on-click
                       (fn []
                         [[::update-component-name (:db/id c) component-name]])})
        (basic/textarea {:text component-name})))
     (component-editor {:component c
                        :detail-layer detail-layer
                        :tool tool}))))

(defn current-component []
  (let [components
        (->> (d/q '[:find ?cid ?name
                    :where
                    [?cid :type :component]
                    [?cid :name ?name]]
                  @conn)
             (into {} (map (fn [[cid name]]
                             [cid
                              {:db/id cid
                               :name name}]))))
        selected-component (or (:selected-component @schematic-state)
                               (-> components
                                   keys
                                   first))]
    (d/pull @conn '[*]
          selected-component))
  )

(defn all-components []
  (let [cids
        (->> (d/q '[:find ?cid
                    :where
                    [?cid :type :component]]
                  @conn)
             (map first))]
    (d/pull-many @conn '[*]
                 cids)))

(defn make-app
  ([ui-var ephemeral-state]
   (let [

         handler (membrane.component/default-handler ephemeral-state)

         arglist (-> ui-var
                     meta
                     :arglists
                     first)
         m (first arglist)
         arg-names (disj (set (:keys m))
                         'extra
                         'context)
         defaults (:or m)
         

         top-level (fn []
                     (let [db @conn
                           state @ephemeral-state

                           components
                           (->> (d/q '[:find ?cid ?name
                                       :where
                                       [?cid :type :component]
                                       [?cid :name ?name]]
                                     db)
                                (into {} (map (fn [[cid name]]
                                                [cid
                                                 {:db/id cid
                                                  :name name}]))))

                           selected-component (or (:selected-component state)
                                                  (-> components
                                                      keys
                                                      first))

                           detail-layer (when-let [detail-layer-id (:detail-layer state)]
                                          (d/pull db '[*]
                                                  detail-layer-id))

                           components (merge components
                                             {selected-component
                                              (d/pull db '[*]
                                                      selected-component)})
                           full-state
                           (merge state
                                  {:components components
                                   :selected-component selected-component
                                   :detail-layer detail-layer})]
                       
                       (membrane.component/top-level-ui
                        {:state full-state
                         :$state []
                         :body ui-var
                         :arg-names arg-names
                         :defaults defaults
                         :handler handler})))]
     top-level)))



(defn run-schematic []
  (backend/run (make-app #'schematic
                         schematic-state)))


(comment
  (clojure.pprint/pprint
   (construct-defui* (current-component))))

(comment
  (require '[rewrite-clj.parser :as p]
           '[rewrite-clj.node :as n]
           '[rewrite-clj.zip :as z])
  ;; (require )

  (-> (z/of-string "[1 2 3]")
      (z/edit* (fn [vec]
                 (prn vec
                      (type vec)
                      
                      (update vec :children n/comma-separated))
                 (update vec :children n/comma-separated)))
      z/root-string
      
      ))

(comment
  (eval (construct-defui*
         '{:db/id 1,
           :name image-placeholder,
           :type :component,
           :component/args
           [{:db/id 3, :k width, :v 42} {:db/id 4, :k height, :v 80}],
           :component/layers
           [{:db/id 2,
             :background-color [0 0 0],
             :bounds [width height],
             :corner-radius (/ (min width height) 4),
             :fill-style :membrane.ui/style-stroke,
             :origin [0 0],
             :type :rect}]}))
  


  (let [component
        '{:db/id 1,
          :name image-placeholder,
          :type :component,
          :component/args
          [{:db/id 3, :k width, :v 42} {:db/id 4, :k height, :v 80}],
          :component/layers
          [{:db/id 2,
            :background-color [0 0 0],
            :bounds [width height],
            :corner-radius (/ (min width height) 4),
            :fill-style :membrane.ui/style-stroke,
            :origin [0 0],
            :type :rect}]}
        defaults (eval
                  (into {}
                        (for [{:keys [k v]} (:component/args component)]
                          [(keyword k) v])))]
    (swap! schematic-state
           update :tools
           assoc (name (:name component)) {:text (name (:name component))
                                           :value (make-defui-tool #'image-placeholder
                                                                   defaults)}))
  ,)







