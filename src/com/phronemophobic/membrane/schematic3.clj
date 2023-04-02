(ns com.phronemophobic.membrane.schematic3
  (:refer-clojure :exclude [compile])
  (:require [clojure.spec.alpha :as s]
            [com.phronemophobic.viscous :as viscous]
            [membrane.components.code-editor.code-editor
             :as code-editor]
            [liq.buffer :as buffer]
            [membrane.alpha.stretch :as stretch]
            [com.rpl.specter :as specter]
            [clojure.zip :as z]
            [zippo.core :as zippo]
            [clojure.test.check.generators :as gen]
            [membrane.basic-components :as basic]
            [membrane.component :as component
             :refer [defui defeffect]]
            [membrane.ui :as ui]
            [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [com.phronemophobic.membrane.drag-and-drop :as dnd] 
            #_[clojure.spec.gen.alpha :as gen]))

(def WALK-ELEM
  (specter/recursive-path [] p
                          (specter/continue-then-stay
                           (specter/cond-path
                            (specter/must :element/children) [:element/children specter/ALL p]
                            (specter/must :prototype/component) [:prototype/component p]
                            (specter/must ::value) [::value p]
                            (specter/must :component/body) [:component/body p]
                            (specter/must :element/body) [:element/body p]))))

(defmulti compile* :element/type)

(defn compile [o]
  (cond
    (map? o)
    ;;(compile* o)
    (if (= (:element/type o)
           ::code)
      (compile* o)
      ;; (compile* o)
      `(when-let [elem# ~(compile* o)]
         (with-meta
           elem#
           (quote ~{::ast (select-keys o [:element/id])}))))

    (or (string? o)
        (nil? o))
    o

    (seqable? o)
    (into (empty o)
          (map compile)
          o)

    :else
    o))


(defprotocol IContentOffset
  (content-offset [view]))

(extend-protocol IContentOffset
  membrane.ui.Padding
  (content-offset [view]
    [(:left view)
     (:top view)])
  Object
  (content-offset [view]
    (ui/origin view)))

(defn ui-zip [view]
  (z/zipper (constantly true)
            ui/children
            #(throw (ex-info "Can't unzip" {:view view}))
            view))


(defn global-coord [zview]
  ;; Walk up the parents to find
  ;; the "global" coordinate
  (let [[ox oy] (-> zview
                    z/node
                    ui/origin)]
    (loop [x ox
           y oy
           zview (z/up zview)]
      (if zview
        (let [view (z/node zview)
              [ox oy] (content-offset view)]
          (recur (+ x ox)
                 (+ y oy)
                 (z/up zview)))
        [x y]))))

(defn highlight-selection [view selection]
  (loop [highlight []
        zview (ui-zip view)]

    (if (z/end? zview)
      highlight
      (let [view (z/node zview)
            ast (-> view
                    meta
                    ::ast)
            eid (:element/id ast)]
        #_(when (selection eid)
          (clojure.pprint/pprint
           {:id (:element/id ast)
            :coord (global-coord zview)
            :bounds (ui/bounds view)})
          )

        (recur (if (and (selection eid)
                        #_(= ::rectangle
                             (:element/type ast)))
                 (conj highlight
                       (let [[gx gy] (global-coord zview)
                             [w h] (ui/bounds view)]
                         (ui/translate gx gy
                                       (ui/filled-rectangle [0 1 0]
                                                            (max 5 w)
                                                            (max 5 h)))))
                 highlight)
               (z/next zview))))))

(s/def :element/id uuid?)
#_(s/def :element/type
  #{::rectangle})

(s/def ::form any?)
(s/def ::coord (s/or :double double?
                     :int int?
                     :form ::form))

(s/def :element/children
  (s/or :literal (s/coll-of :element/element)
        :form ::form))

(s/def :for/xs any?)
(s/def :for/x symbol?)
(s/def ::for
  (s/keys
   :req [:for/xs
         :for/x
         :for/body]))

(s/def :element/x ::coord)
(s/def :element/y ::coord)
(s/def :element/width ::coord)
(s/def :element/height ::coord)

(defmulti element-type :element/type)
(defmethod element-type ::rectangle [_]
  (s/keys :req [:element/width
                :element/height]
          :opt [:element/x
                :element/y]))

(def layout-specs
  (s/keys :opt [:element/children]))
(defmethod element-type ::vertical-layout [_]
  layout-specs)

(defmethod element-type ::horizontal-layout [_]
  layout-specs)

(defmethod element-type ::group [_]
  (s/keys :opt [:element/children]))

(defmethod element-type ::let [_]
  (s/keys :opt [:element/body
                :element/bindings]))

(s/def :element/element
  (s/merge
   (s/multi-spec element-type :element/type)
   (s/keys :opt [:element/id])))

(comment
  (swap! @#'s/registry-ref dissoc :element/type)
  ,)




(defmethod compile* ::rectangle [{:element/keys [width height]}]
  `(ui/filled-rectangle [0 0 0] ~width ~height))



(defmethod compile* ::vertical-layout [{:element/keys [children]}]
  `(apply ui/vertical-layout
          ~(compile children)))

(defmethod compile* ::horizontal-layout [{:element/keys [children]}]
  `(apply ui/horizontal-layout
          ~(compile children)))


(defmethod compile* ::group [{:element/keys [children]}]
  (into []
        (map compile children)))

(defmethod compile* ::for [{:element/keys [body layout]
                            :element.for/keys [x xs]}]
  (let [layout (case layout
                 :vertical `ui/vertical-layout
                 :horizontal `ui/horizontal-layout
                 ;; else
                 `vector)]
    `(apply
      ~layout
      (for [~x ~(compile xs)]
        ~(compile body)))))

(defmethod compile* ::let [{:element/keys [body bindings]}]
  `(let ~(into []
               (comp
                (map (fn [[binding val]]
                       [binding (compile val)]))
                cat)
               bindings)
     ~(compile body)))

(defmethod compile* ::paragraph [{:element/keys [text
                                                 width
                                                 paragraph-style]}]
  `(para/paragraph ~(compile text)
                   ~(compile width)
                   ~(compile paragraph-style)))

(defmethod compile* ::translate [{:element/keys [x y body]}]
  `(ui/translate ~(compile (or x 0))
                 ~(compile (or y 0))
                 ~(compile body)))

(defmethod compile* ::spacer [{:element/keys [width height]}]
  `(ui/spacer ~(compile (or width 0))
              ~(compile (or height 0))))


(defn truncate [s n]
  (let [cnt (count s)]
    (if (> cnt n)
      (subs s 0 n)
      s)))

(defui component-toolbar [{:keys [selected-component interns]}]
  (apply
   ui/horizontal-layout
   (for [[k v] (into
                [[:checkbox {:element/type ::instance
                             :instance/args
                             {:element/type ::code
                              :element/code {:checked? true}}
                             :instance/component `basic/checkbox}]
                 [:paragraph {:element/type ::paragraph
                              :element/text "label"
                              :element/width nil}]
                 [:textarea {:element/type ::instance
                             :instance/args
                             {:element/type ::code
                              :element/code {:text "text"}}
                             :instance/component `basic/textarea}]
                 [:dropdown {:element/type ::instance
                             :instance/args
                             {:element/type ::code
                              :element/code {:options [[:this "This"]
                                                       [:that "That "]
                                                       [:the-other "The Other"]]
                                             :open? true}}
                             :instance/component `basic/dropdown}]
                 [:button {:element/type ::instance
                           :instance/args
                           {:element/type ::code
                            :element/code {:text "button"}}
                           :instance/component `basic/button}]
                 [:number-slider {:element/type ::instance
                                  :instance/args
                                  {:element/type ::code
                                   :element/code {:num 3
                                                  :min 0
                                                  :max 20}}
                                  :instance/component `basic/number-slider}]
                 [:group {:element/type ::group
                          :element/children []}]
                 [:vertical-layout
                  {:element/type ::vertical-layout
                   :element/children []}]
                 [:horizontal-layout
                  {:element/type ::horizontal-layout
                   :element/children []}]]
                (for [[k v] interns]
                  [(str k)
                   {:element/type ::instance
                    :instance/args {:element/type ::code
                                    :element/code {}}
                    :instance/component {:element/type ::reference
                                         ::id k}}]))]
     (let [hover? (get extra [:hover? k])
           hover? (or hover?
                      (= selected-component
                         v))]
       (basic/button {:text (truncate (name k) 10)
                      :hover? hover?
                      :on-click
                      (fn []
                        [[:set $selected-component v]])})))
   ))

(declare my-component)
(def eval-ns *ns*)

(def compile2
  (memoize compile))
(def render2
  (memoize
   (fn [form]
     (eval
      `(fn [~'interns]
         ~(compile2 form))))))

(defui rectangle-editor [{:keys [root selection mpos selected-component interns]}]
  (let [[cw ch :as size] (:membrane.stretch/container-size context)]
    [(ui/on
      :mouse-move
      (fn [_mpos]
        [[:set $mpos _mpos]])
      :mouse-down
      (fn [[mx my]]

        (when selected-component
          [[:update $root
            update-in
            [:element/body
             :element/children]
            conj

            {:element/type ::translate
             :element/id (random-uuid)
             :element/x mx
             :element/y my
             :element/body
             (assoc selected-component
                    :element/id (random-uuid))}]]))
      (ui/fixed-bounds
       [cw ch]
       (ui/no-events
        (let [view
              (binding [*ns* eval-ns]
                (let [f (render2 root)]
                  (f interns)))
              view (if selection
                     [(highlight-selection view selection)
                      view
                      ]
                     view)]
          [
           view
           #_(when mpos
               (ui/translate (nth mpos 0) (nth mpos 1)
                             (ui/filled-rectangle [0 0 0]
                                                  30 30)))
           ]))))
     (component-toolbar {:selected-component selected-component
                         :interns interns})])
  )

(defn form-zip [view]
  (z/zipper (constantly true)
            ui/children
            #(throw (ex-info "Can't unzip" {:view view}))
            view))

(comment
  (let [eid (-> @app-state
                :forms
                rand-nth
                :element/body
                :element/id)]
    (swap! app-state assoc :selection #{eid}))
  ,)

(defn elem-label [elem selected?]
  (let [type (:element/type elem)
        lbl (ui/label (name (if (= ::instance type)
                              (let [component (:instance/component elem)]
                                (if (symbol? component)
                                  component
                                  type))
                              type)))
        view (if selected?
               (ui/fill-bordered [0.9 0.9 0.9]
                                 0
                                 lbl)
               lbl)]
    view))

(defeffect ::reparent-before [eid new-parent-eid i]
  (dispatch! :update
             (specter/path :root)
             (fn [root]
               (let [new-parent (specter/select-one
                                 [WALK-ELEM #(= new-parent-eid
                                                (:element/id %))]
                                 root)]
                 (if (#{::group
                        ::vertical-layout
                        ::horizontal-layout}
                        (:element/type new-parent))
                   (let [elem (specter/select-one
                               [WALK-ELEM #(= eid
                                              (:element/id %))]
                               root)]
                     (->> root
                          (specter/setval
                           [WALK-ELEM #(= eid
                                          (:element/id %))]
                           specter/NONE)
                          (specter/setval [WALK-ELEM
                                           #(= new-parent-eid
                                               (:element/id %))
                                           :element/children
                                           (specter/before-index i)]
                                          elem)))
                   ;; else
                   root))))
  
  )

(defui reparent-drop-element
  [{:keys [eid i]}]
  (let [hover? (get extra [:hover eid i])]
    (dnd/on-hover
     {:hover? hover?
      :$body nil
      :body
      (dnd/on-drop
       (fn [_ from-eid]
         [[::reparent-before from-eid eid i]])
       (ui/filled-rectangle (if hover?
                              [0.5 0.5 0.5]
                              [0.8 0.8 0.8]
                              )
                            100 7))})))

(defui tree-view [{:keys [branch? children root selection collapse?
                          ^:membrane.component/contextual
                          drop-object ]}]
  (if (branch? root)
    (let [eid (:element/id root)
          collapsed? (collapse? eid)]
      (ui/vertical-layout
       (ui/horizontal-layout
        (ui/on
         ::basic/toggle
         (fn [_]
           [[::toggle-collapse eid]])
         (basic/checkbox {:checked? (not collapsed?)}))
        (ui/on
         :mouse-down
         (fn [_]
           [[::select (:element/id root)]
            [::dnd/drag-start eid]])
         (elem-label root (selection eid))))
       (when (not collapsed?)
         (ui/translate
          10 0
          (apply
           ui/vertical-layout
           (when drop-object
             (reparent-drop-element {:eid eid
                                     :i 0}))
           (for [[i child] (eduction
                            (remove #(= drop-object (:element/id %)))
                            (map-indexed vector)
                            (children root))]
             (ui/vertical-layout
              (tree-view {:branch? branch?
                          :children children
                          :selection selection
                          :root child
                          :collapse? collapse?})
              (when drop-object
               (reparent-drop-element
                {:eid eid
                 :i (inc i)})))))))))

    ;; else
    (ui/on
     :mouse-down
     (fn [_]
       [[::select (:element/id root)]
        [::dnd/drag-start(:element/id root) ]])
     (elem-label root (selection (:element/id root))))))


(defonce app-state (atom nil))

(def initial-state
  {:root {:element/type ::let
          :element/id ::root
          :element/bindings []
          :element/body {:element/type ::group
                         :element/id ::root-group
                         :element/children []}}
   :interns {}
   :selection #{}
   :collapsed #{}})

(defn init! []
  (reset! app-state initial-state)
  )

(defn element-branch? [e]
  (or (vector? e)
      (:element/body e)
      (:element/children e)
      (:prototype/component e)
      (::value e)))

(defn element-children [e]
  (if (vector? e)
    e
    (if-let [body (:element/body e)]
      [body]
      (if-let [children (:element/children e)]
        children
        (if-let [component (:prototype/component e)]
          [(-> component
               ::value
               :component/body)])))))

(defn element-make-node [elem children]
  (if (:element/body elem)
    (assoc elem :element/body (first children))
    (assoc elem :element/children (vec children))))

(defn stoggle [s x]
  (if (contains? s x)
    (disj s x)
    (conj s x)))

(defeffect ::update-instance [eid buf]
  (future
    (try
      (let [txt (buffer/text buf)
            data
            (binding [*ns* eval-ns]
              (read-string txt))]
        (prn eid)
        (dispatch! ::update-elem eid
                   (fn [elem]
                     (prn "update elem" eid)
                     (assoc-in elem
                               [:instance/args
                                :element/code]
                               data))))
      (catch Exception e
        (clojure.pprint/pprint e)))))


(defui instance-detail-editor [{:keys [elem]}]
  (let [editing? (get extra :editing?)]
    (if editing?
      (let [buf (get extra :buf)
            buf (or buf
                    (buffer/buffer (with-out-str
                                     (clojure.pprint/pprint (-> elem
                                                                :instance/args
                                                                :element/code)))
                                   {:rows 40 :cols 15
                                    :mode :insert}))]
        (ui/vertical-layout
         (basic/button {:text "done"
                        :on-click (fn []
                                    [[:set $editing? false]
                                     [::update-instance (:element/id elem) buf]
                                     [:delete $buf]])})
         (code-editor/text-editor {:buf buf})))
      
      ;; else
      (ui/vertical-layout
       (basic/button {:text "edit"
                      :on-click (fn []
                                  [[:set $editing? true]])})
       (viscous/inspector {:obj (viscous/wrap elem)}))
      )))






(defeffect ::update-elem [eid f & args]
  (dispatch! :update
             (specter/path [:root WALK-ELEM #(= eid (:element/id %))])
             #(apply f % args)))

(defeffect ::add-let-binding [eid]
  (dispatch! ::update-elem
             eid
             (fn [elem]
               (update elem :element/bindings conj '[a {:element/type ::code
                                                        :element/code 42}]))))
(defeffect ::delete-elem [eid]
  (dispatch! :delete
             (specter/path [:root WALK-ELEM #(= eid (:element/id %))])))

(defeffect ::update-let-binding [eid i binding-str code-str]
  (future
    (try
      (let [
            binding
            (binding [*ns* eval-ns]
              (read-string binding-str))
            code
            (binding [*ns* eval-ns]
              (read-string code-str))]
        (dispatch! ::update-elem
                   eid
                   (fn [elem]
                     (->> elem
                          (specter/setval [:element/bindings
                                           (specter/nthpath i)
                                           (specter/nthpath 0)]
                                          binding)
                          (specter/setval [:element/bindings
                                           (specter/nthpath i)
                                           (specter/nthpath 1)
                                           :element/code]
                                          code)))))
      (catch Exception e
        (clojure.pprint/pprint e))))
  )

(defeffect ::wrap-for [eid]
  (dispatch! ::update-elem eid
             (fn [elem]
               {:element/type ::for
                :element/body elem
                :element/id (random-uuid)
                :element/layout :vertical
                :element.for/x 'x
                :element.for/xs {:element/type ::code
                                 :element/code
                                 `(range 3)}})))


(defeffect ::wrap-group [eid]
  (dispatch! ::update-elem eid
             (fn [elem]
               {:element/type ::group
                :element/children
                [elem]
                :element/id (random-uuid)})))


(defeffect ::wrap-component [eid]
  (dispatch! ::update-elem eid
             (fn [elem]
               {:element/type ::prototype
                :prototype/component
                {:element/type ::define
                 :element/id (random-uuid)
                 ::id eid
                 ::value {:element/type ::component
                          :component/name 'my-component
                          :component/args []
                          :component/body elem}}
                 :prototype/args {:element/type ::code
                                  :element/code {}}
                 :element/id (random-uuid)})))


(defeffect ::wrap-translate [eid]
  (dispatch! ::update-elem eid
             (fn [elem]
               {:element/type ::translate
                :element/id (random-uuid)
                :element/x 0
                :element/y 0
                :element/body elem}
               )))

(defui property-detail-editor [{:keys [elem properties]}]
  (let [eid (:element/id elem)]
    (apply
     ui/vertical-layout
     (for [k properties
           :let [editing? (get extra [:editing? eid k])]]
       (if editing?
         (ui/on
          ::save-binding-edit
          (fn [_ code-str]
            [[:set $editing? false]
             [::update-elem-key eid k code-str]])
          ::cancel-binding-edit
          (fn []
            [[:set $editing? false]])
          (binding-editor {:binding (name k)
                           :code (get elem k)
                           :extra (get extra [:binding eid k])}))
         (ui/horizontal-layout
          (ui/on
           :mouse-down
           (fn [_]
             [[:set $editing? true]])
           (ui/horizontal-layout
            (ui/label k)
            (ui/label (get elem k))))))))))

(defui binding-editor [{:keys [binding code]}]
  (let [binding-buf (get extra :binding-buffer)
        binding-buf (or binding-buf
                        (buffer/buffer
                         (with-out-str
                           (clojure.pprint/pprint binding))
                         {:rows 40 :cols 15
                          :mode :insert}))
        code-buf (get extra :code-buf)
        code-buf (or code-buf
                     (buffer/buffer
                      (with-out-str
                        (clojure.pprint/pprint code))
                      {:rows 40 :cols 15
                       :mode :insert}))]
    (ui/horizontal-layout
     (basic/button {:text "X"
                    :on-click (fn []
                                [[::cancel-binding-edit]])})
     (basic/button {:text "save"
                    :on-click (fn []
                                [[::save-binding-edit
                                  (buffer/text binding-buf)
                                  (buffer/text code-buf)]])})
     (code-editor/text-editor {:buf binding-buf})
     (code-editor/text-editor {:buf code-buf}))))

(defui let-detail-editor [{:keys [elem]}]
  (let [eid (:element/id elem)]
    (apply
     ui/vertical-layout
     (basic/button {:text "+"
                    :on-click
                    (fn []
                      [[::add-let-binding eid]])})
     (for [[i binding] (map-indexed vector (:element/bindings elem))
           :let [editing? (get extra [:editing? eid $binding])]]
       (if editing?
         (ui/on
          ::save-binding-edit
          (fn [binding-str code-str]
            [[:set $editing? false]
             [::update-let-binding eid i binding-str code-str]])
          ::cancel-binding-edit
          (fn []
            [[:set $editing? false]])
          (binding-editor {:binding (nth binding 0)
                           :code (:element/code (nth binding 1))}))
         (ui/horizontal-layout
          (basic/button {:text "X"
                         :hover? (get extra [eid i :hover?])
                         :on-click
                         (fn []
                           [[::delete-let-binding eid i ]])})
          (ui/on
           :mouse-down
           (fn [_]
             [[:set $editing? true]])
           (ui/horizontal-layout
            (ui/label (nth binding 0))
            (ui/label (:element/code (nth binding 1))))))))))
  )

(defeffect ::update-elem-key [eid k txt]
  (future
    (try
      (let [data
            (binding [*ns* eval-ns]
              (read-string txt))]
        (dispatch! ::update-elem eid
                   (fn [elem]
                     (assoc elem k data))))
      (catch Exception e
        (clojure.pprint/pprint e))))
  )

(defeffect ::update-translate [eid buf]
  (future
    (try
      (let [txt (buffer/text buf)
            data
            (binding [*ns* eval-ns]
              (read-string txt))]
        (dispatch! ::update-elem eid
                   (fn [elem]
                     (assoc elem
                            :element/x (first data)
                            :element/y (second data)))))
      (catch Exception e
        (clojure.pprint/pprint e)))))

(defui translate-detail-editor [{:keys [elem]}]
  (let [editing? (get extra :editing?)]
    (if editing?
      (let [buf (get extra :buf)
            buf (or buf
                    (buffer/buffer (with-out-str
                                     (clojure.pprint/pprint
                                      ((juxt :element/x :element/y)
                                       elem)))
                                   {:rows 40 :cols 15
                                    :mode :insert}))]
        (ui/vertical-layout
         (basic/button {:text "done"
                        :on-click (fn []
                                    [[:set $editing? false]
                                     [::update-translate (:element/id elem) buf]
                                     [:delete $buf]]
                                    )})
         (code-editor/text-editor {:buf buf})))
      
      ;; else
      (ui/vertical-layout
       (basic/button {:text "edit"
                      :on-click (fn []
                                  [[:set $editing? true]])})
       (viscous/inspector {:obj (viscous/wrap elem)}))
      ))
  )




(defeffect ::update-prototype [eid buf]
  (future
    (try
      (let [txt (buffer/text buf)
            data
            (binding [*ns* eval-ns]
              (read-string txt))]
        (dispatch! ::update-elem eid
                   (fn [elem]
                     (assoc-in elem
                               [:prototype/component
                                ::value
                                :component/args]
                               data))))
      (catch Exception e
        (clojure.pprint/pprint e)))))

(defui prototype-detail-editor [{:keys [elem]}]
  (let [editing? (get extra :editing?)]
    (if editing?
      (let [buf (get extra :buf)
            buf (or buf
                    (buffer/buffer (with-out-str
                                     (clojure.pprint/pprint
                                      (-> elem
                                          :prototype/component
                                          ::value
                                          :component/args)))
                                   {:rows 40 :cols 15
                                    :mode :insert}))]
        (ui/vertical-layout
         (basic/button {:text "done"
                        :on-click (fn []
                                    [[:set $editing? false]
                                     [::update-prototype (:element/id elem) buf]
                                     [:delete $buf]]
                                    )})
         (code-editor/text-editor {:buf buf})))
      
      ;; else
      (ui/vertical-layout
       (basic/button {:text "edit"
                      :on-click (fn []
                                  [[:set $editing? true]])})
       (viscous/inspector {:obj (viscous/wrap elem)}))
      ))
  )

(defui for-detail-editor [{:keys [elem]}]
  (let [eid (:element/id elem)]
    (apply
     ui/vertical-layout
     (for [k [:element.for/x :element.for/xs]
           :let [editing? (get extra [:editing? eid k])]]
       (if editing?
         (ui/on
          ::save-binding-edit
          (fn [_ code-str]
            [[:set $editing? false]
             [::update-elem-key eid k code-str]])
          ::cancel-binding-edit
          (fn []
            [[:set $editing? false]])
          (binding-editor {:binding (name k)
                           :code (get elem k)}))
         (ui/horizontal-layout
          (ui/on
           :mouse-down
           (fn [_]
             [[:set $editing? true]])
           (ui/horizontal-layout
            (ui/label k)
            (ui/label (get elem k)))))))))
  )



(defui paragraph-detail-editor [{:keys [elem]}]
  (property-detail-editor {:elem elem
                           :properties [:element/text
                                        :element/width]}))

(defui detail-editor [{:keys [elem]}]
  (ui/vertical-layout
   (ui/horizontal-layout
    (basic/button {:text "delete"
                   :on-click (fn []
                               [[::delete-elem (:element/id elem)]])})
    (basic/button {:text "componentize"
                   :on-click (fn []
                               [[::wrap-component (:element/id elem)]])})
    (basic/button {:text "translate"
                   :on-click (fn []
                               [[::wrap-translate (:element/id elem)]])})
    (basic/button {:text "groupize"
                   :on-click
                   (fn []
                     [[::wrap-group (:element/id elem)]])})
    (basic/button {:text "for"
                   :on-click
                   (fn []
                     [[::wrap-for (:element/id elem)]])}))
   (case (:element/type elem)
     ::let (let-detail-editor {:elem elem})
     ::instance (instance-detail-editor {:elem elem})
     ::translate (translate-detail-editor {:elem elem})
     ::prototype (prototype-detail-editor {:elem elem})
     ::for (for-detail-editor {:elem elem})
     ::paragraph (paragraph-detail-editor {:elem elem})
     ;; else
     (viscous/inspector {:obj (viscous/wrap elem)})))
  )

(defui main-view [{:keys [root selection collapsed interns]}]
  (let [[cw ch :as size] (:membrane.stretch/container-size context)]
    (dnd/drag-and-drop
     {:$body nil
      :body
      (stretch/hlayout
       [[(basic/scrollview {:$body nil
                            :body
                            (ui/on
                             ::select
                             (fn [eid]
                               [[:update $selection stoggle eid]])

                             ::toggle-collapse
                             (fn [eid]
                               [[:update $collapsed stoggle eid]])
                             (ui/vertical-layout
                              (ui/horizontal-layout
                               (basic/button {:text "clear selection"
                                              :on-click (fn []
                                                          [[:set $selection #{}]])})
                               (basic/button {:text "debug"
                                              :on-click (fn []
                                                          (init!)
                                                          nil)}))
                              (tree-view {:branch? element-branch?
                                          :children element-children
                                          :selection selection
                                          :collapse? collapsed
                                          :root root})))
                            :scroll-bounds [400 ch]})
         (ui/with-style ::ui/style-stroke
           (ui/with-color [0 0 0]
             (ui/rectangle 400 ch)))]
        (stretch/with-container-size [(max 0
                                           (- cw 400 400))
                                      ch]
          (basic/scrollview
           {:scroll-bounds
            [(max 0
                  (- cw 400 400))
             ch]
            :$body nil
            :body
            (rectangle-editor {:root root
                               :interns interns
                               :selection selection})}))

        (when (> (- cw 400)
                 400)
          (stretch/with-container-size [400
                                        ch]
            (ui/bordered
             (ui/fixed-bounds
              [400 ch]
              (when-let [eid (first selection)]
                
                (when-let [elem (->> (tree-seq
                                      element-branch?
                                      element-children
                                      root)
                                     (filter #(= eid (:element/id %)))
                                     first)]
                  (ui/on
                   ::delete-elem
                   (fn [eid]
                     [[::delete-elem eid]
                      [:update $selection disj eid]])
                   (detail-editor {:elem elem}))))
              ))))])})))

(defn show! []
  (swap! app-state
         (fn [s]
           (if s
             s
             initial-state)))
  (skia/run (component/make-app #'main-view
                                app-state)
    {:include-container-info true}
    )
  )



(comment


  ,)


{:element/type ::code
 :element/name "delete-X"
 :element/code '(ui/with-color
                  [1 0 0]
                  (ui/with-stroke-width
                    3
                    [(ui/path [0 0]
                              [10 10])
                     (ui/path [10 0]
                              [0 10])]))}





(defmethod compile* ::prototype [{:prototype/keys [component args]}]
  `(~(compile component)
    ~(compile args)))

(defmacro define [id value]
  `(let [value# ~value]
     (swap! app-state assoc-in [:interns ~id] value#)
     value#))

(defmethod compile* ::define [{:keys [::id ::value]}]
  `(define ~id ~(compile value)))

(defmethod compile* ::reference [{:keys [::id]}]
  `(get ~'interns ~id))

(defmethod compile* ::component [{:keys [component/name
                                         component/args
                                         component/body]}]
  
  `(fn ~(symbol
         (clojure.core/name name))
     [{:keys [~@(eduction
                 (map symbol)           
                 args)]}]
     ~(compile body)))

(defmethod compile* ::code [{:element/keys [code]}]
  code)

(defmethod compile* ::instance [{:instance/keys [component args]}]
  `(~(compile component)
    ~(compile args)))




