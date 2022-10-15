(ns com.phronemophobic.membrane.figma.ads
  (:require [clojure.java.io :as io]

            [membrane.skia :as backend]
            [com.phronemophobic.membrane.schematic2 :as s2]
            [com.phronemophobic.membrane.figma :as figma]
            [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [membrane.component
             :refer [defui]]
            [clojure.zip :as z]
            [zippo.core :as zippo]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(defn preserve-meta [f]
  (fn [o]
    (with-meta (f o) (meta o))))

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

(defn search-find-all [obj pred]
  (zippo/loc-find-all (clj-zip obj)
                      (zippo/->loc-pred pred)))


(defn search-find [obj pred]
  (zippo/loc-find (clj-zip obj)
                  (zippo/->loc-pred pred)))

(defn search-some [obj pred]
  (loop [zip (clj-zip obj)]
    (when-not (z/end? zip)
      (if-let [match (pred (z/node zip))]
        match
        (recur (z/next zip))))))

(defn search-zip-walk [zip pred]
  (z/root
   (zippo/loc-update-all zip
                         #(z/edit % pred))))

(defn load-view []
  (def design-system-response
    (figma/get-files
     ;; "LX4ucdBEjBu3Bpnw95cJ3E" ;; tutorial design system
     ;; "lGvV046zHH9xwj8OMX2xZB" ;;material design free trial junk
     ;; "ukH49eAeYCjrszVHQ2npTa" ;; bootstrap
     ;; "pIOYSSejwqteM3A68PlF2s" ;; more material design junk
     ;; "82wr5v8a7NKbnjuOGxrHRv" ;; dashboard
     "sAEkytxl4mv5COV7unNcjV" ;; Atlassian design system! :D
     ))

  
  (def body design-system-response)

  (def document (get body "document"))

  (def view (-> document
                (get "children")
                (nth 6)
                figma/normalize-bounds
                figma/figma->membrane))
  (def views (-> document
                 (get "children")
                 (->> (map figma/normalize-bounds)
                      (map figma/figma->membrane))))

  (comment
    (spit "resources/ads-views.edn"
          (pr-str views))
    )

  (print "loaded")
  ,)

(defonce views
  (let [resource (io/resource "ads-views.edn")]
    (with-open [rdr  (io/reader resource)
                pbr (java.io.PushbackReader. rdr)]
      (read pbr))))


(def button-view (some #(when (and (= "CANVAS" (:type %))
                                   (= "Button" (:name %)))
                          %)
                       views))

(def buttons
  (->> (search-find-all
        button-view
        (fn [m]
          (= "COMPONENT_SET"
             (:type m))))
       (map z/node)
       (mapcat (fn [cset]
                 (let [style (figma/name->style (:name cset))
                       variants (search-keep-all
                                 cset
                                 (fn [m]
                                   (when-let [name (:name m)]
                                     (let [variant (figma/name->variant name)]
                                       (when (and (= false (:loading variant))
                                                  (= :none (:icon variant)))
                                         m)))))]
                   (->>
                    variants
                    (map (fn [m]
                           (let [params (-> (figma/name->variant (:name m))
                                            (dissoc :loading :icon)
                                            (assoc :style style))]
                             (assoc m :figma/parameters params))))))))))

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
                 
                 (map (juxt :figma/parameters figma/->ast))
                 (map (fn [[k m]]
                        [k
                         (-> (search-find m :element/text)
                             
                             (z/edit assoc :element/text 'text)
                             z/root)]))
                 (map (fn [[k m]]
                        [k (dissoc m
                                   :element/x
                                   :element/y)])))
           buttons)}]})

(-> schematic-button-component
    com.phronemophobic.membrane.schematic2/export-component
    eval)

(def button-table (into []
                        (comp
                         (map (fn [m]
                                (with-meta m
                                  {:id (:id m)
                                   :figma m})))
                         (map (preserve-meta figma/->ast))
                         (map (preserve-meta #(dissoc %
                                                      :element/x
                                                      :element/y)))
                         (map (preserve-meta s2/compile))
                         (map (preserve-meta eval))
                         (map (fn [view]
                                (let [{:element/keys [x y]} (-> view meta :figma figma/->ast)]
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
(comment
  (backend/run (membrane.component/make-app #'figma/figma-viewer {:view button-table}))
  ,)



;; checkboxes
(def checkbox-view (some #(when (and (= "CANVAS" (:type %))
                                     (= "Checkbox" (:name %)))
                            %)
                         views))
(def checkboxes
  (->> (search-find-all
        checkbox-view
        (fn [m]
          (= "COMPONENT_SET"
             (:type m))))
       (map z/node)
       (mapcat (fn [cset]
                 (let [style (figma/name->style (:name cset))
                       variants (search-keep-all
                                 cset
                                 (fn [m]
                                   (when-let [name (:name m)]
                                     (let [variant (figma/name->variant name)]
                                       (when (and (seq variant)
                                                  (= false (:required variant)))
                                         m)))))]
                   (->>
                    variants
                    (map (fn [m]
                           (let [params (-> (figma/name->variant (:name m))
                                            (dissoc :required)
                                            (assoc :style style))]
                             (assoc m :figma/parameters params))))))))))

(def cb-table (into []
                    (comp
                     (map (fn [m]
                            (with-meta m
                              {:id (:id m)
                               :figma m})))
                     (map (preserve-meta figma/->ast))
                     (map (preserve-meta #(dissoc % :element/x :element/y)))
                     (map (preserve-meta s2/compile))
                     (map (preserve-meta eval))
                     (map (fn [view]
                            (let [{:element/keys [x y]} (-> view meta :figma figma/->ast)]
                              (ui/translate 
                               x y
                               (ui/on
                                :mouse-down
                                (fn [_]
                                  (tap> (-> view meta :figma
                                            :figma/parameters))
                                  nil)
                                (ui/try-draw 
                                 view
                                 (fn [draw e]
                                   (prn e)
                                   (draw (ui/label (str (:id (meta view))"!!!")))))))))))
                    checkboxes))



(comment
  (backend/run (membrane.component/make-app #'figma/figma-viewer {:view cb-table})))

(def schematic-checkbox-component
  {:component/defaults {:checked false
                        :invalid false
                        :indeterminate false
                        :state :default
                        :style :default}
   :element/type :element/component
   :element/name "figma-checkbox"
   :element/children
   [{:element/type :flow-control/case
     :flow-control.case/expression '{:checked checked
                                     :invalid invalid
                                     :indeterminate indeterminate
                                     :state state
                                     :style style}
     :flow-control.case/clauses
     (into {}
           (comp (map (fn [m]
                        (assoc m :type "GROUP")))
                 (map #(dissoc % :name))
                 
                 (map (juxt :figma/parameters figma/->ast))
                 (map (fn [[k m]]
                        [k
                         (-> (search-find m :element/text)
                             (z/replace ::delete)
                             z/root)]))
                 (map (fn [[k m]]
                        [k (dissoc m :element/x :element/y)])))
           checkboxes)}]})

(-> schematic-checkbox-component
    com.phronemophobic.membrane.schematic2/export-component
    eval)

(defui figma-checkbox-with-hover [{:keys [checked? invalid? indeterminate? state hover?]
                                   :or {checked? false
                                        invalid? false
                                        indeterminate? false
                                        state :default}
                                   }]
  (ui/on
   :mouse-down
   (fn [_]
     [[::basic/toggle $checked?]])
   (basic/on-hover {:hover? hover?
                    :body
                    (figma-checkbox {:state (if hover?
                                              :hover
                                              state)
                                     :checked checked?
                                     :invalid invalid?
                                     :style :default
                                     :indeterminate indeterminate?})})))

(defui checkbox-app [{:keys []}]
  (let [all-cbs
        (for [{:keys [invalid
                      indeterminate
                      state]}
              (->> (search-some schematic-checkbox-component
                                :flow-control.case/clauses)
                   keys
                   (remove #(= (:state %) :hover))
                   (remove #(:checked %))
                   distinct)]
          (let [checked (get extra [:checked? [invalid indeterminate state]]
                             false)]
            (figma-checkbox-with-hover
             {:state state
              :indeterminate? indeterminate
              :invalid? invalid
              ;; :hover? (get extra [:hover? [checked invalid indeterminate state]])
              :checked? checked})))]
    (ui/table-layout
     (partition-all
      (int (Math/sqrt (count all-cbs)))
      all-cbs)
     8 8)))

(comment
  (backend/run
    (membrane.component/make-app #'checkbox-app))
  ,)

(def parts-view
  (some #(when (and (= "CANVAS" (:type %))
                    (= "​🧱 Parts" (:name %)))
           %)
        views))

(def dropdown-items
  (->> (search-keep-all
        parts-view
        (fn [m]
          (when (and (= "COMPONENT_SET"
                        (:type m))
                     (= "Dropdown Item"
                        (:name m)))
            m)))
       (mapcat (fn [cset]
                 (let [variants (search-keep-all
                                 cset
                                 (fn [m]
                                   (when-let [name (:name m)]
                                     (let [variant (figma/name->variant name)]
                                       (when (seq variant)
                                         m)))))]
                   (->>
                    variants
                    (map (fn [m]
                           (let [params (figma/name->variant (:name m))]
                             (assoc m :figma/parameters params))))))))))

(def dropdown-item-table
  (let [dropdown-items
        (into []
              (comp
               (map (fn [m]
                      (with-meta m
                        {:id (:id m)
                         :figma m})))
               (map (preserve-meta figma/->ast))
               (map (preserve-meta #(dissoc % :element/x :element/y)))
               (map (preserve-meta s2/compile))
               (map (preserve-meta eval))
               (map (fn [view]
                      (let [{:element/keys [x y]} (-> view meta :figma figma/->ast)]
                        (ui/on
                         :mouse-down
                         (fn [_]
                           (tap> (-> view meta :figma
                                     :figma/parameters))
                           nil)
                         (ui/try-draw 
                          view
                          (fn [draw e]
                            (prn e)
                            (draw (ui/label (str (:id (meta view))"!!!"))))))))))
              dropdown-items)]
    (ui/table-layout (partition-all 2 dropdown-items)
                     5 5)))

(comment
  (backend/run (membrane.component/make-app #'figma/figma-viewer {:view dropdown-item-table}))
  ,)

(def schematic-dropdown-item-component
  {:component/defaults {:state :default
                        :width 360
                        :height 32
                        :text "Option"}
   :element/type :element/component
   :element/name "figma-dropdown-item"
   :element/children
   [{:element/type :flow-control/case
     :flow-control.case/expression '{:state state}
     :flow-control.case/clauses
     (into {}
           (comp (map (fn [m]
                        (assoc m :type "GROUP")))
                 (map #(dissoc % :name))
                 (map figma/->ast)
                 (map (fn [m]
                        (-> (search-find m (fn [m]
                                             (= "Option"
                                                (:element/text m))))
                            (z/edit assoc :element/text 'text)
                            z/root)))
                 (map (fn [m]
                        (dissoc m
                                :element/x
                                :element/y)))
                 (map (fn [m]
                        (assoc m
                               :element/width 'width
                               :element/height 'height)))
                 (map (juxt #(-> % :figma/parameters (select-keys [:state]))
                            identity)))
           dropdown-items)}]})

(-> schematic-dropdown-item-component
    com.phronemophobic.membrane.schematic2/export-component
    eval)

(defui figma-dropdown-item-with-hover [{:keys [text state hover?
                                               width height]
                                        :or {state :default
                                             width 360
                                             height 32}}]
  (let [width (or width 360)
        height (or height 32)]
   (ui/on
    :mouse-down
    (fn [_]
      [[::select text]
       [:set $hover? false]])
    (ui/on
     :set
     (fn [$ref value]
       [[:set $ref value]])
     (basic/on-hover {:hover? hover?
                      :body
                      (figma-dropdown-item
                       {:state (if hover?
                                 :hover
                                 state)
                        :width width
                        :height height
                        :text text})})))))

(defui dropdown-item-app [{:keys []}]
  (let [all-dropdown-items
        (for [{:keys [state]}
              (->> (search-some schematic-dropdown-item-component
                                :flow-control.case/clauses)
                   keys
                   (remove #(= (:state %) :hover))
                   distinct)]
          (figma-dropdown-item-with-hover
           {:state state
            :width 200
            :hover? (get extra [:hover? state])
            :text state}))]
    (ui/table-layout
     (partition-all
      (int (Math/sqrt (count all-dropdown-items)))
      all-dropdown-items)
     8 8)))

(comment
  (backend/run
    (membrane.component/make-app #'dropdown-item-app))
  ,)


;; dropdown
(def dropdown-view
  (some #(when (and (= "CANVAS" (:type %))
                    (= "Dropdown menu " (:name %)))
           %)
        views))

(def dropdowns
  (->> (search-keep-all
        dropdown-view
        (fn [m]
          (when (and (= "COMPONENT_SET"
                        (:type m))
                     (= "Dropdown"
                        (:name m)))
            m)))
       (mapcat (fn [cset]
                 (let [variants (search-keep-all
                                 cset
                                 (fn [m]
                                   (when-let [name (:name m)]
                                     (let [variant (figma/name->variant name)]
                                       (when (seq variant)
                                         m)))))]
                   (->>
                    variants
                    (map (fn [m]
                           (let [params (figma/name->variant (:name m))]
                             (assoc m :figma/parameters params))))))))))

(def dropdown-table (into []
                          (comp
                           (map (fn [m]
                                  (with-meta m
                                    {:id (:id m)
                                     :figma m})))
                           (map (preserve-meta figma/->ast))
                           (map (preserve-meta #(dissoc % :element/x :element/y)))
                           (map (preserve-meta s2/compile))
                           (map (preserve-meta eval))
                           (map (fn [view]
                                  (let [{:element/keys [x y]} (-> view meta :figma figma/->ast)]
                                    (ui/translate 
                                     x y
                                     (ui/on
                                      :mouse-down
                                      (fn [_]
                                        (tap> (-> view meta :figma
                                                  :figma/parameters))
                                        nil)
                                      (ui/try-draw 
                                       view
                                       (fn [draw e]
                                         (prn e)
                                         (draw (ui/label (str (:id (meta view))"!!!")))))))))))
                          dropdowns))



(comment

  (backend/run (membrane.component/make-app #'figma/figma-viewer {:view dropdown-table}))

  ,)

(def schematic-dropdown-component
  {:component/defaults {:open false
                        :state :default
                        :selected nil
                        :options ["a" "b" "c"]
                        :text "Options"
                        :loading false}
   :element/type :element/component
   :element/name "figma-dropdown-raw"
   :element/children
   [{:element/type :flow-control/case
     :flow-control.case/expression '{:open open
                                     :state state
                                     :dropdown (if loading
                                                 :loading
                                                 :default)}
     :flow-control.case/clauses
     (into {}
           (comp (map (fn [m]
                        (assoc m :type "GROUP")))
                 (map #(dissoc % :name))
                 (map (juxt :figma/parameters figma/->ast))
                 (map (fn [[k m]]
                        [k (-> (search-find m #{"Choices"})
                               (z/replace 'text)
                               z/root)]))

                 (map (fn [[k m]]
                        (let [zmenu (search-find m #(when-let [name (:element/name %)]
                                                      (clojure.string/includes? name "Dropdown Menu")))]
                          [k (if-not zmenu
                               m
                               (-> zmenu
                                   (z/edit (fn [x]
                                             (-> x
                                                 (assoc :element/for-bindings '[option options])
                                                 (assoc :element/children
                                                        [{:element/type :element/instance
                                                          :instance/args {:text 'option
                                                                          :state '(if (= selected $option)
                                                                                    :selected
                                                                                    :default)}
                                                          :instance/fn `figma-dropdown-item-with-hover}])
                                                 (assoc :element/strokes [#:element{:color [0.933
                                                                                            0.933
                                                                                            0.933
                                                                                            1.0],
                                                                                    :fill-type "SOLID"}]))
                                             ))
                                   z/root))])))
                 (map (fn [[k m]]
                        [k (dissoc m :element/x :element/y)])))
           dropdowns)}]})



(-> schematic-dropdown-component
    com.phronemophobic.membrane.schematic2/export-component
    eval)

(defui figma-dropdown [{:keys [text open selected options state hover?]}]
  (let [state (or state :default)
        body
        (figma-dropdown-raw {:open open
                             :text (or selected text)
                             :selected selected
                             :options options
                             :state (if (and hover?
                                             (not open))
                                      :hover
                                      state)})]
    (ui/wrap-on
     :mouse-down
     (fn [handler pos]
       (conj (handler pos)
             [:update $open not]))
     (ui/on
      ::select
      (fn [option]
        [[:set $selected option]])
      (basic/on-hover
       {:hover? hover?
        :body body})))))

(s/def :dropdown/open boolean?)
(s/def :dropdown/state #{:default :disabled :hover :active})
(s/def :dropdown/options (s/* string?))
(s/def :dropdown/text string?)

(def dropdown-variants
  (->> dropdowns
       (map :figma/parameters)
       (map #(select-keys % [:open :state]))
       distinct
       (into #{})
       ))

(defn valid-variant? [m]
  (contains? dropdown-variants
             (select-keys m [:open :state])))

(s/def :dropdown/args
  (s/and (s/keys :req-un [:dropdown/options
                          :dropdown/text
                          :dropdown/open
                          :dropdown/state])
         valid-variant?))



(defui ui-pager [{:keys [n views]}]
  (let [n (or n 0)]
    (ui/vertical-layout
     (ui/horizontal-layout
      (ui/button "<" (fn []
                       [[:update $n #(max 0 (dec %))]]))
      
      (ui/button ">" (fn []
                       [[:update $n #(min (dec (count views)) (inc %))]]))
      (ui/label n))
     (nth views n))))

(comment
  (backend/run (membrane.component/make-app #'ui-pager
                                            {:n 0
                                             :views
                                             (map figma-dropdown
                                                  (gen/sample (s/gen :dropdown/args)
                                                              100))}))


  ,)

(defui dropdown-test [{:keys [open state]}]
  (figma-dropdown {:open open
                   :state state
                   :options ["a" "b" "c"]
                   :text "hello"}))

(defonce dropdown-state (atom {:open false
                               :state :default}))

(comment
  (backend/run (membrane.component/make-app #'dropdown-test dropdown-state))
  (swap! dropdown-state update :open not)

  ,)




(def textfield-view
  (some #(when (and (= "CANVAS" (:type %))
                    (= "Text field" (:name %)))
           %)
        views))



(def textfields
  (->> (search-keep-all
        textfield-view
        (fn [m]
          (when (and (= "COMPONENT_SET"
                        (:type m))
                     (#{"Text field"
                        "Text field (subtle)"}
                      (:name m)))
            m)))
       (mapcat (fn [cset]
                 (let [style (figma/name->style (:name cset))
                       variants (search-keep-all
                                 cset
                                 (fn [m]
                                   (when-let [name (:name m)]
                                     (let [variant (figma/name->variant name)]
                                       (when (seq variant)
                                         (when (and (= :none (:element variant)))
                                           m))))))]
                   (->>
                    variants
                    (map (fn [m]
                           (let [params (-> (figma/name->variant (:name m))
                                            (dissoc :element)
                                            (assoc :style style))]
                             (assoc m :figma/parameters params))))))))))

(def textfield-table (into []
                           (comp
                            (map (fn [m]
                                   (with-meta m
                                     {:id (:id m)
                                      :figma m})))
                            (map (preserve-meta figma/->ast))
                            (map (preserve-meta #(dissoc % :element/x :element/y)))
                            (map (preserve-meta s2/compile))
                            (map (preserve-meta eval))
                            (map (fn [view]
                                   (let [{:element/keys [x y]} (-> view meta :figma figma/->ast)]
                                     (ui/translate 
                                      x y
                                      (ui/on
                                       :mouse-down
                                       (fn [_]
                                         (tap> (-> view meta :figma
                                                   :figma/parameters))
                                         nil)
                                       (ui/try-draw 
                                        view
                                        (fn [draw e]
                                          (prn e)
                                          (draw (ui/label (str (:id (meta view))"!!!")))))))))))
                           textfields))



(comment
  (backend/run (membrane.component/make-app #'figma/figma-viewer {:view textfield-table}))
  ,)



(defn variant-component-suffix [m]
  (let [ks (sort (keys m))]
    (->> ks
         (mapcat (juxt name (fn [k]
                              (let [v (get m k)]
                                (if (keyword? v)
                                  (name v)
                                  (str v))))))
         (clojure.string/join "-"))))



(def textfield-styles
  (into []
        (comp (map (fn [m]
                     (assoc m :type "GROUP")))
              (map #(dissoc % :name))
              
              
              (map figma/->ast)
              #_(map (fn [[k m]]
                       [k (-> (search-find m (fn [m]
                                               (and (= "Text" (:element/text m))
                                                    (= :element/label (:element/type m)))))
                              (z/edit #(assoc % :element/text 'text))
                              z/root)]))
              (map (fn [m]
                     (let [match (search-find m (fn [m]
                                                  (when-let [name (:element/name m)]
                                                    (.startsWith name "Caret"))))]
                       (if match
                         (-> match
                             (z/replace ::delete)
                             z/root)
                         m))))
              (map (fn [m]
                     (let [root m]
                       (-> (search-find m (fn [m]
                                            (and (= "Text" (:element/text m))
                                                 (= :element/label (:element/type m)))))
                           (z/edit
                            (fn [m]
                              (let [focus? (= (-> root :figma/parameters :state)
                                              :focus)]
                                {:element/type :element/instance
                                 ;; no way to pass color to text area currently
                                 ;; :element/color (-> m :element/fills first :element/color)
                                 :instance/args {:text 'text
                                                 :focus? focus?
                                                 :font (let [font (:element/font m)]
                                                         `(ui/font ~(:font/name font)
                                                                   ~(:font/size font)))
                                                 :border? false}
                                 :instance/fn `basic/textarea-view})
                              ))
                           z/root))))
              
              (map (fn [m]
                     (dissoc m :element/x :element/y)))
              (map (fn [m]
                     {:component/defaults {:text "text"}
                      :figma/parameters (:figma/parameters m)
                      :element/type :element/component
                      :element/name (str "textfield-style-"
                                         (variant-component-suffix
                                          (:figma/parameters m)))
                      :element/children [m]})))
        textfields))

(doseq [textfield-style-component textfield-styles]
  (-> textfield-style-component
      com.phronemophobic.membrane.schematic2/export-component
      eval))



(def schematic-textfield-component
  {:component/defaults {:state :default,
                        :placeholder false,
                        :compact false,
                        :text "My text" 
                        :style :default}
   :element/type :element/component
   :element/name "figma-textfield-raw"
   :element/children
   [{:element/type :flow-control/case
     :flow-control.case/expression '{:state state
                                     :placeholder placeholder
                                     :compact compact
                                     :style style}
     :flow-control.case/clauses
     (into {}
           (comp (map (fn [m]
                        {:instance/fn (symbol
                                       (-> *ns* ns-name name)
                                       (:element/name m))
                         :figma/parameters (:figma/parameters m)
                         :instance/args {:text 'text
                                         :focus? '(= style :focus)}
                         :element/type :element/instance}))
                 (map (juxt :figma/parameters identity)))
           textfield-styles)
     }]})

(-> schematic-textfield-component
    com.phronemophobic.membrane.schematic2/export-component
    eval)






(defui textfield-app [{:keys []}]
  (let [all-textfields
        (for [variant (map :figma/parameters textfields)]
          (figma-textfield-raw
           {:state (:state variant)
            :placeholder (:placeholder variant)
            :compact (:compact variant)
            :text (pr-str variant)
            :style (:style variant)}))]
    (ui/table-layout
     (partition-all
      (int (Math/sqrt (count all-textfields)))
      all-textfields)
     8 8)))

(comment

  (backend/run (membrane.component/make-app #'figma/figma-viewer {:view (textfield-app {})}) )
  ,)

(defui figma-textfield [{:keys [placeholder compact text style
                                hover?
                                ^:membrane.component/contextual focus]}]
  (let [style (or style :default)
        compact (or compact false)
        placeholder (or placeholder false)
        focused? (= focus $text)]
    (ui/on
     ::basic/request-focus
     (fn []
       [[:set $focus $text]])
     (ui/wrap-on
      :mouse-down
      (fn [handler pos]
        (let [intents (handler pos)]
          (if (seq intents)
            intents
            (when (not focused?)
              [[:set $focus $text]]))))
      (basic/on-hover
       {:hover? hover?
        :body
        (figma-textfield-raw {:state (if focused?
                                       :focus
                                       (if hover?
                                         :hover
                                         :default))
                              :placeholder placeholder
                              :compact compact
                              :style style})})))))


(defui test-textarea [{:keys [a b c]}]
  (ui/vertical-layout
   (figma-textfield {:text a})
   (figma-textfield {:text b})
   (figma-textfield {:text c})))

(comment
  (backend/run (membrane.component/make-app #'test-textarea {:a "a"
                                                             :b "b"
                                                             :c "c"}) )

  ,)



(declare document)


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

(comment
  (backend/run
    (membrane.component/make-app #'button-app))
  ,)




(comment
  ;; doesn't work because slider is scale horizontal
  ;; and there isn't a way to "scale" a shape like a line
  (def ranges-view
    (some #(when (and (= "CANVAS" (:type %))
                      (= "Range" (:name %)))
             %)
          views))

 (def ranges
   (->> (search-keep-all
         ranges-view
         (fn [m]
           (when (and (= "COMPONENT"
                         (:type m)))
             (let [name (:name m)
                   variant (figma/name->variant name)]
               (when (= (keyword "100%")
                        (:value variant))
                 (assoc m :figma/parameters
                        {:state (:state variant)}))))))))
 )




