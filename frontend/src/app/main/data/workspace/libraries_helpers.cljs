;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.data.workspace.libraries-helpers
  (:require
   [cljs.spec.alpha :as s]
   [app.common.spec :as us]
   [app.common.data :as d]
   [app.common.pages-helpers :as cph]
   [app.common.geom.point :as gpt]
   [app.common.pages :as cp]
   [app.util.text :as ut]))

(defonce empty-changes [[] []])

(declare generate-sync-page)
(declare generate-sync-shape)

(declare generate-sync-component-components)
(declare generate-sync-shape-and-children-components)
(declare generate-sync-shape-components)
(declare remove-component-and-ref)
(declare remove-ref)
(declare update-attrs)
(declare calc-new-pos)

(defn generate-sync-file
  "Generic method that given a type of asset will iterate through the file pages
  and call synchronize"
  [asset-type library-id state]

  (s/assert #{:colors :components :typographies} asset-type)
  (s/assert (s/nilable ::us/uuid) library-id)

  (let [library-items
        (if (nil? library-id)
          (get-in state [:workspace-data asset-type])
          (get-in state [:workspace-libraries library-id :data asset-type]))]

    (if (empty? library-items)
      empty-changes

      (loop [pages (vals (get-in state [:workspace-data :pages-index]))
             rchanges []
             uchanges []]
        (if-let [page (first pages)]
          (let [[page-rchanges page-uchanges]
                (generate-sync-page asset-type library-id library-items page)]
            (recur (next pages)
                   (d/concat rchanges page-rchanges)
                   (d/concat uchanges page-uchanges)))
          [rchanges uchanges])))))

(defn has-asset-reference-fn
  [asset-type library-id]
  (case asset-type
    :components
    (fn [shape] (and (some? (:component-id shape))
                     (= (:component-file shape) library-id)))

    :colors
    (fn [shape] (some
                 #(let [attr (name %)
                        attr-ref-id (keyword (str attr "-ref-id"))
                        attr-ref-file (keyword (str attr "-ref-file"))]
                    (and (get shape attr-ref-id)
                         (= library-id (get shape attr-ref-file))))
                 cp/color-sync-attrs))

    :typographies
    (fn [shape]
      (and (= (:type shape) :text)
           (->> shape
                :content
                ;; Check if any node in the content has a reference for the library
                (ut/some-node
                 #(and (some? (:typography-ref-id %))
                       (= library-id (:typography-ref-file %)))))))))

(defn generate-sync-page
  [asset-type library-id library-items page]
  (let [has-asset-reference? (has-asset-reference-fn asset-type library-id)
        linked-shapes (cph/select-objects has-asset-reference? page)]
    (loop [shapes (seq linked-shapes)
           rchanges []
           uchanges []]
      (if-let [shape (first shapes)]
        (let [[shape-rchanges shape-uchanges]
              (generate-sync-shape asset-type library-id library-items page shape)]
          (recur (next shapes)
                 (d/concat rchanges shape-rchanges)
                 (d/concat uchanges shape-uchanges)))
        [rchanges uchanges]))))

(defmulti generate-sync-shape (fn [type _ _ _ _ _] type))

(defmethod generate-sync-shape :components
  [_ library-id library-items page shape]
  (let [root-shape shape
        objects (:objects page)
        components library-items
        page-id nil
        component-id (:id page)
        reset-touched? false]
    (generate-sync-shape-and-children-components root-shape objects components page-id component-id reset-touched?)))

(defmethod generate-sync-shape :colors
  [_ library-id library-items page shape]
  (loop [attrs (seq cp/color-sync-attrs)
         roperations []
         uoperations []]
    (let [attr (first attrs)]
      (if (nil? attr)
        (if (empty? roperations)
          empty-changes
          (let [rchanges [{:type :mod-obj
                           :page-id (:id page)
                           :id (:id shape)
                           :operations roperations}]
                uchanges [{:type :mod-obj
                           :page-id (:id page)
                           :id (:id shape)
                           :operations uoperations}]]
            [rchanges uchanges]))
        (let [attr-ref-id (keyword (str (name attr) "-ref-id"))]
          (if-not (contains? shape attr-ref-id)
            (recur (next attrs)
                   roperations
                   uoperations)
            (let [color (get library-items (get shape attr-ref-id))
                  roperation {:type :set
                              :attr attr
                              :val (:value color)
                              :ignore-touched true}
                  uoperation {:type :set
                              :attr attr
                              :val (get shape attr)
                              :ignore-touched true}]
              (recur (next attrs)
                     (conj roperations roperation)
                     (conj uoperations uoperation)))))))))

(defmethod generate-sync-shape :typographies
  [_ library-id library-items page shape]
  (let [update-node (fn [node]
                      (if-let [typography (get library-items (:typography-ref-id node))]
                        (merge node (d/without-keys typography [:name :id]))
                        node))
        old-content (:content shape)
        new-content (ut/map-node update-node old-content)
        rchanges [{:type :mod-obj
                   :page-id (:id page)
                   :id (:id shape)
                   :operations [{:type :set
                                 :attr :content
                                 :val new-content}]}]
        lchanges [{:type :mod-obj
                   :page-id (:id page)
                   :id (:id shape)
                   :operations [{:type :set
                                 :attr :content
                                 :val old-content}]}]]
    (if (= new-content old-content)
      empty-changes
      [rchanges lchanges])))

;; ---- Create a new component ----

(defn make-component-shape
  "Clone the shape and all children. Generate new ids and detach
  from parent and frame. Update the original shapes to have links
  to the new ones."
  [shape objects]
  (let [update-new-shape (fn [new-shape original-shape]
                           (assoc new-shape :frame-id nil))

        ;; If one of the original shape children already was a component
        ;; instance, the 'instanceness' is copied into the new component,
        ;; and the original shape now points to the new component.
        update-original-shape (fn [original-shape new-shape]
                                (cond-> original-shape
                                  true
                                  (assoc :shape-ref (:id new-shape))

                                  (nil? (:parent-id new-shape))
                                  (assoc :component-id (:id new-shape)
                                         :component-file nil)

                                  (some? (:parent-id new-shape))
                                  (assoc :component-id nil
                                         :component-file nil)))]

    (cph/clone-object shape nil objects update-new-shape update-original-shape)))


;; ---- Synchronize shapes with components

(declare generate-sync-page-components)

(defn generate-sync-file-components
  "Generate changes to synchronize all shapes in current file that are linked
  to some component in the given library. All attributes of the components
  that have changed, and whose group have not been touched in the linked shape,
  will be copied to the shape. Any shape that is linked to a no-longer
  existent component will be detached."
  [state library-id]
  (let [components
        (if (nil? library-id)
          (get-in state [:workspace-data :components])
          (get-in state [:workspace-libraries library-id :data :components]))]
    (if (nil? components)
      [[] []]
      (loop [pages (seq (vals (get-in state [:workspace-data :pages-index])))
             rchanges []
             uchanges []]
        (let [page (first pages)]
          (if (nil? page)
            [rchanges uchanges]
            (let [[page-rchanges page-uchanges]
                  (generate-sync-page-components page library-id components)]
              (recur (next pages)
                     (d/concat rchanges page-rchanges)
                     (d/concat uchanges page-uchanges)))))))))


(defn generate-sync-page-components
  "Generate changes to synchronize all shapes in a particular page.
  Same considerations as above."
  [page library-id components]
  (let [objects       (get page :objects)
        linked-shapes (cph/select-objects #(and (some? (:component-id %))
                                                (= (:component-file %) library-id))
                                          page)]
    (loop [shapes (seq linked-shapes)
           rchanges []
           uchanges []]
      (let [shape (first shapes)]
        (if (nil? shape)
          [rchanges uchanges]
          (let [[shape-rchanges shape-uchanges]
                (generate-sync-shape-and-children-components shape
                                                             objects
                                                             components
                                                             (:id page)
                                                             nil
                                                             false)]
            (recur (next shapes)
                   (d/concat rchanges shape-rchanges)
                   (d/concat uchanges shape-uchanges))))))))


(defn generate-sync-library-components
  "Generate changes to synchronize all shapes inside components of the current
  file library, that are linked to other component in the given library.
  Same considerations as above."
  [state library-id]
  (let [components
        (if (nil? library-id)
          (get-in state [:workspace-data :components])
          (get-in state [:workspace-libraries library-id :data :components]))]
    (if (nil? components)
      empty-changes
      (loop [local-components (seq (vals (get-in state [:workspace-data :components])))
             rchanges []
             uchanges []]
        (let [local-component (first local-components)]
          (if (nil? local-component)
            [rchanges uchanges]
            (let [[comp-rchanges comp-uchanges]
                  (generate-sync-component-components
                    local-component library-id components)]
              (recur (next local-components)
                     (d/concat rchanges comp-rchanges)
                     (d/concat uchanges comp-uchanges)))))))))


(defn generate-sync-component-components
  "Generate changes to synchronize all shapes in a particular component.
  Same considerations as above."
  [local-component library-id components]
  (let [objects       (get local-component :objects)
        linked-shapes (filter #(and (some? (:component-id %))
                                    (= (:component-file %) library-id))
                              (vals objects))]
    (loop [shapes (seq linked-shapes)
           rchanges []
           uchanges []]
      (let [shape (first shapes)]
        (if (nil? shape)
          [rchanges uchanges]
          (let [[shape-rchanges shape-uchanges]
                (generate-sync-shape-and-children-components shape
                                                             objects
                                                             components
                                                             nil
                                                             (:id local-component)
                                                             false)]
            (recur (next shapes)
                   (d/concat rchanges shape-rchanges)
                   (d/concat uchanges shape-uchanges))))))))


(defn generate-sync-shape-and-children-components
  "Generate changes to synchronize one shape that is linked to a component,
  and all its children. If reset-touched? is false, same considerations as
  above. If it's true, all attributes of the component that have changed
  will be copied, and the 'touched' flags in the shapes will be cleared."
  [root-shape objects components page-id component-id reset-touched?]
  (let [all-shapes (cph/get-object-with-children (:id root-shape) objects)
        component (get components (:component-id root-shape))
        root-component (get-in component [:objects (:shape-ref root-shape)])]
    (loop [shapes (seq all-shapes)
           rchanges []
           uchanges []]
      (let [shape (first shapes)]
        (if (nil? shape)
          [rchanges uchanges]
          (let [[shape-rchanges shape-uchanges]
                (generate-sync-shape-components
                  shape
                  root-shape
                  root-component
                  component
                  page-id
                  component-id
                  reset-touched?)]
            (recur (next shapes)
                   (d/concat rchanges shape-rchanges)
                   (d/concat uchanges shape-uchanges))))))))

(defn generate-sync-shape-components
  "Generate changes to synchronize one shape that is linked to other shape
  inside a component. Same considerations as above about reset-touched?"
  [shape root-shape root-component component page-id component-id reset-touched?]
  (if (nil? component)
    (remove-component-and-ref shape page-id component-id)
    (let [component-shape (get (:objects component) (:shape-ref shape))]
      (if (nil? component-shape)
        (remove-ref shape page-id component-id)
        (update-attrs shape
                      component-shape
                      root-shape
                      root-component
                      page-id
                      component-id
                      reset-touched?)))))

(defn remove-component-and-ref
  [shape page-id component-id]
  [[(d/without-nils {:type :mod-obj
                     :id (:id shape)
                     :page-id page-id
                     :component-id component-id
                     :operations [{:type :set
                                   :attr :component-id
                                   :val nil}
                                  {:type :set
                                   :attr :component-file
                                   :val nil}
                                  {:type :set
                                   :attr :shape-ref
                                   :val nil}
                                  {:type :set-touched
                                   :touched nil}]})]
   [(d/without-nils {:type :mod-obj
                     :id (:id shape)
                     :page-id page-id
                     :component-id component-id
                     :operations [{:type :set
                                   :attr :component-id
                                   :val (:component-id shape)}
                                  {:type :set
                                   :attr :component-file
                                   :val (:component-file shape)}
                                  {:type :set
                                   :attr :shape-ref
                                   :val (:shape-ref shape)}
                                  {:type :set-touched
                                   :touched (:touched shape)}]})]])

(defn remove-ref
  [shape page-id component-id]
  [[(d/without-nils {:type :mod-obj
                     :id (:id shape)
                     :page-id page-id
                     :component-id component-id
                     :operations [{:type :set
                                   :attr :shape-ref
                                   :val nil}
                                  {:type :set-touched
                                   :touched nil}]})]
   [(d/without-nils {:type :mod-obj
                     :id (:id shape)
                     :page-id page-id
                     :component-id component-id
                     :operations [{:type :set
                                   :attr :shape-ref
                                   :val (:shape-ref shape)}
                                  {:type :set-touched
                                   :touched (:touched shape)}]})]])

(defn update-attrs
  "The main function that implements the sync algorithm."
  [shape component-shape root-shape root-component page-id component-id reset-touched?]

  ;; === Uncomment this to debug synchronization ===
  ;; (println "SYNC"
  ;;          "[C]" (:name component-shape)
  ;;          "->"
  ;;          (if page-id "[W]" ["C"])
  ;;          (:name shape))

  (let [; The position attributes need a special sync algorith, because we do
        ; not synchronize the absolute position, but the position relative of
        ; the container shape of the component.
        new-pos   (calc-new-pos shape component-shape root-shape root-component)
        pos-group (get cp/component-sync-attrs :x)
        touched   (get shape :touched #{})]

    (loop [attrs (seq (keys (dissoc cp/component-sync-attrs :x :y)))
           roperations (if (or (not (touched pos-group)) reset-touched? true)
                         [{:type :set :attr :x :val (:x new-pos)}        ; ^ TODO: the position-group is being set
                          {:type :set :attr :y :val (:y new-pos)}]       ; | as touched somewhere. Investigate why.
                         [])
           uoperations (if (or (not (touched pos-group)) reset-touched? true)
                         [{:type :set :attr :x :val (:x shape)}
                          {:type :set :attr :y :val (:y shape)}]
                         [])]

      (let [attr (first attrs)]
        (if (nil? attr)
          (let [roperations (if reset-touched?
                              (conj roperations
                                    {:type :set-touched
                                     :touched nil})
                              roperations)

                uoperations (if reset-touched?
                              (conj uoperations
                                    {:type :set-touched
                                     :touched (:touched shape)})
                              uoperations)

                rchanges [(d/without-nils {:type :mod-obj
                                           :id (:id shape)
                                           :page-id page-id
                                           :component-id component-id
                                           :operations roperations})]
                uchanges [(d/without-nils {:type :mod-obj
                                           :id (:id shape)
                                           :page-id page-id
                                           :component-id component-id
                                           :operations uoperations})]]
            [rchanges uchanges])

          (if-not (contains? shape attr)
            (recur (next attrs)
                   roperations
                   uoperations)
            (let [roperation {:type :set
                              :attr attr
                              :val (get component-shape attr)
                              :ignore-touched true}
                  uoperation {:type :set
                              :attr attr
                              :val (get shape attr)
                              :ignore-touched true}

                  attr-group (get cp/component-sync-attrs attr)]
              (if (or (not (touched attr-group)) reset-touched?)
                (recur (next attrs)
                       (conj roperations roperation)
                       (conj uoperations uoperation))
                (recur (next attrs)
                       roperations
                       uoperations)))))))))

(defn calc-new-pos
  [shape component-shape root-shape root-component]
  (let [root-pos           (gpt/point (:x root-shape) (:y root-shape))
        root-component-pos (gpt/point (:x root-component) (:y root-component))
        component-pos      (gpt/point (:x component-shape) (:y component-shape))
        delta              (gpt/subtract component-pos root-component-pos)
        shape-pos          (gpt/point (:x shape) (:y shape))
        new-pos            (gpt/add root-pos delta)]
    new-pos))

