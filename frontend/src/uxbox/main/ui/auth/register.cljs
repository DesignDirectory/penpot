;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns uxbox.main.ui.auth.register
  (:require
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [lentes.core :as l]
   [rumext.alpha :as mf]
   [uxbox.config :as cfg]
   [uxbox.main.ui.icons :as i]
   [uxbox.main.data.auth :as uda]
   [uxbox.main.store :as st]
   [uxbox.main.data.auth :as da]
   [uxbox.main.ui.messages :refer [messages]]
   [uxbox.main.ui.components.forms :refer [input submit-button form]]
   [uxbox.main.ui.navigation :as nav]
   [uxbox.util.dom :as dom]
   [uxbox.util.forms :as fm]
   [uxbox.util.i18n :refer [tr t]]
   [uxbox.util.router :as rt]))


(mf/defc demo-warning

  [_]

  [:div.msg-inline.warning.quick
   [:div.icon i/msg-warning]
    [:span.msg-text
     "WARNING: This is a service, DO NOT USE for real work, the projects will be periodicaly wiped."]
   [:div.close-button i/close]])

(s/def ::fullname ::fm/not-empty-string)
(s/def ::password ::fm/not-empty-string)
(s/def ::email ::fm/email)

(s/def ::register-form
  (s/keys :req-un [::password
                   ::fullname
                   ::email]))

(defn- on-error
  [form error]
  (case (:code error)
    :uxbox.services.mutations.profile/registration-disabled
    (st/emit! (tr "errors.registration-disabled"))

    :uxbox.services.mutations.profile/email-already-exists
    (swap! form assoc-in [:errors :email]
           {:message "errors.email-already-exists"})

    (st/emit! (tr "errors.unexpected-error"))))

(defn- validate
  [data]
  (let [password (:password data)]
    (when (> 8 (count password))
      {:password {:message "errors.password-too-short"}})))

(defn- on-submit
  [form event]
  (let [data (with-meta (:clean-data form)
               {:on-error (partial on-error form)})]
    (st/emit! (uda/register data))))

(mf/defc register-form
  [{:keys [locale] :as props}]
  [:& form {:on-submit on-submit
            :spec ::register-form
            :validators [validate]
            :initial {}}
   [:& input {:name :fullname
              :tab-index "1"
              :label (t locale "auth.fullname-label")
              :type "text"}]
   [:& input {:type "email"
              :name :email
              :tab-index "2"
              :help-icon i/at
              :label (t locale "auth.email-label")}]
   [:& input {:name :password
              :tab-index "3"
              :hint (t locale "auth.password-length-hint")
              :label (t locale "auth.password-label")
              :type "password"}]

   [:& submit-button
    {:label (t locale "auth.register-submit-label")}]])

;; --- Register Page

(mf/defc register-page
  [{:keys [locale] :as props}]
  [:section.generic-form
   [:div.form-container
    [:h1 (t locale "auth.register-title")]
    [:div.subtitle (t locale "auth.register-subtitle")]
    (when cfg/demo-warning
      [:& demo-warning])

    [:& register-form {:locale locale}]

    [:div.links
     [:div.link-entry
      [:span (t locale "auth.already-have-account") " "]
      [:a {:on-click #(st/emit! (rt/nav :auth-login))
           :tab-index "4"}
       (t locale "auth.login-here")]]

     [:div.link-entry
      [:span (t locale "auth.create-demo-profile-label") " "]
      [:a {:on-click #(st/emit! da/create-demo-profile)
           :tab-index "5"}
       (t locale "auth.create-demo-profile")]]]]])
