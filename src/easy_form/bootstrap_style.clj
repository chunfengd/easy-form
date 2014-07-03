(ns easy-form.bootstrap-style
  (:require [easy-form.core :refer :all]))

(defn create-horizontal-style [label-width input-width]
  {:default (fn [{:keys [tag name label attr] :as item} style
                 & {:keys [values params readonly]}]
              [:div.form-group
               [:label {:class (str "control-label "
                                    "col-sm-" label-width)
                        :for name}
                label]
               [:div {:class (str "col-sm-" input-width)}
                [:input.form-control
                 (merge {:type tag
                         :id name
                         :name name
                         :value (get-value item values params)
                         :readonly readonly}
                        attr)]]])
   :hidden (fn [{:keys [name label attr] :as item} style
                & {:keys [values params readonly]}]
             [:input
              (merge {:type :hidden
                      :id name
                      :name name
                      :value (get-value item values params)}
                     attr)])
   :html (fn [{:keys [body]} _ & _] body)
   :group (fn [{:keys [name label children]} style
               & {:keys [values params readonly]}]
            [:field-set {:id name}
             [:h3 label]
             (render-form children style
                          :values values
                          :params params
                          :readonly readonly)])
   :form (fn [{:keys [children attr]} style
              & {:keys [values params readonly]}]
           [:form.form-horizontal attr
            (render-form children style
                         :values values
                         :params params
                         :readonly readonly)])
   :submit (fn [{:keys [label readonly-label readonly-url]} style
                & {:keys [values params readonly]}]
             (if readonly
               [:div.form-group
                [:div {:class (str "col-sm-offset-" label-width " "
                                   "col-sm-" input-width)}
                 [:a.btn.btn-primary {:href readonly-url}
                  readonly-label]]]
               [:div.form-group
                [:div {:class (str "col-sm-offset-" label-width " "
                                   "col-sm-" input-width)}
                 [:button.btn.btn-primary {:type :submit}
                  label]]]))
   :radio (fn [{:keys [name label options attr] :as item} style
               & {:keys [values params readonly]}]
            [:div.form-group
             [:label {:class (str "control-label "
                                  "col-sm-" label-width)
                      :for name}
              label]
             [:div {:class (str "col-sm-" input-width)}
              (for [{option-value :value option-label :name} options]
                [:label.radio-inline
                 [:input
                  (merge {:type :radio
                          :name name
                          :value option-value
                          :checked (= option-value
                                      (get-value item values params))
                          :disabled (and readonly
                                         (not= option-value
                                               (get-value item values params)))}
                         attr)]
                 option-label])]])
   })
