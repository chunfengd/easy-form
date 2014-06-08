(ns easy-form.bootstrap-style
  (:require [easy-form.core :refer :all]))

(defn create-horizontal-style [label-width input-width]
  {:default (fn [{:keys [tag id name label]} style]
              [:div.form-group
               [:label {:class (str "control-label "
                                    "col-sm-" label-width)
                        :for name}
                label]
               [:div {:class (str "col-sm-" input-width)}
                [:input.form-control
                 {:type tag
                  :id name
                  :name name}]]])
   :group (fn [{:keys [tag id name label children]} style]
            [:field-set {:id name}
             [:h1 label]
             (render-form children style)])
   :form (fn [{:keys [tag id name children attr]} style]
           [:form.form-horizontal attr
            (render-form children style)])
   :submit (fn [{:keys [tag id name label]} style]
             [:div.form-group
              [:div {:class (str "col-sm-offset-" label-width " "
                                 "col-sm-" input-width)}
               [:button.btn.btn-primary {:type :submit}
                label]]])
   })
