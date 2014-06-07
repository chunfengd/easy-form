(ns easy-form.core-test
  (:require
   [midje.sweet :refer :all]
   [clojure.string :as str])
  (:require
   [easy-form.core :refer :all]))

(fact
 "test demo"
 (str/split "a/b/c" #"/") => ["a" "b" "c"])

(facts
 "parse single element"
 (fact "basic"
       (parse-form [:text.username])
       =>
       (just {:tag :text
              :id :username
              :name "username"
              :children ()
              :gid :username
              :label "Username"}))
 (fact "extra option"
       (parse-form [:text.username {:extra "added"}])
       =>
       (just {:tag :text
              :id :username
              :name "username"
              :children ()
              :gid :username
              :label "Username"
              :extra "added"}))
 (fact "new id"
       (parse-form [:text.username {:id :new-id}])
       =>
       (just {:tag :text
              :id :new-id
              :name "new-id"
              :children ()
              :gid :new-id
              :label "New-id"}))
 (fact "customized label"
       (parse-form [:text.username
                    {:name "new-name"
                     :label "Username"}])
       =>
       (just {:tag :text
              :id :username
              :name "username"
              :children ()
              :gid :username
              :label "Username"})))

#_(fact "form with children"
        (parse-form [:group.user {:level 1}
                     :text.username
                     (list
                      :number.age)
                     [:empty {:id "added"}]
                     [:group.account
                      :number.id]]))
