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

(facts
 "parse a item with children"
 (fact "simple children"
       (parse-form [:group.user
                    :text.name
                    [:number.id]])
       =>
       (just {:tag :group
              :id :user
              :gid :user
              :name "user"
              :label "User"
              :children
              (just {:tag :text
                     :id :name
                     :gid :user.name
                     :name "user.name"
                     :label "Name"
                     :children ()}
                    {:tag :number
                     :id :id
                     :gid :user.id
                     :name "user.id"
                     :label "Id"
                     :children ()})}))
 (fact "list children"
       (parse-form [:group.user
                    (list
                     :text.name)
                    (for [i (range 2)]
                      [:text {:id (keyword (str "order" i))}])])
       =>
       (just {:tag :group
              :id :user
              :gid :user
              :name "user"
              :label "User"
              :children
              (just (cons
                     {:tag :text
                      :id :name
                      :gid :user.name
                      :name "user.name"
                      :label "Name"
                      :children ()}
                     (for [i (range 2)
                           :let [name (str "order" i)
                                 fullname (str "user." name)]]
                       {:tag :text
                        :id (keyword name)
                        :gid (keyword fullname)
                        :name fullname
                        :label (str/capitalize name)
                        :children ()})))}))
 )
