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
              :label "Username"
              :parser anything
              :validators anything}))
 (fact "extra option"
       (parse-form [:text.username {:extra "added"}])
       =>
       (just {:tag :text
              :id :username
              :name "username"
              :children ()
              :gid :username
              :label "Username"
              :extra "added"
              :parser anything
              :validators anything}))
 (fact "new id"
       (parse-form [:text.username {:id :new-id}])
       =>
       (just {:tag :text
              :id :new-id
              :name "new-id"
              :children ()
              :gid :new-id
              :label "New-id"
              :parser anything
              :validators anything}))
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
              :label "Username"
              :parser anything
              :validators anything})))

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
              :parser anything
              :validators anything
              :children
              (just (just {:tag :text
                           :id :name
                           :gid :user.name
                           :name "user.name"
                           :label "Name"
                           :children ()
                           :parser anything
                           :validators anything})
                    (just {:tag :number
                           :id :id
                           :gid :user.id
                           :name "user.id"
                           :label "Id"
                           :children ()
                           :parser anything
                           :validators anything}))}))
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
              :parser anything
              :validators anything
              :children
              (just (cons
                     (just {:tag :text
                            :id :name
                            :gid :user.name
                            :name "user.name"
                            :label "Name"
                            :children ()
                            :parser anything
                            :validators anything})
                     (for [i (range 2)
                           :let [name (str "order" i)
                                 fullname (str "user." name)]]
                       (just {:tag :text
                              :id (keyword name)
                              :gid (keyword fullname)
                              :name fullname
                              :label (str/capitalize name)
                              :children ()
                              :parser anything
                              :validators anything}))))}))
 )
