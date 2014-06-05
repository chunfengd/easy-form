(ns easy-form.core-test
  (:require
   [midje.sweet :refer :all]
   [clojure.string :as str])
  (:require
   [easy-form.core :refer :all]))

(fact
 "test demo"
 (str/split "a/b/c" #"/") => ["a" "b" "d"])
