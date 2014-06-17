(ns easy-form.util-test
  (:require
   [midje.sweet :refer :all]
   [clojure.string :as str])
  (:require
   [easy-form.util :refer :all]))

(facts
 "test str->num"
 (fact "pre"
  (str->num nil) => (throws #"\(string\? s\)")
  (str->num 1) => (throws #"\(string\? s\)"))
 (fact "number"
  (str->num "1") => 1
  (str->num "1.2") => 1.2
  (str->num "1/2") => 1/2
  (str->num "11111111111111111111111111111111111111111")
  => 11111111111111111111111111111111111111111N)
 (fact "post"
  (str->num "") => (throws #"\(number\? %\)")
  (str->num "{:a 1}") => (throws #"\(number\? %\)")
  (str->int "1.2") => (throws #"\(integer\? %\)"))
 (fact "unsafe read"
  (str->num "{:a 1") => (throws)
  (str->num "#=(println 1)") => (throws)))
