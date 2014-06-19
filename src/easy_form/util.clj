(ns easy-form.util
  (:refer-clojure :exclude [read-string])
  (:require
   [clojure.string :as str]
   [clojure.tools.reader.edn :as reader]))

(defn read-string-safe [s]
  (reader/read-string s))

(defn str->num* [s]
  {:pre [(string? s)]
   :post [(number? %)]}
  (reader/read-string s))

(defn str->num [s]
  {:pre [(or (nil? s) (string? s))]}
  (if s (str->num* s)))

(defn str->int* [s]
  {:pre [(string? s)]
   :post [(integer? %)]}
  (reader/read-string s))

(defn str->int [s]
  {:pre [(or (nil? s) (string? s))]}
  (if s (str->int* s)))

(defn nil-if-empty [s]
  {:pre [(or (nil? s) (string? s))]}
  (if (seq s) s nil))
