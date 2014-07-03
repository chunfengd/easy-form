(ns easy-form.core
  (:require
   [clojure.string :as str]
   [easy-form.util :refer :all]))

(defn create-parser [note func & [unparser]]
  (with-meta func {:note note
                   :unparser unparser}))

(defmacro local-item [& x]
  (let [x (->> x
               (map #(cond
                      (symbol? %) (name %)
                      (keyword? %) (name %)
                      (string? %) %
                      :else (str %))))]
    `(let [item-name# (str/join "." [(:name ~'_*item*_) ~@x])]
       (~'_*values*_ item-name#))))

(defmacro $ [& x] `(local-item ~@x))

(defmacro create-validator
  #_(create-validator
     "this is a note"
     (let [user ($)
           id ($ id)
           name ($ name)]
       [id name]))
  [note & body]
  (with-meta
    `(fn [~'_*values*_ ~'_*item*_]
       ~@body)
     {:note note}))

(def ^{:dynamic true} *default-parser-pool*
  {:int (create-parser "Not a valid integer" str->int)
   :float (create-parser "Not a valid number" str->num)})

(def ^{:dynamic true} *default-validator-poll*
  {:require (create-validator "Required" (-> ($) nil? not))})

(declare parse-element* parse-seq*)

(defn- parse-element* [ancesters item]
  (let [[tag & more] item
        tag (name tag)
        [tag id] (str/split tag #"\.")
        [option & children] (cond
                             (empty? more) [{}]
                             (-> more first map?) more
                             :else (cons {} more))
        element (merge {:tag (keyword tag)
                        :id (keyword id)}
                       option)
        {:keys [id label parser validators]} element
        label (or label (and id (-> id name str/capitalize)))
        parser (and parser
                    (if (keyword? parser)
                      (if-let [p (parser *default-parser-pool*)]
                        p
                        (throw (Exception. (str "No such parser: " (name parser)))))
                      parser))
        validators (->> validators
                        (map #(if (keyword? %)
                               (% *default-validator-poll*)
                               %)))
        next-ancesters (if id
                         (conj ancesters id)
                         ancesters)
        name (->> next-ancesters
                  (map name)
                  (str/join "."))]
    (assoc element
      :children (parse-seq* next-ancesters children)
      :gid (keyword name)
      :name name
      :label label
      :parser parser
      :validators validators)))

(defn- parse-seq* [ancesters items]
  (->> items
       (mapcat
        (fn [item]
          (cond
           (keyword? item) (list (parse-element* ancesters [item]))
           (vector? item) (list (parse-element* ancesters item))
           (seq? item) (parse-seq* ancesters item))))))

(defn parse-form [item]
  (cond
   (keyword? item) (parse-element* [] [item])
   (vector? item) (parse-element* [] item)
   (seq? item) (parse-seq* [] item)))

(defn get-value [{:keys [name parser]} values params]
  (let [unparser (-> parser meta :unparser)]
    (try
      (or (and (values name) unparser
               (unparser (values name)))
          (values name)
          (params name))
      (catch Throwable e
        (params name)))))

(defn render-form [item style
                   & {:keys [values params disabled]
                      :or {values {} params {}}}]
  (cond
   (map? item) (let [{tag :tag name :name} item]
                 ((or (tag style) (:default style))
                  item style
                  :values values
                  :params params
                  :disabled disabled))
   (seq? item) (for [x item]
                 (render-form x style
                              :values values
                              :params params
                              :disabled disabled))))

(def default-style
  {:default (fn [{:keys [tag id name label attr] :as item} style
                 & {:keys [values params disabled]}]
              [:div
               [:span label ": "]
               [:input (merge {:type tag :id name :name name
                               :value (get-value item values params)
                               :disabled disabled}
                              attr)]])
   :html (fn [{:keys [body]} style & _ ] body)
   :group (fn [{:keys [tag id name children]} style
               & {:keys [values params disabled]}]
            [:div
             (render-form children style
                          :values values
                          :params params
                          :disabled disabled)])
   :form (fn [{:keys [tag id name children attr]} style
              & {:keys [values params disabled]}]
           [:form attr
            (render-form children style
                         :values values
                         :params params
                         :disabled disabled)])
   })

(defn extract-values
  "(extract-params {\"user.name\" \"name\"
                   \"user.id\" \"1\"
                   \"user.password\" nil})
  ;;=> {:user {:id 1, :name \"name\"}}"
  [params]
  (reduce
   (fn [m [k v]]
     (let [ks (->> (str/split k #"\.")
                   (map keyword))]
       (assoc-in m ks v)))
   {} params))

(defn map->values [obj]
  (letfn [(f* [obj & [ancesters]]
            (let [ancesters (or ancesters [])]
              (if (map? obj)
                (lazy-seq
                 (mapcat (fn [[k v]]
                           (f* v (conj ancesters
                                       (name k))))
                         obj))
                [[(str/join "." ancesters) obj]])))]
    (->> (f* obj)
         (into {}))))

(defn pre-form-seq [item]
  (lazy-seq
   (cons item (mapcat pre-form-seq (:children item)))))

(defn post-form-seq [item]
  (concat (mapcat post-form-seq (:children item))
          (list item)))

(defn params->values
  #_(params->values
     {"user.name" "" "user.id" "1"}
     (parse-form [:form.user
                  [:text.name {:validators [:require]}]
                  [:number.id {:parser :int}]]))
  [params item]
  (let [[values errors]
        (reduce
         (fn [[values errors]
              {:keys [name parser] :as x}]
           (if-let [[_ value] (find params name)]
             (try
               [(assoc values name
                       (cond-> (nil-if-empty value)
                               parser parser))
                errors]
               (catch Throwable e
                 [values (update-in
                          errors [name]
                          conj
                          ^{:err e :item x}
                          (or (-> parser meta :note)
                              "Invalid value"))]))
             [values errors]))
         [{} {}] (post-form-seq item))
        errors
        (reduce
         (fn [errors {:keys [name validators] :as x}]
           (loop [es errors vds validators]
             (let [vd (first vds)]
               (if (nil? vd)
                 es
                 (try
                   (if (vd values x)
                     es
                     (update-in errors [name]
                                conj
                                ^{:err nil :item x}
                                (or (-> vd meta :note)
                                    "Invalid value")))
                   (catch Throwable e
                     (update-in errors [name]
                                conj
                                ^{:err e :item x}
                                (or (-> vd meta :note)
                                    "Invalid value"))))))))
         errors
         (post-form-seq item))]
    [values errors]))
