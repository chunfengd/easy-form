(ns easy-form.core
  (:require
   [clojure.string :as str]
   [easy-form.util :refer :all]))

(defn create-parser [note func]
  (with-meta func {:note note}))

(defmacro local-item [& x]
  (let [x (->> x
               (map #(cond
                      (symbol? %) (name %)
                      (keyword? %) (name %)
                      (string? %) %
                      :else (str %))))]
    `(let [item-name# (str/join "." [(:name ~'item) ~@x])]
       (~'params item-name#))))

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
    `(fn [~'params ~'item]
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
                      (parser *default-parser-pool*)
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

(defn render-form [item style]
  (cond
   (map? item) (let [{tag :tag} item]
                 ((or (tag style) (:default style))
                  item style))
   (seq? item) (for [x item]
                 (render-form x style))))

(def default-style
  {:default (fn [{:keys [tag id name]} style]
              [:input {:type tag :id id :name name}])
   :group (fn [{:keys [tag id name children]} style]
            [:div
             (render-form children style)])
   :form (fn [{:keys [tag id name children attr]} style]
           [:form attr
            (render-form children style)])})

(defn extract-params
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

(defn pre-form-seq [item]
  (lazy-seq
   (cons item (mapcat pre-form-seq (:children item)))))

(defn post-form-seq [item]
  (concat (mapcat post-form-seq (:children item))
          (list item)))

(defn params->values
  #_(params->values
     {"user.name" "name" "user.id" ""}
     (parse-form [:form.user
                  :text.name
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
         [{} {}] (post-form-seq item))]
    [values errors]))

;; (let [[values errors]
;;       errors
;;       (loop [es errors vds validators]
;;         (let [self (values name)
;;               vd (first vds)]
;;           (if (nil? vd)
;;             es
;;             (if (vd self)
;;               (recur es (rest vds))
;;               (recur (update-in es [name]
;;                                 conj
;;                                 ^{:err e :item x}
;;                                 (or (-> vd meta :note)
;;                                     "Invalid value"))
;;                      (rest vds))))))]
;;   [values errors])
