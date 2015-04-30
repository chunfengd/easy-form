(defproject easy-form "0.1.1-SNAPSHOT"
  :description "Create web forms for clojure"
  :url "https://github.com/chunfengd/easy-form/"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.8.5"]
                 [dire "0.5.2"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}}
  :signing {:gpg-key "chunfengd@gmail.com"})
