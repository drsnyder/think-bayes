(defproject think-bayes "0.1.0-SNAPSHOT"
  :description "Supporting code for ThinkBayes by Allen Downey"
  :url "http://github.com/drsnyder/think-bayes"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [drsnyder/think-stats "0.1.0-SNAPSHOT"]
                 [midje "1.8.2"]]
  :repl-options {:init-ns user}
  :profiles {:dev {:source-paths ["dev"]
                   :plugins [[lein-midje "3.1.3"]]
                   :dependencies [ ]}})
