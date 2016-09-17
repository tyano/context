(defproject context "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.3.1"]]

  :profiles {:dev {:resource-paths ["resources-dev"]
                   :dependencies [[midje "1.8.3"]]}}

  :resource-paths ["resources" "resources-dev"]
  )
