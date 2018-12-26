(defproject aid "0.1.2"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.439"]
                 [funcool/cats "2.3.1"]
                 [potemkin "0.4.5"]]
  :plugins [[lein-ancient "0.6.15"]
            [lein-npm "0.6.2"]]
  :profiles {:dev {:dependencies [[binaryage/devtools "0.9.10"]
                                  [figwheel-sidecar "0.5.18"]]}}
  :npm {:dependencies [[ws "6.1.2"]]})
