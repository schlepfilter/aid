(ns repl
  (:require [figwheel-sidecar.repl-api :as repl-api]))

(def src
  "src")

(def build
  {:id           src
   :source-paths [src]
   :compiler     {:output-to            "target/main.js"
                  :main                 "aid.core"
                  :target               :nodejs
                  :source-map-timestamp true
                  :preloads             ['devtools.preload]
                  :external-config      {:devtools/config {:features-to-install :all}}}
   :figwheel     true})

(repl-api/start-figwheel!
  {:build-ids  [src]
   :all-builds [build]})

(repl-api/cljs-repl)
