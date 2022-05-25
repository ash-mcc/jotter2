(ns user
  (:require [clojure.java.browse :as browse]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

(comment
  
  ;; start without file watcher, open browser when started
  (clerk/serve! {:browse? true})

  ;; start with file watcher for these sub-directory paths
  (clerk/serve! {:watch-paths ["notebooks" "index.md"]})

  ;; start with file watcher and a `show-filter-fn` function to watch
  ;; a subset of notebooks
  (clerk/serve! {:watch-paths    ["notebooks"]
                 :show-filter-fn #(clojure.string/starts-with? % "notebooks")})

  ;; open clerk
  (browse/browse-url "http://localhost:7777")

  ;; or call `clerk/show!` explicitly
  (clerk/show! "notebooks/tsp_using_smile_and_sko.clj")

  (clerk/show! "index.md")

  (clerk/clear-cache!)


  ;; Clerk elides lists after the 20th element; show and tweak the eliding parameter :n
  (-> @v/!viewers :root (get 10) :fetch-opts :n)
  (swap! v/!viewers update-in [:root 10 :fetch-opts] #(assoc % :n 35))
  

  ;; generate a 'static app'
  (clerk/build-static-app! {:bundle?     false
                            :out-path    "public/jotter2"
                            :path-prefix "jotter2/"
                            :paths       ["index.md"
                                          "notebooks/tsp_using_smile_and_sko.clj"]})

  )
