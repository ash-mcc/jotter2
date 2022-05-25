```clojure
^{:nextjournal.clerk/visibility #{:hide :hide-ns}}
(ns index
  (:require [nextjournal.clerk :as clerk]))
```

# Ash's jotter #&#8203;2 🗒️ - Applied Machine Learning

Walk-throughs, notes and other jottings about Applied Machine Learning.

```clojure
(clerk/html
  (into
    [:div.md:grid.md:gap-8.md:grid-cols-2.pb-8]
    (map
      (fn [{:keys [path preview title description]}]
        [:a.rounded-lg.shadow-lg.border.border-gray-300.relative.flex.flex-col.hover:border-indigo-600.group.mb-8.md:mb-0
         {:href (clerk/doc-url path)
          :style {:height 300}}
         [:div.flex-auto.overflow-hidden.rounded-t-md.flex.items-center.px-3.py-4
          [:img {:src preview :width "100%" :style {:object-fit "contain"}}]]
         [:div.sans-serif.border-t.border-gray-300.px-4.py-2.group-hover:border-indigo-600
          [:div.font-bold.block.group-hover:text-indigo-600 title]
          [:div.text-xs.text-gray-500.group-hover:text-indigo-600.leading-normal description]]])
      [{:title "🧳 Solving the Travelling Salesman Problem using Genetic Algorithms from two off-the-shelf libraries"
        :preview "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/Great_Depression_Salesman.png/586px-Great_Depression_Salesman.png"
        :path "notebooks/tsp_using_smile_and_sko.clj"
        :description "We generate solutions to the Travelling Salesman Problem (TSP)
        using Genetic Algorithms (GAs) from two off-the-shelf libraries: Java's SMILE and Python's scikit-opt."}])))