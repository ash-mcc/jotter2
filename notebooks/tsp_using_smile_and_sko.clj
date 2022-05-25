^{:nextjournal.clerk/toc true
  ::clerk/visibility :hide-ns}
(ns tsp-using-smile-and-sko
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk])
  (:import java.net.URL
           javax.imageio.ImageIO
           java.awt.image.BufferedImage
           [java.awt Color Font]))

^{::clerk/visibility :hide}
;; Use a photograph from Jrouse5, of a travelling door-to-door salesman.
;; Licence: CC-BY-SA-4.0
;; 1,108 × 1,452 pixels
(let [url           (URL. "https://upload.wikimedia.org/wikipedia/commons/d/d7/Great_Depression_Salesman.png")
      img           (with-open [in (io/input-stream url)] (ImageIO/read in))
      x             0 
      y             220 
      width         1108 
      height        700
      img-cropped   (.getSubimage img x y width height)
      scaled-width  (* (- width x) 2)
      scaled-height (* (- height y) 2)
      img-scaled    (BufferedImage. scaled-width scaled-height BufferedImage/TYPE_INT_RGB)
      graphics      (.createGraphics img-scaled)]
  (.drawImage graphics img-cropped 0 0 scaled-width scaled-height nil)
  (.setColor graphics Color/WHITE)
  (.setFont graphics (Font. "SansSerif" Font/BOLD 16))
  (.drawString graphics "Photograph from Jrouse5, CC-BY-SA-4.0", (- scaled-width 350), 25)
  img-scaled)

;; # Solving the Travelling Salesman Problem using Genetic Algorithms from two off-the-shelf libraries

;; ## 👋 Introduction

;; We generate solutions to the [Travelling Salesman Problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem) (TSP)
;; using [Genetic Algorithms](https://en.wikipedia.org/wiki/Genetic_algorithm) (GAs) 
;; from two off-the-shelf libraries: Java's [SMILE](https://haifengl.github.io),
;; and Python's [scikit-opt](https://scikit-opt.github.io/scikit-opt/#/en/).

;; ## 📍 Specify the locations to be visited

;; x,y coords of the locations to be visited by the travelling salesman
;; (symmetric TSP - distances the same in both directions, no one way streets, etc.)
(def locations
  {:kilmarnock  [0 0]
   :galashiels  [100 0]
   :standrews   [100 100]
   :callander   [0 100]
   :cumbernauld [25 25]
   :stirling    [25 75]
   :penicuik    [75 25]
   :kinross     [75 75]
   :livingston  [50	50]
   :lauder      [125 25]})

;; ## 📐 Calculate the distances involved

;; use Pythagoras
(defn distance-between [l1 l2]
  (let [[x1 y1] (locations l1)
        [x2 y2] (locations l2)
        xdiff   (- x1 x2)
        ydiff   (- y1 y2)]
    (Math/sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))

;; distance for one complete circuit of the route
(defn circuit-distance [route]
  (let [route-rotated-one-pos (conj (vec (rest route)) (first route))]
    (->> (map (fn [l1 l2] (distance-between l1 l2)) route route-rotated-one-pos)
         (apply +))))

;; ## 🔢 Specify the parameters that are to be used by both of the GA libraries

(def population-size 40)
(def max-generations 100)
(def mutation-rate 0.05)

;; ## 🅰️ Use the SMILE GA

;; [API documentation](http://haifengl.github.io/api/java/smile/gap/GeneticAlgorithm.html) 

;; ### 🔌 Code the problem in terms of the GA

;; the shorter the circuit distinct the better the fitness
(defn fitness [route]
  (/ 1 (circuit-distance route)))

;; use a swap mutation because it works easily with our permutation encoding of the TSP
(defn swap-mutation [route]
  (let [len (count route)]
    (loop [route   (vec route)
           counter 0]
      (if (< counter len) ;; apply the possibility of mutation 'len' times, to the whole route
        (if (< (rand) mutation-rate) ;; probablistically apply a mutation 
          (let [ix1   (rand-int len) ;; select 2 route components at random
                ix2   (rand-int len)
                route (assoc route ix1 (nth route ix2) ix2 (nth route ix1))] ;; and swap 'em
            (recur route (inc counter)))
          (recur route (inc counter))) ;; don't mutate the route
        route ;; we've finished
        ))))

(declare individual)

(defn random-individual []
  (individual (shuffle (keys locations))))

;; import from the SMILE library
(import '[smile.gap Chromosome GeneticAlgorithm])

;; take from parent 1 up to a random index, then fill the remainder from parent 2
;; and vice versa to generate the second Chromosome
(defn one-point-crossover
  ([route1 route2 n]
   (let [orig-seg (take n route1) ;; take n from the first parent
         new-seg  (remove #(contains? (set orig-seg) %) route2)] ;; and fill the remaining places with values from the second parent which aren't in orig-seg
     (concat orig-seg new-seg)))
  ([route1 route2]
   (let [n (rand-int (count route1))] ;; choose the crossover point at random
     (into-array Chromosome [(individual (one-point-crossover route1 route2 n))
                             (individual (one-point-crossover route2 route1 n))]))))

;; a hacky means to read the state of an _individual_, i.e. to read the `route` that is associated with the _individual_ 
(defn ->route [individual]
  (read-string (.toString individual)))

;; Smile's GA operates over abstract Chromosome objects
;; and this fn defns our implementation for a Chromosome for the TSP  
(defn individual
  [route]
  (let [state (atom route)]
    (reify Chromosome
      (crossover [_this another] (one-point-crossover @state (->route another)))
      (fitness [_this] (fitness @state))
      (mutate [_this] (swap! state swap-mutation))
      (newInstance [_this] (random-individual))
      (toString [_this] (pr-str @state)) ;; depended on by ->route which is a hacky means to read the state
      (compareTo [this another] (compare (.fitness this) (.fitness another))) ;; needed because Smile's GA sorts Chromosome objects by fitness
      )))

;; ### 🧬 Run the GA

(def smile-solution 
  (let [seed-population (into-array Chromosome (repeatedly population-size random-individual))
        ga              (GeneticAlgorithm. seed-population #_(Selection/Tournament 3 0.95) #_1) ;; defaults to tournament selection of size 3 and probabilty 0.95, and elitism with 1 best copied into next gen
        best-found      (.evolve ga max-generations) ;; evolve generations of solutions and take the best
        route           (->route best-found)]
    {:route    route
     :distance (circuit-distance route)}))
          
;; ## 🅱️ Use the scikit-opt GA

;; [Usage documentation](https://github.com/guofei9987/scikit-opt/blob/master/examples/demo_ga_tsp.py#L1) 

;; ### 🔌 Code the problem in terms of the GA

;; the scikit-opt fn wants to use ints 0..n to represent locations so index (with ints) the locations
(def locations-index
  (into {} (keep-indexed vector (keys locations))))

;; defn a version of circuit-distance that accepts a list of indexes
(defn circuit-distance-using-indexes [route-indexes]
  (circuit-distance (map locations-index route-indexes)))

;; ### 🧬 Run the GA

;; import from the scikit-opt library
(require '[libpython-clj2.python :as py])
(require '[libpython-clj2.require :refer [require-python]])
(require-python '[sko.GA :as sk-ga :bind-ns true])
(declare run)

(def sko-solution 
  (let [ga                              (sko.GA/GA_TSP circuit-distance-using-indexes (count locations) population-size max-generations mutation-rate)
        [route-indexes distance-holder] (py/py.. ga run)                
        route                           (map locations-index route-indexes) 
        distance                        (first distance-holder)]
    {:route    route
     :distance distance}))

;; ## ✏️ Plot the solutions

;; build 3 vectors which describe a route: xcoords, ycoords and location labels
(defn ->circuit-vectors [route]
  (let [route (concat route [(first route)])] ;; and back to the starting location
    [(->> route (map #(-> locations % first)) vec) ;; xcoords
     (->> route (map #(-> locations % second)) vec);; ycoords
     route ;; location labels
     ]))

(require-python '[matplotlib.pyplot :as plt :bind-ns true])

;; assemble a list of instructions which say how to plot points, joining lines and labels to a matplotlib figure
(defn ->plot-instructions [circuit-vectors]
  (let [[xcoords ycoords route] circuit-vectors]
    (cons (plt/plot xcoords ycoords "-gD") ;; plot the points and joining lines
          (map (fn [x y l]
                 (plt/text (+ x 1.5) (+ y 1.5) (name l)))
               xcoords ycoords route) ;; label the points
          )))

(require-python '[matplotlib.backends.backend_agg :as backend :bind-ns true])
(def mplt (py/import-module "matplotlib"))

;; use the headless mode of matplotlib
(py/py. mplt "use" "WebAgg")

(defn plot-solution [name {:keys [route distance]}]
  (plt/clf)
  (let [fig      (plt/figure)
        canvas   (backend/FigureCanvasAgg fig)
        filename (str name ".png")]
    (-> route
        ->circuit-vectors
        ->plot-instructions
        vec)
    (plt/title (format "%s solution, distance %.2f" name distance))
    (py/py. canvas "draw")
    (plt/savefig filename)
    (ImageIO/read (io/file filename))))

;; ### 🅰️ Plot the SMILE solution

(plot-solution "SMILE" smile-solution)

;; ### 🅱️Plot the scikit-opt solution

(plot-solution "scikit-opt" sko-solution)

;; ## 🎞️ Create an animation of the SMILE GA working

;; run the SMILE GA again but this time record the best solution found by each generation 
(def smile-solutions
  (let [seed-population (into-array Chromosome (repeatedly population-size random-individual))
        ga              (GeneticAlgorithm. seed-population #_(Selection/Tournament 3 0.95) #_1)] ;; defaults to tournament selection of size 3 and probabilty 0.95, and elitism with 1 best copied into next gen
    (loop [generation 1
           acc        []]
      (if (and (<= generation max-generations) ;; not reached max generations?
               (or (empty? acc) ;; no solutions accumulated?
                   (> (:distance (last acc)) 476.6))) ;; best-found's distance is still > a known good solution's distance
        (let [best-individual (.evolve ga 1) ;; evolve another generation of solutions and take its best
              route           (->route best-individual)]
          (recur (inc generation)
                 (conj acc {:route      route
                            :distance   (circuit-distance route)}) ;; add the route to the accumulator 
                 ))
        acc))))

;; use MoviePy to create an animation
(require-python '[moviepy.video.io.bindings :as mpb :bind-ns true])
(require-python '[moviepy.editor :as mpe :bind-ns true])
(declare clear write_gif)

;; how-to create a frame for the animation
(defn make-frame [get-solution-data-fn fig agg-canvas t]
  (let [{:keys [generation-number circuit-distance circuit-vectors]} (get-solution-data-fn t)]
    (py/py. fig clear) ;; clear anything that was previously rendered
    (conj (->plot-instructions circuit-vectors) ;; add the route (dots, labels and joining lines) to the figure
          (plt/title (format "generation %d, distance %.2f" generation-number circuit-distance))) ;; add a title to the figure
    (py/py. agg-canvas "draw") ;; render the figure
    (mpb/mplfig_to_npimage fig))) ;; convert it into the structure that moviepy wants

;; create the animation
(let [solutions            (conj smile-solutions (last smile-solutions)) ;; hack to workaround the last frame being dropped
      fps                  2
      duration             (/ (count solutions) fps) ;; i.e. = the number of routes / the number to be show per second
      get-solution-data-fn (fn [t] ;; closes over fps and solutions
                             (let [solution-ix              (int (* t fps))
                                   {:keys [route distance]} (nth solutions solution-ix)]
                               {:generation-number (inc solution-ix)
                                :circuit-distance  distance
                                :circuit-vectors   (->circuit-vectors route)}))
      fig                  (plt/figure)
      agg-canvas           (be/FigureCanvasAgg fig)
      make-frame'          (py/->python (partial make-frame get-solution-data-fn fig agg-canvas))
      animation            (mpe/VideoClip make-frame' :duration duration)
      filename             "smile-animation.gif"]
  (py/py. animation write_gif filename :fps fps)
  (clerk/html [:img {:src filename}]))

