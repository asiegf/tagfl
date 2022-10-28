(ns tagfl.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [tagfl.hamsters :as hamsters]
            [tagfl.hta :as hta]
            [tagfl.utils :refer [alpha]]))

(def ta     (atom {:nodes       {}
                   :environment {}}))
(def policy (atom {:model   :hamsters
                   :flavour :original
                   :output  :normal
                   :file    "output.svg"}))

(defn parse-children
  [id children]
  (let [style "style=filled;fillcolor=\"black;0.05:white\";gradientangle=270;color=white"
        rank-rule (format "{rank=same %s}"
                          (s/join " " (map alpha children)))
        link-rule (format "%s [color=white]"
                          (s/join " -> " (map alpha children)))
        children-rule (map (fn [child]
                             (format "%s[label=\"%s\"]" (alpha child) child))
                           children)]
    (format "subgraph cluster_%s { %s; %s; %s; %s;}"
            (alpha id)
            style
            (s/join ";" children-rule)
            rank-rule
            link-rule)))

(defn parse-node
  [[id {:keys [label children]
        :or {label id, children []}
        :as props}]]
  (let [id (alpha id)]
    (format "%s[label=\"%s\"]; %s; %s;"
            id
            label
            (when-not (empty? children)
              (parse-children id children))
            (when-not (empty? children)
              (format "%s -> %s [lhead=cluster_%s]"
                      id (alpha (first children)) id)))))

(defn parse
  [ta]
  (let [rules ["compound=true" "node[shape=box]" "edge[arrowhead=none,penwidth=2]" "splines=ortho "]]
    (format "digraph G { %s %s }"
            (s/join ";" rules)
            (reduce-kv
             (fn [s node props]
               (str s (parse-node [node props])))
             "" (:nodes ta))
            )))


;; ------------------------------ generating -----------------------------------

(defn gen-graph! [policy ta]
  (let [graph (case (:model policy)
                :hta      (parse ta)
                :hamsters (hamsters/parse (:flavour policy) ta)
                "this model is not implemented")]
    graph
    ;; (case ;; (:output policy)
    ;;   :svg ;; (let [output (or (:file policy) "output.svg")]
    ;;        ;;   (do (dot "-Tsvg" "-o" output {:in graph})
    ;;        ;;       (str "graph generated successfully to: " output)))
    ;;   graph)
    ))


;; ------------------------------ specifying ta ---------------------------------

(defn update-op-ta! [op parent & children]
  (swap! ta
         update-in
         [:nodes parent]
         (fn [m] (assoc m
                        :children children
                        :operator op))))

(defn update-task-ta! [node k v]
  (swap! ta
         update-in
         [:nodes node]
         (fn [m] (assoc m k v))))

(defn update-env-ta! [node ins outs]
  (swap! ta
         update-in
         [:environment node]
         (fn [env]
           (-> env
               (update :ins #(clojure.set/union % ins))
               (update :outs #(clojure.set/union % outs))))))

(defmacro operate [operator args]
  `(do (gen-graph! @policy
                   (update-op-ta! ~operator ~@args))
       ~(first args)))

(defn df [task & ks]
  (do (gen-graph! @policy
                  (update-task-ta! task :type (set ks)))
      task))

(defn label [task label]
  (do (gen-graph! @policy
                  (update-task-ta! task :label label))
      task))

(defn env [ins task outs]
  (do (update-env-ta! task
                      (set (if (map? ins) [ins] ins))
                      (set (if (map? outs) [outs] outs)))
      task))

(defmacro >>  [& args] `(operate :enable ~args))
(defmacro ||| [& args] `(operate :concurrent ~args))
(defmacro ?   [& args] `(operate :choice ~args))
(defmacro !   [& args] `(operate :disable ~args))
(defmacro |>  [& args] `(operate :suspend-resume ~args))
(defmacro |-| [& args] `(operate :order-independent ~args))


(defn dk
  ([id] (dk id (name id)))
  ([id label] {:id id, :label label, :type :dk}))
(defn inf
  ([id] (inf id (name id)))
  ([id label] {:id id, :label label, :type :inf}))
(defn obj
  ([id] (obj id (name id)))
  ([id label] {:id id, :label label, :type :obj}))


;; ------------------------------ interface --------------------------------------

(defn update-policy [new-policy]
  (gen-graph! (swap! policy #(merge % new-policy))
              @ta))

(defn init! []
  (reset! ta
          {:nodes       {}
           :environment {}}))

(defn dot->image
  "Uses GraphViz to render the DOT into an image"
  [dot format]
  (try
    (let [{:keys [out err]} (sh/sh "dot" (str "-T" format) :in dot :out-enc :bytes)]
      (io/input-stream out))
    (catch java.io.IOException e
      (throw (java.io.IOException. "Graphviz not installed?")))))

(defn render!
  [format]
  (io/copy (dot->image (gen-graph! @policy @ta) format)
           (io/file (str "output." format))))


;; --------------------------------- pad -----------------------------------------

(comment

  (update-policy {:model :hta})
  (parse-children :task (:children (second (first (:nodes @ta)))))

  (let [[node props] (first (:nodes @ta))])

  (load-file "test.clj")

  (reset! ta {})

  (macroexpand '(>> :a :b :c))


  (>> "task a"
      (>> "task b"
          "task ba"
          (>> "task bb"
              "task a"
              "task B")
          "task bc")
      (>> "task c"
          (>> "task ca"
              "task C"
              "task D")
          "task cb"
          "task cc"))


  (df "boil water" :abtract)


  (macroexpand '(>> "make a cup of tea"
                    "boil water"
                    (>> "pour water in a cup"
                        "pick your favorite cup"
                        (>> "*pour boiling water in the cup"
                           "yes"
                           "no"))))


  (update-policy {:model :hta})

  ta#atom
  [{:nodes {"pour water in a cup" {:type #{:abstract}},
            "make a cup of tea" {:children ("boil water" "pour water in a cup"), :operator :concurrent},
            "boil water" {:type #{:abtract}}}
    } 0xff6e1c7]
  (>> "boil water"
      "put the ketle on"
      "wait 5m")


  (render! (hamsters/parse :original @ta))


  (remove :a)

  (spit "output.svg"
        (viz/image "digraph { }"))

  (spit "output.svg"
        (viz/image "digraph { taska -> taskasequential; taskasequential -> taskb; taskasequential -> taskc;taska -> taskasequential; taskasequential -> taskb; taskasequential -> taskc;taska -> taskasequential; taskasequential -> taskb; taskasequential -> taskc; }"))
  (spit "output.svg"
        (viz/image (gen-graph! @policy @ta)))
  (>> :task-a :task-b :task-c)
  (>> :task-a
      (>> :task-b :task-ba :task-bb)
      :task-c))



;;

;; let's generate a new empty Task Analysis
(init!)


;; ... and let's see a simple graph example
(>> "DeepSleep"
    "go to bed"
    "close-my-eyes"
    "count-sheeps")

(render! "png")


;; let's add to our initial
(>> "go to bed"
    "move towards bedroom"
    "go towards bed"
    "lie in the bed")
(render! "png")

;; and even further...
(>> "move towards bedroom"
    "stand up"
    "walk to bedroom")

;; alternatively, we can just set one huge example
(init)
