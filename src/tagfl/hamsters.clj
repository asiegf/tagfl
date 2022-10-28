(ns tagfl.hamsters
  (:require [clojure.string :as s]
            [tagfl.utils :refer [alpha connect gen-image-path]]
            [tagfl.config :as config]))

(defn add-space [s] (str "\n\n\n" s))
(defn parse-prop [[k v]]
  (format "%s=\"%s\""
          (name k) v))

(defn node
  [id props-or-string]
  (let [props
        (if (string? props-or-string) {:label props-or-string}
            (if-not (contains? props-or-string :image) props-or-string
                    (update props-or-string
                            :label add-space)))]
    (format "%s[%s];" id
            (s/join "," (map parse-prop props)))))

(defn op-to-children
  [flavour op children]
  (str (reduce (fn [s child] (str s (connect op child)))
               "" children)
       (format "{rank = same; %s [style=invis];}"
               (s/join " -> " children))))

(defn parse-node
  [flavour id {:keys [type label operator children]
               :or   {type #{:abstract} label (name id)}}]
  (let [id       (alpha id)
        children (map alpha children)]
    [(node id {:label label, :shape "plaintext",
               :image (gen-image-path :hamsters flavour type)})
     (when-not (or (nil? operator) (empty? children))
       (let [op (str id (name operator))]
         (str (node op {:label (format "%s" (get config/operators operator "not-found"))
                        :shape "plaintext"})
              (connect id op)
              (op-to-children flavour op children))))
     ]))

(defn parse [flavour ta]
  (format "digraph { %s }"
          (reduce-kv
           (fn [s id node] (str s (s/join (parse-node flavour id node))))
           "" (:nodes ta))))
