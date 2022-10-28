(ns tagfl.utils
  (:require [tagfl.config :refer [image-names]]))

(defn alpha [k]
  (->> k
       str
       clojure.string/lower-case
       (re-seq #"[a-zA-Z]")
       (apply str)))

(defn connect [from to]
  (format "%s -> %s;" from to))

(defn gen-image-path [model flavour type]
  (let [image-name (get image-names type "abstract.svg")]
       (format "assets/%s/%s/%s"
               (name model)
               (name flavour)
               image-name)))
