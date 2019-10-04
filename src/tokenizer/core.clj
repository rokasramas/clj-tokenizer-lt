(ns tokenizer.core
  (:require [clj-crfsuite.core :as crfsuite]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- load-model [path left-span-length right-span-length]
  {:tagger            (crfsuite/get-tagger path)
   :left-span-length  left-span-length
   :right-span-length right-span-length
   :span-length       (+ left-span-length right-span-length)
   :left-pad          (repeat left-span-length nil)
   :right-pad         (repeat right-span-length nil)})

(def model (atom (load-model (.getPath (io/resource "lt-model.crfsuite")) 12 12)))

(defn homogeneous
  "determines if `string` is made up
  of similar types of characters"
  [string]
  (re-matches #"^\p{L}+$|^\p{Z}+$|^\p{N}+$|^\.+$|^\[?!]+$|^\,+$" string))

(defn get-feature
  "generates single feature given `chars-to-left` and `chars-to-right`"
  [chars-to-left chars-to-right]
  (if (homogeneous (str (last chars-to-left) (first chars-to-right)))
    {:h "1"}
    (let [index (range (- (count chars-to-left)) (count chars-to-right))
          keys (map #(keyword (format "c%s" %)) index)
          chars (map str (concat chars-to-left chars-to-right))]
      (zipmap keys chars))))

(defn- get-features [character-context split-position]
  (map #(->> (split-at split-position %)
              (map (partial remove nil?))
              (apply get-feature))
        character-context))

(defn tag
  "predicts tags for given `character-context`"
  [character-context]
  (let [{split-position :left-span-length tagger :tagger} @model
        features (get-features character-context split-position)]
    (->> (crfsuite/tag features tagger)
         (rest)
         (map :tag)
         (cons "1"))))

(defn- apply-tags
  [tags string]
  (loop [[head & tail] (re-seq #"10*" (str/join tags))
         start 0
         tokens []]
    (if (nil? head)
      tokens
      (let [end (+ start (count head))]
        (->> (subs string start end)
             (conj tokens)
             (recur tail end))))))

(defn- chop
  "divides string `chunks` into tokens
  based on every `character-context`"
  [character-context chunks]
  (lazy-seq
    (when (seq chunks)
      (let [chunk (first chunks)
            chunk-length (count chunk)]
        (if (homogeneous chunk)
          (cons
            chunk
            (chop
              (drop chunk-length character-context)
              (rest chunks)))
          (concat
            (-> (take chunk-length character-context)
                (tag)
                (apply-tags chunk))
            (chop
              (drop chunk-length character-context)
              (rest chunks))))))))

(defn tokenize
  "splits `string` into tokens"
  ([string]
   (tokenize string #"[^\p{Z}\r\t\n]+|[\p{Z}\r\t\n]+"))
  ([string chunk-pattern]
   (when (some? string)
     (chop
       (let [{:keys [left-pad right-pad span-length]} @model]
         (partition span-length 1 (concat left-pad string right-pad)))
       (re-seq chunk-pattern string)))))
