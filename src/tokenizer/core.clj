(ns tokenizer.core
 (:require [clj-crfsuite.core :as crfsuite])
 (:gen-class))


(def tagger (crfsuite/get-tagger "resources/lt-model.crfsuite"))

(def left-span-length 12)

(def right-span-length 12)


(defn homogeneous
  "determines if `string` is made up
  of similar types of characters"
  [string]
  (re-matches #"^\p{L}+$|^\p{Z}+$|^\p{N}+$|^\.+$|^\[?!]+$|^\,+$" string))


(defn get-feature
  "generates single feature given `chars-to-left` and `chars-to-right`"
  [chars-to-left chars-to-right]
  (if (homogeneous (str (last chars-to-left) (first chars-to-right))) {:h "1"}
      (let [index (range (- (count chars-to-left)) (count chars-to-right))
            keys (map #(keyword (format "c%s" %)) index)
            chars (map str (concat chars-to-left chars-to-right))]
        (zipmap keys chars))))


(defn tag
  "predicts labels
  in given `char-spans`"
  [char-spans]
  (apply str "1" (map :tag (crfsuite/tag
                            (for [char-span (rest char-spans)]
                              (->> char-span
                                   (split-at left-span-length)
                                   (map #(filter identity %))
                                   (apply get-feature))) tagger))))


(defn chop
  "divides `chain` of items
  based on every `chunk` length"
  [chain chunks]
  (lazy-seq (if (empty? chunks) []
                (let [chunk (first chunks)
                      chunk-length (count chunk)
                      chunk-spans (take chunk-length chain)]
                  (cons [chunk chunk-spans]
                        (chop (drop chunk-length chain) (rest chunks)))))))


(defn into-tokens
  "processes `chunk` and add product into `tokens`"
  [tokens [chunk chunk-spans]]
  (if (homogeneous chunk) (conj tokens chunk)
      (let [labels (tag chunk-spans)
            label-chunks (re-seq #"10*" labels)
            chunk-tokens (map #(->> % second (apply str)) (chop chunk label-chunks))]
        (apply conj tokens chunk-tokens))))


(defn tokenize
  "splits `string` into tokens"
  [string]
  (let [chunks (re-seq #"[^\p{Z}\r\t\n]+|[\p{Z}\r\t\n]+" string)
        padded-string (concat (repeat left-span-length nil) string (repeat right-span-length nil))
        span-length (+ left-span-length right-span-length)
        spans (partition span-length 1 padded-string)]
    (reduce into-tokens [] (chop spans chunks))))
