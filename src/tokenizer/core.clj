(ns tokenizer.core
  (:require [clj-crfsuite.core :refer :all])
  (:gen-class))

(def model (get-tagger "resources/lt-model.crfsuite"))

(def left-span 5)

(def right-span 3)


(defn get-feature [char-span]
  (let [[chars-to-left chars-to-right] (map #(filter some? %) (split-at left-span char-span))
        index (range (- (count chars-to-left)) (count chars-to-right))
        keys (map #(keyword (format "c%s" %)) index)
        chars (map str (concat chars-to-left chars-to-right))]
    (zipmap keys chars)))


(defn split? [char-span]
  (= "1" (:tag (first (tag [(get-feature char-span)] model)))))


(defn zip [chunks spans]
  (letfn [(lazy-zip [chunks spans]
            (let [chunk (first chunks)
                  length (count chunk)
                  [chunk-spans rest-spans] (split-at length spans)]
              (lazy-seq
               (cons [chunk chunk-spans]
                     (lazy-zip (rest chunks) rest-spans)))))]
    (take-while (fn [[chunk chunk-spans]] (complement nil?) chunk) (lazy-zip chunks spans))))


(defn infer [chunk-seq chunk-spans]
  (let [chunks-with-spans (zip chunk-seq chunk-spans)
        [first-chunk first-spans] (first chunks-with-spans)]
    (loop [tokens [first-chunk]
           chunks-with-spans (rest chunks-with-spans)]
      (if (empty? chunks-with-spans) tokens
          (let [[current-chunk current-spans] (first chunks-with-spans)]
            (recur (if (split? (first current-spans))
                     (conj tokens current-chunk)
                     (conj (pop tokens) (str (peek tokens) current-chunk)))
                   (rest chunks-with-spans)))))))


(defn into-tokens [tokens [chunk chunk-spans]]
  (let [chunk-seq (re-seq #"\p{L}+|\p{N}+|\p{Z}+|." chunk)]
    (if (= 1 (count chunk-seq))
      (conj tokens chunk)
      (apply conj tokens (infer chunk-seq chunk-spans)))))


(defn tokenize [string]
  (let [chunks (re-seq #"[^\p{Z}]+" string)
        span-length (+ left-span right-span)
        spans (partition span-length 1 (concat (repeat left-span nil) string (repeat right-span nil)))]
    (reduce into-tokens [] (zip chunks spans))))
