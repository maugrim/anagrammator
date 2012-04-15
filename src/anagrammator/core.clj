(ns anagrammator.core
  (:use [clojure.java.io :only [reader]])
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def system-dictionary "/usr/share/dict/words")

(defn get-words [dictionary-file]
  (line-seq (reader dictionary-file)))

(defn is-eligible-word? [word]
  (>= (count word) 5))

(defn sanitize [text]
  (->> (str/replace text #"\s" "")
       (str/lower-case)))

(defn trie-add [trie word]
  (update-in trie word merge {:word-at word}))

(defn trie-from [words]
  (reduce trie-add {} words))

(defn decrement-count [freqs key]
  (let [count (get freqs key 0)]
    (if (<= count 1) (dissoc freqs key) (assoc freqs key (dec count)))))

(defn search [dictionary letters]
  (defn search-in [trie-node freqs]
    (when-not (nil? trie-node)
      (concat
       ;; if we've found a complete word, either return it along with any other anagrams we
       ;; can make using our remaining letters, starting at the dictionary root -- or, if
       ;; our letter bag is empty, then just return that word
       (when-let [current-word (:word-at trie-node)]
         (if (seq freqs)
           (map #(conj % current-word) (search-in dictionary freqs))
           [[current-word]]))

       ;; for each letter left in our bag, find all the anagrams that include that letter,
       ;; given the remaining letters which we would have available afterward
       (mapcat #(search-in (trie-node %) (decrement-count freqs %)) (keys freqs)))))

  (search-in dictionary (frequencies letters)))

(defn anagrams [text]
  (let [dictionary (->> (get-words "/Users/marshall/dict.txt")
                        (map sanitize)
                        (filter is-eligible-word?)
                        (trie-from))]
    (map #(apply str (interpose " " %)) (search dictionary (sanitize text)))))