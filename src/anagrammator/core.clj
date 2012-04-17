(ns anagrammator.core
  (:use [clojure.java.io :only [reader]])
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

;; define some utility functions for manipulating tries of words; each trie will be structured
;; as a series of nested hashmaps, e.g. to store the words "bar", "barbie", and "baz" we expect the
;; following trie:
;; {\b {\a {\r {:word-at "bar" \b {\i {\e {:word-at "barbie" }}}} \z {:word-at "baz"}}}}

(defn trie-add [trie word]
  (update-in trie word merge {:word-at word}))

(defn trie-from [words]
  (reduce trie-add {} words))

;; some utility functions for processing input and output and such:

(def system-dictionary "/usr/share/dict/words")

(defn get-words [dictionary-file]
  (line-seq (reader dictionary-file)))

(defn is-eligible-word? [word]
  (>= (count word) 5)) ;; more interesting anagrams come out when we use bigger words!

(defn sanitize [text]
  (->> (str/replace text #"\s" "")
       (str/lower-case)))

(defn stringify [words]
  (apply str (interpose " " words)))

;; finally, the algorithm for searching in our trie:

(defn decrement-count [freqs key]
  "Decreases the count of key in a map representing the frequency of multiple keys, e.g.
   (decrement-count {:a 2 :b 5} :b) --> {:a 2 :b 4}"
  (let [count (get freqs key 0)]
    (if (<= count 1) (dissoc freqs key) (assoc freqs key (dec count)))))

(defn search [dictionary letters]
  "Given a dictionary structured as a trie containing each word, and a string including all of the
   letters we have available, searches through the trie and yields each possible anagram of the string."
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

(defn anagrams
  "Returns a lazy sequence consisting of each complete anagram of text made up of words
   within the contents of dictionary-file."
  [text dictionary-file]
  (let [dictionary (->> (get-words dictionary-file)
                        (map sanitize)
                        (filter is-eligible-word?)
                        (trie-from))]
    (map stringify (search dictionary (sanitize text)))))

;; Examples:
;;
;; (def my-dict "/Users/marshall/dict.txt")
;;
;; anagrammator.core=> (time (anagrams "inventories" my-dict))
;; "Elapsed time: 132.956 msecs"
;; ("soviet inner" "snore invite" "senor invite" "osier invent" "noise invert" "invert noise"
;;  "enter vision" "vision enter" "invent osier" "invite snore" "invite senor" "inner soviet")
;;
;; anagrammator.core=> (time (first (anagrams "liron shapira" my-dict)))
;; "Elapsed time: 145.324 msecs"
;; "solar hairpin"
;;
;; anagrammator.core=> (time (first (anagrams "huge chrome cylinder box unfolding" my-dict)))
;; "Elapsed time: 1577.542 msecs"
;; "mignon influx hodge cherry becloud"