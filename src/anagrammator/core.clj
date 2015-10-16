(ns anagrammator.core
  (:use [clojure.java.io :only [reader]])
  (:require clojure.string)
  (:require clojure.math.combinatorics))

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
  (->> (clojure.string/replace text #"\s" "")
       (clojure.string/lower-case)))

(defn stringify [words]
  (apply str (interpose " " words)))

;; finally, the algorithm for searching in our trie:

(defn decrement-count [freqs key]
  (let [count (get freqs key 0)]
    (if (<= count 1) (dissoc freqs key) (assoc freqs key (dec count)))))

(defn anagrams-in
  ([dictionary letters] (anagrams-in dictionary dictionary letters))
  ([dictionary trie-node letters]

     (defn dfs [trie-node letters]
       ;; for each letter left in our bag, search for all the anagrams that include that letter,
       ;; given the remaining letters which we would have available afterward
       (lazy-seq (mapcat #(anagrams-in dictionary (trie-node %) (decrement-count letters %)) (keys letters))))

     ;; four cases:
     ;;
     ;; * we have no more letters, and
     ;;    - we're not at a word: dfs will return an empty collection, no anagrams here
     ;;    - we're at a word: great, return one new anagram starting with this word
     ;;
     ;; * we have more letters, and
     ;;    - we're not at a word: call dfs to sum all the anagrams we could find
     ;;                           while choosing any one of our letters
     ;;    - we're at a word: the above, and also concat any anagrams given the rest
     ;;                       of our letters if we chose to include this word

     (when-not (nil? trie-node)
       (let [current-word (:word-at trie-node)
             anagrams-using-word (map #(conj % current-word) (dfs dictionary letters))
             anagrams-without-word (dfs trie-node letters)]
         (cond (and current-word (empty? letters)) [[current-word]]
               (and current-word (seq letters)) (concat anagrams-using-word anagrams-without-word)
               :else anagrams-without-word)))))

(defn anagrams [text dictionary-file]
  (let [dictionary (->> (get-words dictionary-file)
                        (map sanitize)
                        (filter is-eligible-word?)
                        (trie-from))]
    (map stringify (anagrams-in dictionary (frequencies (sanitize text))))))

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
