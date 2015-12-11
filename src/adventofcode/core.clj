(ns adventofcode.core
  (:require [clojure.string :refer [split split-lines]]
            [clj-message-digest.core :refer :all]
            [instaparse.core :as insta])) ;; needed for the day 4

;; brutish approach ! Give the function the string and you'll get the answer. Since we're using a lisp, we'll never know the cost of memory it's gonna use... But the call stack should be good since we're using recur. 

(defn parse-day1-input 
  [input]
  (letfn [(parse [input floor]
            (cond 
              (empty? input) floor
              (= (first input) \( ) (recur (rest input) (+ 1 floor))
              :default (recur (rest input) (- floor 1))))]
    (parse input 0)))

;; somewhat better : use a higher order function curryed to simplify the work.

(defn parse-parens 
  [c]
  (cond 
    (= c \( ) #(+ % 1)
              :default #(- % 1)))

(defn find-basement-entry-position [input]
  (letfn [(find-pos [input floor counter]
            (if (empty? input) nil
                (let [char (first input)
                      flr ((parse-parens char) floor)
                      cnt (+ 1 counter)]
                  (if (= flr -1) cnt
                      (recur (rest input) flr cnt)))))]
    (find-pos input 0 0)))

;; day 2


(defn d2-parse 
  [str fun]
  (apply fun (map read-string (split str #"x"))))

(defn d2-find-paper-length
  [l w h]
  (let [sizes [(* l w) (* w h) (* l h)]]
    (+ (apply + (map #(* 2 %) sizes)) (first (sort sizes)))))

(defn d2-get-paper-surface
  [str]
  (d2-parse str d2-find-paper-length))

;; getting final result is easy : we get the info from an input file (slurp "data")
;; then we do a simple (apply + (map d2-parse-nbr (split (slurp "data") #"\n")))
;; on Windows it would probably be #"\rn" as the split regex.

(defn d2-find-ribbon-length
  [l w h]
  (+ (* l w h) (apply + (map #(* 2 %) (take 2 (sort [l w h]))))))

(defn d2-get-ribbon-total-length
  [str]
  (d2-parse str d2-find-ribbon-length))

;; day 3

;; Again, fun with recursion ! This one was better prepared with proper helper function directly though of.

(defn d3-update-pos 
  "Helper to update a position [x y] with an update vector [+x +y]"
  [pos v]
  (into [] (map + pos v)))

(defn d3-parse-symbol 
  "Transform a symbol into its correct move"
  [s]
  (cond (= s \>) [1 0]
        (= s \<) [-1 0]
        (= s \^) [0 1]
        (= s \v) [0 -1]))

(defn d3-count-house 
  "Will parse a string of <>^v symbol"
  [data]
  (loop [d data houses #{[0 0]} cpos [0 0]]  
    (if (empty? d) (count houses)
        (let [updated-pos (d3-update-pos cpos (d3-parse-symbol (first d)))]
          (recur (rest d) (conj houses updated-pos) updated-pos)))))

(defn d3-update-correct-santa 
  "Will update a Santa based of it's key. Map should in the form of {:santakey [x y]}"
  [santa pos v]
  (assoc pos santa (d3-update-pos (santa pos) v)))

(defn d3-count-house2
  [data]
  (loop [d data houses #{[0 0]} cpos {:santa1 [0 0] :santa2 [0 0]} current :santa1]  
    (if (empty? d) (count houses)
        (let [updated-pos (d3-update-correct-santa current cpos (d3-parse-symbol (first d)))]
          (recur (rest d) (apply conj houses (map (fn [[k v]] v) updated-pos)) updated-pos (if (= current :santa1) :santa2 :santa1))))))


;; day 4

(defn d4-find-match 
  "Brute way of find which value produce for a given seed five starting 0 in hex."
  [seed match]
  (take 1 (filter (fn [[k v]](re-find match v)) (map (fn [v] [v (md5-hex (str seed v))]) (iterate inc 1)))))

;; day 5


(defn d5-match-bad-string 
  [text]
  (if (re-find #"(.*ab.*|.*cd.*|.*pq.*|.*xy.*)" text) true false))

(defn d5-match-repeated-char
  [text]
  (if (re-find #"([A-Za-z])\1+" text) true false))

(defn d5-match-vowels 
  [text]
  (if (= 3 (count (take 3 (re-seq #"[aeiou]" text)))) true false))

(defn d5-test-string [text]
  (and (not (d5-match-bad-string text)) (d5-match-repeated-char text) (d5-match-vowels text)))

;; to find the solution : (count (filter true?  (map d5-test-string (split (slurp "d5-input.txt") #"\n")))

(defn d5-create-pairs 
  "Given a string, will build a series of overlapping pair so 'aeb' will return [['ae] ['eb']]"
  [text]
  (loop [t text result []]
    (cond (or (= 1 (count t)) (empty? t)) result
          :default (recur (subs t 1) (conj result (subs t 0 2))))))

(defn d5-find-duplicate-pair 
  "Brute force, no memoization and no shortcut !"
  [double]
  (loop [v (rest double) filter-val (first double) r #{}]
    (let [duplicates (filter #(= (nth filter-val 1) (nth % 1)) v)]
      (cond (empty? v) r 
            :default (recur (rest v) (first v) (if (not (empty? duplicates)) (apply conj r (conj duplicates filter-val)) r))
))))

(defn d5-validate-pair 
  "How to know if pairs are next to each other or not ? By using a simple substraction (far index to nearer index), and if the result is equal to one, they're next to each other. Of course we can't use a base 0 index for that (- 0 is not substracting anything after all)"
  [pair]
  (cond (let [[one two] (nth (first pair) 1)] (not= one two)) true 
        (not= 1 (reduce - (map inc (map first (reverse (sort-by first pair)))))) true
        :default false))

(defn d5-check-pairs 
  [text]
  (if (not (empty? (filter true? (map #(d5-validate-pair (second %)) (group-by second (d5-find-duplicate-pair (keep-indexed #(vector %1 %2) (d5-create-pairs text)))))))) true false))

(defn d5-validate-triplets 
  [triplet]
  (= (get triplet 0) (get triplet 2)))

(defn d5-check-triplets 
  [text]
  (loop [t text]
    (if (empty? t) false
        (let [v (if (= (count t) 3) t
                    (subs t 0 3))]
          (cond (d5-validate-triplets v) true
                (= (count t) 3) false
                :default (recur (subs t 1)))))))

;; took me too long to figure it, and I have the feeling it could be improved (memoization and some shortcuts)
(defn d5-second-test 
  [text]
  (and (d5-check-pairs text) (d5-check-triplets text)))


;; Day 6 !

(defn d6-gen-init-vec 
  "Big improvement to readability : understanding threading macro. Generate an off grid (squared one)"
  [limit]
  (->> 0
       repeat
       (take limit)
       (into [])
       repeat
       (take limit)
       (into [])))

(def d6-instruction-parser
  "Insta parser magic : far easier to extract purposeful instruction from the text one. Like a parser should."
  (insta/parser
   "instructions = instruction startx starty endx endy 
    instruction = 'turn on' | 'turn off' | 'toggle'
    startx = <[' ']>#'[0-9]*'
    starty = <[',']>#'[0-9]*'
    endx = <[' through ']>#'[0-9]*'
    endy = <[',']>#'[0-9]*'"))

(defn d6-clean-instruction
  "Transform the result of an d6-instruction-parser into a 'cleaner' map. Eye of the beholder and so on."
  [instruction]
  (let [instruction (into {} (rest instruction))]
    (apply conj  {:instruction (cond (= "turn off" (:instruction instruction)) :off
                                     (= "turn on" (:instruction instruction)) :on
                                     :default :toggle)}
           (->> instruction
                rest
                (map (fn [[k v]] [k (read-string v)]))
                (into {})))))

(defn d6-action 
  "Return a function, depending of what Santa ordered."
  [instruction]
  (cond (= instruction :toggle) #(if (= % 0) 1 0)
        (= instruction :off) (fn [_] 0)
        :default (fn [_] 1)))

(defn d6-action-part2
  "Return a function, depending of what Santa ordered."
  [instruction]
  (cond (= instruction :toggle) #(+ 2 %)
        (= instruction :off) #(if (= % 0) 0 (- % 1))
        :default #(+ 1 %)))

(defn d6-change-vec
  "Apply a function to a range of index (inclusively) from a vector"
  [from to source limit f]
  (let [to (inc to)
        start (subvec source 0 from) 
        tochange (subvec source from to) 
        end (subvec source to limit)]
    (apply conj (apply conj start (map f tochange)) end)))

(defn d6-apply-instruction
  [instruction grid size chooser]
  (d6-change-vec (:starty instruction) (:endy instruction) grid size
                 #(d6-change-vec (:startx instruction) (:endx instruction) % size (chooser (:instruction instruction)))))

(defn d6-transform-grid [list grid chooser]
  (loop [grid grid list list]
    (if (empty? list) grid
        (recur (d6-apply-instruction (d6-clean-instruction (d6-instruction-parser (first list))) grid (count (first grid)) chooser) (rest list)))))

;; part 1 solution : (reduce + (map (fn [i] (reduce + i)) (d6-transform-grid (split-lines (slurp "d6-input.txt")) (d6-gen-init-vec 1000))))
