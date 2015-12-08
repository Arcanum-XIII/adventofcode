(ns adventofcode.core
  (:require [clojure.string :refer [split]]
            [clj-message-digest.core :refer :all])) ;; needed for the day 4

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

(defn d5-create-groups 
  [text]
  (loop [text text result [] index 0]
    (if (empty? text) result
        (recur (rest text) (conj result [index (take 2 text)]) (inc index)))))
