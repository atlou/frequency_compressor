(ns compress
  (:require [clojure.string :as str]))

(defn split-space
  "Splits a string on whitespace"
  [string]
  (str/split string #"[\n\r\s]+"))

(def frequency-list (distinct (split-space (slurp "./frequency.txt"))))

(defn get-word-frequency
  "Returns the index of the word in the frequency.txt file"
  [word]
  (.indexOf frequency-list (str/lower-case word)))

(defn prepare-string
  "Adds spaces around special characters"
  [string]
  (str/replace string #"[^\p{Alnum}]" #(str " " % " ")))

(defn alpha?
  "Returns true if the word is composed of letters only.
   Returns false otherwise."
  [token]
  (boolean (re-matches #"[A-Za-z]+" token)))

(defn numeric?
  "Returns true if the word is composed of numbers only.
   Returns false otherwise."
  [token]
  (boolean (re-matches #"[0-9]+" token)))

(defn compress-number
  "Adds '@' at the beginning and end of a number."
  [number]
  (str "@" number "@"))

(defn compress-word 
  "Returns the frequency of the word if it exists, 
   returns the unchanged word otherwise"
  [word]
  (let [freq (get-word-frequency word)]
    (if (= freq -1)
      word
      freq)))

(defn process-token
  "Takes a token and compresses it if it's a word or a number. 
   Otherwise returns the same token."
  [token]
  (if (alpha? token) 
    (compress-word token)
    (if (numeric? token)
      (compress-number token)
      token)))

(defn compress-string
  "Returns the compressed verison of a string."
  [string]
  (let [tokens (split-space (prepare-string string))]
    (str/join " " (for [t tokens]
        (process-token t)))))

(defn write-file
  "Writes a string to a file."
  [filename, string]
  (spit (str filename) string))

(defn compress-file
  "Creates a compressed version of a .txt file."
  [filename]
  (write-file (str filename ".ct") (compress-string (slurp (str "./" filename)))))

(compress-file "text.txt")