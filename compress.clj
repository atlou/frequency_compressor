(ns compress
  (:require [clojure.string :as str]))

(defn split-space
  "Splits a string on whitespace"
  [string]
  (str/split string #"[\n\r\s]+"))

(def frequency-list (distinct (split-space (slurp "./frequency.txt"))))

(defn word-to-frequency
  "Returns the index of the word in the frequency.txt file"
  [word]
  (.indexOf frequency-list (str/lower-case word)))

(defn parse-int 
  [string] 
  (Integer. (re-find  #"\d+" string)))

(defn frequency-to-word
  "Returns the word of the index in the frequency.txt file"
  [frequency]
  (nth frequency-list (parse-int frequency)))

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

(defn decompress-number
  "Removes '@' at the beginning and end of a number."
  [number]
  (str/replace number "@" ""))

(defn compress-word 
  "Returns the frequency of the word if it exists, 
   returns the unchanged word otherwise"
  [word]
  (let [freq (word-to-frequency word)]
    (if (= freq -1)
      word
      freq)))

(defn comp-process-token
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
        (comp-process-token t)))))

(defn write-file
  "Writes a string to a file."
  [filename, string]
  (spit (str filename) string))

(defn compress-file
  "Creates a compressed version of a .txt file."
  [filename]
  (write-file (str filename ".ct") (compress-string (slurp (str "./" filename)))))

(defn extract-punctuation
  [string]
  (re-seq #"[\.!\?]" string))

(defn extract-sentences
  [string]
  (str/split string #"[\.!\?]"))

(defn is-number?
  [token]
  (boolean (re-matches #"@[0-9]+@" token)))

(defn is-word
  [token]
  (boolean (re-matches #"[0-9]+" token)))

(defn format-punctuation
  "Takes a string and formats the spacing for all punctuation."
  [s]
  (str/replace s #"\s*[!,.?]\s*" #(str (str/trim %) " ")))

(defn format-brackets
  "Takes a string and formats the spacing for all opening and closing brackets."
  [s]
  (let [closing-brackets (str/replace s #"\s*[\)\]]\s*" #(str (str/trim %) " "))]
  (str/replace closing-brackets #"\s*[\(\[]\s*" #(str " " (str/trim %)))))

(defn format-others
  "Takes a string and formats the spacing for other special characters."
  [s]
  (str/replace s #"\s*[$@]\s*" #(str " " (str/trim %))))

(defn process-special
  "Takes a string and formats the spacing for all non-alphanumeric characters.
   It does so following an order of priority (other special characters -> brackets -> punctuation)"
  [s]
  (format-punctuation (format-brackets (format-others s))))

(defn decomp-process-token
  "Processes the decompression of a single token of text."
  [token]
  (if (is-number? token)
    (decompress-number token)
    (if (is-word token)
      (frequency-to-word token)
      token)))

(defn decompress-tokens
  "Processes the decompression of all tokens of a string."
  [string]
  (let [tokens (split-space string)]
    (str/join " " (for [t tokens]
      (decomp-process-token t)))))

(defn capitalize
  "Capitalizes the first letter of a string, without lowercasing all the other characters."
  [string]
  (str/replace-first string #"[A-Za-z]" #(str/upper-case %)))

(defn capitalize-sentences
  "Capitalizes all sentences from a sequence of strings."
  [sentences]
  (for [s sentences] 
    (capitalize (str/trim s))))

(defn split-sentences
  "Splits a string in a sequence of sentences by splitting where the punctuation is.
   Capitalizes each sentence.
   Returns an interlaced vector of capitalized sentences and punctuation."
  [s]
  (let [pun (extract-punctuation s)
        sen (capitalize-sentences (extract-sentences s))]
    (if (empty? pun)
      sen
      (interleave sen pun))))

(defn format-sentences
  [s]
  (str/join " " (split-sentences s)))

(defn decompress-string
  [s]
  (str/trim (process-special (format-sentences (decompress-tokens s)))))

(defn decompress-file
  "Creates a decompressed version of a .ct file."
  [filename]
  (let [f (str/replace filename ".txt.ct" ".txt")
        d (decompress-string (slurp (str "./" filename)))]
  (write-file f d)))