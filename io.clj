(ns io
  (:require [clojure.java.io :as io]))

(defn file-exists?
  "Returns true if a file exists.
   Returns false otherwise."
  [file]
  (.exists (io/file file)))

(defn read-file
  "Returns the contents of a file."
  [filename]
    (slurp (str "./" filename)))

(defn write-file
  "Writes a string to a file."
  [filename, string]
  (spit (str filename) string))

(def files-in-root 
  (filter #(and (.isFile %) (not (.isDirectory %))) 
          (file-seq (io/file "."))))

(defn print-files
  [files] 
  (doseq [file files] 
    (println (.getName file))))

(read-file "text.txt")
(write-file "test.ct" "salut")