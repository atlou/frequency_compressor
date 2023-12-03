(ns display-file-contents
  (:require [clojure.java.io :as io]))

(defn file-exists?
  [file]
  (.exists (io/file file)))

(defn read-file
  [filename]
  )

(defn print-file-contents 
  [filename]
  (let [file (str (System/getProperty "user.dir") "/" filename)]
  (if (file-exists? file)
    (println (slurp file))
    (println "Error: the file" filename "does not exist."))))
