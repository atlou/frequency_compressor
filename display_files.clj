(ns display-files
  (:require [clojure.java.io :as io]))

(def files-in-root 
  (filter #(and (.isFile %) (not (.isDirectory %))) 
          (file-seq (io/file "."))))

(defn print-files
  [files] 
  (doseq [file files] 
    (println (.getName file))))

(print-files files-in-root)