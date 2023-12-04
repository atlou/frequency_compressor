(ns menu
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [compress :refer [compress-file]])
  (:require [compress :refer [decompress-file]]))
  ; this is where you would also include/require the compress module

; Verify if file exists in the current directory
(defn file-exists?
  "Returns true if a file exists.
   Returns false otherwise."
  [filename]
  (.exists (io/file (str "./" filename))))

; Read file from root directory
(defn read-file 
  [filename]
  (slurp (str "./" filename)))

; Display the menu and ask the user for the option
(defn showMenu
  []
  (println "\n\n*** Compression Menu ***")
  (println "------------------\n")
  (println "1. Display list of files")
  (println "2. Display file contents")
  (println "3. Compress a file")
  (println "4. Uncompress a file")
  (println "5. Exit")
  (do 
    (print "\nEnter an option? ") 
    (flush) 
    (read-line)))


; Display all files in the current folder
; TODO: make more clean display and only read files from the root directory
(def files-in-root 
  (filter #(and (.isFile %) (not (.isDirectory %))) 
          (file-seq (io/file "."))))

(defn print-files
  [files] 
  (println)
  (doseq [file files] 
    (println (.getName file))))

(defn option1
  [] 
  (print-files files-in-root)
  (flush))
    
; Read and display the file contents (if the file exists). Java's File class can be used to 
; check for existence first. 
(defn option2
  []
  (print "\nPlease enter a file name => ") 
  (flush)
  (let [filename (read-line)]
    (println)
    (if (file-exists? filename) 
      (println (read-file filename))
      (println "Error: the file does not exist."))
    (flush)))

; Compress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your compression function
(defn txt-file?
  [filename]
  (boolean (re-matches #".*\.txt" filename)))

(defn option3
  [] ;parm(s) can be provided here, if needed
  (print "\nPlease enter a file name => ") 
  (flush)
  (let [filename (read-line)]
    (println)
    (if (and (txt-file? filename)(file-exists? filename))
      (do 
        (compress-file filename)
        (println "Compressed the file to a new file" (str filename ".ct"))) 
      (println "Error: the file does not exist or is not a .txt file."))
    (flush)))

; Decompress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your decompression function
(defn ct-file?
  [filename]
  (boolean (re-matches #".*\.ct" filename)))

(defn option4
  [] ;parm(s) can be provided here, if needed
  (print "\nPlease enter a file name => ") 
  (flush)
  (let [filename (read-line)]
    (println)
    (if (and (ct-file? filename)(file-exists? filename))
      (do 
        (decompress-file filename)
        (println "Deompressed the file to a new file" (str/replace filename ".txt.ct" ".txt")))
      (println "Error: the file does not exist or is not a .ct file."))
    (flush)))

; If the menu selection is valid, call the relevant function to 
; process the selection
(defn processOption
  [option] ; other parm(s) can be provided here, if needed
  (if( = option "1")
     (option1)
     (if( = option "2")
        (option2)
        (if( = option "3")
           (option3)  ; other args(s) can be passed here, if needed
           (if( = option "4")
              (option4)   ; other args(s) can be passed here, if needed
              (println "Invalid Option, please try again"))))))


; Display the menu and get a menu item selection. Process the
; selection and then loop again to get the next menu selection
(defn menu
  [] ; parm(s) can be provided here, if needed
  (let [option (str/trim (showMenu))]
    (if (= option "5")
      (println "\nGood Bye\n")
      (do 
         (processOption option)
         (recur )))))   ; other args(s) can be passed here, if needed


; ------------------------------
; Run the program. You might want to prepare the data required for the mapping operations
; before you display the menu. You don't have to do this but it might make some things easier

(menu) ; other args(s) can be passed here, if needed
