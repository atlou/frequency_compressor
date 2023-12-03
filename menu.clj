(ns menu
  (:require [clojure.string :as str])
  (:require [io :refer [read-file]]))
  ; this is where you would also include/require the compress module


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

; Verify if file exists in the current directory
(defn file-exists?
  "Returns true if a file exists.
   Returns false otherwise."
  [filename]
  (.exists (io/file (str "./" filename))))


; Display all files in the current folder
(def files-in-root 
  (filter #(and (.isFile %) (not (.isDirectory %))) 
          (file-seq (io/file "."))))

(defn print-files
  [files] 
  (doseq [file files] 
    (println (.getName file))))

(defn option1
  [] 
  (print-files)
  (flush))
    
; Read and display the file contents (if the file exists). Java's File class can be used to 
; check for existence first. 
(defn option2
  []
  (print "\nPlease enter a file name => ") 
  (flush)
  (let [file_name (read-line)]
    (println read-file))



; Compress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your compression function
(defn option3
  [] ;parm(s) can be provided here, if needed
  (print "\nPlease enter a file name => ") 
  (flush)
  (let [file_name (read-line)]
     (println "now compress" file_name "with with the functions(s) you provide in compress.clj")))


; Decompress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your decompression function
(defn option4
  [] ;parm(s) can be provided here, if needed
  (print "\nPlease enter a file name => ") 
  (flush)
  (let [file_name (read-line)]
     (println "now decompress" file_name "with with the functions(s) you provide in compress.clj")))


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
