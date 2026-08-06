;;; (file->string fname) -> string?
;;;  fname : string?
;;; Reads the contents of the file named `fname` and returns it as a string. Raises an error if the file does not exist.
;;; @category file, file->lines, string->file, lines->file, with-file
(define-export file->string (js-var "file_fileToString"))

;;; (file->lines fname) -> list?
;;;  fname : string?
;;; Reads the contents of the file named `fname` and returns it as a list of strings, one per line. A trailing newline at the end of the file does not produce a final empty line. Raises an error if the file does not exist.
;;; @category file, file->string, lines->file, string->file, with-file
(define-export file->lines (js-var "file_fileToLines"))

;;; (string->file str fname) -> void?
;;;  str : string?
;;;  fname : string?
;;; Writes the string `str` to the file named `fname`, creating the file if it does not exist and replacing its contents if it does.
;;; @category file, lines->file, file->string, file->lines
(define-export string->file (js-var "file_stringToFile"))

;;; (lines->file lines fname) -> void?
;;;  lines : list?
;;;  fname : string?
;;; Writes `lines`, a list of strings, to the file named `fname`, one line each, creating the file if it does not exist and replacing its contents if it does. The file ends with a trailing newline, so that `file->lines` reads back exactly `lines`.
;;; @category file, string->file, file->lines, file->string
(define-export lines->file (js-var "file_linesToFile"))
