;;; Functions for handling files

;;; (file-exists? fname) -> boolean?
;;;  fname : string?
;;; Returns `#t` if `fname` names something in storage and `#f` otherwise. Note that a directory counts as existing, so a `#t` here does not guarantee that `file->string` will succeed.
;;; @category file, file->string, file->lines, string->file, lines->file
(define-export file-exists? (js-var "file_fileExistsQ"))

;;; (file->string fname) -> string?
;;;  fname : string?
;;; Reads the contents of the file named `fname` and returns it as a string. Raises an error if the file does not exist, so use `file-exists?` first if that is a possibility.
;;; @category file, file-exists?, file->lines, string->file, lines->file, with-file
(define-export file->string (js-var "file_fileToString"))

;;; (file->lines fname) -> list?
;;;  fname : string?
;;; Reads the contents of the file named `fname` and returns it as a list of strings, one per line. A trailing newline at the end of the file does not produce a final empty line. Raises an error if the file does not exist, so use `file-exists?` first if that is a possibility.
;;; @category file, file-exists?, file->string, lines->file, string->file, with-file
(define-export file->lines (js-var "file_fileToLines"))

;;; (string->file str fname) -> void?
;;;  str : string?
;;;  fname : string?
;;; Writes the string `str` to the file named `fname`, creating the file if it does not exist and replacing its contents if it does.
;;; @category file, lines->file, file->string, file->lines, file-exists?
(define-export string->file (js-var "file_stringToFile"))

;;; (lines->file lines fname) -> void?
;;;  lines : list?
;;;  fname : string?
;;; Writes `lines`, a list of strings, to the file named `fname`, one line each, creating the file if it does not exist and replacing its contents if it does. A non-empty list is written with a trailing newline, so that `file->lines` reads it back unchanged; the empty list writes an empty file. A string that itself contains a newline is written as-is, so it reads back as more than one line.
;;; @category file, string->file, file->lines, file->string, file-exists?
(define-export lines->file (js-var "file_linesToFile"))
