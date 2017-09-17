(in-package :clox)

(-> load-file-to-string (pathname) string)
(defun load-file-to-string (path)
  "Return contents of file at PATH as a string."
  (with-open-file (in path :external-format :utf-8)
    (with-output-to-string (result)
      (loop for line = (read-line in nil 'eof nil)
         until (eql line 'eof) do (format result "~A~%" line)))))
