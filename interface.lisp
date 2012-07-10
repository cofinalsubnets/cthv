(in-package :csv)

(export '(file->csv
          csv->file
          string->csv
          csv->string
          with-csv-parameters))

(defun file->csv (filename)
  (with-open-file (csv-file filename :direction :input)
    (loop for row = (read-csv-line csv-file)
          while row collect row)))

(defun string->csv (str)
  (with-input-from-string (csv-string str)
    (loop for row = (read-csv-line csv-string)
          while row collect row)))

(defun csv->file (csv filename &optional (header nil))
  "Write a CSV to the named file with an optional header line (which should be a list of strings)."
  (with-open-file (out filename :direction :output)
    (princ (csv->string csv header) out)))

(defun csv->string (csv &optional (header nil) &aux (csv (if header (cons header csv) csv)))
  "Write a CSV to a string with an optional header line (which should be a list of strings)."
  (flet ((join (lst j)
           (format nil (format nil "~~{~~A~~^~A~~}" j) lst)))
    (join (mapcar (lambda (n)
                    (join (mapcar #'csv-escape n) *fs*)) csv) *rs*)))

(defun translate-csv-file (infile outfile
                           &key (rs *rs*) (fs *fs*) (quote *quote*)
                           &aux (csv (file->csv infile)))
  (with-csv-parameters (:rs rs :fs fs :quote quote)
                       (csv->file csv outfile)))

(defmacro with-csv-parameters ((&key (rs *rs*) (fs *fs*) (quote *quote*)) &body body)
  `(let ((*rs* ,rs)
         (*fs* ,fs)
         (*quote* ,quote))
      ,@body))
