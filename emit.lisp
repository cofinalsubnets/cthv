(in-package :csv)

(export '(dump-csv))

(defun dump-csv (struct &key (rs *rs*) (fs *fs*) (quote *quote*))
  (flet ((join (lst j) (format nil (format nil "~~{~~A~~^~A~~}" j) lst)))
    (with-csv-parameters (:rs rs :fs fs :quote quote)
      (join (mapcar (lambda (n) (join (mapcar #'csv-escape n) *fs*)) struct) *rs*))))

(defun csv-escape (str)
  "Quote a string as needed for CSV output."
  (labels ((reqt (cs &aux (c (car cs)))
             (when cs (cons c (if (char= c *quote*)
                                (cons c (reqt (cdr cs)))
                                (reqt (cdr cs)))))))
    (let* ((chars (reqt (coerce str 'list)))
           (str (coerce chars 'string)))
      (if (or (member *quote* chars)
              (member *fs* chars)
              (member *rs* chars)
              (member (car chars) +whitespace+)
              (member (car (last chars)) +whitespace+))
        (format nil "~A~A~A" *quote* str *quote*)
        str))))
