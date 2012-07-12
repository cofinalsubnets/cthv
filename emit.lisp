(in-package :csv)

(defun dump (struct &key (rs *rs*) (fs *fs*) (quote *quote*) (to-file nil))
  (flet ((join (lst j) (format nil (format nil "~~{~~A~~^~A~~}" j) lst))
         (stringify (e) (csv-escape (if (stringp e) e
                                      (format nil "~A" e))))
         (validate (l) (and (listp l)
                            (every (lambda (e) (and (listp e)
                                                    (every #'atom e))) l))))
    (unless (validate struct) (error "DUMP-CSV: Argument is not a list of lists of atoms"))
    (let ((dump (with-csv-parameters (:rs rs :fs fs :quote quote)
                  (join (mapcar (lambda (n) (join (mapcar #'stringify n) *fs*)) struct) *rs*))))
      (if to-file
        (with-open-file (f to-file :direction :output)
          (princ dump f)))
      dump)))

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

(defmacro generate ((&key (rs *rs*) (fs *fs*) (quote *quote*) (to-file nil)) &body body)
  (let ((ref (gensym)))
    `(let ((,ref nil))
       (flet ((row (l) (setf ,ref (append ,ref (list l)))))
         ,@body
         (dump-csv ,ref :rs ,rs :fs ,fs :quote ,quote :to-file ,to-file)))))

