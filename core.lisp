(in-package :csv)

(eval-when (:load-toplevel)
  (defparameter *fs* #\,)
  (defparameter *rs* #\Linefeed)
  (defparameter *quote* #\")
  ; elide whitespace adjacent to field/line delimiters?
  (defparameter *elide-whitespace* t)
  ; if so, what counts as whitespace?
  (defparameter +whitespace+ (list #\Space #\Tab)))

(defmacro with-csv-parameters ((&key (rs *rs*) (fs *fs*) (quote *quote*)) &body body)
  `(let ((*rs* ,rs)
         (*fs* ,fs)
         (*quote* ,quote))
      ,@body))
