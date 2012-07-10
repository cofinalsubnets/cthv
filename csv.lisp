(in-package :csv)

(eval-when (:load-toplevel)
  ; we're line-break agnostic for reading purposes,
  ; so we just need these strings for writing.
  (defparameter +cr+ #\Return)
  (defparameter +lf+ #\Linefeed)
  ; quotes and field separators should be characters
  ; for easy char-by-char parsing, though.
  (defparameter +comma+ #\,)
  (defparameter +colon+ #\:)
  (defparameter +tab+ #\Tab)
  (defparameter +pipe+ #\|)
  (defparameter +double-quotes+ #\")
  (defparameter +single-quote+ #\')
  ; some sane defaults 
  ; *rs* is only important when we're writing -
  ; cr, lf, and crlf will all be read the same.
  (defparameter *fs* +comma+)
  (defparameter *rs* +lf+)
  (defparameter *quote* +double-quotes+)
  ; elide whitespace adjacent to field/line delimiters?
  (defparameter *elide-whitespace* t)
  ; if so, what counts as whitespace?
  (defparameter +whitespace+ (list #\Space #\Tab)))

(defmacro with-csv-parameters ((&key (rs *rs*) (fs *fs*) (quote *quote*)) &body body)
  `(let ((*rs* ,rs)
         (*fs* ,fs)
         (*quote* ,quote))
      ,@body))

;; read:

(defun read-csv-line (in &optional (rec-p nil))
  (multiple-value-bind (field more-p) (read-csv-field in)
    (if field (cons field (when more-p (read-csv-line in t)))
      (when rec-p (list "")))))

(defun read-raw-csv-field (in &aux (more-p t))
  (labels ((qtd (&aux (c (read-char in nil nil t)))
             (if (null c) (error "Unexpected EOF while reading quoted extent.")
               (cons c (if (char= c *quote*) (nqtd) (qtd)))))
           (nqtd (&aux (c (read-char in nil nil t)))
             (cond ((or (null c) (char= c *rs*)) (setf more-p nil))
                   ((char= c *fs*) nil)
                   (t (cons c (if (char= c *quote*) (qtd) (nqtd)))))))
    (when (peek-char nil in nil)
      (values (coerce (nqtd) 'string) more-p))))

(defun read-csv-field (in)
  (labels ((qtd (cs &aux (c (car cs)))
             (if (char= c *quote*)
               (if (and (cadr cs) (char= (cadr cs) *quote*))
                 (cons *quote* (qtd (cddr cs)))
                 (nqtd (cdr cs)))
               (cons c (qtd (cdr cs)))))
           (nqtd (cs &aux (c (car cs)))
             (when c
               (if (char= c *quote*)
                 (qtd (cdr cs))
                 (cons c (nqtd (cdr cs)))))))
    (multiple-value-bind (raw more-p) (read-raw-csv-field in)
      (when raw (values
                  (coerce (nqtd (coerce
                                  (if *elide-whitespace*
                                    (string-trim +whitespace+ raw)
                                    raw)
                                  'list))
                          'string)
                  more-p)))))

;; write:

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
