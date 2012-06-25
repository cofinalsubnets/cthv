(in-package :csv)

(export '(*fs*
          *rs*
          +cr+
          +lf+
          +crlf+
          file->csv
          csv->file
          string->csv
          csv->string
          read-csv-field
          read-csv-line))

(eval-when (:load-toplevel)
  (defparameter +cr+ #\Return)
  (defparameter +lf+ #\Linefeed)
  (defparameter +crlf+ (list #\Return #\Linefeed))
  (defparameter +comma+ #\,)
  (defparameter +colon+ #\:)
  (defparameter +tab+ #\Tab)
  (defparameter +pipe+ #\|)
  (defparameter +double-quotes+ #\")
  (defparameter +single-quote+ #\'))

(defvar *fs* +comma+)
(defvar *rs* +lf+)
(defvar *quote* +double-quotes+)
(defvar *drop-whitespace-p* t)
(defvar *drop-empty-rows-p* t)
(defvar +whitespace+ (list #\Tab #\Space))

;; read

(defun file->csv (filename)
  (with-open-file (csv-file filename :direction :input)
    (loop for row = (read-csv-line csv-file)
          while row collect row)))

(defun string->csv (str)
  (with-input-from-string (csv-string str)
    (loop for row = (read-csv-line csv-string)
          while row collect row)))

(defun read-csv-line (in &aux (field nil) (term nil))
  (loop do (multiple-value-setq (field term) (read-csv-field in))
        when (and (null term) (null field) (null line)) return nil
        when field collect field into line
        when (or (null term) (eq t term)) return line))

(defun read-csv-field (in &aux (out (make-string-output-stream)) (quoted? nil))
  (labels ((stringify (c) (write-char c out))
           (quote! nil (setf quoted? t))
           (unquote! nil (setf quoted? nil))
           (terminator? (c) (or (null c) (char= c *fs*) (newline? c)))
           (next nil (peek-char nil in nil))
           (newline? (c) (cond ((characterp *rs*) (char= c *rs*))
                               ((or (not (listp *rs*))
                                    (> (length *rs*) 2))
                                (error "Unsupported line separator `~A'" *rs*))
                               (t (and (next)
                                       (char= c (first *rs*))
                                       (char= (next) (second *rs*)))))))
    (declare (inline stringify quote! unquote terminator? next))
    (loop for c = (read-char in nil nil t)
          initially (when (null (peek-char nil in nil)) (return nil)) ; return nil if we're already at EOF
          if quoted? do (cond
                          ((null c)
                           (error "Unexpected end of stream while reading `~A'"
                                  (get-output-stream-string out))) ; EOF should not occur inside of quotes
                          ((quote? c)
                           (if (quote? (next))
                             (stringify (read-char in)) ; double-quotes while quoted are stringified as a single quote.
                             (unquote!)))
                          (t (stringify c)))
          else do (cond
                    ((terminator? c) ;end-of-field?
                     (let ((str (get-output-stream-string out))) ;return field contents we've read so far, and: if eof, nil; else if eol, t; else the last char we read
                       (return (values (string-trim (and *drop-whitespace-p* +whitespace+) str) (or (and c (newline? c)) c)))))
                    ((quote? c)
                     (quote!))
                    (t (stringify c))))))

;; write

(defun csv->file (csv filename &optional (header nil))
  (with-open-file (out filename :direction :output)
    (princ (csv->string csv header) out)))

(defun csv->string (csv &optional (header nil))
  (let ((csv (if header
               (cons header csv)
               csv))
        (*rs* (if (characterp *rs*)
                *rs*
                (coerce *rs* 'string))))
   (join (mapcar (lambda (n)
                  (join (mapcar #'csv-escape n) *fs*)) csv) *rs*)))


(defun csv-escape (string &aux (string (join (split string *quote*) *quote*)))
  (if (or (position *quote* string)
          (position *fs* string)
          (position *rs* string))
    (wrap string *quote*)
    string))

;some little helper fns

(defun quote? (c)
  (when (characterp c) (char= c *quote*)))
(defun whitespace? (c)
  (position c *whitespace*))
(defun cat (&rest ss)
  (apply #'concatenate 'string ss))
(defun wrap (str w)
  (format nil "~A~A~A" w str w))
(defun join (lst j)
  (format nil (format nil "~~{~~A~~^~A~~}" j) lst))

(defun split (string char &optional (remove-p nil) &aux (last (1- (length string))))
  (loop for start = 0 then pos
        for pos = (position char string) then (position char string :start (1+ start))
        collect (subseq string start pos) into subs
        unless pos return subs
        when (= pos last) collect (subseq string pos (1+ pos)) into subs and return subs))
