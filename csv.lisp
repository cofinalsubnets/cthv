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
  ; we're line-break agnostic for reading purposes,
  ; so we just need these strings for writing.
  (defparameter +cr+ (string #\Return))
  (defparameter +lf+ (string #\Linefeed))
  (defparameter +crlf+ (format nil "~A~A" #\Return #\Linefeed))
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
  (defvar *fs* +comma+)
  (defvar *rs* +lf+)
  (defvar *quote* +double-quotes+)
  ; elide blank lines?
  (defvar *elide-empty-rows* t)
  ; elide whitespace adjacent to field/line delimiters?
  (defvar *elide-whitespace* t)
  ; if so, what counts as whitespace?
  (defparameter +whitespace+ (list #\Space #\Tab)))

(defmacro with-csv-parameters ((&key (rs *rs*) (fs *fs*) (quote *quote*)) &body body)
  `(let ((*rs* ,rs)
         (*fs* ,fs)
         (*quote* ,quote))
      ,@body))

;; read:
(defun file->csv (filename)
  (with-open-file (csv-file filename :direction :input)
    (loop for row = (read-csv-line csv-file)
          while row collect row)))

(defun string->csv (str)
  (with-input-from-string (csv-string str)
    (loop for row = (read-csv-line csv-string)
          while row collect row)))

(defun read-csv-line (in &aux (field nil) (term nil))
  "Read one CSV line from <in> and return a list of strings representing its contents."
  (loop do (multiple-value-setq (field term) (read-csv-field in))
        unless field return nil
        collect field into line
        when (member term (list #\Return #\Linefeed nil)) return line))

(defun read-csv-field (in &aux (out (make-string-output-stream)) (quoted? nil))
  "Read one CSV field from <in> and return a string representing its contents."
  (labels ((stringify (c) (write-char c out))
           (quote! nil (setf quoted? t))
           (unquote! nil (setf quoted? nil))
           (next nil (peek-char nil in nil))
           (quote? (c) (char= c *quote*))
           (terminator? (c)
             (or (null c)
                 (char= c *fs*)
                 (newline? c)))
           (newline? (c)
             "Return T if c is CR or LF. If c is CR and the next character is LF, burn the next character."
             (if (char= c #\Linefeed) t
               (if (char= c #\Return)
                 (prog1 t
                   (when (eq (peek-char nil in nil) #\Linefeed)
                     (read-char in)))))))
    (declare (inline stringify quote! unquote terminator? next))
    (loop for c = (read-char in nil nil t)
          initially (when (null (peek-char nil in nil)) (return nil)) ; return nil if we're already at EOF
          if quoted? do (cond
                          ((null c)
                           (error "Unexpected EOF while reading `~A'"
                                  (get-output-stream-string out))) ; EOF should not occur inside of quotes
                          ((quote? c)
                           (if (quote? (next))
                             (stringify (read-char in)) ; double-quotes while quoted are stringified as a single quote.
                             (unquote!)))
                          (t (stringify c)))
          else do (cond
                    ((terminator? c)
                     (let ((str (get-output-stream-string out))) ;return field contents we've read so far, and: if eof, nil; else if eol, t; else the last char we read
                       (return (values (string-trim +whitespace+ str) c))))
                    ((quote? c) (quote!))
                    (t (stringify c))))))

;; write:
(defun csv->file (csv filename &optional (header nil))
  "Write a CSV to the named file with an optional header line (which should be a list of strings)."
  (with-open-file (out filename :direction :output)
    (princ (csv->string csv header) out)))

(defun csv->string (csv &optional (header nil))
  "Write a CSV to a string with an optional header line (which should be a list of strings)."
  (let ((csv (if header
               (cons header csv)
               csv)))
   (join (mapcar (lambda (n)
                  (join (mapcar #'csv-escape n) *fs*)) csv) *rs*)))

(defun csv-escape (string &aux (string (join (split string *quote*) *quote*)))
  "Quote a string as needed for CSV output."
  (if (or (position *quote* string)
          (position *fs* string)
          (position *rs* string))
    (wrap string *quote*)
    string))

(defun translate-csv-file (infile outfile
                           &key (rs *rs*) (fs *fs*) (quote *quote*)
                           &aux (csv (file->csv infile)))
  (with-csv-parameters (:rs rs :fs fs :quote quote)
                       (csv->file csv outfile)))

;some little helper fns
(defun wrap (str w)
  (format nil "~A~A~A" w str w))

(defun join (lst j)
  (format nil (format nil "~~{~~A~~^~A~~}" j) lst))

(defun split (string char &aux (last (1- (length string))))
  "Splits string on char into a list of strings each beginning with char."
  (loop for start = 0 then pos
        for pos = (position char string) then (position char string :start (1+ start))
        collect (subseq string start pos) into subs
        unless pos return subs
        when (= pos last) collect (subseq string pos (1+ pos)) into subs and return subs))
