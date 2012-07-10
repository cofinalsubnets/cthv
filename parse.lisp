(in-package :csv)

(export '(load-csv
          load-csv-file))

(defun load-csv-file (filename &key (rs *rs*) (fs *fs*) (quote *quote*))
  (with-open-file (f filename)
    (load-csv f :rs rs :fs fs :quote quote)))

(defun load-csv (stream &key (rs *rs*) (fs *fs*) (quote *quote*))
  (with-csv-parameters (:rs rs :fs fs :quote quote)
    (read-csv stream)))

(defun read-csv (in &aux (line (read-csv-line in)))
  (when line (cons line (read-csv in))))

(defun read-csv-line (in &optional (rec-p nil))
  (multiple-value-bind (field more-p) (read-csv-field in)
    (if field (cons field (when more-p (read-csv-line in t)))
      ;; if the fn was called recursively then we haven't actually
      ;; reached EOL, so return an empty string or we'll drop
      ;; trailing commas.
      (when rec-p (list "")))))

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
                  (coerce (nqtd (coerce (if *elide-whitespace*
                                          (string-trim +whitespace+ raw)
                                          raw)
                                        'list))
                          'string)
                  more-p)))))

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
