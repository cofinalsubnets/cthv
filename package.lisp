(defpackage csv
  (:use cl)
  (:shadow load)
  (:export load
           load-file
           dump
           generate
           row
           with-csv-parameters))
