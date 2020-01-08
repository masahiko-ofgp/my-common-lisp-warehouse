; This file use cl-fad.
; $ ros install cl-fad

(defpackage :listdir
  (:use :cl
        :asdf)
  (:export :dirs))
(in-package :listdir)
(ql:quickload :cl-fad)

(defun create-path (dirname)
  (if (eql "." dirname)
    (truename "./")
    (pathname dirname)))

(defun dirs (dirname)
  (let ((path (create-path dirname)))
    (cl-fad:list-directory path)))
