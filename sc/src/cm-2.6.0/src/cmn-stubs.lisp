(defpackage :cmn
  (:use :common-lisp))

(in-package :cmn)

(defmacro defstub (name )
  (let ((f (and (consp name) (eq (car name) 'setf))))
    `(defun ,name , (if f '(a b) '(&rest args))
       #-cmu (error "Attempt to call ~S without CMN loaded."
              ',name)
      , (if f '(values a b) '(values args)))))

(defstub init-clm-input)
(defstub stfdat-staff )
(defgeneric staff-data (x) )
(defgeneric (setf staff-data) (a b))
(defstub set-staff-number)
(defstub set-staff-clef)
(defstub finish-clm-input)
(defstub find-staff)
(defstub add-staff )
(defstub add-data-1)
(defstub add-note-to-staff)

(proclaim '(special *exact-rhythms* staff-descriptors))

(export '(*exact-rhythms*
          staff-descriptors
          init-clm-input
          score
          staff-descriptors
          stfdat-staff 
          staff-data 
          set-staff-number
          set-staff-clef
          finish-clm-input
          find-staff
          add-staff 
          add-data-1
          add-note-to-staff))
