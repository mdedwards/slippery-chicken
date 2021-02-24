(defpackage clm  
  (:use :common-lisp)
  #+openmcl (:import-from :ccl open-shared-library)
  )

(in-package :clm)

;;; in cmu there can be no error message and stubs must return a "value"
;;; to stop cmu's compiler from making a runtime type check for nil
;;;

(defmacro defstub (name )
  (let ((f (and (consp name) (eq (car name) 'setf))))
    `(defun ,name , (if f '(a b) '(&rest args))
       #-cmu (error "Attempt to call ~S without CLM loaded."
              ',name)
      , (if f '(values a b) '(values args)))))

(defstub clm-load)
(defstub init-with-sound)
(defstub finish-with-sound)
(defstub wsdat-play)
(defstub (setf wsdat-play))
(defmacro with-sound ((&rest args) &body body)
  args body
  (error "Attempt to call with-sound without CLM loaded."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(special mus-next mus-aifc mus-bshort mus-riff
              mus-lshort *clm-with-sound-depth* *clm-file-name*
              *clm-channels* *clm-srate* *definstrument-hook*)))

(export '(mus-next
          mus-bshort
          mus-aifc
          mus-riff
          mus-lshort
          *clm-with-sound-depth*
          wsdat-play
          init-with-sound
          finish-with-sound
          *clm-file-name*          
          *clm-channels*
          *clm-srate*
          ;;graph
          spectrum env src  ; these are also used by cm
          clm-load
          definstrument
          *definstrument-hook*
          with-sound
          )
        :CLM)

