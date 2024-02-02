;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; utilities.lsp
;;;
;;; NAME
;;; utilities
;;;
;;; DESCRIPTION
;;; This file adds helper functions to the slippery-chicken package in order to
;;; be used with the Emacs sc-mode. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-12-19
;;;
;;; $$ Last modified:  11:23:45 Fri Feb  2 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* emacs-utilities/validate-rthm-seq
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-20
;;; 
;;; DESCRIPTION
;;; Tests if a given list can be used to instantiate a rthm-seq object.
;;; This is done by calling the make-rthm-seq function and reporting whether
;;; the instantiation process has been successful or erraneous without actually
;;; throwing a Common Lisp error.
;;;
;;; ARGUMENTS
;;; The rthm-seq list as described in the slippery-chicken documentation.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :auto-convert. Automatically convert string input to a list? Must be a
;;;   boolean. Default: T.
;;; 
;;; RETURN VALUE
;;; Either T, when the rthm-seq has been successfully validated, or the values:
;;; - NIL
;;; - The error condition.
;;;
;;; EXAMPLE
#|
(validate-rthm-seq '((((3 4) q q q))
                     :pitch-seq-palette (1 2 3)))

;; => T

(validate-rthm-seq '((((3 4) q (q) q))
                     :pitch-seq-palette (1 2 3)))

;; => NIL, #<SIMPLE-ERROR "pitch-seq-palette::verify-and-store: ~%~
;;                  In pitch-seq ~a from palette ~a:~%~
;;                  Each pitch sequence must have ~a notes (you have ~a):
;;                  ~%~a" {7009EB3043}>
|#
;;; SYNOPSIS
(defun validate-rthm-seq (rs &key (auto-convert t))
;;; ****
  ;;; convert from string to list
  (let ((rs (if (and
                 (stringp rs)
                 auto-convert)
                (read-from-string rs)
                rs)))
    (multiple-value-bind (result condition)
        (handler-case
            (make-rthm-seq rs)
          (error (c)
            (values nil c)))
      (if result
          t
          (values nil condition)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lsp
