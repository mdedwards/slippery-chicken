;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.4 $
;;; $Date: 2004/11/20 03:22:29 $

;;; a few of these that cm uses are defined in level1.lisp
;;; cm cltl sources do not depend on any definitions in this file

(in-package :cm)

(defmacro defscheme (scheme cltl &rest rest)
  (if (null rest)
    `(setf (symbol-function ',scheme)
           (symbol-function ',cltl))
    `(defun ,scheme ,cltl ,@rest)))

;;;
;;; General
;;;

(defmacro define (form &rest def)
  (flet ((expand-scheme-defun (spec body)
           (let ((args '()))
             (loop for i from 0
                   until (null spec)
                   do
                   (cond ((consp spec)
                          (unless (and (car spec)
                                       (symbolp (car spec)))
                            (error "Define: bad function ~
                                    ~:[name~;parameter~]: ~s."
                                   (> i 0) (car spec)))
                          (push (car spec) args)
                          (setf spec (cdr spec)))
                         ((and spec (symbolp spec))
                          (push '&rest args)
                          (push spec args)
                          (setf spec '()))
                         (t
                          (error "Define: bad function ~
                                  ~:[name~;parameter~]: ~s."
                                 (> i 0) spec))))
             (setf args (reverse args))
             `(progn
                (defun ,(pop args) ,args ,@body)
                (values)))))
    (if (symbolp form)
      (if (null def)
        (error "Define: missing value." )
        (if (null (cdr def))
          `(progn (defparameter ,form ,(car def))
                  (values ))
          (error "Define: too many values: ~s" def)))
      (if (and (consp form)
               (symbolp (car form)))
        (if (null def)
          (error "Define: missing body of function define.")
          (expand-scheme-defun form def))
        (error "Define: ~S not a variable or ~
                function specification."
               form)))));;;
(defmacro set! (a b) 
  `(progn (setf ,a ,b) ; guile has genealized set!
          (values)))

(defmacro begin (&rest args) 
  `(progn ,@args))

(defscheme for-each mapc )

(defscheme display (x &optional (p t))
  (format p "~A" x))

;;;
;;; General equivalence
;;;

(defscheme eq? eq)

(defscheme eqv? eql)

(defscheme equal? equal)


;;;
;;; booleans
;;;

(set-dispatch-macro-character #\# #\f #'(lambda (&rest a) a nil) 
                              *cm-readtable*)
(set-dispatch-macro-character #\# #\t #'(lambda (&rest a) a t)
                              *cm-readtable*)

(defscheme boolean? (x)
  (or (eq x t) (eq x nil)))

;;;
;;; symbols
;;;

(defscheme symbol? (x) (and x (symbolp x)))

(defscheme symbol->string symbol-name)

(defscheme string->symbol (x) (intern x))

;;;
;;; numbers
;;;

(defscheme number? numberp)

(defscheme integer? (x)
  (or (integerp x)
      (= (truncate x) x)))

(defscheme real? (x)
  (or (integerp x)
      (floatp x)))

(defscheme rational? rationalp)

(defscheme complex? complexp)

(defscheme exact? integerp)

(defscheme inexact? floatp)

(defscheme zero? zerop)

(defscheme positive? plusp)

(defscheme negative? minusp)

(defscheme even? evenp)        ; not really true

(defscheme odd? oddp)          ; not really true

(defscheme modulo mod)

(defscheme inexact->exact (x)
  (values (round x)))

(defscheme exact->inexact (x)
  (* x 1.0))

(defscheme modulo mod)

(defscheme remainder rem)

(defscheme number->string (n)
  (if (numberp n)
    (format nil "~s" n)
    (error "Not a number: ~S" n)))

(defscheme string->number (s)
  (let ((n (read-from-string s)))
    (if (numberp n)
      n
      (error "Can't read number from ~s." s))))

;;;
;;; characters
;;;

(defscheme char? characterp)

(defscheme char=? char=)

(defscheme char->integer char-code)

(defscheme integer->char code-char)

(defscheme char=? char=)

(defscheme char<? char<)

(defscheme char>? char>)

(defscheme char<=? char<= )

(defscheme char>=? char>=)

(defscheme char-ci=? char-equal)

(defscheme char-ci<? char-lessp)

(defscheme char-ci>? char-greaterp)

(defscheme char-ci<=? char-not-greaterp)

(defscheme char-ci>=? char-not-lessp)

(defscheme char-alphabetic? alpha-char-p)

(defscheme char-numeric? digit-char-p)

(defscheme char-whitespace? (c)
  (find c '(#\space #\tab #\return)))

(defscheme char-upper-case? upper-case-p)

(defscheme char-lower-case? lower-case-p)

;;;
;;; lists
;;;

(defscheme null? null)

(defscheme list? listp) ; not really true...

(defscheme pair? consp)

(defscheme list-ref (list pos)
  (nth pos list))

(defscheme list-set! (list pos val)
  (setf (nth pos list) val)
  (values))

(defscheme list-tail (l i)
  (nthcdr i l))

(defscheme last-pair last )

(defscheme set-cdr! (list val)
  (setf (cdr list) val)
  (values))

(defscheme set-car! (list val)
  (setf (car list) val)
  (values))

(defscheme memq (x l)
  (member x l :test #'eq))

(defscheme memv (x l)
  (member x l :test #'eql))

(defscheme assq (x l)
  (assoc x l :test #'eq))

(defscheme assv (x l)
  (assoc x l :test #'eql))

(defun sort! (a b) (sort a b))

(defscheme reverse! nreverse)

;;;
;;; vectors
;;;

(defscheme vector? vectorp)

(defscheme vector-ref elt)

(defscheme vector-length length)

(defscheme vector-set! (vec ind val) 
  (setf (elt vec ind) val)
  (values))

(defscheme vector->list (v)
  (coerce v 'list))

(defscheme list->vector (l)
  (coerce l 'vector))

;;;
;;; strings
;;;

(defscheme string? stringp)

(defscheme string-ref elt)

(defscheme string-length length)

(defscheme string-set! (str ind val) 
  (setf (elt str ind) val)
  (values))

(defscheme string-append (&rest strs)
  (apply #'concatenate 'string strs))

(defscheme substring (s b e)
  (subseq s b e))

(defscheme list->string (l)
  (coerce l 'string))

(defscheme string->list (s)
  (coerce s 'list))

(defscheme string-copy copy-seq)

(defscheme string-fill (s c)
  (fill s c))

;;;
;;; objects
;;;

(defscheme is-a? (x typ) (typep x typ))

(defscheme slot-ref slot-value)


;;;
;;; needed for testing.cm
;;;

(defscheme file-exists? probe-file)
(defscheme hash-clear! clrhash)

