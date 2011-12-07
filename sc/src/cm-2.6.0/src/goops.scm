;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.1.1.1 $
;;; $Date: 2003/06/25 10:45:05 $

;;; after this file is loaded the following definitions should
;;; be in effect:
;;;
;;; find-class
;;; class-name
;;; class-of
;;; class-slots
;;; is-a?
;;; make-instance
;;; initialize
;;; slot-bound?
;;; slot-ref
;;; slot-set!
;;; slot-definition-name
;;; slot-definition-initargs
;;; slot-definition-initform
;;; slot-definition-reader
;;; next-method
;;; class-direct-subclasses
;;; class-direct-superclasses
;;; class-subclasses

(define (slot-getter-form obj slot)
  `(slot-ref ,obj ',slot))

(define (slot-setter-form obj slot val)
  `(slot-set! ,obj ',slot ,val))

(define (slot-definition-reader slot)
  (or (slot-definition-accessor s)
      (slot-definition-getter s)))

(define (default-slot-reader obj slot)
  `(slot-ref ,obj ',slot))

(define (slot-value-or-default obj slot . args)
  (if (and (slot-exists? obj slot)
	   (slot-bound? obj slot))
    (slot-ref obj slot)
    (car default)))

(define (class-direct-superclasses class)
  (class-direct-supers class))

(define (find-class name . root)
  ;; returns a class given its name or #f. optional
  ;; root class defaults to <top>.
  (letrec ((fc
	    (lambda (name class)
	      (if (null? class)
		#f
		(if (pair? class)
		  (or (fc name (car class))
		      (fc name (cdr class)))
		  (if (and (slot-bound? class 'name)
			   (eq? name (class-name class)))
		    class
		    (fc name (class-direct-subclasses class))))))))
    (fc name (if (null? root) <top> (car root)))))

(define slot-definition-initform slot-definition-init-value)

(define (slot-definition-initargs slotd)
  (let* ((inits (list #f))
	 (tail inits))
    (do ((l (cdr slotd) (cddr l)))
	((null? l) (cdr inits))
      (if (eq? (car l) ':init-keyword)
	(begin (set-cdr! tail (list (cadr l)))
	       (set! tail (cdr tail)))))))

;;;
;;; allow write and display to print objects using #i format
;;;

(define *print-object* #t)

(define-method (write (obj <object>) port)
  (if *print-object*
    (print-object obj port)
    (next-method)
    ))

;(define-method (print-object (obj <object>) port)
;  ;; i hope this doesnt call write! see above :)
;  (format port "~s" obj))

;(define-method (display (obj <object>) port)
;  (if *print-object*
;    (print-object obj port)
;    (next-method)))

(define (finalize-class class) #t)
