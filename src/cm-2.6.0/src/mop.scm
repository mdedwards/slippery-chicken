;;; **********************************************************************
;;; 
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
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
;;; $Revision: 1.2 $
;;; $Date: 2003/12/05 19:54:07 $

;;;
;;; warning: goops has a bug that givs a segmentation error if
;;; a metaclasses and its dependants are redefined.
;;;

;;;
;;; metaclass for events (classes with parameters).
;;;

(define-class <parameterized-class> (<class>)
  (pars :init-value '() :init-keyword :parameters
	:accessor class-parameters)
  :name 'parameterized-class)

(define-method (validate-superclass (class <parameterized-class>)
                                    (superclass <class>))
  ;; yes to any class w/metaclass parmeterized-class
  #t)

;;;
;;; default method returns nil
;;;

(define-method (class-parameters (obj <top>))
  obj
  #f)

(define (object-parameters obj)
  (let ((x (class-parameters (class-of obj))))
    (if (pair? x)
      (map #'car x)
      x)))

;;;
;;; metaclass for io classes. :file-types is a list of file types that
;;; the io class recognizes. :mime-type is the MIME type (currently
;;; unused) :output-hook is #f or a function to call after a file of
;;; that type is written. definer is a function that computes a 
;;; write-event method for a class with parameters. see clm.sco for an
;;; example of this.

(define-class <io-class> (<class>)
  (handles :init-value '() :init-keyword :file-types 
	   :accessor io-class-file-types)
  (mime-type :init-value #f :accessor io-class-mime-type
             :init-keyword :mime-type )
  (output-hook :init-value #f :init-keyword :output-hook
	       :accessor io-class-output-hook)
  (definer :init-value #f :init-keyword :definer
    :accessor io-class-definer)
  (versions :init-value #f :init-keyword :versions
	    :accessor io-class-file-versions)
  :name 'io-class)

(define-method (validate-superclass (class <io-class>)
                                    (superclass <class>))
  ;; yes to any class w/metaclass io-class
  #t)

;;;
;;; default methods returns false. these are overridden in
;;; level2.lisp for the #-no-metaclass

(define-method (io-class-file-types x) x #f)
(define-method (io-class-output-hook x) x #f)
(define-method (io-class-definer x) x #f)
(define-method (io-class-file-versions x) x #f)

;;;
;;; parses initialization list for class. the list is a list of
;;; pairs (init value ...) where each init can be either a keyword
;;; or symbol.
;;; if inits? is #t a list  (#:init value ...) is returned.
;;; if inits? is #f a list (slot value ...) is returned.
;;; if other? is #t then unrecognized inits are returned as a
;;; second value from the function, otherwise an error is signaled.

(define (expand-inits class args inits? other?)
  ;; parse args as possible initargs for class. args is
  ;; a list of pairs (init val ...) where init can be
  ;; be symbol or keyword. if inits? is #t then keyword
  ;; forms of init is returned regardless of how it was
  ;; specified in args. if inits? is #f then the slot for
  ;; :init is returned instead of :init. if other? is #f
  ;; then its an error to specify anything but initargs.
  ;; if other? is :alias then :init is returned if it 
  ;; is an alternate initarg for the slot else an error
  ;; is signaled. if alias? is #t then :init is returned
  ;; and no error is signaled.
  (let* ((slots (class-slots class))
	 (inits (list #f))
	 (tail1 inits)
	 (other (if other? (list #f) #f))
	 (tail2 other)
	 (save args))
    (do ((sym #f)
	 (val #f)
	 (slot #f))
	((null? args)
	 (if other?
	   (values (cdr inits) (cdr other))
	   (cdr inits)))
      (set! sym (pop args))
      (set! val (if (null? args)
		  (err "Uneven initialization list: ~s" save)
		  (pop args)))
      (cond ((keyword? sym) )
	    ((symbol? sym)
	     (set! sym (symbol->keyword sym)))
	    (else
	     (err "'~s' is not an initialization for ~s: ~s."
		  sym class save)))
      (set! slot (find sym slots ':key #'slot-definition-initargs
		       :test #'member))
      (if slot
	(begin
	 (set-cdr! tail1 (list (if inits?
				 sym
				 (slot-definition-name slot))
			       val))
	 (set! tail1 (cddr tail1)))
	(if other?
	  (begin (set-cdr! tail2 (list sym val))
		 (set! tail2 (cddr tail2)))
	  (err "'~s' is not an initialization for ~s."
	       sym class))))))

(define (slot-init-forms o . args)
  ;; returns an initialization list given an object.
  ;; if :eval is #t then non-constant values in the list
  ;; are quoted. if ignore is provide it is a list of
  ;; slotnames to omit from the list returned.
  (with-args (args &key eval omit only key 
                   ignore-defaults)
    (loop for s in (class-slots (class-of o))
          for n = (slot-definition-name s)
          for k = (slot-definition-initargs s)
          for v = (if (slot-bound? o n) (slot-ref o n) 
                      ':unbound-slot)
          when (and (not (eq? v ':unbound-slot))
                    (not (null? k))
                    (if omit
                      (not (memq n omit))
                      (if only (memq n only)
                          #t))
                    (not (and ignore-defaults 
                              (eq? v (slot-definition-initform s)))))
          collect (car k)
          and collect (if key
                        ( key v) ; funcall
                        (if eval
                          (quote-if-necessary v)
                          v)))))

;;;
;;; make-load-form returns an expression that, if evaluated, creates
;;; an object equivalent to the object that produced the form.
;;; assumes the object's class is in a variable named <object>.
;;; this should probably be changed to use find-class.
;;;

(define-method (make-load-form (obj <class>))
  (let ((inits (slot-init-forms obj :eval #t)))
    `(make-instance ,(string->symbol
	              (format #f "<~a>" (class-name (class-of obj))))
       ,@inits)))

(define-method (describe-object (x <object>) )
  (let ((c (class-of x)))
    (format #t "~%Class: ~s" (class-name c))
    (format #t "~%CPL:   ~s" (map #'class-name (compute-cpl c)))
    (if (not (is-a? x <class>))
      (begin
       (format #t "~%Slots:")
       (dolist (s (class-slots c))
	 (let* ((n (slot-definition-name s))
		(l (symbol->string n)))
	   (newline)
	   (display "  ")
	   (display l)
	   (dotimes (i (- 20 (string-length l)))
	     (write-char #\space))
	   (if (slot-bound? x n)
	     (write (slot-ref x n))
	     (display "#<unbound>"))))))
    (newline)
    (values)))
