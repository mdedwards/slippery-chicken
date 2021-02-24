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
;;; $Revision: 1.13 $
;;; $Date: 2005/03/22 17:24:36 $

(define %months #("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
		  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (date-and-time)
  (let ((vect (get-current-time))
	(fnum (lambda (n)
		(if (< n 10)
		  (string-append "0" (number->string n))
		  (number->string n)))))
    (string-append (fnum (vector-ref vect 3))               ; day
                   " "
                   (vector-ref %months (vector-ref vect 4)) ; month
                   " "
                   (fnum (vector-ref vect 5))               ; year
                   ", "
                   (fnum (vector-ref vect 2))               ; hour
                   ":" 
                   (fnum (vector-ref vect 1))               ; min
                   ":" 
                   (fnum (vector-ref vect 0)))))            ; sec

;;;
;;; list utilities
;;;

(define (listify x)
  (if (list? x) x (list x)))

(define (end list)
  (car (last-pair list)))

(defmacro dopairs (decl . body)
  (let* ((m "dopairs: (v1 v2 list [return]) . body")
	 (s (if (pair? decl) (pop decl) (err m)))
	 (v (if (pair? decl) (pop decl) (err m)))
	 (l (if (pair? decl) (pop decl) (err m)))
	 (x (if (pair? decl) (pop decl) #f))
	 (a (gensym))
	 (h (gensym)))
    `(let ((,h ,l))
      (do ((,a ,h (cddr ,a))
	   (,s #f)
	   (,v #f))
	  ((null? ,a) ,x)
	(set! ,s (car ,a))
	(if (null? (cdr ,a))
	  (err "Uneven pair list: ~s" ,h)
	  (set! ,v (cadr ,a)))
	,@ body))))

(defmacro make-cycl () 
  `(make-list 2))

(defmacro cycl-data (cycl)
  `(car ,cycl))

(defmacro cycl-data-set! (cycl data)
  `(set-car! ,cycl ,data))

(defmacro cycl-last (cycl)
  `(cadr ,cycl)) 

(defmacro cycl-last-set! (cycl data)
  `(set-car! (cdr ,cycl) ,data))

(defmacro cycl-tail (cycl)
  `(cddr ,cycl))

(defmacro cycl-tail-set! (cycl tail)
  `(set-cdr! (cdr ,cycl) ,tail)) ; set-cddr!

(defmacro pop-cycl (cycl)
  `(cdr-pop (cdr ,cycl)))

(defmacro reset-cycl (cycl)
  (let ((c (gensym)))
    `(let ((,c ,cycl))
       (cycl-tail-set! ,c (car ,c)))))

;(define (make-cycl) 
;  (make-list 2))
;
;(define (cycl-data cycl)
;  (car cycl))
;
;(define (cycl-data-set! cycl data)
;  (set-car! cycl data))
;
;(define (cycl-last cycl)
;  (cadr cycl)) 
;
;(define (cycl-last-set! cycl data)
;  (set-car! (cdr cycl) data))
;
;(define (cycl-tail cycl)
;  (cddr cycl))
;
;(define (cycl-tail-set! cycl tail)
;  (set-cdr! (cdr cycl) tail)) ; set-cddr!
;
;(define (pop-cycl cycl)
;  (cdr-pop (cdr cycl)))
;
;(define (reset-cycl cycl)
;  (cycl-tail-set! cycl (car cycl)))

;(defmacro cycl-append (x cycl)
;  ;; append new thing to data.
;  (let ((var (gensym))
;        (new (gensym)))
;    `(let ((,var ,cycl)
;           (,new (list ,x)))
;       (if (null? (cycl-data ,var))
;	 (begin
;           (cycl-data-set! ,var ,new)
;	   (cycl-last-set! ,var ,new))
;	 (begin 
;	  (set-cdr! (cycl-last ,var) ,new)
;	  (cycl-last-set! ,var (cdr (cycl-last ,var)))))
;       ,var)))
;
;(defmacro cycl-insert (cycl element . args)
;  ;; accessor is object-time normally, but midifiles enqueue pending
;  ;; note offs as cons cells (time . message)
;  ;; if earliest is NIL then the object is placed at the last possible
;  ;; time avaliable so that objects of the same time are run in the
;  ;; order the user specified.  noteOff sheduling, on the other hand,
;  ;; wants to place a future note off at the earliest time possible
;  (let ((accessor (if (null? args) 'object-time (pop args)))
;	(earliest (if (null? args) #f (car args)))
;	(evar (gensym))
;        (bvar (gensym)))
;    `(let ((,evar ,element)
;           (,bvar ,cycl))
;       (cond ((null? (cycl-tail ,bvar))
;              (cycl-tail-set! ,bvar (list ,evar ))
;              (cycl-last-set! ,bvar (cycl-tail ,bvar)))
;             (else
;	      (let ((time (,accessor ,evar)))
;                (cond ((, (if earliest '> '>=) 
;                          time 
;                          (,accessor (car (cycl-last ,bvar))))
;		       (set-cdr! (cycl-last ,bvar) (list ,evar))
;                       (cycl-last-set! ,bvar (cdr (cycl-last ,bvar))))
;                      ((, (if earliest '<= '< )
;                          time 
;                          (,accessor (car (cycl-tail ,bvar))))
;		       (cycl-tail-set! ,bvar (cons ,evar (cycl-tail ,bvar))))
;                      (else
;		       (let ((tail (cycl-tail ,bvar)))
;			 (do ((head (cdr tail) (cdr tail)))
;			     ((or (null? head)
;				  (not (,(if earliest '< '<=)
;					 (,accessor (car head))
;					 time)))
;			      (set-cdr! tail (cons ,evar head)))
;			   (set! tail head))))))))
;       ,evar)))

;;;
;;;----------------------------------------------------
;;; printing

(define (tell-user . args) 
  (let ((str (apply (function format) #f args))
        (pad "; "))
    (newline)
    (display pad)
    (loop for i below (string-length str)
          for c = (string-ref str i)
          do
          (write-char c)
          (if (char=? c #\newline)
            (display pad)))
    (newline)
    (values)))

(define (warning . args)
  (let ((str (apply (function format) #f args))
        (pad ";          "))
    (newline)
    (display "; WARNING: ")
    (loop for i below (string-length str)
          for c = (string-ref str i)
          do
          (write-char c)
          (if (char=? c #\newline)
            (display pad)))
    (newline)
    (values)))

(define (format-integer int field pad)
  ;; if field is negative then left justify
  (let* ((str (number->string int))
	 (len (string-length str))
	 (wid (abs field)))
    (if (< len wid)
      (let* ((d (- wid len))
	     (s (make-string wid pad)))
	(if (< field 0)			; left justify
	  (dotimes (i len)
	    (string-set! s i (string-ref str i)))
	  (dotimes (i len)
	    (string-set! s d (string-ref str i))
	    (set! d (+ d 1))))
	s)
      str)))

(define (address->string w) ; uses 32 bit word
  (do ((s (make-string 8))
       (n 28 (- n 4))
       (i 0 (+ i 1))
       (c #f))
      ((< n 0) s)
    (set! c (ash (logand w (ash #xf n)) (- n)))
    (string-set! s i (integer->char (if (< c 10) (+ 48 c) (+ 55 c))))))


;(define (address->string w) ; uses 32 bit word
;  (do ((n 28 (- n 4))
;       (c 0)
;       (l '()))
;      ((< n 0) (list->string (reverse l)))
;    (set! c (ash (logand w (ash #xf n)) (- n)))
;    (push (integer->char (if (< c 10) (+ 48 c) (+ 55 c)))
;          l)))

(define (quotify token)
  (string-append "\""
		 (if (string? token) token
		     (symbol->string token))
		 "\""))

(define (quote-if-necessary x)
  (if (or (number? x)
	  (string? x)
	  (vector? x)
	  (and (pair? x)
	       (eq? (car x) 'quote)))
    x
    `(quote ,x)))

;;;
;;; this is also in level1.scm because the routines there need it
;;;

(define (parse-lambda-list pars)
  ;; parse a cltl2 parameter declaration into seperate lists. modified 
  ;; to allow either cltl2 or guile style type decls, ie &key or :key
  (let ((mode '&required)
        (reqs '())
        (opts '())
        (rest '())
        (keys '())
        (auxs '())
        (aok? #f)    ; allow other keys
        (bind
	 (lambda (par)
	   (if (pair? par)
	     (begin
	      (unless (symbol? (car par))
		(err "Not a lambda parameter: ~s" 
		     (car par)))
	      (unless (= (length (cdr par)) 1)
		(err "Not a lambda parameter list: ~s"
		     par))
	      par)
	     (if (symbol? par)
	       (list par #f)
	       (err "Not a lambda parameter: ~s" par)))))
        (this #f)
        (head pars))
    (do ()
        ((null? pars) )
      (set! this (car pars))
      (set! pars (cdr pars))
      ;; recognize cltl2 or guile names
      (if (member this '(&optional &rest &key &aux &allow-other-keys
			 :optional :rest :key :aux :allow-other-keys))
        (cond ((or (eq? this '&optional) 
		   (eq? this ':optional))
               (unless (eq? mode '&required)
                 (err "Bad lambda list: ~s." head))
               (set! mode '&optional))
              ((or (eq? this '&rest)
		   (eq? this ':rest))
               (unless (member mode '(&required &optional))
                 (err "Bad lambda list: ~s." head))
               (set! mode '&rest))
              ((eq? this '&key)
               (unless (member mode '(&required &optional !rest))
                 (err "Bad lambda list: ~s." head))
               (set! mode '&key))
              ((or (eq? this '&allow-other-keys)
		   (eq? this ':allow-other-keys))
               (unless (eq? mode '&key)
                 (err "Bad lambda list: ~s." head))
               (set! mode '&allow-other-keys)
               (set! aok? t))
              ((or (eq? this '&aux) 
		   (eq? this ':aux))
               (set! mode '&aux)))
        (case mode
          ((&required )
           (push this reqs))
          ((&optional )
           (push (bind this) opts))
          ((&rest )
           (push this rest)
           (set! mode '!rest))
          ((&key )
           (push (bind this) keys))
          ((&aux )
           (push (bind this) auxs))
          (else
           (err "Bad lambda list: ~s." head)))))

    (values (reverse reqs)
            (reverse opts)
            rest ; only one
            (reverse keys)
            aok? 
            (reverse auxs))))

;;;
;;; cm-version
;;;   major:       incompatible (not backwards-compatible) API
;;;   minor:       backwards-compatible API change, i.e. "feature-add"
;;;   maintenance: no API change, bug fix only.

(define %cm-version% #x260)

(define (cm-version-number)
  (format #f "~a.~a.~a" 
          (ldb (byte 4 8) %cm-version%)
          (ldb (byte 4 4) %cm-version%)
          (ldb (byte 4 0) %cm-version%)))

(define (cm-version)
  ;; ensure filenames pushed on %patches% by the 1.3 version of 
  ;; load-cmpatches will be filename-names only.  This should be 
  ;; removed in future versions.
  (format #f "Common Music ~a" (cm-version-number)))

;;;
;;; the ultimate in algorithmic iconic artware
;;;

(define *cm-logo* #t)

(define (cm-logo )
  (if *cm-logo*
    (begin
     (format #t "~%")
     (do ((e "~%")
          (v (make-string 15))
          (y 0 (+ y 1)))
         ((= y 7) #f)
       (format #t
               (do ((x 0 (+ x 1)))
                   ((= x 15)
                    (if (= y 3)
                      (string-append v " " (cm-version) e)
                      (string-append v e)))
                 (string-set! v x
                              (if (<= 2 (- x y) 4) #\\
                                  (if (= (- x (- 4 (modulo (+ 13 y) 15))) 1)
                                    #\/
                                    (if (<= 1 y 5) #\-
                                        (if (= (* (- x 6) (- y 3)) 15) #\/
                                            #\space))))))))
     (format #t "~%")))
  (values))

;;;
;;; string hacks
;;;

(define (string-substrings string . args)
  (with-args (args &key (delimiters '(#\space #\tab)) 
		   (start 0) (end (string-length string))
		   key)
    (loop for pos1 = start then (1+ pos2) 
	  until (> pos1 end)
	  for pos2 = (or (position-if (lambda (c) (member c delimiters))
				      string ':start pos1)
			 end)
	  unless (= pos1 pos2)
	  collect 
	  (if key
	    (key (substring string pos1 pos2))
	    (substring string pos1 pos2)))))

; (string-substrings "A B :FOO (BASD     ASD) 123")

; (string-readable? "")
; (string-readable? "    ")
; (string-readable? "()")
; (string-readable? "())")
; (string-readable? "(()")
; (string-readable? "(\"1\")")
; (string-readable? "(\"1)")
; (string-readable? " 1 2 () 3   ")
; (string-readable? " \"bif buf\"   ")
; (string-readable? "#.foo")
; (string-readable? "'foo")
; (string-readable? "'(foo bar)")

(define (string-readable? string . args)
  ;; do some simple checks on strings before reading for lisp expressions.
  ;; return nil or the number of forms in string.
  (with-args (args &optional (start 0) (end (string-length string)))
    (do ((pos start (+ pos 1))
         (tok #f)
         (num 0)
         (lev 0)
         (str #f)
         (chr #f))
        ((not (< pos end))
         (if (and (= pos end) (= lev 0) (not str))
           (if tok (+ num 1) num)
           #f))
      (set! chr (string-ref string pos))
      (cond ((char=? chr #\()
             (if (= lev 0) (incf num))
             (incf lev)
             (set! tok #f))
            ((char=? chr #\))
             (decf lev)
             (if (< lev 0) (set! pos end))
             (set! tok #f))
            ((char=? chr #\") 
             (set! str (not str))
             (if (and (= lev 0) str) (incf num))
             (set! tok #f))
            ((member chr '(#\, #\`))
             ;; backquote is hopeless...
             (if (not str) (set! pos end)))
            ((member chr '(#\space #\return #\tab))
             (if (and tok (= lev 0) (not str))
               (incf num))
             (set! tok #f))
            ((member chr '(#\' #\#))
             (set! tok #f))
            (else (set! tok #t))))))

;;;
;;; string->expr reads one or more exprs from a string.
;;; returns two values, the expr(s) and an error flag that, if not nil
;;; is one of the following errorcodes

(define +se-nullstring+ 0)
(define +se-unreadable+ 1)
(define +se-multiple+   2)
(define +se-incorrect+  3)
(define +se-not-number+ 4)
(define +se-not-symbol+ 5)
(define +se-not-cons+   6)

(define (string->expr str . args)
  (with-args (args &key (read #t) (test #f)
                   (nullok #t) (multiok #f) errval)
    ;; parse input value from a gtk entry or a string
    (let ((text str)
          (trim '(#\space #\newline #\tab))
          (expr #f)
          (err? #f))
      (if (string=? text "")
        (if nullok (values #f #f) (values "" +se-nullstring+))
        (let ((len (string-length text))
              (raw text))
          (if (or (member (string-ref text 0) trim)
                  (member (string-ref text (1- len)) trim))
            (set! text (strip-chars text))) ; remove whitespace
          (if (string=? text "")
            (if nullok (values #f #f) (values raw +se-nullstring+))
            (begin
              (if (not read)
                (begin (set! expr text) (set! err? #f))
                (if (not (eq? read #t))
                  (multiple-value-setq (expr err?) ( read text))
                  (let ((num (string-readable? text)))
                    (cond ((not num)
                           (set! expr text)
                           (set! err? +se-unreadable+))
                          ((= num 1)
                           ;; dont care about num
                           (multiple-value-setq (expr num)
                                                (string-read text))
                           (set! err? #f))
                          ((not multiok)
                           (set! expr text)
                           (set! err? +se-multiple+))
                          (else
                           (do ((n 0)
                                (x #f)
                                (l (list)))
                               ((eq? x ':eof)
                                (set! expr (reverse! l))
                                (set! err? #f))
                             (multiple-value-setq (x n)
                               (string-read text n))
                             (unless (eq? x ':eof)
                               (push x l))))))))
              (if err? 
                (values expr err?) 
                (if test
                  (if ( test expr) (values expr #f)
                      (values test (or errval +se-incorrect+)))
                  (values expr err?))))))))))

; (string->expr "")
; (string->expr "    ")
; (string->expr "(")
; (string->expr "()")
; (string->expr "1 2 3")
; (string->expr "(list 1 2 3)")
; (string->expr "1 2 3" :multiok t)

;;; number hacks
;;;

(define (decimals value places)
  (let ((n (expt 10.0 places)))
    (/ (round (* value n)) n)))

(defmacro sv (obj slot . args)
  (if (null? args)
    (slot-getter-form obj slot)
    (let ((o (gensym)))
      `(let ((,o ,obj))
         ,(slot-setter-form o slot (car args))
         ,@ (if (null? (cdr args))
              (list)
              (let ((res '())) 
                (dopairs (x y (cdr args))
                  (push (slot-setter-form o x y) res))
                (reverse res)))
            (values)))))






