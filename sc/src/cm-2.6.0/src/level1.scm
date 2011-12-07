;;; **********************************************************************
;;; Copyright (C) 2001 Heinrich Taube (taube@uiuc.edu) 
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
;;; $Revision: 1.11 $
;;; $Date: 2004/12/14 18:35:40 $

;;; level1 for scheme. most of this is cltl functionality.

(defmacro multiple-value-bind (vars form . body)
  `(call-with-values
    (lambda () ,form)
    (lambda (,@vars) ,@body)))

(defmacro multiple-value-list (form)
  `(call-with-values
    (lambda () , form)
    (lambda args args)))

(defmacro multiple-value-setq (vars form)
  (let ((lst (map (lambda (x) (gensym (symbol->string x))) 
		  vars)))
    `(call-with-values 
      (lambda () ,form)
      (lambda (,@lst)
	,@(map (lambda (x y) `(set! ,x ,y))
	       vars lst)))))

;;; function and funcall are noops in scheme but are used for
;;; cltl code translation.

(defmacro function (fn) fn)

(define (funcall fn . args)
  (apply fn args))

;;;
;;; symbol setters. these are not general setters!
;;;

(defmacro push (val sym)
  `(begin (set! ,sym (cons ,val ,sym)) ,sym))

(defmacro pop (sym)
  (let ((v (gensym)))
    `(let ((,v (car ,sym)))
       (set! ,sym (cdr ,sym))
       ,v)))

(defmacro incf (sym . val)
  `(begin
    (set! ,sym (+ ,sym ,(if (null? val) 1 (car val))))
    ,sym))

(defmacro decf (sym . val)
  `(begin
    (set! ,sym (- ,sym ,(if (null? val) 1 (car val))))
    ,sym))

(defmacro rotatef (sym1 sym2)
  (let ((v (gensym)))
    `(let ((,v ,sym1))
      (set! ,sym1 ,sym2)
      (set! ,sym2 ,v))))

;;;
;;; true and false provided so cm examples can be portable.
;;;

(define true #t)
(define false #f)

;;;
;;; list operations
;;;

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define rest cdr)

(define (copy-list lis)
  (append lis '()))

(define (copy-tree lis)
  (if (pair? lis)
    (cons (copy-tree (car lis))
          (copy-tree (cdr lis)))
    lis))

(define (remove val lst)
  (if (null? lst)
    '()
    (if (eq? (car lst) val)
      (remove val (cdr lst))
      (cons (car lst) (remove val (cdr lst))))))

(define (butlast lis)
  (if (or (null? lis) 
          (null? (cdr lis)))
    '()
    (let ((h (list '())))
      (do ((l h))
          ((null? (cdr lis))
           (cdr h))
        (set-cdr! l (list (car lis)))
        (set! l (cdr l))
        (set! lis (cdr lis))))))

(define (remove-duplicates lis)
  (letrec ((rd (lambda (l r)
                 (if (null? l)
                   r
                   (if (member (car l) r)
                     (rd (cdr l) r)
                     (rd (cdr l)
                         (append! r (list (car l)))))))))
    (rd lis (list))))

(define (list-delete-if! test data  . args)
  (let ((tail data)
        (last #f)
        (key (if (null? args) #f (car args)))
        (val #f))
    (do ()
        ((null? tail) data)
      (set! val (if key ( key (car tail))
                    (car tail)))
      (if ( test val)
        (if (eq? tail data)
          (begin (set! data (cdr data))
                 (set! tail data)
                 (set! last #f))
          (begin (set-cdr! last (cdr tail))
                 (set! tail (cdr tail))))
        (begin (set! last tail)
               (set! tail (cdr tail)))))))

; (list-delete-if! #'zero? (list 0) )
; (list-delete-if! #'zero? (list 0 1 1 2 2 0 0))
; (list-delete-if! #'zero? (list 1 2 3))
; (list-delete-if! #'zero? (list 0 1 2))
; (list-delete-if! #'zero? (list 0 1 0 1 2))
; (list-delete-if! #'zero? (list 1 0.0 0 0 2))
; (list-delete-if! #'zero? (list 1 0.0 0 0 2 0))

;;;
;;; property list getting and setting
;;;

(define (list-prop lis prop . def)
  (if (null? lis)
      (if (null? def) #f (car def))
      (if (eq? (car lis) prop)
          (cadr lis)
          (apply list-prop (cddr lis) prop def))))

(define (list-prop-set! lis prop val)
  (if (eq? (car lis) prop)
      (set-car! (cdr lis) val)
      (if (null? (cddr lis))
          (set-cdr! (cdr lis) (list prop val))
          (list-prop-set! (cddr lis) prop val))))
  
(define (some pred lis)
  (do ((lst lis (cdr lst))
       (flg #f))
      ((or (null? lst) flg) flg)
    (if ( pred (car lst));; funcall
      (set! flg #t))))

;;; an equivalent for (pop (cdr ..)) 

(defmacro cdr-pop (x)
  (let ((h (gensym))
	(t (gensym))
	(v (gensym)))
    `(let* ((,h ,x)
	    (,t (cdr ,h))
	    (,v (car ,t)))
      (set-cdr! ,h (cdr ,t))
      ,v)))

;;;
;;; define-list-struct creates a list struct. the struct
;;; is represented by a constructor and getters/setters
;;; for each slot:
;;;   (define-list-struct foo a (b 1))  
;;; defines:
;;;     make-foo (&key a b)
;;;     foo-a(s) 
;;;     foo-b(s)
;;;     foo-a-set!(s v)
;;;     foo-b-set!(s v)
;;;

(defmacro define-list-struct (name . slotspecs)
  (expand-list-struct name slotspecs))

(define (expand-list-struct name slotspecs)
  (letrec ((tailform
            (lambda (pos)
              (case pos
                ((0) 'arg)
                ((1) '(cdr arg))
                ((2) '(cddr arg))
                ((3) '(cdddr arg))
                ((4) '(cddddr arg))
                (else `(list-tail arg ,pos)))))
           (setslotform 
            (lambda (pos) `(set-car! ,(tailform pos) val)))
           (getslotform 
            (lambda (pos) `(car ,(tailform pos))))
           (fillslotform
            (lambda (slot)
              (let ((value #f))
                (when (pair? slot)
                  (set! value (cadr slot))
                  (set! slot (car slot)))
                `(list-prop args ',(symbol->keyword slot) ,value))))
           (make-name
            (lambda (str1 . strs)
              (string->symbol (apply string-append str1 strs)))))

  (let  ((name (symbol->string name))
         (slots (map (lambda (s) (if (symbol? s) s (car s)))
                    slotspecs)))
    `(begin
      ;; create constructor
       (define (, (make-name "make-" name) . args)
        , (cons 'list (map fillslotform slotspecs)))
      ;; create accessor functions
       ,@ (do ((i 0 (+ i 1))
	       (l '())
	       (s slots (cdr s)))
	      ((null? s) (reverse l))
	    (push `(define (,(make-name name "-" 
					 (symbol->string (car s)))
			     arg)
		     ,(getslotform i))
		   l))
      ;; create setter functions
      ,@ (do ((i 0 (+ i 1))
	       (l '())
	       (s slots (cdr s)))
	      ((null? s) (reverse l))
	    (push `(define (,(make-name name "-"
					 (symbol->string (car s))
					 "-set!") 
			     arg val)
		     ,(setslotform i))
		   l))))))

;;;
;;; dolist, dotimes. see loop.scm for the cltl2 macro implementation.
;;;

(defmacro dolist (spec . body)
  ;; spec = (var list . return)
  (let ((v (gensym)))
    `(do ((,v ,(cadr spec) (cdr ,v))
          (,(car spec) #f))
         ((null? ,v) ,@ (cddr spec))
       (set! ,(car spec) (car ,v))
       ,@body)))

(defmacro dotimes (spec . body)
  ;; spec = (var end . return)
  (let ((e (gensym))
        (n (car spec)))
    `(do ((,e ,(cadr spec))
          (,n 0))
         ((>= ,n ,e) ,@ (cddr spec))
       ,@body
      (set! ,n (+ ,n 1)))))

;;;
;;; hash tables
;;;

(define (hash-clear! tabl)
  (hash-fold (lambda (k v r)
               (hash-remove! tabl k)
               r)
             #t
             tabl))
;;;
;;; conditionals...
;;;

(defmacro when (test . forms)
  `(if ,test (begin ,@forms)))

(defmacro unless (test . forms)
  `(if (not ,test)
       (begin ,@forms)))

;(defmacro ecase (datum . rest)
;  (let ((var (gensym))
;        (keys '())
;        (case '()))
;    (for-each (lambda (x)
;                (if (pair? (car x))
;                  (begin (push (car x) keys)
;                         (push x case))
;                  (let ((e (list (car x))))
;                    (push e keys)
;                    (push `(,e ,@ (cdr x)) case))))
;              rest)
;    `(let ((,var ,datum))
;       (case ,var
;         ,@ case
;         (else
;          (err "case: key ~s is not one of the expected keys: ~s"
;	       ,var ',(apply append keys)))))))

;;;
;;; Numbers and bit twiddling. requires ash, logand, logior, lognot
;;;

(define pi 3.141592653589793)

;;; common lisp floor and round. 

(define (clfloor n . arg)
  (if (null? arg)
    (let ((v (floor n)))
      (values (inexact->exact v) (- n v)))
    (let* ((d (car arg))
	   (v (/ n d))
	   (i (inexact->exact (floor v)))
	   (r (- n (* i d))))
      (values i r))))

(define (clround n . arg)
  (if (null? arg)
    (let ((v (round n)))
      (values (inexact->exact v) (- n v)))
    (let* ((d (car arg))
	   (v (/ n d))
	   (i (inexact->exact (round v)))
	   (r (- n (* i d))))
      (values i r))))

(define (mod num div)
  (if (and (exact? num)
           (exact? div))
    (modulo num div)
    (let* ((res (/ num div))
           (flo (floor res)))
      (- num (* flo div)))))

(define (rem num div)
  (if (and (exact? num)
           (exact? div))
    (remainder num div)
    (let* ((res (/ num div))
           (flo (truncate res)))
      (- num (* flo div)))))

(define %log2 (log 2))

(define (log2 n) (/ (log n) %log2))

(define (logn num base) (/ (log num) (log base)))

;;;
;;; byte spec
;;;

(define (byte siz pos)
  ;; cache size, position and mask.
  (vector siz pos (ash (- (expt 2 siz) 1) pos)))

(define (byte-size bytespec)
  (vector-ref bytespec 0))

(define (byte-position bytespec)
  (vector-ref bytespec 1))

(define (byte-mask bytespec)
  (vector-ref bytespec 2))

(define (ldb bytespec integer)
  (ash (logand integer (byte-mask bytespec))
       (- (byte-position bytespec))))

(define (dpb integer bytespec into)
  (let ((val (logand integer (ash (byte-mask bytespec) 
                                  (- (byte-position bytespec))))))
    (logior (logand into (lognot (byte-mask bytespec)))
	    (ash val (byte-position bytespec)))))

;;;
;;; cltl2 lambda paramters
;;;

(define (parse-lambda-list pars)
  ;;(format #t "args=~s" pars)
  ;; parse a cltl2 parameter declaration into seperate lists. modified 
  ;; to allow either cltl2 or guile style type decls, ie &key or #:key
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
			 #:optional #:rest #:key #:aux #:allow-other-keys))
        (cond ((or (eq? this '&optional) 
		   (eq? this #:optional))
               (unless (eq? mode '&required)
                 (err "Bad lambda list: ~s." head))
               (set! mode '&optional))
              ((or (eq? this '&rest)
		   (eq? this #:rest))
               (unless (member mode '(&required &optional))
                 (err "Bad lambda list: ~s." head))
               (set! mode '&rest))
              ((or (eq? this '&key)
                   (eq? this #:key))
               (unless (member mode '(&required &optional !rest))
                 (err "Bad lambda list: ~s." head))
               (set! mode '&key))
              ((or (eq? this '&allow-other-keys)
		   (eq? this #:allow-other-keys))
               (unless (eq? mode '&key)
                 (err "Bad lambda list: ~s." head))
               (set! mode '&allow-other-keys)
               (set! aok? #t))
              ((or (eq? this '&aux) 
		   (eq? this #:aux))
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
;;; with-args (list . decl) . body)
;;; binds variables to values from a list according to cltl2's lambda
;;; parameter declaration syntax. any &optional, &key &rest and &aux
;;; parameters without default values are initiaized to #f. Example: 
;;; cltl2:
;;; (defun foo (a b &optional (c 3) &key d (e a) &aux (f -99))
;;;   (list a b c d e f))
;;; scheme:
;;; (define (foo . args) 
;;;   (with-args (args a b &optional (c 3) &key d (e a) &aux (f a)) 
;;;     (list a b c d e f)))
;;;
;;; (foo 0 1 2 :d 3 :e 4)
;;;

(defmacro with-args (spec . body) 
  ;; spec is (list . lambda-decl)
  (let ((args (gensym))
        (reqs '())
        (opts '())
        (rest '())
        (keys '())
        (auxs '())
        (aok? #f))			; allow-other-keys

    (multiple-value-setq (reqs opts rest keys aok? auxs )
                         (parse-lambda-list (cdr spec)))
    ;; each &key entry is represented by a four element list:
    ;; (var default passed? keyword) where var is the variable,
    ;; default is its default value, passed? is a flag set to
    ;; #t if key is passed and keyword is the keyword. the first
    ;; two values in the list were returned by parse-lambda-list
    (for-each (lambda (b)
		(set-cdr! (cdr b)
			  (list (gensym) (symbol->keyword (car b)))))
	      keys)

;    (do ((tail keys (cdr tail))
;         (temp (list)))
;        ((null? tail)
;         (set! keys (reverse! temp)))
;      (let ((b (car tail)))
;        (set! temp (cons (list (car b) (gensym)
;                               (symbol->keyword (car b)))
;                         temp))))
    ;; let* so params can reference earlier ones
    ;; in the lambda list.
    `(let* ((,args ,(car spec))
	    ;; splice in bindings for required args
	    ,@ (map (lambda (r)		; r is required par
		      (let ((v (gensym)))
			`(,r (if (null? ,args)
			       (err "Missing value for required arg ~s."
				    ',r)
			       (let ((,v (car ,args)))
				 (set! ,args (cdr ,args))
				 ,v)))))
		reqs)
	    ;; splice in &optional bindings. optimize the common
	    ;; case of a single optional arg
	    ,@ (if (and (null? rest) (null? keys)
			(= (length opts) 1))
		 ;; skip the let and cdring of args if only one.
		 `((, (car (car opts))
		    (if (null? ,args) ,(cadr (car opts)) (car ,args))))
		 (map (lambda (b)
			;; b is (<var> <val>)
			(let ((v (gensym)))
			  `(,(car b)
			    (if (null? ,args)
			      ,(cadr b)
			      (let ((,v (car ,args)))
				(set! ,args (cdr ,args))
				,v)))))
		      opts))
	    ;; splice in single &rest arg
	    ,@ (if (null? rest) '() rest)
	    ;; initially bind all &key vars and flags to #f
	    ,@ (map (lambda (b) (list (car b) #f)) keys)
	    ,@ (map (lambda (b) (list (caddr b) #f)) keys)
	    ;; initially bind all &aux to #f
	    ,@ (map (lambda (b) (list (car b) #f)) auxs)
	    )
      ,@ 
      (if (pair? keys)
	(let ((head (gensym)))
	  ;; generate a do loop to parses keyword args.
	  ;; the loop signals error for incorrect keys.
	  `((do ((,head ,args))
		((null? ,args) 
		 ;; loop termination clause sets default values
		 ;; for all keys that were not passed in args
		 ;; and whose default value is not #f.
		 ,@
		 (apply append
			(map (lambda (b) 
			       ;; b is (<var> <val> <v?>)
			       ;; skip if default is #f
			       (if (eq? (cadr b) #f)
				 '()
				 `((if (not ,(caddr b))
				     (set! ,(car b) ,(cadr b))))))
			     keys)))
	      ;; body of do
	      (if (null? (cdr ,args))
		(err "Args not keyword format: ~s." ,head))
	      (case (car ,args)
		,@
		(map (lambda (b)
		       ;; b is (<var> <val> <flag> <keyw>)
		       `((,(cadddr b) )
			 (set! ,(car b) (cadr ,args))
			 (set! ,(caddr b) #t)))
		     keys)
		;; splice in error trap unless &allow-other-keys.
		;; error message includes list of valid keywords.
		,@
		(if (not aok?)
		  `((else
		     (err 
		      "Illegal keyword '~s' in: ~s.~%Valid keywords: ~s"
		      (car ,args) ,head ',(map #'cadddr keys))))
		  '()))
	      (set! ,args (cddr ,args)))))
	'())
      ;; spice in &aux params if default value not #f.
      ,@ (apply append (map (lambda (b)
			      (if (eq? (cadr b) #f)
				'()
				`((set! ,(car b) ,(cadr b)))))
			auxs))
      ,@body)))

; (define pprint display)
; (with-args ( '(22 2 3) (a 0) b c) (list a b c))
; (let ((l '()) ) (with-args (l &optional (a 0) b c) (list a b c)))
; (let ((l '(100)) ) (with-args (l  &optional a (b 5) c) (list a b c)))
; (let ((l '(:a 100 :b 200 :c 300))) (with-args (args &key  (a 0) b c) (list a b c)))
; (pprint (macroexpand '(with-args (l &key (a 0) b c) (list a b c))))
; (let ((l '()) ) (with-args (l (a 0) b c) (list a b c)))
; (let ((l '(:c 200 b -9)) ) (with-keys (l a (b 5) c) (list a b c)))
;;errors:
; (let ((l '(:c 200 z -9)) ) (with-args (l &key a (b 5) c) (list a b c)))
; (let ((l '(1 :c 100)) ) (with-args (l &key a (b 5) c) (list a b c)))
; (let ((l '(:c)) ) (with-args (l &key a (b 5) (c b)) (list a b c)))

;;;
;;; find, position, find-if, position-if, find-if-not, position-if-not
;;; for list strings and vectors

(define (find-aux mode obj seq test key start end from-end)
  (let ((lim (if from-end < >=))
	(inc (if from-end - +))
	(get #f))
    (cond ((vector? seq)
           (set! get vector-ref)
           (set! end (or end (vector-length seq))))
          ((list? seq)
           (set! get list-ref)
           (set! end (or end (length seq))))
          ((string? seq)
           (set! get string-ref)
           (set! end (or end (string-length seq))))
          (else
           (err "~s is not a vector, pair or string." seq)))

    (do ((i (if from-end (- end 1) start))
	 (z (if from-end start end))
         (j #f)
         (k #f))
        ((or k (lim i z)) k)
      (set! j (get seq i))
      (if (test obj (key j))
        (if (eq? mode 'find) (set! k j) (set! k i))
	)
      (set! i (inc i 1)))))

(define (find obj seq . args)
  (with-args (args &key (test eq?) (key identity) (start 0) end
                   from-end )
    (find-aux 'find obj seq test key start end from-end)))

(define (find-if fn seq . args)
  (apply find #t seq ':test (lambda (x y) (if (fn y) #t #f)) args))

(define (find-if-not fn seq . args)
  (apply find #f seq ':test (lambda (x y) (if (fn y) #t #f)) args))

(define (position obj seq . args)
  (with-args (args &key (test eq?) (key identity) (start 0) end
                   from-end )
    (find-aux 'position obj seq test key start end from-end)))

(define (position-if fn seq . args)
  (apply position #t seq ':test (lambda (x y) (if (fn y) #t #f)) args))

(define (position-if-not fn seq . args)
  (apply position #f seq ':test (lambda (x y) (if (fn y) #t #f)) args))


; (find 1 '(a b c 1 2 3))
; (position-if (lambda (x) (eq? x 1)) '(a b v 1 2))
; (find 1 #(a b c 1 2 3))
; (find #\1 "abc123")

; (find 1 '((a 1) (b 2) (c 2) (1 a) (2 b) (3 c)) ':key cadr)
; (find 9 '((a 1) (b 2) (c 2) (1 a) (2 b) (3 c)) ':key cadr)
; (find #\1 "")
; (find 1 #())
; (find 1 '())
; (position 1 '((a a) (b b) (c c) (1 1) (2 2) (3 3))  ':key car ':from-end #t)

(define (strip-chars str . args)
  (let ((chars (if (null? args) '(#\space #\tab #\return)
                   (car args))))
    (string-trim-both str (lambda (c) (member c chars)))))

;;;
;;; unix filename twiddling. filenames are just strings.
;;; the level0 files must set the directory character.
;;;

(define (namestring p) p)

(define (filename p) p)

(define (filename-directory file)
  (let ((dir (position directory-delimiter file ':from-end #t)))
    (if dir
      (substring file 0 (+ dir 1))
      #f)))

(define (filename-name file)
  (let ((dir (or (position directory-delimiter
                           file :from-end #t) -1))
	(dot (or (position #\. file ':from-end #t)
		 (string-length file))))
    ;; name from dir+1 to dot-1
    (if (= dot (+ dir 1))
      (substring file dot (string-length file))
      (if (> dot (+ dir 1))
	(substring file (+ dir 1) dot)
	#f))))

(define (filename-type file)
  (let ((dot (position #\. file :from-end #t)))
    (if dot
      (let ((dir (or (position directory-delimiter
                               file ':from-end #t)
                     -1))
	    (len (string-length file)))
	(if (and dot (< dir (- dot 1) dot (- len 1)))
	  (substring file (+ dot 1) len)
	  #f))
      #f)))

(define (merge-filenames p1 p2)
  (let ((pd (filename-directory p1))
	(pn (filename-name p1))
	(pt (filename-type p1)))
    (if (not pd)
      (set! pd (filename-directory p2)))
    (if (not pn)
      (set! pn (filename-name p2)))
    (if (not pt)
      (set! pt (filename-type p2)))
    (apply string-append (or pd "") (or pn "") 
	   (if pt (list "." pt) '()))))

(define (open-file name direction . type)
  (if (eq? direction :output)
    (open-output-file name)
    (open-input-file name)))

(define (close-file fp dir) 
  (if (eq? dir :output)
    (close-output-port fp)
    (close-input-port fp)))

(define (file-form fil)
  (read fil))

(define (file-line fil)
  (read-line fil))

(define (file-eof? x) (eof-object? x))

;(defmacro with-open-output-file (args . body)
;  (let ((var (car args)))
;    `(let ((,var (open-output-file ,(cadr args))))
;       (dynamic-wind (lambda () #f)
;                     (lambda () ,@body 
;                             )
;                     (lambda () (close-output-port ,var))))))
;
;(defmacro with-open-input-file (args . body)
;  (let ((var (car args)))
;    `(let ((,var (open-input-file ,(cadr args))))
;       (dynamic-wind (lambda () #f)
;                     (lambda () ,@body ;(close-input-port ,var)
;                             )
;                     (lambda () (close-input-port ,var))))))

;;;
;;; Scheme expansion for defobject
;;;

(define (expand-defobject name gvar supers decl pars methods)
  ;; slots must be parsed into goops format.
  (let ((slts (map (lambda (s) (parse-slot-spec name s)) decl))
        (opts (if pars
                `(:metaclass <parameterized-class>
                             :parameters (quote ,pars))
                '())))
    `(begin 
      (define-class ,gvar ,(map class-name->class-var supers)
	,@ slts :name ',name ,@ opts )
      
      (define-method (make-load-form (obj ,gvar))
        (list* 'make-instance ', gvar (slot-init-forms obj :eval #t)))
      
      ;; define a #i print-object method
      (define-method (write (obj ,gvar) port)
        (if *print-instance*
          (print-instance obj port)
          (next-method)))
      
      ,@methods
      
      (values))))

;(define (make-load-form-method classname classvar)
;  `(define-method (make-load-form (obj ,classvar))
;     (list* 'make-instance ',classvar (slot-init-forms obj :eval #t))))

(define (parse-slot-spec cname spec)
  (let ((acc (lambda (b)
	       (string->symbol (format #f "~a-~a" cname b))))
	(key (lambda (b) (symbol->keyword b)))
	(val #f)
	(name (if (pair? spec) (car spec) spec))
	(spec (if (pair? spec) (copy-list (cdr spec)) (list))))
    
    ;; convert :initarg to :init-keyword. in cltl the slot
    ;; can have any number of initargs and initargs can
    ;; be symbols or keywords. guile only supports a single
    ;; keyword initarg.
    (set! val (memq ':initarg spec))
    (if val
      (do ((tail val (memq ':initarg tail))
           (sofar '() sofar)
           (key #f))
          ((not tail)  #f)
        (if (keyword? (cadr tail))
          (set! key (cadr tail))
          (set! key (and (cadr tail) ;; not #f
                         (symbol->keyword (cadr tail)))))
        ;; remove duplicate initarg or ':initarg #f'
        (if (or (memq key sofar)
                (not key))
          (begin
           (when (cadr tail)
             (warning "Ignoring duplicate initarg for ~a." 
                      name))
           ;; remove from spec. 
           ;(format #t "~%spec=~s tail=~s" spec tail)
           (if (eq? spec tail)
             (begin (set! spec (cddr spec))
                    (set! tail spec))
             (do ((edit spec (cdr edit)))
                 ((eq? (cdr edit) tail)
                  (set-cdr! edit (cdddr edit))))))
          (begin (set-car! tail ':init-keyword)
                 (set-car! (cdr tail) key)
                 (push key sofar)))
        (set! tail (cddr tail)))
      (set! spec (cons ':init-keyword (cons (key name) spec))))
    ;; add accessor if not supplied
    (unless (memq ':accessor spec)
      (set! spec (cons ':accessor (cons (acc name) spec))))
    ;; convert :initform to :init-value
    (set! val (memq ':initform spec))
    (when val
      (set-car! val ':init-value))
    (cons name spec)))

;;;
;;; scheme expansion for make-load-form
;;;

(define (make-load-form-method classvar classname)
  (define-method make-load-form ((obj ,classvar))
    `(make-instance ,classvar ,@(slot-init-forms obj :eval #t))))

;;;
;;; scheme expansion for write-event
;;;

(define (define-output-method objclassname objclassvar objvar
          fileclassname fileclassvar
          filevar timevar body)
  `(define-method (write-event (,objvar ,objclassvar)
                               (,filevar ,fileclassvar)
                               ,timevar)
     ,@body))

;;;
;;; Scheme expansion for process macro
;;;

(define (process-stop expr)
  ;; stopprocess is lexical var holding continuation 
  ;; return false
  '(stopprocess #f))

(define (expand-process forms ops)
  (let ((parsed (parse-iteration 'process forms ops))
	(code '())
	(func #f)
	(tests '())
	(done #f))
    (set! tests (loop-end-tests parsed))
    (set! done (process-stop #f))
    (if (loop-finally parsed)
      (set! done `(begin ,@(loop-finally parsed) ,done)))
    (if (not (null? tests))
      (begin
       (if (null? (cdr tests))
	 (set! tests (car tests))
	 (set! tests (cons 'or tests)))
       (set! tests `((if ,tests ,done))))
      (unless (process-code-terminates? (loop-looping parsed)
					(process-stop #f))
	(warning "A non-terminating process may have been defined. Use 'repeat', 'while' or 'until' to limit iteration.")))
    (set! func `(lambda ()
		  (call-with-current-continuation
		   (lambda (stopprocess)
		     ,@ tests
		        ,@ (loop-looping parsed)
		           ,@ (loop-stepping parsed)
                              ;;(enqueue *process* *qnext* *qstart* )
		              #t
                              ))))
    (if (and (null? (loop-bindings parsed))
	     (null? (loop-initially parsed)))
      func
      ;; use let* sequential binding
      `(let* ,(loop-bindings parsed)
	 ,@(loop-initially parsed)
	 ,func))))

(define (expand-defprocess forms)
  (let ((args (second forms)))
    (if (not (list? args))
      (err "defprocess arguments not list: ~S" args))
    (if (or (member '&optional args)
            (member '&key args)
            (member '&rest args))
      (let ((v (gensym)))
        `(define (,(first forms) . ,v)
           (with-args (,v ,@args)
             ,@(cddr forms))))
      `(define (,(first forms) . ,@args) 
         ,@(cddr forms)))))

;;;
;;; scheme expansion for make-midi-message-set!
;;;

(define (make-midi-message-set! getter bytespec)
  (let ((setter (string->symbol
                 (string-append (symbol->string getter)
                                "-set!"))))
    `(defmacro ,setter (message value)
       (if (symbol? message)
         (let ((val (gensym)))
           `(let ((,val ,value )) ;
              (set! ,message (dpb ,val ,',bytespec ,message))
              ,val))
         `(dpb ,value ,',bytespec ,message)))))

;;;
;;;
;;;

(define (set-file-postion file amt set?)
  (if set?
    (seek file amt SEEK_SET)
    (seek file amt SEEK_CUR)))

;;;
;;;
;;;

(define (print x . s)
  (format (if (null? s) #t (car s)) "~S~%" x)
  x)

(define (describe x)
  (let ((str #f))
    (cond ((exact? x)
           (set! str "exact number"))
          ((inexact? x)
           (set! str "inexact number"))
          ((complex? x)
           (set! str "complex number"))
          ((number? x)
           (set! str "number"))
          ((char? x)
           (set! str "character"))
          ((keyword? x)
           (set! str "keword"))
          ((symbol? x)
           (set! str "symbol"))
          ((boolean? x)
           (set! str "boolean"))
          ((pair? x)
           (set! str "pair"))
          ((list? x)
           (set! str "list"))          
          ((string? x)
           (set! str "string"))
          ((vector? x)
           (set! str "vector"))
          ((procedure? x)
           (set! str "procedure"))
          ((port? x)
           (set! str "port"))
          ((is-a? x <object>)
           (describe-object x))
          (else
           (set! str "scheme object")))
    (when str
      (format #t "~s is ~a ~a.~%"
              x
              (if (member (string-ref str 0)
                          '(#\a \e #\i #\o #\u))
                "an" "a")
              str))))

(define (cm . verbose)
  ;; a no-op for now, 
  (if (or (null? verbose)
          (not (eq? (car verbose) #f)))
    (cm-logo))
  (values))

