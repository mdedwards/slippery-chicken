;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.2 $
;;; $Date: 2004/04/29 16:48:51 $

(define-class <event-stream> (<container>)
  (time :accessor object-time)
  (open :init-value #f :accessor io-open)
  (stream :init-value #f :init-keyword :stream
          :accessor event-stream-stream)
  (direction :init-value #f :accessor io-direction)
  (version :init-value 0 :accessor file-version
           :init-keyword :version)
  (elt-type :init-value :char :accessor file-elt-type
            :init-keyword :elt-type)
  :name 'event-stream)

(define (io-classes )
  (class-subclasses <event-stream>))

(define (io-filename io)
  (object-name io))

(define-generic init-io)
(define-generic open-io)
(define-generic close-io)
(define-generic initialize-io)
(define-generic deinitialize-io)
(define-generic play)
(define-generic write-event)
(define-generic import-events)

(define (file-versions? stream)
  (io-class-file-versions (class-of stream)))

;;;
;;;
;;;

(define (io-stream-classes )
  ;; return a list of all classes that handle file/port io.
  (do ((l (io-classes ) (cdr l))
       (r '()))
      ((null? l) (reverse r))
    (let ((h (io-class-file-types (car l))))
      (if h (push (car l) r)))))

(define (filename->io-class path)
  ;; returns an io class given a "name.type" namestring. if
  ;; name  is not "*" an exact match is required. othewise 
  ;; if the types are the same its a match.
  (let ((name (filename-name path))
        (type (filename-type path)))
    (if type
      (let ((matchone
	     (lambda (key name type) ; "*.midi" "test" "midi"
               (let ((nam (filename-name key))
		     (ext (filename-type key)))
                 (when (or (eq? nam :wild)  ; cltl...
			   (string=? nam "*"))
                   (set! nam #f))
                 (if nam
                   (if (string=? nam name)
                     (if (string=? ext type) #t #f)
                     #f)
                   (if (string=? ext type) #t #f))))))
        (do ((l (io-stream-classes ) (cdr l))
             (c #f))
            ((or (null? l) c)
             (or c (err "No file or port class for ~s." path)))
          (do ((x (io-class-file-types (car l)) (cdr x)))
              ((or (null? x) c) c)
            (if (matchone (car x) name type)
              (set! c (car l))))))
      (err "Missing .ext in file or port specification: ~s"
	   path))))
      
; (filename->io-class "test.clm")
; (filename->io-class "test.sco")
; (filename->io-class "test.wav")
; (filename->io-class "test")

;;;
;;; classes can specialize this to allow extra args.
;;;

(define-generic io-handler-args?)
(define-generic set-io-handler-args!)
(define-generic io-handler-args)

(define-method (io-handler-args? (io <event-stream>))
  io
  #f)

(define-method (set-io-handler-args! (io <event-stream>) args)
  args
  #f)

(define-method (io-handler-args (io <event-stream>))
  io
  #f)

;;;
;;; init-io called on file/port names or objects to initialize slots.
;;;

;(defmacro io (name . args)
;  (let* ((inits (list #f))
;         (tail inits))
;    (dopairs (i v args)
;      (unless (keyword? i)
;        (if (symbol? i)
;          (set! i (symbol->keyword i))
;          (err "io: not a symbol or keyword: ~s" i)))
;      (set-cdr! tail (list i v))
;      (set! tail (cddr tail)))
;    `(init-io ,name ,@ (cdr  inits))))

(define-method (init-io io . inits)
  inits  ; gag 'unused var' warning from cltl compilers
  io)

(define-method (init-io (string <string>) . inits)
  (let ((io (find-object string))) ; no type filter
    (if io
      (apply (function init-io) io inits)
      (let ((class (filename->io-class string)))
        (if class  ; allow class to specify maker
          (multiple-value-bind (init args)
                               (expand-inits class inits #t #t)
            (let ((n (apply (function make-instance) class 
                            ':name string init )))
              (if (not (null? args ))
                (if (io-handler-args? n)
                  (set-io-handler-args! n args)
                  (err "Not initializations for ~s: ~s."
                       (class-name class) args)))
              n))
          (err "~s is not a valid port or file name." string))))))

(define-method (init-io (io <event-stream>) . inits)
  (unless (null? inits)
    (multiple-value-bind (init args)
                         (expand-inits (class-of io) inits #f #t)
      (dopairs (s v init)
	(slot-set! io s v))
      (if (not (null? args))
        (if (io-handler-args? io)
          (set-io-handler-args! io args)
          (err "Not initializations for ~s: ~s."
               (class-name (class-of io)) args)))))
  io)

(define (file-output-filename file)
  ;; if versioning return filename with version number
  ;; added otherwise return the file name.
  (let ((v (if (file-versions? file)
             (file-version file)
             #f))
	(n (object-name file)))
    (if (integer? v)
      (string-append (or (filename-directory n) "")
		     (filename-name n)
		     "-"
		     (number->string v)
		     "."
		     (filename-type n))
      n)))

;;;
;;; open-io
;;; CHECK GUILE
;;; INIT-IO
;;;

(define-method (open-io (obj <string>) dir . args)
  ;; default method assumes obj is string or filename
  (let ((io (apply (function init-io) obj args)))
    (apply (function open-io) io dir args)))

;(define-method (open-io (obj <event-stream>) dir . args)
;  (format #t "~%open-io: new main method for event-stream")
;  dir args
;  obj)

(define-method (open-io (obj <event-stream>) dir . args)
  args  ; gag 'unused var' warning from cltl compilers
  (let ((file #f)
	(name #f))
    (if (eq? dir :output)
      (if (event-stream-stream obj)
        (set! file (event-stream-stream obj))
        (let ((v (if (file-versions? obj)
                   (file-version obj)
                   #f)))
	  ;; if versioning increment by 1.
	  (if v 
	    (set! (file-version obj)
		  (if (integer? v) (+ v 1) 1)))
	  (set! name (file-output-filename obj))
	  ;; opening existing file is undefined...
	  (if (file-exists? name) (delete-file name))
	  (set! file (open-file name dir (file-elt-type obj)))))
      (if (eq? dir :input)
	(if (event-stream-stream obj)
          (set! file (event-stream-stream obj))
          (set! file (open-file (object-name obj) 
                                dir (file-elt-type obj))))
	(err "Direction not :input or :output: ~s" dir)))
    (set! (io-direction obj) dir)
    (set! (io-open obj) file)
    obj))

(define-method (open-io (obj <seq>) dir . args)
  dir args  ; gag 'unused var' warnings from cltl compilers
  (remove-subobjects obj)
  obj)

;;;
;;; close-io
;;;

(define-method (close-io io .  mode)
  mode  ; gag 'unused var' warnings from cltl compilers
  io)

(define-method (close-io (io <event-stream>) . mode)
  mode  ; gag 'unused var' warnings from cltl compilers
  (when (io-open io)
    (unless (event-stream-stream io)
      (close-file (io-open io) (io-direction io)))
    (set! (io-open io) #f))
  io)

;;;
;;; initalize-io and deinitialize-io. default methods do nothing.
;;;

(define-method (initialize-io obj)
  obj)

(define-method (deinitialize-io obj)
  obj)

(define (io-open? io)
  (io-open io))

(defmacro with-open-io (args . body)
  (let ((io (pop args))
	(path (pop args))
	(dir (pop args))
	(err? (gensym))
        (val (gensym)))
    `(let ((,io #f)
	   (,err? ':error))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
	 (let ((,val #f)) 
           (set! ,io (open-io ,path ,dir ,@args))
           (initialize-io ,io)
           (set! ,val (begin ,@body))
           (set! ,err? #f)
           (if ,err? #f ,val)))
       (lambda ()
         (when ,io
           (deinitialize-io ,io)
           (close-io ,io ,err?)))))))

;;;
;;; events
;;;

(define *in* #f)
(define *out* #f)
(define *last-output-file* #f)

(define (events object to . args)
  ;; args are &key pairs or an optional time offset
  ;; followed by &key pairs.
  (let ((ahead (if (and (pair? args)
			(or (pair? (car args))
			    (number? (car args))))
                 (pop args)
                 0))
        (err? ':error))
    (set! *out* #f)
    (when (odd? (length args))
      (err "Uneven initialization list: ~s." args))
    (dynamic-wind
     (lambda () #f)
     (lambda ()
       (let ((getobj
	      (lambda (x)
		(if (not (null? x))
		  (if (or (string? x) (symbol? x))
		    (find-object x)
		    x)
		  (err "Not an object specification: ~s." x)))))
	 (when to
	   (set! *out* (open-io (apply (function init-io) to args)
                                ':output))
	   (initialize-io *out*))
	 (schedule-events (lambda (e s) (write-event e *out* s))
			  (if (pair? object) 
			    (map (function getobj) object)
			    (getobj object))
			  ahead)
	 (set! err? #f)))
     (lambda ()
       (when *out*
	 (deinitialize-io *out*)
	 (close-io *out* err?))))
    (if (or err? (not *out*)) 
      #f
      (if (is-a? *out* <event-stream>)
	(let ((path (file-output-filename *out*))
              (args (if (io-handler-args? *out*)
                      (io-handler-args *out*)
                      '()))
	      (hook (io-class-output-hook (class-of *out*))))
	  (when hook 
	    (apply hook path args))		; funcall
	  path)
	*out*))))

;Removed for now but may add it back.
;(defmacro defhandler (class handler)
;  (let ((var (gensym)))
;    `(let ((,var (find-class ',class <event-stream>)))
;      (set! (io-class-handler ,var) ,handler))))


; (load "/usr/local/lisp/scm/load.scm")
; (define a (new seq name 'foo))
; (io "test.clm" version #t)
; (events a "test.clm")


;;;
;;; write-event
;;;

(define-method (write-event obj io time)
  obj io time ; gag 'unused var' warnings from cltl compilers
  )
           
(define-method (write-event obj (io <seq>) time)
  (set! (object-time obj) time)
  (insert-object obj io))

;;;
;;; import-events
;;;

(define-method (import-events (file <string>) . args )
  (let ((io (init-io file)))
    (apply (function import-events) io args)))

;;;
;;; play
;;;

;(define-method (play (file <string>) . args)
;  (let ((io (apply init-io file args)))
;    (apply play io args)))
   
;(define-method (play file . args)
;  (format #t "No play method defined for ~s.~%" file))

