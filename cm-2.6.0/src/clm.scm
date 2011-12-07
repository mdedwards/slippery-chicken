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
;;; $Revision: 1.9 $
;;; $Date: 2005/03/20 02:09:26 $

;;;
;;; Definitions for <clm-stream> and <clm-audio-stream>
;;; See also: clm2
;;;

(define (clm-print-par-value v s)
  (when (pair? v) (write-char #\' s))
  (write v s))

(define (clm-par-print par objv filv timv)
  ;; return a form that prints parameter based on its type.
  ;; passed the object, filv and scoretime variables.
  (let* ((raw (if (parameter-time? par)
		timv
		(slot-getter-form objv (parameter-slot par))))
	 (acc (if (parameter-decimals par)
		`(decimals ,raw ,(parameter-decimals par))
		raw))
	 (delim #\space))
    (case (parameter-type par)
      ((required )
       `(begin (write-char ,delim ,filv)
         (clm-print-par-value ,acc ,filv)))
      ((optional )
       (let ((form `(clm-print-par-value ,acc ,filv)))       
	 `(if (slot-bound? ,objv ',(parameter-slot par))
	    (begin (write-char ,delim ,filv) ,form))))
      ((key )
       `(if (slot-bound? ,objv ',(parameter-slot par))
	  (begin 
	   (write-char ,delim ,filv)
	   (write ',(parameter-prefix par) ,filv)
	   (write-char ,delim ,filv)
	   (clm-print-par-value ,acc ,filv))))
      ((rest )
       (let ((v (gensym)))
	 `(if (slot-bound? ,objv ',(parameter-slot par))
	    (do ((,v ,(slot-getter-form objv (parameter-slot par))
                     (cdr ,v)))
	        ((null? ,v) #f)
	      (write-char ,delim ,filv)
	      (write (car ,v) ,filv))))))))

(define (clm-writer objclassname objclassvar pars supers sdecl )
  ;; defines a method on write-event for clm-stream and an event class.
  ;; called by defobject at macro expansion time, ie  before the event
  ;; has actually been defined.  pars is parameter list for the new 
  ;; event class. objclass is the variable name of the new class,
  ;; ie <bell>. supers is a list of event superclasses and sdecl are
  ;; the local slot definitions in the defobject form.
  
  supers sdecl ; gag 'unused var' message in cltl compilers
  (define-output-method objclassname objclassvar 'obj
    'clm-stream '<clm-stream> 'io 'scoretime
    (list `(let ((fp (io-open io)))
             (write-char #\( fp)
             (display (object-name obj) fp) ; allow strings or symbols.
             ,@ (map (lambda (p) (clm-par-print p 'obj 'fp 'scoretime))
                     pars)
                (write-char #\) fp)
                (newline fp)
                (values)))))

(define-class <clm-stream> (<event-stream>)
  (clmargs :init-value '() :accessor clm-args)
  :metaclass <io-class>           ; moved to the files.
  :name 'clm-stream
  :file-types '("*.clm")
  :mime-type "text/x-clm-score"
  :definer (function clm-writer))

(define (set-clm-output-hook! fn)
  (unless (or (not fn) (procedure? fn))
    (err "Not a clm output hook: ~s" fn))
  (set! (io-class-output-hook <clm-stream>) fn)
  (values))

(define (set-clm-file-versions! val)
  (set! (io-class-file-versions <clm-stream>) val)
  (values))

(define-method (io-handler-args? (io <clm-stream>))
  io
  #t)

(define-method (io-handler-args (io <clm-stream>))
  (clm-args io))

(define-method (set-io-handler-args! (io <clm-stream>) args)
  (set! (clm-args io) args)
  (values))

(define-method (initialize-io (io <clm-stream>))
  (when (eq? (io-direction io) ':output)
    (format (io-open io)
            ";;; ~a output on ~a~%"
            (cm-version)
            (date-and-time))))

;;; a default player for clm files.

(define (play-clm-file file . args)
  (apply (function clm-load) file :play #t args))

;;;
;;; <clm-audio-stream>
;;;
;;; snd-writer creates a method on write-event that implements
;;; direct-to-soundfile output by funcalling the object's
;;; associated clm instrument function with an arglist that is
;;; consed up from the object's data slots. the format of this
;;; arglist depends on the format of the instruments lambda
;;; parameter list.
;;; the methods that actually open/close <clm-audio-stream> are
;;; in clm2.lisp or clmsnd.scm
;;;

(define (snd-writer objclassname objclassvar pars supers sdecl )
  supers sdecl ; gag 'unused var' message in cltl compilers
  (let ((parf
	 (lambda (par objv argsv timv)
	   ;; return a form that set-cdr!s parameter data onto arg list.
	   (let* ((raw (if (parameter-time? par)
		         timv
		         (slot-getter-form objv (parameter-slot par))))
	          (acc (if (parameter-decimals par)
		         `(decimals ,raw ,(parameter-decimals par))
		         raw)))
	     (case (parameter-type par)
	       ((required )
		`(begin
		  (set-cdr! ,argsv (list ,acc))
		  (set! ,argsv (cdr ,argsv))))
	       ((optional )
		`(if (slot-bound? ,objv ',(parameter-slot par))
		   (begin
		    (set-cdr! ,argsv (list ,acc))
		    (set! ,argsv (cdr ,argsv)))))
	       ((key )
		`(if (slot-bound? ,objv ',(parameter-slot par))
		   (begin 
		    (set-cdr! ,argsv (list ',(parameter-prefix par)))
		    (set! ,argsv (cdr ,argsv))
		    (set-cdr! ,argsv (list ,acc))
		    (set! ,argsv (cdr ,argsv)))))
	       ((rest )
		`(if (slot-bound? ,objv ',(parameter-slot par))
		   (begin 
		    (set-cdr! ,argsv ,acc)
		    (set! ,argsv (cdr ,argsv))))))))))
    
    (define-output-method objclassname objclassvar 'obj
      'clm-audio-stream '<clm-audio-stream> 'io 'scoretime
      (list 
       `(let* ((args (list #f))
               (tail args))   ; <- this var is appended to

          (if (eq? (slot-ref io 'output-trace) #t)
            (format #t "~a ~s..."
                    (object-name obj) (decimals scoretime 3)))

          ,@ (map (lambda (p) (parf p 'obj 'tail 'scoretime)) pars)
             (apply (symbol-function (object-name obj))
                    (cdr args))
             (values))))))

(define-class <clm-audio-stream> (<event-stream>)
  (clmargs :init-value '() :accessor clm-args)
  (output-trace :init-value :info
		:init-keyword :trace-output
		:accessor audio-file-output-trace)
  :name 'clm-audio-stream
  :metaclass <io-class>           ; moved to the files.
  :file-types '("*.snd" "*.aiff" "*.wav")
  :mime-type "audio/x-clm-audio"
  :definer (function snd-writer))

(define (set-audio-output-hook! fn)
  (unless (or (not fn) (procedure? fn))
    (err "Not an audio output hook: ~s" fn))
  (set! (io-class-output-hook <clm-audio-stream>) fn)
  (values))

(define (set-audio-file-versions! val)
  (set! (io-class-file-versions <clm-audio-stream>) val)
  (values))

(define-method (io-handler-args? (io <clm-audio-stream>))
  io
  #t)

(define-method (io-handler-args (io <clm-audio-stream>))
  (clm-args io))

(define-method (set-io-handler-args! (io <clm-audio-stream>) args)
  (set! (clm-args io) args)
  (values))

;;;
;;; apparently there is no definstrument i can shadow to autocreate
;;; objects for instruments. this function can be used to parse a
;;; instrument declaration like (foo a b &key c d e) into the
;;; "equivalent" defobject form.
;;;

(define (formals->defobject form . args)
  (with-args (args &optional tpar)
    (let* ((&keys '(&optional &rest &key &aux &allow-other-keys))
	   (name (pop form))
	   (pars (map (lambda (x)
                        (case x
                          ((:optional) '&optional)
                          ((:rest) '&rest)
                          ((:key) '&key)
                          ((:aux) '&aux)
                          ((:allow-other-keys) '&allow-other-keys)
                          (else (if (pair? x) (car x) x))))
                      form))
           (slots '()))
      (do ((tail pars (cdr tail)))
          ((null? tail)
           (set! slots (reverse slots)))
        (unless (member (car tail) &keys)
          (if (or (and tpar (equal? (car tail) tpar))
                  (member (car tail)
                          *time-slots*))
            (push (list (car tail) :accessor 'object-time) slots)
            (push (car tail) slots))))
      `(defobject , name () ,slots (:parameters ,@ pars)))))
   
; (formals->defobject '(fm beg dur frq amp &optional amp-env ind-env ind))

;(defobject fm ()
;  ((time :accessor object-time)
;   duration
;   frequency
;   amplitude
;   (amplitude-env :initform '(0 0 25 1 75 1 100 0))
;   (mratio :initform 1)
;   (index :initform 1)
;   (index-env :initform '(0 1 100 1))
;   (degree :initform 0)
;   (distance :initform 0)
;   (reverb :initform 0))
;  (:parameters time duration frequency amplitude
;	       &key amplitude-env mratio index index-env
;	       degree distance reverb) )

;;;
;;; CLM importing
;;;

(define *clm-imports* (list))

(define *clm-import-translations*
  ;; Each translation spec is (<form> <function>)
  '((let import-let)
    (let* import-let)
    (progn import-progn)
    (clm:with-sound import-with-sound)
    (defun import-defun)
    ))


;; import-form translates list expressions whose first element
;; is the name of an object class that has event parameters associated
;; with it, or whose first element can be found on the :translations list.

(define (import-form form translate exclude include . args)
  (with-args (args &optional toplevel?)
    (let ((sym (car form)))
      (if (or (not (symbol? sym))
              (and toplevel? (member sym exclude)))
        #f
        (let* ((obj (find-class sym #f))
               (pars (and obj (class-parameters obj))))
          (if pars
            (import-object pars form)
            (let ((trans (assoc sym translate)))
              (if trans
                ( (cadr trans) form translate exclude include)
                ;; arrg have to figure this out for Scheme!
                (if (or (FBOUNDP SYM)
                        (SPECIAL-OPERATOR-P sym))
                  form
                  (if (and include
                           (or (eq? include #t)
                               (member sym include)))
                    form
                    (begin
                     (format #t "~%Skipping undefined function: ~s."
                             sym)
                     #f)))))))))))

(define (import-let form translate exclude include)
  ;; walk let body to translate forms
  (let ((body (loop for f in (cddr form)
                 for r = (import-form f translate exclude include )
                 when r collect r)))
    (if body
      (list* (car form)           ; let/let*
             (cadr form)          ; bindings
             body )               ; forms
      #f)))

(define (import-progn form translate exclude include)
  ;; walk progn body to translate forms
  (let ((body (loop for f in (cdr form)
                    for r = (import-form f translate exclude include t)
                    when r collect r)))
    (if body
      `(progn ,@ body)
      #f)))

(define (import-with-sound form translate exclude include)
  ;; walk with-sound body to translate forms. return T as second
  ;; value so forms are "spliced" into output file.
  (values (loop for f in (cddr form)
                for r = (import-form f translate exclude include t)
                when r collect r) 
          #t))

(define (import-defun form trans excl inc)
  (list* 'defun (cadr form) (caddr form)
         (loop for f in (cdddr form)
            collect (import-form f trans excl inc))))

(define (import-object pars forms)
  ;; translate form whose car is the name of an event class into 
  ;; a NEW expression that creates the object. expressions in the
  ;;body of form are parsed according to the object's parameters.
  (let ((save forms)
        (name (pop forms))
        (reqs #f)
        (opts #f)
        (rest #f)
        (keys #f))

    (set! reqs
          (loop with par 
             while (and (not (null? forms))
                        (eq? (parameter-type (car pars))
                             'required))
             do (set! par (pop pars))
             collect (parameter-slot par) collect (pop forms)))
    (set! opts
          (loop with par while (and (not (null? forms))
                                    (eq? (parameter-type (car pars)) 
                                         'optional))
             do (set! par (pop pars))
             collect (parameter-slot par) collect (pop forms)))
    (when (and (not (null? forms))
               (eq? (parameter-type (car pars)) 'rest))
      (set! rest forms)
      (pop pars))
    (set! keys
          (loop with par
             while (not (null? forms))
             do
               (set! par (find (car forms) pars 
                               :key (function parameter-prefix)))
               (or par
                   (err "No slot for ~s in ~s." (car forms) save))
             collect (parameter-slot par) collect (cadr forms)
             do (set! forms (cddr forms))))
    `(push (new ,name ,@reqs ,@opts ,@rest ,@keys)
           *clm-imports*)))

(define-method (import-events (io <clm-stream>) . args)
  (with-args (args &key (output #f)
                   (translations *clm-import-translations*)
                   (include ()) (exclude ()) (seq #t))
    (let* ((clmname (io-filename io))
           (seqname (format #f "from-~a"
                            (filename-name clmname)))
           (*print-case* ':downcase)
           (fprint (lambda (f s)
                     (format s "~S~%" f)))
           (outfil #f))
      (unless output
        (set! output
              (string-append (filename-directory clmname)
                             (filename-name clmname)
                             ".cm")))
      (unless (or (eq? include #t) (list? include))
        (err ":include value not ~s or list: ~s" #t include))
      (unless (list? exclude)
        (err ":exclude value not list: ~s." exclude))
      (set! outfil (open-file output :output))
      ;; read each form in clm and and add objects to output file.
      (with-open-io (infil io :input)
        (set! infil (io-open io))
        (format outfil ";;; Imported from ~s on ~a~%" 
                clmname (date-and-time))
        (fprint `(set! *clm-imports* (list)) outfil)
        (loop with trans and flag 
           for form = (file-form infil)
           until (file-eof? form)
           when (and (pair? form)
                     (symbol? (car form)))
           do
           ;; update include list with interal defmacro or defun 
           ;; definitions unless the names are specifically exluded.
           (unless (eq? include #t)     ; doing all anyway
             (when (member (car form) '(defun defmacro define))
               (unless (member (cadr form) exclude)
                 (unless (member (cadr form) include)
                   (push (cadr form) include)))))
           (multiple-value-setq (trans flag) 
             (import-form form translations exclude include #t))
           (when trans
             (if flag              ; if T then splice in translations.
               (dolist (x trans)
                 (fprint x outfil))
               (fprint trans outfil))))
        ;; value of imports is either a seq or a list of objects.
        (if seq
          (fprint `(set! *clm-imports* 
                         (new seq :name ,(if (eq? seq #t)
                                             seqname `(quote ,seq))
                              :subobjects (reverse! *clm-imports*)))
                  outfil)
          (fprint `(set! *clm-imports* (reverse! *clm-imports*))
                  outfil)))
      (close-file outfil ':output)
      (set! *clm-imports* (list))
      (load output)
      *clm-imports*)))


