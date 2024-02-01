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
;;; $Revision: 1.8 $
;;; $Date: 2004/03/26 19:21:50 $

(define-method (copy-object (obj <object>))
  (let ((new (allocate-instance (class-of obj))))
    (fill-object new obj)
    new))

(define-method (fill-object (new <object>) (old <object>))
  (dolist (s (class-slots (class-of old)))
    (let ((n (slot-definition-name s)))
      (when (and (slot-exists? new n) (slot-bound? old n))
        (slot-set! new n (slot-ref old n))))))

;;; print-instance and #i
;;; print-instance prints instances as a list:
;;;   #i(class slot value ...)
;;; where #i is a read-macro that creates the object printed.
;;; If *print-object-terse* is #t then only initializations
;;; that are not the same as their :init-values are printed.
;;;

(define *print-instance* #t)

(define (print-instance obj port)
  (let ((class (class-of obj)))
    (format port "#i(~a"
            (string-downcase (symbol->string (class-name class))))
    (do ((slots (class-slots class) (cdr slots))
	 (d #f)
	 (s #f)
	 (v #f)
	 (k #f))
	((null? slots) #f)
      (set! d (car slots))
      (set! s (slot-definition-name d))
      (if (slot-bound? obj s)
	(begin
	 (set! v (slot-ref obj s))
	 (set! k (slot-definition-initargs d))
	 (unless (null? k)
	   (unless (and (eq? *print-instance* ':terse)
			(eq? v (slot-definition-initform d)))
	     (format port " ~a ~s" 
                     (string-downcase (symbol->string s))
                     v))))))
    (format port ")")
    obj))

(define (i-reader form)
  (if (pair? form)
    `(new ,@ form)
    (err "Can't make instance from ~s." form)))

(read-macro-set! #\i #'i-reader)
(read-macro-set! #\I #'i-reader) ; for cltl.

;;;
;;; cm class definitions. to remain consistent with cltl2 cm class names
;;; do not include <>. variables holding classes do. 
;;;

(define (save-object obj file)
  (let ((fp #f))
    (dynamic-wind (lambda () #f)
                  (lambda ()
                    (set! fp (open-file file :output))
                    (if (pair? obj)
                      (dolist (o obj)
                        (write (make-load-form o) fp))
                      (write (make-load-form obj) fp)))
                  (lambda ()
                    (if fp (close-file fp :output))))
    file))

;;;
;;; containers
;;;

(define *dictionary* (make-hash-table 31))

(define-accessor object-name)

(define-class <container> () 
  (name :init-value #f :accessor object-name 
	:init-keyword :name)
  :name 'container)

(define-method (print-object (obj <container>) port)
  (let ((name (object-name obj)))
    (if name
      (format port "#<~a \"~a\">" 
	      (string-downcase (symbol->string (class-name (class-of obj))))
	      name)
      (format port "#<~a @ x~a>" 
	      (string-downcase (symbol->string (class-name (class-of obj))))
	      (address->string (object-address obj))))))

(define-method (initialize (obj <container>) args)
  (next-method)
  (let ((name (object-name obj)))
    (when name
      (unless (string? name)
        (if (symbol? name)
          (set! name (string-downcase (symbol->string name)))
          (set! name (format #f "~a" name)))
	(set! (object-name obj) name))
      (hash-set! *dictionary*
		 (string-downcase name)
		 obj))
    (values)))

(define-method (make-load-form (obj <container>))
  `(make-instance ,(string->symbol
                    (format #f "<~a>" (class-name (class-of obj))))
    ,@ (slot-init-forms obj :eval #t :omit '(subobjects))
    :subobjects
    ,(cons 'list (map (function make-load-form)
                      (subobjects obj)))))

(define-generic rename-object )

(define-method (rename-object (obj <container>) newname . args)
  (let* ((err? (if (null? args) #t (car args)))
	 (str (if (string? newname) 
		newname (format #f "~a" newname)))
	 (old (find-object str )))
    (if old 
      (if (eq? obj old)
	old
	(if err?
	  (err "The name ~a already references ~s." newname old)
	  #f))
      (begin
       (hash-remove! *dictionary* (object-name obj))
       (set! (object-name obj) str)
       (hash-set! *dictionary* (string-downcase str) obj)
       obj))))

(define (list-named-objects . args)
  (with-args (args &optional type)
    (hash-fold (if type
                 (lambda (k v p) k    ; gag 'unused arg' cltl compilers
                         (if (is-a? v type) (cons v p) p))
                 (lambda (k v p) k    ; gag 'unused arg' cltl compilers
                         (cons v p)))
               '()
	       *dictionary*)))

(define (find-object string . args)
  (with-args (args &optional err? class)
    (let* ((name (if (string? string) string
                   (format #f "~a"  string)))
           (type (filename-type name))
           ;(ioc (find-class 'event-stream)) 
	   (find #f))
      (if (not type)
	(set! find (hash-ref *dictionary* (string-downcase name)))
	(let ((name (filename-name name))
              (path (filename-directory name)))
          (hash-fold 
           (lambda (k val res)
             ;; gag unused var warning from cltl compilers
             k res  
             (when #t ;(is-a? val ioc)
               ;; use case-sensitive file name
               (let* ((key (object-name val))
                      (typ (filename-type key)))
                 (if typ
                   (let ((nam (filename-name key))
                         (dir (or (filename-directory key) "")))
                     (if (and (string=? type typ)
                              (string=? name nam)
                              (or (not path)
                                  (string=? path dir)))
                       (if find
                         (err "More than one file named ~S." string)
                         (set! find val)))))))
             #f)
           #f
           *dictionary*))
	)
      (when (and class find) 
	(unless (is-a? find class)
	  (set! find #f)))
      (or find
	  (if err? (err "No object named ~s." string)
	      #f)))))

;(define ($-reader arg port)
;  (find-object (read port) #t))
;
;(read-hash-extend #\$ $-reader)

(read-macro-set! #\& (lambda (form) `(find-object ',form #t)))

;;;
;;; seq
;;;

(define-class <seq> (<container>) 
  (time :accessor object-time :init-keyword :time
	:init-value 0)
  (subobjects :init-value '() :accessor subobjects ;container-cycl
	      :init-keyword :subobjects)
  :name 'seq)

;;;
;;; default methods for object-name and object-time.
;;;

(define-method (object-name (obj <object>))
  (class-name (class-of obj)))

(define-method (object-time (obj <object>)) 0)

(define-method (subcontainers (obj <object>))
  '())

(define-method (subcontainers (obj <seq>))
  (loop for o in (subobjects obj)
        when (is-a? o <container>) collect o))

(define (map-subobjects fn container . args)
  (with-args (args &key key recurse test type)
    (let ((test (if type
		  (lambda (x) (is-a? x type))
		  test)))
      (if key
	(if test
	  (dolist (o (subobjects container))
	    (if (and recurse (is-a? o <container>))
	      (map-subobjects fn o :key key :recurse recurse :test test)
	      (when (test o) (fn (key o)))))
	  (dolist (o (subobjects container)) 
	    (if (and recurse (is-a? o <container>))
	      (map-subobjects fn o :key key :recurse recurse :test test)
	      (fn (key o)))))
	(if test
	  (dolist (o (subobjects container))
	    (if (and recurse (is-a? o <container>))
	      (map-subobjects fn o :key key :recurse recurse :test test)
	      (when (test o) (fn o ))))
	  (dolist (o (subobjects container))
	    (if (and recurse (is-a? o <container>))
	      (map-subobjects fn o :key key :recurse recurse :test test)
	      (fn o)))))
      (values))))

(define (map-subcontainers fn container . args)
  (with-args (args &key key recurse)
    (if key
      (dolist (o (subobjects container))
	(when (is-a? o <container>)
	  (fn (key o)))
	(if recurse (map-subcontainers fn o :key key :recurse recurse)))
      (dolist (o (subobjects container))
	(when (is-a? o <container>) (fn o))
	(if recurse (map-subcontainers fn o :key key :recurse recurse))))
    (values)))

(define-generic insert-object)
(define-generic append-object)
(define-generic remove-object)

(define-method (insert-object (sub <object>) (obj <seq>))
  (let ((earliest? #f)
	(subs (subobjects obj)))
    (if (null? subs)
      (let ((l (list sub)))
	(set! (subobjects obj) l)
	l)
      (let ((time (object-time sub)))
	(cond 
	  (( (if earliest? (function <=) (function <) )
	     time 
	     (object-time (car subs)))
	   (let ((l (cons sub subs)))
	     (set! (subobjects obj) l)
	     l))
	  (else
	   (do ((top subs)
		(head (cdr subs) (cdr subs)))
	       ((or (null? head)
		    (not ( (if earliest? (function <) (function <=))
			   (object-time (car head))
			   time)))
		(set-cdr! subs (cons sub head))
		top)
	     (set! subs head))))))))
      
(define-method (append-object (sub <object>) (obj <seq>))
  (let ((subs (subobjects obj)))
    (cond ((null? subs)
	   (set! subs (list sub))
	   (set! (subobjects obj) subs))
	  (else
	   (set-cdr! (last-pair subs) (list sub))))
    subs))

(define-method (remove-object sub (obj <seq>))
  (let ((subs (subobjects obj)))
    (unless (null? subs)
      (if (eq? sub (car subs))
	(begin (set! subs (cdr subs))
	       (set! (subobjects obj) subs))
	(do ((prev subs)
	     (tail (cdr subs) (cdr tail)))
	    ((or (null? tail)
		 (eq? sub (car tail)))
	     (set-cdr! prev (cddr prev)))
	  (set! prev tail))))
    subs))

(define-method (remove-subobjects (obj <seq>))
  (set! (subobjects obj) (list)))

(define-method (list-subobjects (obj <seq>) . args)
  (with-args (args &key start end start-time end-time)
    (let ((subs (subobjects obj)))
      (if (or start-time end-time)
	(begin
	 (when (or start end)
	   (err ":start and :end exclude :start-time and :end-time."))
	 (unless start-time (set! start-time 0.0))
	 (unless end-time (set! end-time most-positive-fixnum))
	 (do ((s subs (cdr s))
	      (i 0 (+ i 1)))
	     ((or (null? s) 
		  (> (object-time (car s))  end-time))
	      (values))
	   (unless (< (object-time (car s))  start-time)
	     (display (format-integer i 4 #\space))
	     (display ". ")
	     (write (car s) )
	     (newline))))
	(begin
	 (when (or start-time end-time)
	   (error ":start and :end exclude :start-time and :end-time."))
	 (unless start (set! start 0))
	 (unless end (set! end most-positive-fixnum))
	 (do ((s subs (cdr s))
	      (i 0 (+ i 1)))
	     ((or (null? s) (>= i end)) 
	      (values))
	   (unless (< i start)
	     (display (format-integer i 4 #\space))
	     (display ". ")
	     (write (car s) )
	     (newline)))))
      (values))))

;;;
;;;
;;;

(define-class <event> () 
  (time :accessor object-time 
	:init-keyword :time)
  :name 'event)

;;;
;;; new
;;;

(defmacro new (class . args)
  (let* ((type (or (find-class class)
		   (err "No class named ~s." class)))
	 (inits (expand-inits type args #t #f)))
    `(make-instance (find-class ',class) ,@ inits)))

;;;
;;; DEFOBJECT and parameter processing
;;;

(define (class-name->class-var sym)
  ;; allow name and var to be same
  (let ((str (symbol->string sym)))
    (if (char=? (string-ref str 0) #\<)
      sym
      (string->symbol (string-append "<" str ">")))))

(define (class-var->class-name sym)
  (let ((str (symbol->string sym)))
    (if (char=? #\< (string-ref str 0))
      (string->symbol (substring str 1 (- (string-length str) 1)))
      (err "Class variable not <~a>" sym))))

(define-list-struct parameter 
  slot (type 'required) time? prefix decimals)
  
(define (parse-parameters decl)
  (let ((req '())
	(opt '())
	(rest '())
	(key '())
	(aok '())
	(aux '())
	(par (lambda (p ty)
	       (if (pair? p)
		 (let* ((nam (pop p)))
		   ;; flush value decl
		   (if (odd? (length p)) (pop p))
		   (make-parameter :slot nam
				   :type ty
				   :prefix (if (eq? ty 'key)
					     (or (list-prop p ':prefix)
						 (symbol->keyword nam))
					     #f)
				   :decimals (list-prop p ':decimals)))
		 (make-parameter :slot p :type ty
				 :prefix (if (eq? ty 'key)
					   (symbol->keyword p)
					   #f))))))
    aok aux ; gag 'unused var' cltl compiler warning
    (multiple-value-setq (req opt rest key aok aux)
			 (parse-lambda-list decl))
    (append (map (lambda (p) (par p 'required)) req)
	    (map (lambda (p) (par p 'optional)) opt)
	    (map (lambda (p) (par p 'rest)) rest)
	    (map (lambda (p) (par p 'key)) key))))

; (parse-parameters '(a b c &key (d 1) e))

;(define (find-object-time-slot class)
;  ;; search CPL for a slot that uses object-time as an accessor
;  (if (null? class) 
;    #f
;    (if (pair? class)
;      (or (find-object-time-slot (car class))
;	  (find-object-time-slot (cdr class)))
;      (do ((slots (class-direct-slots class) (cdr slots)))
;	  ((or (null? slots)
;	       (eq? (slot-definition-reader
;		     (car slots))
;		    (function object-time)))
;	   (if (null? slots) 
;	     (find-object-time-slot 
;              (class-direct-superclasses class))
;	     (car slots)))))))
 
(define (insure-parameters pars decl supers)
  ;; each par must be defined in the local slot decl 
  ;; or be inherited from supers. 
  (let ((getslotd
         (lambda (slot sups)
           (do ((tail sups (cdr tail))
                (isit #f))
               ((or (null? tail) isit) isit)
             (set! isit (find slot (class-slots (car tail))
                              :key (function slot-definition-name)))))))
    (dolist (p pars)
      (or (find (parameter-slot p) decl :key (function car))
          (getslotd (parameter-slot p) supers)
          (err "No slot definition for parameter ~s."
               (parameter-slot p))))
    #t)) 

(define *time-slots*
  '(time start start-time starttime startime begin beg))

(define (find-time-parameter pars decl supers)
  ;; one par must have either:
  ;;   object-time in local slot decl
  ;;   name mentioned in time-slots.
  ;;   declared as time par in super-class
  (let ((gettimepar
         (lambda (slot sups)
           (do ((tail sups (cdr tail))
                (pars #f)
                (goal #f))
               ((or (null? tail) goal) goal)
             (set! pars (class-parameters (car tail)))
             (if pars
               (let ((test (find slot pars 
                                 :key (function parameter-slot))))
                 (if (and test (parameter-time? test))
                   (set! goal #t))))))))
    
    (do ((tail pars (cdr tail))
         (goal #f)
         (temp #f))
        ((or (null? tail) goal)
         (when goal (parameter-time?-set! goal #t))
         #t)
      (set! temp (assoc (parameter-slot (car tail)) decl))
      (if (and temp (member 'object-time (cdr temp)))
        (set! goal (car tail))
        (if (member (parameter-slot (car tail))
                    *time-slots*)
          (set! goal (car tail))
          (if (gettimepar (parameter-slot (car tail)) supers)
            (set! goal (car tail))))))))

(defmacro defobject (name supers slots . options)
  (let ((sups (map (lambda (x) (or (find-class x) 
				   (err "No class named ~s." x)))
		   supers))
        (decl (map (lambda (x) (if (pair? x) x (list x))) slots))
        (gvar (string->symbol (format #f "<~a>" name)))
	(make #t)
	(pars #f)
        (methods '()))
    
    (dolist (opt options)
      (unless (pair? opt)
	(err "defobject: not an options list: ~s" opt))
      
      (case (car opt)
	((:parameters)
         (set! pars opt))
	((:writers) 
         (set! make (cdr opt)))))
    
    (when pars
      (set! pars (parse-parameters (cdr pars)))
      ;; signal error if no slot for each par
      (insure-parameters pars decl sups)
      ;; signal warning if no time parameter
      (unless (find-time-parameter pars decl sups)
        (warning "No time parameter for ~s." 
                 name))
      
      ;; generate methods for each class of output stream
      (dolist (c (if (eq? make #t) (io-classes )
                     (map (function find-class) make)))
        (let ((fn (io-class-definer c)))
          (when fn
            (push (fn name gvar pars sups decl) methods))))
      (set! methods (reverse methods)))
    
    ;; expand-defobject is in level1
    (expand-defobject name gvar supers decl pars methods)))

;;;
;;; defprocess
;;;

(define (process-code-terminates? code stop)
  (if (null? code) #f
      (if (pair? code)
	(or (process-code-terminates? (car code) stop)
	    (process-code-terminates? (cdr code) stop))
	(eq? code (car stop)))))

(define (parse-process-clause forms clauses ops)
  clauses ops  ; gag 'unused var' message from cltl compilers
  (let ((head forms)
        (oper (pop forms))
        (expr #f)
        (args '())
        (loop '()))
    (when (null? forms)
      (loop-error ops head "Missing '" oper "' expression."))
    (set! expr (pop forms))
    (unless (null? forms)
      (case (car forms)
;        ((to into)
;         (unless (eq? oper 'output)
;           (loop-error ops head "'~s' is an unknown ~s modifier." 
;                    (car forms) oper))
;         (when (null? (cdr forms))
;           (itererr save "Missing '~s' expression." (car forms)))
;         (set! args (cadr forms))
;         (set! forms (cddr forms)))
        ((at )				
         (unless (eq? oper 'sprout)
           (loop-error ops head "'" (car forms) 
                       "' is an unknown '"
		       oper "' modifier."))
         (set! args
               (if (eq? (car forms) 'ahead)
                 `(+ (now) ,(cadr forms))
                 (cadr forms)))
         (set! forms (cddr forms)))))
    (case oper
      ((output )
       (set! loop (if (null? args)
		    (list `(,oper ,expr))
		    (list `(,oper ,expr ,(car args))))))
      ((wait )
       (set! loop (list `(wait ,expr))))
      ((wait-until )
       (set! loop (list `(wait-until ,expr))))
      ((sprout )
       (set! loop (list `(,oper ,expr , (if (null? args)
                                          '(now)
                                          args))))))
    (values (make-loop-clause 'operator oper 'looping loop)
            forms)))

(define (parse-set-clause forms clauses ops)
  clauses
  (let ((head forms)
        (oper (pop forms))
        (var #f)
        (=opr #f)
        (expr #f)
        (loop '()))
    (when (null? forms)
      (loop-error ops head 
                  "Variable expected but source code ran out."))
    (set! var (pop forms))
    (unless (symbol? var)
      (loop-error ops head "Found '" var 
                  "' where variable expected."))  
    (when (null? forms)
      (loop-error ops head "'=' expected but source code ran out."))
    (set! =opr (pop forms))
    (unless (eq? =opr '=)
      (loop-error ops head "Found '" =opr "' where '=' expected."))    
    (when (null? forms)
      (loop-error ops head "Missing '" oper "' expression."))
    (set! expr (pop forms))
    (set! loop (list `(set! ,var ,expr)))
    (values (make-loop-clause 'operator oper 'looping loop) 
            forms)))

(define (process-while-until forms clauses ops)
  clauses
  (let ((head forms)
        (oper (pop forms))
        (test #f)
        (stop (process-stop #f)))
    (when (null? forms)
      (loop-error ops head "Missing '" oper "' expression."))
    (case oper
      ((until) (set! test (pop forms)))
      ((while) (set! test `(not ,(pop forms)))))
    (values (make-loop-clause 'operator oper 'looping
             (list `(if ,test ,stop)))
            forms)))

(define *each-operators*
  (list (list 'as (function parse-for)
              'iter
              (list 'from (function parse-numerical-for))
              (list 'downfrom (function parse-numerical-for))
              (list 'below (function parse-numerical-for))
              (list 'to (function parse-numerical-for))
              (list 'above (function parse-numerical-for))
              (list 'downto (function parse-numerical-for))
              (list 'in (function parse-sequence-iteration))
              (list 'on (function parse-numerical-for))
              (list 'across (function parse-sequence-iteration))
              (list '= (function parse-general-iteration)))
        (list 'output (function parse-process-clause) 'task 'to 'into)
        (list 'sprout (function parse-process-clause) 'task 'at 'ahead)
        (assoc 'do *loop-operators*)))

(define (parse-each forms clauses ops)
  clauses
  (let ((save forms)
        (forms (cdr forms))
        (subs '())
        (each #f)
        (loop #f)
        (ends #f))
    
    (do ()
        ((or (null? forms)
             (loop-op? (car forms) (cdr *each-operators*))))
      
      ;; error if find process op before EACH op
      (if (and (not (eq? (car forms) 'as))
               (loop-op? (car forms) ops))
        (loop-error *each-operators* forms
                    "Expected 'each' action but found '" (car forms) 
                    "' instead."))
      (push (car forms) subs)
      (set! forms (cdr forms)))
    ;; error if no each stepping
    (when (null? subs)
      (loop-error *each-operators* save
                  "Missing 'each' stepping clause."))
    ;; error if no each action
    (when (null? forms)
      (loop-error *each-operators* save
                  "Expected 'each' action but source code ran out."))
    ;; gobble until next process clause after first action,
    ;; which belongs to each
    (do ((flag #t))
        ((or (null? forms)
             (and (not flag) ; dont check first clause...
                  (loop-op? (car forms) ops)))
         #f)
      (push (car forms) subs)
      (set! forms (cdr forms))
      (set! flag #f))
    
    (set! subs (reverse subs))
    (set! each (parse-iteration 'each (cons 'as subs)
                                *each-operators*))
    (if (null? (loop-end-tests each))
      (loop-error *each-operators* save "No 'each' end test?")
      (set! ends (loop-end-tests each)))
    
    (unless (null? (loop-initially each))
      (loop-error *each-operators* save 
                  "'each' does not support initializations."))
    (when (null? (loop-looping each))
      (loop-error *each-operators* save
                  "Expected 'each' action but source code ran out."))
    (set! loop
          (list `(,'do (,@(loop-bindings each))
                     (,(if (null? (cdr ends))
                         (car ends)
                         `(or ,@ends))
                      #f)
                   ,@(loop-looping each)
                   ,@(loop-stepping each))))
    
    (values (make-loop-clause 'operator 'each 'looping loop)
            forms)))

(define *process-operators*
  (append
   (map (lambda (op) (assoc op *loop-operators*))
        '(with initially repeat for as do finally when unless if))
   
   (list (list 'set (function parse-set-clause) 'task)
         (list 'output (function parse-process-clause) 'task 'to 'into)
         (list 'sprout (function parse-process-clause) 'task 'at 'ahead)
         (list 'wait (function parse-process-clause) 'task)
         (list 'wait-until (function parse-process-clause) 'task)
         (list 'each (function parse-each) 'task )
         (list 'while (function process-while-until) #f )
         (list 'until (function process-while-until) #f )
         )))

;;;
;;; need process expasion for cltl
;;;

(defmacro process forms
  (expand-process forms *process-operators*))


(defmacro defprocess forms
  (expand-defprocess forms))

;;;
;;; Forward chaining (ala Max) using lightweight 'boxes' as nodes.  A
;;; box is just a vector that associates a lisp function with optional
;;; funcall args and zero or more 'outboxes', the boxes to propagate
;;; values/funcalls in left-to-right, depth-first order.  see the
;;; 'bang!' function below for more info.  see cm/etc/examples/rt.cm
;;; for some example networks.
;;;

(define (box op . args)
  (vector op args '()))

(define (box? x)
  ;; any 3+ vector can be a box. chaining only uses the first
  ;; three elements so caching goodies in other locs is safe.
  (and (vector? x) (> (vector-length x) 2)))

(define (boxfunc box . func)
  ;; get/set box's function
  (if (null? func)
    (vector-ref box 0)
    (begin (vector-set! box 0 (car func))
           (car func))))

(define (boxargs box . args)
  ;; get/set box's funargs
  (if (null? args)
    (vector-ref box 1)
    (begin (vector-set! box 1 (car args))
           (car args))))

(define (boxouts box . outs)
  ;; get/set box's outboxes
  (if (null? outs)
    (vector-ref box 2)
    (begin (vector-set! box 2 (car outs))
           (car outs))))

;;;
;;; box-> sets the outboxes of a box to zero or more specified target
;;; boxes, for example:
;;;  (box-> a b c d)
;;; sets a's outboxes to be the boxes b c d, and
;;;  (box-> a)
;;; removes all existing outboxes from a.
;;;

(define (box-> box . boxes)
  (dolist (b boxes)
    (unless (box? b)
      (err "Outbox: ~s not a box." b)))
  (boxouts box boxes)
  (values))

;;; 
;;; bang! applies a box function to its args and then forward chains to
;;; all outboxes in depth-first, left-to-right order. the operator
;;; function can control forward chaining by returning an (optional)
;;; mode as its first value:
;;;   1. if the first value is :bang then forward outlets are banged 
;;;      with any remaining values:
;;;        (values :bang!)  => bangs outboxes without passing values
;;;        (values :bang! 1 2 3) => bangs outboxes with args 1 2 3.
;;;   2. if the first value is :stop! then propagation halts at the
;;;      current box. any remaining return values are ignored:
;;;        (values :stop!) => halts forward propagation
;;;        (values :stop! 1 2 3) => ditto
;;;   3. if the first value is :send! then any remaining values
;;;      are sent forward to the outboxes without a bang.
;;;        (values :send! 1 2 3) => sets outbox args to 1 2 3.
;;;   4. if the marker :argn appears directly after :bang! or :send!
;;;      the remaining values are handled PAIRWISE, where each
;;;      pair of values is interpreted:
;;;        {argn argval}* 
;;;      where argn is a positional index (zero-based) of an outbox arg
;;;      to set and argval becomes its value:
;;;        (values :bang! :argn 2 -99)
;;;          ==> bangs outbox with its arg[2] set to -99 
;;;        (values :send! :argn 0 'a 2 'c)
;;;          ==> sets arg[0] to A and C to arg[3] of outbox
;;;        (values :send! :argn)
;;;          ==> sets outbox args to ()
;;;   5.  Any other values are treated as an implicit :bang
;;;        (values)     => (values :bang!)
;;;        123          => (values :bang! 123)
;;;        (values 1 2) => (values :bang! 1 2)
;;;

(define (bang! box . args)
  ;; args override box's current args
  (let ((pmode ':bang!))   
    ;; parse propagation mode
    (cond ((null? args) #f)
          ((eq? (car args) ':bang!)
           (set! args (cdr args)))
          ((eq? (car args) ':send!)
           (set! pmode ':send!)
           (set! args (cdr args)))
          ((eq? (car args) ':stop!)
           (set! pmode ':stop!))
          (else #f))
    (if (eq? pmode ':stop!)
      (values)
      (begin
       ;; :bang! or :send!, check for :argn in first position
       (if (not (null? args))
         (if (eq? (car args) ':argn)
           (if (null? (cdr args))
             ;; let empty :argn FLUSH current args.
             (vector-set! box 1 (list))
             (let ((fnargs (vector-ref box 1)))
               (dopairs (n v (cdr args))
                 (list-set! fnargs n v))))
           ;; else set all args
           (vector-set! box 1 args)))
       (if (eq? pmode ':bang!)
         (let ((res (multiple-value-list
                        (apply (vector-ref box 0)
                               (vector-ref box 1)))))
           (dolist (o (vector-ref box 2))
             (apply (function bang!) o res))))
       (values)))))
