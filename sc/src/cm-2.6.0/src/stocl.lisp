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
;;; $Revision: 1.11 $
;;; $Date: 2005/03/17 19:38:25 $

(in-package :cl-user)

;;; This file should be executed in a base lisp without CM loaded.
;;; It should work in any CLTL image. Ive tested in OPENMCL and CLISP.
;;; The code translates CM's scheme/goops source files into cltl2/clos.
;;; Most of the functions are general; those that are specific to cm
;;; can be found by searching for "CM". The translation process
;;; is able to handle all of cm's sources without modification. 
;;; To translate a scheme file call the function STOCL and give it the
;;; scheme file.  The function uses two global variables to control
;;; the translation process: 
;;;
;;; toplevel-ignore 
;;;   holds forms that the translator should omit in the output cltl
;;;   file. each entry is (type . names ) or (type t) if all every
;;;   occurance of type should be skipped. for example, the entry
;;;   (define-generic t) skips every occurance of a define-generic 
;;;   form in the scheme file.
;;;
;;; toplevel-translations
;;;   a list of rewrite entries where each entry is a list:
;;;   (scheme-name cltl-name . <rewrite-function>)
;;;   see the comments associated with the varible for more info.
;;;
;;; most of the translation work is done by the function scheme->cltl.
;;; It performs the basic walk and calls rewrite functions when
;;; it encounters a form with an entry in toplevel-translations.
;;; All of the rewrite functions have access to a stack of local
;;; scheme variables that may (possibly) become functions. This
;;; is the 'env' parameter to all the rewrite functions. Note that
;;; the code walker cannot find scheme variables that hold
;;; functions that are not actually called as fuctions. (The CM sources
;;; use a no-op macro called FUNCTION to mark these in the 
;;; scheme source code.)
;;; Backquote forms are a special problem. The code translates these
;;; as well, but the algorithm is not foolproof. see the tests later
;;; in the file for more info.
;;;
;;;
;;; Directions: load this file then either 
;;;  1. call (gencm) for all the sources.
;;;  2. load pkg.lisp by hand and call stocl on the file you want.

(defparameter srcdir 
  (make-pathname :name nil :type nil
                 ;; truname because load path can be "" if pwd=cm/src !
                 :defaults (truename *load-pathname*)))

(defun srcfile (&rest path)
 (let ((subs (butlast path))
       (file (first (last path)))
       (here srcdir))
   (when subs
     (setf here (make-pathname
                 :directory (append (pathname-directory here)
                                    subs)
                 :defaults here)))
   (namestring (merge-pathnames file here))))

(unless (find-package :clm)
  (load (srcfile "clm-stubs.lisp")))
(unless (find-package :cmn)
  (load (srcfile "cmn-stubs.lisp")))
(unless t ;(find-package :cmn)
  (load (srcfile "midishare" "midishare-stubs.lisp")))
(load (srcfile "pkg.lisp"))

(defun gencm (&rest args &aux verb)
  (when (eql (car args) :verbose)
    (pop args)
    (setq verb (pop args)))
  (if (not args)
    (progn
      (stocl (srcfile "loop.scm") :file "iter" :verbose verb)
      (stocl (srcfile "utils.scm") :verbose verb)
      (stocl (srcfile "mop.scm") :verbose verb)
      (stocl (srcfile "objects.scm") :verbose verb)
      (stocl (srcfile "io.scm") :verbose verb)
      (stocl (srcfile "scheduler.scm") :verbose verb)
      (stocl (srcfile "sco.scm") :verbose verb)
      (stocl (srcfile "clm.scm") :verbose verb)
      (stocl (srcfile "clm2.scm") :verbose verb)
      (stocl (srcfile "cmn.scm") :verbose verb)
      (stocl (srcfile "midi1.scm") :verbose verb)
      (stocl (srcfile "midi2.scm") :verbose verb)
      (stocl (srcfile "midi3.scm") :verbose verb)
      (stocl (srcfile "midishare" "midishare.scm") :verbose verb)
      (stocl (srcfile "midishare" "player.scm") :verbose verb)
      (stocl (srcfile "data.scm") :verbose verb)
      (stocl (srcfile "scales.scm") :verbose verb)
      (stocl (srcfile "spectral.scm") :verbose verb)
      (stocl (srcfile "patterns.scm") :verbose verb))
    (dolist (f args)
      (stocl (srcfile f) :verbose verb)))
  (values))

(defvar %false nil)
(defvar %true t)

;;; Define a translation readtable with Scheme dispatch macros
;;; and special expansions of ` , and ,@ 

(defvar %readtable (copy-readtable nil))

(set-dispatch-macro-character #\# #\f #'(lambda (&rest a) a %false) 
                              %readtable)

(set-dispatch-macro-character #\# #\t #'(lambda (&rest a) a %true)
                              %readtable)

;; unique tokens for marking backquote units

(defvar %bquote (gensym "BQUOTE"))
(defvar %bcomma (gensym "BCOMMA"))
(defvar %bsplice (gensym "BSPLICE"))

(defun bquote-reader (stream char)
  ;; Our backquote reader expands
  ;; `(foo) to (#:BQUOTE (FOO))
  (declare (ignore char))
  (list %bquote (read stream t nil t)))

(defun comma-reader (stream char)
  ;; Our comma reader exapands
  ;; ,foo to (#:BCOMMA FOO)
  ;; ,@foo to (#:BSPLICE FOO)
  (declare (ignore char))
  (let ((char (peek-char nil stream)))
    (if (char= char #\@)
      (progn (read-char stream)
             (list %bsplice (read stream t nil t)))
      (list %bcomma (read stream t nil t)))))
      
(set-macro-character #\` #'bquote-reader nil %readtable)

(set-macro-character #\, #'comma-reader  nil %readtable)

#|
(setf x
      (let ((*readtable* %readtable))
        (read-from-string "`(list 1 ,pi 3 ,@ (set! 1 , 2))")))
(backquote-form? x)
(rewrite-backquote x)
|#

;;;
;;; toplevel forms matching entries in toplevel-ignore
;;; are not written in the output file.
;;;

(defparameter toplevel-ignore
  '((defmacro loop when unless push pop function)
    (define scheme-loop read-byte write-byte expand-process
      signum clm:definstrument)
    (define-accessor object-name)
    (define-generic t)
    (define-method describe-object)
    ))

;;;
;;; the translation table. forms matching the first entry are
;;; either replaced by the second or REWRITTEN by the third.
;;; if there is a third entry the second is ignored.
;;; The entries below are not the complete scheme!
;;;

(defparameter toplevel-translations
  '((append!             nconc)
    (begin               progn)
    ;(boolean?            nil             boolean?->typep)
    (bound?              boundp)
    (call-with-values    nil             cwv->mvb)
    (case                case 	         case->case)
    (char?               characterp)
    (char->integer       char-code)
    (char-numeric?       digit-char-p)
    (char=?              char=)
    (char-ci=?           char-equal)
    (cond                cond 	         case->case) ; handles both
    (define              nil             define->defun/defparameter)
    (defmacro            defmacro        defmacro->defmacro)
    (display             princ)
    (do                  do              do->do)
    (dolist              dolist          dolist->dolist)
    (dotimes             dotimes         dolist->dolist) ; does both
    (dynamic-wind        nil             dynamic-wind->unwind-protect)
    (eq?                 eq)
    (equal?              equal)
    (even?               evenp)
    (exact?              integerp)
    (file-exists?        probe-file)
    (for-each            nil              for-each->map)
    (hash-ref            gethash          hash-ref->gethash)
    (hash-remove!        remhash          hash-remove->remhash)
    (hash-set!           nil              hash-set!->setf-gethash)
    (inexact?            floatp)  ; this isnt really true..
    (inexact->exact      nil              inexact->round)
    (integer?            integerp)
    (integer->char       code-char)
    (lambda              lambda           lambda->lambda)
    (let                 let              let->let)
    (let*                let*             let->let)
    (letrec              nil              let->let)
    (last-pair           last)
    (list?               listp)
    (list-copy           copy-list)
    (list-ref            elt   )
    (list-set!           nil              element-set!->setf-elt)
    (list-tail           nil              list-tail->nthcdr)
    (list->string        coerce           list->coerce-string)
    (logbit?             logtest)
    (make-hash-table     nil              mht->mht)
    (make-list           make-list        make-list->make-list)
    (make-string         make-string      make-string->make-string)
    (make-vector         make-array       make-vector->make-array)
    (map                 mapcar           map->mapcar)
    (memq                member           memq->member)
    (modulo              mod)
    (newline             terpri)
    (negative?           minusp)
    (null?               null)
    (number?             numberp)
    (number->string      prin1-to-string)
    (odd?                oddp)
    (pair?               consp)
    (positive?           plusp)
    (procedure?          functionp)
    (reverse!            nreverse)
    (set!                setf             set!->setf)
    (set-car!            rplaca)
    (set-cdr!            rplacd)
    (sort!               sort)
    (string              make-string)
    (string?             stringp)
    (string=?            string=)
    (string-append       nil              string-append->concat)
    (string-capitalize!  nstring-capitalize)
    (string-ci<?         string-lessp)
    (string-ci=?         string-equal)
    (string-length       length)
    (string->number      read-from-string)
    (string->symbol      nil              string->intern)
    (string-ref          elt)
    (string-set!         nil              element-set!->setf-elt) 
    (substring           subseq)
    (string              coerce           string->coerce)
    (symbol?             nil              symbol?->symbolp)
    (symbol->string      symbol-name)
    (vector?             vectorp)
    (vector-length       length)
    (vector-ref          elt)
    (vector-set!         nil              element-set!->setf-elt)
    (write               write            write->write)
    (zero?               zerop)
    
    ;;---- GOOPS:
    
    (define-method        defmethod        define-method->defmethod)
    (define-class         defclass         define-class->defclass)
    (initialize           initialize-instance )
    (make                 make-instance )
    (is-a?                typep)
    (next-method          call-next-method)
    (slot-exists?         slot-exists-p)
    (slot-bound?          slot-boundp)
    (slot-ref             slot-value)
    (slot-set!            nil              slot-set!->setf-slot-value)
    
    ;;---- CM:
    
    (bound?               boundp)
    (cdr-pop              nil              cdr-pop->pop-cdr)
    (defobject            nil              defobject->defobject)
    (dopairs              dopairs          dopairs->dopairs)
    (err                  error)
    (function             nil function->function)
    (hash-clear!          clrhash)
    (clfloor              floor)
    (clround              round)
    (list-prop            getf)
    (list-delete-if!      nil              list-delete-if->delete-if)
    (logn                 log)
    (multiple-value-bind  nil              mvb->mvb)
    (multiple-value-setq  nil              mvs->mvs)
    
    ))

(defun toplevel-ignore? (form)
  (if (consp form)
    (let ((entries (assoc (car form) toplevel-ignore)))
      (if entries
        (let ((match (if (consp (cadr form))
                       (caadr form)
                       (cadr form))))
          (if (or (eql (cadr entries) t)
                  (find match (cdr entries)))
            (list (car entries) match)
            nil))
        nil))
    nil))

;;;
;;; optional package declaration to be written at top of output file.
;;;

(defparameter package ':cm)

;;;
;;; header is a string to put at the top of the output file.
;;;

(defparameter header
  ";;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************~%")

(defun stocl (scm &key (verbose t) file   ; override scm file name
                  directory             ; override scm directory
                  (package package))
  (let ((*readtable* %readtable)
        (*print-case* ':downcase)
        (*print-right-margin* 70)
        (*print-pretty* T)
        (lisp
         (make-pathname :name (if file
                                (pathname-name file)
                                (pathname-name scm))
                        :type "lisp"
                        :defaults (or directory scm))))
    (if (probe-file lisp)
      (delete-file lisp))
    (flet ((tracename (form)
             (if (consp form)
               (let ((name (format nil "~a" (car form))))
                 (if (string-equal name "DEF" :end1 3)
                   (format nil "~a ~a" (car form) (cadr form))
                   (format nil "~a" (car form))))
               (format nil "~a" form)))
           (datestr ()
             (multiple-value-bind (sec min hour day mo year)
                 (get-decoded-time)
               (format nil "~2,'0d-~a-~4,'0d ~2,'0d:~2,'0d:~2,'0d"
	               day 
                       (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
                                 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                              (1- mo))
                       year hour min sec))))
      (with-open-file (in scm)
        (with-open-file (out lisp :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
          (format out header)
          (format out "~%;;; generated by scheme->cltl from ~A.~A ~
                       on ~A~%~%"
                  (pathname-name in) (pathname-type in) (datestr))
          (when package
            (format out "(in-package ~s)~%" package))
          
          (loop with skip and name
                for form = (read in nil ':eof)
                until (eql form ':eof)
                do
                (setf skip (toplevel-ignore? form))
                (if verbose (setf name (tracename form)))
                (cond (skip
                       (when verbose
                         (format t "~%skipping ~A" name)))
                      (t
                       (let ((expr (scheme->cltl form nil)))
                         (when verbose
                           (format t "~%rewriting ~A" name ))
                         (pprint expr out)
                         (terpri out))))))))
    lisp))
        
;;;
;;; scheme->cltl is the main function. 
;;; 
 
(defun scheme->cltl (form &optional env mode)
  ;; env is list of variables in effect
  ;; mode is nil or :quote
  (cond ((null form) '())
        ((consp form)
         (cond ((eq mode ':quote)
                (loop for f in form
                      collect (scheme->cltl f nil ':quote)))
               ((eql (car form) 'quote)
                ;; interpret 'NIL as '() in the output
                ;; this is done by substituting a clos object
                ;; whose print-object method insures that
                ;; NIL is printed as '()
                (if (eq (cadr form) nil)
                  (make-instance 'empty-list)
                  `(quote ,(scheme->cltl (cadr form) nil ':quote))))
               ;; see backquote hackery later in file...
               ((backquote-form? form)
                (rewrite-backquote form env))
               ;; if first form in list is a list and 
               ;; :quote is not in effect its a schemish funcall
               ((not (symbolp (car form)))
                `(funcall ,(car form)
                          ,@(loop for f in (cdr form)
                                  collect (scheme->cltl f env))))
               ;; if the first form is a variable and the
               ;; list is not quoted its a scheme funcall
               ((member (car form) env)
                `(funcall ,(car form)
                          ,@(loop for f in (cdr form)
                                  collect (scheme->cltl f env))))
               (t 
                ;; otherwise
                (let ((e (assoc (car form) 
                                toplevel-translations)))
                  (if e
                    (if (null (cddr e))
                      (cons (cadr e) 
                            (loop for f in (cdr form)
                                  collect (scheme->cltl f env)))
                      (funcall (caddr e) form ENV))
                    (loop for f in form
                          collect (scheme->cltl f env)))))))
        ((eql form %false) 'nil)
        ((eql form %true) 't)
        (t form)))

;;;
;;; special forms and form that augment the environment with
;;; variable binding.
;;;

(defun let->let (form &optional env)
  ;; form is let, let* or letrec.
  ;; walk bindings and check for lambda vars and rewrite these
  ;; as either flet or labels.
  (let* (;(seq? (or (eql (first form) 'let*)
         ;          (eql (first form) 'letrec)))
         (rec? (eq (car form) 'letrec))
         (bind '())
	 (funs '())
         (vars '())
	 (body nil))
    ;; process bindings
    (loop for b in (cadr form)
          do
          (unless (consp b)
            (error "let binding ~s not a list." b))
          ;; have to augment environment as each
          ;; binding is processed
          ;(if seq? (setf env (append env (list (car b)))))
          
          (if (and (consp (cadr b))
	           (eql (car (cadr b)) 'lambda))
	    (push 
             (let ((r (lambda->lambda (cadr b) env)))
               (cons (car b) (cdr r)))
             
             funs )
	    (push (list (car b) (scheme->cltl (cadr b) env))
                  bind )))
    (setf funs (reverse funs))
    (setf bind (reverse bind))
    (setf vars (mapcar #'first bind))
    
    (unless nil ;seq? 
      (setf env (append vars env)))
    
    (setf body 
          (loop for x in (cddr form)
                collect (scheme->cltl x env)))
    (if (null vars)
      (if funs
        (if rec?
          `(labels ,funs ,@body)
          `(flet ,funs ,@body))
        `(let () ,@body))
      (if (null funs)
        `( ,(car form) ,bind ,@body)
        `(, (if rec? 'labels 'flet)
            ,funs
            ( ,(if rec? 'let* (car form)) ,bind ,@body))))))

(defun cwv->mvb (form &optional env)
  ;; form is (call-with-values thunk lambda)
  (let* ((thunk (cadr form))
         (func (caddr form))
         (pars (cadr func))
         (call (if (cdddr thunk)
                 `(progn ,@(loop for f in (cddr thunk)
                                 collect (scheme->cltl f env)))
                 (scheme->cltl (caddr thunk) env))))
    `(multiple-value-bind 
       ,pars
       ,call
       ,@(loop for f in (cddr func)
               collect (scheme->cltl f (append pars env))))))

(defun mvb->mvb (form &optional env)
  ;; CM: cm implements multiple-value-bind too.
  (let* ((env (append (second form) env)))
    `(multiple-value-bind 
       ,(second form)
       ,@(loop for x in (cddr form)
               collect (scheme->cltl x env)))))

(defun mvs->mvs (form &optional env)
  ;; use this expander so the vars are not walked. otherwise 
  ;; first var will be rewritten as funcall!
  `(multiple-value-setq
     ,(second form)
     ,@(loop for x in (cddr form)
             collect (scheme->cltl x env))))

(defun lambda->lambda (form &optional env defun?)
  (let* ((args (cadr form))
         (rest nil)
         (bind '())
         (vars '()))
    (loop while (consp args)
          do (push (pop args) bind)
          finally (when args (setf rest args)))
    ;; bind is now in reverse order
    ;; CM: if function uses with-args wrapper then use
    ;; that declaration for the cltl2 lambda args list.
    (if (and (consp (car (cddr form)))
             (eql (car (car (cddr form))) 'with-args))
      (let ((pars (rest (cadr (car (cddr form))))))
        ;; add with-args to the list of required lambda pars.
        (setf bind 
              (append (reverse bind)
                      (loop for p in pars
                            collect
                            (if (find p lambda-list-keywords)
                              p
                              (if (consp p)
                                (let ((len (length p))
                                      (lst '()))
                                  ;; passed var
                                  (if (> len 2)
                                    (push (third p) lst))
                                  ;; param value
                                  (if (> len 1)
                                    (push (scheme->cltl (cadr p) env)
                                          lst))
                                  ;; param name
                                  (push (car p) lst)
                                  lst)
                                p)))))
        (setf vars (loop for p in bind
                         unless (find p lambda-list-keywords)
                         collect 
                         (if (consp p)
                           (car p)
                           p)))
        ;; remove the with-args wrapper
        (if (cdddr form)
          (error "with-args not wrapper: ~S" form)
          (setf form (list* nil nil (cddr (car (cddr form)))))))
      (progn
        (when rest 
          (push '&rest bind)
          (push rest bind))
        (setf bind (reverse bind))
        (setf vars (loop for b in bind
                         unless (eql b '&rest)
                         collect (if (consp b) (car b) b)))))
    `(,(if defun? 'defun 'lambda)
      ,@(if defun? (list defun?) nil)
      ,bind
      ,@ (loop for f in (cddr form)
               collect (scheme->cltl f (append vars env))))))

(defun function->function (form &optional env)
  ;; maybe rewrite the function symbol
  (if (symbolp (second form))
    (let ((e (assoc (second form) toplevel-translations)))
      (if e
        `(function ,(or (second e) (error "shouldnt")))
        form))
    `(function , (scheme->cltl (second form) env))))

(defun define->defun/defparameter (form &optional env)
  (pop form) ; flush define
  (if (consp (car form))
    ;; ((foo a b) ...)
    (lambda->lambda `(lambda ,(cdr (car form)) ,@(cdr form))
                    env (caar form))
    `(defparameter ,(car form) 
       ,(scheme->cltl (cadr form) env))))

(defun defmacro->defmacro (form &optional env)
  env
  (let ((args (third form))
        pars rest)
    (loop while (consp args)
          do (push (pop args) pars)
          finally (when args (setf rest args)))
    (when rest 
      (push '&body pars)
      (push rest pars))
    `(defmacro ,(second form) ,(reverse pars)
       ,@ (loop for x in (cdddr form)
                collect (scheme->cltl x env)))))

(defun case->case (form &optional env)
  ;; form is (case ...) or (cond ...)
  (let* ((cond? (eql (car form) 'cond))
         (body (if cond? (cdr form) (cddr form))))
    `(, (car form)
        ,@ (if cond? nil (list (scheme->cltl (cadr form) env)))
           ,@ (loop for c in body
                    collect
                    `(, (if (eql (car c) 'else) t
                            (if cond? 
                              (scheme->cltl (car c) env)
                              (scheme->cltl (car c) nil ':quote)))
                        ,@(loop for f in (cdr c)
                                collect (scheme->cltl f env)))))))



(defun do->do (form &optional env)
  (let ((bind (cadr form))
        (test (caddr form))
        (body (cdddr form)))
    (setf bind (loop for b in bind
                     collect
                     (cons (car b)
                           (loop for f in (cdr b)
                                 collect (scheme->cltl f env)))))
    (setf env (append env (mapcar #'first bind)))
    (setf test
          (loop for f in test 
                collect (scheme->cltl f env)))
    (setf body (loop for f in body
                     collect (scheme->cltl f env)))
    `(do ,bind ,test ,@body)))


(defun dolist->dolist (form &optional env)
  ;; augment environment and rewrite
  ;; (do (a b c) ...)
  (let* ((bind (second form))
         (vars (cons (first bind) env))
         (body (cddr form)))
    `(, (first form)      ; dolist or dotimes
        ,(cons (car bind)
               (loop for f in (cdr bind)
                     collect
                     (scheme->cltl f env)))
        ,@(loop for f in body
                collect (scheme->cltl f vars)))))

(defun set!->setf (form &optional env)
  `(setf ,(cadr form) ,(scheme->cltl (caddr form) env)))

(defun dynamic-wind->unwind-protect (form &optional env)
  ;; (dynamic-wind fn fn fn)
  (let ((lam1 (second form))
        (lam2 (third form))
        (lam3 (fourth form)))
    ;; all my dymanic winds have a dummy init thunk
    (unless (eql (third lam1) nil)
      (error "fixme: expected null body in init thunk."))
    `(unwind-protect 
       (progn
         ,@(loop for f in (cddr lam2)
                 collect (scheme->cltl f env)))
       ,@(loop for f in (cddr lam3)
               collect (scheme->cltl f env)))))

;;;
;;; BACKQUOTE 
;;; uhoh backquote rewriting...
;;; 

(defun backquote-form? (form)
  (eql (car form) %bquote))

(defclass bquote ()
  ((form :initarg :form 
         :accessor bquote-form)))

(defmethod print-object ((obj bquote) stream)
  ;; pretty print expr
  (format stream "`~W" (bquote-form obj)))

(defclass bcomma (bquote) ())

(defmethod print-object ((obj bcomma) stream)
  (format stream ",~W" (bquote-form obj)))

(defclass bsplice (bcomma) ())

(defmethod print-object ((obj bsplice) stream)
  (format stream ",@~W" (bquote-form obj)))

(defun rewrite-backquote (form &optional env)
  ;; form is an "expanded" backquote expression in which
  ;; all special macro characters have been expanded:
  ;; `(foo )  =>  (#:bquote (foo))
  ;; ,foo     =>  (#:bcomma foo)
  ;; ,@foo    =>  (#:bsplice foo)
  (let ((expr (CADR form))
        (subs '())
        (done '()))
    (unless (consp expr)
      (error "Backquote not list (fix me): ~S" 
             expr))
    (labels ((divein (form)
               ;; search bqoute expansion and rewrite all
               ;; the backquote units with with unique,
               ;; gensym'ed tokens. then rewrite that expr.
               (cond ((null form)
                      nil)
                     ((and (consp form)
                           (or (eql (first form) %bcomma)
                               (eql (first form) %bsplice)
                               (eql (first form) %bquote)))
                      (let ((id (gensym)))
                        (push (list id form) subs)
                        ;; keep rewriting subexpressions
                        (SETF (SECOND FORM)
                              (DIVEIN (SECOND FORM)))
                        id))
                     ((consp form)
                      (cons (divein (car form))
                            (divein (cdr form))))
                     (t form)))
             (diveout (form)
               ;; replace all gensymed tokens
               ;; with expansion objects
               (cond ((null form) nil)
                     ((eq form %false) nil)
                     ((eq form %true) t)
                     ((consp form)
                      (cons (diveout (car form))
                            (diveout (cdr form))))
                     (t
                      (let ((entry (assoc form subs)))
                        ;; entry is:
                        ;; (#:token (#:BCOMMA EXPR))
                        ;; (#:token (#:BSPLICE EXPR))
                        ;; (#:token (#:BQUOTE EXPR))
                        (if entry
                          (let ((type (first (second entry)))
                                (expr (DIVEOUT (second (second entry)))))
                            (cond ((eql type %bcomma)
                                   (make-instance 'bcomma :form expr))
                                  ((eql type %bsplice)
                                   (make-instance 'bsplice :form expr))
                                  ((eql type %bquote)
                                   (make-instance 'bquote :form expr))
                                  (t
                                   (error "Shouldn't happen!"))))
                          form))))))

      ;; divein replaces comma, splice and recursice backquote
      ;; forms with tokens so the expression can be rewritten
      (setf done (scheme->cltl (divein expr)))

      ;; divein created a rewite list as a side effect
      (dolist (e subs)
        ;; rewrite each of subexpress. each entry is:
        ;; (#:token (#:BCOMMA EXPR))
        ;; (#:token (#:BSPLICE EXPR))
        ;; (#:token (#:BQUOTE EXPR))
        (setf (second (second e))
              (scheme->cltl (second (second e)) env)))
      ;; return a bqoute object whose print-object method
      ;; will pretty-print a backqoute expressoin to the file.
      (make-instance 'bquote :form (diveout done))
      )))

#|
;;; BACKQUOTE TESTS.
(defun bq (s)
  (let ((*readtable* %readtable))
    (let ((l (read-from-string s)))
     ; (pprint l)
      l)))
(setf *print-pretty* T)

(setf s "`(list 1 ,pi 3 ,@ (set! 1 , 2))")

(pprint (setf x (bq s)))

(rewrite-backquote x)

(setf s "`(, 'let ,(loop-bindings iter)
             ,@(loop-initially iter)
             (block nil
               (tagbody 
                 :loop
                 ,@ (let ((tests (loop-end-tests iter)))
                      (if tests
                        (list `(if ,(if (cdr tests)
                                      (cons 'or tests)
                                      (car tests))
                                 (go :done)))
                        (list)))
                    ,@(loop-looping iter)
                    ,@(loop-stepping iter)
                    (go :loop)
                    :done
                    ,@(loop-finally iter)
                    ,return)))")
|#

;;;
;;;
;;;

#|
(defun boolean?->typep (form &optional env)
  ;; form = (boolean? x)
  `(typep ,(scheme->cltl (cadr form) env) 'boolean))
|#

(defun inexact->round (form &optional env)
  ;; check for inexact->exact wrapping round or floor
  (let ((check (second form)))
    (if (consp check)
      (if (member (car check) '(floor round ceiling))
        (scheme->cltl check env)
        `(round ,(scheme->cltl check env)))
      `(round ,(scheme->cltl check env)))))

;;;
;;; LISTS AND SEQUENCES
;;; 

(defun list-tail->nthcdr (form &optional env)
  ;; reverse args for nthcdr
  `(nthcdr ,(scheme->cltl (caddr form) env)
           ,(scheme->cltl (cadr form) env)))

(defun make-list->make-list (form &optional env)
  ;; (make-list n . c)
  (if (cddr form)
    `(make-list ,(scheme->cltl (cadr form) env)
                  :initial-element
                  ,(scheme->cltl (caddr form) env))
    `(make-list ,(scheme->cltl (cadr form) env))))

(defun element-set!->setf-elt (form &optional env)
  ;; used by list-set!, vector-set! and string-set!
  `(setf (elt ,(scheme->cltl (cadr form) env)
              ,(scheme->cltl (caddr form) env))
         ,(scheme->cltl (cadddr form) env)))

(defun memq->member (form &optional env)
  `(member ,(scheme->cltl (second form) env)
           ,(scheme->cltl (third form) env)
           :test (function eq)))

(defun map->mapcar (form &optional env)
  ;; map. this
  `(mapcar ,(scheme->cltl (second form) env)
           ,(scheme->cltl (third form) env)))

(defun for-each->map (form &optional env)
  `(map nil ,(scheme->cltl (second form) env)
        ,(scheme->cltl (third form) env)))

(defun make-vector->make-array (form &optional env)
  (if (third form)
    `(make-array ,(scheme->cltl (second form) env)
                 :initial-element
                 ,(scheme->cltl (third form) env))
    `(make-array ,(scheme->cltl (second form) env))))

;;; 
;;; '() pretty printing. the code rewriter assumes 'nil
;;; should be printed as '() in the translation.
;;;

(defclass empty-list () 
  ((quoted :initarg :quoted :initform t )))

(defmethod print-object ((obj empty-list) stream)
  (if (slot-value obj 'quoted)
    (format stream "'()")
    (format stream "()")))

;;;
;;; STRINGS
;;;

(defun list->coerce-string (form &optional env)
  `(coerce ,(scheme->cltl (second form) env) 'string))

(defun string->coerce (form &optional env)
  ;; (string ...) => (coerce (list ...) 'string)
  `(coerce (list ,@ (loop for f in (cdr form)
                          collect
                          (scheme->cltl f env)))
           'string))

(defun make-string->make-string (form &optional env)
  ;; (make-string n . c)
  (if (cddr form)
    `(make-string ,(scheme->cltl (cadr form) env)
                  :initial-element
                  ,(scheme->cltl (caddr form) env))
    `(make-string ,(scheme->cltl (cadr form) env))))

(defun symbol?->symbolp (form &optional env)
  ;; symbol? doesn't allow NIL as symbol.
  (let ((form (scheme->cltl (cadr form) env)))
    (if (not (consp form))
      `(and ,form (symbolp ,form))
      (let ((v (gentemp)))
        `(let ((,v ,form))
           (and ,v (symbolp ,v)))))))

(defun string-append->concat (form &optional env)
  `(concatenate 'string
                ,@(loop for f in (cdr form)
                        collect (scheme->cltl f env))))

(defun string->intern (form  &optional env)
  ;; (string->symbol x)
  `(intern ,(scheme->cltl (second form) env) :cm))

;;;
;;; input/output
;;;

(defun write->write (form &optional env)
  ; (write x port)
  (let ((port (third form)))
    (if port
      `(write ,(scheme->cltl (second form) env)
              :stream ,(scheme->cltl port env))
      `(write ,(scheme->cltl (second form) env)))))

;;;
;;; HASH TABLES
;;; 

(defun mht->mht (form &optional env)
  ;; make an hash table using equal test.
  `(make-hash-table :size ,(scheme->cltl (cadr form) env)
                    :test (function equal)))

(defun hash-set!->setf-gethash (form &optional env)
  ;; (hash-set! table index value) 
  (let ((table (scheme->cltl (second form) env))
        (index (scheme->cltl (third form) env))
        (value (scheme->cltl (fourth form) env)))
    `(setf (gethash ,index ,table) ,value)))

(defun hash-ref->gethash (form  &optional env)
  `(gethash ,(scheme->cltl (third form) env)
            ,(scheme->cltl (second form) env)))

;(hash-remove! *dictionary* (object-name obj))
;(hash-fold )

(defun hash-remove->remhash (form &optional env)
  ;; (hash-remove tbl index) 
  (let ((table (second form))
        (index (third form)))
    `(remhash ,(scheme->cltl index env)
              ,(scheme->cltl table env))))

;;;
;;; GOOPS. the only real issues with goops->clos are the
;;; use of <class> variables in goops and metaclasses.

(defun slot-set!->setf-slot-value (form &optional env)
  `(setf (slot-value ,(scheme->cltl (second form) env) 
                     ,(scheme->cltl (third form) env))
         ,(scheme->cltl (fourth form) env)))
         
(defun class-var->class-name (form)
  ;; goops uses class variables but clos uses class names.
  ;; this code converts based on the assumption that the class
  ;; name is the same as the varibale name without <>.
  ;;  <foo> ->  foo hackery
  (if (symbolp form)
    (let ((s (string form)))
      (if (char= #\< (elt s 0))
        (let ((len (length s)))
          (if (char= #\> (elt s (- len 1)))
            (intern (subseq s 1 (- len 1)))
            (error "~a class variable not <~a>"
                   form form)))
        (error "~a class variable not <~a>"
               form form)))
    (error "~a class variable not symbol." form)))

(defparameter class-map '((top t)
                          (class standard-class)
                          (object standard-object)
                          (pair cons)
                          (procedure function)))
  
(defun clos-name (var)
  (let ((sym (class-var->class-name var)))
    (let ((e (assoc sym class-map)))
      (if e (cadr e) sym))))

;;;
;;; used to print #+ and #- forms
;;;

(defclass feature () 
  ((marker :initarg :marker)))

(defmethod print-object ((obj feature) stream)
  (format stream "#~(~a~) " (slot-value obj 'marker)))

(defun define-class->defclass (form &optional env)
  (let ((varname (second form))
        (supvars (third form))
        (body (cdddr form))
        name sups
        (slots '())
        (options '())
        (metaoptions '()))
    
    (loop while (consp (car body))
          do (push (pop body) slots))
    ;; slots now in reverse order
    (setf name (class-var->class-name varname))
    (setf sups (loop for v in supvars
                     for s = (class-var->class-name v)
                     for e = (assoc s class-map)
                     collect (if e (cadr e) s)))
    (loop with specs = '()
          for s in slots
          do
          (let ((spec (list (car s))))
            (loop for (p v) on (cdr s) by #'cddr
                  do
                  (case p
                    ((:init-thunk )
                     (setf p :initform)
                     (if (and (symbolp (third v))
                              (null (cdddr v)))
                       (setf v (third v))
                       (setf v `(funcall (function ,v)))))
                    ((:init-value )
                     (setf p :initform))
                    ((:allocation )
                     (setf p :allocation))
                    (:init-keyword
                     (setf p :initarg))
                    (:accessor )
                    (t (error "Not an initarg: ~S" p)))
                  (setf v (scheme->cltl v env))
                  (push p spec)
                  (push v spec))
            (push (nreverse spec) specs))
          ;; slots now in original order
          finally (setf slots specs))
    
    ;; slots now in correct order
    ;; CM:  find any :metaclass, :definer or :handles inits
    ;; and rewrite these.
    (loop for (p v) on body by #'cddr do
          (case p
            (:name )
            (:metaclass
             (push (list :metaclass
                         (class-var->class-name v)) 
                   options)
             (push (make-instance 'feature :marker '+metaclasses)
                   options))
            (:definer
              (push `(setf (io-class-definer ,varname )
                           ,v)
                    metaoptions))
            (:file-types
             (push `(setf (io-class-file-types ,varname )
                          ,v)
                   metaoptions))
            (:mime-type
             (push `(setf (io-class-mime-type ,varname )
                          ,v)
                   metaoptions))
            (t 
             (error "Not an defclass option: ~S" p))))
    
    `(progn
       (defclass ,name ,sups
         ,slots
         ,@options)
       (defparameter ,varname (find-class (quote ,name)))
       (finalize-class ,varname) ; goddam acl!
       ,@metaoptions
       (values))))

;(defmethod initialize :after ((obj container) args))
;(stocl (srcfile "temp.scm")

(defun define-method->defmethod (form &optional env)
  (let* ((decl (copy-tree (second form)))
         (name (first decl))
         (temp (cdr decl)) ; parameters
         (type '()) 
         (body (cddr form))
         cltl qual)
    ;; make method parameters look like regular define
    (loop while (consp temp)
          do (if (consp (car temp))
               (progn
                 (push (car temp) type)
                 (setf (car temp) (caar temp))))
          (setf temp (cdr temp)))
    ;; now rewrite fake define into defun
    (setf cltl (lambda->lambda `(lambda ,(cdr decl) ,@body)
                               env name))
    (setf (first cltl) 'defmethod)
    (setf decl (third cltl))
    (setf body (cdddr cltl))

    ;; now rewrite typed parameters
    (dolist (p type)
      (setf (second p) (clos-name (second p))))

    ;; now destructively substitute typed parameters
    ;; into rewritten lambda parameters
    (loop for tail on decl
          until (member (car tail) lambda-list-keywords)
          do (let ((e (assoc (car tail) type)))
               ;(print (list :e e))
               (if e (setf (car tail) e))))
    
    ;; see if call-next-method is the first or last form.
    ;; if it is then make the method :after or :before
    ;; the check has to skip over variable references
    ;; that are used to gag cltl compilers
    (let ((tail body)
          (junk '()))
      (loop while tail
            until (consp (car tail))
            do (setf junk (cons (car tail) junk)
                     tail (cdr tail))
            finally 
            (if (equal (car tail) '(call-next-method))
              (progn
                (setf qual (list :after))
                (setf body (append (nreverse junk)
                                   (cdr tail))))
              (if (equal (car (last body))
                         '(call-next-method))
                (progn
                  (setf qual (list :before))
                  (setf body (butlast body)))))))
                      

;    (if (equal (first body) '(call-next-method))
;      (progn (setf qual (list :after))
;             (pop body))
;      (if (eql (car (last body)) '(call-next-method))
;        (progn (setf qual (list :before))
;               (setf body (butlast body)))))


    (setf cltl `(defmethod ,name ,@qual ,decl ,@body))

    ;; this should be generalized...
    (if (eql (second cltl) 'initialize)
      (define-initialize->defmethod-initialize cltl)
      cltl)))

(defun define-initialize->defmethod-initialize (form &optional env)
  ;; inits passed to initialze must be
  ;; (defmethod initialize [:after] ((x class) inits) ...)
  (declare (ignore env))
  (let* ((args (loop for x in form when (consp x) do (return x)))
         (var (first (last args))))
    ;; get the &rest initarg and add it as the
    ;; first form in the body. this will stop
    ;; compilers from complaining about it if
    ;; it isnt referenced in the method.
    (let ((body (member args form)))
      (setf (cdr body) (cons var (cdr body))))
    (setf (cdr args) (cons '&rest (cdr args)))
    (setf (second form) 'initialize-instance)
    form))

;;;
;;; CM rewrites
;;;

(defun cdr-pop->pop-cdr (form &optional env)
  `(pop (cdr ,(scheme->cltl (cadr form) env))))

(defun list-delete-if->delete-if (form &optional env)
  (let ((pred (cadr form))
        (from (caddr form))
        (key (if (cdddr form) (cadddr form) nil)))
    `(delete-if ,(scheme->cltl pred env)
                ,(scheme->cltl from env)
                ,@(if key
                    (list ':key (scheme->cltl key env))
                    (list)))))

;;;
;;; since these macros appear in sources they
;;; also need to be walked...

(defun defobject->defobject (form &optional env)
  ;; this stops defobjects from being translated.
  ;; and insures that null classes or slots are
  ;; printed as empty lists rather than nil.
  (declare (ignore env) )
  (when (null (third form))
    (setf (third form)
          (make-instance 'empty-list :quoted nil)))
  (when (null (fourth form))
    (setf (fourth form)
          (make-instance 'empty-list :quoted nil)))
  form)

(defun dopairs->dopairs (form &optional env)
  ;; (dopairs (v1 v2 list . val) . forms)
  (let* ((args (second form))
         (aug (append env (list (first args) (second args)))))
    `(dopairs (,(first args) 
               ,(second args)
               ,(scheme->cltl (third args) env)
               ,@(if (cdddr args)
                   (list (scheme->cltl (fourth args) aug))
                   '()))
       ,@(loop for f in (cddr form)
               collect (scheme->cltl f aug)))))


