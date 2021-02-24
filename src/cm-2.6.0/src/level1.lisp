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
;;; $Revision: 1.16 $
;;; $Date: 2005/01/03 17:24:33 $

(in-package :cm)

;;;
;;; cm-directory is parent directory of src/
;;;

(defparameter cm-directory
  (namestring (make-pathname 
               :directory 
               (butlast
                (pathname-directory *load-pathname*)))))

;;;
;;; level1 provodes support routines that are common to
;;; all cltl2 implementations. after this file is loaded
;;; the scheme and cltlt implementatoin should be "equal"
;;;

;;; these are provided in scheme and cltl so that source
;;; examples can be "dialect independant".

(defvar true t)
(defvar false nil)

;;;
;;; a few scheme functions i require in the sources
;;;

(defun get-current-time ()
  (multiple-value-bind (sec min hour day mo year)
                       (get-decoded-time)
    (vector sec min hour day (- mo 1) year)))

(defun filename->url (file)
  (let* ((norm (truename file))
	 (host (pathname-host norm))
	 (dirs (pathname-directory norm))
	 (name (pathname-name norm))
	 (type (pathname-type norm)))
    (unless (eql (first dirs) :absolute)
      (error "Filename ~a not absolute." file))
    (format nil "file://~a~{/~a~}/~a.~a"
	    (if (stringp host) host "")
	    (cdr dirs) name type)))

;;;
;;; functionality i had to add to scheme that is not exactly
;;; defined in cltl either...
;;;

(defun log2 (n) (log n 2))

(defun log10 (n) (log n 10))

(defmacro define-list-struct (name &body slots)
  (let ((setters
         (loop for x in slots
               for s = (if (consp x) (car x) x)
               collect
               `(defun ,(intern (concatenate 'string 
                                             (string name)
                                             "-"
                                             (string s)
                                             "-"
                                             ;; case sensitivity
                                             (symbol-name 'set!)))
                       (,name value)
                  (setf (,(intern (concatenate 'string 
                                               (string name)
                                               "-"
                                               (string s)))
                         ,name)
                        value)))))
    `(progn
       (defstruct (,name (:type list))
         ,@slots)
       ,@setters)))

;;;
;;; readtable hackery
;;;

(defvar *cm-readtable* (copy-readtable))

(defun read-macro-set! (char func)
  (set-dispatch-macro-character 
   #\# char #'(lambda (stream a b) 
                (declare (ignore a b))
                (funcall func (read stream)))
   *cm-readtable*))

(read-macro-set! #\! (lambda (form) `(find-object ',form t)))

;;;
;;; hash-table
;;;

(defun hash-fold (func prev table)
  (maphash #'(lambda (key val)
               (setf prev (funcall func key val prev)))
           table)
  prev)

;;;
;;; symbols and keywords
;;;

(defun keyword? (x) (typep x 'keyword))

(defun symbol->keyword (sym)
  (let ((str (symbol-name sym)))
    (or (find-symbol str ':keyword)
        (intern str :keyword))))

(defun keyword->symbol (keyword)
  (let ((name (symbol-name keyword)))
    (or (find-symbol name)
        (intern name))))

;;; strings

(defun strip-chars (str &optional (chars '(#\space #\tab #\return)))
  (string-trim chars str))

(defun string-read (str &optional (start 0) (eof ':eof))
  (read-from-string str nil eof :start start))

;;;
;;; filename twiddling. these always return strings.
;;;

(defun filename (file) (namestring file))

(defun filename-directory (file)
  (let ((dir (pathname-directory file)))
    (if dir
      (namestring (make-pathname :directory dir))
      nil)))

(defun filename-name (file) (pathname-name file))

(defun filename-type (file) (pathname-type file))

(defun merge-filenames (file1 file2) 
  (namestring (merge-pathnames file1 file2)))

;;;
;;; file opening and closing.
;;;

(defun open-file (file direction &optional (type :char))
  (let ((etyp (ecase type
                ((:byte :byte8 ) '(unsigned-byte 8))
                ((:byte32 ) '(unsigned-byte 32))
                ((:char ) 'character))))
    (if (eq direction :output)
      (open file :direction :output
            :if-does-not-exist :create
            :if-exists :supersede
            :element-type etyp)
      (open file :direction :input
            :element-type etyp))))

(defun close-file (fp dir)
  (declare (ignore dir))
  (close fp))

(defvar .eofmarker. (gensym))

(defun file-eof? (x) (eq x .eofmarker.))

(defun file-form (fil)
  (read fil nil .eofmarker.))

(defun file-line (fil)
  (read-line fil nil .eofmarker.))

;(defun open-output-file (file)
;  (open file :direction :output
;        :if-does-not-exist :create
;        :if-exists :supersed))
;
;(defun open-input-file (file)
;  (open file :direction :input))
;
;(defmacro with-open-output-file ((var file) &body body)
;  `(with-open-file (,var ,file :direction :output
;                         :if-does-not-exist :create
;                         :if-exists :supersed)
;     ,@body))
;
;(defmacro with-open-input-file ((var file) &body body)
;  `(with-open-file (,var ,file :direction :input)
;     ,@body))

;;;
;;; defobject expansion for cltl
;;;

(defun expand-defobject (name gvar supers slots pars methods)
  `(progn
     (defclass ,name ,supers 
       ,(loop for x in slots
              when (consp x)
              collect
              (let* ((slot (first x))
                     (inits (list slot))
                     (keyword (symbol->keyword slot))
                     (key? nil)
                     (acc? ':default))
                (loop for (key val) on (cdr x) by #'cddr
                      do 
                      (cond ((eq key ':initarg) 
                             ;; check for :initarg nil
                             (if (or (eq val keyword)
                                     (not val))
                               (setf key? t)
                               ;; push user's initarg
                               (progn ;(setf key? t)
                                      (push ':initarg inits)
                                      (push val inits))))
                            ((eq key ':accessor)
                             (setf acc? val))
                            (t (push key inits)
                               (push val inits))))
                (unless key?
                  (push ':initarg inits)
                  (push keyword inits))
                (when acc?
                  (if (eql acc? ':default)
                    (setf acc? (intern (format nil "~a-~a" name slot))))
                  (push ':accessor inits)
                  (push acc? inits)) 
                (nreverse inits))
              else 
              collect 
              (list x ':initarg (symbol->keyword x)
                    ':accessor (intern (format nil "~a-~a" name x))))
       ,@(if (and pars (find ':metaclasses *features*))
           (list '(:metaclass parameterized-class))
           '()))
     
     ;; define a global variable for the class object
     (defparameter ,gvar (find-class ',name))

     ;; sigh. some CLOS make me do this.
     (finalize-class ,gvar)

     ;; define a load-form method
     ,(make-load-form-method name gvar)

     ;; define a #i print-object method
     (defmethod print-object ((obj ,name) port)
       (if *print-instance*
         (print-instance obj port)
         (call-next-method)))

     ;; set class parameters if apropriate.
     ,@(if pars (list `(setf (class-parameters ,gvar)
                             (quote ,pars))))

     ;; splice in any output methods.
     ,@methods

     ;; expansion returns no values.
     (values)))

;;;
;;; cltl expansion for make-load-form
;;;

(defun make-load-form-method (classname classvar)
  `(defmethod make-load-form ((obj ,classname))
     (list* 'make-instance ',classvar
            (slot-init-forms obj :eval t))))

;;;
;;; cltl expansion for write-event
;;;

(defun define-output-method (objclassname objclassvar objvar
                                          fileclassname fileclassvar
                                          filevar timevar body)
  (declare (ignore objclassvar fileclassvar))
  `(defmethod write-event ((,objvar ,objclassname)
                           (,filevar ,fileclassname)
                           ,timevar)
     ,@body))

;;;
;;; CLTL expansion for process macro
;;;

(defun process-stop (expr)
  ;; process not rescheuled if it returns false
  (declare (ignore expr))
  ; '(return-from :process ':stop)
  '(return-from :process nil)
  )

(defun expand-process (forms ops)
  (let ((parsed (parse-iteration 'process forms ops))
        (code '())
        (func nil)
        (tests '())
        (done nil))
    
    (setf tests (loop-end-tests parsed))
    (setf done (process-stop nil))
    (if (not (null tests))
      (progn
        (if (null (cdr tests))
          (setf tests (car tests))
          (setf tests (cons 'or tests)))
        (setf tests `((if ,tests ,done))))
      (unless (process-code-terminates? (loop-looping parsed)
                                        (process-stop nil))
        ;(or (member 'while (loop-operator parsed))
        ;          (member 'until (loop-operator parsed)))
        (warn
         "A non-terminating process may have been defined.~%~
          You can use REPEAT, WHILE or UNTIL to limit iteration.")))
    (setf code
          `(block :process
             ,@ tests
                ,@ (loop-looping parsed)
                   ,@ (loop-stepping parsed)
                      ;; (enqueue *process* *qnext* *qstart*)
                      t 
                      ))
    ;; if there is a finally clause wrap the block
    ;; in a test for :STOP. when true do the 
    ;; finally actions.
    (when (loop-finally parsed)
      ;(setf code
      ;      `(when (eq ':stop ,code)
      ;         ,@ (loop-finally parsed)))
      (setf code `(if (not ,code)
                    (progn ,@ (loop-finally parsed) nil)
                    t)))
    (setf func `(function (lambda () ,code)))
    (if (and (null (loop-bindings parsed))
             (null (loop-initially parsed)))
      func
      ;; use let* sequential binding
      `(let* ,(loop-bindings parsed)
         ,@(loop-initially parsed)
         ,func))))

(defun expand-defprocess (forms)
  `(defun ,(first forms) ,(second forms) ,@(cddr forms)))

;;;
;;; cltl expansion for define-midi-message-set! 
;;;

(defun make-midi-message-set! (getter bytespec)
  (let ((setter (intern
                 (concatenate 'string
                              (string getter)
                              (string '-set!)))))
    `(defmacro ,setter (message value)
       (if (symbolp message)
         (let ((val (gensym)))
           `(let ((,val ,value )) ;
              (setf ,message (dpb ,val ,',bytespec ,message))
              ,val))
         `(dpb ,value ,',bytespec ,message)))))


(defun set-file-position (file pos set?)
  (if (= pos 0)
    (file-position file)
    (if set?
      (file-position file pos)
      (file-position file 
                     (+ (file-position file) pos)))))

;;;
;;;
;;;

(defun cm (&optional (banner t))
  (unless (and (eql *package* (find-package :cm))
               (eql *readtable* *cm-readtable*))
    (in-package :cm)
    (setq *readtable* *cm-readtable*)
    (unless (not banner) (cm-logo))
    (values)))

; (pathname-directory "/Lisp/cm/bin/../bin/clisp_2.32_darwin-powerpc/cm.img")

(defun safe-load (fil)
  ;; load init file, catch all errors.
  (with-simple-restart (safe-load "Error loading init file: ~S" fil)
    (handler-bind ((t #'(lambda (c)
                          c
                          (invoke-restart 'safe-load))))
      (load fil :verbose nil))))

(defun load-cminit ( &optional from)
  ;; 1. Load site-wide cminit.lisp from these runtime locations:
  ;;    {IMAGE_DIR}/cminit.lisp
  ;;    {IMAGE_DIR}/../../etc/cminit.lisp
  ;;    {CM_ROOT_DIR}/etc/cminit.lisp
  ;; 2. Then load user's ~/.cminit

  (let ((dir (or from (cm-image-dir)))
        (loa nil))
    ;; check relative to image directory.
    (when dir
      (let ((fil (merge-pathnames "cminit.lisp" dir)))
        (if (probe-file fil)
          (progn (setq loa t)
                 (safe-load fil))
          ;; try ../../etc
          (unless from
            (let ((etc (append (butlast (butlast (pathname-directory dir)))
                             (list "etc"))))
            ;; check if still valid dir
            (when (member (car etc) '(:absolute :relative))
              (setf fil (make-pathname :defaults fil
                                       :directory etc))
              (when (probe-file fil)
                (setq loa t)
                (safe-load fil))))))))
    (unless (or loa from)
      ;; else check cm.sh var
      (setq dir (env-var "CM_ROOT_DIR"))
      (when (and dir (not (equal dir "")))
        ;; shell var from cm.sh has no trailing delim
        (setq dir (format nil "~A~C" dir directory-delimiter))
        (let ((fil (merge-pathnames "cminit.lisp" dir)))
          (when (probe-file fil)
            (setq loa t)
            (safe-load fil)))))

    ;; load user's .cminit file
    (let ((fil (make-pathname :name ".cminit" :type "lisp"
                              :defaults (user-homedir-pathname))))
      (when (probe-file fil)
        (setq loa t)
        (safe-load fil)))
    loa))

;;;
;;; CLOAD
;;;

(defparameter cload-types '("ins" "lisp"))

(defun cload (file &key (verbose t) (types cload-types))
  (let* ((src (or (loop for e in types
                     for p = (merge-pathnames file (make-pathname :type e))
                     when (probe-file p)
                     return p)
                  (error "Source file ~S does not exist." file)))
         (bin (make-pathname :defaults src
                             :type (cl-user::syscmd :fasl))))
    (when (or (not (probe-file bin))
              (< (file-write-date bin)
                 (file-write-date src)))
      (when verbose (format t "; Compiling ~S." src))
      (compile-file src :verbose nil))
    (when verbose (format t "; Loading ~S." bin))
    (load bin :verbose nil)))


;;; temporary hack until i can figure out what to do 
;;; for lisps without callbacks

#-openmcl
(defmacro defcallback (name args &body body)
  args body
  `(defvar ,name "defcallback not implemented"))