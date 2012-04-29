;;; **********************************************************************
;;; This is the make file for Common Music. To build the system from its
;;; sources, first make sure you are in the :cl-user package, then load 
;;; this file and call the make-cm function:
;;; make-cm (&key (save-image t) (delete-fasls t) (bin-directory t)
;;;               (scheme t) (prompt nil) (verbose t) (force nil))
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.59 $
;;; $Date: 2005/02/20 17:24:57 $

(in-package :cl-user)

#-(or mcl openmcl excl clisp cmu sbcl)
(error "~%Attempt to build Common Music in an unknown Lisp.~%Supported Lisp implementations: ACL, CLISP, CMUCL, Guile, MCL, OpenMCL and SBCL.")

#+:cmu  (declaim (optimize (extensions:inhibit-warnings 3)))
#+:sbcl (declaim (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))

(defparameter this-file
  (truename *load-pathname*))

(defparameter os-directory-delim
  (let ((str (namestring (make-pathname :defaults this-file
					:name nil :type nil))))
    (elt str (- (length str) 1))))

(defun reldir (path &rest subs)
  ;; returns a directory relative to path. subs is an optional
  ;; sequence of subdirs. the first element can be a number
  ;; of parent dirs to move up to before appending any sub dirs.
  (let ((dirs (pathname-directory path)))
    (when (numberp (car subs))
      (if (zerop (car subs))
        (pop subs)
        (let ((num (abs (pop subs))))
          (setf dirs (reverse dirs))
          (dotimes (i num) (pop dirs))
          (setf dirs (nreverse dirs)))))
    (namestring
     (make-pathname :defaults path :name nil :type nil
                    :directory (append dirs subs)))))

(defun is-directory? (dir)
  ;; return namestring if dir is a valid directory string
  ;; WITHOUT triggering an error if it isn't. unfortunately
  ;; there is no standard way to do this in cltl.
  (setq dir (tidydir dir))  ; clean up dirstring
  (if (null dir)
    nil
    (multiple-value-bind (p n) (parse-namestring dir nil #P""
                                                 :junk-allowed t)
      (if (= n (length dir))  ; no junk
        (let* ((d (pathname-directory p))
               (x (and (consp d)
                       (eql (first d) :absolute)  ; full path
                       (not (pathname-name p))    ; no name.ext
                       (not (pathname-type p))
                       ;; clisp directory nonsense...
                       ;; on win32 I cant use probe-file OR
                       ;; probe-directory. so we test the
                       ;; last subdir as a file....
                       #+(and clisp win32)
                       (let* ((n (namestring p))
                              (s (subseq n 0 (- (length n) 1))))
                         (and (probe-file s) p))
                       ;; more clisp directory nonsense...
                       ;; have to call probe-directory on linux.
                       #+(and clisp (or unix linux))
                       (and (probe-directory p) p)
                       #-clisp
                       (probe-file p))))
          (if x (namestring x) nil))))))

(defun tidydir (dir)
  ;; fix up some possible problems in a directory string that the
  ;; user specified at a prompt.
  ;; Check for user providing surrounding "" at prompt...
  (setq dir (string-trim '(#\") dir))
  ;; Check to see if user provided \\ or used unix style
  ;; pathnames in Windows directory.
  #+(or win32 microsoft-32)
  (cond ((search "\\\\" dir)
         (do ((c #\a) (x nil) (l '()) (i 0 (+ i 1)) (e (length dir)))
             ((= i e) 
              (setq dir (coerce (nreverse l) 'string)))
           (setq x c)
           (setq c (elt dir i))
           (unless (and (char= c #\\) (char= x #\\))
             (push c l))))
        ((and (find #\/ dir)
              (not (find #\\ dir)))
         ;; rewrite / as \
         (setq dir (substitute #\\ #\/ dir))))
  ;; Check for null string...
  (if (string= dir "")
    nil
    (progn
      ;; User may not have provided directory char at end...
      (unless (char= (elt dir (- (length dir) 1)) 
                     os-directory-delim)
        (setq dir (format nil "~a~c" dir os-directory-delim)))
      dir)))

;;;
;;; make-cm
;;;

(defvar source-dir )
(defvar binary-dir )
(defvar .fasl-name )
(defvar writable-p nil)
(defvar compiled-p nil)
(defvar silently-p nil)
(defvar *cm-root* nil)
(defvar midishare-installed? nil)

(defun make-cm (&key (bin-directory t rdp)
		(scheme t )
		(save-image nil) ; t, nil, :if-no-compile
		(delete-fasls t)
		(force nil)
		(verbose t)
		(extensions nil)
		(midishare t)
		&aux 
		(usr-directory nil)
		(src-directory (reldir this-file ))
		;; SAR Sun Apr 29 12:49:55 BST 2012: commented out due to CCL
		;; compatibility 
		;; (build-plotter nil)
		)

  (if (find-package ':cm)
      (if (not force)
	  (return-from make-cm nil)))

  (when extensions
    (unless (consp extensions)
      (setq extensions (list extensions)))
    ;; process pre-extensions and collect
    ;; any remaining post-extensions
    (when (eql (car extensions) ':before)
      (setq extensions (do-extensions (cdr extensions) ':after))))

  ;; test user specified bin-directory.
  (if bin-directory
      (if (eql bin-directory t)
	  (setq bin-directory (reldir src-directory -1 "bin"))
	  (let ((dir (is-directory? bin-directory)))
	    (if dir
		(setq bin-directory dir) ; use filtered dir spec.
		(error ":bin-directory '~A' is not a directory."
		       bin-directory))
	    (setq usr-directory t)))
      ;; user explicitly says no bin-directory.
      (if rdp (setq writable-p ':no)))
  
  ;; *** BEGIN BUILD
  ;; insure features (including :loop) for each implementation

  (setq silently-p (not verbose))
  (insure-sys-features )
  (setq source-dir (reldir this-file))
  (setq *cm-root* (reldir this-file -1))
  (setq .fasl-name (syscmd :fasl))
  (setq binary-dir (if usr-directory
		       bin-directory
		       (fasl-directory bin-directory)))

  (tell-user "~%; Installation directory: ~S"
             (namestring *cm-root*))

  ;; not writable if user explicitly says no run dir
  ;; or no privledges in run dir.
  (setq writable-p (if (eql writable-p ':no) 
		       nil
		       (syscmd :fw? bin-directory)))
  (when writable-p
    ;; cache runtime dir in global
    ;; insure binary subdir if writable
    (unless (is-directory? binary-dir)
      (syscmd :mkdir binary-dir)))

  ;; a pox on chatty compilers
  ;;  #+:cmu  (proclaim '(optimize (extensions:inhibit-warnings 3)))
  ;;  #+:sbcl (proclaim '(sb-ext:muffle-conditions style-warning sb-ext:compiler-note))

  #| SAR Sun Apr 29 12:46:39 BST 2012: Commented out due to CCL incompatibility 
  ;; test to see if plotter will be built. issue warning if we are on
  ;; a plotform that could potentially run it but for some reason
  ;; cannot. if we can build then cload the GTK interface file before
  ;; the CM package is defined.
  (let ((flg (plotter-can-build?)))
  (if flg
  (if (stringp flg)
  (warn (format nil "Cannot build plotter because ~A." flg))
  (progn
  (setq build-plotter t)
  #+openmcl (cl "gui" "gtkffi-openmcl")
  #+(or cmu sbcl) (cl "gui" "gtkffi-cmusbcl")
  ))))
  |#

  ;; allow loading override
  (when (and midishare-installed?         ; nil, t or :ms (if no player)
             midishare)
    ;; check if already loaded
    (with-compilation-unit ()
      (unless (find ':midishare *features*)
        (cl "midishare" "MidiShare-Interface"))
      ;; :ms if player NOT installed.
      (unless (eql midishare-installed? ':ms)
        ;; check if already loaded
        (unless (find ':player *features*)
          (cl "midishare" "Player-Interface"))))
    )

  ;; If CLM, CMN or MIDISHARE are loaded use their
  ;; symbols, otherwise load their package stubs

  (unless (find :clm *features*)
    (cl "clm-stubs"))
  (unless (find :cmn *features*)
    (cl "cmn-stubs"))
  (unless (find :midishare *features*)
    (cl "midishare" "midishare-stubs"))
  ;;
  ;; level 0 loading
  ;;

  (with-compilation-unit ()
    (cl "pkg")
    ;; load implementation file
    #+clisp (cl "clisp")
    #+cmu (cl "cmu")
    #+sbcl (cl "sbcl")
    #+excl (cl "acl")
    #+(and mcl (not openmcl)) (cl "mcl")
    #+openmcl (cl "openmcl")
    ;;
    ;; level 1 loading
    ;;
    (cl "clos")
    (cl :stocl "loop" "iter")
    (cl "level1")
    (unless (not scheme) (cl "scheme"))
    (cl :stocl t "utils")
    (cl :stocl t "mop")
    (cl :stocl t "objects")
    (cl :stocl t "data")
    (cl :stocl t "scales")
    (cl :stocl t "spectral")
    (cl :stocl t "patterns")
    (cl :stocl t "io")
    (cl :stocl t "scheduler")
    (cl :stocl t "sco")
    (cl :stocl t "clm")
    (cl :stocl t "clm2")
    (cl :stocl t "midi1")
    (cl :stocl t "midi2")
    (cl :stocl t "midi3")
    (cl :stocl t "midishare" "midishare")
    (cl :stocl t "midishare" "player")
    (cl :stocl t "cmn")
    #| SAR Sun Apr 29 12:48:27 BST 2012: Commented out due to CCL incompatibility
    (when build-plotter
    (cl "gui" "plotter")
    (cl "gui" "support")
    (cl "gui" "widgets")
    (cl "gui" "editing")
    (cl "gui" "drawing")
    (cl "gui" "eventio"))
    |#
    )

  #+:sbcl
  (proclaim '(sb-ext:unmuffle-conditions style-warning sb-ext:compiler-note))

  (when extensions
    (do-extensions extensions))

  ;; only delete fasls if one pass
  (when (and writable-p 
             delete-fasls
             (or (eql save-image t)
                 (and (eql save-image ':if-no-compile)
                      (not compiled-p))))
    (tell-user "~%; Removing fasl files.")
    (let ((types (list .fasl-name)))
      ;; delete clisp's .lib turds too
      #+clisp (push "lib" types)
      (dolist (type types)
        (dolist (f (directory (make-pathname :name :wild :type type
                                             :defaults binary-dir)))
          (delete-file f)))))
  (tell-user "~%; Garbage collecting.")
  (syscmd :gc)

  ;; write a batch file startup on windows if an image is
  ;; being saved or if the script does not already exist.
  (when (and writable-p (find ':win32 *features*))
    (let ((bat (concatenate 'string (reldir *cm-root* "bin")
                            "cm.bat")))
      (when (or save-image (not (probe-file bat)))
        (tell-user "~%; Saving startup script ~s" bat)
        (write-windoze-script save-image))))

  (if (and writable-p 
           (or (eql save-image t)
               (and (eql save-image ':if-no-compile)
                    (not compiled-p))))
      (progn
	(tell-user "~%; Saving application image.~%; Bye!~%")
	(force-output)
	(let ((app (merge-pathnames (syscmd ':image) binary-dir)))
	  (cm-call :save-cm app)
	  (syscmd :bye)))
      (progn
	;; if not saving leave user in cm package with
	;; initfile loaded.
	(cm-call :load-cminit (reldir *cm-root* "etc"))
	(terpri)
	(force-output)
	))
  (values))

(defun cm ()
  (if (not (find-package :cm))
    (error "Attempt to call CM without CM loaded.")
    ;(cm-call :cm)
    (progn (syscmd :cm)
           (cm-call :cm-logo))))

(defun tell-user (&rest args)
  (unless silently-p
    (apply #'format t args)
    (force-output)))

(defun do-extensions (exts &optional stop)
  (let ((thing nil))
    (do ((l exts (cdr l)))
        ((or (null l) 
             (and stop (eql (car l) stop)))
         (cdr l))
      (setq thing (car l))
      (cond ((or (stringp thing) (pathnamep thing))
             (if (probe-file thing)
               (progn
                 (tell-user "; Loading ~A" (namestring thing))
                 (load thing :verbose nil))
               (warn "Extension file ~S does not exist." thing)))
            ((and (consp thing) (eql (car thing) 'lambda))
             (funcall (coerce thing 'function)))
            ((and (consp thing) (eql (car thing) 'function))
             (funcall (coerce (cadr thing) 'function)))
            ((or (functionp thing)
                 (and (symbolp thing) (symbol-function thing)))
             (funcall thing))
            (t (warn "Extension ~S is not a file or function."
                     thing))))))

(defun cl (&rest path &aux scm?)
  (when (eql (car path) ':stocl)
    (setq scm? (second path))
    (setq path (cddr path)))
  (let* ((subs (butlast path))
         (name (first (last path)))
         (srcf (make-pathname :defaults source-dir
                              :directory 
                              (append (pathname-directory source-dir)
                                      subs)
                              :name name :type "lisp"))
         (binf (make-pathname :defaults binary-dir
                              :name name :type .fasl-name))
         (scmf (if scm?
                 (make-pathname :defaults srcf
                                :name (if (eql scm? t) name scm?)
                                :type "scm")
                 nil)))
    ;; if there is no .lisp file, try to generate it from
    ;; scheme sources, else signal error. 
    (when (and scmf (make-file? srcf scmf))
      ;; only load stocl and check permissions
      ;; one time...
      (unless (boundp 'toplevel-translations)
        (load (merge-pathnames "stocl.lisp" source-dir)
              :verbose nil)
        (unless (syscmd :fw? source-dir)
          (error "Can't create ~A because ~A is not writable."
                 srcf source-dir)))
      (tell-user "~%; Generating ~S" 
                 (enough-namestring srcf *cm-root*))
      (funcall 'stocl scmf :file name :verbose nil))
    ;; now we have an up-to-date lisp source or error.
    (unless (probe-file srcf)
      (error "Cant find or generate Common Music source file ~A."
             srcf))
    (when (make-file? binf srcf)
      (if (not writable-p)
	  ;; if we cant write fasls then just load source
	  (setq binf srcf)
	(progn
	  (tell-user "~%; Compiling ~S" 
                     (enough-namestring srcf *cm-root*))
	  (compile-file srcf :output-file binf :verbose nil)
	  (setf compiled-p t))))
    (tell-user "~%; Loading ~S"
               (enough-namestring binf *cm-root*))
    (load binf :verbose nil)))

(defun make-file? (f1 f2)
  ;; return t if no f1 or earlier than f2
  (or (not (probe-file f1))
      (< (file-write-date f1) 
         (file-write-date f2))))

(defun fasl-directory (rundir)
  (format nil "~A~A_~A~C"
          rundir 
          (syscmd :lisp-version)
          (or (syscmd :os-arch)
              (error "FIXME: Unknown OS-MACHINE combination."))
          os-directory-delim))

(defun cm.sh (&optional args)
  ;; use this-file so we can test w/out calling make.
  (format nil "~acm.sh~@[ ~a~]"
          ;(reldir *cm-root* "bin")
          (reldir this-file -1 "bin")
          args))

(defun stream-line (stream)
  ;; read a line from shell stream
  (let ((res (read-line stream nil)))
    (if (or (null res) (equal res ""))
      nil
      res)))

;;; these two access cm vars and functions from other packages
;;; without having to know the case sensitivity of host lisp.

(defun cm-var (keyv)
  (symbol-value (find-symbol (string keyv) :cm)))

(defun cm-call (keyfn &rest args)
  (apply (find-symbol (string keyfn) :cm) args))

;;; get host/lisp features for bin directory name
;;; this requires maintenence!

;;(defvar uname-s nil) ; canonical os, let impls set how they want
;;(defvar uname-p nil) ; canonical processor

;; (defun os-name ()
;;   (cond ((not (null uname-s)) uname-s)
;;         ((or (find ':darwin *features*)
;;              (find ':macosx *features*)
;;              (find ':osx *features*))
;;          "darwin")
;;         ((or (find ':linux *features*)
;;              (find ':linux-target *features*) ; openmcl Linux PPC
;;              (find ':freebsd *features*)
;;              (find ':cygwin *features*))
;;          "linux")
;;         ((or (find ':win32 *features*)
;;              (find ':microsoft-32 *features*))
;;          "windows")
;;         ((find ':apple *features*)
;;          "macos")
;;         (t (error "FIXME: OS-NAME can't get host OS from *features*"))))

;; (defun machine-name ()
;;   (cond ((not (null uname-p)) uname-p)
;;         ((or (find ':pc386 *features*)
;;              (find ':x86 *features*)
;;              (find ':cygwin *features*))
;;          "i686")
;;         ((or (find :darwin *features*)
;;              (find :ppc *features*)
;;              (find :powerpc *features*)
;;              (find :osx *features*))
;;          "powerpc")
;;         ((find :apple *features*)
;;          "macos")
;;         (t (error "FIXME: MACHINE-NAME can't get host machine name from *features*."))))

(defun windoze-writable? (dir)
  ;; goddam windows. I can find no way to test the writability
  ;; of a directory without actually attempting to open a file
  (let ((flag nil)
        (name nil)
        (file nil))
    (with-simple-restart (stupid-windows-hack "Cant write to ~S" dir)
      (handler-bind ((t #'(lambda (c)
                            c
                            (invoke-restart 'stupid-windows-hack))))
        (setq name (merge-pathnames (string (gentemp "winturd")) dir))
        (setq file (open name :direction :output
                         :if-does-not-exist :create))
        (close file)
        (delete-file name)
        (setq flag t)))
    flag))

#| SAR Sun Apr 29 12:47:10 BST 2012: Commented out due to CCL incompatibility
(defun plotter-can-build? ()
  #+(and x86 linux sbcl) t
  #+(and x86 linux cmu) t
  #+(and darwin openmcl)
  (if (probe-file "/Applications/Utilities/X11.app")
    (if (directory "/sw/lib/libgtk-x11-2.0.*.dylib")
      t
      "GTK+2 is not installed (/sw/lib/libgtk-x11-2.0.*.dylib does not exist)")
    "X11 is not installed (/Applications/Utilities/X11.app does not exist)")
  #-(or (and x86 linux sbcl) 
        (and x86 linux cmu) 
        (and darwin openmcl))
  nil)
|#

(defun write-windoze-script (img?)
  (let ((cmd (syscmd :exec))
        (str (syscmd :batargs img?))
        (bat (concatenate 'string (reldir *cm-root* "bin")
                          "cm.bat"))
        (fil (if img?
               (concatenate 'string binary-dir "cm.img")
               (concatenate 'string (reldir *cm-root* "src")
                            "cm.lisp")))
        (*print-escape* nil))
    (with-open-file (out bat :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
      (write-line ":: Windows startup script for Common Music"
                  out)
      (write-line "@echo off" out)
      ;; add string around cmd if it has spaces.
      (if (find #\space cmd)
        (format out "\"~A\" " cmd)
        (format out "~A " cmd))
      (if img?
        (format out str fil)                
        (format out
                str
                (format nil
                        "(progn (load \"~A\" :verbose nil) (cm))" 
                        fil)))
      ;; pass any shell args on to the cmd.
      (write-line " %1 %2 %3 %4 %5 %6 %7 %8 %9" out))
    (values)))

;;;
;;; implementation specific code
;;;

#+clisp
(progn
  (defun insure-sys-features ()
    (setf (ext::package-lock "LISP") nil)
    (setf (ext::package-lock "CLOS") nil)
    (handler-bind ((t #'(lambda (c) c
                         (when (find-restart 'continue)
                           (invoke-restart 'continue)))))
      (load (merge-pathnames "loop.lisp" this-file)
            :verbose nil)))
  
  (defun syscmd (op &rest args)
    (ecase op
      (:lisp-version
       (let ((str (lisp-implementation-version)))
         (format nil "clisp_~a"
                 (subseq str 0 (position #\space str)))))
      (:os-arch
       #+win32 "windows-i686"
       #-win32
       (or (ext:getenv "CM_PLATFORM")
           (stream-line
            (run-shell-command (cm.sh "-q") :output :stream))))
      (:fasl "fas")
      (:image "cm.img")
      (:exec "clisp")
      (:batargs
       (if (car args)
         (let ((init (concatenate 'string (reldir *cm-root* "etc")
                                  "cminit.lisp")))
           ;; We can't format pathnames on windows using ~S 
           ;; or else the \ appears 2x in the output string.
           ;; So we print ~A with explicit " surrounding it.
           (format nil "-I -M \"~~A\" -i \"~A\"" init))
         "-I -q -ansi -x ~S -x t -repl"))
      (:gc (ext:gc))
      (:mkdir
       #+win32 (ext:run-program "mkdir" :arguments args :indirectp t)
       #-win32 (ext:run-program "mkdir" :arguments args))
      (:cp 
       #+win32 (ext:run-program "copy" :arguments args :indirectp t)
       #-win32 (ext:run-program "cp" :arguments args))
      (:fw?
       #+win32 (windoze-writable? (car args))
       #-win32
       (equal 0 (ext:run-shell-command 
                 (format nil "test -w '~A'" (car args)))))
      (:cm 
       (setf *package* (find-package :cm))
       (setf *readtable* (cm-var :*cm-readtable*)))
      (:bye (quit))
      ))
  )

#+cmu
(progn
  (defun insure-sys-features ()
    (declaim (optimize (extensions:inhibit-warnings 3)))
    (setq *compile-print* nil)
    (setq *compile-progress* nil)
    ;; goddam cmucl -- unlock-all-packages is only
    ;; defined in 19 but there is no way of testing
    ;; if one release version is greater than another
    (let ((fn (find-symbol "UNLOCK-ALL-PACKAGES")))
      (when (and fn (symbol-function fn))
	(funcall fn)))
    (setf extensions::*gc-verbose* nil)

    ;; check for midishare libs, if exist then load FFI
    ;; if player not installed then :ms only.
    (if (probe-file "/usr/lib/libMidiShare.so")
      (if (directory "/usr/lib/libPlayer*")
        (setq midishare-installed? t)
        (setq midishare-installed? ':ms))))
  
  (defun get-cmu-version (str)
    ;; cmucl "version numbers" are a total mess.
    (let* ((len (length str))
           (bag '(#\space #\-))
           (beg (or (position-if #'digit-char-p str)
                    (error "Can't parse CMU version from ~S." str)))
           (end (or (position-if #'(lambda (c) (member c bag))
                                 str :start beg)
                    len))
           (ver (subseq str beg end)))
      (flet ((version-field? (str)
               ;; str is a "version field" if it has at least one
               ;; digit and includes only alphanumerics and periods
               (and (find-if #'digit-char-p str)
                    (every #'(lambda (c) 
                               (or (alphanumericp c)
                                   (char= c #\.)))
                           str))))
        (unless (version-field? ver)
          (error "Can't parse CMU version from ~S." str))
        (do ((e nil) (s nil))
            ((not (and end (< end len) (char= (elt str end) #\-)))
             ver)
          (setq e (position-if #'(lambda (c) (member c bag))
                               str :start (+ end 1)))
          (setq s (subseq str (+ end 1) e))
          (if (version-field? s)
            (progn
              (setq ver (concatenate 'string ver "-" s))
              (setq end (if e (+ e 1) len)))
            (setq end len))))))

  (defun syscmd (op &rest args)
    (ecase op
      (:lisp-version 
       (format nil "cmucl_~a"
               (get-cmu-version (lisp-implementation-version))))
      (:os-arch
       (or (cdr (assoc ':cm_platform ext:*environment-list*))
	   (stream-line
	    (ext:process-output
	     (ext:run-program (cm.sh) '("-q") :output :stream)))))
      (:fasl (c:backend-fasl-file-type c:*backend*))
      (:image "cm.img")
      (:exec  (first ext:*command-line-strings*))
      (:batargs (error "Shouldn't call :batargs in cmucl."))
      (:gc (let ((ext:*gc-verbose* nil))
             (extensions:gc)))
      (:mkdir (extensions:run-program "mkdir" args))
      (:cp (extensions:run-program "cp" args))
      (:fw? (extensions:file-writable (car args)))
      (:cm 
       (setf *package* (find-package :cm))
       (setf *readtable* (cm-var :*cm-readtable*)))
      (:bye (quit))))
  )

#+sbcl
(progn
  (defun insure-sys-features ()  
    (setq *compile-print* nil)
    (setq *compile-verbose* nil)
    (setq *compile-progress* nil)
    ;; offending symbols: funcall, warning, random
    (sb-ext:unlock-package (find-package :common-lisp))
    (declaim (optimize (sb-ext:inhibit-warnings 3))) 
    ;(setq sb-ext::*gc-verbose* nil)
    (defvar %sb-mkdir
      (or (probe-file "/bin/mkdir")
          (probe-file "/usr/bin/mkdir")
          (error "Fixme: can't find unix command 'mkdir' on this system.")))
    (defvar %sb-cp
      (or (probe-file "/bin/cp")
          (probe-file "/usr/bin/cp")
          (error "Fixme: can't find unix command 'cp' on this system.")))
    (defvar %sb-test
      (or (probe-file "/bin/test")
          (probe-file "/usr/bin/test")
          (error "Fixme: can't find unix command 'test' on this system.")))
    (require :sb-posix))

  (defun syscmd (op &rest args)
    (declare (special %sb-cp %sb-test %sb-mkdir))
    (ecase op
      (:lisp-version 
       (let ((str (lisp-implementation-version)))
         (format nil "sbcl_~A"
                 (subseq str 0 (position #\space str)))))
      (:os-arch
       (stream-line
        (sb-ext:process-output
         (sb-ext:run-program (cm.sh) '("-q") :output :stream))))
      (:fasl sb-fasl:*fasl-file-type* )
      (:image "cm.img")
      (:exec (elt sb-ext:*posix-argv* 0))
      (:batargs (error "Shouldn't call :batargs in SBCL."))
      (:gc (sb-ext:gc))
      (:mkdir (sb-ext:run-program (namestring %sb-mkdir) args))
      (:cp (sb-ext:run-program (namestring %sb-cp)  args))
      (:fw?
       (let ((p (sb-ext:run-program (namestring %sb-test)
                                    (cons "-w" args) :wait t)))
         (sb-ext:process-exit-code p)))
      (:cm 
       (setf *package* (find-package :cm))
       (setf *readtable* (cm-var :*cm-readtable*)))
      (:bye (sb-ext:quit))))
  )

#+excl
(progn
  (defun insure-sys-features ()
    (cond ((find ':microsoft-32 *features*)
           (pushnew :win32 *features*))
          ((find ':macosx *features*)
           (pushnew :osx *features*)))
    (setf excl::*print-startup-message* nil)
    (setf excl:*cltl1-in-package-compatibility-p* t)
    (setf comp:*cltl1-compile-file-toplevel-compatibility-p* t)
    (setf (excl:package-definition-lock (find-package :common-lisp))
          nil)
    (require :loop))

  (defun syscmd (op &rest args)
    (ecase op
      (:lisp-version 
       (let ((str (lisp-implementation-version)))
         (format nil "acl_~a"
                 (subseq str 0 (position #\space str)))))
      (:os-arch
       #+microsoft-32 "windows-i686"
       #-microsoft-32
       (or (sys:getenv "CM_PLATFORM")
           (stream-line
            (excl:run-shell-command (cm-sh "-q") :output :stream
                                    :wait nil))))
      (:fasl excl:*fasl-default-type*)
      (:image "cm.img")
      (:exec (system:command-line-argument 0))
      (:batargs 
       (if (car args) "-I \"~A\""
           ;; if we are not providing an image we have
           ;; to use the one that was passed in as the
           ;; -I command line arg.
           (let ((img (cadr (member "-I" 
                                    (sys:command-line-arguments
                                     :application nil)
                                    :test #'equal))))
             (if img
               (format nil "-I \"~A\" -e ~~S" img)
               "-e ~S")
             )))
      (:gc (excl:gc t))
      (:mkdir (excl:make-directory (car args)))
      (:cp
       ;; acl's copy-file won't overwrite...
       #+microsoft-32
       (progn (when (probe-file (cadr args))
                (delete-file (cadr args))
                (sys:copy-file (car args) (cadr args)
                               :link-ok nil)))
       #-microsoft-32
       (excl:run-shell-command
        (format nil "cp ~a ~a" (car args) (cadr args))))
      (:fw?
       #+microsoft-32 (windoze-writable? (car args))
       #-microsoft-32
       (equal 0 (excl:run-shell-command 
                 (format nil "test -w '~A'" (car args)))))
      (:cm 
       (tpl:setq-default *package* (find-package :cm))
       (tpl:setq-default *readtable* (cm-var :*cm-readtable*))
       )
      (:bye (exit)))) 
  )

#+(and mcl (not openmcl))
(progn
  (defun insure-sys-features ()
    (if (find :carbon-compat *features*)
      (pushnew ':osx *features*)
      (pushnew ':macos *features*))
    (ccl::replace-base-translation "home:"
                                   (ccl::startup-directory))
    (unless (find ':loop *features*) 
      (load "ccl:library;loop" :verbose nil)
      (pushnew ':loop *features*)))

  (defun syscmd (op &rest args)
    (ecase op
      (:lisp-version 
       (let* ((str (lisp-implementation-version))
              (pos (+ (position #\space str :from-end t)  1)))
         (format nil "mcl_~a" (subseq str pos ))))
      (:os-arch "macos-powerpc")
      (:fasl (pathname-type ccl:*.fasl-pathname*))
      (:image "Common Music")
      (:exec nil)
      (:batargs (error "Shouldn't call :batargs in MCL."))
      (:gc (ccl:gc))
      (:mkdir (ccl:create-directory (car args)))
      (:cp (ccl:copy-file (car args) (cadr args)
                          :if-exists :supersede))
      (:fw? t)
      (:cm 
       (setf *package* (find-package :cm))
       (setf *readtable* (cm-var :*cm-readtable*)))
      (:bye ;(quit)
	    )))
  )

#+openmcl
(progn
  (defun insure-sys-features ()
    (cond ((find ':darwin *features*)
           (pushnew ':osx *features*))
          ((find ':linuxppc *features*)
           (pushnew ':linux *features*)))
    (unless (or (find-package :ansi-loop)
                (find ':loop *features*) )
      (load "ccl:library;loop" :verbose nil)
      (pushnew ':loop *features*))
    ;; cload Midishare's interface files if everything exists.
    (let ((d1 "ccl:darwin-headers;midishare;")
          (d2 "/System/Library/Frameworks/Midishare.framework")
          (d3 "/System/Library/Frameworks/Player.framework"))
      (when (and (probe-file d1) (probe-file d2))
        (if (probe-file d3)
          (setq midishare-installed? t)
          (setq midishare-installed? ':ms)))))

  (defun syscmd (op &rest args)
    (ecase op
      (:lisp-version 
       (let* ((str (lisp-implementation-version))
              (pos (+ (position #\space str :from-end t)  1)))
         (format nil "openmcl_~A" (subseq str pos ))))
      (:os-arch
       (or (ccl::getenv "CM_PLATFORM")
           (stream-line
            (ccl:external-process-output-stream
             (ccl:run-program (cm.sh) '("-q" ) :wait nil
                              :output :stream)))))
      (:fasl (pathname-type ccl:*.fasl-pathname*))
      (:image "cm.img")
      (:exec (first (ccl::command-line-arguments)))
      (:batargs (error "Shouldn't call :batargs in openmcl."))
      (:gc (ccl:gc))
      (:mkdir (ccl:create-directory (car args)))
      (:cp (ccl:copy-file (car args) (cadr args)
                          :if-exists :supersede))
      (:fw?
       (let ((p (ccl:run-program "test" (cons "-w" args) :wait t)))
         (multiple-value-bind (s v) (ccl:external-process-status p)
           (if (eql s :exited)
             (eql v 0)
             nil))))
      (:cm 
       (setf *package* (find-package :cm))
       (setf *readtable* (cm-var :*cm-readtable*)))
      (:bye (quit))))
  )

