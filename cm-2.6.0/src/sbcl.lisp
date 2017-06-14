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
;;; $Revision: 1.5 $
;;; $Date: 2005/03/19 18:30:27 $

(in-package :cm)

(import '(sb-ext::load-foreign
          sb-pcl:slot-definition-initargs
          sb-pcl:slot-definition-initform
          sb-pcl:slot-definition-name
          sb-pcl:class-direct-slots
          sb-pcl:class-slots
          sb-pcl:class-direct-superclasses
          sb-pcl:generic-function-name
          sb-mop:class-direct-subclasses
          )
        :cm)

(require :sb-posix)
(import '(sb-posix:chdir) :cm)

(defun quit () (sb-ext:quit))
(defun exit () (quit))

;(defun class-subclasses (c)
;  (let ((subs (sb-pcl:class-direct-subclasses c)))
;    (if (null subs)
;	'()
;      (loop for s in subs
;	append (cons s (class-subclasses s))))))
	  
(defun finalize-class (class) 
  (sb-pcl:finalize-inheritance class))

(defgeneric validate-class (obj1 obj2))
(defmethod validate-class ((class t) (superclass t))
  ;; this is a no-op except in OpenMCL 014
  t)

(defun slot-defintion-reader (slot) slot nil)

;;;
;;; misc. stuff
;;;

(defun arglist (function)
  (sb-kernel:%simple-fun-arglist function))

(defun object-address (thing)
  (sb-kernel:get-lisp-obj-address thing))

(defconstant directory-delimiter #\/)

;;(defun shell (format &rest strings) 
;;  (let ((str (apply #'format nil format strings)))
;;    (sb-ext:run-program "/bin/csh" (list "-fc" str) :output t)))

(defun shell (cmd &key (wait t) (output t))
  (sb-ext:run-program "/bin/csh" (list "-fc" cmd)
                      :output output :wait wait))

(defun cd (&optional (dir (user-homedir-pathname )))
  (sb-posix:chdir dir)
  (let ((host (pathname-host dir))
        (name (pathname-name dir))
        (path (pathname-directory dir)))
    ;; allow dirs without ending delim "/tmp"
    (when name
      (setq path (append path (list name))))
    (setq *default-pathname-defaults*
          (make-pathname :host host :directory path))
    (namestring *default-pathname-defaults*)))

(defun pwd ()
  (namestring
   (make-pathname :host (pathname-host *default-pathname-defaults*)
                  :directory (pathname-directory
                              *default-pathname-defaults*))))

(defun env-var (var)
  ;(let ((x (assoc var ext:*environment-list*
  ;                  :test #'string=)))
  ;  (and x (cdr x) ))
  )

(defun cm-image-dir ()
  (let ((img (second (member "--core" sb-ext:*posix-argv*
                             :test #'string=))))
    (if img
      (namestring
       (make-pathname :directory (pathname-directory img)))
      nil)))

(defun save-cm (pathname &rest args)
  (declare (ignore args))
  (sb-ext:save-lisp-and-die (namestring pathname)
                            :toplevel
                            #'(lambda ()
                                (declare (special *cm-readtable*))
                                (setf *readtable* *cm-readtable*)
                                (setf *package* (find-package :cm))
                                (load-cminit)                            
                                (cm-logo)
                                (sb-impl::toplevel-init)
                                )))

;;
;;; arrrrg a pox on frigging style warnings!!


(defgeneric class-parameters (obj))
(defgeneric (setf class-parameters) (val obj))
(defgeneric io-class-file-types (obj))
(defgeneric (setf io-class-file-types) (val obj))
(defgeneric io-class-mime-type (obj))
(defgeneric (setf io-class-mime-type) (val obj))
(defgeneric io-class-output-hook (obj))
(defgeneric (setf io-class-output-hook) (val obj))
(defgeneric io-class-definer (obj))
(defgeneric (setf io-class-definer) (val obj))
(defgeneric io-class-file-versions (obj))
(defgeneric (setf io-class-file-versions) (val obj))
(defgeneric validate-superclass (obj1 obj2))
(defgeneric make-load-form (obj))
(defgeneric fill-object (obj1 obj2))
(defgeneric rename-object (obj1 name &rest args))
(defgeneric copy-object (obj))
(defgeneric subcontainers (obj))

(defgeneric insert-object (obj1 obj2))
(defgeneric append-object (obj1 obj2))
(defgeneric remove-object (obj1 obj2))
(defgeneric remove-subobjects (obj))
(defgeneric list-subobjects (obj  &key start end start-time end-time))
(defgeneric amplitude (amp &optional softest loudest power))
(defgeneric rhythm (rhy &optional tempo beat))
(defgeneric tuning->mode (mode keynum force?))
(defgeneric mode->tuning (mode keynum return))
(defgeneric tuning-hertz->keynum (obj hz))
(defgeneric tuning-hertz->note (scale hz acci err?))
(defgeneric tuning-keynum->hertz (obj knum))
(defgeneric tuning-keynum->note (obj knum acci err?))
(defgeneric tuning-note->hertz (obj note err?))
(defgeneric tuning-note->keynum (obj note err?))
(defgeneric tuning-note->note (obj note acci err?))
(defgeneric scale-mod (freq modulus &key offset in accidental))
(defgeneric note-in-scale? (note scale))
(defgeneric transpose (note int &optional scale))
(defgeneric invert (ref &optional pc?))
(defgeneric interval (int &optional int2))

(defgeneric canonicalize-pattern-data (obj data parser inits))
(defgeneric pattern-external-inits (obj))
(defgeneric pattern-period-length (obj))
(defgeneric default-period-length (obj))
(defgeneric pattern? (obj))
(defgeneric eop? (obj))
(defgeneric eod? (obj))
(defgeneric next-1 (obj))
(defgeneric skip-datum? (obj))
(defgeneric reset-period (obj))
(defgeneric next-in-pattern (obj))
(defgeneric map-pattern-data (fn obj))

(defgeneric io-handler-args (obj))
(defgeneric io-handler-args? (obj))
(defgeneric set-io-handler-args! (obj args))
(defgeneric init-io (obj &rest inits))
(defgeneric open-io (obj dir &rest args))
(defgeneric close-io (obj &rest mode))
(defgeneric initialize-io (obj))
(defgeneric deinitialize-io (obj))
(defgeneric write-event (obj io time))
(defgeneric import-events (obj &rest args))

(defgeneric schedule-object (obj start))
(defgeneric unschedule-object (obj &rest recurse))
(defgeneric process-events (obj time start func))
(defgeneric sprout (obj &optional time))

(defgeneric midi-event-data1 (obj))
(defgeneric midi-event-data2 (obj))
(defgeneric midi-event->midi-message (obj))

(defgeneric midi-write-message (msg mf time data))

(defgeneric object->midi (obj))

(defgeneric midishare-stream-refnum (obj))
(defgeneric open-midishare-client (obj name))
(defgeneric close-midishare-client (obj))

(defgeneric player-play (obj))
(defgeneric player-set-sync (obj sync))
(defgeneric player-set-tempo (obj tempo))
(defgeneric player-start (obj))
(defgeneric player-stop (obj))
(defgeneric player-pause (obj))
(defgeneric player-cont (obj))
(defgeneric player-mute (obj track))
(defgeneric player-unmute (obj track))
(defgeneric player-solo (obj track))
(defgeneric player-unsolo (obj track))
(defgeneric player-load-midifile (obj fil))
(defgeneric player-save-midifile (obj fil))

(defgeneric object->cmn (obj))

