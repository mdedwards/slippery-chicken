;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/slippery-chicken.asd
;;; NAME
;;; slippery-chicken.asd
;;;
;;; File:             slippery-chicken.asd
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          System definition for slippery chicken.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 15th 2024
;;;
;;; $$ Last modified:  18:03:15 Mon Mar 18 2024 CET
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 3 of the License, or (at your
;;;                   option) any later version.
;;;
;;;                   slippery-chicken is distributed in the hope that it will
;;;                   be useful, but WITHOUT ANY WARRANTY; without even the
;;;                   implied warranty of MERCHANTABILITY or FITNESS FOR A
;;;                   PARTICULAR PURPOSE.  See the GNU General Public License
;;;                   for more details.
;;;
;;;                   You should have received a copy of the GNU General Public
;;;                   License along with slippery-chicken; if not, write to the
;;;                   Free Software Foundation, Inc., 59 Temple Place, Suite
;;;                   330, Boston, MA 02111-1307 USA
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +slippery-chicken-version+ "1.1.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function is necessary as ASDF's standard compilation routine does
;;; not work with clm instruments.
;;; When evaluated during the compile-op :before hook of a system component,
;;; this preempts ASDF's default compilation procedure
;;; (i.e. asdf::perform-lisp-compilation).
;;; RP  Fri Mar 15 18:58:21 2024
(defun compile-clm-ins (o c)
  (let ((output (first (asdf::output-files o c))))
    (compile-file (asdf::component-pathname c)
                  :output-file output)
    (load output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl (require :sb-bsd-sockets)
#+sbcl (require :sb-posix)
#+sbcl (unlock-package "COMMON-LISP")

(pushnew :cm *features*)
(pushnew :cm-2 *features*)
(pushnew :slippery-chicken *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "slippery-chicken"
  :version cl-user::+slippery-chicken-version+
  :license "GPL Version 3.0 or later"
  :author "Michael Edwards <m@michael-edwards.org>"
  :description "Algorithmic composition software in Common Lisp and CLOS"
  :serial t
  :depends-on ("cm"
               "cl-ppcre")
  :default-component-class cl-source-file.lsp
  :pathname "src/"
  ;;; RP  Sun Mar 17 19:32:01 2024
  :in-order-to ((test-op (test-op "slippery-chicken/tests")))
  :components ((:file "package")
               (:file "cmn" :if-feature :cmn)
               (:file "cmn-glyphs" :if-feature :cmn)
               (:file "cm" :if-feature :cm)
               (:file "cm-cm" :if-feature :cm)
               (:file "cm-load")
               (:file "utilities")
               (:file "named-object")
               (:file "music-xml")
               (:file "linked-named-object")
               (:file "sclist")
               (:file "circular-sclist")
               (:file "assoc-list")
               (:file "recursive-assoc-list")
               (:file "activity-levels")
               (:file "activity-levels-env")
               (:file "activity-levels-pc")
               (:file "palette")
               (:file "pitch-seq-palette")
               (:file "globals")
               (:file "samp5" :if-feature :clm
                :perform (compile-op :before (o c)
                                     (cl-user::compile-clm-ins o c)))
               (:file "sine" :if-feature :clm
                :perform (compile-op :before (o c)
                                     (cl-user::compile-clm-ins o c)))
               (:file "sc-map")
               (:file "set-map")
               (:file "tempo")
               (:file "rhythm")
               (:file "pitch")
               (:file "chord")
               (:file "event")
               (:file "instrument")
               (:file "time-sig")
               (:file "rthm-seq-bar")
               (:file "change-data")
               (:file "change-map")
               (:file "instrument-change-map")
               (:file "simple-change-map")
               (:file "tempo-map")
               (:file "sc-set")
               (:file "tl-set")
               (:file "complete-set")
               (:file "set-palette")
               (:file "pitch-seq")
               (:file "sndfile")
               (:file "sndfile-ext")
               (:file "sndfile-palette")
               (:file "sndfilenet")
               (:file "rthm-seq")
               (:file "rthm-seq-palette")
               (:file "rthm-seq-map")
               (:file "instrument-palette")
               (:file "instruments")
               (:file "player")
               (:file "bar-holder")
               (:file "sequenz")
               (:file "ensemble")
               (:file "l-for-lookup")
               (:file "player-section")
               (:file "section")
               (:file "slippery-chicken")
               (:file "piece")
               (:file "slippery-chicken-edit")
               (:file "clm-defaults" :if-feature :clm)
               (:file "clm" :if-feature :clm)
               (:file "permutations")
               (:file "rthm-chain")
               (:file "rthm-chain-slow")
               (:file "cycle-repeats")
               (:file "recurring-event")
               (:file "intervals-mapper")
               (:file "lilypond")
               (:file "popcorn")
               #+(and (or linux darwin) sbcl) (:file "osc")
               #+(and (or linux darwin) sbcl) (:file "osc-sc")
               #+(and (or linux darwin) sbcl) (:file "osc-sc-bsd")
               (:file "get-spectrum" :if-feature :clm
                :perform (compile-op :before (o c)
                                     (cl-user::compile-clm-ins o c)))
               (:file "spectra")
               (:file "control-wave-ins" :if-feature :clm
                :perform (compile-op :before (o c)
                                     (cl-user::compile-clm-ins o c)))
               (:file "control-wave" :if-feature :clm)
               (:file "wolfram"
                :perform (compile-op :before (o c)
                                     ;; wolfram needs to be loaded before
                                     ;; compilation
                                     ;; RP  Fri Mar 15 18:15:45 2024
                                     (load (component-pathname c))))
               (:file "afu")
               (:file "reaper")
               ;; NB: export-symbols should always be included at the end
               ;;     of this definition
               ;; RP  Fri Mar 15 23:31:51 2024
               (:file "export-symbols")))


;;; This system does not load any modules but defines a test-op for the
;;; slippery-chicken system. The test suite itself is located in the tests/
;;; directory.
;;; RP  Sun Mar 17 19:33:23 2024
(defsystem "slippery-chicken/tests"
  :description "Test suite for slippery-chicken."
  :author "Michael Edwards <m@michael-edwards.org>"
  :license "GPL Version 3.0 or later"
  :depends-on ("slippery-chicken")
  :perform (test-op (o c) (symbol-call :slippery-chicken :run-tests)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from all.lsp
;;; RP  Fri Mar 15 21:54:15 2024
;;; MDE Fri Jun 21 17:08:42 2013 
#+(and (or sbcl ccl) (or linux darwin))
(pushnew :sc-auto-open *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc (&optional (logo t))
  (declare (special +slippery-chicken-src-path+))
  (setf *package* (find-package :sc))
  (let* ((title (format nil "slippery chicken ~a"
                        +slippery-chicken-version+))
         (sc-logo (concatenate 'string +slippery-chicken-src-path+
                               "txt/sc-ascii-logo.txt"))
         (in (open sc-logo :if-does-not-exist nil)))
    (when logo
      (if in 
          (progn
            (loop for line = (read-line in nil)
               while line do (format t "~&~a" line))
            (close in))
          (format t "(\\  }\\   ~%(  \\_('> ~a~%(__(=_)  ~%   -\"=   ~%"
                  title)))
    #+sbcl t
    #-sbcl (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF slippery-chicken.asd
