;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/all.lsp
;;; NAME 
;;; all
;;;
;;; File:             all.lsp
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Load all the lisp files associated with slippery-chicken
;;;                   No public interface envisaged (so no robodoc entries).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    5th December 2000
;;;
;;; $$ Last modified: 16:12:18 Thu Dec 15 2011 ICT
;;;
;;; SVN ID: $Id$
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 2 of the License, or (at your
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

;;; 01.12.11 SEAN: Edited ROBODoc header for hierarchy-based TOC

;;; 28.11.11 SEAN: changed item to "all.lsp" so that robodoc will not highlight
;;; the word "all" in every instance as though it were a class with a doc page.

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl (unlock-package "COMMON-LISP")

(in-package :cl-user)

(defconstant +slippery-chicken-version+ "1.00")

;;; MDE Thu Dec  8 23:19:01 2011 -- get the cwd automatically now, rather
;;; than from user's global 
(defparameter +slippery-chicken-src-path+
  (directory-namestring (truename *load-pathname*)))

(defparameter +slippery-chicken-home-dir+
  (directory-namestring
   (make-pathname
    :directory
    (butlast (pathname-directory cl-user::+slippery-chicken-src-path+)))))

;;; Make sure any typed-in float constants are high-precision; default is
;;; single-float  
(setf *read-default-float-format* 'double-float)

(defparameter +sc-dir-separator+
  #+(or windows mswindows win32) #\\
  ;; #+mcl #\: ; back in pre-OSX days
  #+unix #\/)

(defparameter +sc-fasl-extension+
  #+clisp ".fas"
  #+openmcl ".dfsl"
  #+sbcl ".fasl"
  #+allegro ".fasl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc (&optional (logo t))
  (setf *package* (find-package :sc))
  (let ((title (format nil "slippery chicken version ~a"
                       +slippery-chicken-version+)))
    (if logo
        (format t "(\\  }\\   ~%(  \\_('>   ~a~%(__(=_)  ~%   -\"=   ~%" title)
        (print title)))
  #+sbcl t
  #-sbcl (values))

;;; old stuff; could have used pathname functions for this....
(defun get-path-minus-file-and-last-dir (file)
  (flet ((till-last-slash (x)
                          (subseq x 0 (position +sc-dir-separator+ x
                                                :from-end t))))
    (till-last-slash (till-last-slash file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-allegro-cl-lite
(defun sc-compile-and-load (file &optional (just-load nil) (dir nil))
  (unless dir
    (setq dir (directory-namestring +slippery-chicken-src-path+)))
  #+allegro
  (progn
    (cl-user::chdir +slippery-chicken-src-path+)
    (setf *default-pathname-defaults* +slippery-chicken-src-path+))
  (let ((out (format nil "~a~abin~a~a~a"
                     (get-path-minus-file-and-last-dir dir)
                     +sc-dir-separator+
                     +sc-dir-separator+
                     (pathname-name file)
                     +sc-fasl-extension+))
        (in (format nil "~a~asrc~a~a" (get-path-minus-file-and-last-dir dir)
                    +sc-dir-separator+ +sc-dir-separator+ file)))
    (print out)
    (print in)
    (if just-load
        (load in)
      (progn
        (unless (and (probe-file out)
                     (> (file-write-date out)
                        (file-write-date (print in))))
          (compile-file in :output-file out))
        (load out)))))

#+allegro-cl-lite
(defun sc-compile-and-load (file)
  (load file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 7.12.11 rather than load just parts of CM, let's just run its build script
;;; and make it all (some users might want to take advantage of the whole of
;;; Rick's system). 
(defun sc-load-cm-all ()
  (declare (special +slippery-chicken-src-path+))
  (load (format nil "~acm-2.6.0/src/cm.lisp" 
                +slippery-chicken-src-path+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.2.10: We're no longer expecting a fully-working latest Common Music
;;; system as Rick has ventured off into Scheme land.  Just use CM 2.6.0
;;; (thanks Rick!) and load the files we need for the functionality we need.
;;; NB Only working in SBCL for now.

(defun sc-load-cm-essentials ()
  ;; the patch to common music's src directory needs to be defined
  ;; before we call this function
  ;; e.g. (defparameter *slippery-chicken-cm-path*
  ;;                    "/user/michael/cm-2.6.0/src/")
  (declare (special *slippery-chicken-cm-path*))
  (unless *slippery-chicken-cm-path*
    (error "Variable *slippery-chicken-cm-path* must be set!"))
  (flet ((load-cm-file (file) ;; .lisp extention auto-added
           (load (format nil "~a~a.lisp" *slippery-chicken-cm-path* file))))
    (load-cm-file "midishare/midishare-stubs")
    #-clm
    (load-cm-file "clm-stubs")
    #-cmn
    (load-cm-file "cmn-stubs")
    (loop for f in 
         '("pkg" "sbcl" "clos" "iter" "level1" "utils" "mop" "objects" "data" 
           "scales" "spectral" "patterns" "io" "scheduler" "sco" "clm" "clm2" 
           ;; "midishare" "midishare" "loop" "midishare" "player"
           "midi1" "midi2" "midi3" "cmn")
         do
         (load-cm-file f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (sc-load-cm-essentials)
(sc-load-cm-all)
;;; It seems CM doesn't put itself on the features list but sc needs it.
(pushnew :cm *features*)
(pushnew :cm-2 *features*)

(sc-compile-and-load "package.lsp")
#+cmn
(sc-compile-and-load "cmn.lsp")
#+cmn
(sc-compile-and-load "cmn-glyphs.lsp")
#+cm
(sc-compile-and-load "cm.lsp")
#+clm
(sc-compile-and-load "samp5.lsp")
(sc-compile-and-load "cm-load.lsp" t)
(sc-compile-and-load "utilities.lsp")
(sc-compile-and-load "named-object.lsp")
(sc-compile-and-load "linked-named-object.lsp")
(sc-compile-and-load "sclist.lsp")
(sc-compile-and-load "circular-sclist.lsp")
(sc-compile-and-load "assoc-list.lsp")
(sc-compile-and-load "recursive-assoc-list.lsp")
(sc-compile-and-load "palette.lsp")
(sc-compile-and-load "sc-map.lsp")
(sc-compile-and-load "set-map.lsp")
(sc-compile-and-load "tempo.lsp")
(sc-compile-and-load "rhythm.lsp")
(sc-compile-and-load "pitch.lsp")
(sc-compile-and-load "chord.lsp")
(sc-compile-and-load "event.lsp")
(sc-compile-and-load "time-sig.lsp")
(sc-compile-and-load "rthm-seq-bar.lsp")
(sc-compile-and-load "change-data.lsp")
(sc-compile-and-load "change-map.lsp")
(sc-compile-and-load "instrument-change-map.lsp")
(sc-compile-and-load "simple-change-map.lsp")
(sc-compile-and-load "tempo-map.lsp")
(sc-compile-and-load "pitch-seq-palette.lsp")
(sc-compile-and-load "instrument.lsp")
(sc-compile-and-load "sc-set.lsp")
(sc-compile-and-load "tl-set.lsp")
(sc-compile-and-load "complete-set.lsp")
(sc-compile-and-load "set-palette.lsp")
(sc-compile-and-load "pitch-seq.lsp")
(sc-compile-and-load "sndfile.lsp")
(sc-compile-and-load "sndfile-palette.lsp")
(sc-compile-and-load "rthm-seq.lsp")
(sc-compile-and-load "rthm-seq-palette.lsp")
(sc-compile-and-load "rthm-seq-map.lsp")
(sc-compile-and-load "instrument-palette.lsp")
(sc-compile-and-load "player.lsp")
(sc-compile-and-load "bar-holder.lsp")
(sc-compile-and-load "sequenz.lsp")
(sc-compile-and-load "ensemble.lsp")
(sc-compile-and-load "l-for-lookup.lsp")
(sc-compile-and-load "player-section.lsp")
(sc-compile-and-load "piece.lsp")
(sc-compile-and-load "section.lsp")
(sc-compile-and-load "slippery-chicken.lsp")
(sc-compile-and-load "permutations.lsp")
(sc-compile-and-load "rthm-chain.lsp")
(sc-compile-and-load "rthm-chain-slow.lsp")
(sc-compile-and-load "cycle-repeats.lsp")
(sc-compile-and-load "activity-levels.lsp")
(sc-compile-and-load "recurring-event.lsp")
(sc-compile-and-load "sc-scale.lsp")
(sc-compile-and-load "instruments.lsp")
(sc-compile-and-load "lilypond.lsp")
(sc-compile-and-load "popcorn.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF all.lsp

