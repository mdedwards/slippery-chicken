;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/globals.lsp
;;; NAME
;;; globals
;;;
;;; File:             globals.lsp
;;;
;;; Class Hierarchy:  None: no classes defined.
;;;
;;; Version:          1.0.3
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of the user-changeable configuration data and
;;;                   globals for internal programme use.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    30th May 2013
;;;
;;; $$ Last modified: 16:11:50 Wed Jun 19 2013 BST
;;;
;;; SVN ID: $Id: sclist.lsp 963 2010-04-08 20:58:32Z medward2 $
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

(in-package :slippery-chicken)

;;; ****P* globals/+slippery-chicken-config-data+
;;; DESCRIPTION
;;; A global to hold various user-settable configuration settings.  Use e.g. 
;;; (set-sc-config 'default-dir "~/Desktop") or (get-sc-config 'default-dir) to
;;; set or query the settings.  
;;; 
;;; SYNOPSIS
(defparameter +slippery-chicken-config-data+
  (make-instance
   'assoc-list
   :id 'slippery-chicken-config-data
   :data
   ;; MDE Sun Mar 25 10:39:07 2012 -- The following two are used in
   ;; pitch-seq::get-notes to indicate which lowest number in a pitch-seq would
   ;; indicate that we should select the highest or lowest notes possible for
   ;; the instrument/set.
   ;; 
   ;; The first is used to indicate the lowest number in a pitch-seq that would
   ;; indicate that the get-notes algorithm should select the highest notes
   ;; possible for the instrument/set.
   '((pitch-seq-lowest-equals-prefers-high 5)
     ;; the lowest number in a pitch-seq that would indicate that the get-notes
     ;; algorithm should select the lowest notes possible for the
     ;; instrument/set.
     (pitch-seq-lowest-equals-prefers-low 1)
     ;; Whether to automatically open EPS files generated with CMN via
     ;; cmn-display.  Currently only works with SBCL on Mac OSX.
     (cmn-display-auto-open #+sbcl T #-sbcl nil)
     ;; Whether to automatically open PDF files generated with via lp-display.
     ;; Currently only works with SBCL on Mac OSX.
     (lp-display-auto-open #+sbcl T #-sbcl nil)
     ;; Whether to automatically open MIDI files generated with via midi-play.
     ;; Currently only works with SBCL on Mac OSX.
     (midi-play-auto-open #+sbcl T #-sbcl nil)
     ;; The default directory for output of sound files, EPS files, and
     ;; Lilypond files. Don't forget the trailing slash (i.e. "/tmp/" not
     ;; "/tmp").  Bear in mind that on OSX the /tmp directory is emptied upon
     ;; reboot so you shouldn't store any files you'd like to keep in there.
     (default-dir "/tmp/")
     ;; The full path to the lilypond command.  We need to set this if we'll
     ;; call lp-display, i.e. if we want to automatically call Lilypond and
     ;; open the resultant PDF directly from Lisp.  The default should work if
     ;; you have the Lilypond app in your Applications folder on OSX.
     (lilypond-command
      "/Applications/LilyPond.app/Contents/Resources/bin/lilypond")
     ;; The default amplitude for all events that don't have amplitude/dynamic
     ;; set via some means such as marks.
     (default-amplitude 0.7)
     ;; Bar number offsets for CMN
     (cmn-bar-num-dx-for-sc -0.2)
     (cmn-bar-num-dy-for-sc 1.2)
     ;; font size for CMN bar numbers
     (cmn-bar-num-size-for-sc 6))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-sc-config (key)
  (declare (special +slippery-chicken-config-data+))
  (get-data-data key +slippery-chicken-config-data+))

(defun set-sc-config (key value)
  (declare (special +slippery-chicken-config-data+))
  (replace-data key value +slippery-chicken-config-data+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmn.lsp
(in-package :cmn)

(declaim (special *cmn-units*))
(setf *cmn-units* :cm)

;;; We're not going to have more than 20 nested brackets applied to a single
;;; note are we? :=) 
(defparameter +cmn-open-brackets-for-sc+ (make-list 20))
(defparameter +cmn-grace-notes-for-sc+ nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF globals.lsp
