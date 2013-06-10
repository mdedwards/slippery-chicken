;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/globals.lsp
;;; NAME
;;; globals
;;;
;;; File:             globals.lsp
;;;
;;; Class Hierarchy:  None: no classes defined.
;;;
;;; Version:          1.0.2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of the user-changeable global parameters.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    30th May 2013
;;;
;;; $$ Last modified: 20:07:35 Tue Jun  4 2013 BST
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sun Mar 25 10:39:07 2012 -- The following two constants are used in
;;; pitch-seq::get-notes to indicate which lowest number in a pitch-seq would
;;; indicate that we should select the highest or lowest notes possible for the
;;; instrument/set. 

;;; SAR Tue Aug  7 17:13:41 BST 2012: Added robodoc entry

;;; ****P* pitch-seq/+pitch-seq-lowest-equals-prefers-high+
;;; DESCRIPTION
;;; A slippery-chicken constant variable used to indicate the lowest number in
;;; a pitch-seq that would indicate that the get-notes algorithm should select
;;; the highest notes possible for the instrument/set. If not otherwise
;;; specified, this value defaults to 5.
;;; 
;;; ARGUMENTS
;;; - An integer that is the lowest number in a pitch-seq that would indicate
;;;   that the given instrument prefers high notes for that sequence.
;;; 
;;; SYNOPSIS
(defparameter +pitch-seq-lowest-equals-prefers-high+ 5)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Tue Aug  7 17:19:23 BST 2012: Added robodoc entry

;;; ****P* pitch-seq/ +pitch-seq-lowest-equals-prefers-low+
;;; DESCRIPTION
;;; A slippery-chicken constant variable used to indicate the lowest number in
;;; a pitch-seq that would indicate that the get-notes algorithm should select
;;; the lowest notes possible for the instrument/set. If not otherwise
;;; specified, this value defaults to 1.
;;; 
;;; ARGUMENTS
;;; - An integer that is the lowest number in a pitch-seq that would indicate
;;;   that the given instrument prefers high notes for that sequence.
;;; 
;;; SYNOPSIS
(defparameter +pitch-seq-lowest-equals-prefers-low+ 1)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****P* globals/+cmn-display-auto-open+
;;; DESCRIPTION
;;; Whether to automatically open EPS files generated with CMN via cmn-display.
;;; Currently only works with SBCL on Mac OSX.
;;; 
;;; ARGUMENTS
;;; T or NIL
;;; 
;;; SYNOPSIS
(defparameter +cmn-display-auto-open+ #+sbcl T #-sbcl nil)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****P* globals/+lp-display-auto-open+
;;; DESCRIPTION
;;; Whether to automatically open PDF files generated with via lp-display.
;;; Currently only works with SBCL on Mac OSX.
;;; 
;;; ARGUMENTS
;;; T or NIL
;;; 
;;; SYNOPSIS
(defparameter +lp-display-auto-open+ #+sbcl T #-sbcl nil)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****P* globals/+midi-play-auto-open+
;;; DESCRIPTION
;;; Whether to automatically open MIDI files generated with via midi-play.
;;; Currently only works with SBCL on Mac OSX.
;;; 
;;; ARGUMENTS
;;; T or NIL
;;; 
;;; SYNOPSIS
(defparameter +midi-play-auto-open+ #+sbcl T #-sbcl nil)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****P* globals/+sc-default-dir+
;;; DESCRIPTION
;;; The default directory for output of sound files, EPS files, and Lilypond
;;; files. Don't forget the trailing slash (i.e. "/tmp/" not "/tmp").  Bear in
;;; mind that on OSX the /tmp directory is emptied upon reboot so you shouldn't
;;; store any files you'd like to keep in there.
;;;
;;; ARGUMENTS
;;; T or NIL
;;; 
;;; SYNOPSIS
(defparameter +sc-default-dir+ "/tmp/")
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****P* globals/+lilypond-command+ 
;;; DESCRIPTION
;;; The full path to the lilypond command.  We need to set this if we'll call
;;; lp-display, i.e. if we want to automatically call Lilypond and open the
;;; resultant PDF directly from Lisp.  The default should work if you have the
;;; Lilypond app in your Applications folder on OSX.
;;;
;;; ARGUMENTS
;;; T or NIL
;;; 
;;; SYNOPSIS
(defparameter +lilypond-command+ 
  "/Applications/LilyPond.app/Contents/Resources/bin/lilypond")
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF globals.lsp
