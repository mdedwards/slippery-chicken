;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             instruments-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany instruments.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 20:19:04 Thu Oct 17 2013 BST
;;;
;;; SVN ID: $Id: instruments-examples.lsp 4196 2013-10-18 08:39:48Z medward2 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2012 Michael Edwards
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

(in-scale :quarter-tone)

;;; ============================================================================
;;; instruments.html examples
;;; ============================================================================

;;;  defining a new instrument within an instrument-palette definition
;;; ----------------------------------------------------------------------------
(let* ((inst-pal (make-instrument-palette 
                  'test-inst-pal
                  '((soprano
                     (:staff-name "soprano" :staff-short-name "s"
                      :lowest-written c4 :highest-written c6
                      :starting-clef treble
                      :midi-program 54))))))
  (print (get-data 'soprano inst-pal)))

;;;  creating a separate instrument palette
;;; ----------------------------------------------------------------------------
(progn
  (defparameter +pl-inst-pal+
    (make-instrument-palette
     'plucked-strings-aux-instrument-palette
     '((mandolin
        (:staff-name "mandolin" :staff-short-name "md"
         :lowest-written g3 :highest-written a6 :starting-clef treble
         :chords t :microtones nil :missing-notes nil 
         :midi-program 26))
       (tenor-banjo
        (:staff-name "tenor banjo" :staff-short-name "t-bj"
         :lowest-written c3 :highest-written a4 :starting-clef treble
         :transposition-semitones -12
         :chords t :microtones nil :missing-notes nil 
         :midi-program 106)))))
  (print +pl-inst-pal+))

;;;  adding a single new instrument object to an existing palette
;;; ----------------------------------------------------------------------------
(let* ((scsip-clone 
        (clone +slippery-chicken-standard-instrument-palette+)))
  (add
   (make-instrument 'tenor-banjo
                    :staff-name "tenor banjo" :staff-short-name "t-bj"
         :lowest-written 'c3 :highest-written 'a4 :starting-clef 'treble
         :transposition-semitones -12
         :chords t :microtones nil :missing-notes nil 
         :midi-program 106)
   scsip-clone)
  (print scsip-clone))

;;;  changing one slot of one instrument object in an existing palette
;;; ----------------------------------------------------------------------------
(progn
  (defparameter +ip5+
    (make-instrument-palette
     'ip5
     '((piccolo
        (:staff-name "piccolo" :lowest-written d4 :highest-written c7 
         :chords nil
         :staff-short-name "picc"
         :missing-notes nil :midi-program 73 :starting-clef treble 
         :transposition-semitones 12 :microtones t))
       (flute 
        (:staff-name "flute" :lowest-written c4 :highest-written d7 
         :chords nil 
         :missing-notes (cqs4 dqf4) :midi-program 74 
         :starting-clef treble
         :staff-short-name "fl" :microtones t)))))
  (set-slot 'staff-name "kleine floete" 'piccolo +ip5+)
  (set-slot 'staff-short-name "kl.fl." 'piccolo +ip5+)
  (print (get-data 'piccolo +ip5+)))