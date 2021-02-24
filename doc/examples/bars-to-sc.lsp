;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             bars-to-sc.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          An example to show how events can be made independently of
;;;                   make-slippery-chicken but nevertheless 'stuffed' into bars
;;;                   and then a slippery-chicken object in order to take
;;;                   advantage of that class's functionality.
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    December 11th 2017, Essen
;;;
;;; $$ Last modified:  15:34:36 Mon Dec 11 2017 CET
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
(in-package :sc)
(in-scale :chromatic)

;;              we'll just select random notes from this set
(let* ((chord '(c1 a1 ds2 cs3 g3 d4 f4 bf4 e5 b5 gs6 fs7))
       (chord-len (length chord))
       ;; generate enough 32nd-note events to fill 16 4/4 bars
       (events (loop repeat (* 16 32) collect
                    (make-event (nth (random chord-len) chord) 32)))
       (bars '())
       (ate 0)
       bar
       sc)
  (loop while events do
     (setq bar (make-rthm-seq-bar '((4 4))) ; make an empty bar
           ;; fill the bar with the events we made. This method will stop once
           ;; the bar is full and will return the number of rhythms/events it
           ;; 'ate'.    
           ate (fill-with-rhythms bar events)
           ;; ate should always be 32 but best to check
           events (when ate (nthcdr ate events)))
       ;; we could reverse this after the loop if order was important
       (push bar bars))
  ;; automatically create a slippery-chicken object with the bars we made
  (setq sc (bars-to-sc bars))
  ;; test midi-output
  (midi-play sc)
  ;; test Lilypond output. We could call auto-beam and/or auto-clefs here also
  (lp-display sc)
  sc)
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
