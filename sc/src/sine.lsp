;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/sine
;;; NAME 
;;; samp5
;;;
;;; File:             sine.lsp
;;;
;;; Class Hierarchy:  none, no classes defined
;;;
;;; Version:          1.0.4
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          clm instrument for simple sine wave generation.  This is
;;;                   used mainly as an example to show how user instruments
;;;                   can be used in clm-play but it may also be useful for a
;;;                   quick sinewave rendition of a piece.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    12th June 2004
;;;
;;; $$ Last modified: 11:23:12 Tue Dec  3 2013 GMT
;;;
;;; SVN ID: $Id: samp5.lsp 4223 2013-10-29 10:57:09Z medward2 $
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

(in-package :clm)

;;; The instrument defines all the parameters that will be used in clm-play but
;;; ignores some of them as they're not relevant to sinewave playback.
(definstrument sine
    (ignore time &key       
            (duration 5)
            (frequency 440)
            start                       ; ignored
            srt                         ; ignored
            width                       ; ignored
            (amp .6)
            printing
            (amp-env '(0 0 5 1 95 1 100 0))
            (amp-env-base 2)
            ;; in addition to amp, which is set by clm-play:
            (amp-env-scaler 1.0) 
            ;; scale amplitude of frequency according to a-weighting loudness
            ;; compensation? (needs routines from utilities.lsp):
            a-weighting
            (degree 45)
            (distance 0.0)
            unused-arg-for-testing      ; ignored
            (rev-amt 0.0))
  (let* ((beg (floor (* time *srate*)))
         (end (+ beg (floor (* duration *srate*))))
         (sinewave (make-oscil :frequency frequency))
         (amps (* amp amp-env-scaler))
         (ampw (if a-weighting 
                   ;; MDE Tue Dec 3 10:26:43 2013 -- a-weighting now returns
                   ;; linear (not db) scalers by default.
                   (* amps (sc::a-weighting frequency))
                   amps))
         (envelope (make-env :envelope amp-env :scaler ampw :base amp-env-base
                             :duration duration))
         (loc (make-locsig :degree degree :distance distance 
                           :reverb rev-amt)))
    (run (loop for i from beg to end do
              (clm-print i)
              (locsig loc i (* (env envelope) (oscil sinewave)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sine.lsp
