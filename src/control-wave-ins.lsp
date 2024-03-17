;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/control-wave-ins
;;; NAME
;;; control-wave
;;;
;;; File:             control-wave-ins.lsp      
;;;
;;; Class Hierarchy:  no classes defined
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          The creation of waveforms such as sine, cosine, sawtooth,
;;;                   triangle, square, and pulse train, for the control of
;;;                   musical parameters. See
;;;                   http://michael-edwards.org/wp/?p=977 for context and
;;;                   example code.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 26th 2020, Heidhausen, Germany
;;;                   (taken out of control-wav.lsp)
;;;
;;; $$ Last modified:  01:45:30 Sun Mar 17 2024 CET
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clm)

(definstrument ctlwav
    ;; frequency can also be an envelope
    (frequency duration &key
               ;; (make-fun #'make-oscil)
               ;; (gen-fun #'oscil)
               (time 0.0)
               ;; case in the run loop can't handle symbols hence integers here:
               ;; 1=sine, 2=cosine, 3=sawtooth, 4=triangle, 5=square,
               ;; 6=pulse(single sample of 1.0 follwed by zeros)
               (type 1)
               (initial-phase 0.0)      ; in radians
               (amplitude 1.0)
               (rescale nil) ; or '(minimum maximum)
               (amp-env '(0 1 100 1)))
  ;; for ease and at the expense of a little computation force a freq-env
  (unless (listp frequency)
    (setf frequency (list 0 frequency 100 frequency)))
  (let* ((beg (floor (* time *srate*)))
         ;; 1+ so we can access the value at <duration> if necessary...it's
         ;; just one (or two max.) extra sample 
         (dur-samps (1+ (ceiling (* duration *srate*))))
         (end (+ beg dur-samps))
         (samples (make-double-float-array dur-samps))
         (samp 0.0)
         (rsamp 0.0)
         (indx 0)
         (prop 0.0)
         (fm 0.0)
         (start-freq (second frequency))
         (fenv (make-env :envelope frequency :duration duration
                         :offset (- start-freq)))
         ;; square and pulse go from 0 to 1, the rest -1 to 1
         (wav-min (if (member type '(5 6)) 0.0 -1.0))
         (wav-range (if (zerop wav-min) 1 2))
         (out-min (when rescale (first rescale)))
         (out-max (when rescale (second rescale)))
         (out-range (when rescale (- out-max out-min)))
         (wav (funcall (case type
                         (1 #'make-oscil)
                         (2 #'make-oscil)
                         (3 #'make-sawtooth-wave)
                         (4 #'make-triangle-wave)
                         (5 #'make-square-wave)
                         (6 #'make-pulse-train)
                         (t (error "ctlwav: unknown wave type: ~a" type)))
                       :frequency start-freq :initial-phase initial-phase))
         (ampf (make-env :envelope amp-env :scaler amplitude
                         :duration duration)))
    (when (= type 2)                     ; cosine
      (setf (mus-phase wav) (mod (+ initial-phase (/ pi 2)) pi)))
    ;; save some computation in the run loop if we're not actually rescaling.
    (when (and (= wav-min out-min)
               (= out-max 1.0))
      (setf rescale nil))
    (run* (samples)
          (loop for i from beg to end do
               (setf fm (hz->radians (env fenv))
                     samp (* (env ampf)
                             ;; can't use funcall in run though apply should
                             ;; work (but doesn't for me here)
                             (case type
                               (1 (oscil wav fm))
                               (2 (oscil wav fm))
                               (3 (sawtooth-wave wav fm))
                               (4 (triangle-wave wav fm))
                               (5 (square-wave wav fm))
                               (6 (pulse-train wav fm))))
                     rsamp samp)
               (when rescale
                 (setf prop (/ (- samp wav-min) wav-range)
                       rsamp (+ out-min (* prop out-range))))
               (setf (aref samples indx) rsamp)
               (incf indx)
               (outa i samp)))
    samples))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF control-wave-ins.lsp
