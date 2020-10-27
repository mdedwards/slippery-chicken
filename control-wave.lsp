;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/control-wave
;;; NAME
;;; control-wave
;;;
;;; File:             control-wave.lsp      
;;;
;;; Class Hierarchy:  named-object -> control-wave
;;;
;;; Version:          1.0.11
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
;;; Creation date:    July 6th 2016, Essen Werden, Germany
;;;
;;; $$ Last modified:  08:40:56 Thu Mar 26 2020 CET
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

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Always make sure to compile then load this file (rather than just a plain
;;; load) so that the CLM instrument below is properly compiled in C etc.

;;; The sample data are in the data slot as a 1D array.

;;; Sample generation order: 1. wave form; 2. amplitude envelope (with
;;; amplitude as scaler)--sound file written at this point; 3. rescaling to
;;; minimum/maximum--data stored in an array at this point; 4. waveshaping via
;;; transfer function (done only when get-data or get-last is called).

(defclass control-wave (named-object)
  ;; over how many points/samples should a waveform period be mapped. use
  ;; either this or frequency but not both.
  ((period :accessor period :type integer :initarg :period :initform nil)
   ;; either a single Hertz (number) or an envelope of hertz values
   (frequency :accessor frequency :initarg :frequency :initform nil)
   ;; path to the sndfile we'll write of the wave; this is envisaged as being
   ;; for illustrative purposes only and so that we can view the wave in a
   ;; sound file editor. we write sample values between -1 and 1 i.e. before
   ;; offset/scaling/transfer via the minimum/maximum/transfer slots
   (sndfile :accessor sndfile :type string :initarg :sndfile
            :initform "/tmp/control-wave.wav")
   ;; control rate in Hz. this will also be the sampling rate of the generated
   ;; sound file.
   (rate :accessor rate :type integer :initarg :rate :initform 1000)
   ;; in seconds
   (duration :accessor duration :type number :initarg :duration :initform 1)
   ;; minimum value for the wave to achieve (after scaling with amp and/or
   ;; amp-env) 
   (minimum :accessor minimum :type number :initarg :minimum :initform -1)
   ;; maximum value to achieve
   (maximum :accessor maximum :type number :initarg :maximum :initform 1)
   ;; starting phase of the wave. e.g. to start at lowest point of a sine (-1)
   ;; set to (* pi 1.5)
   (initial-phase :accessor initial-phase :type number :initarg :initial-phase
                  :initform 0.0)
   ;; type of waveform to use. one of sine, cosine, sawtooth, triangle,
   ;; square, pulse (single sample of 1.0 follwed by zeros)
   (type :accessor type :type symbol :initarg :type :initform 'sine)
   ;; waveshaping transfer function. x vals from -1 to 1 (and usually y values
   ;; also) otherwise there's a strong chance we'll fail. this is only used by
   ;; the get-data and get-last methods i.e. not during the generation of the
   ;; wave (thus we can change this slot on the fly).
   (transfer :accessor transfer :type list :initarg :transfer :initform nil)
   ;; amplitude scaler for the whole waveform
   (amp :accessor amp :type number :initarg :amp :initform 1.0)
   ;; amplitude envelope to map over the whole waveform. as usual any x axis
   ;; range is fine. amp is the scaler for this env. bear in mind that this
   ;; envelope is applied before we scale to within minimum/maximum slots.
   (amp-env :accessor amp-env :type list :initarg :amp-env 
            :initform '(0 1 100 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass control-sine (control-wave)
  ((type :initform 'sine)))
(defclass control-cosine (control-wave)
  ((type :initform 'cosine)))
(defclass control-sawtooth (control-wave)
  ((type :initform 'sawtooth)))
(defclass control-triangle (control-wave)
  ((type :initform 'triangle)))
(defclass control-square (control-wave)
  ((type :initform 'square)
   (minimum :initform 0.0)))
(defclass control-pulse (control-wave)
  ((type :initform 'pulse)
   (minimum :initform 0.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((cw control-wave) &rest initargs)
  (declare (ignore initargs))
  (when (and (frequency cw) (period cw))
    (error "control-wave::init: define period or frequency but not both."))
  (unless (or (frequency cw) (period cw))
    (error "control-wave::init: a period or frequency value must be given."))
  ;; don't go above the nyquist
  (let ((nyq (/ (rate cw) 2)))
    (when (and (frequency cw)
               (or (and (numberp (frequency cw))
                        (> (frequency cw) nyq))
                   (and (listp (frequency cw))
                        (every #'(lambda (x)
                                   (> x nyq))
                               (loop for x in (frequency cw) by #'cddr
                                  collect x)))))
      (error "control-wave::init: no frequency (~a) should not be > half the ~
              rate (~a)" (frequency cw) (rate cw))))
  (setf (data cw)
        (let (array
              (types '(sine cosine sawtooth triangle square pulse)))
          (unless (member (type cw) types)
            (error "control-wave::init: unrecognised type: ~a" (type cw)))
          (clm::with-sound (:channels 1 :srate (rate cw) :output (sndfile cw)
                                      :play nil)
            (setf array
                  (clm::ctlwav (if (period cw)
                                   ;; frequency = sampling rate / period
                                   (float (/ (rate cw) (period cw)))
                                   (frequency cw))
                               (duration cw)
                               :type (1+ (position (type cw) types))
                               :amplitude (amp cw)
                               :amp-env (amp-env cw)
                               :rescale (list (minimum cw) (maximum cw))
                               :initial-phase (initial-phase cw))))
          array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((cw control-wave) stream)
  (format stream "~%CONTROL-WAVE: period: ~a, frequency: ~a, rate: ~a ~
                  ~%              sndfile: ~a ~
                  ~%              duration: ~a, minimum: ~a, maximum: ~a ~
                  ~%              initial-phase: ~a, type: ~a, transfer: ~a ~
                  ~%              amp: ~a, amp-env: ~a"
          (period cw) (frequency cw) (rate cw) (sndfile cw) (duration cw)
          (minimum cw) (maximum cw) (initial-phase cw) (type cw) (transfer cw)
          (amp cw) (amp-env cw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if <seconds> is nil then <where> is just a normal 0-based index into the
;;; data array, otherwise it's an actual time in seconds, which assumes that
;;; our duration is long enough to accommodate it.
(defmethod get-data (where (cw control-wave) &optional (seconds t))
  (declare (ignore ignore))
  (let ((y (aref (data cw) (if seconds
                               (floor (* where (rate cw)))
                               where))))
    ;; have updated interpolate to return arg1 if arg2 (envelope) is nil
    (interpolate y (transfer cw))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-last ((cw control-wave))
  (let ((y (aref (data cw) (1- (array-dimension (data cw) 0)))))
    (interpolate y (transfer cw))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-control-sine (&rest args)
  (apply #'make-instance 'control-sine args))
(defun make-control-cosine (&rest args)
  (apply #'make-instance 'control-cosine args))
(defun make-control-sawtooth (&rest args)
  (apply #'make-instance 'control-sawtooth args))
(defun make-control-triangle (&rest args)
  (apply #'make-instance 'control-triangle args))
(defun make-control-square (&rest args)
  (apply #'make-instance 'control-square args))
(defun make-control-pulse (&rest args)
  (apply #'make-instance 'control-pulse args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF control-wave.lsp
