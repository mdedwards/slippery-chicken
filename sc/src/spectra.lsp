;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/spectra
;;; NAME 
;;; spectra
;;;
;;; File:             spectra.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          1.0.5
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of the spectral data for piano and other
;;;                   instruments' notes. The piano data is taken from CLM's
;;;                   spectr.clm which itself was created from data
;;;                   provided courtesy of J.A. Moorer. We'll use this data for
;;;                   the calculation of chord dissonance values in the chord
;;;                   class's calculate-dissonance method. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    July 27th 2015
;;;
;;; $$ Last modified: 15:38:13 Wed Aug  5 2015 BST
;;;
;;; SVN ID: $Id: spectra.lsp 5359 2015-07-24 20:53:22Z medward2 $
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

(in-package :slippery-chicken)                

(defun src-file-path (file)
  (concatenate 'string cl-user::+slippery-chicken-src-path+
               file))

(defun read-from-src-file (file)
  (read-from-file (src-file-path file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; actual data is stored in separate files and read in here.
(defun clm-piano-spectra ()
  (let ((spectr (read-from-src-file "clm-piano-spectra.lsp")))
    ;; What we end up with here is an association list with the MIDI note
    ;; number as key and data consisting of two lists, the first being the
    ;; frequency scalars of the partials, the second being the normalised
    ;; amplitudes of these partials.
    (make-assoc-list
     'piano-spectrum
     (loop for note in spectr with max-partials = 12 collect
          (list (midi-note (make-pitch (first note)))
                (let* ((freq-scalers '())
                       (amps (loop for freq-scaler in (second note) by #'cddr
                                for amp in (rest (second note)) by #'cddr
                                do (push freq-scaler freq-scalers)
                                collect amp)) ;(coerce amp 'double-float)))
                       (len (length amps)))
                  ;; we always want <max-partials> amplitudes exactly
                  (if (> len max-partials)
                      (setq amps (subseq amps 0 max-partials))
                      (when (< len max-partials)
                        (setq amps (append amps
                                           (ml 0.0 (- max-partials len))))))
                  (list (nreverse freq-scalers) (normalise amps))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Aug  5 13:47:41 2015 -- roll your own: analyse a directory of
;;; samples.

(defun process-spectra (spectra fundamental &key (force-harmonics t)
                                              (harmonic-tolerance 0.15)
                                              (num-partials 12))
  (let* ((pairs (loop for freq in (first spectra)
                   for fscaler = (/ freq fundamental)
                   for amp in (second spectra)
                   when (and (> freq (* (- 1 harmonic-tolerance) fundamental))
                             (or (not force-harmonics)
                                 (and force-harmonics
                                      (float-int-p fscaler 0.1))))
                   collect (list fscaler amp)))
         (result (ml nil num-partials)))
    ;;     (sort pairs #'(lambda (x y) (< (first x) (first y))))))
    (loop for pair in pairs
       for fscaler = (first pair)
       for partial = (1- (round fscaler))
       for existing = (nth partial result)
       do
         ;; (print fscaler)
         (when (and (< partial num-partials)
                    (or (not existing)
                        ;; replace the existing partial if we have a closer
                        ;; match 
                        (and existing (< (rem fscaler 1)
                                         (rem (first existing) 1)))))
           (setf (nth partial result) pair)))
    (loop for p in result when p collect p)))

(defun get-spectra-al (sample-dir name-fun &key (force-harmonics t)
                                             (harmonic-tolerance 0.15)
                                             (num-partials 12))
  (unless (fboundp 'clm::spec-an)
    (load (compile-file (src-file-path "get-spectrum.lsp"))))
  (make-assoc-list
   'spectra
   (loop for file in (directory sample-dir)
      for name = (pathname-name file)
      for midi-note = (funcall name-fun name)
      when midi-note collect
        (list midi-note
              (process-spectra
               (loop for start from 300 by 100 repeat 3 collect
                    (multiple-value-bind
                          (freqs amps)
                        (clm::get-spectrum
                         file :order-by 'clm::amp :num-partials 100
                         :fftsize 2048 :start-analysis (/ start 1000.0))
                      (list freqs amps)))
               (midi-to-freq midi-note)
               :force-harmonics force-harmonics :num-partials num-partials
               :harmonic-tolerance harmonic-tolerance)))))         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF spectra.lsp
