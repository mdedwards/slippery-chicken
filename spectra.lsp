;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/spectra
;;; NAME 
;;; spectra
;;;
;;; File:             spectra.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          1.0.10
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of the spectral data for piano and other
;;;                   instruments' notes. We'll use this data for
;;;                   the calculation of chord dissonance values in the chord
;;;                   class's calculate-dissonance method. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    July 27th 2015
;;;
;;; $$ Last modified: 12:27:57 Tue Feb  2 2016 GMT
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Aug  5 13:47:41 2015 -- roll your own: analyse a directory of
;;; samples.

;;; ****f* spectra/get-spectra-al
;;; DATE
;;; August 6th 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Create an assoc-list of spectral data by doing spectral analysis of all the
;;; sound files contained in a given directory. The aim is to extract the
;;; spectral data for harmonic partials, so we specifically look for ascending
;;; partials as close as possible to integer multiples of the sample sound
;;; file's fundamental frequency, as detected from the file name (see below).
;;;
;;; By default we average spectra over three separate readings at 300, 400, and
;;; 500 milliseconds. We do this in order to avoid spectral anomalies at any
;;; fixed point in any of the sound files, and of course to skip the onset
;;; transient and any inharmonicities therein.
;;;
;;; As the extensive analysis and processing here will take some computation
;;; time, in order to create our default spectral data for calculating a
;;; chord's dissonance or spectral centroid, I've performed the analysis on
;;; some of my local sample libraries. By then using the assoc-list method
;;; print-for-init I've copied the data into separate text files for reading
;;; into the +slippery-chicken-spectra+ global assoc-list and thus making these
;;; available to all slippery-chicken users. By modifying the examples at the
;;; bottom of this file, similar sample directories' spectral data could be
;;; added to this global.
;;; 
;;; ARGUMENTS
;;; - the path to the samples directory. This should be something suitable for
;;; passing to the (directory) function so we'll need the extension of the sound
;;; files (see "*.wav" in the example below).
;;; - a function for processing the filenames (as a string, with no directory
;;; or extension e.g. "Stein-R(A0)-V(085)-dB(3478)-T(ML)-M(28)-P(2126-03-01)")
;;; in order to return the MIDI note number of the sample in that file. This
;;; whole approach is predicated on the assumption that sample file names will
;;; contain such pitch data. If that is not the case then get-spectra-al will
;;; be of no use. See the akoustik-piano-name and violin-ensemble-name
;;; functions below for examples.
;;; 
;;; OPTIONAL ARGUMENTS
;;;  Keyword arguments:
;;; - :force-harmonics: whether to look for harmonic partials or not. If this
;;;    is nil then the returned spectra will be the strongest by amplitude
;;;    irrespective of their harmonicity. Default = t
;;; - :harmonic-tolerance: the maximum deviation from the exact integer
;;;    partial. Default = 0.15 
;;; - :fftsize: The FFT window size for spectral analysis. Default = 4096
;;; - :num-partials: the number of partials to identify and return.
;;;    Default = 12
;;; - :id: The default ID for the resultant assoc-list. Default = 'spectra
;;; - :analysis-points: The times in milliseconds at which to do the spectral
;;;    analysis and extract averages. If no averaging is necessary simply pass
;;;    one starting value as a list. Default = '(300 400 500)
;;; 
;;; RETURN VALUE
;;; An assoc-list object whose keys are the MIDI note numbers associated with
;;; the spectral information of sample sound files. Each of these is in the
;;; form of a two element list: a list of fundamental scalers and a list of
;;; their amplitudes (normalised to 1.0).
;;; 
;;; EXAMPLE
#|

(get-spectra-al "/path/to/samples/*.wav"
                #'violin-ensemble-name :id 'violin-ensemble)
-->
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 21, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: VIOLIN-ENSEMBLE, tag: NIL, 
data: (
NAMED-OBJECT: id: 56, tag: NIL, 
data: ((0.9889743194880222d0 2.0263850081954717d0 2.9990610385449314d0
        4.024036707283989d0 5.032546599880615d0 6.013292183763263d0
        7.0551329128964655d0 8.049590274637817d0 9.032029362954297d0
        10.017427921485291d0 11.0461040956976d0 12.028523416637086d0)
       (0.1748299156810844d0 0.6666103041159978d0 0.740572572283973d0
        0.37090168062541756d0 0.2529382535743205d0 0.19313279338531672d0
        0.290088638695093d0 0.20008736984066355d0 0.12280861470679348d0
        0.15534581915388804d0 0.11772253740784348d0 0.11703054477270619d0))
...

|#
;;; SYNOPSIS
#+clm
(defun get-spectra-al (sample-dir name-fun
                       &key (force-harmonics t) (harmonic-tolerance 0.15)
                         (fftsize 4096) (num-partials 12) (id 'spectra)
                         ;; millisec times of fft analysis
                         (analysis-points '(300 400 500)))
;;; ****
  (unless (fboundp 'clm::spec-an)
    (load (compile-file (src-file-path "get-spectrum.lsp"))))
  (make-assoc-list
   id
   (sort 
    (loop for file in (directory sample-dir)
       for name = (pathname-name file)
       for midi-note = (funcall name-fun name)
       for avs = (when midi-note
                   (average-spectra
                    ;; average spectra at separate points in the sound file
                    (loop for start in analysis-points collect
                         (process-spectra
                          (multiple-value-bind
                                (freqs amps)
                              (clm::get-spectrum
                               file :order-by 'clm::amp :num-partials 100
                               :fftsize fftsize
                               :start-analysis (/ start 1000.0))
                            (list freqs amps))
                          (midi-to-freq midi-note)
                          :force-harmonics force-harmonics
                          :num-partials num-partials
                          :harmonic-tolerance harmonic-tolerance))
                    num-partials))
       when avs collect (list midi-note avs))
    #'(lambda (x y) (< (first x) (first y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Not to be confused with chord.lsp's average-spectrum (which works over
;;; notes in an octave): here we average the spectra obtained from a single
;;; sound file at several start times. Each sublist contains a list of
;;; frequency scaler - amps pairs: these will be in the same order i.e. partial
;;; 1, partial 2...
(defun average-spectra (spectra num-partials)
  (let* ((divs (ml 0 num-partials))
         (freqs (ml 0.0 num-partials))
         (amps (ml 0.0 num-partials)))
    (loop for s in spectra do
         (loop for partial in s for f = (first partial)
            for a = (second partial) for i from 0 do
            ;; if we got a partial we've got both freq and amp
              (when partial
                ;; we need to count how many times we've had this partial
                ;; (across the separate spectra) so we can calculate averages. 
                (incf (nth i divs))
                (incf (nth i freqs) f)
                (incf (nth i amps) a))))
    (loop for div in divs for i from 0 for a = (nth i amps)
       for f = (nth i freqs) do
         (unless (zerop f)
           (setf (nth i amps) (/ a div)
                 (nth i freqs) (/ f div))))
    ;;     (list freqs amps)))
    (list freqs (normalise amps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; take spectra as a list of frequencies in any order plus a list of the
;;; respective amplitudes and return a list of partial-scaler - amplitude pairs
;;; in ascending partial order.
(defun process-spectra (spectra fundamental &key (force-harmonics t)
                                              (harmonic-tolerance 0.15)
                                              (num-partials 12))
  (let* ((pairs (loop for freq in (first spectra)
                   for fscaler = (/ freq fundamental)
                   for amp in (second spectra)
                     ;; could be just less than the fundamental...
                   when (and (> freq (* (- 1 harmonic-tolerance) fundamental))
                             (or (not force-harmonics)
                                 (and force-harmonics
                                      (float-int-p fscaler
                                                   harmonic-tolerance))))
                   collect (list fscaler amp)))
         (result (ml nil num-partials)))
    (loop for pair in pairs
       for fscaler = (first pair)
       for partial = (1- (round fscaler))
       for existing = (nth partial result)
       do
         (when (and (< partial num-partials)
                    (or (not existing)
                        ;; replace the existing partial if we have a closer
                        ;; match 
                        (and existing (< (rem fscaler 1)
                                         (rem (first existing) 1)))))
           (setf (nth partial result) pair)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These functions will receive the file name as a string, with no directory
;;; or extension, and return the associated MIDI note number.
(defun akoustik-piano-name120 (name)
  (when name
    (let ((zero-octave (char= #\- (elt name 9))))
      (when (and (string= name "Stein-R(" :end1 8)
                 ;; there are various velocities recorded; this seems like piano
                 (string= name "V(120)" :start1 (if zero-octave 13 12)
                          :end1 (if zero-octave 19 18)))
        ;; samples notes use middle C = C3
        (+ (note-to-midi (read-from-string
                          (subseq name 8 (if zero-octave 11 10))))
           12)))))

(defun violin-ensemble-name (name)
  (when name
    (let* ((pos (position #\_ name :from-end t))
           (sharp (substitute #\s #\# name))
           (len (length name))
           (note (subseq sharp (1+ pos) len)))
      (note-to-midi (read-from-string note)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun src-file-path (file)
  (concatenate 'string cl-user::+slippery-chicken-src-path+
               file))

(defun read-from-src-file (file)
  (let ((result (read-from-file (src-file-path file))))
    (unless result
      (error "spectra.lsp::read-from-src-file: can't read from ~a" file))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-spectra (file
                    &optional (finaliser
                               #'(lambda (f)
                                   (let ((al (clone-with-new-class 
                                              (eval (read-from-src-file f))
                                              'recursive-assoc-list)))
                                     (verify-and-store al)
                                     al))))
  (declare (special +slippery-chicken-spectra+))
  (let ((id (read-from-string (pathname-name file))))
    (unless (get-data id +slippery-chicken-spectra+ nil)
      (add (funcall finaliser file) +slippery-chicken-spectra+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add data to our global assoc-list.
;;; +slippery-chicken-spectra+ is declared in globals.lsp
;;; Actual data is stored in separate files and read in here.

(add-spectra "akoustik-piano-spectra.lsp")
(add-spectra "violin-ensemble-spectra.lsp")
(add-spectra
 ;;  The CLM data is a little different so needs massaging.
 "clm-piano-spectra.lsp"
 #'(lambda (f) 
     (make-ral
      'clm-piano-spectra
      (loop for note in (read-from-src-file f) with max-partials = 12 collect
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
#|

;;; Examples of how the data in the files akoustik-piano-spectra.lsp and 
;;; violin-ensemble-spectra.lsp were generated.
  
(print-for-init
 (get-spectra-al "/Volumes/NIsamples/Akoustik Piano Library/Samples/ConcertGrand/Samples/*.wav" 
                 #'akoustik-piano-name120 :id 'akoustik-piano-spectra))


(print-for-init
 (get-spectra-al "/Volumes/NIsamples/Kontakt 3 Library/Orchestral/Z - Samples/01 Violin ensemble - 14/VI-14_mV_sus_mf/*.wav"
                 #'violin-ensemble-name :id 'violin-ensemble-spectra)))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF spectra.lsp
