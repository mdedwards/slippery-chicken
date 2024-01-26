;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/get-spectrum
;;; NAME 
;;; get-spectrum
;;;
;;; File:             get-spectrum.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Used mainly by spectra.lsp, get-spectrum returns, as a
;;;                   list, the most prominent frequencies in a sound file.
;;;                   The list is ordered from the most prominent to the least
;;;                   prominent frequency (:order-by 'amp) or the highest to
;;;                   lowest frequency (:order-by 'freq) and the number of
;;;                   elements in the list is set by the <num-partials>
;;;                   parameter.  The instrument spec-an is called by
;;;                   get-spectrum and is therefore not meant to be used as a
;;;                   public function (it is written as an instrument to take
;;;                   advantage of CLM's run-loop efficiency).  The results of
;;;                   analysis are stored in
;;;                   *slippery-chicken-get-spectrum-last-result* so that if
;;;                   you request the same analysis repeatedly, the analysis
;;;                   will not need to be reperformed each time.  Be careful
;;;                   though, only the input file, the ordering, the number of
;;;                   partials requested and the analysis start time are used
;;;                   to detect whether analysis has to be reperformed or not.
;;;                   If you want to force reanalysis, do (setf
;;;                   *slippery-chicken-get-spectrum-last-result* nil) before
;;;                   the call to get-spectrum.
;;; 
;;; Creation date:    This is very old code but it was added to slippery
;;;                   chicken on August 5th 2015
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; $$ Last modified:  11:23:34 Tue Jan 16 2024 CET
;;;
;;; SVN ID: $Id: get-spectrum.lsp 5359 2015-07-24 20:53:22Z medward2 $
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
;;; NB: If you try and call this from within an instrument, it will only work
;;; every other time because CLM doesn't allow an instrument call within an
;;; instrument call. (It works every other time because the second time the
;;; spec-an instrument doesn't actually get called--the data is cached the
;;; first time and returned the second.)

(in-package :clm)

(defvar *slippery-chicken-get-spectrum-peak-freqs* 0)
(defvar *slippery-chicken-get-spectrum-peak-amps* 0)
(defvar *slippery-chicken-get-spectrum-last-result* nil)

(defstruct mde-get-spectrum-partial freq amp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-analysis-data (sndfile
                             &key
                               (outputfile nil)
                               ;; How ofen to perform freq analysis (secs)
                               ;; or if a list then these are times to
                               ;; do the analysis at  
                               (interval 0.01)
                               ;; Where to end in the sound file, if nil,
                               ;; analyse it all
                               (end nil) 
                               (num-partials 30)
                               (srate 44100)
                               (fftsize 2048) 
                               (max-peaks 200)
                               (start-analysis 0.0) 
                               (highest-bin (/ fftsize 2)))
  (let* ((stop (or end (sound-duration sndfile)))
         (amp-array (make-array num-partials :initial-element nil))
         (freq-array (make-array num-partials :initial-element nil))
         (normalised-amps nil)
         (times (if (listp interval)
                    interval
                    (loop for start from start-analysis below stop by interval 
                       collect start)))
         (max-amp 0.0))
    ;; (print amp-array)
    (loop ;; for start from start-analysis below stop by interval do
       for start in times do
         (format t "~&Analysing ~a at time ~a" sndfile start)
         (multiple-value-bind (freqs amps)
             (get-spectrum sndfile 
                           :start-analysis start
                           :num-partials num-partials
                           :order-by 'freq
                           :srate srate
                           :fftsize fftsize
                           :normalise nil
                           :max-peaks max-peaks
                           :highest-bin highest-bin)
           (loop for f in freqs and a in amps and i from 0 do 
                (push start (aref freq-array i))
                (push f (aref freq-array i))
                (push start (aref amp-array i))
                (push a (aref amp-array i))
                (when (> a max-amp) (setf max-amp a)))))
    ;; (print amp-array) (print 'here) (print normalised-amps) 
    (loop for i below num-partials do
       ;; max-amp now holds the maximum amp we found in the whole analysis
       ;; over all the partials so now we can normalise.  N.B. At this point
       ;; the amps are in reverse order, with time the second value in each
       ;; amp/time pair.
         (setf normalised-amps (if (zerop max-amp)
                                   (aref amp-array i)
                                   (loop for a in (aref amp-array i)
                                      and time in (cdr (aref amp-array i)) 
                                      by #'cddr collect (/ a max-amp) 
                                      collect time))
               (aref amp-array i) (nreverse normalised-amps)
               (aref freq-array i) (nreverse (aref freq-array i))))
    (when outputfile 
      (format t "~&Writing data file '~a'" outputfile)
      (create-analysis-data-file outputfile freq-array amp-array))
    (values freq-array amp-array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-analysis-data-file (outputfile freq-array amp-array)
  (with-open-file 
      (stream outputfile :direction :output :if-does-not-exist :create 
       :if-exists :supersede)
    (format stream "(~a~%~%" freq-array)
    (format stream "~a)~%~%" amp-array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start-analysis is where in the snd to start freq analysis (in secs)
;; num-partials is the number of partials to return.  order-by is whether to
;; order the list by frequency ('freq => highest to lowest) or amplitude ('amp
;; => highest to lowest) srate is the sampling-rate of the input file.

(defun get-spectrum
    (file &key
            (num-partials 10)
            (order-by 'amp)
            (freq-min 20.0) (freq-max 100000.0)
            (srate 44100)
            (fftsize 4096) 
            (max-peaks 200) ; passed to spec-an instrument
            (normalise t)
            (start-analysis 0.0) 
            (highest-bin (/ fftsize 2))
            ;; DJR Mon 13 Jan 2020 14:34:06 GMT
            ;; Force new analysis?
            (perform-new-analysis? nil))
  (declare (special *slippery-chicken-get-spectrum-last-result*))
  (declare (special *slippery-chicken-get-spectrum-peak-amps*))
  (declare (special *slippery-chicken-get-spectrum-peak-freqs*))
  ;; Make sure the argument to order-by is acceptable.
  (when (not (or (eq order-by 'freq) (eq order-by 'amp)))
    (error 
     "get-spectrum: Argument to :order-by must be either 'freq or 'amp."))
  ;; Test to see if we just called this function with the same file and
  ;; start-analysis values.  If so, return the last result, if not, perform the
  ;; analysis.
  (if (and *slippery-chicken-get-spectrum-last-result*
           (and
            (stringp (third *slippery-chicken-get-spectrum-last-result*))
            (string-equal (third *slippery-chicken-get-spectrum-last-result*)
                              file))
           (= (fourth *slippery-chicken-get-spectrum-last-result*)
              start-analysis)
           (eq (fifth *slippery-chicken-get-spectrum-last-result*) order-by)
           (= (sixth *slippery-chicken-get-spectrum-last-result*) num-partials)
           (null perform-new-analysis?))
      (progn
        (print "Using previous analysis")
        (values (first *slippery-chicken-get-spectrum-last-result*)
                (second *slippery-chicken-get-spectrum-last-result*)))
      ;; Here beginneth the analysis.
      (progn
        ;; Any old bs so we can fill it later (we can't (setf (first nil) x))
        (setf *slippery-chicken-get-spectrum-last-result* '(1 2 3 4 5 6)) 
        ;; Call the spec-an instrument to get our data stored in
        ;; *slippery-chicken-get-spectrum-peak-freqs/amps*
        (with-sound (:srate srate :play nil :statistics nil
                            :output "/tmp/ignore.wav") 
          (spec-an file 
                   :fftsize fftsize 
                   :max-peaks max-peaks 
                   :start-analysis start-analysis
                   :highest-bin highest-bin))
        (let ((max-amp 0.0)
              (amp 0.0)
              (got-partials 0)
              (tmp nil)
              (ordered nil)
              (freqs nil)
              (amps nil)
              (freqs-amps '()))
          ;; If we're going to normalise the amps to 1.0, get the max amp now.
          (if normalise
              (loop for i from 0 below max-peaks do
                   (setf amp (aref *slippery-chicken-get-spectrum-peak-amps* i))
                   (if (> amp max-amp)
                       (setf max-amp amp)))
              (setf max-amp 1.0))
          (when (zerop max-amp)
            (error "get-spectrum: at ~f max-amp is 0!" start-analysis))
          ;; Make the *slippery-chicken-get-spectrum-partal* structures and
          ;; store them in the freqs-amps list.
          (loop for i from 0 below max-peaks
             ;; MDE Thu Jul 19 10:34:17 2018
             for f = (aref *slippery-chicken-get-spectrum-peak-freqs* i) 
             do
               (when (and (>= f freq-min) (<= f freq-max))
                 (setf tmp (make-mde-get-spectrum-partial 
                            :freq f
                            :amp (/ (aref
                                     *slippery-chicken-get-spectrum-peak-amps*
                                     i) 
                                    max-amp)))
                 (push tmp freqs-amps)))
          ;; First sort the list from highest to lowest amp and get the loudest
          ;; <num-partials> elements.
           (setf got-partials (min (length freqs-amps) num-partials)
                 ordered (subseq 
                         (sort (copy-list freqs-amps)
                               #'(lambda (x y)
                                   (> (mde-get-spectrum-partial-amp x)
                                      (mde-get-spectrum-partial-amp y))))
                         0 got-partials)
                ordered (if (eq order-by 'amp)
                            ordered
                            (sort (copy-list ordered)
                                  #'(lambda (x y)
                                      ;; order from lowest to highest freq
                                      (< (mde-get-spectrum-partial-freq x)
                                         (mde-get-spectrum-partial-freq y)))))
                ;; Now just get the freqs.
                freqs (loop for i from 0 below got-partials collect 
                           (mde-get-spectrum-partial-freq (nth i ordered)))
                ;; and now the amps
                amps (loop for i from 0 below got-partials collect 
                          (mde-get-spectrum-partial-amp (nth i ordered))))
          ;; Store the results of the analysis.
          (setf (first *slippery-chicken-get-spectrum-last-result*) freqs
                (second *slippery-chicken-get-spectrum-last-result*) amps
                (third *slippery-chicken-get-spectrum-last-result*) file
                (fourth *slippery-chicken-get-spectrum-last-result*)
                start-analysis
                (fifth *slippery-chicken-get-spectrum-last-result*) order-by
                (sixth *slippery-chicken-get-spectrum-last-result*)
                got-partials)
          (values freqs amps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Hacked code from Bill's san.ins 
(defscins spec-an
    (file &key (fftsize 4096) (max-peaks 200)
          (printing nil) (start-analysis 0.0) (highest-bin (/ fftsize 2)))
  (let* ((fil (open-input* file))
         (fdr (make-double-float-array fftsize))
         (fdi (make-double-float-array fftsize))
         (fftamps (make-double-array fftsize))
         (fft-mag (float (/ *srate* fftsize)))
         (lowest-magnitude .001)
         (start-analysis-sample (floor (* start-analysis *srate*)))
         (rd (make-readin fil :start start-analysis-sample))
         (peak-amps (make-double-float-array max-peaks :initial-element 0.0))
         (peak-freqs (make-double-float-array max-peaks :initial-element 0.0))
         (peaks 0))
    ;;(declare (special *slippery-chicken-get-spectrum-peak-amps*))
    ;;(declare (special *slippery-chicken-get-spectrum-peak-freqs*))
    (setf *slippery-chicken-get-spectrum-peak-amps* 
      (make-double-float-array max-peaks :initial-element 0.0)
      *slippery-chicken-get-spectrum-peak-freqs* 
      (make-double-float-array max-peaks :initial-element 0.0))
    (when printing
      (format t "~&start: ~a, start-analysis-sample: ~a" 
              start-analysis start-analysis-sample))
    (run* 
     (*slippery-chicken-get-spectrum-peak-freqs*
      *slippery-chicken-get-spectrum-peak-amps*)
     (progn
       (dotimes (k fftsize)
         (setf (aref fdr k) (readin rd)))
       ;; (clm-print "first sample is ~%~f" (aref fdr 0))
       (fft fdr fdi fftsize 1)
       (dotimes (k highest-bin)
         (let ((x (aref fdr k))
               (y (aref fdi k)))
           (setf (aref fftamps k) 
             (* 2 (sqrt (+ (* x x) (* y y)))))))  
       (dotimes (k max-peaks)
         (setf (aref peak-amps k) 0.0))
       (let ((ra (aref fftamps 0))
             (la 0.0)
             (ca 0.0))
         ;; search for current peaks following Xavier Serra's
         ;; recommendations in "A System for Sound
         ;; Analysis/Transformation/Synthesis Based on a Deterministic
         ;; Plus Stochastic Decomposition"
         (setf peaks 0)                 ;how many peaks found so far
         (dotimes (k highest-bin)
           (setf la ca)
           (setf ca ra)
           (setf ra (aref fftamps k))
           (if (and (> ca lowest-magnitude)
                    (> ca ra)
                    (> ca la))
               ;; found a local maximum above the current
               ;; threshold (its bin number is k-1)
               (let* ((logla (log la 10))
                      (logca (log ca 10)) 
                      (logra (log ra 10))
                      (offset (/ (* .5 (- logla logra)) 
                                 (+ logla (* -2 logca) logra)))
                      (amp (expt 10.0 
                                 (- logca (* .25 (- logla logra) 
                                             offset))))
                      (freq (* fft-mag (+ k offset -1))))
                 (if (= peaks max-peaks)
                     ;; gotta either flush this peak, or find
                     ;; current lowest and flush that
                     (let ((minp 0)
                           (minpeak (aref peak-amps 0))) 
                       (loop for j from 1 below max-peaks do 
                             (when (< (aref peak-amps j)
                                      minpeak) 
                               (setf minp j)
                               (setf minpeak
                                 (aref peak-amps
                                       j))))
                       (when (> amp minpeak)
                         (setf (aref peak-freqs minp)
                           freq)  
                         (setf (aref peak-amps minp)
                           amp))) 
                   (progn
                     (setf (aref peak-freqs peaks) freq) 
                     (setf (aref peak-amps peaks) amp)
                     (setf (aref *slippery-chicken-get-spectrum-peak-freqs*
                                 peaks)
                       freq) 
                     (setf (aref *slippery-chicken-get-spectrum-peak-amps*
                                 peaks) amp)
                     (when printing
                       (clm-print "freq = ~f amp = ~f~&" 
                                  (aref
                                   *slippery-chicken-get-spectrum-peak-freqs*
                                   peaks)
                                  amp))
                     (incf peaks)))))))))
    (close-input fil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;; some examples:

(create-analysis-data 
 "/music/limine/nuendo/zkm-compressed-reverb-32-16-mono.wav"
 :outputfile "/user/michael/mus/limine/fft-data.lsp" 
 :start-analysis 0
 :end 30
 :interval 1
 :srate 32000
 :num-partials 15)

(read-from-file "/user/michael/mus/limine/fft-data.lsp")
(read-from-file 
 "/music/limine/nuendo/zkm-compressed-reverb-32-16-mono-segment-min.txt")

(get-spectrum
 "/music/limine/nuendo/zkm-compressed-reverb-32-16-mono.wav"
 :start-analysis 2.3
 :order-by 'freq
 :srate 32000
 :num-partials 8)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF get-spectrum.lsp
