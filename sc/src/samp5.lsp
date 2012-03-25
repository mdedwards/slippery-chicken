;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/samp5
;;; NAME 
;;; samp5
;;;
;;; File:             samp5.lsp
;;;
;;; Class Hierarchy:  none, no classes defined
;;;
;;; Version:          0.91
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          clm instrument for sample processing; called by
;;;                   slippery-chicken::clm-play 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    12th June 2004
;;;
;;; $$ Last modified: 13:20:54 Sat Mar 20 2010 GMT
;;;
;;; SVN ID: $Id$
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

;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clm)

;;; We generally set this to double-float to avoid floating point precision
;;; errors but CLM uses single-floats.  Change back at the end of this file.
(setf *read-default-float-format* 'single-float)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Places a sound in stereo space no matter whether input is stereo or mono.
;;; if degree = 0 and input is stereo, then both channels will be put in the
;;; left output channel only vice-versa when degree = 90.  Check out 
;;; get-stereo-scalers for details how the channel scaling is effected.  
;;; Mono input files are handled as per usual.
;;; If more than two channels are used then the two channels the sound
;;; is placed between will be randomly chosen.


;;; Get the channel scaler values for placing a stereo signal in
;;; a different stereo space.
;;; placement > 0 < 1
;;; when 0.5, a-a = 1.0, a-b = 0.0, b-a = 0.0, b-b = 1.0
;;; when 0.0, a-a = 0.5, a-b = 0.0, b-a = 0.5, b-b = 0.0
;;; when 1.0, a-a = 0.0, a-b = 0.5, b-a = 0.0, b-b = 0.5

(defun get-stereo-scalers (placement &optional (normalise nil))
  (when (or (< placement 0) (> placement 1))
    (error "get-stereo-scalers: placement should be >= 0 <= 1: ~f"
           placement))
  (let (a-a a-b b-a b-b max)
    (if (<= placement 0.5)
        (setq a-a 1.0
              a-b 0.0
              b-a (* (- 0.5 placement) 2)
              b-b (- 1.0 b-a))
        (setq b-b 1.0
              b-a 0.0
              a-b (* (- placement 0.5) 2)
              a-a (- 1.0 a-b)))
    (when normalise
      (setq max (max (+ a-a b-a) (+ a-b b-b))
            a-a (/ a-a max)
            b-a (/ b-a max)
            a-b (/ a-b max)
            b-b (/ b-b max)))
    (values a-a a-b b-a b-b)))
            

;;; (get-stereo-scalers .3)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definstrument 
    #+allegro
    (samp5 :c-file "../bin/samp5.c")
  #-allegro 
  samp5
  (file time &key
        (duration 0)
        (start 0)
        (end 0)
        (srt 1.0)
        (width 5)
        (srt-env '(0 0 100 0))
        (srt-scaler 1.0)
        (amp 1.0)
        (amp-env '(0 1 100 1))
        (degree 45)
        (rev-amt 0)
        (printing t)) 
  (unless (zerop amp)
    (let* ((st (floor (* time *srate*)))
           (stereo-input (= 2 (mus-channels file)))
           (b-scaler (/ degree 90.0))
           (a-scaler (- 1.0 b-scaler))
           (snd-dur (sound-duration file))
           (input-dur (progn
                        (when (and (> end 0) (> start end))
                          (warn "start (~a) > end (~a), setting end to ~a"
                                start end snd-dur)
                          (setq end snd-dur))
                        (when (> start snd-dur)
                          (warn "start (~a) > input (~a) duration, ~
                                 setting start to 0"
                                start end)
                          (setq start 0.0))
                        (if (zerop end)
                            (- snd-dur start)
                          (- end start))))
           (start-sample (floor (* *srate* start)))
           (fA (open-input file :start start-sample))
           (fB (when stereo-input 
                 (open-input file :channel 1 :start start-sample)))
           (max-out-dur (/ input-dur srt))
           (dur (if (zerop duration)
                    max-out-dur
                  (min max-out-dur duration)))
           ;; force srt
           (do-src (not (= 1 srt)))
           (genA (if do-src
                     (make-src :input fA :srate srt :width width)
                   ;; got to respecify start sample for some reason....
                   (make-readin :file fA :start start-sample)))
           (genB (when stereo-input
                   (if do-src
                       (make-src :input fB :srate srt :width width)
                     (make-readin :file fB :start start-sample))))
           (senv (make-env :envelope srt-env :scaler srt-scaler :offset 0.0 
                           :duration dur))
           (ampf (make-env :envelope amp-env :scaler amp :duration dur)) 
           (output-chans (mus-channels *output*))
           ;; the next two vars randomly choose the two speakers to place the
           ;; sound between.  
           (out1-chan (random output-chans))
           (out2-chan (if (= out1-chan (1- output-chans))
                          0
                        (1+ out1-chan)))
           (count 0)
           (sampA 0.0)
           (sampB 0.0)
           (amp-val 0.0)
           (sre-val 0.0)
           (nd (+ st (floor (* *srate* dur)))))
      (when printing (format t "~&Start time ~a.~%" time))
      (multiple-value-bind
          (a-a a-b b-a b-b)
          (get-stereo-scalers b-scaler)
        (run
         (loop for i from st to nd do
               (when printing 
                 (setf count (if (= count *srate*) 1 (1+ count)))
                 (when (= count *srate*)
                   (clm-print "~%~d" (round (/ i *srate*)))))
               (setq sre-val (env senv)
                     amp-val (env ampf)
                     sampA (* amp-val (if do-src
                                          (src genA sre-val)
                                        (readin genA)))
                     sampB (when stereo-input (* amp-val 
                                                 (if do-src
                                                     (src genB sre-val)
                                                   (readin genB)))))
               (when *reverb* (outa i (* rev-amt (* .5 (+ sampA sampB)))
                                    *reverb*))
               (if stereo-input
                   (progn 
                     (out-any i (+ (* a-a sampA) (* b-a sampB)) out1-chan)
                     (out-any i (+ (* a-b sampA) (* b-b sampB)) out2-chan))
                 (progn
                   (out-any i (* a-scaler sampA) out1-chan)
                   (out-any i (* b-scaler sampA) out2-chan))))))
      (close-input fA)
      (when stereo-input 
        (close-input fB)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF samp5.lsp

