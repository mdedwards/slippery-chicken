;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* bar-holder/sequenz
;;; NAME 
;;; sequenz
;;;
;;; File:             sequenz.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> rthm-seq
;;;                   -> sequenz
;;;                                     AND
;;;                   named-object -> linked-named-object -> bar-holder
;;;                   -> sequenz
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the sequenz class which holds the
;;;                   necessary data (pitch, rhythms etc.) for one sequenz for
;;;                   one instrument.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 15th 2002
;;;
;;; $$ Last modified:  13:39:55 Wed May 21 2025 CEST
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; German word for sequence because the latter is already a lisp type.

(defclass sequenz (rthm-seq bar-holder)
  ;; 31/3/10: a copy of the pitch curve, for info
  ((pitch-curve :accessor pitch-curve :type list :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((s sequenz) stream)
  (format stream "~%SEQUENZ: pitch-curve: ~a" (pitch-curve s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((s sequenz))
  (clone-with-new-class s 'sequenz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((s sequenz) new-class)
  (declare (ignore new-class))
  (let ((rs (call-next-method)))
    (setf (slot-value rs 'pitch-curve) (my-copy-list (pitch-curve rs)))
    rs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-first-bar ((s sequenz))
  (first (bars s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-last-bar ((s sequenz))
  (first (last (bars s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-slots ((s sequenz)
                         &optional
                           tempo-map
                           (start-time 0.0) 
                           (start-time-qtrs 0.0)
                           (start-bar 1)
                           (current-section nil)
                           (sequenz-num nil) ; 1-based
                           (warn-ties t)
                           (update-write-bar-nums nil))
  (declare (ignore current-section warn-ties update-write-bar-nums))
  ;; (print tempo-map)
  (setf (start-bar s) start-bar
        (start-time s) start-time
        (start-time-qtrs s) start-time-qtrs
        (num-bars s) (length (bars s))
        (end-bar s) (+ start-bar (num-bars s) -1)
        ;; MDE Sat Jun 28 15:27:17 2014 -- got to reset and recount these slots
        (num-bars s) 0
        (num-notes s) 0
        (num-score-notes s) 0
        (num-rests s) 0
        (duration s) 
        (loop 
          for rsb in (bars s) 
          for bar-count from 1
          with time = start-time
          with time-qtrs = start-time-qtrs
          with tempo
          with bar-dur
          do 
             (setf (bar-num rsb) (+ start-bar bar-count -1)
                   ;; 9.2.11
                   (nth-bar rsb) (1- bar-count)
                   (nth-seq rsb) (1- sequenz-num)
                   tempo (let ((tpo (if tempo-map
                                        (scm-get-data (bar-num rsb)
                                                      tempo-map)
                                        (make-tempo 60.0))))
                           (unless tpo
                             (error "sequenz::update-slots: bar ~a: no tempo!"
                                    (bar-num rsb)))
                           (data tpo))
                   bar-dur (update-time rsb time time-qtrs 
                                        tempo))
             (update-events-bar-nums rsb (bar-num rsb))
             (incf time bar-dur)
             (incf time-qtrs (bar-qtr-duration rsb))
             ;; MDE Wed Apr 18 10:09:22 2012 -- moved whether to write bar nums
             ;; into the sc class  
             ;; MDE Mon Jul 16 16:22:27 2012 
             (gen-stats rsb)
             ;; MDE Sat Jun 28 15:28:34 2014 --  recalculate stats
             (incf (num-bars s))
             (incf (num-notes s) (notes-needed rsb))
             (incf (num-score-notes s) (num-score-notes rsb))
             (incf (num-rests s) (num-rests rsb))
          sum bar-dur)
        (end-time s) (+ start-time (duration s))
        (duration-qtrs s) (loop for rsb in (bars s) sum (bar-qtr-duration rsb))
        (end-time-qtrs s) (+ start-time-qtrs (duration-qtrs s)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If simple-nth is t (it could be a player reference made by recursive
;;; calls), then we just get the nth bar in the sequenz, otherwise the given
;;; bar-num is relative to the start-bar of the sequenz.

(defmethod get-bar ((s sequenz) bar-num &optional (simple-nth nil))
  (let ((nth (if (eq simple-nth t)
                 bar-num
               (- bar-num (start-bar s)))))
    (when (and (>= nth 0) (< nth (num-bars s)))
      (nth nth (bars s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The argument is the active sequenz that we want to mimic using rest bars.
;;; We don't actually clone as that would be too expensive.
;;; The player-ref is needed to set the right player-section-ref for the empty
;;; bars we create. 

(defmethod clone-as-rest-sequenz ((s sequenz) &optional
                                                (show-rests t)
                                                missing-duration
                                                player-ref)
  ;;   (print 'cars)
  (let ((seq (make-instance 'sequenz)))
    ;; from the rthm-seq class
    (setf (slot-value seq 'num-bars) (num-bars s)
          (slot-value seq 'duration) (duration s)
          ;; make rest bars from the bars in the active sequenz
          (slot-value seq 'bars)
          (loop for b in (bars s) and i from 1 
             for rb = 
               (make-rest-bar (get-time-sig b)
                              (write-time-sig b)
                              show-rests
                              (when (= i 1)
                                missing-duration)
                              (let ((psr (copy-list (player-section-ref b))))
                                (when psr
                                  (setf-last psr player-ref))
                                psr)
                              (nth-seq b)
                              (nth-bar b))
             do
             ;; MDE Mon May  5 20:33:24 2014 
               (setf (player (first (rhythms rb))) player-ref)
             collect rb)
          ;; from the bar-holder class
          (slot-value seq 'start-bar) (start-bar s) 
          (slot-value seq 'end-bar) (end-bar s) 
          (slot-value seq 'num-bars) (num-bars s) 
          (slot-value seq 'start-time) (start-time s) 
          (slot-value seq 'end-time) (end-time s) 
          (slot-value seq 'duration) (duration s))
    seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This assumes that the start-time slot of each event has been correctly set
;;; according to the tempo, hence it's this class and not rthm-seq.  threshold
;;; is the max duration of a fast note, in seconds.  Returns a list of those
;;; notes that are leaped quickly __to__, not from.  If last-seq is given then
;;; we can check for quick notes on the changeover of sequences too.

(defmethod get-quick-notes-indices ((s sequenz) 
                                    &optional last-seq (threshold 0.125)
                                    verbose)
  ;; MDE Tue Aug 4 14:12:16 2020, Heidhausen -- don't do this if threshold is
  ;; nil
  (let ((result '()))
    (when (and (numberp threshold) (> (num-notes s) 0))
    ;; (when (and (numberp (print threshold)) (> (print (num-notes s)) 0))
      (loop 
        with last = (if last-seq 
                      (get-last-attack last-seq nil)
                      (get-nth-attack 0 s))
        for i from (if last-seq 0 1) below (num-notes s) 
        for this = (get-nth-attack i s)
        do
           (when (and verbose last this)
             (format t "~&last: ~a ~a, this: ~a ~a"
                     (get-pitch-symbol last) (start-time last)
                     (get-pitch-symbol this) (start-time this)))
           (when (and last this
                      (<= (- (start-time this) (start-time last))
                          threshold))
             ;; (print 'gotit)
             (push i result))
           (setf last this)))
    (when verbose (print result))
    (nreverse result)))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns a list of bars, each bar being a list of cmn data
;;; If section is not nil, then it will be printed as text above the staff.

#+cmn
(defmethod get-cmn-data ((s sequenz) &optional section write-section-info
                         process-event-fun in-c
                         display-marks-in-part
                         display-time ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;; this is cheap but to avoid always passing another arg, we push the sequenz
  ;; number for this section into the section argument in
  ;; player-section::get-cmn-data, so pop it out here.
  (let* ((seq-num (when (and section (listp section))
                    (pop section)))
         (seq-id (if (this s)
                     (this s)
                     (id s)))
         (text (when (and section write-section-info)
                 (if (eq section 'show-id-and-tag-only)
                     ;; when displaying the rthm-seq-palette
                     (format nil "~a     ~a"
                             (list-to-string seq-id) 
                             (if (tag s) (tag s) ""))
                     (format nil "[~a~a~a~a~a]"
                             (list-to-string section ".")
                             (if seq-num (format nil "#~a" seq-num) "")
                             (if seq-id ":" "")
                             (if seq-id (list-to-string seq-id ".") "")
                             (if (pitch-curve s)
                                 (format nil ":~a"
                                         (list-to-string (pitch-curve s) ",") )
                                 "")))))
         (bars (progn
                 (when (> (num-rhythms (get-first-bar s))
                          0)
                   (add-mark (get-nth-event 0 (get-first-bar s))
                                 (when text
                                   (cmn::sc-cmn-text text 
                                                     :dx 0.0
                                                     :dy 2 ; 1.5
                                                     :font-size 7.0))))
                 (loop for rsb in (bars s) 
                    ;; 31/3/07 skip the bar if it's part of a
                    ;; multi-bar-rest  
                    unless (eq (multi-bar-rest rsb) t)
                    collect (get-cmn-data rsb process-event-fun in-c
                                          display-marks-in-part
                                          display-time)))))
    bars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; start is 0-based (for nth)
;;; it is up to the caller to call update-slots afterwards.
(defmethod delete-bars ((s sequenz) start &key (num 1) ignore)
  (declare (ignore ignore))
  ;; we assume num does not exceed the number of bars
  (setf (bars s) (remove-elements (bars s) start num))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri May 17 21:40:16 2013 -- make sure that any dynamic marks that were
;;; added via the :marks slot during the creation of the rthm-seq-palette are
;;; reflected in the amplitudes of the events.
(defmethod dynamics-to-amplitudes ((s sequenz))
  (loop for bar in (bars s) do
       (dynamics-to-amplitudes bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jul 11 19:20:27 2019
(defmethod pedals-to-controllers ((s sequenz) &optional (update-amplitude t))
  (loop for bar in (bars s) do
       (pedals-to-controllers bar update-amplitude)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sequenz-p (thing)
  (typep thing 'sequenz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sequenz.lsp
