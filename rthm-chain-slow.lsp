;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* circular-sclist/rthm-chain-slow
;;; NAME 
;;; rthm-chain
;;;
;;; File:             rthm-chain-slow.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> rthm-chain-slow
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          class used in rthm-chain
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th February 2010
;;;
;;; $$ Last modified: 22:06:28 Fri Jan 13 2012 ICT
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

;;; 02.12.11 SEAN: changed robodoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Class to handle the slow-rthms in our chain: these are (presently) 2 and 3
;;; beat rthm groups.
(defclass rthm-chain-slow (circular-sclist)
 ;; the data slot holds the alternation sequence that sets the order of 2beat
 ;; 3beat bars.
 ;; a list of groups of 2- and 3-beat seqs, minimum four in each
 ((2beat :accessor 2beat :type list :initarg :2beat :initform nil)
  (3beat :accessor 3beat :type list :initarg :3beat :initform nil)
  ;; these two are only set once we know how many times we'll use the 1-beat
  ;; rthms. 
  (2beat-order :accessor 2beat-order :initform nil)
  (3beat-order :accessor 3beat-order :initform nil)
  ;; we can have different groups of 2beat/3beat rhythms; if we do
  ;; we'll transition through them according to the same fibonacci
  ;; principle we have for the 1-beat rhythms above.  This is created
  ;; automatically as a cscl when init-order is called.
  ;; NB this is the reason for the extra nesting of the 2 and 3-beat rthms
  (transitions :accessor transitions :initform nil)
  ;; the number of 2beat/3beat groups we have to transition through
  (num-groups :accessor num-groups :type integer :initform 0)
  (current-beats :accessor current-beats :type integer :initform 0)
  ;; denominator (4, 8?)
  (beat :accessor beat :type integer :initarg :beat :initform 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((rcs rthm-chain-slow) &rest initargs)
  (declare (ignore initargs))
  (unless (data rcs)
    ;; this is the default order of the time signatures i.e. the order of
    ;; selecting a two-beat or a three-beat rhythms
    (setf (data rcs) '(2 3 2 2 3 2 2 3 3 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((rcs rthm-chain-slow) stream)
  (format stream "~%RTHM-CHAIN-SLOW: 2beat: (too long to print)~
                  ~%                 3beat: (too long to print)~
                  ~%                 2beat-order: ~a~
                  ~%                 3beat-order: ~a~
                  ~%                 transitions: ~a~
                  ~%                 num-groups: ~a~
                  ~%                 current-beats: ~a~
                  ~%                 beat: ~a"
          (2beat-order rcs) (3beat-order rcs) (transitions rcs) (num-groups rcs)
          (current-beats rcs) (beat rcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bear in mind a single group looks something like
;;; 
;;; ((h) (q q) ((e) e+q)
;;;  ({ 3 tq te } { 3 +te tq }) 
;;;  ({ 3 (tq) te } { 3 +te tq })
;;;  ({ 3 tq te } (q)))
;;; 
;;; each group must have the same number of bars and there must be at least 4
;;; of them, but there can be a different number of bars in the groups of the
;;; 3beat or 2beat slots.
(defmethod verify-and-store :after ((rcs rthm-chain-slow))
  (let ((num-groups (length (2beat rcs)))
        (3beat-bars (length (first (3beat rcs))))
        (2beat-bars (length (first (2beat rcs))))
        (bars-per-group (length (first (2beat rcs)))))
    (unless (= num-groups (length (3beat rcs)))
      (error "rthm-chain-slow::verify-and-store: 2beat (~a) and 3beat (~a) ~
              slots must be the same length"
             num-groups (length (3beat rcs))))
    (unless (>= bars-per-group 4)
      (error "rthm-chain-slow::verify-and-store: 2beat and 3beat slots need ~
              at least 4 bars each: ~%~a ~%~a" (2beat rcs) (3beat rcs)))
    (setf (num-groups rcs) num-groups)
    (flet ((process-rthms (bars meter)
             (let ((len (if (= meter 2) 2beat-bars 3beat-bars)))
               (unless (= (length bars) len)
                 (error "rthm-chain-slow::verify-and-store: the ~abeat groups ~
                         must all have ~a bars" meter len))
               (loop for bar in bars collect 
                  ;; the t 3rd arg splits into beats so our slots are now
                  ;; a list of lists of rhythms, one sublist per beat
                    (make-rhythms bar (list meter (beat rcs)) t)))))
      ;; unless we've already parsed...
      (unless (and (listp (caaar (2beat rcs)))
                   (rhythm-p (caaaar (2beat rcs))))
        (setf (2beat rcs) (loop for group in (2beat rcs) collect
                               (process-rthms group 2))
              (3beat rcs) (loop for group in (3beat rcs) collect 
                               (process-rthms group 3))))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset :after ((rcs rthm-chain-slow) &optional where)
  (setf (current-beats rcs) 0)
  (when (transitions rcs)
    (reset (transitions rcs) where))
  (when (2beat-order rcs)
    (reset (2beat-order rcs) where))
  (when (3beat-order rcs)
    (reset (3beat-order rcs) where)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gets the next bar as a list of rhythms
(defmethod get-next ((rcs rthm-chain-slow))
  (unless (2beat-order rcs)
    (error "rthm-chain-slow::get-next: you must call init-order first."))
  (let* ((2or3 (call-next-method)) ;; get 2 or 3 from the data slot
         (two (= 2or3 2))
         ;; we can have as many groups as we want so we select the group we'll
         ;; now use
         (group-index (get-next (transitions rcs)))
         ;; 23.1.11 if we want more than 2 voices, we need to get more indices,
         ;; as a list
         (bar-index (1- (get-next (if two
                                      ;; the selection indices for the seqs in
                                      ;; the group
                                      (2beat-order rcs)
                                      (3beat-order rcs))))))
    (setf (current-beats rcs) (if two 2 3))
    ;; (format t "~&2or3: ~a group: ~a bar: ~a current: ~a" 
    ;;     2or3 group-index bar-index (current (transitions rcs)))
    (nth bar-index (nth group-index (if two 
                                        (2beat rcs)
                                        (3beat rcs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We should generate the same number of 2beat and 3beat rhythms, which
;;; averages 1/2.5 x the number of 1-beat rhythms we'll have.  This in turn
;;; means we'll want 1/2 of the 1/2.5 devoted to 2beat and the other 1/2 to
;;; 3beat rhythms so it's 1/5 for each of them (though calculate this to be
;;; sure) 
;;; 19.1.11: we can't actually know how many slow bars we'll want until we've
;;; generated all the 1-beat rhythms, taking the activity-curve for this into
;;; consideration.  So this will actually be called twice
(defmethod init-order ((rcs rthm-chain-slow) num-slow-bars
                       ;; if not, use processions instead
                       &optional (use-fibonacci t) wrap)
  ;; data is something like '(2 3 2 2 3 2 2 3 3 3); it's the order we'll do the
  ;; 2- and 3-beat rthms in; we'll no doubt have to repeat it 
  (let* ((2count (count 2 (data rcs)))
         (3count (count 3 (data rcs)))
         ;; so we won't get through all the data in the last cycle but this
         ;; won't hurt
         (slow-cycles (ceiling num-slow-bars (length (data rcs))))
         (num2s (* slow-cycles 2count))
         (num3s (* slow-cycles 3count)))
    ;; (format t "~&~a slow bars, ~a 2s, ~a 3s" num-slow-bars num2s num3s)
    (setf (2beat-order rcs) 
          (rthm-chain-get-order 
           num2s (length (first (2beat rcs))) '2beat-order use-fibonacci wrap)
          (3beat-order rcs) 
          (rthm-chain-get-order 
           num3s (length (first (3beat rcs))) '3beat-order use-fibonacci wrap)
          ;; init the fib transition through the various groups of rthms as a
          ;; cscl; we need to transition over the total number of slow rthms
          ;; we have; this is calculated above in num2s and num3s
          (transitions rcs)
          (make-cscl (fibonacci-transitions (+ num2s num3s) (num-groups rcs))
                     :id 'rcs-transitions)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF rthm-chain-slow.lsp
