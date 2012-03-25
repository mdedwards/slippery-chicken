;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* recursive-assoc-list/ensemble
;;; NAME 
;;; ensemble
;;;
;;; File:             ensemble.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   ensemble
;;;
;;; Version:          0.92
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the ensemble class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th September 2001
;;;
;;; $$ Last modified: 12:57:23 Sat Jan  7 2012 ICT
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

;;; 02.12.11 SEAN: Changed robodoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; id is the ensemble name, data the instruments

(defclass ensemble (recursive-assoc-list)
  ;; simple list of instrument names / num instrument pairs that is then used
  ;; to set the score-write-bar-line slot of each instrument in the ensemble.
  ;; All the score-write-bar-line slots are first set to nil, then updated
  ;; according to this list. Eg '(cello 3 tuba 5 bassoon 6)
  ;; ***** NB when using CMN use the staff-groupings slot of the
  ;; slippery-chicken class instead!.  
  ((bar-line-writers :accessor bar-line-writers :type list 
                     :initarg :bar-line-writers :initform nil)
   (players :accessor players :type list :initform nil)
   ;; an instrument-palette that contains the instrument objects to be
   ;; cloned. The instruments list contains a simple symbol (id for lookup into
   ;; the instrument-palette) instead of a new instrument
   ;; definition/instantiation.
   (instrument-palette :accessor instrument-palette
                       :initarg :instrument-palette :initform nil))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((e ensemble) &rest initargs)
  (declare (ignore initargs))
  (score-write-bar-line-all-nil e)
  (let ((ip (instrument-palette e)))
    (when (and ip (not (typep ip 'instrument-palette)))
      (error "ensemble::initialize-instance: ~
              the instrument-palette slot must be an ~
              instrument-palette instance: ~a"
             ip)))
  (let ((blws (bar-line-writers e))
        (player nil))
    (loop for ins in blws by #'cddr and num in (cdr blws) by #'cddr do
          (setf player (get-data ins e))
          (unless player
            (error "ensemble::initialize-instance: ~
                    bar-line-writers: reference to instrument ~
                    not in the ensemble: ~a" ins))
          (set-score-write-bar-line player num)))
  (get-players e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-players ((e ensemble))
  (if (players e)
      (players e)
    (progn
      (link-named-objects e)
      (let* ((current (get-first e))
             (players (remove-duplicates
                       (loop while current 
                           collect (id current)
                           do (setf current (get-data (next current)
                                                      e nil))))))
        ;; if the number of players is not equal to the length of players after
        ;; duplicates are removed, then we must have some players with same
        ;; name, which is illegal
        (unless (= (num-data e) (length players))
          (error "ensemble::get-players: Found duplicate names for players in ~
              ensemble with id ~a" (id e)))
        (setf (players e) players)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((e ensemble))
  (ral-to-ensemble e (instrument-palette e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Don't print the whole instrument-palette instance as it will probably be
;;; huge.  Print its id only.

(defmethod print-object :before ((e ensemble) stream)
  (let ((ip (instrument-palette e)))
    (format stream "~&ENSEMBLE: bar-line-writers: ~a~
                    ~%          players: ~a~
                    ~%          (id instrument-palette): ~a"
            (bar-line-writers e) (players e) (when ip (id ip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((e ensemble))
  (clone-with-new-class e 'ensemble))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((e ensemble) new-class)
  (declare (ignore new-class))
  (let ((ral (call-next-method)))
    (setf (slot-value ral 'bar-line-writers) (my-copy-list 
                                              (bar-line-writers e))
          (slot-value ral 'players) (my-copy-list (players e))
          ;; don't clone the instrument-palette, it's almost certain not to be
          ;; garbage-collected 
          (slot-value ral 'instrument-palette) (instrument-palette e))
    ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-first-bar-ins-for-doubling-players
    ((e ensemble) (icm instrument-change-map) first-section-ref)
  (loop for player in (data e) do
        (when (doubles player)
          (instrument-for-first-bar icm (id player) first-section-ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod score-write-bar-line-all-nil ((e ensemble))
  (score-write-bar-line-all-nil-aux e)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod num-notes ((e ensemble))
  (num-notes-aux e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tessitura ((e ensemble))
  (degree-to-note (tessitura-aux e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod num-players ((e ensemble))
  (num-data e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod players-exist ((e ensemble) players)
  (let ((e-players (players e)))
    (loop for player in players do
          (unless (member player e-players)
            (error "ensemble::players-exist: ~a is not a member of the ~
                    ensemble" player)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. If you have an ensemble with a player doubling two instruments, be
;;; sure to indicate some key-word argument or other as
;;; (fl1 ((piccolo violin) :midi-channel 1)) works but
;;; (fl1 ((piccolo violin))) thinks that piccolo is a nested ensemble!!!

(defun make-ensemble (id ensemble &key bar-line-writers instrument-palette)
  (make-instance 'ensemble :id id :data ensemble 
                 :bar-line-writers bar-line-writers 
                 :instrument-palette instrument-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-notes-aux (ensemble)
  (loop for thing in (data ensemble) sum 
        (cond ((typep thing 'player)
               (total-notes thing))
              ((is-ral (data thing))
               (num-notes-aux (data thing)))
              (t (error "ensemble::num-notes-aux: what's this doing here?: ~a"
                        thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tessitura-aux (ensemble)
  (/ (loop for thing in (data ensemble) 
        sum (cond ((typep thing 'player)
                   (tessitura-degree thing))
                  ((is-ral (data thing))
                   (tessitura-aux (data thing)))
                  (t (error "ensemble::tessitura-aux: ~
                             wrong data type: ~a"
                            thing))))
     (num-data ensemble)))
                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun score-write-bar-line-all-nil-aux (ensemble)
  (loop for thing in (data ensemble) do
        (cond ((typep thing 'player)
               (set-score-write-bar-line thing nil))
              ((is-ral (data thing))
               (score-write-bar-line-all-nil-aux (data thing)))
              (t (error "ensemble::score-write-bar-line-all-nil-aux: ~
                         what's this doing here?: ~a"
                        thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ral-to-ensemble (ral ins-palette)
  (when (and ral (data ral))
    (let ((num-players 0))
      (loop for i in (data ral) and j from 0 do
            (let ((data (data i)))
              (if (is-ral data)
                  (multiple-value-bind
                      (ensemble nplayers)
                      (ral-to-ensemble data ins-palette)
                    (incf num-players nplayers)
                    (setf (data (nth j (data ral)))
                      ensemble))
                (progn
                  (incf num-players)
                  (setf (nth j (data ral))
                    (apply #'make-player (append
                                          (list (id i) 
                                                ins-palette)
                                          (if (listp data)
                                              data
                                            (list data)))))))))
      (values (sc-change-class ral 'ensemble)
              num-players))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensemble-p (thing)
  (typep thing 'ensemble))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF ensemble.lsp

