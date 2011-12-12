;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* bar-holder/section
;;; NAME 
;;; section
;;;
;;; File:             section.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> bar-holder 
;;;                   -> section
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of section class which is simply a bar
;;;                   holder and recursive-assoc-list that contains (possibly
;;;                   subsections which contain) player-sections. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    23rd March 2002
;;;
;;; $$ Last modified: 10:52:37 Wed Apr 21 2010 BST
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass section (bar-holder recursive-assoc-list)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((s section) stream)
  (format stream "~%SECTION: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((s section))
  (clone-with-new-class s 'section))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod has-subsections ((s section))
  (typep (data (first (data s))) 'section))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; bar-num is 1-based!
;;; N.B. although optional, the player argument is required.  It is optional so
;;; that we can have a sequenz method with the same name which only requires
;;; the bar-num argument. 

(defmethod get-bar ((s section) bar-num &optional player)
  (unless player
    (error "bar-holder::get-bar: player argument is required!"))
  (if (has-subsections s)
      (call-next-method)
    (let ((player-section (get-data player s)))
      (when player-section
        (get-bar player-section bar-num player)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. The data for just one instrument is collected here!!!!  Returns a list
;;; of bars for the section (and subsections) for <instrument>, each bar being
;;; a flat list of cmn data.

;;; If <empty> is t, then all the bars will just contain invisible whole rests

(defmethod get-cmn-data ((s section) &optional 
                         instrument 
                         empty 
                         (append? t)
                         (write-section-info t)
                         process-event-fun
                         (in-c t)
                         display-marks-in-part
                         display-time)
  (unless (and instrument (atom instrument))
    (error "section::get-cmn-data: One and only one instrument must be ~
            given!: ~a" instrument))
  (if (has-subsections s)
      (loop for no in (data s) append
         ;; (data no) here is a subsection.
           (get-cmn-data (data no) instrument empty append? 
                         write-section-info process-event-fun in-c
                         display-marks-in-part display-time))
      (let ((player-section (get-data instrument s)))
        (if empty
            (get-cmn-data (clone-as-rest-player-section player-section nil nil
                                                        empty)
                          nil append? write-section-info display-time)
            ;; pass the section reference to the call to the player-section so
            ;; that it can be written at the beginning of the first sequenz.
            (get-cmn-data player-section
                          (when write-section-info (full-ref s))
                          append? write-section-info process-event-fun in-c
                          display-marks-in-part display-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is an aux procedure because we only want to call it (from
;;; bar-holder::update-write-time-sig) when we know there are no subsections in
;;; this section.

;;; This only changes the first bar of each sequenz dependent on the last bar
;;; of the previous sequenz--bars within the sequenz remain the same as when
;;; the rthm-seq-bar was initialized.

(defmethod update-write-time-sig-aux ((s section)
                                      &optional 
                                      (force nil)
                                      (last-bar nil)
                                      (players nil))
  (flet ((set-all-players (n val)
           (loop
               for player in players 
               for player-section = (get-data player s)
               for player-seq = (nth n (data player-section))
               do (setf (write-time-sig (first (bars player-seq)))
                    val))))
    (let ((lb last-bar))
      ;; we loop in the sequenzes of the first player-section
      (loop for seq in (data (first (data s))) and i from 0 do
            (let ((first-bar (get-first-bar seq)))
              (when (and force (zerop i))
                (set-all-players i t))
              (when lb
                ;; time-sig-equal returns 'time-sig-equal-duration for
                ;;  3/4 ? 6/8  
                (when (eq t (time-sig-equal first-bar lb))
                  ;; maybe here we should update all the bars in the
                  ;; seq for each instrument?  If so, define method in
                  ;; rthm-seq and call it here.
                  (set-all-players i nil)))
              (setf lb (get-last-bar seq))))
      lb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-all-players ((s section))
  (loop for player-section in (data s) collect
        (id player-section)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This function will only combine short bars into longer ones, it won't split
;;; up bars and recombine. 
(defmethod re-bar ((s section)
                   &key start-bar 
                        end-bar
                        min-time-sig
                        verbose
                        ;; could also be a beat rhythmic unit
                        (auto-beam t))
  (if (has-subsections s)
      (loop for sub-section in (data s) do
            (re-bar sub-section :start-bar start-bar :end-bar end-bar
                    :min-time-sig min-time-sig :verbose verbose))
    ;; looping for each player means we do a lot of the detection arithmetic
    ;; not once but once for each player, but it's necessary for the glorious
    ;; future when we might have different meters for different instruments.
    (loop 
        with first-time-sigs
        with this-time-sigs
        for player-section in (data s) do
          (when (and (<= start-bar (end-bar s))
                     (>= start-bar (start-bar s)))
            (setf this-time-sigs
              (re-bar player-section 
                    :start-bar (max start-bar (start-bar player-section))
                    :end-bar (min end-bar (end-bar player-section))
                    :min-time-sig min-time-sig :verbose verbose 
                    :auto-beam auto-beam))
            (if first-time-sigs
                (unless (equal first-time-sigs this-time-sigs)
                  (warn "section::re-bar: not all time-sigs are the same! ~
                         ~%first: ~a ~%this:  ~a"
                        first-time-sigs this-time-sigs))
              (setf first-time-sigs this-time-sigs)))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF section.lsp
