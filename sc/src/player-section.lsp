;;; 02.12.11 SEAN: changed robodoc header to reflect class hierarchy (assigned
;;; to bar-holder rather than sclist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* bar-holder/player-section
;;; NAME 
;;; player-section
;;;
;;; File:             player-section.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> bar-holder 
;;;                   -> player-section
;;;                   AND
;;;                   named-object -> linked-named-object -> sclist
;;;                   -> player-section
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of player-section class which is simply a
;;;                   bar holder that contains a list of sequenzes for a
;;;                   particular player.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    18th March 2002
;;;
;;; $$ Last modified: 11:03:07 Wed Apr 21 2010 BST
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

;;; German word for sequence because the latter is already a lisp type.

(defclass player-section (bar-holder sclist)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((s player-section) stream)
  (format stream "~%PLAYER-SECTION: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ps player-section))
  (clone-with-new-class ps 'player-section))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod num-rthm-seqs ((ps player-section))
  (sclist-length ps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We want to duplicate the bar structure of a section for one player but make
;;; all the bars rest bars and assign the id of another player to the
;;; duplicate.   If <player-id> is nil however, we leave the id as it is
;;;
;;; This is the method called when the player is not at all mentioned in the
;;; section.  

(defmethod clone-as-rest-player-section ((ps player-section)
                                         &optional 
                                         (player-id nil)
                                         (show-rests t)
                                         (missing-duration nil))
  (let ((clone (clone ps)))
    (setf (data clone) (loop for seq in (data ps) collect
                         (clone-as-rest-sequenz seq show-rests
                                                (unless
                                                    (eq t
                                                        missing-duration)
                                                  missing-duration)
                                                player-id)))
    (when player-id
      (setf (id clone) player-id))
    clone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns a list of bars for the whole section (each bar being a flat list of
;;; cmn data) or if <append?> is nil, collect each sequenz as a list. 

#+cmn
(defmethod get-cmn-data ((ps player-section) &optional
                         (section-ref nil)
                         (append? t)
                         write-section-info
                         process-event-fun 
                         (in-c t)
                         display-cmn-marks-in-part
                         display-time
                         ignore)
  (declare (ignore ignore))
  (unless (listp section-ref)
    (setf section-ref (list section-ref)))
  (loop 
     for seq in (data ps) 
     for i from 1
     ;; sequenz class.
     for data = (get-cmn-data seq (when section-ref (cons i section-ref))
                              write-section-info process-event-fun in-c
                              display-cmn-marks-in-part display-time)
     if append? 
     append data
     else collect data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; This function will only combine short bars into longer ones, it won't split
;;; up bars and recombine. 

(defmethod re-bar ((ps player-section)
                   &key start-bar 
                        end-bar ;; inclusive
                        ;; the following is just a list like '(3 8) '(5 8)
                        min-time-sig
                        verbose
                        ;; could also be a beat rhythmic unit
                        (auto-beam t))
  (let ((best-dur (duration (make-time-sig min-time-sig)))
        (dur 0.0)
        (short-bars '())
        (result '())
        (new-bnum start-bar)
        (before (loop 
                    for bnum from (start-bar ps) to (1- start-bar)
                                                    ;; must specify player arg
                    for bar = (get-bar ps bnum 'blah)
                    do 
                      (unless bar
                        (error "player-section::re-bar: before: no bar ~a"
                               bnum))
                    collect (clone bar)))
        (after (loop 
                   for bnum from (1+ end-bar) to (end-bar ps)
                   for bar = (get-bar ps bnum 'blah)
                   do 
                     (unless bar
                       (error "player-section::re-bar: after: no bar ~a"
                              bnum))
                   collect (clone bar)))
        ;; return a list of all the time sigs (as lists), whether modified or
        ;; not 
        (time-sigs '())
        new-bar new-bars)
    (flet ((do-rebar ()
             (when short-bars
               (setf short-bars (nreverse short-bars))
               (when verbose
                 (format t "~&old bar number: ~a, " 
                         (bar-num (first short-bars))))
               (setf new-bar (if (> (length short-bars) 1)
                                 (let* ((nb (re-bar-aux short-bars verbose
                                                        auto-beam))
                                        (rl (rehearsal-letter nb)))
                                   ;; rehearsal letters come on the bar line of
                                   ;; the __previous__ bar so there's a good
                                   ;; chance that due to re-barring we will
                                   ;; have a letter too late. move it one
                                   ;; forward here
                                   (when rl
                                     (setf (rehearsal-letter nb) nil
                                           (rehearsal-letter (first result))
                                           rl))
                                   nb)
                               (clone (first short-bars)))
                     (bar-num new-bar) new-bnum
                     (old-bar-nums new-bar) (loop for b in short-bars collect
                                                  (bar-num b))
                     short-bars '()
                     dur 0.0)
               (when new-bar
                 (push new-bar result)
                 (let* ((ts (get-time-sig new-bar))
                        (d (denom ts))
                        (n (num ts)))
                   (push (list n d) time-sigs))
                 (incf new-bnum))
               (when verbose
                 (format t "new bar number: ~a" 
                         (bar-num (first result)))))))
      ;; put all the seqs' bars into one seq
      (concatenate-seqs ps)
      ;; there is now only one seq in the data list
      (setf new-bars
        (loop 
            for bnum from start-bar to end-bar 
                                       ;; we have to specify player arg...
            for bar = (get-bar ps bnum 'blah)
            while bar
            do
              (incf dur (bar-duration bar))
              (push bar short-bars)
              (when (>= dur best-dur)
                (do-rebar))
            finally (do-rebar)
                    (return (nreverse result))))
      (loop 
          for bar in after
          for new-bar-num from (1+ (bar-num (first (last new-bars))))
          do (setf (bar-num bar) new-bar-num))
      (unless new-bars 
        (error "player-section::re-bar: no new-bars!"))
      (let* ((bars (append before new-bars after))
             (len-bars (length bars)))
        (setf (bars (first (data ps))) bars
              (num-bars ps) len-bars
              (end-bar ps) (+ -1 (start-bar ps) len-bars)))
      (nreverse time-sigs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Instead of having a list of sequenzes, each with its own list of bars, have
;;; only one 'super-seq' that contains all the bars of the former sequenzes.
;;; NB It is assumed that we would do this after notes have been generated for
;;; all the seqs and that we no longer need rthm-seq slots like
;;; pitch-seq-palette etc. 

(defmethod concatenate-seqs ((ps player-section))
  (let* ((seqs (data ps))
         (last-seq (first (last seqs)))
         (new-seq (clone (first seqs)))
         (num-rhythms 0) ;; this isn't a bar-holder slot so we have to count
         (all-bars (loop for seq in seqs 
                       do (incf num-rhythms (num-rhythms seq))
                       appending (bars seq))))
    (setf (end-bar new-seq) (end-bar last-seq)
          (num-bars new-seq) (num-bars ps)
          (end-time new-seq) (end-time ps)
          (end-time-qtrs new-seq) (end-time-qtrs ps)
          (num-notes new-seq) (num-notes ps)
          (num-score-notes new-seq) (num-score-notes ps)
          (num-rests new-seq) (num-rests ps)
          (duration new-seq) (duration ps)
          (duration-qtrs new-seq) (duration-qtrs ps)
          (num-rhythms new-seq) num-rhythms
          (bars new-seq) (my-copy-list all-bars)
          (data ps) (list new-seq))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun re-bar-aux (bars &optional verbose auto-beam)
  (when bars
    (let* ((rehearsal-letter nil)
           (beat (if (eq auto-beam t) 
                     nil
                   ;; it's a rhythmic value for the beat
                   auto-beam))
           (ts (loop 
                   with denom = 1 with num = 0 
                   for bar in bars 
                   for time-sig = (get-time-sig bar)
                   for bdenom = (denom time-sig)
                   for bnum = (num time-sig)
                   do
                     (unless (and (integer>0 bdenom)
                                  (integer>0 bnum))
                       (error "player-section::re-bar-aux: bar's time sig is ~
                               ~a/~a!!!" bnum bdenom))
                     (unless (= bdenom denom)
                       (setf num (* num (/ bdenom denom))
                             denom bdenom))
                     (incf num bnum)
                     ;; (format t "~&~a ~a~%" num denom)
                     (when verbose
                       (format t "~a/~a " bnum bdenom))
                     (when (rehearsal-letter bar)
                       (when rehearsal-letter
                         (error "player-section::re-bar-aux: result bar can't ~
                              have two rehearsal letters (~a and ~a)!"
                                rehearsal-letter (rehearsal-letter bar)))
                       (setf rehearsal-letter (rehearsal-letter bar)))
                   finally
                     (return
                       (loop
                         (if (whole-num-p num)
                             ;; see time-sig.lsp to add more replacements
                             (let ((pts (get-preferred-time-sig
                                         (list num denom))))
                               (unless pts
                                 (error "player-section::re-bar-aux: ~
                                       couldn't get time-sig!"))
                               (when verbose
                                 (format t "-> ~a/~a, "
                                         (first pts) (second pts)))
                               (return (make-time-sig pts)))
                           (setf num (* 2 num)
                                 denom (* 2 denom)))))))
           (first-bar (clone (first bars))))
      (unless ts
        (error "player-section::re-bar-aux: couldn't get new time-signature"))
      ;; when we saw an e.g. 3/8 rest bar then the single rhythm in that bar
      ;; didn't have a dot but rather data was 8/3, duration 1.5 etc.   Need 
      ;; to correct that by adding the dot.
      (loop 
          for bar in bars 
          for first = (first (rhythms bar))
          do
            (when (and (is-rest-bar bar)
                       (numberp (data first)))
              (setf (rhythms bar)
                (list (make-rest (get-rhythm-letter-for-value 
                                  (data first)))))))
      (setf (time-sig first-bar) ts
            (rhythms first-bar) (loop for bar in bars
                                    appending (my-copy-list (rhythms bar)))
            ;; this of course means that some rehearsal letters will be moved
            ;; around a little. 
            (rehearsal-letter first-bar) rehearsal-letter)
      ;; no need to update score-tuplets, tuplets, beams, missing-duration
      ;; 
      ;; the parent-start-end, id, previous, this, next and tag slots of the
      ;; first bar will be copied over in the clone; those of the other bars
      ;; will be lost
      ;; 
      ;; this updates num-rhythms, is-rest-bar, notes-needed, num-rests, and
      ;; num-score-notes 
      (gen-stats first-bar)
      (unless (is-full first-bar)
        (error "~a~%player-section::re-bar-aux: bar isn't full, time-sig: ~a"
               first-bar (get-time-sig first-bar)))
      ;; (print first-bar)
      ;; (print (bar-duration first-bar))
      ;; (print (get-time-sig first-bar))
      (consolidate-rests first-bar beat)
      (consolidate-notes first-bar nil beat)
      (when auto-beam
        ;; todo: this doesn't work either...
        (auto-beam first-bar beat nil))
      first-bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF player-section.lsp
