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
;;; Version:          1.0.0-beta2
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
;;; $$ Last modified: 21:47:20 Sun May  6 2012 BST
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

;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy

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

;;; ****m* section/has-subsections
;;; DESCRIPTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod has-subsections ((s section))
;;; ****
  (typep (data (first (data s))) 'section))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 12:20:17 BST 2012: Added robodoc entry

;;; ****m* section/get-sequenz
;;; DESCRIPTION
;;; Get the specified sequenz object from a given section object.
;;; 
;;; ARGUMENTS
;;; - A section object.
;;; - The ID of the player from whose part the sequenz object is to be
;;;   returned. 
;;; - An integer that is the number of the sequence object to be returned from
;;;   within the given section object. This number is 1-based.
;;; 
;;; RETURN VALUE
;;; A sequenz object.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5))))
                            (2 ((((4 4) q e s s h))
                                :pitch-seq-palette ((1 2 3 4 5))))
                            (3 ((((4 4) e s s h q))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (2 2 2 2 2))
                            (vc (2 2 2 2 2))))
                        (3 ((cl (3 3 3 3 3))
                            (vc (3 3 3 3 3))))))))
  (get-sequenz (get-section mini 2) 'vc 2))

=>
SEQUENZ: pitch-curve: (1 2 3 4 5)
RTHM-SEQ: num-bars: 1
          num-rhythms: 5
          num-notes: 5
          num-score-notes: 5
          num-rests: 0
          duration: 4.0
          psp-inversions: NIL
          marks: NIL
          time-sigs-tag: NIL
          handled-first-note-tie: NIL
         (for brevity's sake, slots pitch-seq-palette and bars are not printed)
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: (1), this: (2), next: (3)
BAR-HOLDER: 
            start-bar: 7
            end-bar: 7
            num-bars: 1
            start-time: 24.0
            end-time: 28.0
            start-time-qtrs: 24.0
            end-time-qtrs: 28.0
            num-notes (attacked notes, not tied): 5
            num-score-notes (tied notes counted separately): 5 
            num-rests: 0
            duration-qtrs: 4.0 
            duration: 4.0 (4.000)

|#
;;; SYNOPSIS
(defmethod get-sequenz ((s section) player seq-num) ; 1-based
;;; ****
  (let ((player-section (get-data player s))) ; issues warning if none
    (when player-section 
        (get-nth (1- seq-num) player-section)))) ; issues warning if none

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 12:31:29 BST 2012: Added robodoc entry

;;; ****m* section/num-sequenzes
;;; DESCRIPTION
;;; Get the number of sequenz objects in a given section object.
;;; 
;;; ARGUMENTS
;;; - A section object.
;;; 
;;; RETURN VALUE
;;; An integer that is the number of sequenz objects in the specified section
;;; object. 
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((f3 g3 a3 b3 c4))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1 1))))
                        (2 ((vc (1 1 1 1 1))))
                        (3 ((vc (1 1 1 1 1))))))))
  (num-sequenzes (get-section mini 2)))

=> 5

|#
;;; SYNOPSIS
(defmethod num-sequenzes ((s section))
;;; ****
  ;; the data is a list of player-sections
  (sclist-length (first (data s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May 30 21:34:53 BST 2012: Added robodoc entry

;;; bar-num is 1-based!

;;; N.B. although optional, the player argument is required.  It is optional so
;;; that we can have a sequenz method with the same name which only requires
;;; the bar-num argument. 

;;; ****m* section/get-bar
;;; DESCRIPTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod get-bar ((s section) bar-num &optional player)
;;; ****
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

;;; SAR Sat May 19 15:31:53 EDT 2012: Added robodoc entry

;;; ****m* section/get-all-players
;;; DESCRIPTION
;;; Return a list of the IDs from all players in a section object. NB: When
;;; retrieving from a section within a slippery-chicken object, all players in
;;; the ensemble will be returned, as the slippery-chicken object will generate
;;; rest bars for players even when they're not active in a given section.
;;; 
;;; ARGUMENTS
;;; - A section object.
;;; 
;;; RETURN VALUE
;;; - A list of player IDs (symbols).
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
		     (hn (french-horn :midi-channel 2))
		     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
		   (2 (1 1 1 1 1))
		   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
				:pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
			    (hn (1 1 1 1 1))
			    (vc (1 1 1 1 1))))
			(2 ((cl (1 1 1 1 1))
			    (vc (1 1 1 1 1))))
			(3 ((hn (1 1 1 1 1))
			    (vc (1 1 1 1 1))))))))
  (print (get-all-players (get-section mini 1)))
  (print (get-all-players (get-section mini 2)))
  (print (get-all-players (get-section mini 3))))

=>
(CL HN VC) 
(CL HN VC) 
(CL HN VC) 

|#
;;; SYNOPSIS
(defmethod get-all-players ((s section))
;;; ****
  (loop for player-section in (data s) collect
        (id player-section)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 31 18:46:06 BST 2012: Added robodoc entry

;;; ****m* section/re-bar
;;; DESCRIPTION
;;; Regroup the consecutive note and rest events of a given section object into
;;; new bars of the specified time signature. 
;;;
;;; This method will only combine short bars into longer ones; it will not
;;; split up longer bars and recombine them. 
;;;
;;; The method will also use the specified (or default) time signature as a
;;; target, and may be forced to create a number of bars that are not of the
;;; specified time signature if the number of beats in the given section object
;;; do not correspond.
;;;
;;; NB: The user must call the update-slots method after using this method as a
;;;     post-generation editing method.
;;; 
;;; ARGUMENTS
;;; - A section object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. An integer that is the first bar within the specified section
;;;   that is to be re-barred. Default = First bar of the given section.
;;; - :end-bar. An integer that is the last bar within the specified section
;;;   that is to be re-barred. Default = Last bar of the given section.
;;; - :min-time-sig. The target time signature for all new bars. NB: Depending
;;;   on the number of beats in the given section, the method may have to
;;;   deviate from this target time signature. Default = '(2 4).
;;; - :verbose. T or NIL to indicate whether to print feedback on the
;;;   re-barring process to the Lisp listener. Default = NIL.
;;; - :auto-beam.  T, NIL, or an integer. If T, the method will automatically
;;;   attach beam indications to the corresponding events according to the beat
;;;   unit of the time signature. If an integer, the method will beam in
;;;   accordance with a beat unit that is equal to that integer. If NIL, the
;;;   method will not automatically place beams. Default = T.
;;;
;;; RETURN VALUE
;;; T.
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod re-bar ((s section)
                   &key start-bar 
                   end-bar
                   (min-time-sig '(2 4))
                   verbose
                   ;; could also be a beat rhythmic unit
                   (auto-beam t))
;;; ****
  (if (has-subsections s)
      (loop for sub-section in (data s) do
           (re-bar sub-section :start-bar start-bar :end-bar end-bar
                   :min-time-sig min-time-sig :verbose verbose))
      ;; looping for each player means we do a lot of the detection arithmetic
      ;; not once but once for each player, but it's necessary for the glorious 
      ;; future when we might have different meters for different instruments.
      (progn 
        ;; MDE Sun May  6 21:46:38 2012 
        (unless start-bar
          (setf start-bar (start-bar s)))
        (unless end-bar
          (setf end-bar (end-bar s)))
        (loop 
           with first-time-sigs
           with this-time-sigs
           for player-section in (data s) do
           ;; MDE Thu Feb  9 11:43:51 2012 -- fixed the logic here
	     (when (and (<= start-bar (end-bar s))
			(>= end-bar (start-bar s)))
	       (setf this-time-sigs
		     (re-bar player-section 
			     :start-bar (max start-bar (start-bar
							player-section)) 
			     :end-bar (min end-bar (end-bar player-section)) 
			     :min-time-sig min-time-sig :verbose verbose 
			     :auto-beam auto-beam))
	       (if first-time-sigs
		   (unless (equal first-time-sigs this-time-sigs)
		     (warn "section::re-bar: not all time-sigs are the same! ~ 
                         ~%first: ~a ~%this:  ~a"
			   first-time-sigs this-time-sigs))
		   (setf first-time-sigs this-time-sigs))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF section.lsp
