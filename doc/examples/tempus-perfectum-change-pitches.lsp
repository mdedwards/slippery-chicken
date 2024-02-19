;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             tempus-perfectum-change-pitches.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          To show how pitches can be changed algorithmically using
;;;                   post-generation techniques. 
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    February 19th 2024
;;;
;;; $$ Last modified:  11:49:10 Mon Feb 19 2024 CET
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
(in-package :sc)

;;; First let's generate an existing piece that we can clone and then act
;;; upon. Comment out and load tempus-perfectum.lsp by hand if working in
;;; another directory.
(load-from-same-dir "tempus-perfectum.lsp")
;;; To avoid warnings about globals in the code below.
(proclaim '(special +tempus-perfectum+))

(defmethod tendency-unison ((sc slippery-chicken)
                            &optional
                            (main-player 'ob)
                            ;; x values can range over whatever you like but y
                            ;; values are expressed as a percentage where 0 = no
                            ;; pitch change and 100 = definite pitch
                            ;; change. change 'em all by default, just to prove
                            ;; it works.
                            (change-chance-env '(0 100 100 100)))
  (let* ((all-events (get-events-sorted-by-time sc))
         (chance 0.0)
         (do-it nil)
         (main-player-note nil)
         (event-count 0)
         (changed-count 0))
    ;; we've used an x-axis from 0-100 (or whatever), but need to stretch this
    ;; over all the notes in the whole piece (including, for simplicity's sake
    ;; the main-player
    (setq change-chance-env (new-lastx change-chance-env (num-notes sc)))
    (loop for event in all-events do
             (when (needs-new-note event) ; only process attacked notes
               (if (eq main-player (player event))
                   ;; for simplicity's sake we'll just grab the last main-player
                   ;; note and use that to change subsequent non-main-player
                   ;; notes, accepting or even enjoying that, depending on
                   ;; rhythmic structure, we might be a little behind.
                   (setq main-player-note (pitch-or-chord event))
                   ;; don't change main-player's notes, and only change other
                   ;; player's notes if we've seen a main-player note already
                   (when main-player-note
                     ;; get the chance of changing 
                     (setq chance (interpolate event-count change-chance-env)
                           ;; determine whether to change the note to the
                           ;; main-player's note using fixed-seed
                           ;; randomness. Note that we reset the random
                           ;; generator when event-count is 0, i.e. on the first
                           ;; time through the loop. Note also the less-than
                           ;; test assumes that the Y values of the
                           ;; change-chance-env range from 0 to 100
                           do-it (< (random-rep 100.0 (zerop event-count))
                                    chance))
                     (when do-it
                       ;; bear in mind that no checks are made here that the
                       ;; instrument we're dealing with can actually play the
                       ;; main player's note! If we wanted to be more
                       ;; sophisticated, then we could use the in-range method
                       ;; and octavise where necessary.
                       (setf (pitch-or-chord event) main-player-note)
                       (incf changed-count))))
               (incf event-count)))
    ;; bear in mind that we've only changed attacked notes' pitches, so any
    ;; tied-to notes will be wrong! Helpfully the check-ties method takes care
    ;; of fixing these :-)
    (check-ties sc t)
    ;; these two for statistics' sake really
    (update-slots sc)
    (update-instrument-slots sc)
    (format t "~&Changed ~a notes." changed-count)
    sc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now actually do it, cloning the original so as not to have to reload, and/or
;;; to enable easier comparisons.
(let ((sc (tendency-unison (clone +tempus-perfectum+))))
  (cmn-display sc :size 10
                  :in-c t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
