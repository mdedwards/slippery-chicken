;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             second-law.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Piano chord selection function used in Michael Edwards's
;;;                   "you are coming into us, who cannot withstand you".  Note
;;;                   the use of static lexical closure before the defun so
;;;                   that the last-chord and cs2 variables retain their values
;;;                   between calls (mainly in order to compare current with
;;;                   previous chords).
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    27th April 2012
;;;
;;; $$ Last modified: 09:13:16 Wed May 23 2012 BST
;;;
;;; SVN ID: $Id: coming-piano-chords.lsp 1940 2012-05-23 08:14:52Z medward2 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2012 Michael Edwards
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

(let ((last-chord (make-chord '(d8 e8)))
      (cs2 (make-pitch 'cs2)))
  (defun coming-piano-chord (curve-num index pitch-list pitch-seq instrument
                             set) 
    (declare (ignore set instrument pitch-seq curve-num))
    (let* ((start (max 0 (- index 3))) 
           (at-start (nth start pitch-list))
           (nump (length pitch-list))
           (lots (> nump 4))
           (down-low (pitch< at-start cs2))
           (result (list at-start)))
      (loop 
         ;; try and get every other note
         for i from (if lots 
                        (if down-low
                            (+ 3 start) ;; 4ths if low 
                            (+ 2 start))
                        (1+ start))
         by (if lots 2 1) 
         ;; 26.2.11 don't get more than 2 notes if we're down low (remember
         ;; at-start is already in there, so we try for 4 notes if > c2
         ;; otherwise 2
         repeat (if (not down-low) 3 2)
         for p = (nth i pitch-list)
         for interval = (when p (pitch- p at-start))
         do
         (when (and p (<= interval 12)
                    (not (member p result :test #'note=))
                    ;; 24.7.11 no low M7ths
                    (and down-low (/= interval 11)))
           (push p result)))
      (if (> (length result) 1)
          (progn 
            (setf result (make-chord result))
            ;; 24.7.11 (Pula) don't have repeated chords or new chords with
            ;; more than 1 common note 
            (if (> (common-notes result last-chord) 1)
                (coming-piano-chord nil (1+ index) pitch-list nil nil nil)
                (setf last-chord result)))
          (first result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF coming-piano-chords.lsp
