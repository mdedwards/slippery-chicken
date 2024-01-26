;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             nouveau-reich.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          This file contains the data associated with the slippery
;;;                   chicken User Guide tutorial for Intra-Phrasal Looping.
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    25th July 2012
;;;
;;; $$ Last modified: 17:10:18 Fri Jul 20 2012 CEST
;;;
;;; SVN ID: $Id: nouveau-reich.lsp 3406 2013-01-28 15:13:21Z medward2 $
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

;;; For detailed comments on the code in this file, please see:
;;; doc/manual/nouveau-reich.html

;;; NB: Evaluating this code in SBCL may result in a heap exhaustion error if
;;;     run three or four times in a row. This appears to be an SBCL issue, as
;;;     the same code does not produce this error in other Lisps with which it
;;;     has been tested.

(defun move-first-to-end (list)
  (let (i)
    (setf i (pop list))
    (setf list (append list (list i)))))

(defun collect-n-rotations (num-rotations list-to-rotate)
  (loop repeat num-rotations
     collect list-to-rotate
     do (setf list-to-rotate (move-first-to-end list-to-rotate))))

(defun auto-curve-from-indices-and-items (indices items)
  (when (or (<= (apply #'min indices) 0)
            (> (apply #'max indices) (length items)))
    (error "all indices must be >0 and <= length of items"))
  (loop for pos in indices
     for x from 1
     collect x
     collect (nth (1- pos) items)))

(let* ((num-bars 646)
       (set-pal '((1 ((fs2 b2 d4 a4 d5 e5 a5 d6)))
                  (2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                  (3 ((cs3 fs3 e4 a4 e5 a5 e6)))
                  (4 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e6)))
                  (5 ((d2 a2 e4 fs4 gs4 b4 e5 b5)))
                  (6 ((a2 e3 e4 fs4 gs4 b4 cs5 e5 b5)))
                  (7 ((cs3 fs3 fs4 gs4 a4 cs5 a5 cs6)))
                  (8 ((fs2 cs3 fs4 gs4 a4 b4 cs5 fs5)))
                  (9 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6)))
                  (10 ((d2 a2 fs4 gs4 a4 e5 a5 e6)))
                  (11 ((a2 d2 e4 fs4 a4 e5 a5)))))
       (fib-trans-ids (fibonacci-transitions 
                       num-bars
                       (loop for i from 1 to (length set-pal) collect i)))
       (set-limits-lists 
        (loop for pchs in '(((pno-one (a4 a4 e4 b4 fs4 gs4 gs4 gs4 b4 gs4
                                          e4)))
                            ((pno-two (d5 a4 e5 b4 gs4 gs4 a4 a4 gs4 a4
                                          a4))))  
           collect (loop for inst in pchs
                      collect (list (first inst)
                                    (auto-curve-from-indices-and-items 
                                     fib-trans-ids (second inst))))))
       (basic-bar '(((6 8) - s s s s s s - - s s s s s s -)))
       (ps-orig '(1 2 3 4 5 2 1 4 3 2 5 4))
       (ps-list (fibonacci-transitions num-bars 
                                       (collect-n-rotations 13 ps-orig)))
       (rsp (loop for rs from 1 to 2
               for psp in (list (list ps-orig) ps-list)
               collect (list rs (list basic-bar :pitch-seq-palette psp)))) 
       (rsm `((1 ,(loop for p in '(pno-one pno-two)
                     for rs from 1
                     collect (list p (loop repeat num-bars collect rs)))))) 
       (nouveau-reich
        (make-slippery-chicken
         '+nouveau-reich+
         :title "Nouveau Reich"
         :ensemble '(((pno-one (piano :midi-channel 1))
                      (pno-two (piano :midi-channel 2))))
         :staff-groupings '(1 1)
         :tempo-map '((1 (q. 72)))
         :set-palette set-pal
         :set-limits-low (first set-limits-lists)
         :set-limits-high (second set-limits-lists)
         :avoid-used-notes nil
         :avoid-melodic-octaves nil
         :set-map `((1 ,fib-trans-ids))
         :rthm-seq-palette rsp
         :rthm-seq-map rsm)))  
  (loop for p in '(pno-one pno-two)
     for n in '(("piano one" "pno i") ("piano two" "pno ii"))
     with e = (ensemble nouveau-reich)
     do 
       (setf (staff-name (get-data-data p e)) (first n))
       (setf (staff-short-name (get-data-data p e)) (second n)))
  (midi-play nouveau-reich :midi-file "/tmp/nouveau-reich.mid")
  #+cmn (cmn-display nouveau-reich :file "/tmp/nouveau-reich")
  (write-lp-data-for-all nouveau-reich))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
