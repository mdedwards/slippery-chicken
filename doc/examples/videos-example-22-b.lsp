;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-22-b.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 22
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    22nd December 2012
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

(let* ((rch
        (make-rthm-chain
         'test-rch 37
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s))
           (({ 3 (te) te (te) }) ; what we transition to
            ({ 3 - te (te) te - })
            ({ 3 (te) - te te - })
            ({ 3 (te) (te) te })))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.))
            (({ 3 te+te te+te te+te }) ; what we transition to
             (q - s e. -)
             (q (s) e.)
             (q (s) - s e -)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))
            (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
             (- e. s - (q) (s) - s e -)
             ({ 3 te+te te } (q) q)
             ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
         :players '(fl cl))))
  (add-voice rch '(1 fl) 'ob)
  (create-psps (palette rch))
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))
                        (cl (b-flat-clarinet :midi-channel 3))))
           :set-limits-high '((fl (0 a5 50 c7 100 a5))
                              (cl (0 d5 50 a5 100 d5)))
           :set-limits-low '((fl (0 a4 50 a5 100 a4))
                             (cl (0 e3 50 d5 100 c4)))
           :tempo-map '((1 (q 72)))
           :set-palette '((1 ((cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    (tie-over-rests mini 1 2 'cl :consolidate-notes nil)
    (handle-ties mini)
    (re-bar mini :min-time-sig '(4 4))
    (map-over-bars mini nil nil nil #'consolidate-notes)
    (map-over-bars mini nil nil nil #'consolidate-rests-max)
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF