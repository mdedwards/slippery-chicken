;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             rhythm-chains-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany rhythm-chains.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    23rd November 2012
;;;
;;; $$ Last modified: 14:13:47 Tue Jul  1 2014 BST
;;;
;;; SVN ID: $Id: rhythm-chains-examples.lsp 5028 2014-07-01 13:16:38Z medward2 $
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; procession
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(procession 73 '(a b c d e))

(procession 73 4)

(procession 500 '(a b c d e f g h i j k l m n o p q r s t u v w x y z) 
            :peak 0.9 
            :expt 0.7 
            :orders '((1 3 2 2 3 1) (3 1 1 2 3 3) (1 2 1 3 2 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((rch
        (make-rthm-chain
         'test-rch 143
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s)))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))))
         :players '(fl cl))))
  (create-psps (palette rch))
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (cl (b-flat-clarinet :midi-channel 2))))
           :set-palette '((1 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :tempo-map '((1 (q 120)))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Using fibonacci-transitions instead of procession
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((rch
        (make-rthm-chain
         'test-rch 143
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s)))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))))
         :players '(fl cl)
         :1-beat-fibonacci t
         :slow-fibonacci t)))
  (create-psps (palette rch))
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (cl (b-flat-clarinet :midi-channel 2))))
           :set-palette '((1 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :tempo-map '((1 (q 120)))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An extra level of fibonacci-transitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((rch
        (make-rthm-chain
         'test-rch 143
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s))
           (({ 3 (te) te (te) }) ; what we transition to
            ({ 3 - te (te) te - })
            ({ 3 (te) - te te - })
            ({ 3 (te) (te) te }))
           (((q)) ; the second transition
            (- s e s -)
            ({ 3 te (tq) })
            (s (e.)))
           ((- e e -) ; the third transition
            (- s s s - (s))
            ((32) 32 (e.))
            ((q))))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.))
            (({ 3 te+te te+te te+te }) ; what we transition to
             (q - s e. -)
             (q (s) e.)
             (q (s) - s e -))
            ((q - e e -) ; the second transition
             ((e) q.)
             (q - s (e) s -)
             ((s) q e.)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))
            (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
             (- e. s - (q) (s) - s e -)
             ({ 3 te+te te } (q) q)
             ({ 3 - te te te - } (e) e { 3 (te) (te) te }))
            ((h.) ; the second transition
             (h (q))
             (- e e - +q (q))
             (s (e.) (h)))))
         :players '(fl cl)))
       (mini
        (progn
          (create-psps (palette rch))
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (cl (b-flat-clarinet :midi-channel 2))))
           :set-palette '((1 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :tempo-map '((1 (q 120)))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-voice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((rch
        (make-rthm-chain
         'test-rch 173
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s)))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))))
         :players '(fl cl))))
  (add-voice rch '(1 fl) 'ob)
  (create-psps (palette rch))
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))
                        (cl (b-flat-clarinet :midi-channel 3))))
           :set-limits-high '((fl (0 e6 100 e6))
                              (ob (0 b5 100 b5))
                              (cl (0 d5 100 d5)))
           :set-limits-low '((fl (0 gs5 100 gs5))
                             (ob (0 a4 100 a4))
                             (cl (0 e3 100 e3)))
           :tempo-map '((1 (q 72)))
           :set-palette '((1 ((cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF rhythm-chains-examples.lsp
