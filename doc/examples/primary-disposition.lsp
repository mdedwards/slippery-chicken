;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             primary-disposition.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          This file contains the data associated with the slippery
;;;                   chicken User Guide tutorial on Core Usage.
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    23rd November 2011
;;;
;;; $$ Last modified:  15:17:45 Wed Jan 12 2022 CET
;;;
;;; SVN ID: $Id: primary-disposition.lsp 5531 2016-01-28 15:25:28Z medward2 $
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
;;; doc/manual/coreUsage.html

(in-package :sc)

(in-scale :chromatic)

(make-slippery-chicken  
 '+primary-disposition+ 
 :title "Primary Disposition" 
 :instrument-palette +slippery-chicken-standard-instrument-palette+ 
 :ensemble '(((flt (flute :midi-channel 1))
              (clr (b-flat-clarinet :midi-channel 2))
              (vln-one (violin :midi-channel 3)) 
              (vla (viola :midi-channel 4)) 
              (cel (cello :midi-channel 5))))
 :staff-groupings '(2 3)
 :tempo-map '((1 (q 84)))
 :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                (set3 ((cs3 fs3 e4 a4 e5 a5 e6))) 
                (set4 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e6)))) 
 :set-map '((1 (set1 set1 set2 set1 set2)) 
            (2 (set2 set2 set3 set2 set3 set2 set3)) 
            (3 (set3 set2 set4 set2 set4 set3 set4 set4 set2 set3 set4))
            (4 (set4 set1 set4 set1 set4 set1 set1)))   
 :set-limits-high '((vla (0 b4 100 b4))
                    (cel (0 f4 100 f4)))
 :rthm-seq-palette
 '((seq1 ((((4 4) - 16 16 8 - { 5 - 20 10 20 20 - } { 3 3 6 } )   
           ( - s s s s - (s) - s s s - - +e. s - q))   
          :pitch-seq-palette (1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
          :marks (mp 1 a 1 s 3 slur 1 3 a 4 slur 4 7 s 9 a 10 slur 16 18)))  
   (seq2 ((((4 4) - e e - - s s s s - (s) s (s) s { 3 - te te te - } )  
           ( - s s s - (s)  { 3 - te te - (te) } q \+8 (e) )) 
          :pitch-seq-palette (11 10 9 11 10 8 7 11 10 6 5 11 10 9 8 7 11) 
          :marks (mf 1 s 1 2 a 3 slur 3 4 a 5 slur 5 6 s 7 s 8 a 9 
                     slur 9 11 a 12 slur 12 14 s 15 16 a 17))) 
   (seq3 ((((4 4) { 3 - te te te - } - e e - - s s s s - (s) s (s) s ) 
           (- s s s s - - s s s s - { 3 - te te te - } { 3 - +te te te - } ))
          :pitch-seq-palette (1 2 1 2 3 1 2 3 4 5 6 1 2 3 5 4 4 4 2 7 5 4 5 6)   
          :marks (f 1 slur 1 3 a 4 s 4 5 slur 6 7 slur 8 9 s 10 11 
                    slur 12 13 slur 14 15 a 16 slur 18 19 slur 20 21 a 22 
                    slur 24 25)))  
   (seq4 ((((4 4) - s s (s) s - - s s - (e) - e s - (s) { 3 - te te te - } )
           ( { 5 - 10 10 20 - } - +s s s s - (e) - e+32 32 s s s - ))
          :pitch-seq-palette (4 5 4 4 5 4 5 4 7 13 
                                17 15 14 15 11 12 13 4 5 7 8) 
          :marks (ff 1 slur 1 2 s 3 slur 4 5 a 6 slur 8 10 s 10 a 11 s 11 
                     a 12 s 12 slur 13 15 slur 16 17 a 18 slur 19 22))))
 :rthm-seq-map
 '((1 ((flt (seq1 seq1 seq2 seq1 seq2)) 
       (clr (seq1 seq2 seq1 seq2 seq1)) 
       (vln-one (seq1 seq1 seq2 seq1 seq2))   
       (vla (seq1 seq2 seq1 seq2 seq1))   
       (cel (seq1 seq1 seq2 seq1 seq2))))
   (2 ((flt (seq1 seq1 seq2 seq1 seq2 seq1 seq2))
       (clr (seq1 seq1 seq3 seq1 seq3 seq1 seq3))
       (vln-one (seq2 seq2 seq3 seq2 seq3 seq2 seq3))  
       (vla (seq2 seq2 seq1 seq2 seq1 seq2 seq1))  
       (cel (seq3 seq3 seq2 seq3 seq2 seq3 seq2))))  
   (3 ((flt (seq1 seq1 seq2 seq1 seq2 seq1 seq2 seq2 seq1 seq2 seq2))
       (clr (seq2 seq2 seq3 seq2 seq3 seq2 seq3 seq3 seq2 seq3 seq3))
       (vln-one (seq3 seq3 seq4 seq3 seq4 seq3 seq4 seq4 seq3 seq4 seq4)) 
       (vla (seq4 seq4 seq3 seq4 seq3 seq4 seq3 seq3 seq4 seq3 seq3))  
       (cel (seq4 seq4 seq2 seq4 seq2 seq4 seq2 seq2 seq4 seq2 seq2))))  
   (4 ((flt (seq4 seq3 seq4 seq1 seq4 seq1 seq1))  
       (clr (seq3 seq2 seq3 seq2 seq1 seq2 seq1))
       (vln-one (seq2 seq1 seq1 seq1 seq2 seq1 seq1)) 
       (vla (seq4 seq3 seq4 seq2 seq1 seq1 seq1)) 
       (cel (seq2 seq2 seq2 seq1 seq2 seq1 seq1))))))

(midi-play +primary-disposition+ :midi-file "/tmp/primary-disposition.mid")

#+cmn ;; <-- that means only run the next Lisp form if that CMN package is
      ;; available  
(cmn-display +primary-disposition+ :file "/tmp/primary-disposition.eps")

;; (write-lp-data-for-all +primary-disposition+ :base-path "/tmp/")
(lp-display +primary-disposition+ :base-path "/tmp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF primary-disposition.lsp
