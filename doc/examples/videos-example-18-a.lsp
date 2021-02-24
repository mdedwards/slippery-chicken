;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-18-a.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 18
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

(let* ((orig-palette 
        (make-rsp 'orig ; original rthm-seq-palette
                  '((1 ((((1 4) - s e s - ))
                        :pitch-seq-palette ((1 2 3))))
                    (2 ((((1 4) - e. s - ))
                        :pitch-seq-palette ((1 2))))
                    (3 ((((1 4) - (e.) s - ))
                        :pitch-seq-palette ((1)))))))
       (chopped-palette 
        (chop orig-palette ; chopped rthm-seq-palette
              '((1 4) ; chop points  
                (1 3) (2 4) 
                (1 2) (2 3) (3 4) 
                (1 1) (2 2) (3 3) (4 4)) 
              's)) ; chopping unit
       (chop-examp
        (make-slippery-chicken
         '+chop-examp+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4))))
         :set-map '((1 (1 1 1 1 1)))
         :rthm-seq-palette chopped-palette
         :rthm-seq-map '((1 ((vn ((1 1) (1 2) (1 3) (2 1) (3 2))))))))) 
  (midi-play chop-examp)
  (cmn-display chop-examp)
  (write-lp-data-for-all chop-examp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF