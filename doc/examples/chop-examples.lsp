;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             chop-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany chop.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 22:41:11 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: chop-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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

;;; ============================================================================
;;;  chop.html examples
;;; ============================================================================

;;;  straightforward example, no re-bar
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((orig-palette (make-rsp 'orig ; original rthm-seq-palette
                               '((1 ((((1 4) - s e s - ))
                                     :pitch-seq-palette ((1 2 3))))
                                 (2 ((((1 4) - e. s - ))
                                     :pitch-seq-palette ((1 2))))
                                 (3 ((((1 4) - (e.) s - ))
                                     :pitch-seq-palette ((1)))))))
       (chopped-palette (chop orig-palette ; chopped rthm-seq-palette
                              '((1 4) 
                                (1 3) (2 4) 
                                (1 2) (2 3) (3 4) 
                                (1 1) (2 2) (3 3) (4 4)) ; chop points  
                              's)) ; chopping unit
       (chop-examp
        (make-slippery-chicken
         '+chop-examp+
         :ensemble '(((vn (violin :midi-channel 1))))
         :bars-per-system-map '((1 10))
         :set-palette '((1 ((c4 d4 e4))))
         :set-map '((1 (1 1 1 1 1)))
         :rthm-seq-palette chopped-palette
         :rthm-seq-map '((1 ((vn ((1 1) (1 2) (1 3) (2 1) (3 2)))))))))
  (midi-play chop-examp)
  (cmn-display chop-examp)
  (write-lp-data-for-all chop-examp))

;;;  chop with re-bar
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((orig-palette (make-rsp 'orig ; original rthm-seq-palette
                               '((1 ((((1 4) - s e s - ))
                                     :pitch-seq-palette ((1 2 3))))
                                 (2 ((((1 4) - e. s - ))
                                     :pitch-seq-palette ((1 2))))
                                 (3 ((((1 4) - (e.) s - ))
                                     :pitch-seq-palette ((1)))))))
       (chopped-palette (chop orig-palette ; chopped rthm-seq-palette
                              '((1 4) 
                                (1 3) (2 4) 
                                (1 2) (2 3) (3 4) 
                                (1 1) (2 2) (3 3) (4 4)) ; chop points  
                              's)) ; chopping unit
       (chop-examp
        (make-slippery-chicken
         '+chop-examp+
         :ensemble '(((vn (violin :midi-channel 1))))
         :bars-per-system-map '((1 10))
         :set-palette '((1 ((c4 d4 e4))))
         :set-map '((1 (1 1 1 1 1)))
         :rthm-seq-palette chopped-palette
         :rthm-seq-map '((1 ((vn ((1 1) (1 2) (1 3) (2 1) (3 2)))))))))
  (re-bar chop-examp :min-time-sig '(1 4))
  (midi-play chop-examp)
  (cmn-display chop-examp)
  (write-lp-data-for-all chop-examp))