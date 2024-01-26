;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             tempo.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany tempo.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    17th July 2012
;;;
;;; $$ Last modified: 22:51:21 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: tempo-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;; tempo.html examples
;;; ============================================================================

;;;  tempo-map using integer bar numbers
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 60)) (15 (e 72)) 
                      (84 (q. 176 "prestissimo"))) 
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map (list (list 1 (loop repeat 100 collect 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn (loop repeat 100 
                                            collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  tempo-map using section-sequence-bar refs
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 60)) ((2 1 1) (e 72)) 
                      ((3 3 1) (q. 176 "prestissimo")))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map (list (list 1 (loop repeat 5 collect 1))
                        (list 2 (loop repeat 5 collect 1))
                        (list 3 (loop repeat 5 collect 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn (loop repeat 5 
                                            collect 1))))
                        (list 2 
                              (list 
                               (list 'vn (loop repeat 5 
                                            collect 1))))
                        (list 3
                              (list 
                               (list 'vn (loop repeat 5 
                                            collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  tempo-curve
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-curve '(10 q (0 60 30 144 75 52 100 120))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map (list (list 1 (loop repeat 100 collect 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map (list (list 1 
                                   (list 
                                    (list 'vn (loop repeat 100 
                                                 collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
