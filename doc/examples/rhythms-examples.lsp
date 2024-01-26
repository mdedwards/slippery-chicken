;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             rhythms-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany rhythms.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    17th July 2012
;;;
;;; $$ Last modified: 22:49:31 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: rhythms-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;; rhythms.html examples
;;; ============================================================================

;;;  durations - numeric
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) 1)
                                  (2 4 8 16 32 32)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  durations - alphabetic
;;; ----------------------------------------------------------------------------  
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) w)
                                  (h q e s s)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  rests - numeric
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) h (q) e (16) 16)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  rests - alphabetic
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) (s) (s) e s (s) (s) (s) (e) (e)
                                   q)))))   
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  dots
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) e. s (e..) 32 (8\.) 16 8\.. 32))))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  ties
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) q+e e 4+8 8)
                                  (+q +e e 4 \+8 8)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  tuplets - numeric - without brackets
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) - 12 12 - (12) - 20 20 (10) 20
                                   -))))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  tuplets - alphabetic - without brackets
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) - te t8 - (te) - fs fs (f8) f16
                                   -))))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  tuplets - partial beats - no brackets
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) te e ts s ts s ts ts)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  tuplets - brackets and numbers
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) { 3 te te (te) } 
                                   { 5 - fs fs (f8) f16 - } )))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  tuplets - brackets and numbers - partial beats
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) s { 3 te ts } e { 3 t32 t32 } s  
                                   { 3 ts ts }))))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  tuplets - nested
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) { 3 12 12 12 } 
                                   { 3  12 12 { 3 36 36 36 } } ))))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  beams
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e - - s s s s - e - s s - 
                                   (s) - s (s) s -)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  repeat rhythms shorthand
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) s x 16)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
