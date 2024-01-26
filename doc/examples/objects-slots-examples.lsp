;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             objects-slots-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany objects-slots.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 20:36:22 Tue May 28 2013 BST
;;;
;;; SVN ID: $Id: objects-slots-examples.lsp 3564 2013-05-28 20:02:29Z medward2 $
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
;;; objects-slots.html examples
;;; ============================================================================

(in-scale :quarter-tone)

;;; making an instance of a class object
;;; ----------------------------------------------------------------------------
(let* ((al (make-instance 'assoc-list :id 'al-examp 
                          :data '((3 17) (ob bf3) (c4 q)))))
  (print al))

;;; get-first method of class assoc-list
;;; ----------------------------------------------------------------------------
(let* ((al-object-1 
        (make-instance 'assoc-list 
                       :id 'al-object-1 
                       :data '((3 17) (ob bf3) (c4 q)))))
  (print (get-first al-object-1))
  (print  (id (get-first al-object-1)))
  (print (data (get-first al-object-1))))

;;; functions with keyword arguments
;;; ----------------------------------------------------------------------------
(print (get-harmonics 63 :start-partial 2 :max-freq 1010))
      
;;; make-functions
;;; ----------------------------------------------------------------------------
(let* ((r (make-rhythm 16 :is-rest t)))
  (print r))

;;; make-slippery-chicken mini
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 e4 g4))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
