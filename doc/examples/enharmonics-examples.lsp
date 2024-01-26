;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             enharmonics-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany enharmonics.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    23rd November 2012
;;;
;;; $$ Last modified: 22:41:46 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: enharmonics-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;; enharmonic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((cs4 fs4 gs4 c5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q - e s s -))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (enharmonic (get-event mini 2 1 'vn))
  (enharmonic (get-event mini 3 4 'vn) :force-naturals t)
  (cmn-display mini :respell-notes nil)
  (write-lp-data-for-all mini :respell-notes nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enharmonics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((fl (flute :midi-channel 1))
                     (ob (oboe :midi-channel 2))))
        :set-palette '((1 ((cs4 fs4 gs4 c5 ds5))))
        :set-limits-low '((fl (0 fs4 100 fs4)))
        :set-limits-high '((ob (0 c5 100 c5)))
        :avoid-used-notes nil
        :set-map '((1 (1 1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q - e s s -))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((fl (1 1 1 1 1 1 1 1))
                            (ob (1 1 1 1 1 1 1 1))))))))
  (enharmonics mini 2 4 'fl)
  (enharmonics mini '(5 1) '(7 2) 'ob :pitches '(fs4 gs4))
  (cmn-display mini :respell-notes nil)
  (write-lp-data-for-all mini :respell-notes nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; respell-notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((fl (flute :midi-channel 1))
                     (ob (oboe :midi-channel 2))))
        :set-palette '((1 ((cs4 fs4 gs4 c5 ds5))))
        :set-limits-low '((fl (0 fs4 100 fs4)))
        :set-limits-high '((ob (0 c5 100 c5)))
        :avoid-used-notes nil
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q - e s s -))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((fl (1 1 1))
                            (ob (1 1 1))))))))
  (respell-notes mini '((fl (2 1) (2 2) (2 3)) 
                        (ob (3 2) (3 3) (3 4))))
  (cmn-display mini :respell-notes nil)
  (write-lp-data-for-all mini :respell-notes nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF enharmonics-examples.lsp