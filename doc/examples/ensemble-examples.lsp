;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             ensemble-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany ensemble.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 22:42:02 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: ensemble-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;;  ensemble.html examples
;;; ============================================================================

;;;  one instrument per player
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((fl (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  doubling instruments (with instrument-change-map)
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (cl ((b-flat-clarinet bass-clarinet) 
                           :midi-channel 2)))) 
         :instrument-change-map '((1 ((fl ((1 flute)))
                                      (cl ((1 b-flat-clarinet))))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((fl (1))
                             (cl (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  microtonal midi channels (with instrument-change-map)
;;; ----------------------------------------------------------------------------  
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl ((alto-flute piccolo) :midi-channel 1 
                           :microtones-midi-channel 2))
                      (cl ((b-flat-clarinet bass-clarinet) 
                           :midi-channel 3  
                           :microtones-midi-channel 4))
                      (hn (french-horn :midi-channel 5 
                                       :microtones-midi-channel 6))
                      (perc (marimba :midi-channel 7))
                      (solo (violin :midi-channel 8 
                                    :microtones-midi-channel 9)) 
                      (vln (violin :midi-channel 11 
                                   :microtones-midi-channel 12)) 
                      (vla (viola :midi-channel 13 
                                  :microtones-midi-channel 14))
                      (vc (cello :midi-channel 15 
                                 :microtones-midi-channel 16))))
         :instrument-change-map '((1 ((fl ((1 piccolo)))
                                      (cl ((1 b-flat-clarinet))))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((fl (1))
                             (cl (1))
                             (hn (1))
                             (perc (1))
                             (solo (1))
                             (vln (1))
                             (vla (1))
                             (vc (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  instrument-change-map
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl ((flute piccolo) 
                           :midi-channel 1 :microtones-midi-channel 2))
                      (cl ((b-flat-clarinet bass-clarinet) 
                           :midi-channel 3 :microtones-midi-channel 4))))
         :instrument-change-map '((1 ((fl ((1 flute) (3 piccolo) 
                                           (5 flute)))
                                      (cl ((1 b-flat-clarinet) 
                                           (2 bass-clarinet) 
                                           (6 b-flat-clarinet)))))
                                  (2 ((fl ((2 piccolo) (4 flute)))
                                      (cl ((2 bass-clarinet) 
                                           (3 b-flat-clarinet))))))
         :set-palette '((1 ((d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((fl (1 1 1 1 1 1 1))
                             (cl (1 1 1 1 1 1 1))))
                         (2 ((fl (1 1 1 1 1 1 1))
                             (cl (1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  instruments-hierarchy
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1 ))
                      (cl (b-flat-clarinet :midi-channel 3))
                      (hn (french-horn :midi-channel 5))
                      (perc (marimba :midi-channel 7))
                      (solo (violin :midi-channel 8)) 
                      (vln (violin :midi-channel 11)) 
                      (vla (viola :midi-channel 13))
                      (vc (cello :midi-channel 15))))
         :instruments-hierarchy '(solo vln fl cl vla hn perc vc)
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((fl (1))
                             (cl (1))
                             (hn (1))
                             (perc (1))
                             (solo (1))
                             (vln (1))
                             (vla (1))
                             (vc (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
