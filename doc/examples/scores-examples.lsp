;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             scores.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany scores.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    17th July 2012
;;;
;;; $$ Last modified: 22:50:36 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: scores-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;; scores.html examples
;;; ============================================================================

;;;  the header
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+new-piece+
         :title "A Slippery Chicken Piece"
         :composer "Joe Green"
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((f4 g4 a4))))
         :set-map '((1 (1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2))))) 
         :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  starting key signature
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((ef4 f4 g4 af4 bf4 c5 d5 ef5))))
         :key-sig '(ef major)
         :avoid-melodic-octaves nil
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  key changes
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :key-sig '(ef major)
         :avoid-melodic-octaves nil
         :set-palette '((1 ((ef4 f4 g4 af4 bf4 c5 d5 ef5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (add-mark-before-note mini 2 1 'vn '(key af major))
  (add-mark-before-note mini 3 1 'vn '(key a major))
  (cmn-display mini)
  (add-mark-to-note mini 2 8 'vn '(key a major))
  (add-mark-to-note mini 1 8 'vn '(key af major))
  (write-lp-data-for-all mini)
  (midi-play mini))
  
;;;  score order
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A Slippery Chicken Piece"
         :composer "Joe Green"   
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (hn (french-horn :midi-channel 3))
                      (tp (b-flat-trumpet :midi-channel 4))
                      (vn (violin :midi-channel 5))
                      (va (viola :midi-channel 6))
                      (vc (cello :midi-channel 7))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-limits-high '((fl (0 c6 100 c6))
                            (ob (0 b4 100 b4))
                            (hn (0 e4 100 e4))
                            (tp (0 d5 100 d5))
                            (vn (0 g5 100 g5))
                            (va (0 e3 100 e3))
                            (vc (0 e3 100 e2)))
         :set-limits-low '((fl (0 a5 100 a5))
                           (ob (0 g4 100 g4))
                           (hn (0 c4 100 c4))
                           (tp (0 f4 100 f4))
                           (vn (0 e5 100 e5))
                           (va (0 c3 100 c3))
                           (vc (0 c2 100 c2)))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2))))) 
         :rthm-seq-map '((1 ((fl (1 1 1 1))
                             (ob (1 1 1 1))
                             (hn (1 1 1 1))
                             (tp (1 1 1 1))
                             (vn (1 1 1 1))
                             (va (1 1 1 1))
                             (vc (1 1 1 1))))))))
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (midi-play mini))
  
;;;  staff groups
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A Slippery Chicken Piece"
         :composer "Joe Green"   
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (hn (french-horn :midi-channel 3))
                      (tp (b-flat-trumpet :midi-channel 4))
                      (vn (violin :midi-channel 5))
                      (va (viola :midi-channel 6))
                      (vc (cello :midi-channel 7))))
         :staff-groupings '(2 2 3)
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-limits-high '((fl (0 c6 100 c6))
                            (ob (0 b4 100 b4))
                            (hn (0 e4 100 e4))
                            (tp (0 d5 100 d5))
                            (vn (0 g5 100 g5))
                            (va (0 e3 100 e3))
                            (vc (0 e3 100 e2)))
         :set-limits-low '((fl (0 a5 100 a5))
                           (ob (0 g4 100 g4))
                           (hn (0 c4 100 c4))
                           (tp (0 f4 100 f4))
                           (vn (0 e5 100 e5))
                           (va (0 c3 100 c3))
                           (vc (0 c2 100 c2)))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2))))) 
         :rthm-seq-map '((1 ((fl (1 1 1 1))
                             (ob (1 1 1 1))
                             (hn (1 1 1 1))
                             (tp (1 1 1 1))
                             (vn (1 1 1 1))
                             (va (1 1 1 1))
                             (vc (1 1 1 1))))))))
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (midi-play mini))
  
;;;  bars per system - cmn only
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A Slippery Chicken Piece"
         :ensemble '(((fl (flute :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :bars-per-system-map '((1 1) (2 2) (3 3) (7 4) (11 5))
         :set-palette '((1 ((a5 b5 c6))))
         :set-map (list (list 1 (loop repeat 15 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2))))) 
         :rthm-seq-map (list (list 1 
                                   (list (list 'fl (loop repeat 15 
                                                      collect 1))))))))
  (cmn-display mini :auto-bar-nums 5)
  (midi-play mini))
  
;;;  bar line types
;;; ----------------------------------------------------------------------------
(let* ((bar-lines-piece
        (make-slippery-chicken
         '+bar-lines-piece+
         :title "bar-lines piece"
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -)))))
         :rthm-seq-map '((1 ((fl (1 1 1))))))))
  (change-bar-line-type bar-lines-piece 1 1)
  (change-bar-line-type bar-lines-piece 3 5)
  (midi-play bar-lines-piece)
  (cmn-display bar-lines-piece)
  (write-lp-data-for-all bar-lines-piece :base-path "/tmp/"))
  
;;;  rehearsal letters - keyword
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A Slippery Chicken Piece"
         :composer "Joe Green"
         :tempo-map '((1 (q 72)))
         :rehearsal-letters '(3 6 10)
         :ensemble '(((fl (flute :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((a5 b5 c6))))
         :set-map (list (list 1 (loop repeat 12 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2))))) 
         :rthm-seq-map (list (list 1 
                                   (list (list 'fl (loop repeat 12 
                                                      collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))    
  
;;;  rehearsal letters - post-generation editing method
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A Slippery Chicken Piece"
         :composer "Joe Green"
         :tempo-map '((1 (q 72)))
         :ensemble '(((fl (flute :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((a5 b5 c6))))
         :set-map (list (list 1 (loop repeat 12 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
         :rthm-seq-map (list (list 1 
                                   (list (list 'fl (loop repeat 12 
                                                      collect 1))))))))
  (set-rehearsal-letter mini 3 'A)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))    
  
;;;  rehearsal letters in all parts
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A Slippery Chicken Piece"
         :composer "Joe Green"
         :tempo-map '((1 (q 72)))
         :rehearsal-letters '(3 6 10)
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (hn (french-horn :midi-channel 3))
                      (tp (b-flat-trumpet :midi-channel 4))
                      (vn (violin :midi-channel 5))
                      (vc (cello :midi-channel 6))))
         :staff-groupings '(2 2 2)
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map (list (list 1 (loop repeat 12 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2))))) 
         :rthm-seq-map (list (list 1 
                                   (loop for p in '(fl ob hn tp vn vc)
                                      collect (list p 
                                                    (loop repeat 12 
                                                       collect 1))))))))
  (midi-play mini)
  (cmn-display mini :rehearsal-letters-all-players t)
  (write-lp-data-for-all mini :rehearsal-letters-all-players t))    

;;;  disabling auto-clefs calls to cmn-display and write-lp-data-for-all
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((vc (cello :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8)))) 
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (midi-play +sc-object+)
  (cmn-display +sc-object+ :file "/tmp/mini.eps" :auto-clefs nil)
  (write-lp-data-for-all +sc-object+ :base-path "/tmp/" :auto-clefs nil))

;;;  auto-clefs as post-generation editing method
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((vc (cello :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (auto-clefs +sc-object+)
  (midi-play +sc-object+)
  (cmn-display +sc-object+ :file "/tmp/mini.eps" :auto-clefs nil)
  (write-lp-data-for-all +sc-object+ :base-path "/tmp/" :auto-clefs nil)) 
  
;;;  add-clef
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vc (cello :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (add-clef mini 'vc 2 2 'tenor)
  (add-clef mini 'vc 3 3 'treble)
  (midi-play +sc-object+)
  (cmn-display mini :file "/tmp/mini.eps" :auto-clefs nil)
  (write-lp-data-for-all mini :base-path "/tmp/" :auto-clefs nil))
  
;;;  delete-clefs
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vc (cello :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (add-clef mini 'vc 2 2 'tenor)
  (add-clef mini 'vc 3 3 'treble)
  (delete-clefs mini 'vc 2 2)
  (delete-clefs mini 'vc 3 3)
  (midi-play +sc-object+)
  (cmn-display mini :file "/tmp/mini.eps" :auto-clefs nil)
  (write-lp-data-for-all mini :base-path "/tmp/" :auto-clefs nil))
  
;;;  c-scores
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (cl (b-flat-clarinet :midi-channel 2))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((b4 c5 d5 e5 f5 g5 a5 b5 c6 d6 e6 f6 g6))))
         :set-limits-high '((cl (0 g5 100 g5)))
         :set-limits-low '((fl (0 a5 100 a5)))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((fl (1 1 1 1))
                             (cl (1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini :in-c t)
  (write-lp-data-for-all mini :in-c t))
  
;;;  parts
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((flt (flute :midi-channel 1))
                      (clr (b-flat-clarinet :midi-channel 2))
                      (hrn (french-horn :midi-channel 3))
                      (tpt (b-flat-trumpet :midi-channel 4))
                      (tbn (tenor-trombone :midi-channel 5))
                      (tba (tuba :midi-channel 6))
                      (vln (violin :midi-channel 7))
                      (vla (viola :midi-channel 8))
                      (vlc (cello :midi-channel 9))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((c1 d1 e1 f1 g1 a1 b1
                                c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 
                                c6 d6 e6 f6 g6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8)))) 
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((flt (1 1 1))
                             (clr (1 1 1))
                             (hrn (1 1 1))
                             (tpt (1 1 1))
                             (tbn (1 1 1))
                             (tba (1 1 1))
                             (vln (1 1 1))
                             (vla (1 1 1))
                             (vlc (1 1 1))))))))
  (cmn-display mini :file "/tmp/mini.eps" :players '(tbn))
  (write-lp-data-for-all mini))
  
;;;  sectional scores
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((flt (flute :midi-channel 1))
                      (clr (b-flat-clarinet :midi-channel 2))
                      (hrn (french-horn :midi-channel 3))
                      (tpt (b-flat-trumpet :midi-channel 4))
                      (tbn (tenor-trombone :midi-channel 5))
                      (tba (tuba :midi-channel 6))
                      (vln (violin :midi-channel 7))
                      (vla (viola :midi-channel 8))
                      (vlc (cello :midi-channel 9))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((c1 d1 e1 f1 g1 a1 b1
                                c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 
                                c6 d6 e6 f6 g6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7
                                                       8)))) 
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((flt (1 1 1))
                             (clr (1 1 1))
                             (hrn (1 1 1))
                             (tpt (1 1 1))
                             (tbn (1 1 1))
                             (tba (1 1 1))
                             (vln (1 1 1))
                             (vla (1 1 1))
                             (vlc (1 1 1))))))))
  (midi-play mini)
  (cmn-display mini :file "/tmp/mini.eps" :players '(hrn tpt tbn tba))
  (write-lp-data-for-all mini :base-path "/tmp/" :players '(hrn tpt tbn tba)))
