;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             marks-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany marks.html
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 09:13:16 Wed May 23 2012 BST
;;;
;;; SVN ID: $Id: marks-examples.lsp 3406 2013-01-28 15:13:21Z medward2 $
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
;;; marks.html examples
;;; ============================================================================

;;; assigning one rhythm object per mark
;;; ----------------------------------------------------------------------------

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - s s s s - - s s s s - - s s s s - 
                                   - s s s s -))  
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8 8
                                                        7 6 5 4 3 2 1)) 
                                 :marks (a 1 s 3 beg-sl 5 end-sl 6 
                                           dim-beg 9 dim-end 13)))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
    
;;; dynamics are added the same way
;;; ----------------------------------------------------------------------------

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks (ppp 1 cresc-beg 2 cresc-end 4 
                                             fff 5))))  
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; multiple rhythm objects per mark
;;; ----------------------------------------------------------------------------

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks (a 1 4 s 5 7 8)))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; slurs and phrases long form
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks (beg-sl 1 end-sl 2 beg-sl 5 
                                                end-sl 6 
                                                beg-phrase 1 
                                                end-phrase 8))))   
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  

;;; slurs and phrases short form
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)) 
                                 :marks (slur 1 2 slur 5 6
                                              phrase 1 8))))   
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; user-defined text marks
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)) 
                                 :marks ("etwas rascher" 1))))  
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; changing note-heads via :marks; individual notes
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks (x-head 1 triangle-up 5))))  
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; changing note-heads via :marks; series of notes
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - s s s s - - s s s s -
                                   - s s s s - - s s s s -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8 8
                                                        7 6 5 4 3 2 1))
                                 :marks (x-head 1 8))))  
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; interspersing note-heads with other marks
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks (ppp 1 a 1 3 5 s 2 
                                             "like a whisper" 4 
                                             slur 6 7 x-head 3 8))))   
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; lilypond arrows
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))   
         :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (add-arrow-to-events +mini+ "start" "end" '(1 5) '(3 4) 'vn )
  (write-lp-data-for-all mini :base-path "/tmp/"))
  
;;; lilypond marks that use graphics
;;; nb: the downloaded eps files must be in the same folder as the resulting
;;; .ly file.
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) h. (q))
                                  ((q) h.)
                                  ((e) e (q) - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7))
                                 :marks (aeolian-dark 1 mphonic-arr 2
                                                      arrow-up-down 3
                                                      bracket-end 7))))
         :rthm-seq-map '((1 ((fl (1))))))))
  (write-lp-data-for-all mini :base-path "/tmp/" :use-custom-markup t))
  
;;; fingerings
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (add-mark-to-note mini 1 2 'vn 1)
  (add-mark-to-note mini 1 3 'vn 2)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; all marks that work for both cmn and lp
;;; ----------------------------------------------------------------------------
(let* ((marks-list 
        (loop for m in '(a as at arco bartok batt col-legno cl clb clt
                         cresc-beg cresc-end dim-beg dim-end pause beg-gliss
                         end-gliss lhp mv harm open ord beg-8va end-8va beg-8vb
                         end-8vb beg-phrase end-phrase pizz pizzp poco-crini sv
                         short-pause beg-sl end-sl s I II III IV sp ped ped^
                         ped-up te ts t3 uc tc) 
           for r from 1 
           collect m 
           collect r))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -)
                                  (- e e e e - - e e e e -)
                                  (- e e e e - - e e e e -)
                                  (- e e e e - - e e e e -)
                                  (- e e e e - - e e e e -)
                                  (- e e e e - - e e e e -)
                                  (- e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8
                                                        1 2 3 4 5 6 7 8
                                                        1 2 3 4 5 6 7 8
                                                        1 2 3 4 5 6 7 8
                                                        1 2 3 4 5 6 7 8
                                                        1 2 3 4 5 6 7 8
                                                        1 2 3 4 5 6 7 8)) 
                                 :marks ,marks-list)))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini :use-custom-markup t))
  
;;; all cmn-only marks
;;; ----------------------------------------------------------------------------
(let* ((marks-list (loop for m in '(i-ii i-ii-iii ii-iii iii-iv nail stopped 
                                    trill-f trill-n trill-s) 
                      for r from 1
                      collect m
                      collect r))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette `((1 ((((5 4) - e e e e - - e e e e - - e e -)) 
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8
                                                        1 2))
                                 :marks ,marks-list)))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini))
  
;;; all lp-only marks (there is only one: hairpin0)
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks (hairpin0 3 cresc-beg 3 
                                                  cresc-end 5
                                                  dim-beg 6 dim-end 8 
                                                  hairpin0 6)))) 
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (write-lp-data-for-all mini))
  
;;; all dynamcis that work for both cmn and lp
;;; ----------------------------------------------------------------------------
(let* ((marks-list (loop for m in '(pppp ppp pp p mp mf f ff fff ffff) 
                      for r from 1
                      collect m
                      collect r))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette `((1 ((((5 4) - e e e e - - e e e e - - e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8 1 2)) 
                                 :marks ,marks-list)))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini :use-custom-markup t))
  
;;; all cmn-only dynamics
;;; ----------------------------------------------------------------------------
(let* ((marks-list (loop for m in '(pppp-p ppp-p pp-p p-p mp-p mf-p f-p ff-p
                                    fff-p ffff-p)  
                      for r from 1
                      collect m
                      collect r))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette `((1 ((((5 4) - e e e e - - e e e e - - e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8 1 2)) 
                                 :marks ,marks-list)))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini))
  
;;; all note-heads that work for both cmn and lp
;;; ----------------------------------------------------------------------------
(let* ((marks-list (loop for m in '(circled-x flag-head triangle-up x-head)  
                      for r from 1
                      collect m
                      collect r))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks ,marks-list)))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;; all cmn-only note-heads
;;; ----------------------------------------------------------------------------
(let* ((marks-list (loop for m in '(airy-head arrow-down arrow-up slash square
                                    none)  
                      for r from 1
                      collect m
                      collect r))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks ,marks-list)))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini))
  
;;; all lp-only note-heads
;;; ----------------------------------------------------------------------------
(let* ((marks-list (loop for m in '(triangle)  
                      for r from 1
                      collect m
                      collect r))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks ,marks-list)))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (write-lp-data-for-all mini))

;;; lp flag-dots-on, flag-dots-off
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((6 4) q. q. q. q.))
                                 :pitch-seq-palette ((1 2 3 4))
                                 :marks (flag-head 1 4))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (add-mark-before-note mini 1 1 'vn 'flag-dots-on)
  (add-mark-before-note mini 1 3 'vn 'flag-dots-off)
  (midi-play mini)
  (write-lp-data-for-all mini))