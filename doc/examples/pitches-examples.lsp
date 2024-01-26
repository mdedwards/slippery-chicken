;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             pitches-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany pitches.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    17th July 2012
;;;
;;; $$ Last modified: 22:45:33 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: pitches-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;; pitches.html examples
;;; ============================================================================

;;; constructing a set-palette and using cmn-play to print it
;;; ----------------------------------------------------------------------------
(let ((sp (make-set-palette 
           'test
           '((set1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)))
             (set2 ((c3 fs3 cs4 e4 g4 a4 b4 c5 df5 f5 g5 af5 ef6)))
             (set3 ((d3 f3 cs4 e4 fs4 a4 b4 c5 d5 e5 fs5 af5 ef6)))
             (set4 ((d3 e3 cs4 ef4 fs4 a4 b4 c5 d5 e5 fs5 af5 d6))))))) 
  (cmn-display sp :break-line-each-set nil :size 16))
  
;;; using the recurse-simple-data keyword for 2-pitch sets
;;; ----------------------------------------------------------------------------
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '(((1 ((gs4 bf4)))) :recurse-simple-data nil)
        :set-map '((1 (1)))
        :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                :pitch-seq-palette ((1 2 3 4 5 6 7
                                                       8))))) 
        :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
   
;;;  creating a set-map and using the :display-sets keyword of cmn-display to
;;;  print it  below the score
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((set1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5
                                   ef6))) 
                        (set2 ((c3 fs3 cs4 e4 g4 a4 b4 c5 df5 f5 g5 af5
                                   ef6))) 
                        (set3 ((d3 f3 cs4 e4 fs4 a4 b4 c5 d5 e5 fs5 af5
                                   ef6))) 
                        (set4 ((d3 e3 cs4 ef4 fs4 a4 b4 c5 d5 e5 fs5 af5
                                   d6)))) 
         :set-map '((1 (set1 set1 set1 set1 set1))
                    (2 (set2 set3 set2 set3 set2 set3 set3))
                    (3 (set3 set3 set4 set3 set4 set3 set4 set4 set3 set4
                        set4))    
                    (4 (set4 set4 set1 set4 set1 set4 set1 set1 set1)))
         :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
         :rthm-seq-map `((1 ((vn ,(loop repeat 5 collect 1))))
                         (2 ((vn ,(loop repeat 7 collect 1))))
                         (3 ((vn ,(loop repeat 11 collect 1))))
                         (4 ((vn ,(loop repeat 9 collect 1))))))))
  (midi-play mini)
  (cmn-display mini :display-sets t)
  (write-lp-data-for-all mini))

;;;  simple pitch-seq/pitch-seq-palette example
;;; ----------------------------------------------------------------------------  
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                :pitch-seq-palette ((1 4 3 2 5 7 8 6)))))
        :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  same pitch-seq produces different pitches for different instruments
;;; ----------------------------------------------------------------------------
(let* ((scsip-clone (clone +slippery-chicken-standard-instrument-palette+)))
  (set-slot 'starting-clef 'tenor 'bassoon scsip-clone)
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :instrument-palette scsip-clone
           :ensemble '(((fl (flute :midi-channel 1))
                        (bn (bassoon :midi-channel 2))))
           :set-palette '((1 ((b3 d4 g4 b4 e5 a5 d6 a6 b6))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map '((1 ((fl (1))
                               (bn (1))))))))
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;;  indicating chords in a pitch-seq
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((pn (piano :midi-channel 1))))
         :set-palette '((1 ((c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 (4) (3) 2 5 (7) 8 
                                                        6))))) 
         :rthm-seq-map '((1 ((pn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
      
;;;  avoiding/allowing melodic octaves
;;; ----------------------------------------------------------------------------
(let* ((mini-1
        (make-slippery-chicken
         '+mini-1+
         :title "mini 1"
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map '((1 ((fl (1 1 1)))))))
       (mini-2
        (make-slippery-chicken
         '+mini-2+
         :title "mini 2"
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :avoid-melodic-octaves nil
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map '((1 ((fl (1 1 1))))))))
  (midi-play mini-1 :midi-file "/tmp/mini-1.mid")
  (cmn-display mini-1 :file "/tmp/mini-1.eps")
  (write-lp-data-for-all mini-1)
  (midi-play mini-2 :midi-file "/tmp/mini-2.mid")
  (cmn-display mini-2 :file "/tmp/mini-2.eps")
  (write-lp-data-for-all mini-2))

;;;  multiple pitch-seq curves in the same pitch-seq-palette
;;; ----------------------------------------------------------------------------
(let* ((multi-ps
        (make-slippery-chicken
         '+multi-ps+
         :title "Multiple pitch-seqs"
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))))
         :tempo-map '((1 (q 60)))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((8 7 8 7 8 7 8 7)
                                                     (5 4 3 4 5 4 3 4)
                                                     (1 2 1 2 1 2 1 2)))))
         :rthm-seq-map '((1 ((fl (1))
                             (ob (1))
                             (cl (1))))))))
  (midi-play multi-ps)
  (cmn-display multi-ps)
  (write-lp-data-for-all multi-ps))

;;;  using set-limits-high and set-limits-low to constrain pitches
;;; ----------------------------------------------------------------------------
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))
                     (cb (double-bass :midi-channel 3))))
        :set-palette '((1 ((e1 f1 g1 a1 b1
                               c2 d2 e2 f2 g2 a2 b2
                               c3 d3 e3 f3 g3 a3 b3
                               c4 d4 e4 f4 g4 a4 b4 
                               c5 d5 e5 f5 g5 a5 b5 c6))))
        :set-limits-high '((cl (0 c6 50 c5 100 c6))
                           (vc (0 g4 50 g3 100 g4))
                           (cb (0 f3 50 f2 100 f3)))
        :set-limits-low '((cl (0 c5 50 c4 100 c5))
                          (vc (0 g3 50 g2 100 g3))
                          (cb (0 f2 50 e1 100 f2)))
        :set-map `((1 ,(loop repeat 10 collect 1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map (list (list 1 
                                  (loop for p in '(cl vc cb)
                                     collect (list p (loop repeat 10 
                                                        collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  specifying simple subsets in a set-palette and using cmn-display to print
;;;  the result
;;;  ----------------------------------------------------------------------------
(let* ((sp (make-set-palette 
            'test
            '((1 ((f3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 g5)
                  :subsets ((pno1 (c4 e4 g4))
                            (pno2 (d4 f4 a4))
                            (mba1 (a3 c4 e4)))
                  :related-sets ((pno3 (gs4 bf4 df5))
                                 (mba2 (fs3 af3 cs4)))))))))
  (cmn-display sp :break-line-each-set nil :size 16))
  
;;;  specifying nested subsets in a set-palette and using cmn-display to print
;;;  the result
;;;  ----------------------------------------------------------------------------
(let ((sp (make-set-palette 
           'test
           '((1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)
                 :subsets ((piano ((pno1 (cs4 e4 fs4))
                                   (pno2 (e4 fs4 a4))))
                           (marimba ((mba1 (c3 g3 cs4))
                                     (mba2 (g3 cs4 e4)))))
                 :related-sets ((piano ((pno3 (d3 a3 d5))
                                        (pno4 (c3 g3 d5)))))))))))
  (cmn-display sp :break-line-each-set nil :size 16))
  
;;;  limiting a player's pitches using subset-id
;;;  ----------------------------------------------------------------------------
(progn
  (set-slot 'subset-id 'flute-notes 'flute 
            +slippery-chicken-standard-instrument-palette+) 
  (set-slot 'subset-id 'oboe-notes 'oboe
            +slippery-chicken-standard-instrument-palette+)
  (set-slot 'subset-id 'clarinet-notes 'b-flat-clarinet
            +slippery-chicken-standard-instrument-palette+)
  (let* ((subset-id-piece
          (make-slippery-chicken
           '+subset-id-piece+
           :title "subset id piece"
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))
                        (cl (b-flat-clarinet :midi-channel 3))))
           :set-palette 
           '((1 ((b3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 g5 a5 b5 
                     c6 d6 e6 f6 g6 a6 b6) 
                 :subsets ((flute-notes (b5 c6 d6 e6 f6 g6 a6 b6))   
                           (oboe-notes (a4 b4 c5 d5 e5 f5 g5 a5)) 
                           (clarinet-notes (b3 c4 d4 e4 f4 g4))))))  
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette 
           '((1 ((((4 4) - e. s - - e e - 
                   - +s s s s - (s) - s s s - )) 
                 :pitch-seq-palette ((2 2 3 1 1 2 1 2 2 2) 
                                     (6 8 5 5 7 7 9 6 8 10)  
                                     (5 4 3 1 1 2 3 3 4 4) 
                                     (1 3 3 2 1 2 3 1 1 1)))))
           :rthm-seq-map '((1 ((fl (1 1 1))
                               (ob (1 1 1))
                               (cl (1 1 1))))))))
    (midi-play subset-id-piece)
    (cmn-display subset-id-piece)
    (write-lp-data-for-all subset-id-piece)
    (loop for p in '(flute oboe b-flat-clarinet)
       collect
         (set-slot 'subset-id nil p 
                   +slippery-chicken-standard-instrument-palette+))))