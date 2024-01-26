;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             chords-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany chords.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 10:33:50 Sat Mar 19 2016 GMT
;;;
;;; SVN ID: $Id: chords-examples.lsp 5626 2016-03-19 10:43:04Z medward2 $
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
;;;  chords.html examples
;;; ============================================================================

;;;  indicating chords in the pitch-seq-palette
;;; ----------------------------------------------------------------------------

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((pn (piano :midi-channel 1))))
        :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                               c3 d3 e3 f3 g3 a3 b3
                               c4 d4 e4 f4 g4 a4 b4 
                               c5 d5 e5 f5 g5 a5 b5 c6))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 (3) 2 3 (5) 4 6 3))))) 
        :rthm-seq-map '((1 ((pn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  defining an instrument object that is chord-capable
;;; ----------------------------------------------------------------------------

(let ((pno
       (make-instrument
        'piano
        :staff-name "piano" :staff-short-name "pno"
        :lowest-written 'a0 :highest-written 'c8 
        :clefs '(treble bass double-treble double-bass) 
        :starting-clef 'treble 
        :microtones nil :largest-fast-leap 9
        :chords t :chord-function 'piano-chord-fun
        :midi-program 1)))
  (print pno))

;;;  default chord function
;;; ----------------------------------------------------------------------------
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vb (vibraphone :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                :pitch-seq-palette ((1 (2) 3 4 (5) (6) (7)
                                                       8))))) 
        :rthm-seq-map '((1 ((vb (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  piano-chord-fun
;;; ----------------------------------------------------------------------------
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((pn (piano :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                :pitch-seq-palette ((1 (2) 3 4 (5) (6) (7) 
                                                       8))))) 
        :rthm-seq-map '((1 ((pn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  set chord-function slot of instrument object
;;; ----------------------------------------------------------------------------
(progn
  (set-slot 'chord-function 'chord-fun1 'piano
            +slippery-chicken-standard-instrument-palette+)
  (print
   (chord-function 
      (get-data 'piano +slippery-chicken-standard-instrument-palette+)))
  (set-slot 'chord-function 'PIANO-CHORD-FUN 'piano
            +slippery-chicken-standard-instrument-palette+)
  (print
   (chord-function 
      (get-data 'piano +slippery-chicken-standard-instrument-palette+))))

;;;  chord-fun1
;;; ----------------------------------------------------------------------------
(progn
  (set-slot 'chord-function 'chord-fun1 'piano
            +slippery-chicken-standard-instrument-palette+)
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((pn (piano :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 (2) 3 4 (5) (6) (7) 
                                                         8))))) 
          :rthm-seq-map '((1 ((pn (1))))))))

    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini))
  (set-slot 'chord-function 'piano-chord-fun 'piano
            +slippery-chicken-standard-instrument-palette+))

;;;  chord-fun2
;;; ----------------------------------------------------------------------------
(progn
  (set-slot 'chord-function 'chord-fun2 'piano
            +slippery-chicken-standard-instrument-palette+)
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((pn (piano :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 g5 a5 b5 c6 d6
                                 e6)))) 
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 (3) 5 7 (11) (13) 
                                                         (15) 17)))))  
          :rthm-seq-map '((1 ((pn (1))))))))
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini))
  (set-slot 'chord-function 'piano-chord-fun 'piano
                +slippery-chicken-standard-instrument-palette+))

;;;  user-defined chord function via chord-fun-aux 
;;; ----------------------------------------------------------------------------
(defun new-chord-function (curve-num index pitch-list pitch-seq
                           instrument set) 
  (chord-fun-aux curve-num index pitch-list pitch-seq instrument set 4 3
                 14)) 

(set-slot 'chord-function 'new-chord-function 'piano
          +slippery-chicken-standard-instrument-palette+)

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((pn (piano :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 
                               c5 d5 e5 f5 g5 a5 b5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                :pitch-seq-palette ((1 (14) (13) (12) 11
                                                       10 9 8))))) 
        :rthm-seq-map '((1 ((pn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

(set-slot 'chord-function 'piano-chord-fun 'piano
          +slippery-chicken-standard-instrument-palette+)

;;;  chords and subsets example 1
;;; ----------------------------------------------------------------------------
(defun piano-subset-fun (curve-num index pitch-list pitch-seq instrument set)   
  (declare (ignore curve-num index pitch-list pitch-seq instrument))
  (let* ((ss (when (subsets set) (get-data 'piano-chord (subsets set) nil))))
    (when ss 
      (make-chord (data ss)))))

(set-slot 'chord-function 'piano-subset-fun 'piano 
          +slippery-chicken-standard-instrument-palette+)

(let* ((ss-chords-piece-1
        (make-slippery-chicken
         '+ss-chords-piece-1+
         :title "ss chords piece 1"
         :instrument-palette +slippery-chicken-standard-instrument-palette+
         :ensemble '(((pno (piano :midi-channel 1))))
         :tempo-map '((1 (q 60)))
         :set-palette '((1 ((f3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5
                                g5) 
                            :subsets ((piano-chord (c4 e4 g4))))))
         :set-map '((1 (1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((8 (7) 8 7 (8) 7 8
                                                        7))))) 
         :rthm-seq-map '((1 ((pno (1 1 1 1 1 1))))))))
  (midi-play ss-chords-piece-1)
  (cmn-display ss-chords-piece-1)
  (write-lp-data-for-all ss-chords-piece-1))

(set-slot 'chord-function 'piano-chord-fun 'piano
          +slippery-chicken-standard-instrument-palette+)

;;;  chords and subsets example 2
;;; ----------------------------------------------------------------------------
;;; NB piano-subset-fun defined above
(defun piano-chord-master (curve-num index pitch-list pitch-seq instrument 
                           set)  
  (let* ((xss (piano-subset-fun curve-num index pitch-list pitch-seq
                                instrument set)))
    (if xss xss 
        (chord-fun2 curve-num index pitch-list pitch-seq instrument set)))) 

(set-slot 'chord-function 'piano-chord-master 'piano 
          +slippery-chicken-standard-instrument-palette+)

(let* ((ss-chords-piece-2
        (make-slippery-chicken
         '+ss-chords-piece-2+
         :title "ss chords piece 2"
         :instrument-palette +slippery-chicken-standard-instrument-palette+ 
         :ensemble '(((pno (piano :midi-channel 1))))
         :tempo-map '((1 (q 60)))
         :set-palette '((1 ((f3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5
                                g5) 
                            :subsets ((piano-chord (c4 e4 g4)))))
                        (2 ((fs3 gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5
                                 ds5 e5 fs5 gs5))))
         :set-map '((1 (1 2 1 2 1 2)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((8 (7) 8 7 (8) 7 8 
                                                        7))))) 
         :rthm-seq-map '((1 ((pno (1 1 1 1 1 1))))))))
  (midi-play ss-chords-piece-2)
  (cmn-display ss-chords-piece-2)
  (write-lp-data-for-all ss-chords-piece-2))

(set-slot 'chord-function 'piano-chord-fun 'piano
          +slippery-chicken-standard-instrument-palette+)

;;;  chords and subsets example 3
;;; ----------------------------------------------------------------------------
(defun piano-ss-fun2 (curve-num index pitch-list pitch-seq instrument set)   
  (declare (ignore curve-num index pitch-list pitch-seq instrument))
  (let* ((pcs (get-data 'piano-chords (subsets set))))
    (when pcs
      (make-chord (data (get-next (data pcs)))))))

(defun piano-mc-2 (curve-num index pitch-list pitch-seq instrument set)
  (let* ((ss (when (subsets set) (get-data 'piano-chords (subsets set) nil)))
         (sss (when ss (data ss))))
    (if (is-ral sss)
        (piano-ss-fun2 curve-num index pitch-list pitch-seq instrument set) 
        (piano-chord-fun curve-num index pitch-list pitch-seq instrument
                         set)))) 

(set-slot 'chord-function 'piano-mc-2 'piano
          +slippery-chicken-standard-instrument-palette+)

(let* ((subset-piece-3
        (make-slippery-chicken
         '+subset-piece-3+
         :title "subset piece 3"
         :instrument-palette +slippery-chicken-standard-instrument-palette+
         :ensemble '(((pno (piano :midi-channel 1))))
         :tempo-map '((1 (q 60)))
         :set-palette '((1 ((f3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 
                                g5) 
                            :subsets ((piano-chords ((pno1 (c4 e4 g4))
                                                     (pno2 (d4 f4 a4))
                                                     (pno3 (e4 g4 b4)))))))  
                        (2 ((fs3 gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4
                                 cs5))))     
         :set-map '((1 (1 2 1 2 1 2)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((8 (7) 8 7 (8) 7 8 
                                                        7))))) 
         :rthm-seq-map '((1 ((pno (1 1 1 1 1 1))))))))
  (midi-play subset-piece-3)
  (cmn-display subset-piece-3)
  (write-lp-data-for-all subset-piece-3))

(set-slot 'chord-function 'piano-chord-fun 'piano
          +slippery-chicken-standard-instrument-palette+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
