;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             sc-test-webpages.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Run tests of all user's guide webpage examples. Only FAIL
;;;                   feedback and desired warnings are printed. A
;;;                   message will be printed at the end of the run to
;;;                   indicate whether all tests passed or not.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    7th December 2011 (Edinburgh)
;;;
;;; $$ Last modified:  10:01:45 Wed Feb 27 2019 CET
;;;
;;; SVN ID: $Id: sc-test-suite.lsp 2976 2012-07-24 10:06:19Z sreed23 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 2 of the License, or (at your
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

(in-package :sc)

(in-scale :quarter-tone)

(set-sc-config 'cmn-display-auto-open nil)
(set-sc-config 'default-dir "/tmp/")


(load-from-test-suite-dir "sc-test-suite-aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; webpage example-code tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chop.html

;;; SAR Fri Jul  6 12:02:20 BST 2012
(sc-deftest test-webpage-chop ()
  (let* ((orig-palette (make-rsp 'orig
                                 '((1 ((((1 4) - s e s - ))
                                       :pitch-seq-palette ((1 2 3))))
                                   (2 ((((1 4) - e. s - ))
                                       :pitch-seq-palette ((1 2))))
                                   (3 ((((1 4) - (e.) s - ))
                                       :pitch-seq-palette ((1)))))))
         (chopped-palette (chop orig-palette
                                '((1 4) 
                                  (1 3) (2 4) 
                                  (1 2) (2 3) (3 4) 
                                  (1 1) (2 2) (3 3) (4 4)) ; chop points  
                                's)) ; chopping unit
         (sc-chopped-example
          (make-slippery-chicken
           '+sc-chopped-example+
           :title "sc chopped example"
           :instrument-palette +slippery-chicken-standard-instrument-palette+
           :ensemble '(((vn (violin :midi-channel 1))))
           :tempo-map '((1 (q 60)))
           :bars-per-system-map '((1 10))
           :set-palette '((1 ((c4 d4 e4))))
           :set-map '((1 (1 1 1 1 1)))
           :rthm-seq-palette chopped-palette
           :rthm-seq-map '((1 ((vn ((1 1) (1 2) (1 3) (2 1) (3 2)))))))))
    #+cmn (probe-delete "/tmp/sc-chopped-example.eps")
    (sc-test-check
      #+cmn (cmn-display sc-chopped-example :file
                         "/tmp/sc-chopped-example.eps")  
      #+cmn (file-write-ok "/tmp/sc-chopped-example.eps" 13000)
      (= 5 (num-bars sc-chopped-example))
      (equalp
       (loop for b from 1 to (num-bars sc-chopped-example)
          collect (data (get-time-sig (get-bar sc-chopped-example b 'vn))))
       '((1 4) (3 16) (3 16) (1 4) (3 16)))
      (not (next-event sc-chopped-example 'vn nil 1))
      (equalp 
       (loop for ne = (next-event sc-chopped-example 'vn)
          while ne
          collect (data ne))
       '(S E S S E E S E. S 16/3))
      (not (next-event sc-chopped-example 'vn nil 1))
      (equalp
       (loop for ne = (next-event sc-chopped-example 'vn)
          while ne
          collect (is-rest ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL T)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chords.html

;;; SAR Fri Jul  6 12:12:49 BST 2012
(sc-deftest test-webpage-chords-parens-make-chord-objs ()
  (let ((slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((pn (piano :midi-channel 1))))
          :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                 c3 d3 e3 f3 g3 a3 b3
                                 c4 d4 e4 f4 g4 a4 b4 
                                 c5 d5 e5 f5 g5 a5 b5 c6))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 (3) 2 3 (5) 4 6 3))))) 
          :rthm-seq-map '((1 ((pn (1))))))))
    #+cmn (probe-delete "/tmp/slippery-chicken-piece.eps")
    (sc-test-check
      #+cmn (cmn-display slippery-chicken-piece)
      #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 10000)
      (not (next-event slippery-chicken-piece 'pn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'pn)
          while ne
          collect (chord-p (pitch-or-chord ne)))
       '(NIL T NIL NIL T NIL NIL NIL)))))

;;; SAR Thu Mar 15 11:36:26 GMT 2012 
(sc-deftest test-webpage-chords-inst-obj-chords-t ()
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
    (sc-test-check
      (equalp (loop for i in '(id staff-name staff-short-name clefs
                               starting-clef microtones largest-fast-leap
                               chords chord-function midi-program) 
                 collect  (funcall i pno))
              '(PIANO "piano" "pno" (DOUBLE-BASS DOUBLE-TREBLE BASS TREBLE) 
                TREBLE NIL 9 T PIANO-CHORD-FUN 1))
      (pitch-p (highest-written pno))
      (pitch-p (lowest-written pno))
      (equalp (data (highest-written pno)) 'c8)
      (equalp (data (lowest-written pno)) 'a0))))

;;; SAR Fri Jul  6 12:23:20 BST 2012
(sc-deftest test-webpage-chords-def-chrd-fn-examp ()
  (let ((slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vb (vibraphone :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 (2) 3 4 (5) (6) (7)
                                                         8))))) 
          :rthm-seq-map '((1 ((vb (1))))))))
    #+cmn (probe-delete "/tmp/slippery-chicken-piece.eps")
    (sc-test-check
      #+cmn (cmn-display slippery-chicken-piece)
      #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 7500)
      (not (next-event slippery-chicken-piece 'vb nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vb)
          while ne
          collect (chord-p (pitch-or-chord ne)))
       '(NIL T NIL NIL T T T NIL))
      (not (next-event slippery-chicken-piece 'vb nil 1))
      (equalp
       (print (loop for ne = (next-event slippery-chicken-piece 'vb)
          while ne
          collect (get-pitch-symbol ne)))
       '(C4 (C4 e4) E4 F4 (e4 G4) (f4 A4) (g4 B4) C5)))))

;;; SAR Fri Jul  6 12:28:00 BST 2012
(sc-deftest test-webpage-chords-pno-chrd-fun-examp ()
  (let ((slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((pn (piano :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 (2) 3 4 (5) (6) (7) 
                                                         8))))) 
          :rthm-seq-map '((1 ((pn (1))))))))
    #+cmn (probe-delete "/tmp/slippery-chicken-piece.eps")
    (sc-test-check
      #+cmn (cmn-display slippery-chicken-piece)
      #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 8400)
      (not (next-event slippery-chicken-piece 'pn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'pn)
          while ne
          collect (chord-p (pitch-or-chord ne)))
       '(NIL T NIL NIL T T T NIL))
      (not (next-event slippery-chicken-piece 'pn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'pn)
          while ne
          collect (get-pitch-symbol ne))
       ;;'((C4 (C4 D4 E4 F4) E4 F4 (D4 E4 F4 G4) (E4 F4 G4 A4) (F4 G4 A4 B4) C5))
       '(C4 (C4 D4 E4 F4) E4 F4 (D4 E4 F4 G4) (E4 F4 G4 A4) (F4 G4 A4 B4) C5)
       ))))

;;; SAR Fri Jul  6 12:37:39 BST 2012
(sc-deftest test-webpage-chords-set-slot ()
  (sc-test-check 
    (set-slot 'chord-function 'PIANO-CHORD-FUN 'piano
              +slippery-chicken-standard-instrument-palette+)
    (equalp
     (chord-function 
      (get-data 'piano +slippery-chicken-standard-instrument-palette+))
     'PIANO-CHORD-FUN)
    (set-slot 'chord-function 'chord-fun1 'piano
              +slippery-chicken-standard-instrument-palette+)
    (equalp
     (chord-function 
      (get-data 'piano +slippery-chicken-standard-instrument-palette+))
     'CHORD-FUN1)
    (set-slot 'chord-function 'PIANO-CHORD-FUN 'piano
              +slippery-chicken-standard-instrument-palette+)
    (equalp
     (chord-function 
      (get-data 'piano +slippery-chicken-standard-instrument-palette+))
     'PIANO-CHORD-FUN)))

;;; SAR Fri Jul  6 12:49:00 BST 2012
(sc-deftest test-webpage-chords-chrd-fun-1-examp ()
  (progn
    (set-slot 'chord-function 'chord-fun1 'piano
              +slippery-chicken-standard-instrument-palette+)
    (let ((slippery-chicken-piece
           (make-slippery-chicken
            '+slippery-chicken-piece+
            :title "slippery-chicken-piece"
            :ensemble '(((pn (piano :midi-channel 1))))
            :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
            :set-map '((1 (1)))
            :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                    :pitch-seq-palette ((1 (2) 3 4 (5) (6) (7) 
                                                           8))))) 
            :rthm-seq-map '((1 ((pn (1))))))))
      #+cmn (probe-delete "/tmp/slippery-chicken-piece.eps")
      (sc-test-check
        #+cmn (cmn-display slippery-chicken-piece)
        #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 8500)
        (not (next-event slippery-chicken-piece 'pn nil 1))
        (equalp
         (loop for ne = (next-event slippery-chicken-piece 'pn)
            while ne
            collect (chord-p (pitch-or-chord ne)))
         '(NIL T NIL NIL T T T NIL))
        (not (next-event slippery-chicken-piece 'pn nil 1))
        (equalp
         (loop for ne = (next-event slippery-chicken-piece 'pn)
            while ne
            collect (get-pitch-symbol ne))
         '(C4 (C4 E4 G4) E4 F4 (C4 E4 G4) (D4 F4 A4) (E4 G4 B4) C5))
        (set-slot 'chord-function 'piano-chord-fun 'piano
                  +slippery-chicken-standard-instrument-palette+)
        (equalp
         (chord-function 
          (get-data 'piano +slippery-chicken-standard-instrument-palette+))
         'PIANO-CHORD-FUN)))))

;;; SAR Fri Jul  6 12:54:33 BST 2012
(sc-deftest test-webpage-chords-chrd-fun-2-examp ()
  (progn
    (set-slot 'chord-function 'chord-fun2 'piano
              +slippery-chicken-standard-instrument-palette+)
    (let ((slippery-chicken-piece
           (make-slippery-chicken
            '+slippery-chicken-piece+
            :title "slippery-chicken-piece"
            :ensemble '(((pn (piano :midi-channel 1))))
            :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 g5 a5 b5 c6 d6
                                   e6)))) 
            :set-map '((1 (1)))
            :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                    :pitch-seq-palette ((1 (3) 5 7 (11) (13) 
                                                           (15) 17)))))  
            :rthm-seq-map '((1 ((pn (1))))))))
      #+cmn (probe-delete "/tmp/slippery-chicken-piece.eps")
      (sc-test-check
        #+cmn (cmn-display slippery-chicken-piece)
        #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 8500)
        (not (next-event slippery-chicken-piece 'pn nil 1))
        (equalp
         (loop for ne = (next-event slippery-chicken-piece 'pn)
            while ne
            collect (chord-p (pitch-or-chord ne)))
         '(NIL T NIL NIL T T T NIL))
        (not (next-event slippery-chicken-piece 'pn nil 1))
        (equalp
         (loop for ne = (next-event slippery-chicken-piece 'pn)
            while ne
            collect (get-pitch-symbol ne))
         '(C4 (C4 F4 B4 E5) G4 B4 (D4 G4 C5 F5) (F4 B4 E5 A5) 
           (A4 D5 G5 C6) E6))
        (set-slot 'chord-function 'piano-chord-fun 'piano
                  +slippery-chicken-standard-instrument-palette+)
        (equalp
         (chord-function 
          (get-data 'piano +slippery-chicken-standard-instrument-palette+))
         'PIANO-CHORD-FUN)))))

;;; SAR Fri Jul  6 13:06:53 BST 2012
(sc-deftest test-webpage-chords-chord-fun-aux-examp ()
    (progn
      (defun new-chord-function (curve-num index pitch-list pitch-seq
                                 instrument set) 
        (chord-fun-aux curve-num index pitch-list pitch-seq instrument set 4 3
                       14)) 
      (set-slot 'chord-function 'new-chord-function 'piano
                +slippery-chicken-standard-instrument-palette+)
      (let ((slippery-chicken-piece
             (make-slippery-chicken
              '+slippery-chicken-piece+
              :title "slippery-chicken-piece"
              :ensemble '(((pn (piano :midi-channel 1))))
              :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 
                                     c5 d5 e5 f5 g5 a5 b5))))
              :set-map '((1 (1)))
              :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                      :pitch-seq-palette ((1 (14) (13) (12) 11
                                                             10 9 8))))) 
              :rthm-seq-map '((1 ((pn (1))))))))
        #+cmn (probe-delete "/tmp/slippery-chicken-piece.eps")
        (sc-test-check
          #+cmn (cmn-display slippery-chicken-piece)
          #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 7500)
          (not (next-event slippery-chicken-piece 'pn nil 1))
          (equalp
           (loop for ne = (next-event slippery-chicken-piece 'pn)
              while ne
              collect (chord-p (pitch-or-chord ne)))
           '(NIL T T T NIL NIL NIL NIL))
          (not (next-event slippery-chicken-piece 'pn nil 1))
          (equalp
           (loop for ne = (next-event slippery-chicken-piece 'pn)
              while ne
              collect (get-pitch-symbol ne))
           '(C4 (A4 E5 B5) (G4 D5 A5) (F4 C5 G5) F5 E5 D5 C5))
          (set-slot 'chord-function 'piano-chord-fun 'piano
                    +slippery-chicken-standard-instrument-palette+)
          (equalp
           (chord-function 
            (get-data 'piano +slippery-chicken-standard-instrument-palette+))
           'PIANO-CHORD-FUN)))))

;;; MDE Sat Mar 19 09:56:05 2016 -- taking this out of the tests below and
;;; defining only once  
(defun piano-subset-fun (curve-num index pitch-list pitch-seq instrument set)   
  (declare (ignore curve-num index pitch-list pitch-seq instrument))
  (let* ((ss (when (subsets set) (get-data 'piano-chord (subsets set) nil))))
    (when ss 
      (make-chord (data ss)))))

;;; SAR Fri Jul  6 13:25:21 BST 2012
(sc-deftest test-webpage-chords-ss-chords-piece-1 ()
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
    (loop for f in '("_ss-chords-piece-1-score.ly" "ss-chords-piece-1-def.ly"  
                     "ss-chords-piece-1-pno-part.ly"
                     "ss-chords-piece-1-pno.ly") 
       do (probe-delete (concatenate 'string "/tmp/" f)))  
    (sc-test-check
      (write-lp-data-for-all ss-chords-piece-1 :base-path "/tmp/")
      (notany #'not
              (loop for f in '("_ss-chords-piece-1-score.ly"
                               "ss-chords-piece-1-def.ly"  
                               "ss-chords-piece-1-pno-part.ly"
                               "ss-chords-piece-1-pno.ly")
                 for s in '(180 680 190 570)
                 do (file-write-ok (concatenate 'string "/tmp/" f) s)))
      (not (next-event ss-chords-piece-1 'pno nil 1))
      (equalp
       (loop for ne = (next-event ss-chords-piece-1 'pno)
          while ne
          collect (chord-p (pitch-or-chord ne)))
       '(NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL
         T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL
         NIL T NIL NIL T NIL NIL NIL))
      (not (next-event ss-chords-piece-1 'pno nil 1))
      (equalp
       (loop for ne = (next-event ss-chords-piece-1 'pno)
          while ne
          collect (get-pitch-symbol ne))
       '(G5 (C4 E4 G4) G5 F5 (C4 E4 G4) F5 G5 F5 G5 (C4 E4 G4) G5 F5 (C4 E4 G4)
         F5 G5 F5 G5 (C4 E4 G4) G5 F5 (C4 E4 G4) F5 G5 F5 G5 (C4 E4 G4) G5 F5 
         (C4 E4 G4) F5 G5 F5 G5 (C4 E4 G4) G5 F5 (C4 E4 G4) F5 G5 F5 G5 
         (C4 E4 G4) G5 F5 (C4 E4 G4) F5 G5 F5))
      (set-slot 'chord-function 'piano-chord-fun 'piano
                +slippery-chicken-standard-instrument-palette+)
      (equalp
       (chord-function 
        (get-data 'piano +slippery-chicken-standard-instrument-palette+))
       'PIANO-CHORD-FUN))))

(defun piano-chord-master (curve-num index pitch-list pitch-seq instrument 
                           set)  
  (let* ((xss (piano-subset-fun curve-num index pitch-list pitch-seq
                                instrument set)))
    (if xss xss 
        (chord-fun2 curve-num index pitch-list pitch-seq instrument set))))

;;; SAR Fri Jul  6 13:48:16 BST 2012
(sc-deftest test-webpage-chords-ss-chords-piece-2 ()
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
    (loop for f in '("_ss-chords-piece-2-score.ly"
                     "ss-chords-piece-2-def.ly"  
                     "ss-chords-piece-2-pno-part.ly"
                     "ss-chords-piece-2-pno.ly") 
       do (probe-delete (concatenate 'string "/tmp/" f)))
    (sc-test-check
      (write-lp-data-for-all ss-chords-piece-2 :base-path "/tmp/")
      (notany #'not
              (loop for f in '("_ss-chords-piece-2-score.ly"
                               "ss-chords-piece-2-def.ly"  
                               "ss-chords-piece-2-pno-part.ly"
                               "ss-chords-piece-2-pno.ly")
                 for s in '(180 680 190 640)
                 do (file-write-ok (concatenate 'string "/tmp/" f) s)))
      (not (next-event ss-chords-piece-2 'pno nil 1))
      (equalp
       (loop for ne = (next-event ss-chords-piece-2 'pno)
          while ne
          collect (chord-p (pitch-or-chord ne)))
       '(NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL
         NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL
         NIL NIL NIL T NIL NIL T NIL NIL NIL))
      (not (next-event ss-chords-piece-2 'pno nil 1))
      (equalp
       (loop for ne = (next-event ss-chords-piece-2 'pno)
          while ne
          collect (get-pitch-symbol ne))
       '(G5 (C4 E4 G4) G5 F5 (C4 E4 G4) F5 G5 F5 GS5 (DS4 GS4 CS5 FS5) GS5
         FS5 (E4 AS4 DS5 GS5) FS5 GS5 FS5 G5 (C4 E4 G4) G5 F5 (C4 E4 G4) F5
         G5 F5 GS5 (DS4 GS4 CS5 FS5) GS5 FS5 (E4 AS4 DS5 GS5) FS5 GS5 FS5 G5
         (C4 E4 G4) G5 F5 (C4 E4 G4) F5 G5 F5 GS5 (DS4 GS4 CS5 FS5) GS5 FS5
         (E4 AS4 DS5 GS5) FS5 GS5 FS5))
      (set-slot 'chord-function 'piano-chord-fun 'piano
                +slippery-chicken-standard-instrument-palette+)
      (equalp
       (chord-function 
        (get-data 'piano +slippery-chicken-standard-instrument-palette+))
       'PIANO-CHORD-FUN))))

;;; SAR Fri Jul  6 14:08:03 BST 2012
(sc-deftest test-webpage-chords-subset-piece-3 ()
  (progn
    (defun piano-ss-fun2 (curve-num index pitch-list pitch-seq instrument set)   
      (declare (ignore curve-num index pitch-list pitch-seq instrument))
      (make-chord (data (get-next (get-data-data 'piano-chords 
                                                 (subsets set))))))  
    (defun piano-mc-2 (curve-num index pitch-list pitch-seq instrument set)
      (let* ((ss (when (subsets set)
                   (get-data 'piano-chords (subsets set) nil)))
             (sss (when ss (data ss))))
        (if (is-ral sss)
            (piano-ss-fun2 curve-num index pitch-list pitch-seq instrument set) 
            (piano-chord-fun curve-num index pitch-list pitch-seq instrument
                             set)))) 
    (set-slot 'chord-function 'piano-mc-2 'piano
              +slippery-chicken-standard-instrument-palette+)
    (let* ((ly-files '("_subset-piece-3-score.ly" "subset-piece-3-def.ly"  
                       "subset-piece-3-pno-part.ly" "subset-piece-3-pno.ly"))
           (subset-piece-3
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
      (loop for f in ly-files 
         do (probe-delete (concatenate 'string "/tmp/" f)))
      (sc-test-check
        (write-lp-data-for-all subset-piece-3 :base-path "/tmp/")
        (notany #'not
                (loop for f in ly-files
                   for s in '(170 670 190 610)
                   collect 
                     (file-write-ok (concatenate 'string "/tmp/" f) s)))
        (not (next-event subset-piece-3 'pno nil 1))
        (equalp
         (loop for ne = (next-event subset-piece-3 'pno)
            while ne
            collect (chord-p (pitch-or-chord ne)))
         '(NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL
           NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL
           NIL NIL NIL T NIL NIL T NIL NIL NIL))
        (not (next-event subset-piece-3 'pno nil 1))
        (equalp
         (loop for ne = (next-event subset-piece-3 'pno)
            while ne
            collect (get-pitch-symbol ne))
         '(G5 (C4 E4 G4) G5 F5 (D4 F4 A4) F5 G5 F5 CS5 (FS4 GS4 AS4 B4) CS5 B4  
           (GS4 AS4 B4 CS5) B4 CS5 B4 G5 (E4 G4 B4) G5 F5 (C4 E4 G4) F5 G5 F5
           CS5 (FS4 GS4 AS4 B4) CS5 B4 (GS4 AS4 B4 CS5) B4 CS5 B4 G5 (D4 F4 A4)
           G5 F5 (E4 G4 B4) F5 G5 F5 CS5 (FS4 GS4 AS4 B4) CS5 B4 
           (GS4 AS4 B4 CS5) B4 CS5 B4))
        (set-slot 'chord-function 'piano-chord-fun 'piano
                  +slippery-chicken-standard-instrument-palette+)
        (equalp
         (chord-function 
          (get-data 'piano +slippery-chicken-standard-instrument-palette+))
         'PIANO-CHORD-FUN)))))

;;; MDE Tue Mar 20 16:01:47 2012
(sc-deftest test-chord-funs ()
  (let* ((pl (init-pitch-list '(c4 d4 fs4 b4 ds5 e5 f5)))
         (pl2 (init-pitch-list '(c4 d4 fs4 b4 ds5 e5 f5 g5 b5 cs6)))
         (dcf1 (default-chord-function nil 2 pl nil nil nil))
         ;; illegal index should result in nil but no error/warning
         (dcf2 (default-chord-function nil 8 pl nil nil nil))
         (dcf3 (default-chord-function nil 0 pl nil nil nil))
         (cf1a (chord-fun1 nil 0 pl nil nil nil))
         (cf1b (chord-fun1 nil 6 pl nil nil nil))
         (cf2a (chord-fun2 nil 6 pl2 nil nil nil)))
    ;;(print cf2a)
    (sc-test-check
      (equalp 'd4 (data (get-pitch dcf1 1)))
      (equalp 'fs4 (data (get-pitch dcf1 2)))
      (equalp 'd4 (data (get-pitch dcf3 2)))
      (equalp 'c4 (data (get-pitch dcf3 1)))
      (equalp dcf2 nil)
      (= 2 (sclist-length cf1a)) ; can't get 3 as > octave!
      (= 3 (sclist-length cf1b))
      (= 4 (sclist-length cf2a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ensemble.html

;;; SAR Fri Jul  6 15:38:27 BST 2012
(sc-deftest test-webpage-ensemble-one-plr-one-inst ()
  (let ((o-files '("slippery-chicken-piece.mid" 
                   #+cmn "slippery-chicken-piece.eps" 
                   "slippery-chicken-piece-fl.ly"
                   "slippery-chicken-piece-fl-part.ly"
                   "slippery-chicken-piece-def.ly"
                   "_slippery-chicken-piece-score.ly"))
        (o-files-sizes '(70 #+cmn 7000 150 200 660 190))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((fl (flute :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((fl (1)))))))
        plr)
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (loop for p in (get-keys (ensemble slippery-chicken-piece) nil)
          do 
            (setf plr (get-data p (ensemble slippery-chicken-piece)))
          collect 
            (list p (id (data plr)) (doubles plr) (midi-channel plr)
                  (microtones-midi-channel plr))
          when
            (and (stringp (id (data plr)))
                 (search "doublings" (id (data plr))))
          collect
            (get-keys (data (get-data p (ensemble slippery-chicken-piece)))
                      nil))
       ;; player ins doubles midi-chan microtones-chan
       ;; MDE Thu Sep 20 09:58:48 2018 -- mmc will be 1 now, not default of -1
       '((FL FLUTE NIL 1 1))) ; '((FL FLUTE NIL 1 -1)))
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul  6 16:53:16 BST 2012
(sc-deftest test-webpage-ensemble-doublings ()
  (let ((o-files '("slippery-chicken-piece.mid" 
                   #+cmn "slippery-chicken-piece.eps" 
                   "slippery-chicken-piece-fl.ly"
                   "slippery-chicken-piece-fl-part.ly"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-cl.ly"
                   "slippery-chicken-piece-cl-part.ly"
                   "_slippery-chicken-piece-score.ly"))
        (o-files-sizes '(110 #+cmn 11000 150 200 900 150 200 190))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
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
                              (cl (1)))))))
        plr)
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (print (loop for p in (get-keys (ensemble slippery-chicken-piece) nil)
          do 
            (setf plr (get-data p (ensemble slippery-chicken-piece)))
          collect 
            (list p (id (data plr)) (doubles plr) (midi-channel plr)
                  (microtones-midi-channel plr))
          when
            (and (stringp (id (data plr)))
                 (search "doublings" (id (data plr))))
          collect
            (get-keys (data (get-data p (ensemble slippery-chicken-piece)))
                      nil)))
       ;; MDE Thu Sep 20 10:00:46 2018 -- microtones channel now same as given
       ;; channel  
       '((FL FLUTE NIL 1 1) (CL "CL-doublings" T 2 2)
         (B-FLAT-CLARINET BASS-CLARINET)))
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; Fri Jul  6 18:58:04 BST 2012
(sc-deftest test-webpage-ensemble-ensemble-block ()
  (let* ((o-files '("slippery-chicken-piece.mid" 
                    #+cmn "slippery-chicken-piece.eps" 
                    "_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece-cl-part.ly"
                    "slippery-chicken-piece-cl.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"
                    "slippery-chicken-piece-hn-part.ly"
                    "slippery-chicken-piece-hn.ly"
                    "slippery-chicken-piece-perc-part.ly"
                    "slippery-chicken-piece-perc.ly"
                    "slippery-chicken-piece-solo-part.ly"
                    "slippery-chicken-piece-solo.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"
                    "slippery-chicken-piece-vla-part.ly"
                    "slippery-chicken-piece-vla.ly"
                    "slippery-chicken-piece-vln-part.ly"
                    "slippery-chicken-piece-vln.ly"))
         (o-files-sizes '(390 #+cmn 32000 190 200 150 2900 200 150 200 150
                          200 150 200 150 200 170 200 170 200 150))
         (slippery-chicken-piece
          (make-slippery-chicken
           '+slippery-chicken-piece+
           :title "slippery-chicken-piece"
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
                               (vc (1)))))))
         plr)
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (loop for p in (get-keys (ensemble slippery-chicken-piece) nil)
          do 
            (setf plr (get-data p (ensemble slippery-chicken-piece)))
          collect 
            (list p (id (data plr)) (doubles plr) (midi-channel plr)
                  (microtones-midi-channel plr))
          when
            (and (stringp (id (data plr)))
                 (search "doublings" (id (data plr))))
          collect
            (get-keys (data (get-data p (ensemble slippery-chicken-piece)))
                      nil))
       '((FL "FL-doublings" T 1 2) (ALTO-FLUTE PICCOLO) 
         (CL "CL-doublings" T 3 4)  
         (B-FLAT-CLARINET BASS-CLARINET) (HN FRENCH-HORN NIL 5 6)
         (PERC MARIMBA NIL 7 7) (SOLO VIOLIN NIL 8 9) (VLN VIOLIN NIL 11 12)
         (VLA VIOLA NIL 13 14) (VC CELLO NIL 15 16)))
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Sat Jul  7 13:21:19 BST 2012
(sc-deftest test-webpage-ensemble-inst-change-map ()
  (let* ((o-files '("slippery-chicken-piece.mid" 
                    #+cmn "slippery-chicken-piece.eps" 
                    "_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece-cl-part.ly"
                    "slippery-chicken-piece-cl.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"))
         (o-files-sizes '(820 #+cmn 51000 190 200 900 900 200 900))
         (slippery-chicken-piece
          (make-slippery-chicken
           '+slippery-chicken-piece+
           :title "slippery-chicken-piece"
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (loop for s from 1 to 2
          collect 
            (loop for p in '(fl cl)
               collect
                 (loop for b from 1 to 7
                    collect 
                      (cm-get-data (instrument-change-map slippery-chicken-piece) 
                                   (list s p) b))))
       '(((FLUTE FLUTE PICCOLO PICCOLO FLUTE FLUTE FLUTE)
          (B-FLAT-CLARINET BASS-CLARINET BASS-CLARINET BASS-CLARINET 
           BASS-CLARINET
           B-FLAT-CLARINET B-FLAT-CLARINET))
         ((FLUTE PICCOLO PICCOLO FLUTE FLUTE FLUTE FLUTE)
          (B-FLAT-CLARINET BASS-CLARINET B-FLAT-CLARINET B-FLAT-CLARINET
           B-FLAT-CLARINET B-FLAT-CLARINET B-FLAT-CLARINET))))
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Sat Jul  7 13:57:38 BST 2012
(sc-deftest test-webpage-ensemble-instrument-hierarchy ()
  (let* ((o-files '("slippery-chicken-piece.mid" 
                    #+cmn "slippery-chicken-piece.eps" 
                    "_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece-cl-part.ly"
                    "slippery-chicken-piece-cl.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"
                    "slippery-chicken-piece-hn-part.ly"
                    "slippery-chicken-piece-hn.ly"
                    "slippery-chicken-piece-perc-part.ly"
                    "slippery-chicken-piece-perc.ly"
                    "slippery-chicken-piece-solo-part.ly"
                    "slippery-chicken-piece-solo.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"
                    "slippery-chicken-piece-vla-part.ly"
                    "slippery-chicken-piece-vla.ly"
                    "slippery-chicken-piece-vln-part.ly"
                    "slippery-chicken-piece-vln.ly"))
         (o-files-sizes-1 '(370 #+cmn 32000 150 160 150 2900 160 150 160 150 
                            160 150 160 150 160 150 160 170 160 150))
         (o-files-sizes-2 '(360 #+cmn 33000 150 160 150 2900 160 150 160 150 
                            160 150 160 150 160 140 160 150 160 150))
         (slippery-chicken-piece-1
          (make-slippery-chicken
           '+slippery-chicken-piece-1+
           :title "slippery-chicken-piece"
           :ensemble '(((fl (flute :midi-channel 1))
                        (cl (b-flat-clarinet :midi-channel 3))
                        (hn (french-horn :midi-channel 5))
                        (perc (marimba :midi-channel 7))
                        (solo (violin :midi-channel 8))
                        (vln (violin :midi-channel 11)) 
                        (vla (viola :midi-channel 13))
                        (vc (cello :midi-channel 15))))
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
                               (vc (1)))))))
         (slippery-chicken-piece-2
          (make-slippery-chicken
           '+slippery-chicken-piece-2+
           :title "slippery-chicken-piece"
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not 
       (equalp
        (loop for p in (get-keys (ensemble slippery-chicken-piece-1))
           do
             (next-event slippery-chicken-piece-1 p nil 1)
           collect 
             (loop for ne = (next-event slippery-chicken-piece-1 p)
                while ne
                collect (get-pitch-symbol ne)))
        (loop for p in (get-keys (ensemble slippery-chicken-piece-2))
           do
             (next-event slippery-chicken-piece-2 p nil 1)
           collect 
             (loop for ne = (next-event slippery-chicken-piece-2 p)
                while ne
                collect (get-pitch-symbol ne)))))
      (midi-play slippery-chicken-piece-1)
      #+cmn (cmn-display slippery-chicken-piece-1)
      (write-lp-data-for-all slippery-chicken-piece-1)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes-1)
      (probe-delete-multi "/tmp/" o-files)
      (midi-play slippery-chicken-piece-2)
      #+cmn (cmn-display slippery-chicken-piece-2)
      (write-lp-data-for-all slippery-chicken-piece-2)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fibonacci.html

;;; SAR Sat Jul  7 15:03:20 BST 2012
(sc-deftest test-webpage-fibonacci-fib-transition-1 ()
  (sc-test-check
  (equalp (fibonacci-transition 50) 
          '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 0
            1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1)))) 


;;; SAR Thu Mar 15 11:43:02 GMT 2012
(sc-deftest test-webpage-fibonacci-fib-transition-2 ()
  (sc-test-check
    (equalp (fibonacci-transition 50 's 'e)
            '(S S S S S S S S S S S S E S S S S S S S E S S S S E S S E S E S E
              S E E S E E E E S E E E E E E E E))))

;;; SAR Sat Jul  7 15:06:07 BST 2012
(sc-deftest test-webpage-fibonacci-fib-transitions-1 ()
  (sc-test-check
  (equalp (fibonacci-transitions 76 4) 
          '(0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 2 1 1 
            2 1 2 1 2 2 1 2 1 2 2 2 2 2 2 2 3 2 2 3 2 3 2 3 3 2 3 2 3 3 3 2 3 3
            3 3 3 3 3 3 3 3))))

;;; SAR Thu Mar 15 11:44:09 GMT 2012
(sc-deftest test-webpage-fibonacci-fib-transitions-2 ()
  (sc-test-check
    (equalp 
     (fibonacci-transitions 304 '(s e q h)) 
     '(S S S S S S S S S S S S S S S S S S S S S S S S S S S S S E S S S S S S
       S S S S S S E S S S S S S S E S S S S E S S S S E S S E S S E S S E S E
       S E S E E S E S E S E E S E E S E E E E S E E E E S E E E E E E E E E E
       E E E E E E E E E E Q E E E E E E E Q E E E E Q E E E E Q E E Q E E Q E
       E Q E Q E Q E Q Q E Q E Q E Q Q E Q Q E Q Q Q Q E Q Q Q Q E Q Q Q Q Q Q
       Q Q Q Q Q Q Q Q Q Q Q Q Q Q H Q Q Q Q Q Q Q H Q Q Q Q H Q Q Q Q H Q Q H
       Q Q H Q Q H Q H Q H Q H H Q H Q H Q H H Q H H Q H H H H Q H H H H Q H H
       H H H H H H Q H H H H H H H H H H H H H H H H H H H H H H H H H H H H H
       H H H H H H H H H H H H H H H H))))

;;; SAR Sat Jul  7 15:11:51 BST 2012
(sc-deftest test-webpage-fibonacci-remix-in-1 ()
  (sc-test-check 
    (equalp (remix-in '(1 2 3 4 5 6 7 8 9 10 11))
            '(1 2 3 4 5 1 6 7 2 8 9 3 10 4 11 5))))

;;; SAR Sat Jul  7 15:13:03 BST 2012
(sc-deftest test-webpage-fibonacci-remix-in-2 ()
  (sc-test-check
    (equalp (remix-in '(1 2 3 4 5 6 7 8 9 10 11) :remix-in-fib-seed 1)
            '(1 2 3 1 4 2 5 3 6 4 7 5 8 6 9 7 10 8 11 9))))

;;; SAR Sat Jul  7 15:23:12 BST 2012
(sc-deftest test-webpage-fibonacci-usage-example-1 ()
  (let* ((o-files '("slippery-chicken-piece.mid" 
                    "_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (slippery-chicken-piece
          (make-slippery-chicken
           '+slippery-chicken-piece+
           :title "slippery-chicken-piece"
           :instrument-palette +slippery-chicken-standard-instrument-palette+
           :ensemble '(((vn (violin :midi-channel 1))))
           :tempo-map '((1 (q 60)))
           :set-palette '((1 ((c4 d4 f4 g4 a4)))
                          (2 ((cs4 ds4 fs4 gs4 as4))))
           :set-map (list (list 1 (fibonacci-transition 17 1 2)))
           :rthm-seq-palette '((1 ((((2 4) q - e s 32 32 -))
                                   :pitch-seq-palette ((1 2 3 4 5)))))
           :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))))))
    (loop for f in o-files 
       do (probe-delete (concatenate 'string "/tmp/" f)))
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (list (get-pitch-symbol ne)
                        (data ne)))
       '((C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (C4 Q) (D4 E) (F4 S) (G4 32) 
         (A4 32) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (C4 Q) (D4 E) (F4 S) 
         (G4 32) (A4 32) (CS4 Q) (DS4 E) (FS4 S) (GS4 32) (AS4 32) (C4 Q) 
         (D4 E) (F4 S) (G4 32) (A4 32) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32)
         (CS4 Q) (DS4 E) (FS4 S) (GS4 32) (AS4 32) (C4 Q) (D4 E) (F4 S) (G4 32)
         (A4 32) (CS4 Q) (DS4 E) (FS4 S) (GS4 32) (AS4 32) (CS4 Q) (DS4 E) 
         (FS4 S) (GS4 32) (AS4 32) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (CS4 Q) 
         (DS4 E) (FS4 S) (GS4 32) (AS4 32) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) 
         (CS4 Q) (DS4 E) (FS4 S) (GS4 32) (AS4 32) (CS4 Q) (DS4 E) (FS4 S) 
         (GS4 32) (AS4 32) (CS4 Q) (DS4 E) (FS4 S) (GS4 32) (AS4 32)))
      (midi-play slippery-chicken-piece :midi-file "/tmp/slippery-chicken-piece.mid")
      (write-lp-data-for-all slippery-chicken-piece :base-path "/tmp/")
      (notany #'not
              (loop for f in o-files
                 for s in '(760 150 650 165 800)
                 collect 
                   (file-write-ok (concatenate 'string "/tmp/" f) s))))))

;;; SAR Sat Jul  7 15:29:08 BST 2012
(sc-deftest test-webpage-fibonacci-usage-example-2 ()
  (let* ((o-files '("slippery-chicken-piece.mid" 
                    "_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (slippery-chicken-piece
          (make-slippery-chicken
           '+slippery-chicken-piece+
           :title "slippery-chicken-piece"
           :instrument-palette +slippery-chicken-standard-instrument-palette+
           :ensemble '(((vn (violin :midi-channel 1))))
           :tempo-map '((1 (q 60)))
           :set-palette '((1 ((c4 d4 f4 g4 a4))))
           :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((2 4) q - e s 32 32 -))
                                   :pitch-seq-palette ((1 2 3 4 5))))
                               (2 ((((2 4) (q) (s) - s s - (s)))
                                   :pitch-seq-palette ((1 2)))))
           :rthm-seq-map (list 
                          (list 1 
                                (list 
                                 (list 'vn 
                                       (fibonacci-transition 17 1 2))))))))
    (loop for f in o-files 
       do (probe-delete (concatenate 'string "/tmp/" f)))
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (list (get-pitch-symbol ne)
                        (data ne)))
       '((C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (C4 Q) (D4 E) (F4 S) (G4 32) 
         (A4 32) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (C4 Q) (D4 E) (F4 S) 
         (G4 32) (A4 32) (NIL Q) (NIL S) (C4 S) (D4 S) (NIL S) (C4 Q) (D4 E) 
         (F4 S) (G4 32) (A4 32) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (NIL Q) 
         (NIL S) (C4 S) (D4 S) (NIL S) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) 
         (NIL Q) (NIL S) (C4 S) (D4 S) (NIL S) (NIL Q) (NIL S) (C4 S) (D4 S) 
         (NIL S) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (NIL Q) (NIL S) (C4 S) 
         (D4 S) (NIL S) (C4 Q) (D4 E) (F4 S) (G4 32) (A4 32) (NIL Q) (NIL S) 
         (C4 S) (D4 S) (NIL S) (NIL Q) (NIL S) (C4 S) (D4 S) (NIL S) (NIL Q) 
         (NIL S) (C4 S) (D4 S) (NIL S)))
      (midi-play slippery-chicken-piece :midi-file "/tmp/slippery-chicken-piece.mid")
      (write-lp-data-for-all slippery-chicken-piece :base-path "/tmp/")
      (notany #'not
              (loop for f in o-files
                 for s in '(560 150 650 165 740)
                 collect 
                   (file-write-ok (concatenate 'string "/tmp/" f) s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instruments.html

;;; SAR Sat Jul  7 17:51:20 BST 2012
(sc-deftest test-instruments-instrument-objects ()
  (let* ((inst-pal (make-instrument-palette 
                    'test-inst-pal
                    '((soprano
                       (:staff-name "soprano" :staff-short-name "s"
                        :lowest-written c4 :highest-written c6
                        :starting-clef treble
                        :midi-program 54)))))
         (inst (get-data 'soprano inst-pal)))
    (sc-test-check
      (instrument-palette-p inst-pal)
      (instrument-p inst)
      (equalp
       (loop for s in '(staff-name staff-short-name starting-clef midi-program) 
          collect (funcall s inst))
       '("soprano" "s" TREBLE 54))
      (equalp
       (loop for p in '(lowest-written highest-written)
          collect (data (funcall p inst)))
       '(C4 C6)))))


;; SAR Thu Mar 15 12:07:10 GMT 2012
(sc-deftest test-webpage-instruments-separate ()
  (sc-test-check
    (defparameter +pl-inst-pal+
      (make-instrument-palette
       'plucked-strings-aux-instrument-palette
       '((mandolin
          (:staff-name "mandolin" :staff-short-name "md"
           :lowest-written g3 :highest-written a6 :starting-clef treble
           :chords t :microtones nil :missing-notes nil 
           :midi-program 26))
         (tenor-banjo
          (:staff-name "tenor banjo" :staff-short-name "t-bj"
           :lowest-written c3 :highest-written a4 :starting-clef treble
           :transposition-semitones -12
           :chords t :microtones nil :missing-notes nil 
           :midi-program 106)))))
    (instrument-palette-p +pl-inst-pal+)
    (= (num-data +pl-inst-pal+) 2)
    (every #'instrument-p (data +pl-inst-pal+))  
    (equalp (loop for i in '(id staff-name staff-short-name 
                             starting-clef 
                             chords microtones missing-notes 
                             midi-program)  
               collect (funcall i (get-data 'mandolin +pl-inst-pal+)))
            '(MANDOLIN "mandolin" "md" TREBLE T NIL NIL 26))
    (equalp (data (lowest-written (get-data 'mandolin +pl-inst-pal+))) 'g3)
    (equalp (data (highest-written (get-data 'mandolin +pl-inst-pal+))) 'a6) 
    (equalp (loop for i in '(id staff-name staff-short-name 
                             starting-clef 
                             transposition-semitones
                             chords microtones missing-notes 
                             midi-program)  
               collect (funcall i (get-data 'tenor-banjo +pl-inst-pal+)))
            '(TENOR-BANJO "tenor banjo" "t-bj" TREBLE -12 T NIL NIL 106))
    (equalp (data (lowest-written (get-data 'tenor-banjo +pl-inst-pal+))) 'c3) 
    (equalp 
     (data (highest-written (get-data 'tenor-banjo +pl-inst-pal+)))
     'a4)))

;;; SAR Thu Mar 15 12:22:20 GMT 2012 - This is not the exact code from the
;;; webpage but tests the same function of using the COMBINE method with
;;; instrument-palettes
(sc-deftest test-webpage-instruments-combine ()
  (sc-test-check
    (defparameter +ip1+
      (make-instrument-palette
       'ip-1
       '((piccolo
          (:staff-name "piccolo" :lowest-written d4 :highest-written c7 
           :chords nil
           :staff-short-name "picc"
           :missing-notes nil :midi-program 73 :starting-clef treble 
           :transposition-semitones 12 :microtones t))
         (flute 
          (:staff-name "flute" :lowest-written c4 :highest-written d7 
           :chords nil 
           :missing-notes (cqs4 dqf4) :midi-program 74 :starting-clef treble
           :staff-short-name "fl" :microtones t)))))
    (defparameter +ip2+
      (make-instrument-palette
       'ip-2
       '((mandolin
          (:staff-name "mandolin" :staff-short-name "md"
           :lowest-written g3 :highest-written a6 :starting-clef treble
           :chords t :microtones nil :missing-notes nil 
           :midi-program 26)))))
    (= (num-data +ip1+) 2)
    (= (num-data +ip2+) 1)
    (= (num-data (combine +ip1+ +ip2+)) 3)
    (every #'instrument-p (data (combine +ip1+ +ip2+)))
    (equalp 
     (get-all-refs (combine +ip1+ +ip2+))
     '((PICCOLO) (FLUTE) (MANDOLIN)))))

;; SAR Thu Mar 15 12:41:58 GMT 2012 
;; NB: This is an sc-test-check within an sc-test-check, which is surely poor
;; programming practice, but allows me to create the global variable here.  It
;; means, though, that if an inner test fails, so does the outer and two FAIL
;; messages get printed.
(sc-deftest test-webpage-instruments-combine-2 ()
  (sc-test-check
    (defparameter +ip3+
      (make-instrument-palette
       'ip-1
       '((piccolo
          (:staff-name "piccolo" :lowest-written d4 :highest-written c7 
           :chords nil
           :staff-short-name "picc"
           :missing-notes nil :midi-program 73 :starting-clef treble 
           :transposition-semitones 12 :microtones t))
         (flute 
          (:staff-name "flute" :lowest-written c4 :highest-written d7 
           :chords nil 
           :missing-notes (cqs4 dqf4) :midi-program 74 :starting-clef treble 
           :staff-short-name "fl" :microtones t)))))
    (let ((combi-ip
           (combine +ip3+
                    (make-instrument-palette
                     'ip-4
                     '((mandolin
                        (:staff-name "mandolin" :staff-short-name "md"
                         :lowest-written g3 :highest-written a6 
                         :starting-clef treble
                         :chords t :microtones nil :missing-notes nil 
                         :midi-program 26)))))))
      (sc-test-check
        (= (num-data combi-ip) 3)
        (every #'instrument-p (data combi-ip))
        (equalp (get-all-refs combi-ip) '((PICCOLO) (FLUTE) (MANDOLIN)))))))

;;; SAR Sat Jul  7 18:12:03 BST 2012
(sc-deftest test-webpage-instruments-add ()
  (let* ((scsip-clone 
          (clone +slippery-chicken-standard-instrument-palette+))
         (scsip-len (length (data scsip-clone))))
    (sc-test-check
      ;; (= 31 (length (data scsip-clone)))
      ;; (equalp 'computer (id (first (last (data scsip-clone)))))
      (add
       ;; MDE Thu Oct 17 19:42:36 2013 -- we now have mandolin
       (make-instrument 'mandolino 
                        :staff-name "mandolin" :staff-short-name "md"
                        :lowest-written 'g3 :highest-written 'a6 
                        :starting-clef 'treble 
                        :chords t :microtones nil :missing-notes nil 
                        :midi-program 26)
       scsip-clone)
      (= (length (data scsip-clone)) (1+ scsip-len))
      (equalp 'mandolino (id (first (last (data scsip-clone))))))))

;;; SAR Thu Mar 15 12:56:44 GMT 2012
(sc-deftest test-webpage-instruments-changing ()
  (sc-test-check
    (defparameter +ip5+
      (make-instrument-palette
       'ip5
       '((piccolo
          (:staff-name "piccolo" :lowest-written d4 :highest-written c7 
           :chords nil
           :staff-short-name "picc"
           :missing-notes nil :midi-program 73 :starting-clef treble 
           :transposition-semitones 12 :microtones t))
         (flute 
          (:staff-name "flute" :lowest-written c4 :highest-written d7 
           :chords nil 
           :missing-notes (cqs4 dqf4) :midi-program 74 
           :starting-clef treble
           :staff-short-name "fl" :microtones t)))))
    (equalp (staff-name (get-data 'piccolo +ip5+)) "piccolo")
    (set-slot 'staff-name "kleine floete" 'piccolo +ip5+)
    (equalp (staff-name (get-data 'piccolo +ip5+)) "kleine floete")
    (not (loop for i in '(lowest-written highest-written microtones)
            for n in '(b3 c7 nil)
            do (set-slot i n 'flute +ip5+)))
    (equalp (loop for i in '(lowest-written highest-written microtones)
               collect (funcall i (get-data 'flute +ip5+))) 
            '(B3 C7 NIL))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; marks.html

;;; SAR Sat Jul  7 18:30:20 BST 2012
(sc-deftest test-webpage-marks-via-marks-1 ()
  (let ((o-files '("_slippery-chicken-piece-score.ly" 
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 12000 170 670 200 230))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) s x 16))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8 8
                                                         7 6 5 4 3 2 1)) 
                                  :marks (a 1 s 3 beg-sl 5 end-sl 6 
                                            dim-beg 9 dim-end 13)))) 
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((A) NIL (S) NIL (BEG-SL) (END-SL) NIL NIL (DIM-BEG) NIL NIL NIL 
         (DIM-END) NIL NIL NIL))
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:15:11 BST 2012
(sc-deftest test-webpage-marks-via-marks-dynamics ()
  (let ((o-files '("_slippery-chicken-piece-score.ly"
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 11000 120 670 200 180))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (ppp 1 cresc-beg 2 cresc-end 4 
                                              fff 5))))  
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((PPP) (CRESC-BEG) NIL (CRESC-END) (FFF) NIL NIL NIL))
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:19:21 BST 2012
(sc-deftest test-webpage-marks-via-marks-multi-rthms-per-mark ()
  (let ((o-files '("_slippery-chicken-piece-score.ly" 
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 8000 120 670 200 190))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (a 1 4 s 5 7 8)))) 
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((A) (A) (A) (A) (S) NIL (S) (S)))
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:22:52 BST 2012
(sc-deftest test-webpage-marks-via-marks-slurs-phrases-long ()
  (let ( (o-files '("_slippery-chicken-piece-score.ly" 
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 8000 120 670 200 180))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (beg-sl 1 end-sl 2 beg-sl 5 
                                                 end-sl 6 
                                                 beg-phrase 1 
                                                 end-phrase 8))))   
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((BEG-PHRASE BEG-SL) (END-SL) NIL NIL (BEG-SL) (END-SL) NIL
         (END-PHRASE))) 
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:24:58 BST 2012
(sc-deftest test-webpage-marks-via-marks-slurs-phrases-short ()
  (let ((o-files '("_slippery-chicken-piece-score.ly" 
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 8000 120 670 200 180))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)) 
                                  :marks (slur 1 2 slur 5 6
                                               phrase 1 8))))   
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((BEG-PHRASE BEG-SL) (END-SL) NIL NIL (BEG-SL) (END-SL) NIL
         (END-PHRASE))) 
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:28:22 BST 2012
(sc-deftest test-webpage-marks-via-marks-user-defined-text ()
  (let ((o-files '("_slippery-chicken-piece-score.ly" 
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 8000 120 670 200 190))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)) 
                                  :marks ("etwas rascher" 1))))  
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '(("etwas rascher") NIL NIL NIL NIL NIL NIL NIL)) 
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:36:31 BST 2012
(sc-deftest test-webpage-marks-note-heads-1 ()
  (let ((o-files '("_slippery-chicken-piece-score.ly" 
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 8000 120 670 200 200))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (x-head 1 triangle-up 5))))  
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((X-HEAD) NIL NIL NIL (TRIANGLE-UP) NIL NIL NIL)) 
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:41:11 BST 2012
(sc-deftest test-webpage-marks-note-heads-2 ()
  (let ((o-files '("_slippery-chicken-piece-score.ly" 
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 12000 170 670 200 250))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) s x 16))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8 8
                                                         7 6 5 4 3 2 1))
                                  :marks (x-head 1 8))))  
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((X-HEAD) (X-HEAD) (X-HEAD) (X-HEAD) (X-HEAD) (X-HEAD) (X-HEAD) 
         (X-HEAD) NIL NIL NIL NIL NIL NIL NIL NIL)) 
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 13:45:10 BST 2012
(sc-deftest test-webpage-marks-note-heads-3 ()
  (let ((o-files '("_slippery-chicken-piece-score.ly" 
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(150 #+cmn 10000 120 650 160 250))
        (slippery-chicken-piece
         (make-slippery-chicken
          '+slippery-chicken-piece+
          :title "slippery-chicken-piece"
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (ppp 1 a 1 3 5 s 2 
                                              "like a whisper" 4 
                                              slur 6 7 x-head 3 8))))   
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '((A PPP) (S) (X-HEAD A) (X-HEAD "like a whisper") (X-HEAD A)
         (X-HEAD BEG-SL) (X-HEAD END-SL) (X-HEAD))) 
      (midi-play slippery-chicken-piece)
      #+cmn (cmn-display slippery-chicken-piece)
      (write-lp-data-for-all slippery-chicken-piece)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))


;;; SAR Mon Jul  9 13:56:23 BST 2012
(sc-deftest test-webpage-marks-lp-arrows ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly" 
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(150 650 160 780))
         (slippery-chicken-piece
          (make-slippery-chicken
           '+slippery-chicken-piece+
           :title "slippery-chicken-piece"
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))   
           :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (add-arrow-to-events +slippery-chicken-piece+ "start" "end" '(1 5) '(3 4) 'vn )
      (not (next-event slippery-chicken-piece 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'vn)
          while ne
          collect (marks ne))
       '(NIL NIL NIL NIL (START-ARROW) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL (END-ARROW) NIL NIL NIL NIL))
      (write-lp-data-for-all slippery-chicken-piece :base-path "/tmp/")
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 14:21:07 BST 2012
(sc-deftest test-webpage-marks-lp-graphics ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly" 
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"))
         (o-files-sizes '(150 635 165 235))
         (slippery-chicken-piece
          (make-slippery-chicken
           '+slippery-chicken-piece+
           :title "slippery-chicken-piece"
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-piece 'fl nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-piece 'fl)
          while ne
          collect (marks ne))
       '((AEOLIAN-DARK) NIL NIL (MPHONIC-ARR) NIL (ARROW-UP-DOWN) NIL NIL NIL
         NIL (BRACKET-END)))
      (write-lp-data-for-all slippery-chicken-piece :base-path "/tmp/"
                             :use-custom-markup t)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 14:26:50 BST 2012
(sc-deftest test-webpage-marks-fingerings ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly" 
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 670 200 170))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (add-mark-to-note slippery-chicken 1 2 'vn 1)
      (add-mark-to-note slippery-chicken 1 3 'vn 2)
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '(NIL (1) (2) NIL NIL NIL NIL NIL))
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))


;;; SAR Mon Jul  9 14:48:32 BST 2012
(sc-deftest test-webpage-marks-all-marks-cmn-and-lp ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"
                    "slippery-chicken-piece.mid"))
         (o-files-sizes '(170 #+cmn 25000 640 100 860 450))
         (marks-list (loop for m in '(a as at arco bartok
                                      batt col-legno cl clb
                                      clt cresc-beg 
                                      cresc-end dim-beg
                                      dim-end pause
                                      beg-gliss end-gliss
                                      lhp mv 
                                      harm open ord beg-8va
                                      end-8va beg-8vb
                                      end-8vb beg-phrase 
                                      end-phrase pizz pizzp
                                      poco-crini sv
                                      short-pause beg-sl
                                      end-sl 
                                      s I II III IV sp ped
                                      ped^ ped-up te ts
                                      t3 uc tc) 
                        for r from 1
                        collect m
                        collect r))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '((A) (AS) (AT) (ARCO) (BARTOK) (BATT) (COL-LEGNO) (CL) (CLB) (CLT)
         (CRESC-BEG) (CRESC-END) (DIM-BEG) (DIM-END) (PAUSE) (BEG-GLISS)
         (END-GLISS) (LHP) (MV) (HARM) (OPEN) (ORD) (BEG-8VA) (END-8VA) 
         (BEG-8VB) (END-8VB) (BEG-PHRASE) (END-PHRASE) (PIZZ) (PIZZP) 
         (POCO-CRINI) (SV) (SHORT-PAUSE) (BEG-SL) (END-SL) (S) (I) (II) (III) 
         (IV) (SP) (PED) (PED^) (PED-UP) (TE) (TS) (T3) (UC) (TC) NIL NIL NIL 
         NIL NIL NIL NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken :use-custom-markup t)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 15:05:00 BST 2012
(sc-deftest test-webpage-marks-cmn-only-marks ()
  (let* ((o-files '(#+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"))
         (o-files-sizes '(#+cmn 12000 140))
         (marks-list (loop for m in '(i-ii i-ii-iii ii-iii iii-iv nail stopped
                                      trill-f trill-n trill-s) 
                        for r from 1
                        collect m
                        collect r))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette `((1 ((((5 4) - e e e e - - e e e e - - e e -)) 
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8
                                                          1 2))
                                   :marks ,marks-list)))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '((I-II) (I-II-III) (II-III) (III-IV) (NAIL) (STOPPED) (TRILL-F) 
         (TRILL-N) (TRILL-S) NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 15:10:03 BST 2012
(sc-deftest test-webpage-marks-lp-only ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly" 
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 670 200 270))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (loop for f in o-files 
       do (probe-delete (concatenate 'string "/tmp/" f)))
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '(NIL NIL (CRESC-BEG HAIRPIN0) NIL (CRESC-END) (HAIRPIN0 DIM-BEG) NIL 
         (DIM-END)))
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 15:15:25 BST 2012
(sc-deftest test-webpage-marks-dynamics ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(160 #+cmn 20000 110 640 170 200))
         (marks-list (loop for m in '(pppp ppp pp p mp mf f ff fff ffff) 
                        for r from 1
                        collect m
                        collect r))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette `((1 ((((5 4) - e e e e - - e e e e - - e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8 1 2)) 
                                   :marks ,marks-list)))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '((PPPP) (PPP) (PP) (P) (MP) (MF) (F) (FF) (FFF) (FFFF)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken :use-custom-markup t)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 15:19:13 BST 2012
(sc-deftest test-webpage-marks-dynamics-cmn-only ()
  (let* ((o-files '(#+cmn "slippery-chicken-piece.eps" "slippery-chicken-piece.mid"))
         (o-files-sizes '(#+cmn 28000 140))
         (marks-list (loop for m in '(pppp-p ppp-p pp-p p-p mp-p mf-p f-p ff-p
                                      fff-p ffff-p)  
                        for r from 1
                        collect m
                        collect r))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette `((1 ((((5 4) - e e e e - - e e e e - - e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8 1 2)) 
                                   :marks ,marks-list)))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '((PPPP-P) (PPP-P) (PP-P) (P-P) (MP-P) (MF-P) (F-P) (FF-P) (FFF-P)
         (FFFF-P))) 
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 15:24:23 BST 2012
(sc-deftest test-webpage-marks-note-heads ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 7000 120 670 200 270))
         (marks-list (loop for m in '(circled-x flag-head triangle-up x-head)  
                        for r from 1
                        collect m
                        collect r))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                   :marks ,marks-list)))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '((CIRCLED-X) (FLAG-HEAD) (TRIANGLE-UP) (X-HEAD) NIL NIL NIL NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 15:29:14 BST 2012
(sc-deftest test-webpage-marks-note-heads-cmn-only ()
  (let* ((o-files '(#+cmn "slippery-chicken-piece.eps" "slippery-chicken-piece.mid"))
         (o-files-sizes '(#+cmn 7000 120))
         (marks-list (loop for m in '(airy-head arrow-down arrow-up slash square
                                      none)  
                        for r from 1
                        collect m
                        collect r))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                   :marks ,marks-list)))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '((AIRY-HEAD) (ARROW-DOWN) (ARROW-UP) (SLASH) (SQUARE) (NONE) NIL NIL)) 
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Mon Jul  9 15:33:41 BST 2012
(sc-deftest test-webpage-marks-note-heads-lp-only ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (marks-list (loop for m in '(triangle)  
                        for r from 1
                        collect m
                        collect r))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                   :marks ,marks-list)))
           :rthm-seq-map '((1 ((vn (1))))))))
    (loop for f in o-files 
       do (probe-delete (concatenate 'string "/tmp/" f)))
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne))
       '((TRIANGLE) NIL NIL NIL NIL NIL NIL NIL))
      (midi-play slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (notany #'not
              (loop for f in o-files
                 for s in '(190 120 670 200 223)
                 collect 
                   (file-write-ok (concatenate 'string "/tmp/" f) s))))))


;;; SAR Mon Jul  9 15:41:09 BST 2012
(sc-deftest test-webpage-marks-flag-dots-on-off ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((6 4) q. q. q. q.))
                                   :pitch-seq-palette ((1 2 3 4))
                                   :marks (flag-head 1 4))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (loop for f in o-files 
       do (probe-delete (concatenate 'string "/tmp/" f)))
    (sc-test-check
      (add-mark-before-note slippery-chicken 1 1 'vn 'flag-dots-on)
      (add-mark-before-note slippery-chicken 1 3 'vn 'flag-dots-off)
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks ne)
          collect (marks-before ne))
       '((FLAG-HEAD) (FLAG-DOTS-ON) (FLAG-HEAD) NIL (FLAG-HEAD) 
         (FLAG-DOTS-OFF) (FLAG-HEAD) NIL))
      (midi-play slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (notany #'not
              (loop for f in o-files
                 for s in '(190 80 670 200 260)
                 collect 
                   (file-write-ok (concatenate 'string "/tmp/" f) s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; notenames-scales.html

;; SAR Thu Mar 15 13:46:25 GMT 2012
(sc-deftest test-webpage-notenames-scales-scales ()
  (sc-test-check
    (in-scale :chromatic)
    (equalp (cm::object-name cm::*scale*) "chromatic-scale")
    (every (lambda (x y) (equal-within-tolerance x y .0001))
           (list (note-to-freq 'B3)
                 (note-to-freq 'BF3)
                 (note-to-freq 'BFF3)
                 (note-to-freq 'C4)
                 (note-to-freq 'CS4)
                 (note-to-freq 'CSS4))
           (list 246.94164752960205
                 233.08189153671265
                 220.0
                 261.6255569458008
                 277.1826362609863
                 293.6647605895996))
    (in-scale :twelfth-tone)
    (equalp (cm::object-name cm::*scale*) "twelfth-tone")
    (every (lambda (x y) (equal-within-tolerance x y .0001))
           (list (note-to-freq 'BTF3)
                 (note-to-freq 'BSF3)
                 (note-to-freq 'BQF3)
                 (note-to-freq 'FTS4)
                 (note-to-freq 'FSS4) 
                 (note-to-freq 'FQS4)
                 (note-to-freq 'FSSF4)
                 (note-to-freq 'FSTF4)
                 (note-to-freq 'FS4))
           (list 244.57573388145875
                 242.23248668749875
                 239.91169499521448
                 352.6064858525342
                 356.01744536887963
                 359.46140199740967
                 362.93863643189525
                 366.4495541188944
                 369.99440456398133))
    (in-scale :quarter-tone)
    (equalp (cm::object-name cm::*scale*) "quarter-tone")
    (every (lambda (x y) (equal-within-tolerance x y .0001))
           (list  (note-to-freq 'BQF3)
                  (note-to-freq 'FQS4))
           (list 239.91169499521448
                 359.46140199740967))))

;;; SAR Mon Jul  9 15:59:19 BST 2012
(sc-deftest test-webpage-notenames-scales-scales-2 ()
  (sc-test-check
    (in-scale :chromatic)
    (make-pitch 'bf3)
    (equal-within-tolerance (frequency (make-pitch 'bf3)) 233.08 0.01)
    (make-pitch 'cs4)
    (equal-within-tolerance (frequency (make-pitch 'cs4)) 277.18 0.01)
    (in-scale :quarter-tone)
    (make-pitch 'bqf3)
    (equal-within-tolerance (frequency (make-pitch 'bqf3)) 239.91 0.01)  
    (make-pitch 'fqs4)
    (equal-within-tolerance (frequency (make-pitch 'fqs4)) 359.46 0.01)
    (in-scale :twelfth-tone)
    (make-pitch 'bsf3)
    (equal-within-tolerance (frequency (make-pitch 'bsf3)) 242.23 0.01)
    (make-pitch 'cts4)
    (equal-within-tolerance (frequency (make-pitch 'cts4)) 264.16 0.01)
    (make-pitch 'css4)
    (equal-within-tolerance (frequency (make-pitch 'css4)) 266.71 0.01)
    (in-scale :quarter-tone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects-slots.html

;;; SAR Mon Jul  9 16:26:38 BST 2012
(sc-deftest test-webpage-objects-slots-make-instance ()
  (let ((al (make-instance 'assoc-list :id 'al-examp 
                           :data '((3 17) (ob bf3) (c4 q)))))
    (sc-test-check
      (equalp (id al) 'al-examp)
      (equalp (get-keys al) '(3 ob c4))
      (every #'named-object-p (data al))
      (equalp
       (loop for k in (get-keys al)
          collect (get-data-data k al))
       '(17 BF3 Q)))))

;;; SAR Mon Jul  9 16:31:41 BST 2012
(sc-deftest test-webpage-objects-slots-methods ()
  (let* ((al-object-1 
          (make-instance 'assoc-list 
                         :id 'al-object-1 
                         :data '((3 17) (ob bf3) (c4 q)))))
    (sc-test-check
      (equalp 3 (id (get-first al-object-1)))
      (equalp '17 (data (get-first al-object-1))))))


;;; SAR Mon Jul  9 16:34:59 BST 2012
(sc-deftest test-webpage-objects-slots-functions-arguments ()
  (sc-test-check
    (equalp (get-harmonics 63 :start-partial 2 :max-freq 1010)
            '(126 189 252 315 378 441 504 567 630 693 756 819 882 945 1008))))

;;; SAR Mon Jul  9 16:39:35 BST 2012
(sc-deftest test-webpage-objects-slots-make-functions ()
  (let ((r (make-rhythm 16 :is-rest t)))
    (sc-test-check
      (rhythm-p r)
      (is-rest r))))

;;; SAR Mon Jul  9 16:49:24 BST 2012
(sc-deftest test-webpage-objects-slots-make-sc ()
  (progn
    (in-scale :quarter-tone)
    (let* ((o-files '("_slippery-chicken-piece-score.ly"
                      #+cmn "slippery-chicken-piece.eps"
                      "slippery-chicken-piece.mid"
                      "slippery-chicken-piece-def.ly"
                      "slippery-chicken-piece-vn-part.ly"
                      "slippery-chicken-piece-vn.ly"))
           (o-file-sizes '(190 #+cmn 6000 70 670 200 150))
           (slippery-chicken
            (make-slippery-chicken
             '+slippery-chicken+
             :ensemble '(((vn (violin :midi-channel 1))))
             :set-palette '((1 ((c4 e4 g4))))
             :set-map '((1 (1)))
             :rthm-seq-palette '((1 ((((2 4) q e e))
                                     :pitch-seq-palette ((1 2 3)))))
             :rthm-seq-map '((1 ((vn (1))))))))
      (probe-delete-multi "/tmp/" o-files)
      (sc-test-check
        (not (next-event slippery-chicken 'vn nil 1))
        (equalp
         (loop for ne = (next-event slippery-chicken 'vn)
            while ne
            collect (get-pitch-symbol ne)
            collect (data ne))
         '(C4 Q E4 E G4 E))
        (midi-play slippery-chicken)
        #+cmn (cmn-display slippery-chicken)
        (write-lp-data-for-all slippery-chicken)
        (file-write-ok-multi "/tmp/" o-files o-file-sizes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output.html

;;; SAR Mon Jul  9 18:38:12 BST 2012
#+cmn 
(sc-deftest test-webpage-output-cmn-display-sp ()
  (let* ((sp (make-set-palette 
              'sp-data
              '((set1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)))
                (set2 ((c3 fs3 cs4 e4 g4 a4 b4 c5 df5 f5 g5 af5 ef6)))
                (set3 ((d3 f3 cs4 e4 fs4 a4 b4 c5 d5 e5 fs5 af5 ef6)))
                (set4 ((d3 e3 cs4 ef4 fs4 a4 b4 c5 d5 e5 fs5 af5 d6)))))))
    (probe-delete "/tmp/sp-out.eps")
    (sc-test-check
      (cmn-display sp :file "/tmp/sp-out.eps" 
                   :break-line-each-set nil 
                   :size 16)
      (file-write-ok "/tmp/sp-out.eps" 18000))))

;;; SAR Mon Jul  9 18:43:40 BST 2012
#+cmn 
(sc-deftest test-webpage-output-cmn-display-rsp ()
  (let* ((rsp (make-rsp
               'rsp-frag
               '((seq1 ((((2 4) q +e. s)
                         ((s) e (s) q)
                         (+e. s { 3 (te) te te } ))
                        :pitch-seq-palette (1 2 3 4 5 6 7)))
                 (seq2 ((((3 4) (e.) s { 3 te te te } +q)
                         ({ 3 +te (te) te } e e (q)))
                        :pitch-seq-palette (2 3 4 5 6 7 8)))
                 (seq3 ((((2 4) e e { 3 te te te })
                         ((5 8) (e) e e e s s))
                        :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2)))))))
    (probe-delete "/tmp/rsp-out.eps")
    (sc-test-check
      (cmn-display rsp :file "/tmp/rsp-out.eps")
      (file-write-ok "/tmp/rsp-out.eps" 23000))))

;;; SAR Mon Jul  9 18:49:27 BST 2012
#+cmn 
(sc-deftest test-webpage-output-cmn-display-sets-in-scores ()
  (let* ((sc-piece
          (make-slippery-chicken
           '+sc-piece+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5)))
                          (2 ((af3 bf3 c4 df4 ef4 f4 g4 af4)))
                          (3 ((fs3 gs3 as3 b3 cs4 ds4 es4 fs4))))
           :set-map '((1 (1 2 3)))
           :rthm-seq-palette '((1 ((((2 4) (e) e e e))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (probe-delete "/tmp/sc-piece.eps")
    (sc-test-check
      (cmn-display sc-piece :file "/tmp/sc-piece.eps" :display-sets t)
      (file-write-ok "/tmp/sc-piece.eps" 22000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pitches.html

;;; SAR Thu Mar 15 13:59:42 GMT 2012
(sc-deftest test-webpage-pitches-set-palette ()
  (let ((sp (make-set-palette 
             'test
             '((set1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)))
               (set2 ((c3 fs3 cs4 e4 g4 a4 b4 c5 df5 f5 g5 af5 ef6)))
               (set3 ((d3 f3 cs4 e4 fs4 a4 b4 c5 d5 e5 fs5 af5 ef6)))
               (set4 ((d3 e3 cs4 ef4 fs4 a4 b4 c5 d5 e5 fs5 af5 d6))))))) 
    #+cmn (probe-delete "/tmp/slippery-chicken.eps")
    (sc-test-check
      (every #'sc-set-p (data sp))
      (equalp
       (loop for cs in (data sp) 
          collect (pitch-list-to-symbols (data cs)))
       '((C3 G3 CS4 E4 FS4 A4 BF4 C5 D5 F5 GF5 AF5 EF6)
         (C3 FS3 CS4 E4 G4 A4 B4 C5 DF5 F5 G5 AF5 EF6)
         (D3 F3 CS4 E4 FS4 A4 B4 C5 D5 E5 FS5 AF5 EF6)
         (D3 E3 CS4 EF4 FS4 A4 B4 C5 D5 E5 FS5 AF5 D6)))
      #+cmn (cmn-display sp :break-line-each-set nil :size 16)
      #+cmn (file-write-ok "/tmp/test.eps" 18000))))

;;; SAR Tue Jul 10 14:10:28 BST 2012
(sc-deftest test-webpage-pitches-two-item-set ()
  (let ((o-files '("_slippery-chicken-piece-score.ly"
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (file-sizes '(190 #+cmn 7000 120 670 200 180))
        (slippery-chicken
         (make-slippery-chicken
          '+slippery-chicken+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '(((1 ((gs4 bf4)))) :recurse-simple-data nil)
          :set-map '((1 (1)))
          :rthm-seq-palette `((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7
                                                         8))))) 
          :rthm-seq-map '((1 ((vn (1))))))))
    (loop for f in o-files 
       do (probe-delete (concatenate 'string "/tmp/" f)))
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(GS4 GS4 GS4 BF4 BF4 BF4 BF4 BF4))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files file-sizes))))

;;; SAR Tue Jul 10 16:01:25 BST 2012
(sc-deftest test-webpage-pitches-set-map ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 211000 1800 670 200 1800))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken :display-sets t)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Tue Jul 10 17:55:40 BST 2012
(sc-deftest test-webpage-pitches-pitch-curves-1 ()
  (let ((o-files '("_slippery-chicken-piece-score.ly"
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vn-part.ly"
                   "slippery-chicken-piece-vn.ly"))
        (o-files-sizes '(190 #+cmn 6500 120 670 200 170))
        (slippery-chicken
         (make-slippery-chicken
          '+slippery-chicken+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 4 3 2 5 7 8 6)))))
          :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Tue Jul 10 18:13:38 BST 2012
(sc-deftest test-webpage-pitches-one-curve-two-insts ()
  (let* ((scsip-clone (clone +slippery-chicken-standard-instrument-palette+)))
    (set-slot 'starting-clef 'tenor 'bassoon scsip-clone)
    (let* ((o-files '("_slippery-chicken-piece-score.ly"
                      #+cmn "slippery-chicken-piece.eps"
                      "slippery-chicken-piece.mid"
                      "slippery-chicken-piece-bn-part.ly"
                      "slippery-chicken-piece-bn.ly"
                      "slippery-chicken-piece-def.ly"
                      "slippery-chicken-piece-fl-part.ly"
                      "slippery-chicken-piece-fl.ly"))
           (o-files-sizes '(190 #+cmn 12000 200 200 170 900 200 180))
           (slippery-chicken
            (make-slippery-chicken
             '+slippery-chicken+
             :instrument-palette scsip-clone
             :ensemble '(((fl (flute :midi-channel 1))
                          (bn (bassoon :midi-channel 2))))
             :set-palette '((1 ((b3 d4 g4 b4 e5 a5 d6 a6 b6))))
             :set-map '((1 (1)))
             :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                     :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
             :rthm-seq-map '((1 ((fl (1))
                                 (bn (1))))))))
      (probe-delete-multi "/tmp/" o-files)
      (sc-test-check
        (midi-play slippery-chicken)
        #+cmn (cmn-display slippery-chicken)
        (write-lp-data-for-all slippery-chicken)
        (file-write-ok-multi "/tmp/" o-files o-files-sizes)))))

;;; SAR Tue Jul 10 18:28:56 BST 2012
(sc-deftest test-webpage-pitches-melodic-octaves ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"))
         (o-files-sizes-1 '(190 #+cmn 11000 260 670 200 290))
         (o-files-sizes-2 '(190 #+cmn 11000 260 670 200 290))
         (slippery-chicken-1
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((fl (flute :midi-channel 1))))
           :set-palette '((1 ((c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map '((1 ((fl (1 1 1)))))))
         (slippery-chicken-2
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((fl (flute :midi-channel 1))))
           :set-palette '((1 ((c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :avoid-melodic-octaves nil
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map '((1 ((fl (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken-1 'fl nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-1 'fl)
          while ne
          collect (get-pitch-symbol ne))
       '(C5 D5 E5 F5 G5 A5 B5 C6 D5 D5 E5 F5 G5 A5 B5 C6 D5 D5 E5 F5 G5 A5 B5 
         C6))
      (midi-play slippery-chicken-1)
      #+cmn (cmn-display slippery-chicken-1)
      (write-lp-data-for-all slippery-chicken-1)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes-1)
      (not (next-event slippery-chicken-2 'fl nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken-2 'fl)
          while ne
          collect (get-pitch-symbol ne))
       '(C5 D5 E5 F5 G5 A5 B5 C6 C5 D5 E5 F5 G5 A5 B5 C6 C5 D5 E5 F5 G5 A5 B5 
         C6))
      (probe-delete-multi "/tmp/" o-files)
      (midi-play slippery-chicken-2)
      #+cmn (cmn-display slippery-chicken-2)
      (write-lp-data-for-all slippery-chicken-2)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes-2))))

;;; SAR Tue Jul 10 18:48:08 BST 2012
(sc-deftest test-webpage-pitches-pitch-seq-chords ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-pn-part.ly"
                    "slippery-chicken-piece-pn.ly"))
         (o-files-sizes '(190 #+cmn 8000 190 670 200 230))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((pn (piano :midi-channel 1))))
           :set-palette '((1 ((c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 (4) (3) 2 5 (7) 8 
                                                          6))))) 
           :rthm-seq-map '((1 ((pn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'pn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'pn)
          while ne
          collect (chord-p (pitch-or-chord ne)))
       '(NIL T T NIL NIL T NIL NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Mar 15 17:56:48 GMT 2012
;;; SAR Tue Jul 10 19:07:11 BST 2012: Replaced this test
(sc-deftest test-webpage-pitches-multi-ps ()
  (let* ((o-files '("_multiple-pitch-seqs-score.ly"
                  "multiple-pitch-seqs-cl-part.ly"
                  "multiple-pitch-seqs-cl.ly"
                  "multiple-pitch-seqs-def.ly"
                  "multiple-pitch-seqs-fl-part.ly"
                  "multiple-pitch-seqs-fl.ly"
                  "multiple-pitch-seqs-ob-part.ly"
                  "multiple-pitch-seqs-ob.ly"))
       (o-files-sizes '(180 200 170 900 200 180 200 170))
       (multi-ps
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
  (probe-delete-multi "/tmp/" o-files)
  (sc-test-check
    (equalp
     (loop for p in '(fl ob cl)
        do 
          (next-event multi-ps p nil 1)
        collect 
          (loop for ne = (next-event multi-ps p)
             while ne
             collect (get-pitch-symbol ne)))
     '((C5 B4 C5 B4 C5 B4 C5 B4) (F4 E4 D4 E4 F4 E4 D4 E4)
       (D4 A4 D4 A4 D4 A4 D4 A4)))
    (write-lp-data-for-all multi-ps)
    (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Tue Jul 10 22:09:34 BST 2012
(sc-deftest test-webpage-pitches-set-limits-high-low ()
  (let ((o-files '("_slippery-chicken-piece-score.ly"
                   #+cmn "slippery-chicken-piece.eps"
                   "slippery-chicken-piece.mid"
                   "slippery-chicken-piece-cb-part.ly"
                   "slippery-chicken-piece-cb.ly"
                   "slippery-chicken-piece-cl-part.ly"
                   "slippery-chicken-piece-cl.ly"
                   "slippery-chicken-piece-def.ly"
                   "slippery-chicken-piece-vc-part.ly"
                   "slippery-chicken-piece-vc.ly"))
        (o-files-sizes-1 '(190 #+cmn 84000 1900 200 610 200 580 900 200 860))
        (o-files-sizes-2 '(190 #+cmn 85000 1900 200 610 200 580 900 200 740))
        (slippery-chicken-1
         (make-slippery-chicken
          '+slippery-chicken+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))
                       (cb (double-bass :midi-channel 3))))
          :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                 c4 d4 e4 f4 g4 a4 b4 
                                 c5 d5 e5 f5 g5 a5 b5 c6))))
          :set-map `((1 ,(loop repeat 10 collect 1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map (list (list 1 
                                    (loop for p in '(cl vc cb)
                                       collect (list p (loop repeat 10 
                                                          collect 1)))))))
        (slippery-chicken-2
         (make-slippery-chicken
          '+slippery-chicken+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))
                       (cb (double-bass :midi-channel 3))))
          :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                 c4 d4 e4 f4 g4 a4 b4 
                                 c5 d5 e5 f5 g5 a5 b5 c6))))
          :set-limits-high '((cl (0 c6 50 c5 100 c6))
                             (vc (0 g4 50 c5 100 g4))
                             (cb (0 f3 50 b3 100 f3)))
          :set-map `((1 ,(loop repeat 10 collect 1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map (list (list 1 
                                    (loop for p in '(cl vc cb)
                                       collect (list p (loop repeat 10 
                                                          collect 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (loop for p in '(cl vc cb)
          do 
            (next-event slippery-chicken-1 p nil 1)
          collect 
            (loop for ne = (next-event slippery-chicken-1 p)
               while ne
               collect (get-pitch-symbol ne)))
       '((E3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3
          CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3
          FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3
          CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4)
         (C3 E4 F4 G4 A4 B4 C5 D5 C3 E4 F4 G4 A4 B4 C5 D5 C3 E4 F4 G4 A4 B4 C5
          D5 C3 E4 F4 G4 A4 B4 C5 D5 C3 E4 F4 G4 A4 B4 C5 D5 C3 E4 F4 G4 A4 B4
          C5 D5 C3 E4 F4 G4 A4 B4 C5 D5 C3 E4 F4 G4 A4 B4 C5 D5 C3 E4 F4 G4 A4
          B4 C5 D5 C3 E4 F4 G4 A4 B4 C5 D5)
         (D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4 G4
          G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4
          G4 G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4
          F4 G4 G4 D4 D4 E4 F4 F4 F4 G4 G4)))
      (midi-play slippery-chicken-1)
      #+cmn (cmn-display slippery-chicken-1)
      (write-lp-data-for-all slippery-chicken-1)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes-1)
      (equalp
       (loop for p in '(cl vc cb)
          do 
            (next-event slippery-chicken-2 p nil 1)
          collect 
            (loop for ne = (next-event slippery-chicken-2 p)
               while ne
               collect (get-pitch-symbol ne)))
       '((E3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3
          CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3
          FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3
          CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4 FS3 FS3 G3 A3 B3 CS4 D4 E4)
         (C3 C3 E4 F4 F4 F4 G4 G4 C3 C3 E4 F4 F4 F4 G4 G4 C3 E4 E4 F4 F4 G4 A4
          A4 C3 E4 F4 F4 G4 A4 A4 B4 C3 E4 F4 G4 A4 A4 B4 C5 E4 E4 F4 F4 G4 A4
          A4 B4 C3 E4 E4 F4 F4 G4 A4 A4 C3 E4 E4 F4 F4 G4 A4 A4 C3 C3 E4 F4 F4
          F4 G4 G4 C3 C3 E4 F4 F4 F4 G4 G4)
         (C4 C4 D4 E4 E4 E4 F4 F4 C4 C4 D4 E4 E4 E4 F4 F4 D4 D4 E4 F4 F4 F4 G4
          G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4
          G4 G4 D4 D4 E4 F4 F4 F4 G4 G4 D4 D4 E4 F4 F4 F4 G4 G4 C4 C4 D4 E4 E4
          E4 F4 F4 C4 C4 D4 E4 E4 E4 F4 F4)))
      (midi-play slippery-chicken-2)
      #+cmn (cmn-display slippery-chicken-2)
      (write-lp-data-for-all slippery-chicken-2)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes-2))))

;;; SAR Thu Mar 15 14:17:36 GMT 2012
;;; SAR Wed Jul 11 11:47:37 BST 2012 - Added cmn test
(sc-deftest test-webpage-pitches-subsets-related-sets-simple ()
  (let* ((sp (make-set-palette 
              'test
              '((1 ((f3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 g5)
                    :subsets ((pno1 (c4 e4 g4))
                              (pno2 (d4 f4 a4))
                              (mba1 (a3 c4 e4)))
                    :related-sets ((pno3 (gs4 bf4 df5))
                                   (mba2 (fs3 af3 cs4)))))))))
    #+cmn (probe-delete "/tmp/test.eps")
    (sc-test-check
      (equalp (loop for ss in (data (subsets (get-data 1 sp)))
                 collect (id ss)
                 collect (pitch-list-to-symbols (data ss)))
              '(PNO1 (C4 E4 G4) PNO2 (D4 F4 A4) MBA1 (A3 C4 E4)))
      (equalp (loop for ss in (data (related-sets (get-data 1 sp)))
                 collect (id ss)
                 collect (pitch-list-to-symbols (data ss)))
              '(PNO3 (GS4 bF4 DF5) MBA2 (FS3 AF3 CS4)))
      #+cmn (cmn-display sp :break-line-each-set nil :size 16)
      #+cmn (file-write-ok "/tmp/test.eps" 13000))))

;;; SAR Thu Mar 15 14:31:29 GMT 2012
;;; SAR Wed Jul 11 11:49:19 BST 2012: Added cmn test
(sc-deftest test-webpage-pitches-subsets-related-sets-nested ()
  (let ((sp (make-set-palette 
             'test
             '((1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)
                   :subsets ((piano ((pno1 (cs4 e4 fs4))
                                     (pno2 (e4 fs4 a4))))
                             (marimba ((mba1 (c3 g3 cs4))
                                       (mba2 (g3 cs4 e4)))))
                   :related-sets ((piano ((pno3 (d3 a3 d5))
                                          (pno4 (c3 g3 d5)))))))))))
    #+cmn (probe-delete "/tmp/test.eps")
    (sc-test-check
      (equalp (get-all-refs (subsets (get-data 1 sp)))
              '((PIANO PNO1) (PIANO PNO2) (MARIMBA MBA1) (MARIMBA MBA2)))
      (equalp (get-all-refs (related-sets (get-data 1 sp)))
              '((PIANO PNO3) (PIANO PNO4)))
      (equalp
       (loop for ref in (get-all-refs (subsets (get-data 1 sp)))
          collect (id (get-data ref (subsets (get-data 1 sp))))
          collect
            (pitch-list-to-symbols 
             (get-data-data ref (subsets (get-data 1 sp)))))
       '(PNO1 (CS4 E4 FS4) PNO2 (E4 FS4 A4) MBA1 (C3 G3 CS4) MBA2 (G3 CS4 E4))) 
      (equalp
       (loop for ref in (get-all-refs (related-sets (get-data 1 sp)))
          collect (id (get-data ref (related-sets (get-data 1 sp))))
          collect
            (pitch-list-to-symbols
             (get-data-data ref (related-sets (get-data 1 sp)))))
       '(PNO3 (D3 A3 D5) PNO4 (C3 G3 D5)))
      #+cmn (cmn-display sp :break-line-each-set nil :size 16)
      #+cmn (file-write-ok "/tmp/test.eps" 14000)
      )))

;;; SAR Wed Jul 11 12:09:08 BST 2012
(sc-deftest test-webpage-pitches-limit-pitches-via-subsets ()
  (progn
    (set-slot 'subset-id 'flute-notes 'flute 
              +slippery-chicken-standard-instrument-palette+) 
    (set-slot 'subset-id 'oboe-notes 'oboe
              +slippery-chicken-standard-instrument-palette+)
    (set-slot 'subset-id 'clarinet-notes 'b-flat-clarinet
              +slippery-chicken-standard-instrument-palette+)
    (let* ((o-files '("_subset-id-piece-score.ly"
                      "subset-id-piece-cl-part.ly"
                      "subset-id-piece-cl.ly"
                      "subset-id-piece-def.ly"
                      "subset-id-piece-fl-part.ly"
                      "subset-id-piece-fl.ly"
                      "subset-id-piece-ob-part.ly"
                      "subset-id-piece-ob.ly"))
           (o-files-sizes '(180 190 370 900 190 410 190 370))
           (subset-id-piece
            (make-slippery-chicken
             '+subset-id-piece+
             :title "subset id piece"
             :ensemble '(((fl (flute :midi-channel 1))
                          (ob (oboe :midi-channel 2))
                          (cl (b-flat-clarinet :midi-channel 3))))
             :set-palette 
             '((1 ((b3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 g5 a5 b5 c6
                       d6 e6 f6 g6 a6 b6) 
                   :subsets ((flute-notes (b5 c6 d6 e6 f6 g6 a6 b6))  
                             (oboe-notes (a4 b4 c5 d5 e5 f5 g5 a5)) 
                             (clarinet-notes (b3 c4 d4 e4 f4
                                                 g4))))))  
             :set-map '((1 (1 1 1)))
             :rthm-seq-palette '((1 ((((4 4) - e. s - - e e - 
                                       - +s s s s - - (s) s s s - )) 
                                     :pitch-seq-palette ((2 2 3 1 1 2 1 2 2 2) 
                                                         (6 8 5 5 7 7 9 6 8 10)  
                                                         (5 4 3 1 1 2 3 3 4 4) 
                                                         (1 3 3 2 1 2 3 1 1
                                                            1)))))  
             :rthm-seq-map '((1 ((fl (1 1 1))
                                 (ob (1 1 1))
                                 (cl (1 1 1))))))))
      (probe-delete-multi "/tmp/" o-files)
      (sc-test-check
        (equalp
         (loop for p in '(fl ob cl)
            do 
              (next-event subset-id-piece p nil 1)
            collect 
              (loop for ne = (next-event subset-id-piece p)
                 while ne
                 collect (list (get-pitch-symbol ne)
                               (data ne))))
         '(((C6 E.) (C6 S) (D6 E) (B5 E) (B5 "S") (B5 S) (C6 S) (B5 S) 
            (NIL S) (C6 S) (C6 S) (C6 S) (B5 E.) (D6 S) (D6 E) (C6 E) (C6 "S") 
            (B5 S) (C6 S) (D6 S) (NIL S) (B5 S) (B5 S) (B5 S) (F6 E.) (E6 S) 
            (D6 E) (B5 E) (B5 "S") (B5 S) (C6 S) (D6 S) (NIL S) (D6 S) (E6 S) 
            (E6 S))
           ((D5 E.) (F5 S) (C5 E) (C5 E) (C5 "S") (E5 S) (E5 S) (G5 S) (NIL S) 
            (D5 S) (F5 S) (A5 S) (B4 E.) (B4 S) (C5 E) (A4 E) (A4 "S") (A4 S) 
            (B4 S) (A4 S) (NIL S) (B4 S) (B4 S) (B4 S) (A4 E.) (C5 S) (C5 E) 
            (B4 E) (B4 "S") (A4 S) (B4 S) (C5 S) (NIL S) (A4 S) (A4 S) (A4 S)) 
           ((G4 E.) (FS4 S) (E4 E) (CS4 E) (CS4 "S") (CS4 S) (D4 S) (E4 S) 
            (NIL S) (E4 S) (FS4 S) (FS4 S) (D4 E.) (FS4 S) (CS4 E) (CS4 E) 
            (CS4 "S") (E4 S) (E4 S) (G4 S) (NIL S) (D4 S) (FS4 S) (A4 S) 
            (D4 E.) (D4 S) (E4 E) (CS4 E) (CS4 "S") (CS4 S) (D4 S) (CS4 S) 
            (NIL S) (D4 S) (D4 S) (D4 S))))
        (write-lp-data-for-all subset-id-piece :base-path "/tmp/")
        (file-write-ok-multi "/tmp/" o-files o-files-sizes)
        (every 
         #'not
         (loop for p in '(flute oboe b-flat-clarinet)
            collect
              (set-slot 'subset-id nil p 
                        +slippery-chicken-standard-instrument-palette+)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; post-gen-editing.html

;;; SAR Wed Jul 11 12:16:10 BST 2012
(sc-deftest test-webpage-post-gen-ed-getting-first-event ()
  (let* ((slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (sc-test-check
      (equalp 's (data (get-event slippery-chicken 1 1 'vn))))))

;;; SAR Wed Jul 11 12:18:12 BST 2012
(sc-deftest test-webpage-post-gen-ed-getting-first-note ()
  (let* ((slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (sc-test-check
      (equalp 'e (data (get-note slippery-chicken 1 1 'vn))))))

;;; SAR Wed Jul 11 12:37:45 BST 2012
(sc-deftest test-webpage-post-gen-ed-basic-usage ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 28000 540 900 200 250 200 280 200 250))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))
                        (va (viola :midi-channel 2))
                        (vc (cello :midi-channel 3))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6)))))
           :rthm-seq-map '((1 ((vn (1 1 1))
                               (va (1 1 1))
                               (vc (1 1 1)))))))
         ms)
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (notany #'not 
              (loop for m in
                   (loop for p in '(vn va vc)
                      do 
                        (next-event slippery-chicken p nil 1)
                      collect 
                        (loop for ne = (next-event slippery-chicken p)
                           while ne
                           collect (marks ne)))
                 collect (every #'not m)))
      (add-mark-to-note slippery-chicken 2 4 'va 'ppp)
      (setf ms (loop for p in '(vn va vc)
                  do 
                    (next-event slippery-chicken p nil 1)
                  collect 
                    (loop for ne = (next-event slippery-chicken p)
                       while ne
                       collect (marks ne))))
      (every #'not (first ms))
      (every #'not (third ms))
      (equalp (second ms) '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
                            NIL (PPP) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)) 
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 12:44:24 BST 2012
(sc-deftest test-webpage-post-gen-ed-change-pitch ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"))
         (o-files-sizes '(190 #+cmn 1000 210 670 200 240))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6)))))
           :rthm-seq-map '((1 ((va (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event +sc-object+ 'va nil 1))
      (equalp
       (loop for ne = (next-event +sc-object+ 'va)
          while ne
          collect (get-pitch-symbol ne))
       '(C3 NIL D3 E3 NIL F3 G3 A3 C3 NIL D3 E3 NIL F3 G3 A3 C3 NIL D3 E3 NIL
         F3 G3 A3))
      (change-pitch +sc-object+ 3 2 'va 'c4)
      (not (next-event +sc-object+ 'va nil 1))
      (equalp
       (loop for ne = (next-event +sc-object+ 'va)
          while ne
          collect (get-pitch-symbol ne))
       '(C3 NIL D3 E3 NIL F3 G3 A3 C3 NIL D3 E3 NIL F3 G3 A3 C3 NIL C4 E3 NIL
         F3 G3 A3))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 12:48:27 BST 2012
(sc-deftest test-webpage-post-gen-ed-add-mark-to-note ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"))
         (o-files-sizes '(190 #+cmn 11000 210 670 200 250))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))) 
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6)))))
           :rthm-seq-map '((1 ((va (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event +sc-object+ 'va nil 1))
      (every #'not
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (marks ne)))
      (add-mark-to-note +sc-object+ 3 2 'va 'ppp)
      (not (next-event +sc-object+ 'va nil 1))
      (equalp
       (loop for ne = (next-event +sc-object+ 'va)
          while ne
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL (PPP) NIL NIL NIL NIL NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 12:54:50 BST 2012
(sc-deftest test-webpage-post-gen-ed-rm-marks-from-note ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"))
         (o-files-sizes '(190 #+cmn 48000 260 670 200 350))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))) 
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e x 8))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                   :marks (ppp 1 8))))
           :rthm-seq-map '((1 ((va (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event +sc-object+ 'va nil 1))
      (every #'(lambda (x) (equalp x '(ppp)))
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (marks ne)))
      (rm-marks-from-note +sc-object+ 3 2 'va 'ppp)
      (not (next-event +sc-object+ 'va nil 1))
      (equalp
       (loop for ne = (next-event +sc-object+ 'va)
          while ne
          collect (marks ne))
       '((PPP) (PPP) (PPP) (PPP) (PPP) (PPP) (PPP) (PPP) (PPP) (PPP) (PPP) 
         (PPP) (PPP) (PPP) (PPP) (PPP) (PPP) NIL (PPP) (PPP) (PPP) (PPP) (PPP) 
         (PPP)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 13:02:37 BST 2012
(sc-deftest test-webpage-post-gen-ed-trill ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"))
         (o-files-sizes '(190 670 200 300))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e x 8))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map '((1 ((va (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event +sc-object+ 'va nil 1))
      (every #'not
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (marks-before ne)))
      (not (next-event +sc-object+ 'va nil 1))
      (every #'not
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (marks ne)))
      (trill +sc-object+ 'va 3 2 'd4)
      (not (next-event +sc-object+ 'va nil 1))
      (equalp
       (loop for ne = (next-event +sc-object+ 'va)
          while ne
          collect (list (marks-before ne)
                        (marks ne)))
       '((NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) 
         (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) 
         (NIL NIL) (NIL NIL) (NIL NIL) ((BEG-TRILL-A) ((TRILL-NOTE D4)))
         (NIL (END-TRILL-A)) (NIL NIL) (NIL NIL) (NIL NIL) (NIL NIL) 
         (NIL NIL)))
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 13:17:50 BST 2012
(sc-deftest test-webpage-post-gen-ed-tie-over-rests ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"))
         (o-files-sizes '(190 #+cmn 10000 190 670 200 230))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e (e) e (e) (e) e e e))
                                   :pitch-seq-palette ((1 2 3 4 5)))))
           :rthm-seq-map '((1 ((va (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event +sc-object+ 'va nil 1))
      (every #'(lambda (x) (equalp x '(nil nil)))
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (list (is-tied-to ne)
                              (is-tied-from ne))))
      (not (next-event +sc-object+ 'va nil 1))
      (every #'(lambda (x) (equalp 'e x))
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (data ne)))
      (not (next-event +sc-object+ 'va nil 1))
      (every #'not
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (beam ne)))
      (tie-over-rests +sc-object+ 1 1 'va :auto-beam 'q 
                           :consolidate-notes t)
      (not (next-event +sc-object+ 'va nil 1))
      (every #'(lambda (x) (equalp x '(nil nil)))
             (loop for ne = (next-event +sc-object+ 'va)
                while ne
                collect (list (is-tied-to ne)
                              (is-tied-from ne))))
      (not (next-event +sc-object+ 'va nil 1))
      (equalp
       (loop for ne = (next-event +sc-object+ 'va)
          while ne
          collect (data ne))
       '(Q E E E E E E E E E E E E E E E E E E E E E E))
      (not (next-event +sc-object+ 'va nil 1))
      (equalp
       (loop for ne = (next-event +sc-object+ 'va)
          while ne
          collect (beam ne))
       '(NIL NIL NIL NIL NIL 1 0 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 14:29:48 BST 2012
(sc-deftest test-webpage-post-gen-ed-re-bar ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"))
         (o-files-sizes '(190 #+cmn 32000 770 670 200 560))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 1))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
           :set-map (list (list 1 (loop repeat 20 collect 1)))
           :rthm-seq-palette '((1 ((((2 4) e x 4))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map (list (list 1 (list (list 'va
                                                   (loop repeat 20 
                                                      collect 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (= 20 (num-bars slippery-chicken))
      (every #'(lambda (x) (equalp x '(2 4)))
             (loop for b from 1 to (num-bars slippery-chicken)
                collect (data (get-time-sig (first (get-bar slippery-chicken b))))))
      (re-bar +sc-object+ :min-time-sig '(4 4))
      (= 10 (num-bars slippery-chicken))
      (every #'(lambda (x) (equalp x '(4 4)))
             (loop for b from 1 to (num-bars slippery-chicken)
                collect (data (get-time-sig (first (get-bar slippery-chicken b))))))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 14:41:02 BST 2012
(sc-deftest test-webpage-post-gen-ed-move-events ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 26000 420 900 200 290 200 280))
         (slippery-chicken       
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((vn (violin :midi-channel 1))
                        (vc (cello :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map (list (list 1 (loop repeat 5 collect 1)))
           :rthm-seq-palette '((1 ((((4 4) e x 8))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map (list 
                          (list 1 
                                (list 
                                 (list 'vn
                                       (loop repeat 5 collect 1))
                                 (list 'vc 
                                       (loop repeat 5 collect nil))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (loop for p in '(vn vc)
          do 
            (next-event slippery-chicken p nil 1)
          collect 
            (loop for ne = (next-event slippery-chicken p)
               while ne
               collect (list (get-pitch-symbol ne)
                             (data ne))))
       '(((G3 E) (A3 E) (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) (G4 E) (A3 E) (A3 E)
          (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) (G4 E) (A3 E) (A3 E) (B3 E) (C4 E)
          (D4 E) (E4 E) (F4 E) (G4 E) (A3 E) (A3 E) (B3 E) (C4 E) (D4 E) (E4 E) 
          (F4 E) (G4 E) (A3 E) (A3 E) (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) 
          (G4 E)) 
         ((NIL 1) (NIL 1) (NIL 1) (NIL 1) (NIL 1))))
      (move-events +sc-object+ 'vn 'vc 1 1 3 1)
      (equalp
       (loop for p in '(vn vc)
          do 
            (next-event slippery-chicken p nil 1)
          collect 
            (loop for ne = (next-event slippery-chicken p)
               while ne
               collect (list (get-pitch-symbol ne)
                             (data ne))))
       '(((NIL 1) (NIL 1) (NIL E) (A3 E) (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) 
          (G4 E) (A3 E) (A3 E) (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) (G4 E) 
          (A3 E) (A3 E) (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) (G4 E))
         ((G3 E) (A3 E) (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) (G4 E) (A3 E) (A3 E)
          (B3 E) (C4 E) (D4 E) (E4 E) (F4 E) (G4 E) (A3 E) (NIL E) (NIL 4) 
          (NIL 4) (NIL 4) (NIL 1) (NIL 1))))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 14:48:41 BST 2012
(sc-deftest test-webpage-post-gen-ed-double-events ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 39000 710 900 200 271 200 270 200 310))
         (slippery-chicken       
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((vn (viola :midi-channel 1))
                        (va (viola :midi-channel 2))
                        (vc (cello :midi-channel 3))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map (list (list 1 (loop repeat 5 collect 1)))
           :rthm-seq-palette '((1 ((((4 4) e x 8))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map (list 
                          (list 1 
                                (list 
                                 (list 'vn
                                       (loop repeat 5 collect 1))
                                 (list 'va
                                       (loop repeat 5 collect nil))
                                 (list 'vc 
                                       (loop repeat 5 collect nil))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (loop for p in '(vn va vc)
          do 
            (next-event slippery-chicken p nil 1)
          collect 
            (loop for ne = (next-event slippery-chicken p)
               while ne
               collect (list (get-pitch-symbol ne)
                             (data ne))))
       '(((C3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E)
          (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E) (E3 E) (F3 E)
          (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E)
          (B3 E) (C4 E) (D3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E))
         ((NIL 1) (NIL 1) (NIL 1) (NIL 1) (NIL 1))
         ((NIL 1) (NIL 1) (NIL 1) (NIL 1) (NIL 1))))
      (double-events +sc-object+ 'vn '(va vc) 1 1 3 1)
      (equalp
       (loop for p in '(vn va vc)
          do 
            (next-event slippery-chicken p nil 1)
          collect 
            (loop for ne = (next-event slippery-chicken p)
               while ne
               collect (list (get-pitch-symbol ne)
                             (data ne))))
       
       '(((C3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E)
          (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E) (E3 E) (F3 E)
          (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E)
          (B3 E) (C4 E) (D3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E))
         ((C3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E)
          (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (NIL E) (NIL 4) 
          (NIL 4) (NIL 4) (NIL 1) (NIL 1))
         ((C3 E) (D3 E) (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (D3 E)
          (E3 E) (F3 E) (G3 E) (A3 E) (B3 E) (C4 E) (D3 E) (NIL E) (NIL 4) 
          (NIL 4) (NIL 4) (NIL 1) (NIL 1))))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rhythms.html

;;; SAR Thu Mar 15 17:09:07 GMT 2012
;;; SAR Wed Jul 11 15:03:48 BST 2012: Added file-write tests
(sc-deftest test-webpage-rhythms-durations-1 ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 8000 110 670 200 180))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) 1)
                                    (2 4 8 16 32 32)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (data ne))
       '(1 2 4 8 16 32 32))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Mar 15 18:02:52 GMT 2012
;;; SAR Wed Jul 11 15:07:06 BST 2012: Added file-write tests
(sc-deftest test-webpage-rhythms-durations-2 ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 7000 100 670 200 170))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) w)
                                    (h q e s s)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (data ne))
       '(W H Q E S S))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Mar 15 18:07:40 GMT 2012
;;; SAR Wed Jul 11 15:10:58 BST 2012: Added file-write tests
(sc-deftest test-webpage-rhythms-rests-1 ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 7000 70 670 200 150))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) h (q) e (16) 16)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (is-rest ne)))
       '((H NIL) (Q T) (E NIL) (16 T) (16 NIL)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 15:14:14 BST 2012
(sc-deftest test-webpage-rhythms-rests-2 ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 7000 70 670 200 170))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) (s) (s) e s (s) (s) (s) (e) (e)
                                     q)))))   
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (is-rest ne)))
       '((S T) (S T) (E NIL) (S NIL) (S T) (S T) (S T) (E T) (E T) (Q NIL)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Mar 15 18:09:12 GMT 2012
;;; SAR Wed Jul 11 15:18:24 BST 2012: Added file-write tests
(sc-deftest test-webpage-rhythms-dots ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 8000 100 670 200 170))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) e. s (e..) 32 (8\.) 16 8\.. 32))))) 
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (data ne))
       '(E. S E.. 32 |8.| 16 |8..| 32))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Mar 15 18:11:29 GMT 2012
;;; SAR Wed Jul 11 16:41:54 BST 2012: Added file-write tests
(sc-deftest test-webpage-rhythms-ties ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 9000 110 670 200 200))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) q+e e 4+8 8)
                                    (+q +e e 4 \+8 8)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (is-tied-to ne)
                        (is-tied-from ne)))
       '(("Q" NIL T) ("E" T NIL) (E NIL NIL) ("4" NIL T) ("8" T NIL) 
         (8 NIL T) ("Q" T T) ("E" T NIL) (E NIL NIL) (4 NIL T) ("8" T NIL) 
         (8 NIL NIL)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 17:04:54 BST 2012
;;; MDE Mon Jun 29 15:45:00 2015 -- no longer valid since 1.0.6 -- need tuplet
;;; numbers now
#|
(sc-deftest test-webpage-rhythms-tuplets-no-brackets-1 ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 6000 90 670 200 200))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) - { 3 12 12 - (12) } -
                                     { 5 20 20 (10) 20 }  -))))) 
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (tuplet-scaler ne)
                        (bracket ne)))
       '((12 2/3 ((1 3))) (12 2/3 (-1)) (12 2/3 (1)) (20 4/5 ((2 5))) 
         (20 4/5 (-2)) (10 4/5 (-2)) (20 4/5 (2))))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 17:10:39 BST 2012
(sc-deftest test-webpage-rhythms-tuplets-no-brackets-2 ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 6000 90 670 200 200))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) - te t8 - (te) - fs fs (f8) f16
                                     -))))) 
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (tuplet-scaler ne)
                        (bracket ne)))
       '((te 2/3 NIL) (t8 2/3 NIL) (te 2/3 NIL) (fs 4/5 NIL) (fs 4/5 NIL)
         (f8 4/5 NIL) (f16 4/5 NIL)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 17:18:11 BST 2012
;;; MDE Mon Jun 29 15:44:09 2015 -- no longer valid

(sc-deftest test-webpage-rhythms-tuplets-partial-beats ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 8000 110 670 200 190))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) { 3 te e ts s ts s ts ts)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (tuplet-scaler ne)
                        (bracket ne)))
       '((TE 2/3 NIL) (E 1 NIL) (TS 2/3 NIL) (S 1 NIL) (TS 2/3 NIL) (S 1 NIL) 
         (TS 2/3 NIL) (TS 2/3 NIL)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))
|#

;;; SAR Wed Jul 11 17:16:55 BST 2012
(sc-deftest test-webpage-rhythms-tuplets-brackets-numbers ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 7000 90 670 200 200))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) { 3 te te (te) } 
                                     { 5 - fs fs (f8) f16 - } )))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (tuplet-scaler ne)
                        (bracket ne)))
       '((TE 2/3 ((1 3))) (TE 2/3 (-1)) (TE 2/3 (1)) (FS 4/5 ((2 5)))
         (FS 4/5 (-2)) (F8 4/5 (-2)) (F16 4/5 (2))))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 17:23:42 BST 2012
(sc-deftest test-webpage-rhythms-tuplets-brackets-nums-partial-beats ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 9000 120 670 200 230))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) s { 3 te ts } e { 3 t32 t32 } s  
                                     { 3 ts ts }))))) 
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (tuplet-scaler ne)
                        (bracket ne)))
       '((S 1 NIL) (TE 2/3 ((1 3))) (TS 2/3 (1)) (E 1 NIL) (T32 2/3 ((2 3))) 
         (T32 2/3 (2)) (S 1 NIL) (TS 2/3 ((3 3))) (TS 2/3 (3))))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 17:30:34 BST 2012
(sc-deftest test-webpage-rhythms-tuplets-nested ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 9000 120 670 200 220))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) { 3 12 12 12 } 
                                     { 3  12 12 { 3 36 36 36 } } ))))) 
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (print (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (tuplet-scaler ne)
                        (bracket ne))))
       '((12 2/3 ((1 3))) (12 2/3 (-1)) (12 2/3 (1)) (12 2/3 ((2 3)))
         (12 2/3 (-2)) (36 4/9 (-2 (3 3))) (36 4/9 (-2 -3)) (36 4/9 (2 3)))) 
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Mar 15 18:14:09 GMT 2012
;;; SAR Wed Jul 11 17:36:14 BST 2012: Replaced with better test
(sc-deftest test-webpage-rhythms-beams ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 8000 140 670 200 200))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) - e e - - s s s s - e - s s - 
                                     (s) - s (s) s -)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (data ne)
                        (beam ne)))
       '((E 1) (E 0) (S 1) (S NIL) (S NIL) (S 0) (E NIL) (S 1) (S 0) (S NIL) 
         (S 1) (S NIL) (S 0)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 17:39:28 BST 2012
(sc-deftest test-webpage-rhythms-rhythm-repeater ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 11000 170 670 200 210))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) s x 16)))))
           :rthm-seq-map '((1 ((vn (1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (every #'(lambda (x) (equalp x '(s)))
             (loop for ne = (next-event slippery-chicken 'vn)
                while ne
                collect (list (data ne))))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rsp-rsm.html

;;; SAR Wed Jul 11 17:53:39 BST 2012
(sc-deftest test-webpage-rsp-rsm-rthm-seq-measures ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 10000 170 670 200 240))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
           :set-map '((1 (1)))
           :rthm-seq-palette '((seq1 ((((2 4) (s) - s e - - e e -)
                                       (q - e s s -)
                                       ((5 8)  - e s s e - q)))))
           :rthm-seq-map '((1 ((vn (seq1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (data ne))
       '(S S E E E Q E S S E S S E Q))
      (equalp
       (loop for b from 1 to (num-bars slippery-chicken)
          collect (data (get-time-sig (get-bar slippery-chicken b 'vn))))
       '((2 4) (2 4) (5 8)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 18:03:20 BST 2012
(sc-deftest test-webpage-rsp-rsm-rthm-seq-pitch-curves ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 34000 550 670 200 620))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c4))))
           :set-map '((1 (1 1 1 1)))
           :rthm-seq-palette 
           '((seq1 ((((2 4) (s) - s e - - e e -)
                     (q - e s s -)
                     ((5 8)  - e s s e - q))
                    :pitch-seq-palette ((5 5 3 3 3 4 3 3 1 1 1 1 1) 
                                        (4 6 6 8 6 7 5 6 7 4 2 2 3)
                                        (3 4 1 1 1 3 3 2 1 1 2 2 3)))))
           :rthm-seq-map '((1 ((vn (seq1 seq1 seq1 seq1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (get-pitch-symbol ne)
                        (data ne)))
       '((NIL S) (G4 S) (G4 E) (E4 E) (E4 E) (E4 Q) (F4 E) (E4 S) (E4 S) (C4 E) 
         (C4 S) (C4 S) (C4 E) (C4 Q) (NIL S) (E4 S) (G4 E) (G4 E) (B4 E) (G4 Q) 
         (A4 E) (F4 S) (G4 S) (A4 E) (E4 S) (C4 S) (C4 E) (D4 Q) (NIL S) (E4 S) 
         (F4 E) (C4 E) (C4 E) (C4 Q) (E4 E) (E4 S) (D4 S) (C4 E) (C4 S) (D4 S) 
         (D4 E) (E4 Q) (NIL S) (G4 S) (G4 E) (E4 E) (E4 E) (E4 Q) (F4 E) (E4 S) 
         (E4 S) (C4 E) (C4 S) (C4 S) (C4 E) (C4 Q)))
      (equalp
       (loop for b from 1 to (num-bars slippery-chicken)
          collect (data (get-time-sig (get-bar slippery-chicken b 'vn))))
       '((2 4) (2 4) (5 8) (2 4) (2 4) (5 8) (2 4) (2 4) (5 8) (2 4) (2 4) 
         (5 8)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 18:14:58 BST 2012
(sc-deftest test-webpage-rsp-rsm-rthm-seq-marks ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"))
         (o-files-sizes '(190 670 200 710 #+cmn 37000 550))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c4))))
           :set-map '((1 (1 1 1 1)))
           :rthm-seq-palette 
           '((seq1 ((((2 4) (s) - s e - - e e -)
                     (q - e s s -)
                     ((5 8)  - e s s e - q))
                    :pitch-seq-palette ((5 5 3 3 3 4 3 3 1 1 1 1 1)
                                        (4 6 6 8 6 7 5 6 7 4 2 2 3)
                                        (3 4 1 1 1 3 3 2 1 1 2 2 3))
                    :marks (as 1 te 2 s 3 4 slur 5 6 dim-beg 9 dim-end 13)))) 
           :rthm-seq-map '((1 ((vn (seq1 seq1 seq1 seq1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (list (get-pitch-symbol ne)
                        (data ne)
                        (marks ne)))
       '((NIL S NIL) (G4 S (AS)) (G4 E (TE)) (E4 E (S)) (E4 E (S)) 
         (E4 Q (BEG-SL)) (F4 E (END-SL)) (E4 S NIL) (E4 S NIL) 
         (C4 E (DIM-BEG)) (C4 S NIL) (C4 S NIL) (C4 E NIL) (C4 Q (DIM-END))
         (NIL S NIL) (E4 S (AS)) (G4 E (TE)) (G4 E (S)) (B4 E (S)) 
         (G4 Q (BEG-SL)) (A4 E (END-SL)) (F4 S NIL) (G4 S NIL) (A4 E (DIM-BEG)) 
         (E4 S NIL) (C4 S NIL) (C4 E NIL) (D4 Q (DIM-END)) (NIL S NIL)
         (E4 S (AS)) (F4 E (TE)) (C4 E (S))
         (C4 E (S)) (C4 Q (BEG-SL)) (E4 E (END-SL)) (E4 S NIL) (D4 S NIL)
         (C4 E (DIM-BEG)) (C4 S NIL) (D4 S NIL) (D4 E NIL) (E4 Q (DIM-END))
         (NIL S NIL) (G4 S (AS)) (G4 E (TE)) (E4 E (S)) (E4 E (S)) 
         (E4 Q (BEG-SL))
         (F4 E (END-SL)) (E4 S NIL) (E4 S NIL) (C4 E (DIM-BEG)) (C4 S NIL)
         (C4 S NIL) (C4 E NIL) (C4 Q (DIM-END))))
      (equalp
       (loop for b from 1 to (num-bars slippery-chicken)
          collect (data (get-time-sig (get-bar slippery-chicken b 'vn))))
       '((2 4) (2 4) (5 8) (2 4) (2 4) (5 8) (2 4) (2 4) (5 8) (2 4) (2 4) 
         (5 8)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Jul 11 18:25:44 BST 2012
#+cmn
(sc-deftest test-webpage-rsp-rsm-rthm-seq-palette ()
  (let* ((rsp (make-rsp
               'rsp-test
               '((seq1 ((((2 4) (s) - s e - - e e -)
                         (q - e s s -)
                         ((5 8)  - e s s e - q))
                        :pitch-seq-palette ((5 5 3 3 3 4 3 3 1 1 1 1 1)
                                            (4 6 6 8 6 7 5 6 7 4 2 2 3)
                                            (3 4 1 1 1 3 3 2 1 1 2 2 3)) 
                        :marks (as 1 te 2 s 3 4 slur 5 6 dim-beg 9 
                                   dim-end 13)))
                 (seq2 ((((3 4) { 3 - te (te) te - } +q s (e.))
                         ((2 4) (q.) e)
                         (e q e))
                        :pitch-seq-palette ((6 3 5 5 2 2 2) 
                                            (1 3 5 4 5 2 1))
                        :marks (s 1 a 2 slur 2 4 s 5 6 te 7 s 8)))
                 (seq3 ((((7 8) - s s s s - +q - +e (s) s - (e)))
                        :pitch-seq-palette ((6 6 3 5 7) 
                                            (5 4 4 2 3) 
                                            (2 2 3 4 2) 
                                            (1 1 2 3 1) 
                                            (2 3 4 2 3))
                        :marks (s 1 2 a 3 slur 3 6 as 7)))))))
    (sc-test-check
      (probe-delete "/tmp/rsp-test.eps")
      (cmn-display rsp)
      (file-write-ok "/tmp/rsp-test.eps" 24000))))

;;; SAR Wed Jul 11 18:59:23 BST 2012
(sc-deftest test-webpage-rsp-rsm-rthm-seq-map ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-va-part.ly"
                    "slippery-chicken-piece-va.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 77000 900 900 200 520 200 560 200 520))
         (random-rs (inefficiently-permutate '(q e s (s)) :max 9 :sublists t))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))
                        (va (viola :midi-channel 2))
                        (vc (cello :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a3 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4 
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1))
                      (2 (1 1 1 1 1))
                      (3 (1 1 1 1 1 1 1)))
           :rthm-seq-palette 
           (loop for rs in random-rs
              for rs-di in '(seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9)
              collect (list rs-di
                            (list (list (list '(2 4)
                                              (first rs)
                                              (second rs)
                                              (third rs)
                                              (fourth rs)))
                                  ':pitch-seq-palette
                                  '((1 2 3)))))
           :rthm-seq-map '((1 ((vn (seq3 seq1 seq2))
                               (va (seq1 seq2 seq3))
                               (vc (seq5 seq3 seq4))))
                           (2 ((vn (seq5 seq1 seq1 seq1 seq5))
                               (va (seq7 seq4 seq2 seq3 seq2))
                               (vc (seq6 seq5 seq3 seq4 seq7))))
                           (3 ((vn (seq9 seq8 seq1 seq5 seq3 seq2 seq1))
                               (va (seq9 seq2 seq4 seq5 seq4 seq1 seq1))
                               (vc (seq9 seq3 seq7 seq5 seq1 seq3 seq1)))))))) 
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-and-clm.html

;;; SAR Thu Jul 12 11:19:35 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-snd-output-dir ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 7000 80 670 200 160))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((vn (1)))))
           :snd-output-dir "/tmp/")))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp "/tmp/" (snd-output-dir slippery-chicken))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 11:45:07 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-sndfile-palette ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 7000 80 670 200 160))
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((vn (1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((developing-group
                                ((test-sndfile-1)
                                 (test-sndfile-2)
                                 (test-sndfile-3)))
                               (percussive-group
                                ((test-sndfile-4)
                                 (test-sndfile-5)
                                 (test-sndfile-6))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 12:16:23 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-call-to-clm-1 ()
  (let* 
      ((o-files
        '("sc-object-1-player-one-player-two-source-sndfile-grp-1-seq1-1.aif"
          "_slippery-chicken-piece-score.ly"
          #+cmn "slippery-chicken-piece.eps"
          "slippery-chicken-piece.mid"
          "slippery-chicken-piece-def.ly"
          "slippery-chicken-piece-playerone-part.ly"
          "slippery-chicken-piece-playerone.ly"
          "slippery-chicken-piece-playerthree-part.ly"
          "slippery-chicken-piece-playerthree.ly"
          "slippery-chicken-piece-playertwo-part.ly"
          "slippery-chicken-piece-playertwo.ly"))
       (o-files-sizes '(1200000 190 #+cmn 15000 180 900 210 160 210
                        150 210 150)) 
       (sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "test-suite/test-sndfiles-dir-1/"))
       (sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "test-suite/test-sndfiles-dir-2/"))
       (slippery-chicken
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))
                      (player-three (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) h q e s (s)))
               :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1))
                             (player-two (1))
                             (player-three (1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (source-sndfile-grp-2
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfiles-dir-1 sndfiles-dir-2)))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 '(player-one player-two) 
                'source-sndfile-grp-1
                :srate 44100
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif"
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 12:22:09 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-call-to-clm-play-2 ()
  (let* ((o-files '("sc-object-1-player-one-player-two-player-three-source-sndfile-grp-1-seq1-1.aif"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-playerone-part.ly"
                    "slippery-chicken-piece-playerone.ly"
                    "slippery-chicken-piece-playerthree-part.ly"
                    "slippery-chicken-piece-playerthree.ly"
                    "slippery-chicken-piece-playertwo-part.ly"
                    "slippery-chicken-piece-playertwo.ly"))
         (o-files-sizes '(2400000 190 #+cmn 15000 180 900 210 160 210
                          150 210 150)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((player-one (flute :midi-channel 1))
                        (player-two (oboe :midi-channel 2))
                        (player-three (bassoon :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1)))
           :rthm-seq-palette 
           '((1 ((((4 4) h q e s (s)))
                 :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((player-one (1))
                               (player-two (1))
                               (player-three (1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1
                                ((test-sndfile-1.aiff)
                                 (test-sndfile-2.aiff)
                                 (test-sndfile-3.aiff)))
                               (source-sndfile-grp-2
                                ((test-sndfile-4.aiff)
                                 (test-sndfile-5.aiff)
                                 (test-sndfile-6.aiff))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 nil 'source-sndfile-grp-1
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif"
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 12:54:24 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-inc-strt-dur-sclr-ign-rests ()
  (let* ((o-files '("sc-object-1-player-one-source-sndfile-grp-1-seq1-3.aif"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-playerone-part.ly"
                    "slippery-chicken-piece-playerone.ly"
                    "slippery-chicken-piece-playerthree-part.ly"
                    "slippery-chicken-piece-playerthree.ly"
                    "slippery-chicken-piece-playertwo-part.ly"
                    "slippery-chicken-piece-playertwo.ly"))
         (o-files-sizes '(5100000 190 #+cmn 23000 380 900 210 220 210 
                          220 210 220)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((player-one (flute :midi-channel 1))
                        (player-two (oboe :midi-channel 2))
                        (player-three (bassoon :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((player-one (1 1 1))
                               (player-two (1 1 1))
                               (player-three (1 1 1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1
                                ((test-sndfile-1.aiff)
                                 (test-sndfile-2.aiff)
                                 (test-sndfile-3.aiff)))
                               (source-sndfile-grp-2
                                ((test-sndfile-4.aiff)
                                 (test-sndfile-5.aiff)
                                 (test-sndfile-6.aiff))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 'player-one 'source-sndfile-grp-1 
                :inc-start t
                :duration-scaler 1.3
                :ignore-rests nil
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif"
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 13:17:44 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-strt-end-dur ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "sc-object-1-player-one-source-sndfile-grp-1-seq1-3.aif"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-playerone-part.ly"
                    "slippery-chicken-piece-playerone.ly"
                    "slippery-chicken-piece-playerthree-part.ly"
                    "slippery-chicken-piece-playerthree.ly"
                    "slippery-chicken-piece-playertwo-part.ly"
                    "slippery-chicken-piece-playertwo.ly"))
         (o-files-sizes '(190 #+cmn 23000 5200000 380 900 210 220 210 
                          220 210 220)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((player-one (flute :midi-channel 1))
                        (player-two (oboe :midi-channel 2))
                        (player-three (bassoon :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((player-one (1 1 1))
                               (player-two (1 1 1))
                               (player-three (1 1 1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1 
                                ((test-sndfile-1 :start 0.000 
                                                 :end 2.100) 
                                 (test-sndfile-1 :start 0.000 
                                                 :duration 0.308) 
                                 (test-sndfile-1 :start (0 1 000))
                                 (test-sndfile-1 :end 1.308)
                                 (test-sndfile-1 :duration 1.736))))
                              ,(list sndfiles-dir-1)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 'player-one 'source-sndfile-grp-1 
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif"
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 13:52:09 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-base-frequency ()
  (let* ((o-files '("sc-object-1-flt-percussive-models-group-seq1-3.aif"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-clr-part.ly"
                    "slippery-chicken-piece-clr.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-flt-part.ly"
                    "slippery-chicken-piece-flt.ly"
                    "slippery-chicken-piece-obo-part.ly"
                    "slippery-chicken-piece-obo.ly"))
         (o-files-sizes '(5200000 190 #+cmn 23000 380 200 220
                          900 200 220 200 220)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((flt (flute :midi-channel 1))
                        (obo (oboe :midi-channel 2))
                        (clr (bassoon :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((flt (1 1 1))
                               (obo (1 1 1))
                               (clr (1 1 1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((percussive-models-group
                                ((test-sndfile-4 :start 0.000 :end 2.100) 
                                 (test-sndfile-4 :start 0.000 
                                                 :duration 0.308 
                                                 :frequency 860) 
                                 (test-sndfile-4 :start (0 1 000)
                                                 :frequency a5)
                                 (test-sndfile-4 :end 1.308
                                                 :frequency a7)
                                 (test-sndfile-4 :duration 1.736
                                                 :frequency b6))))
                              ,(list sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 'flt 'percussive-models-group
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif"
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 13:52:18 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-pitch-synchronous ()
  (let* ((o-files '("sc-object-1-flt-source-sndfile-grp-1-seq1-3-psync.aif"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-clr-part.ly"
                    "slippery-chicken-piece-clr.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-flt-part.ly"
                    "slippery-chicken-piece-flt.ly"
                    "slippery-chicken-piece-obo-part.ly"
                    "slippery-chicken-piece-obo.ly"))
         (o-files-sizes '(5200000 190 #+cmn 23000 380 200 220 900 200 
                          220 200 220)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((flt (flute :midi-channel 1))
                        (obo (oboe :midi-channel 2))
                        (clr (bassoon :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((flt (1 1 1))
                               (obo (1 1 1))
                               (clr (1 1 1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1
                                ((test-sndfile-4 :start 0.000 :end 2.100) 
                                 (test-sndfile-4 :start 0.000 
                                                 :duration 0.308 
                                                 :frequency 860) 
                                 (test-sndfile-4 :start (0 1 000)
                                                 :frequency a5)
                                 (test-sndfile-4 :end 1.308
                                                 :frequency a7)
                                 (test-sndfile-4 :duration 1.736
                                                 :frequency b6))))
                              ,(list sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 'flt 'source-sndfile-grp-1
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif"
                :pitch-synchronous t
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 14:08:20 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-sound-file-groups ()
  (let* ((o-files '("sc-object-1-player-three-source-sndfile-grp-3-seq1-3.aif"
                    "sc-object-1-player-one-player-two-source-sndfile-grp-1-seq1-3.aif"
                    "sc-object-1-player-one-player-two-source-sndfile-grp-2-seq1-3.aif"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-playerone-part.ly"
                    "slippery-chicken-piece-playerone.ly"
                    "slippery-chicken-piece-playerthree-part.ly"
                    "slippery-chicken-piece-playerthree.ly"
                    "slippery-chicken-piece-playertwo-part.ly"
                    "slippery-chicken-piece-playertwo.ly"))
         (o-files-sizes '(5200000 5200000 5300000 
                          190 #+cmn 23000 380 900 210 220 210 220 210 220)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((player-one (flute :midi-channel 1))
                        (player-two (oboe :midi-channel 2))
                        (player-three (bassoon :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4))))
                               (2 ((((4 4) q e s (s) h))
                                   :pitch-seq-palette ((1 2 3 4))))
                               (3 ((((4 4) e s (s) h q))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((player-one (1 2 3))
                               (player-two (2 3 1))
                               (player-three (3 1 2)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1
                                ((test-sndfile-1)
                                 (test-sndfile-2)))
                               (source-sndfile-grp-2
                                ((test-sndfile-3)
                                 (test-sndfile-4)))
                               (source-sndfile-grp-3
                                ((test-sndfile-5)
                                 (test-sndfile-6))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 '(player-one player-two) 
                'source-sndfile-grp-1
                :duration-scaler 0.7
                :inc-start nil
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      #+clm (clm-play +sc-object+ 1 '(player-one player-two) 
                'source-sndfile-grp-2
                :duration-scaler 1.3
                :inc-start t
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      #+clm (clm-play +sc-object+ 1 '(player-three) 
                'source-sndfile-grp-3
                :ignore-rests nil
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 14:15:35 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-fib-trans ()
  (let* ((o-files
          '("sc-object-1-player-one-player-two-source-sndfile-grp-1-to-source-sndfile-grp-2-seq1-3.aif"
            "_slippery-chicken-piece-score.ly"
            #+cmn "slippery-chicken-piece.eps"
            "slippery-chicken-piece.mid"
            "slippery-chicken-piece-def.ly"
            "slippery-chicken-piece-playerone-part.ly"
            "slippery-chicken-piece-playerone.ly"
            "slippery-chicken-piece-playerthree-part.ly"
            "slippery-chicken-piece-playerthree.ly"
            "slippery-chicken-piece-playertwo-part.ly"
            "slippery-chicken-piece-playertwo.ly"))
         (o-files-sizes '(5200000 190 #+cmn 23000 380 900 210 220 210 
                          220 210 220))
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((player-one (flute :midi-channel 1))
                        (player-two (oboe :midi-channel 2))
                        (player-three (bassoon :midi-channel 3))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4))))
                               (2 ((((4 4) q e s (s) h))
                                   :pitch-seq-palette ((1 2 3 4))))
                               (3 ((((4 4) e s (s) h q))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((player-one (1 2 3))
                               (player-two (2 3 1))
                               (player-three (3 1 2)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1
                                ((test-sndfile-1)
                                 (test-sndfile-2)))
                               (source-sndfile-grp-2
                                ((test-sndfile-3)
                                 (test-sndfile-4))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 '(player-one player-two) 
                'source-sndfile-grp-1  
                :sound-file-palette-ref2 'source-sndfile-grp-2
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 14:44:22 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-amplitude-description ()
  (let* ((o-files '("sc-object-1-fl-percussive-models-group-seq1-3.aif"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"
                    "slippery-chicken-piece-ob-part.ly"
                    "slippery-chicken-piece-ob.ly"))
         (o-files-sizes '(5200000 190 #+cmn 16000 260 900 200 220 
                          200 230)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((fl (1 1 1))
                               (ob (1 1 1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((percussive-models-group
                                ((test-sndfile-4 
                                  :description "initial attack"
                                  :start 0.000 :end 2.100
                                  :frequency 860
                                  :amplitude 0.1)
                                 (test-sndfile-4
                                  :description "snap"
                                  :start 0.000 :duration 0.308 
                                  :frequency a5
                                  :amplitude 1.0))))
                              ,(list sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 'fl 'percussive-models-group
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 17:46:13 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-useful-clm-play-args ()
  (let* ((o-files '("sc-object-3-fl-ob-source-sndfile-grp-2-seq1-3.aif"
                    "sc-object-3-fl-ob-source-sndfile-grp-1-seq1-3.aif"
                    "sc-object-2-fl-ob-source-sndfile-grp-2-seq1-3.aif"
                    "sc-object-2-fl-ob-source-sndfile-grp-1-seq1-3.aif"
                    "sc-object-1-fl-ob-source-sndfile-grp-2-seq2-3.aif"
                    "sc-object-1-fl-ob-source-sndfile-grp-1-seq1-3.aif"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"
                    "slippery-chicken-piece-ob-part.ly"
                    "slippery-chicken-piece-ob.ly"))
         (o-files-sizes '(5200000 5200000 15400000
                          9400000 3800000 9400000 190 
                          #+cmn 35000 670 900 200 420 200 430)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1))
                      (2 (1 1 1))
                      (3 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((fl (1 1 1))
                               (ob (1 1 1))))
                           (2 ((fl (1 1 1))
                               (ob (1 1 1))))
                           (3 ((fl (1 1 1))
                               (ob (1 1 1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1 
                                ((test-sndfile-1)
                                 (test-sndfile-2)
                                 (test-sndfile-3)))
                               (source-sndfile-grp-2
                                ((test-sndfile-4)
                                 (test-sndfile-5)
                                 (test-sndfile-6))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 nil 'source-sndfile-grp-1 
                :num-sections 2
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      #+clm (clm-play +sc-object+ 1 nil 'source-sndfile-grp-2 
                :num-sections 1
                :from-sequence 2
                :num-sequences 2
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      #+clm (clm-play +sc-object+ 2 nil 'source-sndfile-grp-1
                :reset-snds-each-rs nil
                :reset-snds-each-player nil
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      #+clm (clm-play +sc-object+ 2 nil 'source-sndfile-grp-2
                :time-scaler 1.7
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      #+clm (clm-play +sc-object+ 3 nil 'source-sndfile-grp-1 
                :src-scaler 1.9
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      #+clm (clm-play +sc-object+ 3 nil 'source-sndfile-grp-2
                :rev-amt 0.1
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif"
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 17:57:07 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-args-for-output-format ()
  (let* ((o-files '("sc-object-1-fl-ob-source-sndfile-grp-1-seq1-3.aiff"
                    "_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"
                    "slippery-chicken-piece-ob-part.ly"
                    "slippery-chicken-piece-ob.ly"))
         (o-files-sizes '(10500000 190 #+cmn 16000 260 900 200 220 
                          200 220)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((fl (1 1 1))
                               (ob (1 1 1)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1 
                                ((test-sndfile-1)
                                 (test-sndfile-2)
                                 (test-sndfile-3)))
                               (source-sndfile-grp-2
                                ((test-sndfile-4)
                                 (test-sndfile-5)
                                 (test-sndfile-6))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      #+clm (clm-play +sc-object+ 1 nil 'source-sndfile-grp-1 
                :channels 8
                :srate 44100
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aiff"
                :src-width 5
                :check-overwrite nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 18:14:16 BST 2012
#+clm
(sc-deftest test-webpage-sc-clm-tape-part-only ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "output.eps"
                    "output.mid"
                    "sc-object-1-cp-source-sndfile-grp-1-seq1-6.aif"
                    "slippery-chicken-piece-cl-part.ly"
                    "slippery-chicken-piece-cl.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"
                    "slippery-chicken-piece-ob-part.ly"
                    "slippery-chicken-piece-ob.ly"))
         (o-files-sizes '(190 #+cmn 38000 670 9400000 200 350 900 200 320 200 
                          330)) 
         (sndfiles-dir-1 
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-1/"))
         (sndfiles-dir-2
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+ 
                       "test-suite/test-sndfiles-dir-2/"))
         (slippery-chicken
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))
                        (cl (b-flat-clarinet :midi-channel 3))
                        (cp (computer))))
           :set-limits-high '((cp (0 c6 100 c6)))
           :set-limits-low '((cp (0 f3 100 f3)))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                  c5 d5 e5 f5 g5 a5 b5 c6))))
           :set-map '((1 (1 1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                   :pitch-seq-palette ((1 2 3 4))))
                               (2 ((((4 4) q e s (s) h))
                                   :pitch-seq-palette ((1 2 3 4))))
                               (3 ((((4 4) e s (s) h q))
                                   :pitch-seq-palette ((1 2 3 4)))))
           :rthm-seq-map '((1 ((fl (1 2 3 1 3 2))
                               (ob (2 3 1 3 2 1))
                               (cl (3 1 3 2 1 2))
                               (cp (1 3 2 1 2 3)))))
           :snd-output-dir "/tmp/"
           :sndfile-palette `(((source-sndfile-grp-1 
                                ((test-sndfile-1)
                                 (test-sndfile-2)
                                 (test-sndfile-3)))
                               (source-sndfile-grp-2
                                ((test-sndfile-4)
                                 (test-sndfile-5)
                                 (test-sndfile-6))))
                              ,(list sndfiles-dir-1 sndfiles-dir-2)
                              ("aiff")))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      #+clm (clm-play +sc-object+ 1 'cp 'source-sndfile-grp-1
                :srate 441000
                :header-type clm::mus-aiff
                :data-format clm::mus-bshort
                :sndfile-extension ".aif" 
                :src-width 5
                :check-overwrite nil)
      (midi-play +sc-object+ :voices '(fl ob cl) 
                 :midi-file "/tmp/output.mid")
      #+cmn (cmn-display +sc-object+ :players '(fl ob cl) 
                         :file "/tmp/output.eps")
      (write-lp-data-for-all +sc-object+ :players '(fl ob cl) 
                             :base-path "/tmp/") 
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scores.html

;;; SAR Thu Jul 12 18:32:41 BST 2012
(sc-deftest test-webpage-scores-header ()
  (let* ((o-files '("_a-slippery-chicken-piece-score.ly"
                    "a-slippery-chicken-piece-def.ly"
                    "a-slippery-chicken-piece-vn-part.ly"
                    "a-slippery-chicken-piece-vn.ly"
                    #+cmn "a-slippery-chicken-piece.eps"
                    "a-slippery-chicken-piece.mid"))
         (o-files-sizes '(200 670 210 410 #+cmn 17000 480)) 
         (slippery-chicken
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 18:41:08 BST 2012
(sc-deftest test-webpage-scores-starting-key-sig ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 11000 260 280 200 290))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((ef4 f4 g4 af4 bf4 c5 d5 ef5))))
           :key-sig '(ef major)
           :avoid-melodic-octaves nil
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 18:55:54 BST 2012
(sc-deftest test-webpage-scores-key-changes ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 12000 260 670 200 350))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vn (violin :midi-channel 1))))
           :key-sig '(ef major)
           :avoid-melodic-octaves nil
           :set-palette '((1 ((ef4 f4 g4 af4 bf4 c5 d5 ef5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
           :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (every #'not
             (loop for ne = (next-event slippery-chicken 'vn)
                while ne
                collect (marks-before ne)
                collect (marks ne)))
      (add-mark-before-note slippery-chicken 2 1 'vn '(key af major))
      (add-mark-before-note slippery-chicken 3 1 'vn '(key a major))
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         ((KEY AF MAJOR)) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL ((KEY A MAJOR)) NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL))
      #+cmn (cmn-display slippery-chicken)
      (add-mark-to-note slippery-chicken 2 8 'vn '(key a major))
      (add-mark-to-note slippery-chicken 1 8 'vn '(key af major))
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         ((KEY AF MAJOR)) ((KEY AF MAJOR)) NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL ((KEY A MAJOR)) ((KEY A MAJOR))
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (write-lp-data-for-all slippery-chicken)
      (midi-play slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 21:09:10 BST 2012
(sc-deftest test-webpage-scores-score-order ()
  (let* ((o-files '("_a-slippery-chicken-piece-score.ly"
                    "a-slippery-chicken-piece-def.ly"
                    "a-slippery-chicken-piece-fl-part.ly"
                    "a-slippery-chicken-piece-fl.ly"
                    "a-slippery-chicken-piece-hn-part.ly"
                    "a-slippery-chicken-piece-hn.ly"
                    "a-slippery-chicken-piece-ob-part.ly"
                    "a-slippery-chicken-piece-ob.ly"
                    "a-slippery-chicken-piece-tp-part.ly"
                    "a-slippery-chicken-piece-tp.ly"
                    "a-slippery-chicken-piece-va-part.ly"
                    "a-slippery-chicken-piece-va.ly"
                    "a-slippery-chicken-piece-vc-part.ly"
                    "a-slippery-chicken-piece-vc.ly"
                    "a-slippery-chicken-piece-vn-part.ly"
                    "a-slippery-chicken-piece-vn.ly"
                    #+cmn "a-slippery-chicken-piece.eps"
                    "a-slippery-chicken-piece.mid"))
         (o-files-sizes '(200 2700 210 350 210 310 210 310 210 340 210 280 210 
                          310 210 350 #+cmn 78000 1900))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (midi-play slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 21:15:21 BST 2012
(sc-deftest test-webpage-scores-staff-groupings ()
  (let* ((o-files '("_a-slippery-chicken-piece-score.ly"
                    "a-slippery-chicken-piece-def.ly"
                    "a-slippery-chicken-piece-fl-part.ly"
                    "a-slippery-chicken-piece-fl.ly"
                    "a-slippery-chicken-piece-hn-part.ly"
                    "a-slippery-chicken-piece-hn.ly"
                    "a-slippery-chicken-piece-ob-part.ly"
                    "a-slippery-chicken-piece-ob.ly"
                    "a-slippery-chicken-piece-tp-part.ly"
                    "a-slippery-chicken-piece-tp.ly"
                    "a-slippery-chicken-piece-va-part.ly"
                    "a-slippery-chicken-piece-va.ly"
                    "a-slippery-chicken-piece-vc-part.ly"
                    "a-slippery-chicken-piece-vc.ly"
                    "a-slippery-chicken-piece-vn-part.ly"
                    "a-slippery-chicken-piece-vn.ly"
                    #+cmn "a-slippery-chicken-piece.eps"
                    "a-slippery-chicken-piece.mid"))
         (o-files-sizes '(200 2700 210 350 210 310 210 310 210 340 210 280 210 
                          310 210 350 #+cmn 82000 1900))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (midi-play slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 21:25:21 BST 2012
#+cmn
(sc-deftest test-webpage-scores-cmn-bars-per-system ()
  (let* ((o-files '("a-slippery-chicken-piece.eps"
                    "a-slippery-chicken-piece.mid"))
         (o-files-sizes '(50000 900))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (cmn-display slippery-chicken :auto-bar-nums 5)
      (midi-play slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Jul 12 21:44:36 BST 2012
(sc-deftest test-webpage-scores-bar-line-types ()
  (let* ((o-files '("_bar-lines-piece-score.ly"
                    "bar-lines-piece-def.ly"
                    "bar-lines-piece-fl-part.ly"
                    "bar-lines-piece-fl.ly"
                    #+cmn "bar-lines-piece.eps"
                    "bar-lines-piece.mid"))
         (o-files-sizes '(180 660 190 270 #+cmb 10000 260))
         (bar-lines-piece
          (make-slippery-chicken
           '+bar-lines-piece+
           :title "bar-lines piece"
           :ensemble '(((fl (flute :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -)))))
           :rthm-seq-map '((1 ((fl (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (change-bar-line-type bar-lines-piece 1 1)
      (change-bar-line-type bar-lines-piece 3 5)
      (midi-play bar-lines-piece)
      #+cmn (cmn-display bar-lines-piece)
      (write-lp-data-for-all bar-lines-piece :base-path "/tmp/")
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 11:00:22 BST 2012
(sc-deftest test-webpage-scores-rehearsal-letters-keyword ()
  (let* ((o-files '("_a-slippery-chicken-piece-score.ly"
                    "a-slippery-chicken-piece-def.ly"
                    "a-slippery-chicken-piece-fl-part.ly"
                    "a-slippery-chicken-piece-fl.ly"
                    #+cmn "a-slippery-chicken-piece.eps"
                    "a-slippery-chicken-piece.mid"))
         (o-files-sizes '(200 670 210 960 #+cmn 39000 910))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (equalp
       (loop for b from 1 to (num-bars slippery-chicken)
          collect (rehearsal-letter (get-bar slippery-chicken b 'fl)))
       '(NIL "A" NIL NIL "B" NIL NIL NIL "C" NIL NIL NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)    
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 11:11:10 BST 2012
(sc-deftest test-webpage-scores-rehearsal-letters-post-gen ()
  (let* ((o-files '("_a-slippery-chicken-piece-score.ly"
                    "a-slippery-chicken-piece-def.ly"
                    "a-slippery-chicken-piece-fl-part.ly"
                    "a-slippery-chicken-piece-fl.ly"
                    #+cmn "a-slippery-chicken-piece.eps"
                    "a-slippery-chicken-piece.mid"))
         (o-files-sizes '(200 670 210 870 #+cmn 39000 910))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (set-rehearsal-letter slippery-chicken 3 'A)
      (equalp
       (loop for b from 1 to (num-bars slippery-chicken)
          collect (rehearsal-letter (get-bar slippery-chicken b 'fl)))
       '(NIL A NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)    
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 11:22:15 BST 2012
(sc-deftest test-webpage-scores-rehearsal-letters-all-parts ()
  (let* ((o-files '("_a-slippery-chicken-piece-score.ly"
                    "a-slippery-chicken-piece-def.ly"
                    "a-slippery-chicken-piece-fl-part.ly"
                    "a-slippery-chicken-piece-fl.ly"
                    "a-slippery-chicken-piece-hn-part.ly"
                    "a-slippery-chicken-piece-hn.ly"
                    "a-slippery-chicken-piece-ob-part.ly"
                    "a-slippery-chicken-piece-ob.ly"
                    "a-slippery-chicken-piece-tp-part.ly"
                    "a-slippery-chicken-piece-tp.ly"
                    "a-slippery-chicken-piece-vc-part.ly"
                    "a-slippery-chicken-piece-vc.ly"
                    "a-slippery-chicken-piece-vn-part.ly"
                    "a-slippery-chicken-piece-vn.ly"
                    #+cmn "a-slippery-chicken-piece.eps"
                    "a-slippery-chicken-piece.mid"))
         (o-files-sizes '(200 1900 210 840 210 750 210 820 210 750 210 950 
                          210 870 #+cmn 183000 4800))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken :rehearsal-letters-all-players t)
      (write-lp-data-for-all slippery-chicken :rehearsal-letters-all-players t)    
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 11:35:24 BST 2012
(sc-deftest test-webpage-scores-auto-clefs-nil ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"))
         (o-files-sizes '(190 #+cmn 11000 260 670 200 260))
         (slippery-chicken
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vc nil 1))
      (every #'not
             (loop for ne = (next-event slippery-chicken 'vc)
                while ne
                collect (marks-before ne)
                collect (marks ne)))
      (midi-play +sc-object+)
      #+cmn (cmn-display +sc-object+ 
                         :file "/tmp/slippery-chicken.eps" 
                         :auto-clefs nil)
      (write-lp-data-for-all +sc-object+ 
                             :base-path "/tmp/" 
                             :auto-clefs nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 11:45:21 BST 2012
(sc-deftest test-webpage-scores-auto-clefs-as-post-gen-1 ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"))
         (o-files-sizes '(190 #+cmn 12000 260 670 200 280))
         (slippery-chicken
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vc nil 1))
      (every #'not
             (loop for ne = (next-event slippery-chicken 'vc)
                while ne
                collect (marks-before ne)
                collect (marks ne)))
      (auto-clefs +sc-object+)
      (not (next-event slippery-chicken 'vc nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vc)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL ((CLEF TENOR)) NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (not (delete-clefs +sc-object+ 'vc 1 5))
      (add-clef +sc-object+ 'vc 2 2 'tenor)
      (add-clef +sc-object+ 'vc 3 3 'treble)
      (not (next-event slippery-chicken 'vc nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vc)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         ((CLEF TENOR)) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL ((CLEF TREBLE)) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL))
      (midi-play +sc-object+)
      #+cmn (cmn-display +sc-object+ :file "/tmp/slippery-chicken.eps"
                         :auto-clefs nil)
      (write-lp-data-for-all +sc-object+ :base-path "/tmp/" :auto-clefs nil) 
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 11:53:41 BST 2012
(sc-deftest test-webpage-scores-add-clef ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"))
         (o-files-sizes '(190 #+cmn 12000 260 670 200 280))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vc (cello :midi-channel 1))))
           :tempo-map '((1 (q 72)))
           :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette (1 2 3 4 5 6 7 8))))
           :avoid-melodic-octaves nil
           :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vc nil 1))
      (every #'not
             (loop for ne = (next-event slippery-chicken 'vc)
                while ne
                collect (marks-before ne)
                collect (marks ne)))
      (add-clef slippery-chicken 'vc 2 2 'tenor)
      (add-clef slippery-chicken 'vc 3 3 'treble)
      (not (next-event slippery-chicken 'vc nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vc)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL ((CLEF TENOR)) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL))
      (midi-play +sc-object+)
      #+cmn (cmn-display slippery-chicken :file "/tmp/slippery-chicken.eps"
                         :auto-clefs nil)
      (write-lp-data-for-all slippery-chicken :base-path "/tmp/"
                             :auto-clefs nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 11:58:44 BST 2012
(sc-deftest test-webpage-scores-delete-clefs ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vc-part.ly"
                    "slippery-chicken-piece-vc.ly"))
         (o-files-sizes '(190 #+cmn 11000 260 670 200 260))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
           :ensemble '(((vc (cello :midi-channel 1))))
           :tempo-map '((1 (q 72)))
           :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                   :pitch-seq-palette (1 2 3 4 5 6 7 8))))
           :avoid-melodic-octaves nil
           :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vc nil 1))
      (every #'not
             (loop for ne = (next-event slippery-chicken 'vc)
                while ne
                collect (marks-before ne)
                collect (marks ne)))
      (add-clef slippery-chicken 'vc 2 2 'tenor)
      (add-clef slippery-chicken 'vc 3 3 'treble)
      (not (next-event slippery-chicken 'vc nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vc)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL ((CLEF TENOR)) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL))
      (not (delete-clefs slippery-chicken 'vc 2 2))
      (not (delete-clefs slippery-chicken 'vc 3 3))
      (not (next-event slippery-chicken 'vc nil 1))
      (every #'not
             (loop for ne = (next-event slippery-chicken 'vc)
                while ne
                collect (marks-before ne)
                collect (marks ne)))
      (midi-play +sc-object+)
      #+cmn (cmn-display slippery-chicken :file "/tmp/slippery-chicken.eps"
                         :auto-clefs nil)
      (write-lp-data-for-all slippery-chicken :base-path "/tmp/"
                             :auto-clefs nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 12:13:19 BST 2012
(sc-deftest test-webpage-scores-in-c-nil ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-cl-part.ly"
                    "slippery-chicken-piece-cl.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"))
         (o-files-sizes '(190 #+cmn 28000 610 200 350 900 200 370))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'fl nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'fl)
          while ne
          collect (get-pitch-symbol ne)
          when (written-pitch-or-chord ne)
          collect (written-pitch-or-chord ne))
       '(A5 B5 C6 D6 E6 E6 F6 G6 A5 B5 C6 D6 E6 E6 F6 G6 A5 B5 C6 D6 E6 E6 F6
         G6 A5 B5 C6 D6 E6 E6 F6 G6))
      (not (next-event slippery-chicken 'cl nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'cl)
          while ne
          collect (get-pitch-symbol ne)
          when (written-pitch-or-chord ne)
          collect (data (written-pitch-or-chord ne)))
       '(CS5 CS5 D5 D5 E5 E5 E5 E5 FS5 FS5 G5 G5 G5 G5 A5 A5 CS5 CS5 D5 D5 E5
         E5 E5 E5 FS5 FS5 G5 G5 G5 G5 A5 A5 CS5 CS5 D5 D5 E5 E5 E5 E5 FS5 FS5
         G5 G5 G5 G5 A5 A5 CS5 CS5 D5 D5 E5 E5 E5 E5 FS5 FS5 G5 G5 G5 G5 A5
         A5))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken :in-c nil)
      (write-lp-data-for-all slippery-chicken :in-c nil)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 12:17:04 BST 2012
(sc-deftest test-webpage-scores-in-c-t ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-cl-part.ly"
                    "slippery-chicken-piece-cl-written.ly"
                    "slippery-chicken-piece-cl.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-fl-part.ly"
                    "slippery-chicken-piece-fl.ly"))
         (o-files-sizes '(190 #+cmn 26000 610 210 350 340 900 200 370))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'fl nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'fl)
          while ne
          collect (get-pitch-symbol ne)
          when (written-pitch-or-chord ne)
          collect (written-pitch-or-chord ne))
       '(A5 B5 C6 D6 E6 E6 F6 G6 A5 B5 C6 D6 E6 E6 F6 G6 A5 B5 C6 D6 E6 E6 F6
         G6 A5 B5 C6 D6 E6 E6 F6 G6))
      (not (next-event slippery-chicken 'cl nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'cl)
          while ne
          collect (get-pitch-symbol ne)
          when (written-pitch-or-chord ne)
          collect (data (written-pitch-or-chord ne)))
       '(CS5 CS5 D5 D5 E5 E5 E5 E5 FS5 FS5 G5 G5 G5 G5 A5 A5 CS5 CS5 D5 D5 E5
         E5 E5 E5 FS5 FS5 G5 G5 G5 G5 A5 A5 CS5 CS5 D5 D5 E5 E5 E5 E5 FS5 FS5
         G5 G5 G5 G5 A5 A5 CS5 CS5 D5 D5 E5 E5 E5 E5 FS5 FS5 G5 G5 G5 G5 A5
         A5))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken :in-c t)
      (write-lp-data-for-all slippery-chicken :in-c t)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 12:54:44 BST 2012
(sc-deftest test-webpage-scores-parts ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken.eps"
                    "slippery-chicken-piece-clr-part.ly"
                    "slippery-chicken-piece-clr.ly"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-flt-part.ly"
                    "slippery-chicken-piece-flt.ly"
                    "slippery-chicken-piece-hrn-part.ly"
                    "slippery-chicken-piece-hrn.ly"
                    "slippery-chicken-piece-tba-part.ly"
                    "slippery-chicken-piece-tba.ly"
                    "slippery-chicken-piece-tbn-part.ly"
                    "slippery-chicken-piece-tbn.ly"
                    "slippery-chicken-piece-tpt-part.ly"
                    "slippery-chicken-piece-tpt.ly"
                    "slippery-chicken-piece-vla-part.ly"
                    "slippery-chicken-piece-vla.ly"
                    "slippery-chicken-piece-vlc-part.ly"
                    "slippery-chicken-piece-vlc.ly"
                    "slippery-chicken-piece-vln-part.ly"
                    "slippery-chicken-piece-vln.ly"))
         (o-files-sizes '(190 #+cmn 10000 200 270 3300 200 270 200 250 200  
                          280 200 330 200 310 200 260 200 300 200 310))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      #+cmn (cmn-display slippery-chicken :file "/tmp/slippery-chicken.eps"
                         :players '(tbn))
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 12:59:21 BST 2012
(sc-deftest test-webpage-scores-sectional-scores ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-hrn-part.ly"
                    "slippery-chicken-piece-hrn.ly"
                    "slippery-chicken-piece-tba-part.ly"
                    "slippery-chicken-piece-tba.ly"
                    "slippery-chicken-piece-tbn-part.ly"
                    "slippery-chicken-piece-tbn.ly"
                    "slippery-chicken-piece-tpt-part.ly"
                    "slippery-chicken-piece-tpt.ly"))
         (o-files-sizes '(190 #+cmn 43000 1900 1700 200 250 200 280
                          200 330 200 310))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken :file "/tmp/slippery-chicken.eps" 
                         :players '(hrn tpt tbn tba))
      (write-lp-data-for-all slippery-chicken :base-path "/tmp/" 
                             :players '(hrn tpt tbn tba))
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tempo.html

;;; SAR Thu Mar 15 19:22:37 GMT 2012
;;; SAR Fri Jul 13 13:21:15 BST 2012: Replaced with better code that
;;; tests the same thing plus file-write sizes
(sc-deftest test-webpage-tempo-tempo-map ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 156000 2800 670 200 2900))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          when (tempo-change ne)
          collect (list (bar-num ne)
                        (beat (tempo-change ne))
                        (bpm (tempo-change ne))
                        (description (tempo-change ne))))
       '((1 Q 60 NIL) (15 E 72 NIL) (84 Q. 176 "prestissimo")))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Fri Jul 13 13:28:47 BST 2012
(sc-deftest test-webpage-tempo-tempo-map-refs ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 29000 480 670 200 650))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          when (tempo-change ne)
          collect (list (bar-num ne)
                        (beat (tempo-change ne))
                        (bpm (tempo-change ne))
                        (description (tempo-change ne))))
       '((1 Q 60 NIL) (6 E 72 NIL) (13 Q. 176 "prestissimo")))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Thu Mar 15 19:41:31 GMT 2012
;;; SAR Fri Jul 13 13:38:23 BST 2012: Better code for same test, plus addition
;;; of file-write sizes

(sc-deftest test-webpage-tempo-tempo-curve ()
  (let* ((o-files '("_slippery-chicken-piece-score.ly"
                    #+cmn "slippery-chicken-piece.eps"
                    "slippery-chicken-piece.mid"
                    "slippery-chicken-piece-def.ly"
                    "slippery-chicken-piece-vn-part.ly"
                    "slippery-chicken-piece-vn.ly"))
         (o-files-sizes '(190 #+cmn 165000 2900 670 200 2900))
         (slippery-chicken
          (make-slippery-chicken
           '+slippery-chicken+
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
    (probe-delete-multi "/tmp/" o-files)
    (sc-test-check
      (not (next-event slippery-chicken 'vn nil 1))
      (equalp
       (loop for ne = (next-event slippery-chicken 'vn)
          while ne
          when (tempo-change ne)
          collect (list (bar-num ne)
                        (beat (tempo-change ne))
                        (bpm (tempo-change ne))
                        (description (tempo-change ne))))
       '((1 Q 60.0 NIL) (10 Q 88.0 NIL) (20 Q 116.0 NIL) (30 Q 144.0 NIL)
         (40 Q 123.55556 NIL) (50 Q 103.111115 NIL) (60 Q 82.666664 NIL)
         (70 Q 62.22222 NIL) (80 Q 65.6 NIL) (90 Q 92.8 NIL) 
         (100 Q 120.0 NIL)))
      (midi-play slippery-chicken)
      #+cmn (cmn-display slippery-chicken)
      (write-lp-data-for-all slippery-chicken)
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l-systems.html

;;; SAR Fri Aug  3 13:43:12 BST 2012
(sc-deftest test-webpages-l-systems-make-l-for-lookup ()
  (let ((lfl
         (make-l-for-lookup 'l-sys 
                            '((1 ((a))) 
                              (2 ((b)))) 
                            '((1 (1 2)) 
                              (2 (1))))))
    (sc-test-check
      (equalp
       (loop repeat 2
          for i from 1
          collect (get-data-data i (rules lfl)))
       '((1 2) (1)))
      (equalp
       (loop for nob in (data lfl)
          collect (loop for cscl in (data nob)
                     collect (data cscl)))
       '(((A)) ((B)))))))

;;; SAR Fri Aug  3 13:48:26 BST 2012
(sc-deftest test-webpages-l-systems-do-simple-lookup ()
  (let* ((lfl (make-l-for-lookup 'l-sys
                                 '((1 ((a)))
                                   (2 ((b))))
                                 '((1 (1 2)) (2 (1))))))
    (sc-test-check
      (equalp
       (do-simple-lookup lfl 1 29)
       '((A) (B) (A) (A) (B) (A) (B) (A) (A) (B) (A) (A) (B) (A) (B) (A) (A) 
         (B) (A) (B) (A) (A) (B) (A) (A) (B) (A) (B) (A))))))

;;; SAR Fri Aug  3 13:50:43 BST 2012
(sc-deftest test-webpages-l-systems-flatten-do-simple-lookup ()
  (let* ((lfl (make-l-for-lookup 'l-sys
                                 '((1 ((a)))
                                   (2 ((b))))
                                 '((1 (1 2)) (2 (1))))))
    (sc-test-check
      (equalp
       (flatten (do-simple-lookup lfl 1 29))
       '(A B A A B A B A A B A A B A B A A B A B A A B A A B A B A)))))

;;; SAR Fri Aug  3 13:52:41 BST 2012
(sc-deftest test-webpages-l-systems-get-l-sequence ()
  (let* ((lfl (make-l-for-lookup 'l-sys
                                 '((1 ((a)))
                                   (2 ((b))))
                                 '((1 (1 2)) (2 (1))))))
    (sc-test-check
      (equalp
       (get-l-sequence lfl 1 29)
       '(1 2 1 1 2 1 2 1 1 2 1 1 2 1 2 1 1 2 1 2 1 1 2 1 1 2 1 2 1)))))

;;; SAR Fri Aug  3 13:54:55 BST 2012
(sc-deftest test-webpages-l-systems-get-l-sequence-nil ()
  (let* ((lfl (make-l-for-lookup 'l-sys
                                 nil
                                 '((1 (1 2)) (2 (1))))))
    (sc-test-check
      (equalp
       (get-l-sequence lfl 1 29)
       '(1 2 1 1 2 1 2 1 1 2 1 1 2 1 2 1 1 2 1 2 1 1 2 1 1 2 1 2 1)))))

;;; SAR Fri Aug  3 13:56:47 BST 2012
(sc-deftest test-webpages-l-systems-do-lookup ()
  (let* ((lfl (make-l-for-lookup 'l-sys-a
                                 '((1 ((a) (c)))
                                   (2 ((b))))
                                 '((1 (1 2)) (2 (1))))))
    (sc-test-check
      (equalp
       (do-lookup lfl 1 73) '(A B A A B A B A A B A C B A B A A B A B C A B A A
                              B A B C A B A C B A B A C B A B C A B C C B A B C
                              A B C B A C B C A B C B C A B C C B C B C C
                              B)))))

;;; SAR Fri Aug  3 13:58:54 BST 2012
(sc-deftest test-webpages-l-systems-get-linear-sequence ()
  (let* ((lfl (make-l-for-lookup 'lfl-test
                                 nil
                                 '((1 (2 3))
                                   (2 (3 1 2))
                                   (3 (1))))))
    (sc-test-check
      (equalp
       (get-linear-sequence lfl 1 23)
       '(1 2 3 1 3 1 2 1 3 1 2 2 3 1 3 1 2 1 3 1 2 2 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (sc-test-test-all)
    (progn 
      (setf *sc-test-webpage-examples-tests-state*
            "- ALL WEBPAGE EXAMPLE TESTS PASSED.")
      (format t "~%~%~a~%~%" *sc-test-webpage-examples-tests-state*))
    (progn
      (setf *sc-test-webpage-examples-tests-state*
            "- WEBPAGE EXAMPLE TESTS FAILED.")
      (error "WEBPAGE EXAMPLE TESTS FAILED.")))

(in-scale :quarter-tone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sc-test-webpages.lsp
