;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             output-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany output.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 10:59:41 Mon Jun 10 2013 +0100
;;;
;;; SVN ID: $Id: output-examples.lsp 3670 2013-06-10 09:53:11Z medward2 $
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
;;; output.html examples
;;; ============================================================================

;;; simple midi-play example
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini :midi-file "/tmp/midi-output.mid"))

;;; simple cmn-display example
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (cmn-display mini :file "/tmp/cmn-output.mid"))

;;; including sets in cmn score output
;;; ----------------------------------------------------------------------------
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
  (cmn-display sc-piece :file "/tmp/sc-piece.eps" :display-sets t))
 
;;; cmn output of set-palette object only
;;; ----------------------------------------------------------------------------
(let* ((sp (make-set-palette 
            'sp-data
            '((set1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)))
              (set2 ((c3 fs3 cs4 e4 g4 a4 b4 c5 df5 f5 g5 af5 ef6)))
              (set3 ((d3 f3 cs4 e4 fs4 a4 b4 c5 d5 e5 fs5 af5 ef6)))
              (set4 ((d3 e3 cs4 ef4 fs4 a4 b4 c5 d5 e5 fs5 af5 d6)))))))
  (cmn-display sp :file "/tmp/sp-out.eps" :break-line-each-set nil :size 16))

;;; cmn output of rthm-seq-palette object only
;;; ----------------------------------------------------------------------------
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
  (cmn-display rsp :file "/tmp/rsp-out.eps"))
  
;;; simple write-lp-data-for-all example
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e (s) s))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (write-lp-data-for-all mini :base-path "/tmp/"))

;;; simple clm-play example
;;; ----------------------------------------------------------------------------
#+clm
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1))
                             (player-two (1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-2)
                               (test-sndfile-3)))
                             (source-sndfile-grp-2
                              ((test-sndfile-5)
                               (test-sndfile-6))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (clm-play +sc-object+ 1 nil 'source-sndfile-grp-1 :check-overwrite nil))