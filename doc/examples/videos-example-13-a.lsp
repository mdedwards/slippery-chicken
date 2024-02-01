;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-13-a.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 13
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    22nd December 2012
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

(let* ((sndfiles-dir
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))))
         :set-palette '((1 ((c3 d3 g3 a3 e4 f4 c5 g5 a5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -))
               :pitch-seq-palette ((1 2 3 5)))))
         :rthm-seq-map '((1 ((fl (1 1 1))
                             (ob (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1)
                               (test-sndfile-2))))
                            ,(list sndfiles-dir)
                            ("aiff")))))
  (clm-play mini 1 'fl 'group-1 :channels 8 :srate 44100
            :header-type clm::mus-aiff :data-format clm::mus-bshort 
            :sndfile-extension ".aiff"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
