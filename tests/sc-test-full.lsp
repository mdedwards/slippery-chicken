;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             test-full.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Load the test suite and run full tests, including demo
;;;                   compositions etc. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    11th December 2011 (Bangkok)
;;;
;;; $$ Last modified:  13:17:34 Mon Mar 25 2024 CET
;;;
;;; SVN ID: $Id: bar-holder.lsp 431 2011-12-08 14:44:30Z medward2 $
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-scale :quarter-tone)

;;; SAR Mon Jan 16 13:37:27 GMT 2012: Added a global variable here to store the
;;; pass-fail state of the sc-test-suite before re-initing the globals below,
;;; so that we can print the results of both the sc-test-suite and the piece
;;; tests at the end of sc-test-full.
(defparameter *sc-test-meth-and-func-tests-state-carry-over* nil)

;; MDE Mon Oct 20 17:25:49 2014 -- seems there's always a memory error or
;; something when we try to test all at once, so load sc-test-suite.lsp first
;; then restart lisp and load this file.
(load-from-test-suite-dir "sc-test-suite-aux.lsp")
;;(load-from-test-suite-dir "sc-test-suite.lsp")

;;; MDE Mon Jun 10 10:40:53 2013 
(set-sc-config 'cmn-display-auto-open nil)
(set-sc-config 'midi-play-auto-open nil)
(set-sc-config 'default-dir "/tmp/")
;;; DJR Mon 16 Sep 2019 16:14:35 BST
(set-sc-config 'lp-display-auto-open nil)



;;; SAR Mon Jan 16 13:38:51 GMT 2012: Transferring the results from
;;; sc-test-suite to the new global variable prior to re-init
(setf *sc-test-meth-and-func-tests-state-carry-over*
      *sc-test-meth-and-func-tests-state*)

;; re-init globals
(load-from-test-suite-dir "sc-test-suite-aux.lsp")

;;; SAR Tue Jul 24 11:18:55 BST 2012: Added similar global for webpage tests. 
(defparameter *sc-test-webpage-tests-state-carry-over* nil)

(load-from-test-suite-dir "sc-test-webpages.lsp")

(setf *sc-test-webpage-tests-state-carry-over*
      *sc-test-webpage-examples-tests-state*)

;; re-init globals
(load-from-test-suite-dir "sc-test-suite-aux.lsp")

(sc-deftest test-primary-disposition ()
  (declare (special +primary-disposition+))
  (let ((mid "/tmp/primary-disposition.mid")
        (eps "/tmp/primary-disposition.eps")
        (lpb "/tmp/")
        (lpn "primary-disposition-"))
    (loop for f in (list mid eps) do (probe-delete f))
    (loop for f in '(cel-part cel db-part db def flt-part flt tptc-part tptc
                     vla-part vla vlnone-part vlnone vlntwo-part vlntwo)
       do (probe-delete 
           (concatenate 'string lpb lpn (string-downcase f) ".ly")))
    (probe-delete (concatenate 'string lpb "_" lpn "score.ly"))
    (load-from-examples-dir "primary-disposition.lsp")
    (flet ((pdnotes ()
             (loop for i from 1 to 4 collect
                  (get-pitch-symbol
                   (get-note +primary-disposition+ 2 i 'clr)))))
      (sc-test-check
        ;; MDE Mon Apr 25 10:21:14 2016
        (equalp '(e4 b4 b4 e5) (pdnotes))
        (rm-repeated-pitches +primary-disposition+ 'clr)
        (equalp '(e4 b4 e5 fs5) (pdnotes))
        (file-write-ok mid 23000)))))

;;; SAR Wed Aug 8 13:57:10 BST 2012 
;;; This applies auto-slurs to all instruments over the whole piece. It is only
;;; to test that auto-slur produces no errors, rather than to check that the
;;; slurs are correct. It only tests with LP, since there's currently still an
;;; error if calling cmn-play after calling write-lp-data-for-all with the same
;;; object, which is the case in the given piece's primary test.
;;; These auto-slurs tests have all been check for basic accuracy by eye.
(sc-deftest test-pd-auto-slurs ()
  (progn
    (probe-delete "/tmp/_primary-disposition-score.ly")
    (sc-test-check
      (auto-slur +primary-disposition+ '(flt clr vln-one vla cel))
      (write-lp-data-for-all +primary-disposition+)
      (file-write-ok "/tmp/_primary-disposition-score.ly" 180))))

(sc-deftest test-second-law ()
  (let ((mid "/tmp/second-law.mid")
        (eps "/tmp/second-law.eps")
        (lpb "/tmp/")
        (lpn "second-law-"))
    (loop for f in (list mid eps) do (probe-delete f))
    (loop for f in '(bn-part bn cb-part cb cl-part cl def fl-part fl hn-part
                     hn ob-part ob tb-part tb tp-part tp va-part va vc-part
                     vc vno-part vno vnt-part vnt)
       do (probe-delete 
           (concatenate 'string lpb lpn (string-downcase f) ".ly")))
    (probe-delete (concatenate 'string lpb "_" lpn "score.ly"))
    (load-from-examples-dir "second-law.lsp")
    (sc-test-check 
      (file-write-ok mid 38000)
      #+cmn (file-write-ok eps 340000)
      (file-write-ok "/tmp/_second-law-score.ly" 17)
      (every #'(lambda (x y) (file-write-ok x y))
             (loop for f in '(bn-part bn cb-part cb cl-part cl def fl-part fl
                              hn-part hn ob-part ob tb-part tb tp-part tp
                              va-part va vc-part vc vno-part vno vnt-part vnt)
                do (concatenate 'string lpb lpn (string-downcase f) ".ly"))
             '(18 6000 18 6000 18 9000 4000 18 9000 18 7000 18 9000 18 5000 18
               6000 18 5000 18 6000 18 4000 18 4000)))))

;;; SAR Wed Aug  8 14:20:48 BST 2012
;;; See comment for test-pd-auto-slurs
(sc-deftest test-sl-auto-slurs ()
  (progn
    (probe-delete "/tmp/_second-law-score.ly")
    (sc-test-check
      (auto-slur +second-law+ '(fl ob cl bn hn tp tb vno vnt va vc cb))
      (write-lp-data-for-all +second-law+)
      (file-write-ok "/tmp/_second-law-score.ly" 160))))

#| SAR Wed Aug  8 18:46:46 BST 2012: Commenting these out because N-R seems to
   exhaust the SBCL heap 

;;; SAR Wed Aug  8 14:34:19 BST 2012
(sc-deftest test-nouveau ()
  (let* ((o-files '("nouveau-reich-pnoone.ly"
                    "nouveau-reich-pnotwo.ly"
                    "_nouveau-reich-score.ly"
                    "nouveau-reich-def.ly"
                    "nouveau-reich-pnoone-part.ly"
                    "nouveau-reich-pnotwo-part.ly"
                    #+cmn"nouveau-reich.eps"
                    "nouveau-reich.mid"))
         (o-files-sizes '(58000 75000 170 900 190 190 #+cmn 6500000 120)))
    (probe-delete-multi "/tmp/" o-files)
    (load-from-examples-dir "nouveau-reich.lsp")
    (sc-test-check
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Aug  8 14:36:38 BST 2012
;;; See comment for test-pd-auto-slurs
(sc-deftest test-nr-auto-slurs ()
  (progn
    (probe-delete "/tmp/_nouveau-reich-score.ly")
    (sc-test-check
      (auto-slur +nouveau-reich+ '(pno-one pno-two))
      (write-lp-data-for-all +nouveau-reich+)
      (file-write-ok "/tmp/_nouveau-reich-score.ly" 170))))
|#

;;; SAR Wed Aug  8 14:49:53 BST 2012
(sc-deftest test-tempus ()
  (let* ((o-files
          '("tempus-perfectum-cl-written.ly" "tempus-perfectum-hn-written.ly"
            "_tempus-perfectum-score.ly" "tempus-perfectum-bn-part.ly"
            "tempus-perfectum-bn.ly" "tempus-perfectum-cl-part.ly"
            "tempus-perfectum-cl.ly" "tempus-perfectum-def.ly"
            "tempus-perfectum-hn-part.ly" "tempus-perfectum-hn.ly"
            "tempus-perfectum-ob-part.ly" "tempus-perfectum-ob.ly"
            "tempus-perfectum-pl-part.ly" "tempus-perfectum-pl.ly"
            "tempus-perfectum-pr-part.ly" "tempus-perfectum-pr.ly"
            "tempus-perfectum-tb-part.ly" "tempus-perfectum-tb.ly"
            "tempus-perfectum-tp-part.ly" "tempus-perfectum-tp.ly"
            "tempus-perfectum-va-part.ly" "tempus-perfectum-va.ly"
            "tempus-perfectum-vc-part.ly" "tempus-perfectum-vc.ly"
            "tempus-perfectum-vn-part.ly" "tempus-perfectum-vn.ly"
            #+cmn "tempus-perfectum.eps" "tempus-perfectum.mid"
            #+clm
            "tempus-perfectum-1-c2-mouth-pops-clicks-seq1-71-psync.aiff"  
            #+clm
            "tempus-perfectum-1-c1-vocal-sounds-seq1-71-psync.aiff"))
         (o-files-sizes '(14000 14000 180 190 13000 200 13000 4600 200 13000
                          190 15000 190 18000 190 18000 190 16000 190 15000 190
                          14000 190 13000 190 15000 #+cmn 9000000 58000 #+clm
                          120000000 #+clm 119000000)))
    (probe-delete-multi "/tmp/" o-files)
    (load-from-examples-dir "tempus-perfectum.lsp")
    (sc-test-check
      (file-write-ok-multi "/tmp/" o-files o-files-sizes))))

;;; SAR Wed Aug  8 19:05:55 BST 2012
;;; See comment for test-pd-auto-slurs
(sc-deftest test-tp-auto-slurs ()
  (progn
    (probe-delete "/tmp/_tempus-perfectum-score.ly")
    (sc-test-check
      (auto-slur +tempus-perfectum+ '(ob cl bn hn tp tb pr pl vn va vc))
      (write-lp-data-for-all +tempus-perfectum+
                             :players '(ob cl bn hn tp tb pr pl vn va vc))
      (file-write-ok "/tmp/_tempus-perfectum-score.ly" 180))))


(sc-deftest test-slippery ()
  (declare (special +slippery-when-wet+))
    (let ((mid "/tmp/slippery.mid")
          (eps "/tmp/slippery.eps")
          (asco "/tmp/slippery.asco.txt")
          (player nil)
          (lp "/tmp/_slippery-when-wet-score.ly"))
      (when (probe-file mid)
        (delete-file mid))
      (when (probe-file eps)
        (delete-file eps))
      (when (probe-file lp)
        (delete-file lp))
      (load-from-examples-dir "slippery.lsp")
      ;; MDE Mon Dec 12 10:16:59 2011 -- just to test the bar numbers
      ;; vs. (section seq bar) references 
      (setf (tempo-map +slippery-when-wet+)
            ;; each section is 7 seqs long, each seq is 3 bars long, so (2 2 2)
            ;; is bar 26
            '((1 44) (4 66) (7 (h 60)) ((2 2 2) 96))
            player (get-player +slippery-when-wet+ 'solo))
      (sc-test-check 
        ;; MDE Thu Aug 28 20:36:31 2014 
        (empty-bars? +slippery-when-wet+ 22 24 'fl )
        (not (empty-bars? +slippery-when-wet+ 22 25 'fl ))
        ;; MDE Wed Apr 30 17:06:40 2014 
        (equalp (find-boundaries +slippery-when-wet+)
                '(3 16 25 46 52 58 67 115 151 169 213 232 245))
        (= 66 (bpm (get-tempo +slippery-when-wet+ 4)))
        (= 44 (bpm (get-tempo +slippery-when-wet+ 3)))
        (= 60 (bpm (get-tempo +slippery-when-wet+ 25)))
        (= 96 (bpm (get-tempo +slippery-when-wet+ 26)))
        ;; MDE Thu Jul  5 17:13:44 2012 -- test some stats
        (= 252 (num-bars (piece +slippery-when-wet+)))
        (equal-within-tolerance 630.0 (end-time (piece +slippery-when-wet+)))
        (= 5040 (num-notes (piece +slippery-when-wet+)))
        (= 5482 (num-score-notes (piece +slippery-when-wet+)))
        (= 3846 (num-rests (piece +slippery-when-wet+)))
        (= 215 (total-bars player))
        (= 949 (total-notes player))
        ;; MDE Mon May  5 20:48:19 2014 -- Write the antescofo file in the /tmp
        ;; directory  
        (> (print (write-antescofo +slippery-when-wet+ 'solo :file asco)) 5500)
        (file-write-ok asco 240000)
        (file-write-ok mid 40000)
        #+cmn (file-write-ok eps 3600000)
        (file-write-ok lp 150)
        ;; MDE Mon Jan 12 15:15:04 2015 -- make sure nested tuplets are working
        (string= (get-lp-data (get-event +slippery-when-wet+ 1 4 'vc) t)
                 "\\times 2/3 {" :end1 12))))

;;; SAR Thu Aug  9 11:45:24 BST 2012
;;; See comment for test-pd-auto-slurs
(sc-deftest test-slippery-auto-slurs ()
  (progn
    (probe-delete "/tmp/_slippery-when-wet-score.ly")
    (sc-test-check
      (auto-slur +slippery-when-wet+ '(solo vln vla vc fl cl hn perc))
      ;; MDE Thu Jun 25 10:53:24 2015 -- see if generating this score actually
      ;; works, rather than just the writing of the data files 
      ;; (write-lp-data-for-all +slippery-when-wet+)
      (lp-display +slippery-when-wet+)
      (file-write-ok "/tmp/_slippery-when-wet-score.ly" 190))))

(sc-deftest test-coming ()
    (let ((mid "/tmp/coming.mid")
          (asco "/tmp/coming.asco.txt"))
      (when (probe-file mid)
        (delete-file mid))
      (when (probe-file asco)
        (delete-file asco))
      (load-from-test-suite-dir "coming.lsp")
      (sc-test-check 
       ;; MDE Tue Apr  3 18:10:37 2012 
       (equalp 'bass-clarinet
               (get-first-for-player (instrument-change-map +coming-rthm-chain+)
                                     'clarinet))
       (> (write-antescofo +coming-rthm-chain+ 'flute :file asco) 12000)
       (file-write-ok asco 540000)
       (file-write-ok mid 113000))))

;;; SAR Thu Aug  9 11:53:21 BST 2012
;;; See comment for test-pd-auto-slurs
(sc-deftest test-coming-auto-slurs ()
  (progn
    (probe-delete "/tmp/_coming-score.ly")
    (sc-test-check
      (auto-slur +coming-rthm-chain+ '(flute clarinet bassoon marimba piano-rh
                                       piano-lh violin viola cello))
      (write-lp-data-for-all +coming-rthm-chain+)
      (file-write-ok "/tmp/_coming-score.ly" 160))))

(sc-deftest test-cheat-sheet ()
  (declare (special +cheat-sheet+))
  (let ((mid "/tmp/cheat-sheet.mid")
        clone)
      (when (probe-file mid)
        (delete-file mid))
      (load-from-test-suite-dir "cheat-sheet.lsp")
      ;; MDE Thu Jan 12 11:20:21 2012 -- test cloning of sc object
      (setf clone (clone +cheat-sheet+))
      (sc-test-check 
        (= (num-notes (piece +cheat-sheet+)) (num-notes (piece clone)))
        (file-write-ok mid 245000))))

#| SAR Thu Aug  9 12:05:15 BST 2012: Commenting this out, since LP produces
   hundreds of "warning: cannot end slur" messages, most of which appear to be
   related to \acciaccatura 

;;; SAR Thu Aug  9 12:00:57 BST 2012
;;; See comment for test-pd-auto-slurs
(sc-deftest test-cheat-auto-slurs ()
  (progn
    (probe-delete "/tmp/_slippery-chicken-piece-score.ly")
    (sc-test-check
      (auto-slur +cheat-sheet+ '(fl cl perc pno-rh pno-lh gtr vln vla vc db))
      (write-lp-data-for-all +cheat-sheet+)
      (file-write-ok "/tmp/_slippery-chicken-piece-score.ly" 190))))
|#

;;; MDE Thu Dec 29 20:43:21 2011 -- we can't actually make skin's sc structure
;;; as it relied upon the random-rep function of another lisp in order to
;;; work.  But the functions used until that point give sc a pretty heavy
;;; workout so let's do that at least.
(sc-deftest test-skin ()
  (load-from-test-suite-dir "skin.lsp"))

;;; SAR Thu Aug  9 12:22:53 BST 2012: Added no auto-slurs test for skin, as
;;; skin.lsp does not call make-slippery-chicken and there is therefore no
;;; global variable left over.

;;; MDE Sun Jan  1 14:40:42 2012 -- similar to skin: we give l-for-lookup and
;;; some other things a good workout here so let's run it. 

(sc-deftest test-tramontana ()
  (load-from-test-suite-dir "tramontana.lsp"))

;;; MDE Fri May 17 19:50:00 2013 -- test other pieces in the examples dir
;;; (simply by loading).  
(sc-deftest test-other-pieces ()
            (loop for i in '("hello-slippery.lsp" #+clm "sc-and-clm-examples.lsp"
                             #+clm "second-law-clm.lsp" "cavatina.lsp"
                             "chop-examples.lsp" "chords-examples.lsp"
                             "coming-piano-chords.lsp" "enharmonics-examples.lsp"
                             "ensemble-examples.lsp" "exercises.lsp"
                             "fibonacci-examples.lsp" "instruments-examples.lsp" 
                             "l-systems-examples.lsp" "marks-examples.lsp"
                             "notenames-scales-examples.lsp"
                             "o-haupt-voll-blut.lsp" 
                             "objects-slots-examples.lsp" "output-examples.lsp" 
                             "permutation-examples.lsp" "pitches-examples.lsp"
                             "post-gen-editing-examples.lsp" 
                             "reeling-trains.lsp" "rhythms-examples.lsp"
                             "rsp-rsm-examples.lsp"
                             "scores-examples.lsp"
                             "tempo-examples.lsp" "tonal-algo-example.lsp"
                             "rhythm-chains-examples.lsp")
                  do (load-from-examples-dir i))
            t)


(load-from-test-suite-dir "workshop-exercises.lsp")
(load-from-test-suite-dir "mini-examples-all.lsp")

;;; SAR Thu Aug  9 12:52:21 BST 2012: No auto-slurs test added for tramontana,
;;; as tramontana.lsp does not call make-slippery-chicken and there is
;;; therefore no global variable left over.

;;; SAR Mon Jan 16 13:08:09 GMT 2012: Modified the call to sc-test-test-all
;;; here to print the results of both the sc-test-suite and sc-test-full.

;;; SAR Mon Jan 16 12:54:26 GMT 2012: Amended sc-test-test-all to now change
;;; the value of the global parameter *sc-test-meth-and-func-tests-state*, so
;;; that it can be printed together with the results of the sc-test-full.

;;; SAR Mon Dec 26 10:41:36 EST 2011: 
;;; Added "PIECE" to the PASSED statement and changed the FAILED statment. As
;;; we're re-init'ing the globals at the top of this file, there is a separate
;;; PASS or FAIL statment being issued for each the sc-test-suite.lsp and
;;; test-full.lsp files, the former only applying to the tests contained in the
;;; sc-test-suite, the latter only applying to the composition tests.


;;; SAR Tue Jun  5 13:25:33 BST 2012: Moved all three CLM-PLAY tests to
;;; sc-test-suite.lsp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.12.11 SAR: Changed to print a SUCCESS report as well.
(if (sc-test-test-all)
    (progn
      (format t "~%~%- ALL PIECE TESTS PASSED.~%")
      (format t "~a~%" *sc-test-meth-and-func-tests-state-carry-over*)
      ;;; SAR Tue Jul 24 11:23:30 BST 2012: added for webpage tests:
      (format t "~a~%" *sc-test-webpage-tests-state-carry-over*))
    (error "PIECE TESTS FAILED."))

;;; SAR Fri Jan  6 14:28:11 EST 2012 -- putting this here instead
;;; MDE Sat Jan  7 13:12:29 2012 -- it's necessary because tramontana sets
;;; *scale* to be twelfth tone 
;;; MDE Fri Mar  2 16:24:39 2012 -- use in-scale instead
;;; (setf cm::*scale* (cm::find-object 'cm::quarter-tone))
(in-scale :quarter-tone)

;;; MDE Mon Jun 10 10:41:14 2013
(set-sc-config 'cmn-display-auto-open #+sc-auto-open T #-sc-auto-open nil)
(set-sc-config 'midi-play-auto-open #+sc-auto-open T #-sc-auto-open nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sc-test-full.lsp
