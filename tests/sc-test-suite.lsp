;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             sc-test-suite.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Load the test suite and run tests of all documented
;;;                   methods and functions. Only FAIL feedback and desired
;;;                   warnings are printed. A message will be printed at the
;;;                   end of the run to indicate whether all tests passed or
;;;                   not.   
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    7th December 2011 (Edinburgh)
;;;
;;; $$ Last modified:  10:18:45 Sat Jun 18 2022 CEST
;;;
;;; SVN ID: $Id: sc-test-suite.lsp 6249 2017-06-07 16:05:15Z medward2 $
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

;;; MDE Thu May 30 16:17:47 2013 
(set-sc-config 'cmn-display-auto-open nil)
(set-sc-config 'midi-play-auto-open nil)
(set-sc-config 'default-dir "/tmp/")
;;; DJR Fri 30 Aug 2019 10:08:28 BST
(set-sc-config 'lp-display-auto-open nil)

;;; Thu Dec 15 10:01:31 GMT 2011 SAR: Changed intro
;;; Load this file in the Lisp prompt while in the slippery-chicken package.
(load-from-test-suite-dir "sc-test-suite-aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assoc-list tests

(sc-deftest test-al-get-keys () 
  (sc-test-check
    (equal (get-keys (make-assoc-list 'test '((cat felix)
                                              (dog fido)
                                              (cow bessie)))) 
           '(cat dog cow))
    (equal (get-keys (make-assoc-list 'test '((cat felix)
                                              (dog ((scottish terrier)   
                                                    (german shepherd)
                                                    (irish wolfhound)))  
                                              (cow bessie))))
           '(cat dog cow))))

(sc-deftest test-al-get-first ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey))))) 
    (sc-test-check
      (named-object-p (get-first al))
      (eq (id (get-first al)) 'jim)
      (eq (data (get-first al)) 'beam))))

(sc-deftest test-al-get-last ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey))))) 
    (sc-test-check
      (named-object-p (get-last al))
      (eq (id (get-last al)) 'wild)
      (eq (data (get-last al)) 'turkey))))

(sc-deftest test-al-get-position ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey))))) 
    (sc-test-check
      (eq (get-position 'four al) 1)  
      (eq (get-position 'jack al) nil)
      (eq (get-position 'jim al 1) nil))))

;; this one is supposed to produce a warning for the third EQ boolean 
(sc-deftest test-al-get-data-data ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey))))) 
    (sc-test-check
      (eq (get-data-data 'jim al) 'beam)
      ;; 8.12.11 ME: this was 'nil: removed quote
      (eq (get-data-data 'jack al) nil))))

;; this one is supposed to produce warnings for the 3rd and 4th EQ booleans
(sc-deftest test-al-get-data ()
  (let ((al (make-assoc-list 'al-test '((jim beam) 
                                        (four roses) 
                                        (wild turkey)))))
    (sc-test-check
      (named-object-p (get-data 'four al))
      (eq (id (get-data 'four al)) 'four)
      (eq (data (get-data 'four al)) 'roses)
      (eq (get-data 'jack al) nil)
      (eq (get-data 'jack al t) nil)
      (eq (get-data 'jack al nil) nil))))

(sc-deftest test-al-add ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey))))) 
    (sc-test-check
      (add '(makers mark) al)
      (named-object-p (get-data 'makers al))
      (eq (id (get-data 'makers al)) 'makers)
      (eq (data (get-data 'makers al)) 'mark)
      (eq (get-position 'makers al) 3)
      (add '(knob creek) al)
      (eq 4 (get-position 'knob al))
      (add '(jack daniels) al t)
      (eq 0 (get-position 'jack al))
      (eq 5 (get-position 'knob al)))))

;; this one is supposed to produce a warning on the 3rd EQ boolean
(sc-deftest test-al-set-data ()
  (let ((al (make-assoc-list 'test '((cat felix)
                                     (dog fido)
                                     (cow bessie)))))
    (sc-test-check
      (named-object-p (set-data 'dog '(dog spot) al))
      (eq (id (set-data 'dog '(dog spot) al)) 'dog)
      (eq (data (set-data 'dog '(dog spot) al)) 'spot)
      (eq (set-data 'pig '(pig wilber) al) nil)
      (eq (id (set-data 'dog '(pig wilbur) al)) 'pig)
      (eq (get-data-data 'pig al) 'wilbur))))

(sc-deftest test-al-add-to-list-data ()
  (let ((al (make-assoc-list 'test '((cat felix)
                                     (dog (fido spot))
                                     (cow bessie)))))
    (sc-test-check
      (named-object-p (add-to-list-data 'rover 'dog al))
      (eq (id (get-data 'dog al)) 'dog)
      (equal (get-data-data 'dog al) '(fido spot rover)))))

(sc-deftest test-al-add-to-list-data-force ()
  (let ((al (make-assoc-list 'test '((cat felix)
                                     (dog (fido spot))
                                     (cow bessie)))))
    (sc-test-check
      (named-object-p (add-to-list-data-force 'rover 'dog al))
      (eq (id (get-data 'dog al)) 'dog)
      (equal (get-data-data 'dog al) '(fido spot rover))
      (add-to-list-data-force 'wilber 'pig al)
      (equal (get-keys al) '(cat dog cow pig)))))

;;; 08.12.11 SAR
(sc-deftest test-al-set-nth-of-data ()
  (let ((al (make-assoc-list 'test '((cat felix)
                                     (dog (fido spot rover))
                                     (cow bessie)))))
    (sc-test-check
      (eq (set-nth-of-data 'dog 0 'snoopy al) 'snoopy)
      (named-object-p (get-data 'dog al))
      (equal (get-data-data 'dog al) '(snoopy spot rover)))))

;;; 08.12.11 SAR
(sc-deftest test-al-map-data ()
  (let ((al (make-assoc-list 'test '((1 (2 3))
                                     (2 (3 4))
                                     (3 (5 6))))))
    (sc-test-check
      ;; MDE Wed Aug  5 13:43:52 2015 
      (print-for-init al)
      (equalp (map-data al #'(lambda (y)
                               (loop for i in y collect (* i 2)))) 
              '((4 6) (6 8) (10 12)))
      ;; MDE Thu Nov  1 10:51:15 2018
      (assoc-list-p (nmap-data al #'(lambda (l) (length l))))
      ;; (print-for-init al)
      (equalp '(2 2 2) (mapcar #'data (data al))))))

;;; 08.12.11 SAR
(sc-deftest test-al-make-assoc-list ()
  (let ((al (make-assoc-list 'test '((bugs bunny)
                                     (daffy duck)
                                     (porky pig)))))
    (sc-test-check
      (named-object-p al)
      (equal (get-keys al) '(bugs daffy porky))
      (eq (get-data-data 'daffy al) 'duck))))

;;; MDE Mon Feb  1 12:51:26 2016 - do this for the ral class too
(sc-deftest test-remove-data ()
  (let ((al (make-assoc-list 'test '((bugs bunny)
                                     (daffy duck)
                                     (1 int)
                                     (2.3 float)
                                     ("string" string)
                                     (porky pig))))
        (ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red) ; rm
                                (violets ((blue velvet) 
                                          (red ((dragon den)
                                                ("viper" nest) ; rm
                                                (fox hole))) ; rm
                                          (white ribbon))))))))) ; rm
    (sc-test-check
      (remove-data al 'daffy 1 2.3 "string")
      (= 2 (sclist-length al))
      (not (remove-data al))
      (zerop (sclist-length al))
      (= 8 (r-count-elements ral))
      ;; shouldn't work because it's a string
      (remove-data ral '(four violets red viper)) 
      (= 8 (r-count-elements ral))
      (remove-data ral '(four violets red "viper"))
      (= 7 (r-count-elements ral))
      (remove-data ral 'jim '(four violets red fox))
      (remove-data ral '(four violets white))
      (remove-data ral '(four roses))
      (= 3 (r-count-elements ral))
      (eq 'turkey (get-data-data 'wild ral))
      (remove-data ral 'wild) ; try another top-level element
      (= 2 (r-count-elements ral))
      (not (get-data '(four violets red fox) ral nil))
      (eq 'den (get-data-data '(four violets red dragon) ral)))))

;; MDE Sat Sep 19 11:32:07 2020, Heidhausen
(sc-deftest test-assoc-list-ascending-ids? ()
  (sc-test-check
    (ascending-ids? (make-assoc-list 'test '((1 dog) (2 cat) (3 horse))))
    ;;; 3 is missing
    (not (ascending-ids? (make-assoc-list 'test '((1 dog) (2 cat) (4 horse)))))
    ;; doesn't start at 2
    (not (ascending-ids? (make-assoc-list 'test '((1 dog) (2 cat) (3 horse)))
                         2))
    ;; missing several integers
    (not (ascending-ids? (make-assoc-list 'test '((4 dog) (2 cat) (7 horse)))
                         2))
    (ascending-ids? (make-assoc-list 'test '((4 dog) (2 cat) (3 horse))) 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rthm-seq-bar tests

;;; MDE Sat Jun  6 15:15:09 2020, Heidhausen
(sc-deftest test-make-rsb-from-unit-multipliers ()
  (let ((b1 (make-rsb-from-unit-multipliers 'e '(3 4 1 2)))
        (b2 (make-rsb-from-unit-multipliers 's '(3)))
        (b3 (make-rsb-from-unit-multipliers 'h '(2 1))))
    (sc-test-check
      (time-sig-equal b1 '(10 8))
      (time-sig-equal b2 '(3 16))
      (eq 'e. (data (get-nth-event 0 b2)))
      (eq 'w (data (get-nth-event 0 b3)))
      (eq 'h (data (get-nth-event 1 b1)))
      (time-sig-equal b3 '(3 2)))))

;;; 09.12.11 SAR
;;; MDE Thu Aug 22 18:18:10 2013 -- made some changes for more detailed tests
(sc-deftest test-rsb-make-rthm-seq-bar ()
  (let (bar)
    (sc-test-check
      (rthm-seq-bar-p (make-rthm-seq-bar '((2 4) q e s s)))
      (equal (data (make-rthm-seq-bar '((2 4) q e s s))) '((2 4) q e s s))
      (setf bar (make-rthm-seq-bar '((2 4) q e s s) 'test))
      (= 4 (notes-needed bar))
      (make-rthm-seq-bar '((2 4) 4 8 16 16))
      (make-rthm-seq-bar '((2 4) q. e))
      (make-rthm-seq-bar '((2 4) 4\. 8))
      (make-rthm-seq-bar '((2 4) q +16.+32 e))
      (make-rthm-seq-bar '((2 4) q +16\.+32 e))
      ;; MDE Thu Jun  4 09:44:28 2020, Heidhausen -- test the tuplet as rational
      ;; case  
      (= 7/6 (first
              (first
               (tuplets
                (make-rthm-seq-bar '((3 4) { 7/6 (28/3) - 28/3 x 6 - }))))))
      (setf bar (make-rthm-seq-bar '((2 4) q \+16\.+32 e)))
      (= 2 (notes-needed bar))
      (= 2 (bar-pos (get-nth-event 2 bar)))
      (make-rthm-seq-bar '((2 4) 4+8 8))
      (make-rthm-seq-bar '((2 4) 4.+8))
      (make-rthm-seq-bar '((2 4) { 3 te te te } q)))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-fill-with-rhythms ()
  (let ((rsb (make-rthm-seq-bar '((3 4) q q q)))
        (rsb2 (make-rthm-seq-bar '((4 4) q q q q))))
    (sc-test-check
      (rthm-seq-bar-p rsb)
      (= 6 (fill-with-rhythms rsb (loop for r in '(e e e e e e) 
                                     collect (make-rhythm r))))
      (equal (loop for r in (rhythms rsb) collect (data r)) '(e e e e e e))
      (equal (id rsb) "rhythms-inserted-by-fill-with-rhythms")
      (fill-with-rhythms rsb2 (loop for r in '(h q e s s)
                                 for p in '(c4 dqs4 e4 gqf4 a4)
                                 collect (make-event p r))
                         :transposition -14
                         ;; MDE Sat May 19 20:47:39 2012 -- we need this!
                         :microtones-midi-channel 12
                         :midi-channel 11)
      (equalp
       (loop for e in (rhythms rsb2)
          collect (data (pitch-or-chord e)))
       '(C4 DQS4 E4 GQF4 A4))
      (equalp
       (loop for e in (rhythms rsb2)
          collect (data (written-pitch-or-chord e)))
       '(D5 EQS5 FS5 AQF5 B5))
      ;; MDE Sat May 19 20:53:33 2012 -- changed this to include 12
      (every #'(lambda (x) (or (= x 12) (= x 11)))
             (loop for e in (rhythms rsb2)
                collect (midi-channel (pitch-or-chord e))))
      ;; MDE Fri May 13 14:26:50 2016 -- see if we can allow underfull bars
      (= 5 (fill-with-rhythms rsb (loop for r in '(e e e e e q) 
                                     collect (make-rhythm r))
                              :is-full-error nil))
      (equal (loop for r in (rhythms rsb) collect (data r)) '(e e e e e))
      (= 5 (fill-with-rhythms rsb (loop for r in '(e e e e e) 
                                     collect (make-rhythm r))
                              :is-full-error nil))
      (equal (loop for r in (rhythms rsb) collect (data r)) '(e e e e e))
      )))

;;; 12.12.11 SAR
(sc-deftest test-rsb-all-rests? ()
  (let ((rsb1 (make-rthm-seq-bar '((2 4) (q) (e) (s) (s))))
        (rsb2 (make-rthm-seq-bar '((2 4) q e s s))))
    (sc-test-check
      (all-rests? rsb1)
      (not (all-rests? rsb2)))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-force-rest-bar ()
  (let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
    (sc-test-check
      (force-rest-bar rsb)
      (rthm-seq-bar-p rsb)
      (is-rest-bar rsb)
      (equal (loop for r in (rhythms rsb) collect (data r)) '(2))
      (equal (loop for r in (rhythms rsb) collect (is-rest r)) '(T)))))

;;; MDE Wed Nov  7 16:50:45 2018 
(sc-deftest test-rsb-force-all-rests ()
  (let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
    (sc-test-check
      (rthm-seq-bar-p (force-all-rests rsb))
      (rthm-seq-bar-p rsb)
      ;; (print rsb)
      (not (is-rest-bar rsb))
      (= 4 (num-rhythms rsb))
      (= 4 (num-rests rsb)))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-delete-beams ()
  (let ((rbs (make-rthm-seq-bar '((2 4) - s s - s - s s s - s s))))
    (sc-test-check
      (delete-beams rbs)
      (rthm-seq-bar-p rbs)
      (not (beams rbs))
      (equal (loop for r in (rhythms rbs) collect (beam r)) 
             '(NIL NIL NIL NIL NIL NIL NIL NIL)))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-auto-beam ()
  (let ((rsb (make-rthm-seq-bar '((2 4) e e s s s s)))
        ;; MDE Thu Nov 29 18:56:24 2012 
        (rsb2 (make-rthm-seq-bar '((9 8) (e.) (e.) e. { 3 - te +te te - } 
                                  (16) (16) (16) e)))
        (rsb3 (make-rthm-seq-bar '((2 2) { 3 te tq } { 3 tq te } 
                                   { 5 fs fs fs fe } +q)))
        (rsb4 (make-rthm-seq-bar '((9 8) s e s { 3 (te) te te } (q) e e e)))
        rr)
    (sc-test-check
      (auto-beam rsb)
      (rthm-seq-bar-p rsb)
      (equal (loop for r in (rhythms rsb) collect (beam r)) 
             '(1 0 1 NIL NIL 0)) 
      (auto-beam rsb2 nil nil)
      ;; (not (beam (get-nth-event 2 rsb2)))
      ;; (= 1 (beam (get-nth-event 3 rsb2)))
      ;; (= 0 (beam (get-nth-event 5 rsb2)))
      (auto-beam rsb3 nil nil)
      (not (beams rsb3))
      (auto-beam rsb4 nil nil)
      ;; (print (loop for r in (rhythms rsb4) collect (beam r)))
      (auto-beam rsb 8)
      (equal (loop for r in (rhythms rsb) collect (beam r)) 
             '(NIL NIL 1 0 1 0))
      ;; MDE Fri Apr 28 18:45:30 2017
      (replace-rhythms rsb3 5 4 (list (make-rhythm 'q)) t)
      (equalp '(te tq tq te q "Q") (get-rhythm-symbols rsb3))
      ;; MDE Wed Apr 26 16:13:22 2017
      ;; (print (rhythms rsb4))
      (setf rr (remove-rhythms rsb4 8 3))
      (= 7 (length rr))
      (equalp '(s e s te te te q) (get-rhythm-symbols rsb4))
      )))

;;; 12.12.11 SAR
(sc-deftest test-rsb-get-nth-non-rest-rhythm ()
  (let ((rsb (make-rthm-seq-bar '((2 4) e (e) s s (s) s))))
    (sc-test-check
      (rhythm-p (get-nth-non-rest-rhythm 0 rsb))
      (eq (data (get-nth-non-rest-rhythm 0 rsb)) 'E)
      (eq (data (get-nth-non-rest-rhythm 1 rsb)) 'S)
      (not (get-nth-non-rest-rhythm 4 rsb nil)))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-get-nth-rest ()
  (let ((rsb (make-rthm-seq-bar '((3 4) e (e) s s (s) s (q)))))
    (sc-test-check
      (rhythm-p (get-nth-rest 0 rsb))
      (eq (data (get-nth-rest 1 rsb)) 'S)
      (eq (data (get-nth-rest 2 rsb)) 'Q)
      (not (get-nth-rest 3 rsb nil)))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-get-nth-attack ()
  (let ((rsb (make-rthm-seq-bar '((3 4) q+e (e) s (s) e))))
    (sc-test-check
      (rhythm-p (get-nth-attack 0 rsb))
      (eq (data (get-nth-attack 1 rsb)) 'S)
      (eq (data (get-nth-attack 2 rsb)) 'E)
      (not (get-nth-attack 3 rsb nil)))))

;;; MDE Fri Jul 24 11:39:49 2015 
(sc-deftest test-rsb-get-nth-attack-with-tied ()
  ;; can't just make an rsb as that won't have proper tied-from info (that's
  ;; handled at rthm-seq level) 
  (let* ((rsb (first (bars (make-rthm-seq '(1 ((((5 4) q+e (e) s+s+e +s e s+e
                                                 (e)))))))))
         (las1 (get-last-attacks rsb 3))
         (las2 (get-last-attacks rsb 2))
         (las3 (get-last-attacks rsb 15)))
    (sc-test-check
      (= 2 (length (get-nth-attack-with-tied 0 rsb)))
      (= 4 (length (get-nth-attack-with-tied 1 rsb)))
      (= 1 (length (get-nth-attack-with-tied 2 rsb)))
      (= 2 (length (get-nth-attack-with-tied 3 rsb)))
      (= 3 (length las1))
      (= 2 (length las2))
      (not las3)
      (= 6 (bar-pos (fourth (first las1))))
      (= 1 (length (first las2))))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-set-nth-attack ()
  (let ((rsb (make-rthm-seq-bar '((2 4) q+e s s))))
    (sc-test-check
      (event-p (set-nth-attack 1 (make-event 'e4 'q) rsb))
      (equal (loop for r in (rhythms rsb) collect (data r)) '("Q" "E" Q S))
      (not (set-nth-attack 3 (make-event 'e4 'q) rsb nil)))))

;;; 12.12.11 SAR
(sc-deftest test-rsb-get-last-attack ()
  (let ((rsb (make-rthm-seq-bar '((3 4) q+e (e) s (s) e)))
        ;; MDE Wed Sep  4 12:50:52 2013 
        (rsb2 (make-rthm-seq-bar '((3 4) q+e (e) s.+32+e))))
    (sc-test-check
      (rhythm-p (get-last-attack rsb2))
      (= 3/8 (rq (get-last-attack rsb2)))
      (rhythm-p (get-last-attack rsb))
      (eq (data (get-last-attack rsb)) 'e))))

;;; 13.12.11 SAR
(sc-deftest test-rsb-get-time-sig ()
  (let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
    (sc-test-check
      (time-sig-p (get-time-sig rsb))
      (equal (data (get-time-sig rsb)) '(2 4)))))

;;; 13.12.11 SAR
(sc-deftest test-rsb-get-time-sig-as-list ()
  (let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
    (sc-test-check
      (listp (get-time-sig-as-list rsb))
      (equal (get-time-sig-as-list rsb) '(2 4)))))

;;; 13.12.11 SAR
(sc-deftest test-rsb-time-sig-equal ()
  (let ((rsb1 (make-rthm-seq-bar '((2 4) q e s s)))
        (rsb2 (make-rthm-seq-bar '((2 4) s s e q)))
        (rsb3 (make-rthm-seq-bar '((3 4) q+e e s s s s))))
    (sc-test-check
      (time-sig-equal rsb1 rsb2)
      (time-sig-equal rsb1 '(2 4))
      (time-sig-equal rsb3 (make-time-sig '(3 4)))
      (not (time-sig-equal rsb2 rsb3)))))

;;; 13.12.11 SAR
(sc-deftest test-rsb-make-rest-bar ()
  (let ((rsb-rb-t (make-rest-bar '(2 4) t t))
        (rsb-rb-nil (make-rest-bar '(2 4) nil nil)))
    (sc-test-check
      (rthm-seq-bar-p rsb-rb-t)
      (equal (data (get-time-sig rsb-rb-t)) '(2 4))
      (is-rest-bar rsb-rb-t)
      (write-time-sig rsb-rb-t)
      (show-rest rsb-rb-t)
      (not (write-time-sig rsb-rb-nil))
      (not (show-rest rsb-rb-nil)))))

;;; Wed Dec 14 14:03:13 GMT 2011 SAR
(sc-deftest test-rsb-delete-tuplets ()
  (let ((rsb1 (make-rthm-seq-bar '((2 4) { 3 te te te } q)))
        (rsb2 (make-rthm-seq-bar '((2 4) { 3 te te te } q))))
    (delete-tuplets rsb2)
    (sc-test-check
      (tuplets rsb1)
      (equal (loop for r in (rhythms rsb1) collect (bracket r)) 
             '(((1 3)) (-1) (1) NIL))
      (not (tuplets rsb2))
      (equal (loop for r in (rhythms rsb2) collect (bracket r))
             '(NIL NIL NIL NIL)))))

;;; SAR Thu Mar  1 13:22:02 GMT 2012: No further edits necessary
;;; Wed Dec 14 17:39:05 GMT 2011 SAR
(sc-deftest test-rsb-auto-put-tuplet-bracket-on-beats ()
  (let ((rsb (make-rthm-seq-bar '((4 4) q { 3 te te te } q q))))
    (sc-test-check
      ;; MDE Sun Jun 28 16:40:55 2015 -- these are no longer true because we
      ;; now require the { 3 in order to parse the rhythms and set slots
      ;; properly 
      ;;      (not (tuplets rsb))
      ;; (equal (loop for r in (rhythms rsb) collect (print (bracket r)))
      ;;    '(NIL NIL NIL NIL NIL NIL)) 
      (auto-put-tuplet-bracket-on-beats rsb 3)
      (equal (tuplets rsb) '((3 1 3)))
      (rthm-seq-bar-p (delete-tuplets rsb))
      (auto-put-tuplet-bracket-on-beats rsb 3 nil 1)
      (equal (tuplets rsb) '((3 1 3))))))

;;; Wed Dec 14 18:54:08 GMT 2011 SAR
(sc-deftest test-rsb-split ()
  (let* ((rsb (make-rthm-seq-bar '((7 4) h. e e +e. e. e q)))
         (rsb-splt (split rsb :min-beats 1 :max-beats 3)))
    (sc-test-check
      (listp rsb-splt)
      (equal (loop for i in rsb-splt 
                collect (loop for r in (rhythms i) 
                           collect (data r)))
             '((H.) (E E) ("E." E. E) (Q)))
      (not (split rsb :max-beats 1)))))

;;; MDE
(sc-deftest test-consolidate ()
  (let ((rs (make-rthm-seq 
             '(7 ((((2 4) h)
                   ((5 8) (e) e+q.)
                   ((3 4) { 7 28+28+28 28+28+28+28 } +q { 5 fs+fs+fs+fs+fs })
                   ;; MDE Sun Jun 28 16:42:41 2015 -- was repeated without
                   ;; tuplet info but that's now required
                   ({ 7 28+28+28 28+28+28+28 } q { 5 fs+fs+fs+fs+fs })
                   ((2 4) { 3 te tq } +q)
                   (+q \+32 (32) s e)))))))
    (sc-test-check
      (consolidate-notes (second (bars rs)))
      (consolidate-notes (third (bars rs)))
      (consolidate-notes (fourth (bars rs)))
      ;; todo: add more tests here
      (rhythm-equal (second (rhythms (second (bars rs)))) (make-rhythm 'h))
      (rhythm-equal (first (rhythms (third (bars rs)))) (make-rhythm '14\.))
      (rhythm-equal (fourth (rhythms (fourth (bars rs)))) (make-rhythm 'q)))))

;;; SAR Sun Dec 25 21:00:10 EST 2011
(sc-deftest test-rsb-get-nth-event ()
  (let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
    (sc-test-check
      (rhythm-p (get-nth-event 0 rsb))
      (equalp (data (get-nth-event 1 rsb)) 'e)
      (not (get-nth-event 4 rsb nil)))))

;;; SAR Sun Dec 25 21:12:17 EST 2011
(sc-deftest test-rsb-get-last-event ()
  (let ((rsb (make-rthm-seq-bar '((2 4) s s e q))))
    (sc-test-check
      (rhythm-p (get-last-event rsb))
      (equalp (data (get-last-event rsb)) 'Q))))

;;; SAR Sun Dec 25 21:52:14 EST 2011
(sc-deftest test-rsb-transpose ()
  (let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                             collect (make-event 'c4 'e))))))
    (sc-test-check
      (rthm-seq-bar-p (transpose rsb 3))
      (transpose rsb 3)
      (equalp (loop for p in (rhythms rsb)
                 collect (data (pitch-or-chord p)))
              '(C4 C4 C4))
      (equalp (loop for p in (rhythms (transpose rsb 3))
                 collect (data (pitch-or-chord p)))
              '(EF4 EF4 EF4))
      (transpose rsb 3 :destructively t)
      (equalp (loop for p in (rhythms rsb)
                 collect (data (pitch-or-chord p)))
              '(EF4 EF4 EF4)))))

;;; SAR Mon Dec 26 12:26:58 EST 2011
(sc-deftest test-rsb-enharmonic ()
  (let ((rsb1 (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                              collect (make-event 'cs4 'e)))))
        (rsb2 (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                              collect (make-event 'c4 'e)))))
        ;; MDE Wed Apr 18 11:45:51 2012 
        (rsb3 (make-rthm-seq-bar 
               (make-rthm-seq-bar `((2 4)
                                    ,@(loop for e in
                                           '((cs4 e) (ds4 e) (g4 e) (fs4 e))
                                         collect (apply #'make-event e))))))
        ;; MDE Mon Apr 23 13:27:59 2012 -- test it works on chords too
        (rsb4 (make-rthm-seq-bar 
               (make-rthm-seq-bar `((2 4)
                                    ,@(loop for e in
                                           '((cs4 e) ((ds4 fs4) e) (g4 e)
                                             (fs4 e))
                                         collect (apply #'make-event e)))))))
    (sc-test-check
      ;; MDE Wed Apr 18 11:48:10 2012 -- test whether the pitches keyword works
      (enharmonic rsb3)
      (equalp 'df4 (get-pitch-symbol (get-nth-event 0 rsb3)))
      (equalp 'ef4 (get-pitch-symbol (get-nth-event 1 rsb3)))
      (equalp 'g4 (get-pitch-symbol (get-nth-event 2 rsb3)))
      (equalp 'gf4 (get-pitch-symbol (get-nth-event 3 rsb3)))
      (enharmonic rsb3 :pitches '(df4 gf4))
      (equalp 'cs4 (get-pitch-symbol (get-nth-event 0 rsb3)))
      (equalp 'ef4 (get-pitch-symbol (get-nth-event 1 rsb3)))
      (equalp 'fs4 (get-pitch-symbol (get-nth-event 3 rsb3)))
      (enharmonic rsb4)
      (equalp '(ef4 gf4) (get-pitch-symbol (get-nth-event 1 rsb4)))
      (enharmonic rsb4 :pitches '(gf4))
      (equalp '(ef4 fs4) (get-pitch-symbol (get-nth-event 1 rsb4)))
      (enharmonic rsb1)
      (equalp (loop for p in (rhythms rsb1)
                 collect (get-pitch-symbol p))
              '(DF4 DF4 DF4))
      (enharmonic rsb2)
      (equalp (loop for p in (rhythms rsb2)
                 collect (get-pitch-symbol p))
              '(C4 C4 C4))
      (enharmonic rsb2 :force-naturals t)
      (equalp (loop for p in (rhythms rsb2)
                 collect (get-pitch-symbol p))
              '(BS3 BS3 BS3))
      (set-written rsb1 -3)
      (equalp (loop for p in (rhythms rsb1)
                 collect (get-pitch-symbol p))
              '(BF3 BF3 BF3))
      (enharmonic rsb1 :written t)
      (equalp (loop for p in (rhythms rsb1)
                 collect (get-pitch-symbol p))
              '(AS3 AS3 AS3)))))

;;; SAR Mon Dec 26 12:59:27 EST 2011
(sc-deftest test-rsb-set-written ()
  (let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                             collect (make-event 'cs4 'e)))))) 
    (sc-test-check
      (equalp (loop for p in (rhythms rsb)
                 collect (written-pitch-or-chord p))
              '(NIL NIL NIL))
      (set-written rsb -2)
      (equalp (loop for p in (rhythms rsb)
                 collect (get-pitch-symbol p))
              '(B3 B3 B3)))))

;;; SAR Tue Jan 17 21:28:29 GMT 2012: Optimized using notany and every
;;; SAR Mon Dec 26 14:28:18 EST 2011
(sc-deftest test-rsb-delete-written ()
  (let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
    (sc-test-check
      (notany #'(lambda (x) (written-pitch-or-chord x))
             (rhythms rsb))
      (set-written rsb -2)
      (every #'(lambda (x) (equalp (get-pitch-symbol x) 'B3))
             (rhythms rsb))
      (not (delete-written rsb))
      (notany #'(lambda (x) (written-pitch-or-chord x))
             (rhythms rsb)))))

;;; SAR Mon Dec 26 14:46:53 EST 2011
(sc-deftest test-rsb-set-midi-channel ()
  (let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                             collect (make-event 'cs4 'e))))))
    (sc-test-check
      (equalp (loop for p in (rhythms rsb)
                 collect (midi-channel (pitch-or-chord p)))
              '(1 1 1))
      (not (set-midi-channel rsb 13 14))
      (equalp (loop for p in (rhythms rsb)
                 collect (midi-channel (pitch-or-chord p)))
              '(13 13 13)))))

;;; SAR Mon Dec 26 20:01:02 EST 2011
(sc-deftest test-rsb-set-8va ()
  (let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                             collect (make-event 'cs4 'e))))))
    (sc-test-check
      (equalp (loop for e in (rhythms rsb) collect (8va e)) '(0 0 0))
      (not (set-8va rsb 1))
      (equalp (loop for e in (rhythms rsb) collect (8va e)) '(1 1 1)))))

;;; SAR Mon Dec 26 20:13:04 EST 2011
(sc-deftest test-rsb-reset-8va ()
  (let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                             collect (make-event 'cs4 'e))))))
    (sc-test-check
      (equalp (loop for e in (rhythms rsb) collect (8va e)) '(0 0 0))
      (not (set-8va rsb 1))
      (equalp (loop for e in (rhythms rsb) collect (8va e)) '(1 1 1))
      (not (reset-8va rsb))
      (equalp (loop for e in (rhythms rsb) collect (8va e)) '(0 0 0 )))))

;;; SAR Mon Dec 26 20:47:38 EST 2011
(sc-deftest test-rsb-delete-marks ()
  (let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                             collect (make-event 'cs4 'e))))))
    (sc-test-check
      (equalp (loop for e in (rhythms rsb) collect (marks e))
              '(NIL NIL NIL))
      (not (loop for e in (rhythms rsb) do (add-mark-once e 's)))
      (equalp (loop for e in (rhythms rsb) collect (marks e))
              '((S) (S) (S)))
      (not (delete-marks rsb))
      (equalp (loop for e in (rhythms rsb) collect (marks e))
              '(NIL NIL NIL)))))

;;; SAR Tue Jan 17 18:27:42 GMT 2012
(sc-deftest test-rsb-chop ()
  (let* ((rsb1 (make-rthm-seq-bar '((2 4) q q)))
         (rsb-chop1 (chop rsb1
                          '((1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) 
                            (3 4) (4 4)) 
                          's))
         (rsb2 (make-rthm-seq-bar '((2 4) q q)))
         (rsb-chop2 (chop rsb2 '((1 2) (1 1) (2 2)) 'e))
         (rsb3 (make-rthm-seq-bar '((4 4) - (s) (32) 32 (s) s - - +s+32 (32)
                                    (e) - (q) (s) s (e))))
         (rsb-chop3 (chop rsb3 
                          '((1 4) (1 3) (1 2) (1 1) (2 4) (2 3) (2 2) (3 4) 
                            (3 3) (4 4)) 
                          's))
         (rsb4 (make-rthm-seq-bar '((4 4) - (s) (32) 32 (s) s - - +s+32 (32)
                                    (e) - (q) (s) s (e))))
         (rsb-chop4 (chop rsb4 '((1 2) (1 1) (2 2)) 'e))
         (rsb5 (make-rthm-seq-bar '((2 4) { 3 te te te } e e)))
         (rsb-chop5 (chop rsb5))
         (rsb6 (make-rthm-seq-bar '((2 4) e { 3 te te te } e)))
         (rsb-chop6 (chop rsb6)))
    (sc-test-check
      (listp rsb-chop1)
      (every #'rthm-seq-bar-p rsb-chop1)
      (equalp (loop for rsb-obj in rsb-chop1
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (data r)))
              '((S) (E) (E.) (Q) (16) (8) (16/3) (16) (8) (16) (S) (E) (E.) (Q)
                (16) (8) (16/3) (16) (8) (16)))
      (equalp (loop for rsb-obj in rsb-chop1 
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (is-rest r)))
              '((NIL) (NIL) (NIL) (NIL) (T) (T) (T) (T) (T) (T) (NIL) (NIL)
                (NIL) (NIL) (T) (T) (T) (T) (T) (T)))
      (equalp (loop for rsb-obj in rsb-chop2
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (data r)))
              '((Q) (E) (8) (Q) (E) (8)))
      (equalp (loop for rsb-obj in rsb-chop2
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (is-rest r)))
              '((NIL) (NIL) (T) (NIL) (NIL) (T)))
      (equalp (loop for rsb-obj in rsb-chop3
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (data r)))
              '((S 32 32 S S) (S 32 32 S) (S 32 32) (16) (32 32 S S) (32 32 S) 
                (32 32) (S S) (16) (S) (4) (16/3) (8) (16) (16/3) (8) (16) (8)
                (16) (16) (4) (16/3) (8) (16) (16/3) (8) (16) (8) (16) (16) 
                (S S E)(S S S) (S S) (16) (S E) (S S) (S) (8) (16) (16)))
      (equalp (loop for rsb-obj in rsb-chop3
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (is-rest r)))
              '((T T NIL T NIL) (T T NIL T) (T T NIL) (T) (T NIL T NIL) 
                (T NIL T) (T NIL) (T NIL) (T) (NIL) (T) (T) (T) (T) (T) (T) (T)
                (T) (T) (T) (T) (T) (T) (T) (T) (T) (T) (T) (T) (T) (T NIL T)
                (T NIL T) (T NIL) (T) (NIL T) (NIL T) (NIL) (T) (T) (T)))
      (equalp (loop for rsb-obj in rsb-chop4
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (data r)))
              '((S 32 32 S S) (S 32 32) (S S) (4) (8) (8) (4) (8) (8) (S S E)  
                (S S) (8)))
      (equalp (loop for rsb-obj in rsb-chop4
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (is-rest r)))
              '((T T NIL T NIL) (T T NIL) (T NIL) (T) (T) (T) (T) (T) (T) 
                (T NIL T) (T NIL) (T)))
      (equalp (loop for rsb-obj in rsb-chop5
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (rq r)))
              '((1/3 1/3 1/3) (1/3 1/3 1/12) (1/3 1/6) (1/12 1/3 1/3)
                (1/12 1/3 1/12) (1/6 1/3) (1/4) (1/12 1/6) (1/6 1/12) (1/4)
                (1/2 1/2) (1/2 1/4) (1/2)
                (1/4 1/2) (1/4 1/4) (1/2) (1/4) (1/4) (1/4) (1/4)))
      (equalp (loop for rsb-obj in rsb-chop5
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (is-rest r)))
              '((NIL NIL NIL) (NIL NIL NIL) (NIL NIL) (T NIL NIL) (T NIL NIL)
                (T NIL) (NIL) (T NIL) (T NIL) (T) (NIL NIL) (NIL NIL) (NIL) 
                (T NIL) (T NIL) (NIL) (NIL) (T) (NIL) (T)))
      (equalp (loop for rsb-obj in rsb-chop6
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (rq r)))
              '((1/2 1/3 1/6) (1/2 1/4) (1/2) (1/4 1/3 1/6) (1/4 1/4)
                (1/3 1/6) (1/4) (1/4) (1/4) (1/12 1/6) (1/6 1/3 1/2)
                (1/6 1/3 1/4) (1/6 1/3) (1/4 1/2)
                (1/4 1/4) (1/2) (1/6 1/12) (1/4) (1/4) (1/4)))
      (equalp (loop for rsb-obj in rsb-chop6
                 collect (loop for r in (rhythms rsb-obj) 
                            collect (is-rest r)))
              ;; MDE Mon Mar 19 18:19:28 2012 rationalize-if-necessary changed
              #|
              '((NIL NIL NIL) (NIL NIL) (NIL) (T NIL NIL) (T NIL) (NIL NIL)
                (NIL) (T) (NIL) (T NIL) (T NIL NIL) (T NIL NIL) (T NIL) (T NIL)
                (T NIL) (T) (T NIL) (T) (T) (T))))))
              '((NIL NIL NIL) (NIL NIL) (NIL) (T NIL NIL) (T NIL) (NIL NIL)
                (NIL) (T) (NIL) (T NIL) (T NIL NIL) (T NIL NIL) (T NIL) 
                (T NIL) (T NIL) (T) (T NIL) (T) (T) (T))))))
              |#
              '((NIL NIL NIL) (NIL NIL) (NIL) (T NIL NIL) (T NIL) (NIL NIL)
                (NIL) (T) (NIL) (T NIL) (T NIL NIL) (T NIL NIL) (T NIL) (T NIL)
                (T NIL) (NIL) (T NIL) (T) (NIL) (T))))))

;;; SAR Thu Mar  1 13:48:09 GMT 2012
(sc-deftest test-rsb-scale ()
  (let ((rsb1 (make-rthm-seq-bar '((2 4) q e s s)))
        (rsb2 (make-rthm-seq-bar '((6 8) q e q s s))))
    (sc-test-check
      (equalp 
       (loop for r in (rhythms (scale rsb1 3)) collect (data r))
       '(H. Q. E. E.))
      (equalp
       (loop for r in (rhythms (scale rsb2 2 nil)) collect (data r))
       '(H Q H E E))
      (equalp (data (get-time-sig (scale rsb2 2 t))) '(6 4))
      (equalp (data (get-time-sig (scale rsb2 2 nil))) '(12 8)))))


;;; MDE Thu Apr 19 11:12:15 2012.  sounding-duration is a new slot
(sc-deftest test-rsb-sounding-duration ()
  (let ((rsb (make-rthm-seq-bar '((3 4) q q q))))
    (sc-test-check
      (not (sounding-duration rsb))
      (= 1 (set-sounding-duration rsb 1))
      (= 1 (sounding-duration rsb))
      ;; resets sounding-duration to nil so it's calculated next time
      (rhythms-to-events rsb)
      (not (set-sounding-duration rsb nil))
      ;; (print (rhythms rsb))
      (zerop (sounding-duration rsb)))))

;;; SAR Wed May  2 17:49:06 BST 2012
;;; This doesn't test the :min argument yet, since I can't get it to produce a
;;; different result.
(sc-deftest test-rsb-consolidate-rests ()
  (let ((rsb1 (make-rthm-seq-bar '((4 4) (e) (e) (e) (e) (e) (s) (s) (s) e.))) 
        (rsb2 (make-rthm-seq-bar '((4 4) (e) (e) (e) (e) (e) (s) (s) (s) e.))) 
        (rsb3 (make-rthm-seq-bar '((2 2) (e) (e) (e) (e) (e) (s) (s) (s) e.)))
        (rsb4 (make-rthm-seq-bar `((4 4) ,@(ml '(32) 16) (s) (s) e (e) (e))))
        (rsb5 (make-rthm-seq-bar `((4 4) ,@(ml '(32) 16) (s) (s) e (e) (e))))
        ;; MDE Mon Aug 26 16:57:27 2013 
        (rsb6 (make-rthm-seq-bar '((4 4) (q) { 3 (te) (tq) }
                                   { 3 (tq) tq (tq) })))
        ;; MDE Sun Sep  7 18:27:38 2014 
        (rsb7 (make-rthm-seq-bar '((4 4) (h) (s) (e.) (s) e.)))
        ;; MDE Thu Sep 11 20:34:15 2014 
        (rsb8 (make-rthm-seq-bar '((4 4) (s) (q..) h))))
    (sc-test-check
      (consolidate-rests rsb1)
      (equalp (loop for r in (rhythms rsb1) collect (data r))
              '(4 4 4 S E.))
      (consolidate-rests rsb2 :beat 2)
      (equalp (loop for r in (rhythms rsb2) collect (data r))
              '(2 E E S E.))
      (consolidate-rests rsb3)
      (equalp (loop for r in (rhythms rsb3) collect (data r))
              '(2 E E S E.))
      (consolidate-rests rsb3)
      (consolidate-rests rsb4 :min nil)
      (consolidate-rests rsb5 :min 'e)
      ;; (print (get-beats rsb6))
      (consolidate-rests rsb6 :warn t)
      ;; (print-simple rsb6)
      ;; MDE Sun Sep  7 18:28:24 2014 
      (equalp '(h 4 s e.)
              (loop for r in (rhythms (consolidate-rests rsb7))
                 collect (data r)))
      ;; MDE Thu Sep 11 20:34:55 2014 
      (equalp '(h h)
              (loop for r in (rhythms (consolidate-rests rsb8))
                 collect (data r)))
      (equalp (loop for r in (rhythms rsb4) collect (data r))
              '(4 4 e e 4))
      (equalp (loop for r in (rhythms rsb5) collect (data r))
              '(4 4 s s e 4))
      (equalp (loop for r in (rhythms rsb3) collect (data r))
              '(2 Q S E.)))))

;;; SAR Wed May  2 18:05:34 BST 2012
;;; This just tests the core function, including :beats, but doesn't test :min 
(sc-deftest test-rsb-consolidate-rests-max ()
  (let ((rsb1 (make-rthm-seq-bar '((2 2) (e) (e) (e) (e) (e) (s) (s) (s) e.)))
        (rsb2 (make-rthm-seq-bar '((2 2) (e) (e) (e) (e) (e) (s) (s) (s) e.)))
        (rsb3 (make-rthm-seq-bar `((4 4) ,@(ml '(32) 16) (s) (s) e (e) (e))))
        (rsb4 (make-rthm-seq-bar `((4 4) ,@(ml '(32) 16) (s) (s) e (e) (e)))))
    (sc-test-check
      (consolidate-rests rsb1)
      (consolidate-rests rsb1)
      (equalp (loop for r in (rhythms rsb1) collect (data r))
              '(2 Q S E.))
      (consolidate-rests-max rsb2)
      (equalp (loop for r in (rhythms rsb2) collect (data r))
              '(2 Q S E.))
      (consolidate-rests-max rsb3 :min nil :beat 2)
      (consolidate-rests-max rsb4 :min 'q :beat 2)
      (equalp (get-rhythm-symbols rsb3) '(2 e e q))
      (equalp (get-rhythm-symbols rsb4) '(2 s s e e e)))))

;;; SAR Wed May  2 19:17:24 BST 2012
(sc-deftest test-rsb-get-rhythm-symbols ()
  (let ((rsb (make-rthm-seq-bar '((4 4) q e s s q. e))))
    (sc-test-check
      (equalp (get-rhythm-symbols rsb) '(Q E S S Q. E)))))

;;; MDE Mon May  7 17:07:00 2012 
(sc-deftest test-rsb-auto-tuplets ()
  (let ((b1 (make-rthm-seq-bar '((4 4) { 3 tq tq 18 18 18 } h )))
        (b2 (make-rthm-seq-bar '((4 4) { 3 ts ts ts } e h.)))
        (b3 (make-rthm-seq-bar '((4 4) { 6 ts x 6 } q { 5 fe x 5 })))
        (b4 (make-rthm-seq-bar '((4 4) { 5 fe x 5 } { 6 ts x 6 } q))))
    (flet ((recreate-test (rsb)
             (let ((tups (copy-list (tuplets rsb))))
               (equalp tups (recreate-tuplets rsb)))))
      (sc-test-check
        ;; MDE Fri Apr  7 10:39:24 2017 -- before we delete let's test that
        ;; recreate-tuplets works
        (recreate-test b1)
        (recreate-test b2)
        (recreate-test b3)
        (recreate-test b4)
        (delete-tuplets b1)
        (delete-tuplets b2)
        (delete-tuplets b3)
        (delete-tuplets b4)
        (not (tuplets b1))
        (not (tuplets b2))
        (not (tuplets b3))
        (not (tuplets b4))
        (not (bracket (get-nth-event 0 b1)))
        (auto-tuplets b1)
        (equalp '((3 0 4)) (tuplets b1))
        (auto-tuplets b2)
        (equalp '((3 0 2)) (tuplets b2))
        ;; (print 'here)
        (auto-tuplets b3)
        (equalp '((3 0 2) (3 3 5) (5 7 11)) (tuplets b3))
        (auto-tuplets b4)
        (equalp '((5 0 4) (3 5 7) (3 8 10)) (tuplets b4))))))

;;; SAR Sun May 20 16:02:01 EDT 2012
(sc-deftest test-rsb-check-tuplets ()
  (let ((rsb (make-rthm-seq-bar '((4 4) { 3 te te te } q q q)))
        (rsb2 (make-rthm-seq-bar '((2 4) { 3 tq x 3 }))))
    (sc-test-check
      (equalp (bracket (get-nth-event 2 rsb)) '(1))
      (not (setf (bracket (get-nth-event 2 rsb)) nil))
      (not (bracket (get-nth-event 2 rsb)))
      (not (check-tuplets rsb #'warn))
      ;; MDE Wed Jul  4 13:43:02 2012 -- 
      (setf (bracket (get-nth-event 2 rsb2)) '(-1))
      (not (check-tuplets rsb2 nil)))))

;;; SAR Sun May 20 16:36:59 EDT 2012
(sc-deftest test-rsb-auto-tuplets-2 ()
  (let ((rsb (make-rthm-seq-bar '((4 4) { 3 tq tq tq } +q 
                                  { 5 fs fs fs fs fs }))))
    (sc-test-check
      (delete-tuplets rsb)
      (every #'not (loop for r in (rhythms rsb) collect (bracket r)))
      (auto-tuplets rsb)
      (equalp (loop for r in (rhythms rsb) collect (bracket r))
              '(((1 3)) (-1) (1) NIL ((2 5)) (-2) (-2) (-2) (2))))))

;;; SAR Wed May 30 21:27:11 BST 2012
(sc-deftest test-rsb-respell-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((gs4 af bf))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e - - e e -))
                                  :pitch-seq-palette ((1 2 1 1 1 1 1 1)))))
          :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (sc-test-check
      (equalp (loop for r in (rhythms (get-bar mini 2 'vn))
                 collect (get-pitch-symbol r))
              '(GS4 AF4 GS4 GS4 GS4 GS4 GS4 GS4))
      (rthm-seq-bar-p (respell-bar (get-bar mini 2 'vn) mini 'vn))
      (every #'(lambda (x) (equalp x 'gs4))
             (loop for r in (rhythms (get-bar mini 2 'vn))
                collect (get-pitch-symbol r))))))

;;; SAR Tue Jun  5 12:07:47 BST 2012
(sc-deftest test-rsb-rhythm-repeater-shorthand ()
  (let ((rsb (make-rthm-seq-bar '((4 4) s x 16))))
    (sc-test-check
      (equalp
       (loop for r in (rhythms rsb) collect (data r))
       '(S S S S S S S S S S S S S S S S)))))

;;; MDE Sat Jun  9 14:52:53 2012 -- 
(sc-deftest test-check-beams ()
  (let ((rsb (make-rthm-seq-bar '((6 16) (s) s (s) - s s s -))))
    (sc-test-check
      (setf (beam (get-nth-event 1 rsb)) 0)
      (equalp 'not-open (nth-value 1 (check-beams rsb :on-fail nil)))
      (setf (beam (get-nth-event 1 rsb)) 1)
      (equalp 'two-ones (nth-value 1 (check-beams rsb :on-fail nil)))
      (not (setf (beam (get-nth-event 1 rsb)) nil))
      (check-beams rsb :on-fail nil)
      (setf (beam (get-nth-event 1 rsb)) 1)
      (setf (beam (get-nth-event 2 rsb)) 0)
      (check-beams rsb :on-fail nil)
      (setf (beam (get-nth-event 1 rsb)) 1)
      (setf (beam (get-nth-event 2 rsb)) 1)
      (equalp 'two-ones (nth-value 1 (check-beams rsb :on-fail nil)))
      (setf (beam (get-nth-event 1 rsb)) 0)
      (setf (beam (get-nth-event 2 rsb)) 0)
      (equalp 'not-open (nth-value 1 (check-beams rsb :on-fail nil)))
      (not (setf (beam (get-nth-event 1 rsb)) nil))
      (not (setf (beam (get-nth-event 2 rsb)) nil))
      (not (setf (beam (get-nth-event 5 rsb)) nil))
      (equalp 'not-closed (nth-value 1 (check-beams rsb :on-fail nil)))
      (check-beams rsb :auto-beam t :on-fail nil))))


;;; MDE Tue Aug 27 15:18:50 2013 
(sc-deftest test-check-beams2 ()
  (let ((rsb (make-rthm-seq-bar '((4 4) s (e.) (q) (q) (e) e))))
    ;; break the beaming
    (setf (beam (get-nth-event 0 rsb)) 1
          (beam (get-nth-event 5 rsb)) 0)
    (sc-test-check
      (not (check-beams rsb :fix nil :on-fail nil))
      (auto-beam rsb)
      (every #'(lambda (r) (not (beam r))) (rhythms rsb)))))


;;; MDE Fri Apr 19 08:57:32 2013 
(sc-deftest test-check-beams-on-rests ()
  (let ((rsb1 (make-rthm-seq-bar '((6 16) - (s) s (s) - - s s s -)))
        (rsb2 (make-rthm-seq-bar '((6 16) (s) s (s) - s s s -))))
    (sc-test-check
      (beams-on-rests? rsb1)
      (not (beams-on-rests? rsb2)))))

;;; SAR Wed Jul 18 13:33:17 BST 2012
(sc-deftest test-rsb-set-amplitudes ()
  (let* ((mini
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e (e) e (e) (e) e e e))
                                   :pitch-seq-palette ((1 2 3 4 5)))))
           :rthm-seq-map '((1 ((va (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'va nil 1))
      (every #'not
             (loop for ne = (next-event mini 'va)
                while ne
                collect (marks ne)))
      (set-amplitudes (get-bar mini 2 'va) 0.9)
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL (FFF) NIL (FFF) NIL
         NIL (FFF) (FFF) (FFF) NIL NIL NIL NIL NIL NIL NIL NIL)))))

;;; SAR Wed Aug  8 11:59:07 BST 2012
(sc-deftest test-rsb-set-dynamics ()
  (let ((mini
         (make-slippery-chicken
          '+sc-object+
          :ensemble '(((va (viola :midi-channel 2))))
          :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e (e) e (e) (e) e e e))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((va (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'va nil 1))
      (every #'not
             (loop for ne = (next-event mini 'va)
                while ne
                collect (marks ne)))
      (set-dynamics (get-bar mini 2 'va) 'ppp)
      (not (next-event mini 'va nil 1))
      ;; MDE Sat Dec  3 16:03:17 2016 -- also test the new player method
      (eq 'va (player (get-bar mini 3 'va)))
      (loop for ne = (next-event mini 'va)
         while ne
         collect (marks ne))
      '(NIL NIL NIL NIL NIL NIL NIL NIL (PPP) NIL (PPP) NIL NIL (PPP) (PPP) 
        (PPP) NIL NIL NIL NIL NIL NIL NIL NIL))))

;;; MDE Thu May 28 12:29:54 2015 -- test the new rqq handling routines
(sc-deftest test-rqq-divide ()
  (let* ((rsp
          (make-rsp
           'rqq-test
           '((l1 ((((2 8) (1 ((4 (1 (1 (1 1 1)) 1 1)) (2 (1 1)) 1 
                              (1 (1 1 1))))))))
             ;; todo: this one produces a 40/3 with a letter-value of 10!
             (l2 ((((2 8) (1 ((4 (1 (1 (1 1 1)) 1 1)) (2 (1 1)) 3
                              (1 (1 1 1))))))))
             (l3 ((((4 4) (4 ((4 (1 (1 (1 1 1)) 1 1)) (2 (1 1)) 3
                              (1 (1 1 1))))))))
             ;; this one is a simple 1/4 note followed by 3 separate
             ;; rqq's. calling rqq-divide on this whole list will fail, as it
             ;; will be called on the rqq seqments only
             (l4 ((((4 4) q (1 (1 2 1 1 1)) (1 (2 1)) (1 (1 1 2))))))
             (l4a ((((4 4) q (1 (1 2 1 1 1)) e e (1 (1 1 2))))))
             ;; MDE Thu Jun 25 12:05:04 2015 -- test consolidating rqq rests
             (lc ((((2 8) (1 ((4 (1 (2) (1) 1)) (2 (1 (2) (1) (1) (1)))))))))
             (l4b ((((4 4) q (1 (1 (2) 1 1 1)) e (e) (1 ((1) 1 (2)))))))
             (l5 ((((4 4) (4 ((3 (2 1 1)) (2 (1 1 2 1)) (2 (1 (1 (1 2 2)) 
                                                              1))))))))
             (l5b ((((2 8) (1 ((4 (1 (2) (1) 1)) (2 (1 (2) (1) (1) (1)))))))))
             ;; first part of this is the slippery when wet cello opening.
             (mix ((((4 4) { 3 (te) { 3 (18) 36 } { 3 - 36 36 36 - } }
                     (1 ((4 (1 (1) 1 1 1)) (5 (1 1 1 1))))
                     (1 (1 (3 (1 1 1 1))))
                     { 5 fs x 5 }))))
             (l6 ((((2 4) (2 ((1 (1 1 1)) 2 (4 (1 2 3)))))))))))
         (l1rsb (first (bars (get-data 'l1 rsp))))
         (l4b-rsb (first (bars (get-data 'l4b rsp)))))
    (flet ((gd (id) (cadaar (get-data-data id rsp))))
      (sc-test-check
        ;; (cmn-display rsp)
        (= 4 (num-rests l4b-rsb))
        (= 7 (notes-needed l4b-rsb))
        (= 11 (num-rhythms l4b-rsb))
        (equalp (rqq-divide (gd 'l1))
                '(- 32 { 3 T64 T64 T64 } 32 32 32 32 32 { 3 T64 T64 T64 } -))
        (not (under-triplet l1rsb 0))
        (under-triplet l1rsb 1)
        (under-triplet l1rsb 2)
        (under-triplet l1rsb 3)
        (under-triplet l1rsb 9)
        (not (under-triplet l1rsb 4))
        ;; no beams with rests at beg or end!
        (not (beamable '({ 2/3 - { 5 30 (15) (30) 30 } { 3 72 (72/5) } - })))
        (equalp (rqq-divide (gd 'l2))
                '({ 5 - F32 { 3 120 120 120 } F32 F32 F32 F32 fs.
                  { 3 120 120 120 } - }))
        (equalp (rqq-divide (gd 'l5b))
                '({ 3 { 5 - 30 (15) (30) 30 - } { 3 72 (18) (72) } }))
        (equalp (rqq-divide (gd 'l6))
                '({ 7/4 { 3 - 42 42 42 - } 7 { 3 21 21/2 7 } }))
        (equalp (rqq-divide (gd 'l5))
                '({ 7/4 "7." "14." "14." { 5 - 35/2 35/2 35/4 35/2 - } {
                  3 - 21/2 { 5  105/2 105/4 105/4 } 21/2 - } }))
        ;; test the tuplet under tuplet
        (= 1 (num-flags (get-nth-rhythm 1 (get-data 'mix rsp))))
        (= 2 (num-flags (get-nth-rhythm 2 (get-data 'mix rsp))))
        (= 1 (num-dots (get-nth-attack 13 (get-data 'mix rsp))))
        (equalp (rqq-divide (gd 'l3))
                '({ 5 - FE { 3 30 30 30 } FE FE FE FE - FQ.
                  { 3 - 30 30 30 - } }))))))

(sc-deftest test-consolidate-rqq-rests ()
  (sc-test-check
    (equalp (consolidate-rqq-rests '((2) 1 (2) (1))) '((2) 1 (2) (1)))
    (equalp (consolidate-rqq-rests '(1 1 2 1 2 1)) '(1 1 2 1 2 1))
    (equalp (consolidate-rqq-rests '(3 (1) (2) 1 (1) (4) (1) 2))
            '(3 (2) (1) 1 (4) (2) 2))))

;;; MDE Wed Jun 24 20:36:49 2015 -- make sure nested tuplets have the right
;;; number of flags 
(sc-deftest test-letter-value ()
            (let* ((rsb (make-rthm-seq-bar
                         ;; mix up normal notation with rqq
                         '((4 4) { 3 (te) { 3 (18) 36 } { 3 - 36 36 36 - } }
                           (1 ((4 (1 (1) 1 1 1)) (5 (1 1 1 1))))
                           ;; 16th rest then 4 x 32.
                           (1 (1 (3 (1 1 1 1))))
                           { 5 fs x 5 })))
                   (rsb2 (make-rthm-seq-bar '((4 2) Q (7 (1 (1) 1 (3)))))))
              ;; (rhythms-to-events rsb) ; won't work: no pitches
              ;; (print-simple rsb)
              ;; (print (get-lp-data rsb t))
              (labels ((get-it (i) (get-nth-event i rsb))
                       (flags (i) (num-flags (get-it i)))
                       (lv (i) (letter-value (get-it i))))
                ;; (print-simple rsb)
                (sc-test-check
                 (= 1 (num-dots (get-nth-event 4 rsb2)))
                 (= 8 (lv 1))
                 (= 2 (flags 2))
                 (= 32 (lv 6))
                 (= 32 (lv 11))
                 (= 3 (flags 12))
                 (= 32 (lv 18))
                 (string= "32." (data (get-it 18)))
                 (= 16 (lv 20))
                 (= 2 (flags 21))))))

;;; MDE Thu Aug 25 20:53:23 2016 
(sc-deftest test-add-half-beat-rest ()
  (let ((bar1 (make-rthm-seq-bar '((3 4) q q q)))
        (bar2 (make-rthm-seq-bar '((2 4) q s s s s)))
        (bar3 (make-rthm-seq-bar '((5 8) q q s s))))
    (add-half-beat-rest bar1)
    (add-half-beat-rest bar2)
    (add-half-beat-rest bar3)
    (flet ((test-last (bar val)
             (let ((e (get-last-event bar)))
               (and (is-rest e)
                    (= (value e) val)))))
      (sc-test-check
        (time-sig-equal (get-time-sig bar1) (make-time-sig '(7 8)))
        (time-sig-equal (get-time-sig bar2) (make-time-sig '(5 8)))
        (time-sig-equal (get-time-sig bar3) (make-time-sig '(11 16)))
        (test-last bar1 8)
        (test-last bar2 8)
        (test-last bar3 16)))))

;;; MDE Mon Dec 16 09:57:29 2019
(sc-deftest test-get-nearest-by-start-time ()
  (let ((mini
         (make-slippery-chicken
          '+sc-object+
          :ensemble '(((va (viola :midi-channel 2))))
          :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) 32 x 32)))))
          :rthm-seq-map '((1 ((va (1 1 1))))))))
    (flet ((test-it (bar time etime)
             (equal-within-tolerance
              etime
              (start-time
               (get-nearest-by-start-time
                (get-bar mini bar 'va)
                (make-event 'c4 'e :start-time time))))))
      (sc-test-check
        (test-it 1 1.1 1.125)
        (test-it 3 7.9 8.0)
        (test-it 2 7.9 7.875)
        ;; (print (get-bar mini 2 'va))
        (test-it 2 7.1 7.125)
        (test-it 2 3.1 4.0)
        (test-it 1 0.05 0.0)
        (test-it 1 0.15 0.125)))))

;;; MDE Sat Jul 11 14:23:42 2020, Heidhausen
(sc-deftest test-invert-rsb ()
  (let* ((r (make-rest 'e))
         (mini
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) (32) 32 x 7 (16) s (16) (16) e (e)
                                     (s) s (e))))))
           :rthm-seq-map '((1 ((va (1 1 1)))))))
         (rsb1 (get-bar mini 1 'va))
         (rsb (clone rsb1)))
    (setf (pitch-or-chord r) '(c4 e4))
    ;; (print-simple rsb)
    (sc-test-check
      (invert rsb)
      (is-rest (get-nth-event 1 rsb))
      (is-rest (get-nth-event 2 rsb))
      (is-rest (get-nth-event 3 rsb))
      (is-rest (get-nth-event 5 rsb))
      (pitch-or-chord (get-nth-event 0 rsb))
      (pitch-or-chord (get-nth-event 4 rsb))
      (pitch-or-chord (get-nth-event 6 rsb))
      (pitch-or-chord (get-nth-event 7 rsb))
      (pitch-or-chord (get-nth-event 9 rsb))
      (pitch-or-chord (get-nth-event 10 rsb))
      (pitch-or-chord (get-nth-event 12 rsb))
      (setq rsb (clone rsb1))
      (invert rsb '(c4 d4 e4) nil)
      ;; (print-simple rsb)
      (is-rest (get-nth-event 1 rsb))
      (is-rest (get-nth-event 2 rsb))
      (is-rest (get-nth-event 3 rsb))
      (is-rest (get-nth-event 5 rsb))
      (eq 'c4 (get-pitch-symbol (get-nth-event 0 rsb)))
      (eq 'd4 (get-pitch-symbol (get-nth-event 4 rsb)))
      (eq 'e4 (get-pitch-symbol (get-nth-event 6 rsb)))
      (eq 'c4 (get-pitch-symbol (get-nth-event 7 rsb)))
      (eq 'd4 (get-pitch-symbol (get-nth-event 9 rsb)))
      (eq 'e4 (get-pitch-symbol (get-nth-event 10 rsb)))
      (eq 'c4 (get-pitch-symbol (get-nth-event 12 rsb)))
      (setq rsb (clone rsb1))
      (invert rsb '(c4 d4 e4) t)        ; with ties
      (is-rest (get-nth-event 1 rsb))
      (is-rest (get-nth-event 2 rsb))
      (is-rest (get-nth-event 3 rsb))
      (is-rest (get-nth-event 5 rsb))
      (eq 'c4 (get-pitch-symbol (get-nth-event 0 rsb)))
      (eq 'd4 (get-pitch-symbol (get-nth-event 4 rsb)))
      (eq 'e4 (get-pitch-symbol (get-nth-event 6 rsb)))
      (eq 'e4 (get-pitch-symbol (get-nth-event 7 rsb)))
      (eq 'c4 (get-pitch-symbol (get-nth-event 9 rsb)))
      (eq 'c4 (get-pitch-symbol (get-nth-event 10 rsb)))
      (eq 'd4 (get-pitch-symbol (get-nth-event 12 rsb)))
      (is-tied-from (get-nth-event 6 rsb))
      (is-tied-to (get-nth-event 7 rsb))
      (is-tied-from (get-nth-event 9 rsb))
      (is-tied-to (get-nth-event 10 rsb))
      ;; (print-simple rsb)
      (not (is-rest r))
      (equalp '(c4 e4) (get-pitch-symbols r)))))

;;; MDE Thu Jul  8 19:16:30 2021, Heidhausen 
(sc-deftest test-lotsa-rqqs ()
  (let* ((proportions '((1 1 1 1) (1 1 1) (1 1 2 1) (1 3 2 1) (8 3 2 4 5)))
         (permutations (permutate proportions))
         (meters (loop for n from 2 to 7 collect (list n 4)))
         ;; loop through each of the permutations and create an rq with each of
         ;; the given meters  
         (rqqs (loop for meter in meters append
                    (loop for permutation in permutations collect
                         (list
                          (list
                           (list meter
                                 (list (first meter)
                                       (loop for props in permutation collect
                                            (list 1 props)))))))))
         (rsp (make-rsp 'rqq-permutations nil))
         (file "/tmp/lotsa-rqqs.eps"))
    (loop for rs in rqqs and i from 1 do (add (make-rthm-seq (list i rs)) rsp))
    (sc-test-check
      #+cmn
      (progn
        (probe-delete file)
        (cmn-display rsp :file file)    ; this will generate about 103 pages
        (file-write-ok file 6000000)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rhythm tests

;;; 13.12.11 SAR
(sc-deftest test-rhythm-make-rhythm ()
  (let ((r1 (make-rhythm 16))
        (r2 (make-rhythm 8 :is-rest t :is-tied-to t))
        (r3 (make-rhythm .23 :duration t))
        (r4 (make-rhythm 'b)) ; brevis
        (r5 (make-rhythm 'l)) ; longa
        (r6 (make-rhythm 'm))) ; maxima
    (sc-test-check
      ;; MDE Wed Dec  4 14:55:44 2019 -- test the long ones
      (equal-within-tolerance 0.5 (value r4))
      (equal-within-tolerance 0.25 (value r5))
      (equal-within-tolerance 0.125 (value r6))
      (write-xml r4)
      (write-xml r5)
      (write-xml r6)
      (get-lp-data (make-event 'c4 'e) t)
      #+cmn (get-cmn-data (make-event 'c4 'm))
      (get-lp-data (make-event 'c4 'm) t)
      (get-lp-data (make-event 'c4 'l) t)
      (get-lp-data (make-event 'c4 'b) t)
      (rhythm-p r1)
      (not (is-rest r1))
      (not (is-tied-to r1))
      (= (duration r1) .25)
      (rhythm-p r2)
      (is-rest r2)
      (is-tied-to r2)
      (= (duration r2) .5)
      (rhythm-p r3)
      (= (duration r3) .23))))

;;; Wed Dec 14 20:49:46 GMT 2011 SAR
(sc-deftest test-rhythm-force-rest ()
  (let ((r (make-rhythm 8)))
    (sc-test-check
      (rhythm-p (force-rest r))
      (is-rest r))))

;;; Wed Dec 14 21:33:25 GMT 2011 SAR
(sc-deftest test-rhythm-scale ()
  (let ((r (make-rhythm 4))
        ;; MDE Tue Sep 29 10:46:20 2020, Heidhausen -- test the new method
        (sar (scale-as-rests (make-event '(cs4 e4) 's) 7)))
    (sc-test-check
      (= (value (scale r 2)) 2.0)
      (is-chord (first sar))
      (is-rest (first (last sar)))
      (= 7 (length sar))
      ;; MDE Mon Mar 19 17:56:18 2012
      ;; (= (value (scale r 3)) (/ 4 3.0))
      (equal-within-tolerance (value (scale r 3)) 4/3)
      (= (value (scale r .5)) (/ 4 .5))
      (= 1 (get-duration-as-beat (list (scale (make-rhythm 8) 3)
                                       (scale (make-rhythm 8) 5))))
      (equalp (loop for i from 1 to 5 collect (value (scale r .5))) 
              (loop for i from 1 to 5 collect 8.0))
      (equalp (loop for i from 1 to 5 collect (value (scale r .5 nil)))
              (loop for i from 0 to 4 collect (* 8.0 (expt 2 i)))))))

;;; Wed Dec 14 21:50:29 GMT 2011 SAR
(sc-deftest test-rhythm-rhythm-equal ()
  (let ((r1 (make-rhythm 4))
        (r2 (make-rhythm 4))
        (r3 (make-rhythm 4 :is-tied-to T))
        (r4 (make-rhythm 4 :is-tied-to NIL))
        (r5 (make-rhythm 4 :is-rest T))
        (r6 (make-rhythm 4 :is-rest NIL))
        (r7 (make-rhythm 8)))
    (sc-test-check
      (rhythm-equal r1 r2)
      (rhythm-equal r3 r4)
      (rhythm-equal r5 r6)
      (not (rhythm-equal r1 r7)))))

;;; Wed Dec 14 22:03:23 GMT 2011 SAR
(sc-deftest test-rhythm-rhythm/ ()
  (let ((r1 (make-rhythm 'q))
        (r2 (make-rhythm 'e))
        (r3 (make-rhythm 's.)))
    ;; MDE Tue Mar 20 09:10:41 2012 -- added the test-check
    (sc-test-check
      (= (rhythm/ r1 r2) 2.0)
      (equal-within-tolerance (rhythm/ r1 r3) (/ 1.0 0.375)))))

;;; 18.12.11 SAR
(sc-deftest test-rhythm-is-multiple ()
  (let ((r1 (make-rhythm 'q))
        (r2 (make-rhythm 'e))
        (r3 (make-rhythm 'e.)))
    (sc-test-check
      (is-multiple r1 r2)
      (not (is-multiple r1 r3)))))

;;; 18.12.11 SAR
(sc-deftest test-rhythm-add-mark ()
  (let ((r1 (make-rhythm 'q))
        (r2 (make-rhythm 'e :is-rest t)))
    (sc-test-check
      (add-mark r1 'a)
      (equalp (marks r1) '(a))
      (add-mark r1 's)
      (equalp (marks r1) '(s a))
      (add-mark r1 'a)
      (equalp (marks r1) '(a s a))
      (add-mark r2 'pizz)
      (is-rest r2)
      (equalp (marks r2) '(pizz)))))

;;; SAR Mon Dec 19 11:33:50 GMT 2011
(sc-deftest test-rhythm-add-mark-once ()
  (let ((r (make-rhythm 'q)))
    (sc-test-check
      (add-mark-once r 'a)
      (not (add-mark-once r 'a))
      (add-mark-once r 's)
      (equalp (marks r) '(s a)))))

;;; SAR Mon Dec 19 15:18:54 GMT 2011
(sc-deftest test-rhythm-rm-marks ()
  (let ((r (make-rhythm 'q)))
    (sc-test-check
      (progn
        (add-mark-once r 'a)
        (rm-marks r 'a)
        (not (marks r)))
      (progn 
        (loop for m in '(a s pizz col-legno x-head) do (add-mark-once r m)) 
        (rm-marks r 's)
        (equalp (marks r) '(x-head col-legno pizz a)))
      (progn
        (rm-marks r '(pizz a))
        (equalp (marks r) '(x-head col-legno))))))

;;; SAR Mon Dec 19 16:02:58 GMT 2011
(sc-deftest test-rhythm-replace-mark ()
  (let ((r (make-rhythm 'q)))
    (sc-test-check
      (add-mark-once r 'a)
      (listp (replace-mark r 'a 's))
      (equalp (marks r) '(s))
      (progn
        (loop for m in '(pizz col-legno x-head) do (add-mark-once r m)) 
        (equalp (marks r) '(x-head col-legno pizz s)))
      (progn
        (replace-mark r 's 'a)
        (equalp (marks r) '(x-head col-legno pizz a))))))

;;; SAR Mon Dec 19 19:51:14 GMT 2011
(sc-deftest test-rhythm-has-mark ()
  (let ((r (make-rhythm 'q)))
    (add-mark r 'a)
    (sc-test-check
      (has-mark r 'a)
      (not (has-mark r 's)))))

;;; SAR Thu Dec 22 09:44:28 EST 2011
(sc-deftest test-rhythm-accented-p ()
  (let ((r (make-rhythm 'q)))
    (sc-test-check
      (add-mark-once r 'a)
      (accented-p r)
      (progn
        (add-mark-once r 's)
        (rm-marks r 'a)
        (not (accented-p r))))))

;;; SAR Thu Dec 22 10:24:38 EST 2011
(sc-deftest test-rhythm-begin-slur-p ()
  (let ((r (make-rhythm 'q)))
    (sc-test-check
      (add-mark-once r 'beg-sl)
      (begin-slur-p r)
      (progn
        (add-mark-once r 's)
        (rm-marks r 'beg-sl)
        (not (begin-slur-p r))))))

;;; SAR Thu Dec 22 10:33:19 EST 2011
(sc-deftest test-rhythm-end-slur-p ()
  (let ((r (make-rhythm 'q)))
    (sc-test-check
      (add-mark-once r 'end-sl)
      (end-slur-p r)
      (progn
        (add-mark-once r 's)
        (rm-marks r 'end-sl)
        (not (end-slur-p r))))))

;;; SAR Thu Dec 22 11:04:54 EST 2011
(sc-deftest test-rhythm-delete-beam ()
  (let ((r (make-rhythm 'e))
        (rsb (make-rthm-seq-bar '((2 4) - s s e - q))))
    (sc-test-check
      (setf (beam r) 1)
      (not (delete-beam r))
      (progn
        (loop for r in (rhythms rsb) do (delete-beam r))
        (equalp (loop for r in (rhythms rsb) collect (beam r))
                '(NIL NIL NIL NIL))))))

;;; SAR Thu Dec 22 14:25:18 EST 2011
(sc-deftest test-rhythm-duration-secs ()
  (let ((r (make-rhythm 'q)))
    (sc-test-check
      (= (duration-secs r) 1.0)
      (= (duration-secs r 96) 0.625))))

;;; SAR Thu Dec 22 15:21:59 EST 2011
(sc-deftest test-rhythm-add ()
  (let ((r1 (make-rhythm 'q))
        (r2 (make-rhythm 'e))
        (r3 (make-rhythm 'te))
        (r4 (make-rhythm 4))
        (r5 (make-rhythm 28)))
    (sc-test-check
      (rhythm-p (add r1 r2))
      (equalp (data (add r1 r2)) 'Q.)
      (equalp (data (add r1 r3)) 'TH)
      (equal-within-tolerance (value (add r4 r5)) 3.5)
      ;; MDE Mon Mar 19 17:59:33 2012 
      ;; (= (duration (add r4 r5)) (+ 1.0 (/ 1.0 7.0)))
      (equal-within-tolerance (duration (add r4 r5)) 8/7)
      (not (data (add r4 r5))))))

;;; SAR Thu Dec 22 16:45:01 EST 2011
(sc-deftest test-rhythm-subtract ()
  (let ((r1 (make-rhythm 'q))
        (r2 (make-rhythm 'e))
        (r3 (make-rhythm 'h))
        (r4 (make-rhythm 'e.))
        (r5 (make-rhythm 4))
        (r6 (make-rhythm 28)))
    (sc-test-check
      (rhythm-p (subtract r1 r2))
      (equalp (data (subtract r1 r2)) 'E)
      (rhythm-p (subtract r3 r4))
      ;; MDE Fri May 29 11:11:58 2015 -- this was wrong
      ;; (equalp (data (print (subtract r3 r4))) 'TQ...)
      ;; this is correct
      (equal-within-tolerance 1.25 (duration (subtract r3 r4)))
      (rhythm-p (subtract r5 r6))
      (not (data (subtract r5 r6)))
      (equal-within-tolerance (value (subtract r5 r6)) 
         (/ 4.0 (/ 1029394200541831.0 1200959900632136.0)))
      (equal-within-tolerance (duration (subtract r5 r6))
         (/ 1029394200541831.0 1200959900632136.0)))))

;;; SAR Thu Dec 22 17:16:28 EST 2011
(sc-deftest test-rhythm-rhythm-list ()
  (let ((rl (rhythm-list '(q w+e q. h.+s e.+ts)))
        (rlc (rhythm-list '(q w+e q. h.+s e.+ts) t)))
    (sc-test-check
      (listp rl)
      (= (length rl) 8)
      (equalp (loop for r in rl collect (data r)) 
              '(Q "W" "E" Q. "H." "S" "E." "TS")) 
      (cscl-p rlc))))

;;; SAR Mon Dec 26 21:06:40 EST 2011
(sc-deftest test-rhythm-delete-marks ()
  ;; MDE Thu Dec 29 19:08:35 2011 -- why make-r and make-e???
  (let ((r (make-rhythm (make-event 'c4 'q))))
    (sc-test-check
      ;; MDE Thu Dec 29 19:01:34 2011 -- this was failing
      ;; (equalp (marks r) NIL)
      (not (marks r))
      (not (loop for m in '(a s pizz) do (add-mark-once r m)))
      (equalp (marks r) '(PIZZ S A))
      (not (delete-marks r))
      ;; MDE Thu Dec 29 19:02:55 2011 -- sim.
      ;; (equalp (marks r) NIL))))
      (not (marks r)))))

;;; MDE Sat Jun 20 18:48:05 2015 
(sc-deftest test-rthm-num-flags ()
  (= 0 (rthm-num-flags 4))
  (= 0 (rthm-num-flags 5))
  (= 1 (rthm-num-flags 8))
  (= 2 (rthm-num-flags 16))
  (= 3 (rthm-num-flags 32))
  (= 4 (rthm-num-flags 64))
  (= 5 (rthm-num-flags 128)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; event tests

;;; SAR Thu Dec 22 19:29:27 EST 2011
(sc-deftest test-event-make-event ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event '(c4 e4 g4) 4 :duration t))
        (e3 (make-event 'c4 8 :is-tied-to t :midi-channel 1 :amplitude 0.5
                        :is-tied-from t))
        (e4 (make-event nil 'q. :is-rest t))
        ;; MDE Wed Apr 26 15:36:02 2017 
        (e2m (make-event '(c4 eqf4 gqs4) 4 :duration t))
        ;; MDE Sat Apr 20 15:08:08 2013 
        (e5 (make-event 'cs4 'tq :transposition -14)))
    (flet ((mtonal (e expected &optional force-number)
             (multiple-value-bind
                   (micro num)
                 (microtonal e force-number)
               (equalp expected (list micro num)))))
      (sc-test-check
        ;; MDE Tue Nov  6 08:17:38 2018
        (= 3 (num-notes e2))
        (= 0 (num-notes e4))
        (= 1 (num-notes e1))
        (equal-within-tolerance 261.62554931 (get-frequency e1))
        (float-list= (get-frequency e2) 
                     '(261.62554931 329.62756347656 391.995422363)
                     0.0001)
        (equal-within-tolerance 327.749511718
                                (get-frequency e2 :average t) .0001)
        (event-p e1)
        (event-p e2)
        (event-p e3)
        (event-p e4)
        (event-p e5)
        ;; MDE Wed Apr 26 15:33:42 2017
        (mtonal e1 '(nil nil))
        (mtonal e1 '(0 1) t) ; force numbers
        (mtonal e2 '(0 3) t)
        (mtonal e2m '(2 3))
        ;; MDE Sat Apr 20 15:09:52 2013 - so sounding pitch was given (cs4) but
        ;; transposition is -14 so written pitch must be a 9th higher: ds5/ef5 
        (= 75 (midi-note (written-pitch-or-chord e5)))
        ;; (print e5)
        (equalp (data e2) 'W)
        (= (value e2) 1.0)
        (= (duration e2) 4.0)
        (equalp (loop for p in (data (pitch-or-chord e2)) collect (data p))
                '(C4 E4 G4))
        (is-tied-to e3)
        (is-tied-from e3)
        (= (midi-channel (pitch-or-chord e3)) 1)
        (= (amplitude e3) 0.5)
        (not (pitch-or-chord e4))
        (equalp (data e4) 'Q.)
        (is-rest e4)
        ;; MDE Wed Nov 13 18:50:05 2013 
        (add-pitches e1 'cs3 'd5)
        (add-pitches e2 'cs2)
        (chord= (pitch-or-chord e1) (make-chord '(c4 cs3 d5)))
        (chord= (pitch-or-chord e2) (make-chord '(c4 e4 g4 cs2)))
        ;; MDE Thu Aug 22 11:21:44 2019 -- make sure midi channel is retained
        (add-pitches e2 (make-pitch 'c1 :midi-channel 16))
        (chord= (pitch-or-chord e2) (make-chord '(c1 c4 e4 g4 cs2)))
        (= 16 (midi-channel (lowest e2)))))))

;;; SAR Thu Dec 22 21:07:38 EST 2011
(sc-deftest test-event-make-rest ()
  (let ((e (make-rest 4 :start-time 13.7 :duration t)))
    (sc-test-check
      (is-rest e)
      (equalp (data e) 'W)
      (= (duration e) 4.0)
      (= (value e) 1.0)
      (= (start-time e) 13.7))))

;;; SAR Fri Dec 23 13:31:22 EST 2011
(sc-deftest test-event-make-punctuation-events ()
  (let ((pe (make-punctuation-events '(2 3 5 8) 's '(c4 e4)))
        (g nil))
    (setf g (loop for e in pe collect (not (is-rest e))))
    (sc-test-check
      (listp pe)
      (= (length pe) (+ 2 3 5 8))
      (equalp (loop for e in pe collect (data e))
              (loop for c from 1 to (+ 2 3 5 8) collect 's))
      (equalp (loop for e in pe
                 when (not (is-rest e))
                 collect (data (pitch-or-chord e)))
              '(C4 E4 E4 E4))
      (equalp (loop for e in pe
                 when (is-rest e)
                 collect (pitch-or-chord e))
              (loop for c from 1 to (- (+ 2 3 5 8) 4) collect NIL))
      (equalp (let ((n 1)
                    (v nil))
                (loop for i from 1 to (length g)
                   do (setf n (1+ n))
                   when (nth i g)
                   do (progn
                        (push (- n 1) v)
                        (setf n 1)))
                (push (- n 1) v)
                (reverse v))
              '(2 3 5 8)))))

;;; SAR Fri Dec 23 14:32:37 EST 2011
(sc-deftest test-event-make-events ()
  (let ((e1 (make-events '((g4 q) e s ((d4 fs4 a4) s))))
        (e2 (make-events '((g4 q) e s (a4 s) q e (b4 s)) 3)))
    (sc-test-check
      (listp e1)
      (listp e2)
      (equalp (loop for i in e1
                 when (not (is-rest i))
                 collect (cond
                           ((pitch-p (pitch-or-chord i)) 
                            (data (pitch-or-chord i)))
                           ((chord-p (pitch-or-chord i)) 
                            (loop for p in (data (pitch-or-chord i)) 
                               collect (data p)))))
              '(G4 (D4 FS4 A4)))
      (equalp (loop for i in e1 collect (data i))
              '(Q E S S))
      (equalp (loop for i in e1 collect (is-rest i))
              '(NIL T T NIL))
      (equalp (loop for i in e2
                 when (not (is-rest i))
                 collect (midi-channel (pitch-or-chord i)))
              '(3 3 3)))))

;;; SAR Fri Dec 23 15:17:05 EST 2011
(sc-deftest test-event-make-events2 ()
  (let ((e1 (make-events2 '(q e e. h+s 32 q+te) '(cs4 d4 (e4 g4 b5) nil a3 r)))
        (e2 (make-events2 '(q e. h+s 32 q+te) '(cs4 b5 nil a3 r) 3)))
    (sc-test-check
      (listp e1)
      (listp e2)
      (equalp (loop for i in e1
                 when (not (is-rest i))
                 collect (cond
                           ((pitch-p (pitch-or-chord i)) 
                            (data (pitch-or-chord i)))
                           ((chord-p (pitch-or-chord i)) 
                            (loop for p in (data (pitch-or-chord i)) 
                               collect (data p)))))
              '(CS4 D4 (E4 G4 B5) A3))
      (equalp (loop for i in e1 collect (data i))
              '(Q E E. "H" "S" 32 "Q" "TE"))
      (equalp (loop for i in e1 collect (is-rest i))
              '(NIL NIL NIL T T NIL T T))
      (equalp (loop for i in e2
                 when (not (is-rest i))
                 collect (midi-channel (pitch-or-chord i)))
              '(3 3 3)))))

;;; MDE Sat Sep 12 18:30:43 2020, Heidhausen 
(sc-deftest test-event-make-events3 ()
  (sc-test-check 
    (= 4 (length (make-events3 '(g4 s a s b 32 c 32) nil)))
    ;; pitch and rhythms in pairs in a single list, including chords and rests
    (= 4 (length (make-events3 '((g4 q) e s ((d4 fs4 a4) s)) nil)))
    ;; using separate lists for rhythms and pitches, tied rhythms, single
    ;; pitches, a chord, and both nil and r to indicate rests
    (= 8 (length
          (make-events3 '(q e e. h+s 32 q+te) '(cs4 d4 (e4 g4 b5) nil a3 r))))
    ;; just one pitch but various rhythms (but here we can't use tied rhythms) 
    (= 9 (length (make-events3 '(q e e. s e s s s s) 'cs6)))
    ;; just one rhythm but various pitches/chords and all in octave 4 without
    ;; having to retype (here we can't use tied rhythms either)
    (= 5 (length (make-events3 's '(c4 d e (f a) bf))))))


(sc-deftest test-event-make-tuplet-events ()
  (let ((events1 (make-tuplet-events '((g4 q) e s s e q) '(7/3 3)))
        (events2 (make-tuplet-events '((g4 q) e s s e q) '(7/6 6/5))))
    (sc-test-check
      (equal-within-tolerance 1 (sum-rhythms-duration events1))
      (equal-within-tolerance 2.5 (sum-rhythms-duration events2)))))

;;; SAR Fri Dec 23 15:28:12 EST 2011
(sc-deftest test-event-event-p ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-rhythm 4))
        (e3 (make-rest 4))
        (e4 (make-events '((g4 q) e s))))
    (sc-test-check
      (event-p e1)
      (not (event-p e2))
      (event-p e3)
      (not (event-p e4)))))

;;; SAR Fri Dec 23 15:41:59 EST 2011
(sc-deftest test-event-is-dynamic ()
  (let ((d1 'ff)
        (d2 'pizz))
    (sc-test-check
      (is-dynamic d1)
      (not (is-dynamic d2)))))

;;; SAR Fri Dec 23 15:58:13 EST 2011
(sc-deftest test-event-set-midi-channel ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (= 1 (midi-channel (pitch-or-chord e)))
      (= (set-midi-channel e 7 8) 7))))

(sc-deftest test-event-get-midi-channel ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (= 1 (get-midi-channel e))
      (= (set-midi-channel e 11 12) 11)
      (= (get-midi-channel e) 11))))

;;; SAR Fri Dec 23 16:38:20 EST 2011
(sc-deftest test-event-get-dynamics ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (get-dynamics e))
      (add-mark-once e 'ppp)
      (add-mark-once e 'pizz)
      (equalp (get-dynamics e) '(ppp))
      (push 'fff (marks e))
      (equalp (get-dynamics e) '(fff ppp)))))

;;; SAR Fri Dec 23 16:52:18 EST 2011
(sc-deftest test-event-remove-dynamics ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (remove-dynamics e))
      (add-mark-once e 'ppp)
      (add-mark-once e 'pizz)
      (equalp (marks e) '(pizz ppp))
      (equalp (remove-dynamics e) '(pizz)))))

;;; SAR Fri Dec 30 14:05:43 EST 2011
(sc-deftest test-event-setf-amplitude ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (equal-within-tolerance (amplitude e) 0.7)
      (setf (amplitude e) .3)
      (equal-within-tolerance (amplitude e) .3)
      (equalp (marks e) '(pp))
      (setf (amplitude e) 1.3)
      (equal-within-tolerance (amplitude e) 1.3)
      (equalp (marks e) '(FFFF))
      (setf (amplitude e) -1.3)
      (equal-within-tolerance (amplitude e) -1.3)
      (equalp (marks e) '(NIENTE)))))

;;; SAR Fri Dec 23 18:30:23 EST 2011
(sc-deftest test-event-tempo-change ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (tempo-change e))
      (setf (tempo-change e) 132)
      (= (data (tempo-change e)) 132))))

;;; SAR Fri Dec 23 18:42:48 EST 2011
(sc-deftest test-event-sharp-p ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event 'df4 'q))
        (e3 (make-event 'cs4 'q)))
    (sc-test-check
      (not (sharp-p e1))
      (not (sharp-p e2))
      (sharp-p e3))))

;;; SAR Fri Dec 23 18:51:03 EST 2011
(sc-deftest test-event-flat-p ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event 'df4 'q))
        (e3 (make-event 'cs4 'q)))
    (sc-test-check
      (not (flat-p e1))
      (flat-p e2)
      (not (flat-p e3)))))

;;; SAR Fri Dec 23 18:56:07 EST 2011
(sc-deftest test-event-natural-p ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event 'df4 'q))
        (e3 (make-event 'cs4 'q)))
    (sc-test-check
      (natural-p e1)
      (not (natural-p e2))
      (not (natural-p e3)))))

;;; SAR Fri Dec 23 20:29:31 EST 2011
(sc-deftest test-event-enharmonic ()
  (let ((e1 (make-event 'cs4 'q))
        (e2 (make-event 'b3 'q)))
    (sc-test-check
      (event-p (enharmonic e1))
      (event-p (enharmonic e2))
      (equalp (data (pitch-or-chord e1)) 'df4)
      (equalp (data (pitch-or-chord e2)) 'b3)
      (enharmonic e2 :force-naturals t)
      (equalp (data (pitch-or-chord e2)) 'cf4))))

;;; SAR Fri Dec 23 20:42:17 EST 2011
(sc-deftest test-event-pitch- ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event 'a3 'q)))
    (sc-test-check
      (= (pitch- e1 e2) 3.0)
      (= (pitch- e2 e1) -3.0))))

;;; SAR Sat Dec 24 09:12:51 EST 2011
(sc-deftest test-event-set-midi-time-sig ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (midi-time-sig e))
      (set-midi-time-sig e (make-time-sig '(3 4)))
      (equalp (data (midi-time-sig e)) '(3 4)))))

;;; SAR Sat Dec 24 10:57:22 EST 2011
(sc-deftest test-event-end-trill ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (marks e))
      (end-trill e)
      (equalp (marks e) '(end-trill-a)))))

;;; SAR Sat Dec 24 11:11:51 EST 2011
(sc-deftest test-event-add-clef ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (marks-before e))
      (add-clef e 'treble)
      (add-clef e 'treble)
      (equalp (marks-before e) '((clef treble))))))

;;; SAR Sat Dec 24 11:25:33 EST 2011
(sc-deftest test-event-get-clef ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (get-clef e))
      (add-clef e 'treble)
      (symbolp (get-clef e))
      (equalp (get-clef e) 'TREBLE))))

;;; SAR Sat Dec 24 11:58:31 EST 2011
(sc-deftest test-event-delete-clefs ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (delete-clefs e))
      (add-clef e 'treble)
      (equalp (marks-before e) '((clef treble)))
      (not (delete-clefs e))
      (not (marks-before e)))))

;;; SAR Sat Dec 24 12:11:09 EST 2011
(sc-deftest test-event-get-amplitude ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (equal-within-tolerance (get-amplitude e) 0.7)
      (equalp (get-amplitude e t) 89)
      (setf (amplitude e) 0.3)
      (equal-within-tolerance (get-amplitude e) 0.3))))

;;; SAR Sat Dec 24 12:25:00 EST 2011
(sc-deftest test-event-get-pitch-symbol()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event '(c4 e4 g4) 'q)))
    (sc-test-check
      (equalp (get-pitch-symbol e1) 'C4)
      (equalp (get-pitch-symbol e2) '(C4 E4 G4)))))

;;; SAR Sat Dec 24 12:42:36 EST 2011
(sc-deftest test-event-no-accidental ()
  (let ((e (make-event 'cs4 'q)))
    (sc-test-check
      (show-accidental (pitch-or-chord e))
      (not (no-accidental e))
      (not (show-accidental (pitch-or-chord e))))))

;;; SAR Sat Dec 24 15:28:30 EST 2011
(sc-deftest test-event-is-chord ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event '(c4 e4 g4) 'q))
        (r (make-rest 'q)))
    (sc-test-check
      (not (is-chord e1))
      (is-chord e2)
      (= 3 (is-chord e2))
      (not (is-chord r)))))

;;; SAR Sat Dec 24 15:41:27 EST 2011
(sc-deftest test-event-is-single-pitch ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event '(c4 e4 g4) 'q))
        (r (make-rest 'q)))
    (sc-test-check
      (is-single-pitch e1)
      (not (is-single-pitch e2))
      (not (is-single-pitch r)))))

;;; SAR Sat Dec 24 16:27:14 EST 2011
(sc-deftest test-event-transpose ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event '(c4 e4 g4) 'q)))
    (sc-test-check
      (event-p (transpose e1 1))
      (event-p (transpose e2 1))
      (equalp (data (pitch-or-chord (transpose e1 1))) 'CS4)
      (equalp (data (pitch-or-chord e1)) 'C4)
      (transpose e1 1 :destructively t)
      (equalp (data (pitch-or-chord e1)) 'CS4)
      (transpose e1 0 :destructively t)
      (equalp (data (pitch-or-chord e1)) 'CS4)
      (transpose e1 -3 :destructively t)
      (equalp (data (pitch-or-chord e1)) 'BF3) ; is there a danger of this ever
                                        ; returning as AS3?  
      (transpose e2 -3 :destructively t) 
      (equalp (loop for p in (data (pitch-or-chord e2)) collect (data p))
              '(A3 CS4 E4)))))

;;; SAR Sat Dec 24 17:13:29 EST 2011
(sc-deftest test-event-set-written ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (written-pitch-or-chord e))
      (set-written e -2)
      (written-pitch-or-chord e)
      (pitch-p (written-pitch-or-chord e))
      (equalp (data (written-pitch-or-chord e)) 'BF3)
      ;; MDE Thu May 30 18:52:35 2013 -- now make sure we can set the written
      ;; pitch and have the sounding pitch auto-updated 
      (set-written-pitch-or-chord e 'fs4)
      (= 68 (midi-note (pitch-or-chord e)))
      ;; (setq e (make-event '(c4 'e4)
      ;; MDE Sun Dec 31 17:21:30 2017 -- make sure this works with chords too,
      ;; esp. after changing the transposition method to be destructive 
      (set-written-pitch-or-chord
       e '(c4 e4)
       ;; passing the intrument uses its transp. rather than an existing one
       ;; between written and sounding slots of the event
       (get-data 'alto-sax +slippery-chicken-standard-instrument-palette+))
      (equalp '(c4 e4) (get-pitch-symbols (written-pitch-or-chord e)))
      (equalp '(ef3 g3) (get-pitch-symbols (pitch-or-chord e)))
      ;; now without the ins
      (set-written-pitch-or-chord e '(gs5 g6))
      (equalp '(gs5 g6) (get-pitch-symbols (written-pitch-or-chord e)))
      (equalp '(b4 bf5) (get-pitch-symbols (pitch-or-chord e)))
      ;; MDE Wed Nov  7 18:40:57 2018
      (setq e (make-rest 'e))
      (set-pitch-or-chord e 'fs5)
      (not (is-rest e))
      (not (written-pitch-or-chord e))
      (set-pitch-or-chord e 'fs5 (get-standard-ins 'alto-flute))
      (eq 'b5 (data (written-pitch-or-chord e)))
      (set-pitch-or-chord e 'fs2 (get-standard-ins 'double-bass))
      (eq 'fs3 (data (written-pitch-or-chord e))))))

;;; SAR Sat Dec 24 17:28:04 EST 2011
(sc-deftest test-event-delete-written ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (written-pitch-or-chord e))
      (set-written e -2)
      (data (written-pitch-or-chord e))
      (pitch-p (written-pitch-or-chord e))
      (not (delete-written e))
      (not (written-pitch-or-chord e)))))

;;; SAR Sat Dec 24 17:37:59 EST 2011
(sc-deftest test-event-lowest ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event '(d4 fs4 a4) 'q)))
    (sc-test-check
      (pitch-p (lowest e1))
      (pitch-p (lowest e2))
      (equalp (data (lowest e1)) 'c4)
      (equalp (data (lowest e2)) 'd4))))

;;; SAR Sat Dec 24 19:12:44 EST 2011
(sc-deftest test-event-highest ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event '(d4 fs4 a4) 'q)))
    (sc-test-check
      (pitch-p (lowest e1))
      (pitch-p (lowest e2))
      (equalp (data (highest e1)) 'c4)
      (equalp (data (highest e2)) 'a4))))

;;; SAR Sat Dec 24 19:30:57 EST 2011
(sc-deftest test-event-event-distance ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event 'e4 'q))
        (e3 (make-event '(c4 e4 g4) 'q))
        (e4 (make-event '(d4 f4 a4) 'q)))
    (sc-test-check
      (= (event-distance e1 e2) 4.0)
      (= (event-distance e2 e1) -4.0)
      (= (event-distance e2 e1 t) 4.0)
      (= (event-distance e3 e4) 9.0)
      (= (event-distance e4 e3) -9.0))))

;;; SAR Sun Dec 25 08:34:18 EST 2011
(sc-deftest test-event-force-rest ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (equalp (data (pitch-or-chord e)) 'c4)
      (set-written e -2)
      (equalp (data (written-pitch-or-chord e)) 'bf3)
      (not (is-rest e))
      (event-p (force-rest e))
      (not (pitch-or-chord e))
      (not (written-pitch-or-chord e))
      (is-rest e))))

;;; SAR Sun Dec 25 09:11:15 EST 2011
(sc-deftest test-event-force-artificial-harmonic ()
  (let ((e (make-event 'c7 'q)))
    (sc-test-check
      (pitch-p (pitch-or-chord e))
      (equalp (data (pitch-or-chord e)) 'c7)
      (chord-p (force-artificial-harmonic e))
      (not (pitch-p (pitch-or-chord e)))
      (chord-p (pitch-or-chord e))
      (equalp (loop for p in (data (pitch-or-chord e)) collect (data p))
              '(C5 F5))
      (equalp (loop for p in (data (pitch-or-chord e)) collect (marks p))
              '(NIL (FLAG-HEAD))))))

;;; SAR Fri Dec 30 10:06:12 EST 2011
(sc-deftest test-event-add-arrow ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event nil 'q)))
    (sc-test-check
      ;; e1
      (not (marks-before e1))
      (not (marks e1))
      (add-arrow e1 "start here" "end here")
      (equalp (marks-before e1) '((ARROW "start here" "end here")))
      (equalp (marks e1) '(START-ARROW))
      ;; e2
      (not (marks-before e2))
      (not (marks e2))
      (is-rest e2)
      (add-arrow e2 "hier anfangen" "dort enden")
      (equalp (marks-before e2) '((ARROW "hier anfangen" "dort enden")))
      (equalp (marks e2) '(START-ARROW)))))

;; SAR Fri Dec 30 11:46:30 EST 2011
(sc-deftest test-event-add-trill ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event nil 'q)))
    (sc-test-check
      ;; e1
      (not (marks-before e1))
      (not (marks e1))
      (add-trill e1 'd4)
      (equalp (marks-before e1) '(BEG-TRILL-A))
      (equalp (marks e1) '((TRILL-NOTE D4)))
      ;; e2
      (not (marks-before e2))
      (not (marks e2))
      (add-trill e2 'd4 nil)
      (equalp (marks-before e2) '(BEG-TRILL-A))
      (equalp (marks e2) '((TRILL-NOTE D4))))))

;;; SAR Sat Dec 31 12:44:17 EST 2011
(sc-deftest test-event-get-dynamic ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (not (marks e))
      (not (get-dynamic e))
      (add-mark-once e 'ppp)
      (add-mark-once e 'pizz)
      (equalp (get-dynamic e) 'ppp)
      (not (rm-marks e 'ppp))
      (not (get-dynamic e)))))

;;; SAR Fri Mar  2 13:50:31 GMT 2012
(sc-deftest test-event-end-arrow ()
  (let ((e (make-event 'c4 'q)))
    (sc-test-check
      (equalp (marks-before e) nil)
      (equalp (marks e) nil)
      (end-arrow e)
      (equalp (marks-before e) nil)
      (equalp (marks e) '(end-arrow)))))

;;; MDE Thu Apr 19 12:12:35 2012 -- 
(sc-deftest test-event-get-degree ()
  (let ((event (make-event '(cs4 d4) 'e))
        (rest (make-rest 'e)))
    (sc-test-check
      (equalp '(122 124) (get-degree event))
      (zerop (get-degree rest :sum t))
      (= 123.0 (get-degree event :average t))
      (= 246 (get-degree event :sum t)))))

;;; SAR Sat Apr 28 22:52:15 BST 2012
(sc-deftest test-event-sort-event-list ()
  (let ((e-list (loop repeat 8
                   for nn in '(c4 d4 e4 f4 g4 a4 b4 c5)
                   for st in '(1.0 3.0 2.0 5.0 8.0 4.0 7.0 6.0)
                   collect (make-event nn 'e :start-time st))))
    (sc-test-check
      (equalp
       (loop for e in e-list 
          collect (get-pitch-symbol e)
          collect (start-time e))
       '(C4 1.0 D4 3.0 E4 2.0 F4 5.0 G4 8.0 A4 4.0 B4 7.0 C5 6.0))
      (sort-event-list e-list)
      (equalp
       (loop for e in e-list 
          collect (get-pitch-symbol e)
          collect (start-time e))
       '(C4 1.0 E4 2.0 D4 3.0 A4 4.0 F4 5.0 C5 6.0 B4 7.0 G4 8.0)))))

;;; SAR Sun Apr 29 14:33:41 BST 2012
(sc-deftest test-event-inc-duration ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vc (cello :midi-channel 1))))
           :set-palette '((1 ((gs3 as3 b3))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((vc (1)))))))
         (e1 (get-event mini 1 1 'vc))
         (e2 (get-event mini 1 2 'vc))
         (e3 (get-event mini 1 3 'vc)))
    (sc-test-check
      (equalp
       (loop for s in '(end-time duration-in-tempo compound-duration-in-tempo)  
          collect (funcall s e3))
       '(1.75 0.25 0.25))
      (inc-duration (get-event mini 1 3 'vc) 7.0)
      ;; MDE Tue May 13 09:39:15 2014 
      (event-equal e1 e1)
      (event-equal e2 e2)
      (not (event-equal e1 e2))
      (equalp
       (loop for s in '(end-time duration-in-tempo compound-duration-in-tempo) 
          collect (funcall s e3))
       '(8.75 7.25 7.25)))))

;;; SAR Wed May  2 14:16:09 BST 2012
(sc-deftest test-event-replace-mark ()
  (let ((e1 (make-event 'c4 'q))
        (e2 (make-event 'c4 'q)))
    (sc-test-check
      (not (marks e1))
      (not (marks-before e2))
      (not (loop for m in '(a s pizz) 
              do (add-mark e1 m)))
      (equalp (marks e1) '(PIZZ S A))
      (replace-mark e1 'a 'batt)
      (equalp (marks e1) '(PIZZ S BATT))
      (not (loop for m in '(arco col-legno) 
              do (add-mark-before e2 m)))
      (equalp (marks-before e2) '(COL-LEGNO ARCO))
      (replace-mark e2 'arco 'at t)
      (equalp (marks-before e2) '(COL-LEGNO AT)))))

;;; SAR Tue May 22 11:18:38 EDT 2012
(sc-deftest test-event-wrap-events-list ()
  (let ((e-list (loop for st from 1.0 by 0.5
                   for nn in '(c4 d4 e4 f4 g4 a4 b4 c5)
                   collect (make-event nn 'e :start-time st))))
    (equalp
     (loop for e in e-list
        collect (get-pitch-symbol e)
        collect (start-time e))
     '(C4 1.0 D4 1.5 E4 2.0 F4 2.5 G4 3.0 A4 3.5 B4 4.0 C5 4.5))
    (wrap-events-list e-list 3)
    (equalp
     (loop for e in e-list
        collect (get-pitch-symbol e)
        collect (start-time e))
     '(C4 3.5 D4 4.0 E4 4.5 F4 1.0 G4 1.5 A4 2.0 B4 2.5 C5 3.0))))

;;; SAR Tue Aug  7 16:32:08 BST 2012
(sc-deftest test-event-has-mark-before ()
  (let ((e (make-event 'c4 4)))
    (add-mark-before e 'ppp)
    (sc-test-check
      (has-mark-before e 'ppp)
      (not (has-mark-before e 'fff)))))

;;; MDE Wed Nov 28 18:44:25 2018
(sc-deftest test-event-voice-chord ()
  (let ((e (make-event '(c4 d4 e4 f4 g5 a4 b4) 4 :start-time 0.0)))
    (sc-test-check
      (event-p (voice-chord e '(50 60 70)))
      (= 50 (amplitude (get-nth 0 (pitch-or-chord e))))
      (= 60 (amplitude (get-nth 1 (pitch-or-chord e))))
      (= 70 (amplitude (get-nth 2 (pitch-or-chord e))))
      (= 50 (amplitude (get-nth 3 (pitch-or-chord e))))
      (event-p (voice-chord e #'(lambda (e)
                                  (loop for p in (get-pitch-symbol e) collect
                                       (case p
                                         (c4 1) (d4 2) (e4 3) (f4 4) (t 5))))))
      (= 1 (amplitude (get-nth 0 (pitch-or-chord e))))
      (= 2 (amplitude (get-nth 1 (pitch-or-chord e))))
      (= 3 (amplitude (get-nth 2 (pitch-or-chord e))))
      (= 4 (amplitude (get-nth 3 (pitch-or-chord e))))
      ;; make sure midi writing works too
      (= 1 (cm::midi-amplitude (second (first (last (output-midi e)))))))))

;;; MDE Mon Jan  8 16:09:59 2018 
(sc-deftest test-event-write-xml ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (e) g e+e. g 32 (32)))
                                  :pitch-seq-palette (((1) 3 5 2)))))
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (vc (1 1 1)))))))
        (s (make-string-output-stream)))
    (sc-test-check
      ;; this will write "/tmp/slippery-chicken-piece.xml". would be a good
      ;; idea to open this in Dorico/Finale/Sibelius and make sure the import of
      ;; grace note chords and single notes works
      (write-xml mini)
      (file-write-ok "/tmp/slippery-chicken-piece.xml" 15000)
      (write-xml (get-event mini 1  2 'vn) :stream s)
      (string= (get-output-stream-string s)
               "      <note> <!-- bar-pos=1 - - - - - - - - - - - - -->
        <grace />
        <pitch>
          <step>C</step>
          <octave>4</octave>
        </pitch>
        <type>eighth</type>
        <stem default-y=\"3\">up</stem>
      </note>
      <note>
        <grace />
        <chord />
        <pitch>
          <step>A</step>
          <octave>4</octave>
        </pitch>
        <type>eighth</type>
        <stem default-y=\"3\">up</stem>
      </note>"))))

;;; MDE Tue Jul 16 13:10:42 2019 
(sc-deftest test-event-common-notes ()
  (let ((e1 (make-event '(c3 cs4 d5) 'q))
        (e2 (make-event '(c3 cs4 ds5) 'q))
        (e3 (make-event 'c3 'q))
        (e4 (make-event 'c3 'q))
        (e5 (make-event 'cs3 'q))
        (e6 (make-event 'bs2 'q))
        (e7 (make-event 'c5 'q))
        (r (make-rest 'e))
        )
    (sc-test-check
      (= 2 (common-notes e1 e2))
      (nth-value 1 (common-notes e1 e2))
      (equalp '(cs4 c3) (nth-value 2 (common-notes e1 e2)))
      (zerop (common-notes e1 r))
      (zerop (common-notes e2 r))
      (zerop (common-notes e3 e5))
      (not (nth-value 1 (common-notes e3 e5)))
      (= 1 (common-notes e4 e6 t))
      (pitch= (make-pitch 'c3) (first (nth-value 1 (common-notes e4 e6 t))))
      (= 1 (common-notes e1 e7 nil t))
      (= 1 (common-notes e1 e3))
      (= 1 (common-notes e4 e3))
      (= 1 (common-notes e3 e1)))))

(sc-deftest test-event-add-pcs ()
  (let ((e (make-event 'c4 'e)))
    (sc-test-check
      ;;                   channel | change
      (add-midi-program-changes e 1 2)
      ;; check it worked
      (equalp '(1 2) (first (midi-program-changes e)))
      ;; check they're returned; remember existing changes remain
      (equalp '((12 5) (11 5) (1 2))
              (add-midi-program-changes e '(11 12) 5))
      (equalp '(4 66) ; prog change 66 is sax
              (first (add-midi-program-changes e 4 'alto-sax)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rthm-seq tests

;;; MDE Sat Jun  6 15:49:53 2020, Heidhausen
(sc-deftest test-make-rthm-seq-from-unit-multipliers-simp ()
  (let ((rs1 (make-rthm-seq-from-unit-multipliers-simp 1 'e
                                                       '((3 4 1 2) (2 1 2)))))
    (sc-test-check
      (time-sig-equal (first (bars rs1)) '(10 8))
      (eq 'h (data (get-nth-event 1 (first (bars rs1))))))))

;;; MDE Fri Apr 19 14:52:44 2013 -- make sure we can make a seq with a list of
;;; rthm-seq-bars  
(sc-deftest test-make-rthm-seq-with-rsbs ()
  (let ((rs (make-rthm-seq 
             (loop repeat 100 collect
                  (make-rthm-seq-bar 
                   (cons '(5 4) (loop repeat 5 collect
                                     (make-event 'c4 'q))))))))
    (sc-test-check
      (= 100 (num-bars rs))
      (= 500 (num-rhythms rs))
      (= 500 (num-notes rs))
      (zerop (num-rests rs)))))

;;; MDE Mon Dec 12 09:04:01 2011 -- rthm-seq tests
(sc-deftest test-rthm-seq ()
  (let* ((rs (make-rthm-seq '(x1 ((((2 4) 32+32+32+32+32+32+32 32 
                                    32+32+32+32+32+32+32 32))))))
         (b1 (first (bars rs))))
    (sc-test-check
      (= 1 (num-bars rs))
      (= 4 (num-notes rs))
      (eq 'x1 (rsp-id b1))
      (= 16 (num-score-notes rs))
      ( = 0.125 (duration (first (rhythms b1)))))))

(sc-deftest test-rthm-seq-marks ()
  (let ((rs (make-rthm-seq 
             '((((2 4) (s) e. 32 (32) (s) (e))
                ((e) q.)
                (+q { 3 - +te +te - (te) })
                ((q) (s) e.))
               :pitch-seq-palette (1 1 1 1)
               ;; so (s 1 3) means staccato from notes 1 to 3 incl. (counting
               ;; all notes, incl. ties, but not rests)  whereas (a 1 3 7)
               ;; means an accent on those notes only
               ;; if you want an accent on notes 1 and 4, you have to do (a 1)
               ;; (a 4) 
               :marks (s 1 3 te 4 6 a 1 3 7)))))
    (sc-test-check
      ;; MDE Mon Sep 30 17:31:45 2013 -- we now have a setf method: make sure
      ;; it doesn't trigger an error 
      (setf (pitch-seq-palette rs) '(1 1 2 1))
      (member 'a (marks (third (rhythms (fourth (bars rs))))))
      (not (member 'a (marks (first (rhythms (fourth (bars rs)))))))
      (member 's (marks (second (rhythms (second (bars rs))))))
      (setf (marks rs) '(as 1 4 6))
      ;; so now the old marks should be deleted and the new ones added
      (equalp '((as 1 4 6)) (marks rs))
      (not (member 'a (marks (third (rhythms (fourth (bars rs)))))))
      (member 'as (marks (third (rhythms (third (bars rs)))))))))

(sc-deftest test-rthm-seq-add-bar ()
  ;;                          notes needed 8, 9 score-notes, 11 rhythms, 2 rests
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
    ;;                                 3 notes needed, 5 score notes
    (add-bar rs (make-rthm-seq-bar '((5 8) e e+32 s. +q)))
    (sc-test-check
      (= 4 (num-bars rs))
      (= 4 (length (bars rs)))
      (= 11 (num-notes (pitch-seq-palette rs)))
      (= 1 (num-data (pitch-seq-palette rs)))
      (equalp '(1 2 3 1 1 2 3 4 3 4 3)
              (data (first (data (pitch-seq-palette rs))))))))

;;; SAR Tue Dec 27 15:09:59 EST 2011
(sc-deftest test-rthm-seq-make-rthm-seq ()
  (let ((rs (make-rthm-seq '(seq1 ((((2 4) q e s s)
                                    ((e) q (e))
                                    ((3 8) (e) e. s))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7)
                                                       (8 9 8 7 6 5 4)))))))
    (sc-test-check
      (rthm-seq-p rs)
      (= (num-bars rs) 3)
      (= (num-rhythms rs) 10)
      (= (num-notes rs) 7)
      (= (num-rests rs) 3)
      (= (duration rs) 5.5)
      (equalp (loop for b in (bars rs)
                 collect (loop for r in (rhythms b)
                            collect (data r)))
              '((Q E S S) (E Q E) (E E. S)))
      (equalp (loop for ps in (data (pitch-seq-palette rs))
                 collect (data ps))
              '((1 2 3 4 5 6 7) (8 9 8 7 6 5 4))))))

;;; SAR Tue Dec 27 19:30:12 EST 2011
(sc-deftest test-rthm-seq-make-rhythms ()
  (let ((rs1 (make-rhythms '(q e s s) '(2 4)))
        (rs2 (make-rhythms '(q e s s) '(2 4) t))
        (rs3 (make-rhythms '(q - e s s -) '(2 4)))
        (rs4 (make-rhythms '( { 3 te te te } - e s s -) '(2 4))))
    (sc-test-check
      (listp rs1)
      (listp rs2)
      (listp rs3)
      (listp rs4)
      (= (length rs1) 4)
      (= (length rs2) 2)
      (equal (loop for b in rs2 collect (length b)) '(1 3)) 
      (equal (loop for b in rs2 collect (loop for r in b collect (data r))) 
             '((Q) (E S S)))
      (equal (loop for r in rs3 collect (beam r)) '(NIL 1 NIL 0))
      (equal (loop for r in rs4 collect (bracket r))
             '(((1 3)) (-1) (1) NIL NIL NIL)))))

;;; SAR Tue Dec 27 20:10:21 EST 2011
(sc-deftest test-rthm-seq-get-nth-non-rest-rhythm ()
  (let ((rs (make-rthm-seq '((((2 4) q e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 4 1 1 2 3 4))))))
    (sc-test-check
      (rthm-seq-p rs)
      (rhythm-p (get-nth-non-rest-rhythm 4 rs))
      (equalp (data (get-nth-non-rest-rhythm 4 rs)) 'Q)
      (not (get-nth-non-rest-rhythm 11 rs nil)))))

;;; SAR Tue Dec 27 20:25:08 EST 2011
(sc-deftest test-rthm-seq-get-nth-attack ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
    (sc-test-check
      (rthm-seq-p rs)
      (rhythm-p (get-nth-attack 4 rs))
      (equalp (data (get-nth-attack 4 rs)) 'S)
      (not (get-nth-non-rest-rhythm 11 rs nil)))))

;;; SAR Tue Dec 27 21:06:46 EST 2011
(sc-deftest test-rthm-seq-set-nth-attack ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
    (sc-test-check
      (rthm-seq-p rs)
      (event-p (set-nth-attack 2 (make-event 'c4 'q) rs))
      (equalp (loop for b in (bars rs) 
                 collect (loop for r in (rhythms b) collect (data r)))
              '(("Q" "E" S Q) (E Q E) (S S E. S)))
      (not (set-nth-attack 11 (make-event 'c4 'q) rs nil)))))

;;; SAR Tue Dec 27 21:43:36 EST 2011
(sc-deftest test-rthm-seq-set-nth-bar ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
    (sc-test-check
      (rthm-seq-p rs)
      (rthm-seq-bar-p 
       (set-nth-bar 1 (make-rthm-seq-bar '((2 4) (s) e (s) q)) rs)) 
      (equalp (loop for b in (bars rs)
                 collect (loop for r in (rhythms b) collect (data r)))
              '(("Q" "E" S S) (S E S Q) (S S E. S))))))

;;; SAR Wed Dec 28 10:00:53 EST 2011
(sc-deftest test-rthm-seq-get-nth-bar ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
    (sc-test-check
      (rthm-seq-bar-p (get-nth-bar 1 rs))
      (equalp (data (get-nth-bar 1 rs)) '((E) Q (E)))
      (not (get-nth-bar 11 rs)))))

;;; SAR Wed Dec 28 10:07:30 EST 2011
(sc-deftest test-rthm-seq-get-last-bar ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
    (sc-test-check
      (rthm-seq-bar-p (get-last-bar rs))
      (equalp (data (get-last-bar rs)) '((3 8) S S E. S)))))

;;; SAR Wed Dec 28 10:24:12 EST 2011
(sc-deftest test-rthm-seq-get-last-attack ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
    (sc-test-check
      (rhythm-p (get-last-attack rs))
      (equalp (data (get-last-attack rs)) 'S))))

;;; SAR Wed Dec 28 10:36:55 EST 2011
(sc-deftest test-rthm-seq-get-last-event ()
  (let ((rs1 (make-rthm-seq '((((2 4) q+e s s)
                               ((e) q (e))
                               ((3 8) s s e. s))
                              :pitch-seq-palette ((1 2 3 4 1 1 2 3)))))
        (rs2 (make-rthm-seq `((((2 4) q+e s s)
                               ((e) q (e))
                               ((3 8) s s e. ,(make-event 'c4 's)))
                              :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
    (sc-test-check
      (rhythm-p (get-last-event rs1))
      (event-p (get-last-event rs2))
      (equalp (data (get-last-event rs1)) 'S)
      (equalp (get-pitch-symbol (get-last-event rs2)) 'C4)
      (equalp (data (get-last-event rs2)) 'S))))

;;; SAR Wed Dec 28 13:06:39 EST 2011
(sc-deftest test-rthm-seq-insert-bar ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
    (sc-test-check
      (= (num-bars rs) 3)
      (insert-bar rs (make-rthm-seq-bar '((3 4) q. e e s s)) 3)
      (= (num-bars rs) 4)
      (equalp (data (get-nth-bar 2 rs)) '((3 4) q. e e s s))
      (insert-bar rs (make-rthm-seq-bar '((3 4) q. e e s s)) 3 '((1 2 3 4 5)))
      (equalp (loop for ps in (data (pitch-seq-palette rs)) collect (data ps))
              '((1 2 3 1 1 2 3 4 5 1 1 1 1 1 1 2 3 4))))))

;;; SAR Sat Jan 14 19:26:04 GMT 2012: Optizing via EVERY
;;; SAR Wed Dec 28 14:06:00 EST 2011
(sc-deftest test-rthm-seq-get-time-sigs ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
    (sc-test-check
      (listp (get-time-sigs rs))
      (every #'time-sig-p (get-time-sigs rs))
      (equalp (get-time-sigs rs t) '((2 4) (2 4) (3 8))))))

;;; SAR Wed Dec 28 14:34:41 EST 2011
(sc-deftest test-rthm-seq-combine ()
  (let* ((rs1 (make-rthm-seq '((((2 4) q - +e s s -)
                               ((e) q (e))
                               ((3 8) s s e. s))
                              :pitch-seq-palette ((1 2 3 1 1 2 3 4)))))
        (rs2 (make-rthm-seq '((((4 4) h+e (e) { 3 te te te })
                               ((5 8) e e+32 s. +q)
                               ((3 4) (q) q q))
                              :pitch-seq-palette ((1 2 3 4 1 2 3 1 2)))))
        (clone (clone rs1)))
    (sc-test-check
      (rthm-seq-p (combine rs1 rs2))
      (= (num-bars (combine rs1 rs2)) 6)
      (= (num-rhythms (combine rs1 rs2)) 25)
      (= (num-notes (combine rs1 rs2)) 17)
      (equalp (loop for b in (bars (combine rs1 rs2)) collect (data b))
              '(((2 4) Q - +E S S -) ((E) Q (E)) ((3 8) S S E. S)
                ((4 4) H+E (E) { 3 TE TE TE }) ((5 8) E E+32 S. +Q) 
                ((3 4) (Q) Q Q)))
      (equalp (loop for ps in (data (pitch-seq-palette (combine rs1 rs2)))
                 collect (data ps))
              '((1 2 3 1 1 2 3 4 1 2 3 4 1 2 3 1 2)))
      ;; MDE Wed Sep  4 13:03:26 2013 
      (= 1 (rq (get-nth-rhythm 0 clone)))
      (eq (data (get-nth-rhythm 5 clone)) 'q)
      (eq (data (get-nth-rhythm 9 clone)) 'e.)
      (setf (is-tied-to (get-nth-rhythm 0 clone)) t
            (is-tied-from (get-nth-rhythm 10 clone)) t)
      ;; (print (bars clone))
      ;; (print 'before)
      (clear-ties-beg-end clone) ; -> T
      (check-beams (first (bars clone)))
      (not (clear-ties-beg-end clone)) ; -> NIL, because nothing was cleared
      (is-rest (get-nth-rhythm 0 clone))
      (not (is-tied-from (get-nth-rhythm 10 clone))))))

;;; SAR Wed Dec 28 16:45:31 EST 2011
(sc-deftest test-rthm-seq-get-rhythms ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
    (sc-test-check
      (listp (get-rhythms rs))
      (= (length (get-rhythms rs)) (num-rhythms rs))
      (equalp (loop for r in (get-rhythms rs) collect (data r))
              '("Q" "E" S S E Q E S S E. S)))))

;;; SAR Wed Dec 28 20:31:10 EST 2011
(sc-deftest test-rthm-seq-split ()
  (let* ((rs (make-rthm-seq '((((4 4) q e s s (e) e e (e))
                               ((3 4) s s e s e s e. s)
                               ((5 4) h q. e e s s))
                              :pitch-seq-palette ((1 2 3 4 5 6 1 2 3 4 5 6 7 8 
                                                   1 2 3 4 5 6)))))
         ;; MDE Mon Sep 30 17:04:14 2013 
         (subseq (rs-subseq rs 2))
         (rssp1 (split rs))
         (rssp2 (split rs :min-beats 4))
         (rssp3 (split rs :max-beats 2)))
    (sc-test-check
      ;; MDE Mon Sep 30 17:04:45 2013 -- 
      (= 2 (num-bars subseq))
      (= 14 (num-notes subseq))
      (equalp (data (first (data (pitch-seq-palette subseq))))
              '(1 2 3 4 5 6 7 8 1 2 3 4 5 6))
      (setf subseq (rs-subseq rs 1 2))
      (= 14 (num-notes subseq))
      (= 2 (num-bars subseq))
      (equalp (data (first (data (pitch-seq-palette subseq))))
              '(1 2 3 4 5 6 1 2 3 4 5 6 7 8))
      (setf subseq (rs-subseq rs 2 2))
      (= 8 (num-notes subseq))
      (= 1 (num-bars subseq))
      (rthm-seq-p rssp1)
      (rthm-seq-p rssp2)
      (rthm-seq-p rssp3)
      (= (num-bars rssp1) 5)
      (= (num-bars rssp2) 3)
      (= (num-bars rssp3) 5)
      (equalp (loop for b in (bars rssp1) 
                 collect (loop for r in (rhythms b) collect (data r)))
              '((Q E S S) (E E E E) (S S E S E S E. S) (H) (Q. E E S S)))
      (equalp (loop for b in (bars rssp2) 
                 collect (loop for r in (rhythms b) collect (data r)))
              '((Q E S S E E E E) (S S E S E S E. S) (H Q. E E S S)))
      (equalp (loop for b in (bars rssp3) 
                 collect (loop for r in (rhythms b) collect (data r)))
              '((Q E S S) (E E E E) (S S E S E S E. S) (H) (Q. E E S S))))))  

;;; MDE Sat Jun  9 15:48:47 2012
(sc-deftest test-split-beams ()
  (let* ((bars (bars
                (split
                 (make-rthm-seq '((((8 8) - s x 4 - - s x 4 - - s x 4 -
                                    - s x 4 -))))
                 :min-beats 1))))
    (sc-test-check
      (= 8 (length bars))
      (equalp (ml t 8)
              (loop for bar in bars collect (check-beams bar))))))


;;; SAR Wed Dec 28 21:29:22 EST 2011
(sc-deftest test-rthm-seq-scale ()
  (let* ((rs (make-rthm-seq '((((2 4) q+e s s)
                               ((e) q (e))
                               ((3 8) s s e. s))
                              :pitch-seq-palette ((1 2 3 1 1 2 3 4)))))
         (rss (scale rs 3)))
    (sc-test-check
      (rthm-seq-p rss)
      (equalp (loop for b in (bars rss)
                 collect (loop for r in (rhythms b) collect (data r)))
              '((H. Q. E. E.) (Q. H. Q.) (E. E. E. E.))))))

;;; SAR Fri Dec 30 13:22:10 EST 2011
(sc-deftest test-rthm-seq-setf-marks ()
  (let ((rs (make-rthm-seq '((((2 4) q+e s s)
                              ((e) q (e))
                              ((3 8) s s e. s))
                             :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
    (sc-test-check
      (not (marks rs))
      (setf (marks rs) '(a 1 3 5 7))
      (equalp (marks rs) '((a 1 3 5 7))))))

;;; SAR Thu Dec 29 14:43:33 EST 2011
(sc-deftest test-rthm-seq-make-rthm-seq-from-unit-multipliers ()
  (let ((rs1 (make-rthm-seq-from-unit-multipliers 's '(4 2 2 4 4) '(2 4)))
        (rs2 (make-rthm-seq-from-unit-multipliers 32 '(7 9 16) '(4 4))))
    (sc-test-check
      (rthm-seq-p rs1)
      (rthm-seq-p rs2)
      (= (num-bars rs1) 2)
      (= (num-bars rs2) 1)
      (equalp (loop for b in (bars rs1) 
                 collect (loop for r in (rhythms b) collect (data r)))
              '((Q E E) (Q Q)))
      (equalp (loop for b in (bars rs2) 
                 collect (loop for r in (rhythms b) collect (data r)))
              '((E.. 32 Q H)))
      (equalp (loop for b in (bars rs2)
                 collect (loop for r in (rhythms b) collect (is-tied-from r)) 
                 collect (loop for r in (rhythms b) collect (is-tied-to r)))
              '((NIL T NIL NIL) (NIL NIL T NIL))))))

;;; SAR Tue Jan 17 22:17:26 GMT 2012
(sc-deftest test-rthm-seq-chop ()
  (let* ((rs (make-rthm-seq 
              '(seq1 ((((2 4) q e s s)
                       ((e) q (e))
                       (s s (e) e. s))
                      :pitch-seq-palette ((1 2 3 4 5 6 7 8 9)
                                          (9 8 7 6 5 4 3 2 1))))))  
         (ch (chop rs
                   '((1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) 
                     (4 4)) 
                   's)))
    (sc-test-check
      (listp ch)
      (every #'rthm-seq-p ch)
      (equalp (loop for rs-obj in ch 
                 collect (loop for e in (get-rhythms rs-obj)
                            collect (data e)
                            collect (is-rest e)))
              '((S NIL) (E NIL) (E. NIL) (Q NIL) (16 T) (8 T) (16/3 T) (16 T)
                (8 T) (16 T) (S NIL) (E NIL) (E NIL S NIL) (E NIL S NIL S NIL)
                (16 T) (S T S NIL) (S T S NIL S NIL) (S NIL) (S NIL S NIL) 
                (S NIL) (16 T) (8 T) (E T S NIL) (E T E NIL) (16 T) (S T S NIL)
                (S T E NIL) (S NIL) (E NIL) (16 T) (16 T) (8 T) (16/3 T) (4 T)
                (16 T) (8 T) (16/3 T) (16 T) (8 T) (16 T) (S NIL) (S NIL S NIL) 
                (S NIL S NIL S T) (S NIL S NIL E T) (S NIL) (S NIL S T) 
                (S NIL E T) (16 T) (8 T) (16 T) (S NIL) (E NIL) (E. NIL)
                (E. NIL S NIL) (16 T) (8 T) (E T S NIL) (16 T) (S T S NIL) 
                (S NIL))))))

;;; SAR Tue Jan 31 13:33:52 GMT 2012
(sc-deftest test-rthm-seq-get-multipliers ()
  (let ((rs (make-rthm-seq '(seq1 ((((2 4) q e s s))
                                   :pitch-seq-palette ((1 2 3 4))))))) 
    (sc-test-check
      (equalp (get-multipliers rs 'e)
              '(2.0 1.0 0.5 0.5))
      (equalp (get-multipliers rs 'e t)
              '(2 1 0 0)))))

;;; SAR Sun Apr 29 14:52:46 BST 2012
(sc-deftest test-rthm-seq-make-rthm-seq-with-psp-inversions ()
  (let ((mrs-1
         (make-rthm-seq '(seq1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))))))
        (mrs-2
         (make-rthm-seq '(seq1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))))
                        :psp-inversions t)))
    (sc-test-check
      (equalp 
       (data (first (data (pitch-seq-palette mrs-1))))
       '(1 2 3 4))
      (equalp
       (loop for ps in (data (pitch-seq-palette mrs-2)) collect (data ps))
       '((1 2 3 4) (4 3 2 1))))))

;;; SAR Sun Apr 29 15:12:03 BST 2012
(sc-deftest test-rthm-seq-make-rthm-seq-from-multipliers-with-tuplets ()
  (let ((rs (make-rthm-seq-from-unit-multipliers 'te '(7 9 16) '(4 4)
                                                 :tuplet 3)))
    (sc-test-check
      (print-simple rs)
      (equalp
       (loop for b in (bars rs)
          collect (loop for r in (rhythms b) collect (bracket r)))
       '((NIL NIL ((1 3)) (1) NIL) (NIL ((1 3)) (1) NIL NIL)
         (NIL NIL ((1 3)) (1) NIL))))))


;;; SAR Wed May  2 19:27:54 BST 2012
;;; A very cursory test of core function
(sc-deftest test-rthm-seq-get-bar ()
  (let ((rs (make-rthm-seq '(seq1 ((((2 4) q e s s)
                                    ((e) q (e))
                                    ((3 8) s s e. s)))))))
    (sc-test-check
      (rthm-seq-bar-p (get-bar rs 3))
      (equalp (get-rhythm-symbols (get-bar rs 3)) '(S S E. S)))))

;;; MDE Mon May 14 12:56:04 2012  -- from a preliminary version of altogether
;;; disproportionate 
(sc-deftest test-rthm-seq-from-frags ()
  (let* ((mults '(5 4 6 3 7 2 8 1))
         (mults54 '(4 5 5 4 4 5 5 4))
         (multsr (reverse mults))
         (swapped (swap-elements mults))
         (swappedr (reverse swapped))       
         temp
         (rsp 
          (make-rsp 
           'altogether 
           ;; the *-cp rthm-seqs are counterpoints to the main rthm-seqs
           '((m-cp ((((2 4) q. e) (+q q) ((3 4) +q q+e e) ((2 4) +e q.))))
             (mr-cp ((((2 4) q. e) (+q q) ((3 4) +q q+e e) ((2 4) +q q))))
             (s-cp ((((2 4) q. e) (+q+q) ((3 4) q+e e+q) ((2 4) +e q.))))
             (sr-cp ((((2 4) { 3 tq tq tq }) ({ 3 (tq) tq tq }) 
                      ((3 4) { 3 (tq) te+te (tq) } { 3 tq te }) 
                      ((2 4) +te tq (q)))))
             (m54-cp ((((2 4) q. e) ((q) q) ((3 4) (q) q+e e) ((2 4) (e) q.))))
             (c1-cp1 ((((3 4) (e) e (e) e e (e)) ((2 4) (e) e (e) e)
                       (e (e) (e) e) (q e e))))
             (c1-cp2 ((((3 4) (e) e (q) e (e)) ((2 4) (e) e (q))
                       (e (e) (e) e) ((q) e e))))
             (c2-cp ((((2 4) (e) e (q)) (q (e) e) ((q) q) ((3 4) (e) e (q) q))))
             (c3-cp ((((2 4) { 3 tq tq tq }) ((3 4) (q) { 3 tq te+te tq })
                      ((2 4) (q) { 3 tq te }) ({ 3 (te) tq } (q))))) )))
         (brutal '((1 (- s s - (e))) (2 (s (s) (s) s)) (3 ((s) - s e -))
                   (4 (- s s (s) s -)) (5 ((q.))) (6 ((e) - s s -)) (7 ((q))) 
                   ;; not using 5,8,9 but am using variants of 2, 3, 4:
                   (2b ((e.) s)) (4b ((s) s (s) s)) (3b ((s) s (e)))
                   (8 (e (e))) (9 ((e) s (s)))))
         (brutal-rthm-seqs 
          ;; so c1 combines brutal fragment 1, 2, and 3 into the first bar, 1
          ;; and 4 into the second bar etc. of a newly created rthm-seq
          '((c1 ((1 2 3) (1 4) (7 1) (6 7)))           ; 15 notes
            (c1-least ((1 7 3) ( 7 7) (7 1) (7 7)))    ; 6 notes
            (c1-less ((1 2 7) (7 4) (7 7) (6 7)))      ; 9 notes
            (c2 ((2 3) (2 3) (7 7) (1 2 3)))           ; 14 notes
            (c2-least ((7 3) (2b 3) (7 7) (7 2b 7)))   ; 6 notes
            (c2-less ((2b 3) (7 3) (7 7) (7 2b 3)))    ; 8 notes
            (c3 ((2 3) (2 3 7) (7 2) (3 4)))           ; 15 notes
            (c3-least ((7 7) (2b 3b 7) (7 8) (7 4b)))  ; 5 notes
            (c3-less ((7 7) (2 3 7) (7 2) (3b 2b)))))) ; 8 notes
    (flet ((rspaddmult (nums id rthm meter &optional (auto-beam t) tuplet)
             (add
              (make-rthm-seq-from-unit-multipliers
               rthm nums meter :auto-beam auto-beam :tuplet tuplet :id id)
              rsp))
           (scale-mults (nums scaler)
             (loop for m in nums collect (* scaler m))))
      (loop for brs in brutal-rthm-seqs do
           (add
            (make-rthm-seq-from-fragments
             (first brs) brutal (second brs) 
             ;; in this case we can tell the time signature from the length of
             ;; the bar list as each brutal fragment is a 1/4 long 
             (loop for bar in (second brs) collect (length bar)))
            rsp))
      (loop repeat (sclist-length rsp) 
         for rs = (get-next rsp)
         for rsid = (id rs)
         for mults = (get-multipliers rs 's)
         for mwrap1 = (wrap-list mults 1)
         for mwrap2 = (wrap-list mults 2)
         do
         (when (or (equal rsid 'c1) (equal rsid 'c2) (equal rsid 'c3))
           ;; can't foresee the best metrical structure so just use 3/4
           ;;(rspaddmult mwrap1 (format nil "~a~a" (id rs) "wrap1") 's '(1 4))
           (rspaddmult mwrap1 (combine-into-symbol (id rs) 'wrap1) 's '(1 4))
           (rspaddmult mwrap2 (combine-into-symbol (id rs) 'wrap2) 's '(1 4))))
      (sc-test-check
        (rspaddmult mults54 'm54 's '(1 4))
        (rspaddmult (wrap-list mults54 1) 'm54wrap1 's '(1 4))
        (rspaddmult (wrap-list mults54 2) 'm54wrap2 's '(1 4))
        (rspaddmult (wrap-list mults 1) 'mwrap1 's '(1 4))
        (rspaddmult (wrap-list multsr 1) 'mrwrap1 's '(1 4))
        (rspaddmult mults 'm 's '(1 4))
        (rspaddmult multsr 'mr 's '(1 4))
        (rspaddmult swapped 's 's '(1 4))
        (rspaddmult swappedr 'sr 's '(1 4))
        (setf temp (get-data 'c1 rsp))
        (rthm-seq-p temp)
        (= 4 (num-bars temp))
        (= 6 (notes-needed (first (bars temp))))
        (= 15 (num-notes temp))))))

;;; SAR Wed Aug  8 11:34:06 BST 2012
(sc-deftest test-rthm-seq-delete-marks ()
  (let ((mrs (make-rthm-seq '(seq1 ((((2 4) q e (s) s))
                                    :pitch-seq-palette ((1 2 3))
                                    :marks (ff 1 a 1 pizz 1 ppp 2 s 2))))))
    (sc-test-check
      (equalp (marks mrs) '((FF 1) (A 1) (PIZZ 1) (PPP 2) (S 2)))
      (not (delete-marks mrs))
      (not (marks mrs)))))

;;; SAR Wed Aug  8 11:41:04 BST 2012
(sc-deftest test-rthm-seq-get-first ()
  (let ((mrs (make-rthm-seq '(seq1 ((((2 4) q e (s) s))
                                    :pitch-seq-palette ((1 2 3))
                                    :marks (ff 1 a 1 pizz 1 ppp 2 s 2))))))
    (sc-test-check
      (rhythm-p (get-first mrs))
      (equalp 'Q (data (get-first mrs))))))

;;; SAR Wed Aug  8 11:44:30 BST 2012
(sc-deftest test-rthm-seq-get-last ()
  (let ((mrs (make-rthm-seq '(seq1 ((((2 4) q e (s) s))
                                    :pitch-seq-palette ((1 2 3))
                                    :marks (ff 1 a 1 pizz 1 ppp 2 s 2))))))
    (sc-test-check
      (rhythm-p (get-last mrs))
      (equalp 'S (data (get-last mrs))))))

;;; MDE Sun Nov 18 19:34:28 2012 
(sc-deftest test-rthm-seq-tidy-ties ()
  (let* ((rs (make-rthm-seq '(seq1 ((((2 4) q e (s) s))))))
         (rthms (rhythms (first (bars rs)))))
    ;; this will issue a warning, which is OK
    (setf (is-tied-from (third rthms)) t
          (is-tied-to (fourth rthms)) t)
    (tidy-ties rs)
    (sc-test-check
      (not (is-tied-from (third rthms)))
      (not (is-tied-from (fourth rthms))))))

;;; MDE Mon Jun 10 19:32:03 2013 
(sc-deftest test-is-rest-seq ()
  (let* ((rs1 (make-rthm-seq '(seq1 ((((2 4) (h)) ((h)))))))
         (rs2 (make-rthm-seq '(seq1 ((((2 4) (h)) ((q) (q)))))))
         (rs3 (make-rthm-seq '(seq1 ((((2 4) q e (s) s)))))))
    (sc-test-check
      (is-rest-seq rs1)
      (is-rest-seq rs2)
      (not (is-rest-seq rs3))
      (force-rest-seq rs3)
      (= 1 (num-rests rs3))
      (zerop (num-notes rs3))
      (is-rest-seq rs3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tempo tests

;;; MDE Mon Dec 12 09:45:06 2011 -- tempo stuff
(sc-deftest test-tempo ()
  (let ((tc (make-tempo-map
             'tc
             (tempo-curve-to-map nil '(10 q (0 60 100 120)) 500)))
        (tm (make-tempo-map 'tm '((1 44) (4 66) (7 (h 60))))))
    (sc-test-check
      (tempo-map-p tc)
      (tempo-map-p tm)
      (tempo-p (get-data-data 1 tm))
      (= 44 (bpm (get-data-data 1 tm))))))

;;; SAR Thu Dec 29 19:58:22 EST 2011
(sc-deftest test-tempo-make-tempo ()
  (let ((tt1 (make-tempo 60))
        (tt2 (make-tempo 96 :beat 'q.))
        (tt3 (make-tempo 76 :beat 2 :description "Allegretto")))
    (sc-test-check
      (tempo-p tt1)
      (tempo-p tt2)
      (tempo-p tt3)
      (= (bpm tt1) 60)
      (= (bpm tt2) 96)
      (= (bpm tt3) 76)
      ;; MDE Mon May  5 17:25:27 2014 
      (equal-within-tolerance 0.7894737 (beat-dur tt3))
      (equal-within-tolerance 0.39473686 (qtr-dur tt3))
      (= (beat tt1) 4)
      (equalp (beat tt2) 'q.)
      (= (beat tt3) 2)
      (= (qtr-dur tt1) 1.0)
      ;; MDE Sat Nov 27 10:54:56 2021, Heidhausen
      (search "<beat-unit-dot></beat-unit-dot>"
              (write-xml tt2))
      (not (search "<beat-unit-dot></<beat-unit-dot>"
                   (write-xml tt3)))
      ;; MDE Mon Mar 19 18:10:55 2012 -- 
      (equal-within-tolerance (qtr-dur tt2) (* (/ 60.0 96) (/ 1.0 1.5)))
      (equal-within-tolerance (qtr-dur tt3) (/ (/ 60 76) 2.0))
      (not (description tt1))
      (not (description tt2))
      (equalp (description tt3) "Allegretto"))))

;;; SAR Thu Dec 29 20:17:12 EST 2011
(sc-deftest test-tempo-tempo-equal ()
  (let ((tt1 (make-tempo 60))
        (tt2 (make-tempo 60))
        (tt3 (make-tempo 96)))
    (sc-test-check
      (tempo-equal tt1 tt2)
      (not (tempo-equal tt1 tt3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; time-sig tests

;;; SAR Thu Dec 29 20:34:36 EST 2011
(sc-deftest test-time-sig-make-time-sig ()
  (let ((ts1 (make-time-sig '(2 4)))
        (ts2 (make-time-sig '(6 8))))
    (sc-test-check
      (time-sig-p ts1)
      (= (num ts1) 2)
      (= (denom ts1) 4)
      (= (duration ts1) 2.0)
      (not (compound ts1))
      (equalp (data ts1) '(2 4))
      (time-sig-p ts2)
      (= (num ts2) 6)
      (= (denom ts2) 8)
      (= (duration ts2) 3.0)
      (compound ts2)
      (equalp (data ts2) '(6 8)))))

;;; SAR Thu Dec 29 20:42:00 EST 2011
(sc-deftest test-time-sig-beat-duration ()
  (let ((ts1 (make-time-sig '(2 4)))
        (ts2 (make-time-sig '(6 8))))
    (sc-test-check
      (= (beat-duration ts1) 1.0)
      (= (beat-duration ts2) 1.5))))

;;; SAR Fri Dec 30 17:34:54 EST 2011
(sc-deftest test-time-sig-get-beat-as-rhythm ()
  (let ((ts1 (make-time-sig '(2 4)))
        (ts2 (make-time-sig '(6 8))))
    (sc-test-check
      (rhythm-p (get-beat-as-rhythm ts1))
      (rhythm-p (get-beat-as-rhythm ts2))
      (equalp (data (get-beat-as-rhythm ts1)) 4)
      (equalp (data (get-beat-as-rhythm ts2)) 8)
      (equalp (data (get-beat-as-rhythm ts2 t)) 'Q.))))

;;; SAR Fri Dec 30 17:56:16 EST 2011
(sc-deftest test-time-sig-scale ()
  (let ((ts (make-time-sig '(2 4))))
    (sc-test-check
      (time-sig-p (scale ts 3))
      (equalp (data (scale ts 3)) '(6 4))
      (time-sig-p (scale ts 2))
      (equalp (data (scale ts 2)) '(2 2))
      (time-sig-p (scale ts 2))
      (equalp (data (scale ts 2 nil)) '(4 4))
      (time-sig-p (scale ts .5))
      (equalp (data (scale ts .5)) '(2 8)))))

;;; SAR Fri Dec 30 18:02:25 EST 2011
(sc-deftest test-time-sig-is-compound ()
  (let ((ts1 (make-time-sig '(2 4)))
        (ts2 (make-time-sig '(6 8))))
    (sc-test-check
      (not (is-compound ts1))
      (is-compound ts2))))

;;; SAR Fri Dec 30 18:13:14 EST 2011
(sc-deftest test-time-sig-time-sig-equal ()
  (let ((ts1 (make-time-sig '(2 4)))
        (ts2 (make-time-sig '(2 4)))
        (ts3 (make-time-sig '(4 8)))
        (ts4 (make-time-sig '(3 4))))
    (sc-test-check
      (time-sig-equal ts1 ts2)
      (equalp (time-sig-equal ts2 ts3) 'TIME-SIG-EQUAL-DURATION)
      (not (time-sig-equal ts3 ts4)))))

;;; SAR Fri Dec 30 18:27:07 EST 2011
(sc-deftest test-time-sig-get-whole-bar-rest ()
  (let* ((ts (make-time-sig '(2 4)))
         (tswbr (get-whole-bar-rest ts)))
    (sc-test-check
      (event-p tswbr)
      (= (value tswbr) 2.0)
      (not (pitch-or-chord tswbr))
      (is-rest tswbr))))

;;; MDE Mon Jan 19 21:02:54 2015 
(sc-deftest test-num-beats-at-tempo ()
  (sc-test-check
    (= 2 (num-beats-at-tempo (make-time-sig '(2 4)) (make-tempo 120 :beat 4)))
    (= 4 (num-beats-at-tempo (make-time-sig '(4 4)) (make-tempo 120 :beat 4)))
    (= 2.5 (num-beats-at-tempo (make-time-sig '(5 8)) (make-tempo 120 :beat 4)))
    (= 2 (num-beats-at-tempo (make-time-sig '(6 8)) (make-tempo 120 :beat 'q.)))
    (= 3 (num-beats-at-tempo (make-time-sig '(6 8)) (make-tempo 120 :beat 'q)))
    (= 12 (num-beats-at-tempo (make-time-sig '(6 8))
                              (make-tempo 150 :beat 's)))))

(sc-deftest test-make-time-sig-from-duration ()
  (sc-test-check
    (equalp '(3 8) (data (make-time-sig-from-duration 1.5)))
    (equalp '(3 4) (data (make-time-sig-from-duration 1.5 120)))
    (equalp '(6 16) (data (make-time-sig-from-duration
                           1.5 60 '(((6 16) (3 8))))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pitch tests

;;; SAR Mon Jan 2 10:39:59 EST 2012
(sc-deftest test-pitch-make-pitch ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 261.63 :src-ref-pitch 'a4 :midi-channel 1)))
    (sc-test-check
      ;; p1
      (pitch-p p1)
      (= (frequency p1) (note-to-freq 'c4))
      (= (midi-note p1) 60)
      (= (midi-channel p1) 1)
      (= (src p1) 1.0)
      (equalp (src-ref-pitch p1) 'C4)
      (= (octave p1) 4)
      (equalp (data p1) 'c4)
      ;; p2
      (pitch-p p2)    
      (equal-within-tolerance (frequency p2) 261.63 0.00001)
      (= (midi-note p2) 60)
      (= (midi-channel p2) 1)
      (equal-within-tolerance (src p2) (/ 261.63 (note-to-freq 'a4)))
      (equalp (src-ref-pitch p2) 'A4)
      (= (octave p2) 4)
      (equalp (data p2) 'C4))))

;;; SAR Mon Jan  2 12:40:15 EST 2012
(sc-deftest test-pitch-add-mark ()
  (let ((p (make-pitch 'c4)))
    (sc-test-check
      (not (marks p))
      (listp (add-mark p 'pizz))
      (add-mark p 'a)
      (equalp (marks p) '(A PIZZ)))))

;;; SAR Mon Jan  2 12:48:56 EST 2012
(sc-deftest test-pitch-delete-marks ()
  (let ((p (make-pitch 'c4)))
    (sc-test-check
      (add-mark p 'pizz)
      (add-mark p 'a)
      (equalp (marks p) '(A PIZZ))
      (not (delete-marks p))
      (not (marks p)))))

;;; SAR Mon Jan  2 13:41:20 EST 2012
(sc-deftest test-pitch-transpose ()
  (let* ((p (make-pitch 'c4))
         (tp1 (transpose p 2))
         (tp2 (transpose p 2.2))
         (tp3 (transpose p 2.5))
         (tp4 (transpose p 2.7)))
    (sc-test-check
      (pitch-p tp1)
      (equalp (data tp1) 'D4)
      (equal-within-tolerance (frequency tp1) 293.665 .001)
      (= (midi-note tp1) 62) 
      (equal-within-tolerance 
       (src tp1) 
       (/ (note-to-freq 'd4) (note-to-freq 'c4)))
      (not (micro-tone tp1))
      (= (frequency tp1) (frequency tp2))
      (equalp (data tp1) (data tp2))
      (= (frequency tp3) (frequency tp4))
      (equalp (data tp3) (data tp4)))))

;;; MDE Fri Mar 25 14:29:31 2016 -- this arose out of an example for Adam
;;; Linson but it's worth testing too
(sc-deftest test-pitch-transpose2 ()
    (let* ((intervals '(0 1 3 5))
           (starting-pitches (init-pitch-list '(cs3 d4 ef5)))
           (pitches
            (loop for sp in starting-pitches appending
                 (loop for i in intervals collect
                      (transpose (clone sp) i))))
           (events (loop for p in pitches collect 
                        (make-event p 'q :amplitude 60)))
           (time 0.0))
      (loop for e in events do
           (setf (start-time e) time)
           (incf time (between 5.0 9.0)))
      (sc-test-check
        (= 12 (length events))
        (eq 'cs3 (data (pitch-or-chord (first events))))
        (eq 'af5 (data (pitch-or-chord (first (last events)))))
        (cm::event-list-to-midi-file events "/tmp/test.mid" 120 0)
        ;; MDE Wed Jun 29 13:45:20 2016 -- why not test this too?
        (stringp (event-list-to-midi-file
                  (midi-file-to-events "/tmp/test.mid")
                  :midi-file "/tmp/mftoe.mid"))
        (file-write-ok "/tmp/mftoe.mid" 160)
        (file-write-ok "/tmp/test.mid" 160))))


;;; SAR Mon Jan  2 14:00:07 EST 2012
(sc-deftest test-pitch-pitch-round ()
  (let* ((p1 (make-pitch 'CQS4))
         (p2 (make-pitch 269.0))
         (pr1 (pitch-round p1))
         (pr2 (pitch-round p2)))
    (sc-test-check
      (= 3 (midi-channel (pitch-round (make-pitch 'aqs4 :midi-channel 3))))
      ;; pr1
      (pitch-p pr1)
      (equalp (data pr1) 'C4)
      (= (midi-note pr1) 60)
      (not (micro-tone pr1))
      (= (src pr1) 1.0)
      ;; pr2
      (pitch-p pr2)
      (equalp (data pr2) 'C4)
      (= (midi-note pr2) 60)
      (not (micro-tone pr2))
      (= (src pr2) 1.0))))

;;; SAR Mon Jan  2 14:12:28 EST 2012
(sc-deftest test-pitch-transpose-to-octave ()
  (let* ((p (make-pitch 'c4))
         (ttop (transpose-to-octave p 5))
         (ttopas (transpose-to-octave p 5 :as-symbol t)))
    (sc-test-check
      (pitch-p ttop)
      (equalp (data ttop) 'C5)
      (= (midi-note ttop) 72)
      (= (src ttop) 2.0)
      (not (micro-tone ttop))
      (equal ttopas 'C5))))

;;; SAR Mon Jan  2 16:58:55 EST 2012
(sc-deftest test-pitch-pitch= ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'c4))
        (p3 (make-pitch 'cs4))
        (p4 (make-pitch 'df4))
        (p5 (make-pitch 261.63)))
    (sc-test-check
      (pitch= p1 p2)
      (not (pitch= p2 p3))
      (not (pitch= p3 p4))
      (pitch= p3 p4 t)
      ;; MDE Wed Jan  4 18:21:35 2012 -- changed to pitch= instead of (not
      ;; (pitch= as it should now work 
      (pitch= p1 p5))))

;;; SAR Mon Jan  2 17:15:07 EST 2012
(sc-deftest test-pitch-pitch-class-eq ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'c5))
        (p3 (make-pitch 'cs5))
        (p4 (make-pitch 'bs4)))
    (sc-test-check
      (pitch-class-eq p1 p2)
      (not (pitch-class-eq p2 p3))
      (not (pitch-class-eq p1 p4))
      (pitch-class-eq p1 p4 t))))

;;; SAR Mon Jan  2 18:32:06 EST 2012
(sc-deftest test-pitch-note= ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'c4))
        (p3 (make-pitch 'd4))
        (p4 (make-pitch 261.63)))
    (sc-test-check
      (note= p1 p2)
      (not (note= p2 p3))
      (note= p1 p4))))

;;; SAR Mon Jan  2 18:50:17 EST 2012
(sc-deftest test-pitch-pitch< ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'd4))
        (p3 (make-pitch 'c4))
        (p4 (make-pitch 261.63))
        (p5 (make-pitch 293.66)))
    (sc-test-check
      (pitch< p1 p2)
      (not (pitch< p2 p1))
      (not (pitch< p1 p3))
      (pitch< p4 p5)
      (pitch< p1 p4))))

;;; SAR Mon Jan  2 20:28:43 EST 2012
(sc-deftest test-pitch-pitch> ()
  (let ((p1 (make-pitch 'd4))
        (p2 (make-pitch 'c4))
        (p3 (make-pitch 'd4))
        (p4 (make-pitch 293.66))
        (p5 (make-pitch 261.63)))
    (sc-test-check
      (pitch> p1 p2)
      (not (pitch> p2 p1))
      (not (pitch> p1 p3))
      (pitch> p4 p5)
      (pitch> p1 p4))))

;;; SAR Mon Jan  2 20:40:09 EST 2012
(sc-deftest test-pitch-pitch<= ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'd4))
        (p3 (make-pitch 'c4))
        (p4 (make-pitch 261.63))
        (p5 (make-pitch 293.66)))
    (sc-test-check
      (pitch<= p1 p2)
      (not (pitch<= p2 p1))
      (pitch<= p1 p3)
      (pitch<= p4 p5)
      (pitch<= p1 p4))))

;;; SAR Mon Jan  2 20:47:04 EST 2012
(sc-deftest test-pitch-pitch>= ()
  (let ((p1 (make-pitch 'd4))
        (p2 (make-pitch 'c4))
        (p3 (make-pitch 'd4))
        (p4 (make-pitch 293.66))
        (p5 (make-pitch 261.63)))
    (sc-test-check
      (pitch>= p1 p2)
      (not (pitch>= p2 p1))
      (pitch>= p1 p3)
      (pitch>= p4 p5)
      (pitch>= p1 p4))))

;;; SAR Mon Jan  2 21:42:19 EST 2012
(sc-deftest test-pitch-pitch-in-range ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'g3))
        (p3 (make-pitch 'a7)))
    (sc-test-check
      (pitch-in-range p1 p2 p3)
      (not (pitch-in-range p2 p1 p3))
      (not (pitch-in-range p3 p1 p2))
      (not (pitch-in-range p1 p3 p2)))))

;;; SAR Tue Jan  3 08:44:12 EST 2012
(sc-deftest test-pitch-pitch-min ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'd4))
        (p3 (make-pitch 'c4)))
    (sc-test-check
      (pitch-p (pitch-min p1 p2))
      (pitch-p (pitch-min p1 p3))
      (equalp (data (pitch-min p1 p2)) 'c4)
      (equalp (data (pitch-min p1 p3)) 'c4))))

;;; SAR Tue Jan  3 09:19:24 EST 2012
(sc-deftest test-pitch-pitch-max ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'd4))
        (p3 (make-pitch 'c4)))
    (sc-test-check
      (pitch-p (pitch-max p1 p2))
      (pitch-p (pitch-max p1 p3))
      (equalp (data (pitch-max p1 p2)) 'd4)
      (equalp (data (pitch-max p1 p3)) 'c4))))

;;; SAR Tue Jan  3 09:30:34 EST 2012
(sc-deftest test-pitch-midi- ()
  (let ((p1 (make-pitch 'd4))
        (p2 (make-pitch 'c4)))
    (sc-test-check
      (= (midi- p1 p2) 2)
      (= (midi- p2 p1) -2))))

;;; SAR Tue Jan  3 09:44:34 EST 2012
(sc-deftest test-pitch-degree- ()
  (let ((p1 (make-pitch 'd4))
        (p2 (make-pitch 'c4)))
    (sc-test-check
      (= (degree- p1 p2) 4)
      (= (degree- p2 p1) -4))))

;;; SAR Tue Jan  3 10:34:56 EST 2012
(sc-deftest test-pitch-set-midi-channel ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'cqs4)))
    (sc-test-check
      (= (set-midi-channel p1 11 12) 11)
      (= (set-midi-channel p2 11 12) 12))))

;;; SAR Tue Jan  3 11:07:01 EST 2012
(sc-deftest test-pitch-transpose-pitch-list ()
  (let ((pl))
    (setf pl (loop for m from 60 to 71 collect (make-pitch (midi-to-note m))))
    (sc-test-check
      (listp pl)
      (listp (transpose-pitch-list pl 2))
      (equalp (loop for p in (transpose-pitch-list pl 2) collect (data p))
              '(D4 EF4 E4 F4 FS4 G4 AF4 A4 BF4 B4 C5 CS5))
      (listp (transpose-pitch-list pl 2 :return-symbols t))
      ;; MDE Tue Apr 10 08:13:14 2012 -- 
      ;; (equalp (print-simple-pitch-list pl)
      ;;      '(C4 CS4 D4 EF4 E4 F4 FS4 G4 AF4 A4 BF4 B4))
      (string= (print-simple-pitch-list pl nil)
               "(C4 CS4 D4 EF4 E4 F4 FS4 G4 AF4 A4 BF4 B4)")
      (equalp (transpose-pitch-list pl 2 :return-symbols t) 
              '(D4 EF4 E4 F4 FS4 G4 AF4 A4 BF4 B4 C5 CS5)))))

;;; SAR Tue Jan  3 11:53:23 EST 2012
(sc-deftest test-pitch-transpose-pitch-list-to-octave ()
  (let ((pl)
        (pl2))
    (setf pl (loop for m from 0 to 127 by 13 
                collect (data (make-pitch (midi-to-note m)))))
    (setf pl2 (loop for m from 0 to 127 by 12 
                 collect (data (make-pitch (midi-to-note m)))))
    (sc-test-check
      (equalp pl '(C-1 CS0 D1 EF2 E3 F4 FS5 G6 AF7 A8))
      (equalp (loop for p in (transpose-pitch-list-to-octave pl 4)
                 collect (data p))
              '(C4 CS4 D4 EF4 E4 F4 FS4 G4 AF4 A4))
      (equalp (transpose-pitch-list-to-octave pl 4 :as-symbols t)
              '(C4 CS4 D4 EF4 E4 F4 FS4 G4 AF4 A4))
      (equalp (loop for p in (transpose-pitch-list-to-octave pl2 4) 
                 collect (data p))
              '(C4))
      (equalp (transpose-pitch-list-to-octave pl2 4 
                                              :as-symbols t
                                              :remove-duplicates nil) 
              (loop repeat 11 collect 'C4)))))

;;; SAR Tue Jan  3 12:00:35 EST 2012
(sc-deftest test-pitch-pitch-list-to-symbols ()
  (let ((pl))
    (setf pl (loop for m from 0 to 127 by 13 
                collect (make-pitch (midi-to-note m))))
    (sc-test-check
      (listp (pitch-list-to-symbols pl))
      (equal (pitch-list-to-symbols pl)
             '(C-1 CS0 D1 EF2 E3 F4 FS5 G6 AF7 A8)))))

;;; SAR Tue Jan  3 12:37:53 EST 2012
(sc-deftest test-pitch-sort-pitch-list ()
  (let ((pl))
    (setf pl (loop for m from 64 downto 60
                collect (make-pitch (midi-to-note m))))
    (sc-test-check
      (listp (sort-pitch-list pl))
      (equalp (loop for p in pl collect (pitch-p p))
              (loop repeat 5 collect T))
      (equalp (loop for p in (sort-pitch-list pl) 
                 collect (data p))
              '(C4 CS4 D4 EF4 E4))
      (equalp (sort-pitch-list pl t) '(C4 CS4 D4 EF4 E4)))))

;;; SAR Tue Jan  3 13:38:46 EST 2012
(sc-deftest test-pitch-remove-octaves ()
  (let ((pl1 '(c1 c2 c3 g3))
        (pl2 '(261.63 523.26 1046.52 196.00))
        (pl3 '(c1 c2 c3 c4 g3)))
    (sc-test-check
      (listp (remove-octaves pl1))
      (equalp (loop for p in (remove-octaves pl1) collect (pitch-p p))
              (loop repeat 2 collect T))
      (equalp (loop for p in (remove-octaves pl1) collect (data p)) '(C1 G3))
      (equal-within-tolerance 
       (first (remove-octaves pl2)) 261.63 0.00001)    
      (equal-within-tolerance 
       (second (remove-octaves pl2)) 196.00 0.00001)    
      (equalp (remove-octaves pl2 :as-symbol t) '(C4 G3))
      (equalp (remove-octaves pl1 :as-symbol t :allow 2) '(C1 G3))
      (equalp (remove-octaves pl3 :as-symbol t :allow 2) '(C1 G3)))))

;;; SAR Tue Jan  3 14:35:36 EST 2012
(sc-deftest test-pitch-invert-pitch-list ()
  (let ((pl1 '(E4 G4 A4 C4))
        (pl2)
        (pl3 '(329.63 392.00 440.00 261.63)))
    (setf pl2 (loop for nn in pl1 collect (make-pitch nn)))
    (sc-test-check
      (equalp (loop for p in (invert-pitch-list pl1) collect (pitch-p p))
              (loop repeat 4 collect T))
      (equalp (loop for p in (invert-pitch-list pl2) collect (pitch-p p))
              (loop repeat 4 collect T))
      (equalp (loop for p in (invert-pitch-list pl3) collect (pitch-p p))
              (loop repeat 4 collect T))
      (equalp (loop for p in (invert-pitch-list pl1) collect (data p))
              '(C4 AF3 F3 EF3))    
      (equalp (invert-pitch-list pl2 t) '(C4 AF3 F3 EF3))
      (equalp (invert-pitch-list pl3 t) '(C4 AF3 F3 EF3)))))

;;; SAR Tue Jan  3 15:27:03 EST 2012
(sc-deftest test-pitch-pitch-member ()
  (let ((pl1 '(c4 d4 e4))
        (pl2 '(261.62 293.66 329.62))
        (pl3 (loop for n in '(c4 d4 e4) collect (make-pitch n)))
        (pl4 '(c4 ds4 e4)))
    (sc-test-check
      (listp (pitch-member 'd4 pl1))
      (listp (pitch-member 261.62 pl2))
      (listp (pitch-member 'd4 pl3))
      (equal (loop for pm in (pitch-member 'd4 pl1) collect (pitch-p pm))
             (loop repeat 2 collect T))
      (equal (loop for pm in (pitch-member 293.66 pl2) collect (pitch-p pm))
             (loop repeat 2 collect T))
      (equal (loop for pm in (pitch-member 'd4 pl3) collect (pitch-p pm))
             (loop repeat 2 collect T))
      (not (pitch-member 'f4 pl1))
      (equal (pitch-list-to-symbols (pitch-member 'd4 pl1)) '(D4 E4))
      (equal (pitch-list-to-symbols (pitch-member 293.66 pl2)) '(D4 E4))
      (equal (pitch-list-to-symbols (pitch-member 'd4 pl3)) '(D4 E4))
      (pitch-member 'ef4 pl4)
      (not (pitch-member 'ef4 pl4 nil)))))

;;; SAR Tue Jan  3 16:19:32 EST 2012
(sc-deftest test-pitch-remove-pitches ()
  (let ((pl1 '(c4 d4 e4))
        (pl2 '(261.62 293.66 329.62))
        (pl3 (loop for n in '(c4 d4 e4) collect (make-pitch n)))
        (pl4 '(c4 ds4 e4)))
    (sc-test-check
      (equal (loop for p in (remove-pitches pl1 '(d4 e4)) 
                collect (pitch-p p))
             '(T))
      (equal (loop for p in (remove-pitches pl2 '(293.66))
                collect (pitch-p p))
             (loop repeat 2 collect T))
      (equal (loop for p in (remove-pitches pl3 `(,(make-pitch 'e4)))
                collect (pitch-p p))
             (loop repeat 2 collect T))
      (equalp (pitch-list-to-symbols (remove-pitches pl1 '(d4 e4))) '(C4)) 
      (equalp (remove-pitches pl2 '(293.66) :return-symbols t) '(C4 E4)) 
      (equalp (remove-pitches pl3 `(,(make-pitch 'e4)) :return-symbols t) 
              '(C4 D4)) 
      (equalp (remove-pitches pl4 '(ef4) :return-symbols t) '(C4 E4)) 
      (equalp (remove-pitches pl4 '(ef4) 
                              :return-symbols t
                              :enharmonics-are-equal nil)
              '(C4 DS4 E4)))))

;;; SAR Tue Jan  3 16:37:19 EST 2012
(sc-deftest test-pitch-enharmonic ()
  (let ((p1 (make-pitch 'cs4))
        (p2 (make-pitch 'f4))
        (p3 (make-pitch 'g4)))
    (sc-test-check
      (pitch-p (enharmonic p1))
      (pitch-p (enharmonic p2))
      (pitch-p (enharmonic p3))
      (equalp (data (enharmonic p1)) 'DF4)
      (equalp (data (enharmonic p2)) 'ES4)
      (equalp (data (enharmonic p3)) 'G4))))

;;; SAR Tue Jan  3 17:37:33 EST 2012
(sc-deftest test-pitch-pitch-intersection ()
  (let ((p1 '(c4 d4 e4 f4))
        (p2 (loop for nn in '(d4 e4 f4 g4) collect (make-pitch nn))))
    (sc-test-check
      (equalp (loop for p in (pitch-intersection p1 p2) collect (pitch-p p))
              (loop repeat 3 collect T))
      (equalp (pitch-list-to-symbols (pitch-intersection p1 p2)) 
              '(D4 E4 F4))))) 

;;; SAR Tue Jan  3 17:47:47 EST 2012
(sc-deftest test-pitch-in-octave ()
  (let ((p1 (make-pitch 'c4))
        (p2 261.63)
        (p3 'c4))
    (sc-test-check
      (not (in-octave p1 3))
      (in-octave p1 4)
      (in-octave p2 4)
      (in-octave p3 4))))

;;; SAR Fri Jan  6 14:51:34 EST 2012
(sc-deftest test-pitch-pitch-inc ()
  (let ((p (make-pitch 'c4)))
    (sc-test-check
      (pitch-p (pitch-inc p))
      (pitch-p (pitch-inc p 2))
      (equalp (data (pitch-inc p)) 'CQS4)
      (equalp (loop for i from 0 to 4 collect (data (pitch-inc p i)))
              '(C4 CQS4 CS4 DQF4 D4)))))

;;; MDE Sat Jan  7 16:40:24 2012 -- test making pitches from frequencies close
;;; to chromatic pitches but not quite 
(sc-deftest test-pitch-inexact-freqs ()
  (let* ((notes '(c1 cs2 d3 ds4 e5 f6 fs7 g8 af9 a10 bf0 cf4 cqs3 eqf6 bqs0))
         (ps (loop for n in notes collect (make-pitch n)))
         ;; randomise our pitch freqs by 2% and see what pitches they turn into
         (psi (loop for p in ps collect 
                   (make-pitch (randomise (frequency p) 2))))
         (data-ok t)
         (pitch-bend-ok t))
    (loop for p in psi and n in notes do
         (unless (or (equalp n (data p))
                     (equalp n (enharmonic-equivalent (data p))))
           (format t "~a~&data should be ~a" p n)
           (setf data-ok nil))
         (unless (and (>= (pitch-bend p) -1.0) (<= (pitch-bend p) 1.0))
           (format t "~a~&pitch-bend should be >= -1.0 and <= 1.0" p)
           (setf pitch-bend-ok nil)))
    (sc-test-check
      (= 0.5 (pitch-bend (nth 12 ps)))
      (= 0.5 (pitch-bend (nth 13 ps)))
      (= 0.5 (pitch-bend (nth 14 ps)))
      data-ok pitch-bend-ok)))

;;; SAR Sat Jan  7 10:14:31 EST 2012
(sc-deftest test-pitch-pitch- ()
  (let ((p1 (make-pitch 'd4))
        (p2 (make-pitch 'c4))
        (p3 (make-pitch 'dqs4))
        (p4 (make-pitch 293.66))
        (p5 (make-pitch 261.63))
        (p6 (make-pitch 293.665))
        (p7 (make-pitch 261.626)))
    (sc-test-check
      (= (pitch- p1 p2) 2.0)
      (= (pitch- p3 p2) 2.5)
      (= (pitch- p4 p5) 2.0)
      (= (pitch- p6 p7) 2.0))))

;;; SAR Tue Aug  7 17:53:52 BST 2012
(sc-deftest test-pitch-no-accidental ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 cs4 fs4))))
          :set-map '((1 (1 1)))
          :rthm-seq-palette '((1 ((((2 4) - s s s s - - s s s s -))
                                  :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
          :rthm-seq-map '((1 ((vn (1 1))))))))
    (probe-delete "/tmp/slippery-chicken-piece.eps")
    (probe-delete "/tmp/_slippery-chicken-piece-score.ly")
    (sc-test-check
      #+cmn (cmn-display mini :respell-notes t)
      #+cmn (show-accidental (pitch-or-chord (get-note mini 1 7 'vn)))
      #+cmn (accidental-in-parentheses (pitch-or-chord 
                                        (get-note mini 1 7 'vn)))
      (not (no-accidental (pitch-or-chord (get-note mini 1 7 'vn ))))
      (not (show-accidental (pitch-or-chord (get-note mini 1 7 'vn))))
      (not (accidental-in-parentheses (pitch-or-chord 
                                       (get-note mini 1 7 'vn))))
      #+cmn (cmn-display mini :respell-notes nil)
      #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 12000)
      (write-lp-data-for-all mini :respell-notes nil)
      (file-write-ok "/tmp/_slippery-chicken-piece-score.ly" 190))))

#+cmn
(sc-deftest test-cmn-display-pitch-list ()
  (sc-test-check
    (typep (cmn-display-pitch-list (init-pitch-list '(c4 d4 e4)))
           'cmn::score)))

(sc-deftest test-midi-note-float ()
  (let ((p440 (make-pitch 440))
        (p445 (make-pitch 445))
        (p450 (make-pitch 450))
        (pc4 (make-pitch 'c4))
        (pcqs4 (make-pitch 'cqs4))
        (pdqf7 (make-pitch 'dqf7)))
    (sc-test-check
      (equal-within-tolerance (pitch-bend p450) .39)
      (equal-within-tolerance (pitch-bend p445) .2)
      (equal-within-tolerance (pitch-bend pcqs4) .5)
      (equal-within-tolerance (pitch-bend pdqf7) .5)
      (equal (nearest-chromatic pdqf7) 'df7)
      (equal (nearest-chromatic pcqs4) 'c4)
      (= 69 (midi-note p440) (midi-note p445))
      (equal 'a4 (id p445))
      (equal 'a4 (data p445))
      (equal 'aqs4 (id p450) )
      (equal 'aqs4 (data p450))
      (equal-within-tolerance 69.39 (midi-note-float p450))
      ;; MDE Mon May  5 16:32:32 2014 -- test the in-cents arg
      (equal-within-tolerance 6939 (midi-note-float p450 t))
      (equal-within-tolerance 69.2 (midi-note-float p445))
      (equal-within-tolerance 60.0 (midi-note-float pc4))
      (equal-within-tolerance 97.5 (midi-note-float pdqf7))
      (equal-within-tolerance 60.5 (midi-note-float pcqs4)))))

;; MDE Thu Jan  9 08:53:05 2020 -- test those 12th tones!
(sc-deftest test-nearest-chromatic-12th ()
  (let ((scale cm::*scale*))
    (in-scale :twelfth-tone)
    (let ((t1 (make-pitch 'cts4))
          (t2 (make-pitch 'css7))
          (t3 (make-pitch 'fssf1))
          (t4 (make-pitch 'astf5))
          (t5 (make-pitch 'csts4))
          (t6 (make-pitch 'gfts1))
          (t7 (make-pitch 'fsss8))
          (t8 (make-pitch 'assf6))
          (t9 (make-pitch 'dfss3))
          (t10 (make-pitch 'fsf0))
          (t11 (make-pitch 'ftf1)))
      (sc-test-check
        (equal (nearest-chromatic t1) 'c4)
        (equal (nearest-chromatic t2) 'c7)
        (equal (nearest-chromatic t3) 'fs1)
        (equal (nearest-chromatic t4) 'as5)
        (equal (nearest-chromatic t5) 'cs4)
        (equal (nearest-chromatic t6) 'gf1)
        (equal (nearest-chromatic t7) 'fs8)
        (equal (nearest-chromatic t8) 'as6)
        (equal (nearest-chromatic t9) 'df3)
        (equal (nearest-chromatic t10) 'f0)
        (equal (nearest-chromatic t11) 'f1))
      (in-scale scale))))


;;; MDE Wed Aug  7 10:01:08 2013 
(sc-deftest test-rm-bad-intervals ()
  (let* ((ps1 (init-pitch-list '(c4 cs4 ds4 e4 a4 b4)))
         (ps2 (init-pitch-list '(c4 d4 e4 gs4 b4))))
    (sc-test-check
      (equalp '(c4 ds4 b4) (pitch-list-to-symbols (rm-bad-intervals ps1 '(1 6))))
      (equalp '(c4 ds4 a4) (pitch-list-to-symbols (rm-bad-intervals ps1 '(1 2))))
      (equalp '(c4 e4 a4) (pitch-list-to-symbols (rm-bad-intervals ps1 '(1 2 3))))
      (equalp '(c4 gs4 b4) (pitch-list-to-symbols
                            (rm-bad-intervals ps2 '(2 4)))))))

(sc-deftest test-natural-harmonic ()
  (sc-test-check
    (equalp '(6 5) (natural-harmonic 'gs5))
    ;; double bass
    (equalp '(4 6) (natural-harmonic 'b3 :tuning '(g2 d2 a1 e1)))
    ;; three goes at 7th harmonic of B string
    (not (natural-harmonic 'aqf7 :highest-partial 8))
    (equalp '(2 7) (natural-harmonic 'aqf7 :highest-partial 8 :tolerance 20))
    (not (natural-harmonic 'aqf7 :tolerance 20))
    (equalp '(6 5) (natural-harmonic 'gs5))))

(sc-deftest test-enharmonic-equivalents ()
  (sc-test-check
    (enharmonic-equivalents (make-pitch 'gs4) (make-pitch 'af4))
    (enharmonic-equivalents (make-pitch 'f0) (make-pitch 'es0))
    (enharmonic-equivalents (make-pitch 'c5) (make-pitch 'bs4))
    (not (enharmonic-equivalents (make-pitch 'gs4) (make-pitch 'gs4)))
    (not (enharmonic-equivalents (make-pitch 'gs4) (make-pitch 'af5)))))

;;; MDE Sun Dec 29 14:23:03 2013 -- make sure our pitches work in octave -1
(sc-deftest test-pitch-octave-minus-1 ()
  (let ((c (make-pitch 'c-1))
        (cs (make-pitch 'cs-1))
        (df (make-pitch 'df-1))
        (dn (make-pitch 'dn-1))
        (eqf (make-pitch 'eqf-1))
        (fqs (make-pitch 'fqs-1))
        ;; now higher octaves
        (g (make-pitch 'g4))
        (gs (make-pitch 'gs4))
        (gf (make-pitch 'gf4))
        (gqs (make-pitch 'gqs4))
        (gqf (make-pitch 'gqf4)))
    (sc-test-check
      (natural c)
      (not (micro-tone c))
      (not (sharp c))
      (not (flat c))
      (sharp cs)
      (flat df)
      (natural dn)
      (qtr-flat eqf)
      (micro-tone eqf)
      (qtr-sharp fqs)
      (micro-tone eqf)
      (natural g)
      (sharp gs)
      (flat gf)
      (qtr-sharp gqs)
      (micro-tone gqs)
      (qtr-flat gqf)
      (micro-tone gqf))))

;;; MDE Mon Apr 25 10:15:56 2016 
(sc-deftest test-find-nearest-pitch ()
  (sc-test-check
    (= 0 (nth-value 1 (find-nearest-pitch '(b0 d1 fs1 a4) 'c1)))
    (= 3 (nth-value 1 (find-nearest-pitch '(b0 d1 fs1 a4) 'c5)))
    (pitch= (make-pitch 'd1) (find-nearest-pitch '(b0 d1 fs1 a4) 'e1))
    ;; remember: fs1 will be sorted lower when optional sort arg is t
    (= 4 (nth-value 1 (find-nearest-pitch '(b0 d1 cs2 d3 fs1 a4) 'fs3 t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Oct 18 15:07:34 2018 -- 
(sc-deftest test-round-to-nearest ()
  (in-scale :quarter-tone)
  (let ((e1 (make-event (make-pitch 265) 'q))
        (e2 (make-event '(113 1023) 'q :transposition 2)))
    (sc-test-check
      (pitch= (make-pitch 'c4) (round-to-nearest (make-pitch 265)))
      (pitch= (make-pitch 'cqs4) (round-to-nearest (make-pitch 270)))
      (chord= (make-chord '(cqs4 eqs4))
              (round-to-nearest (make-chord '(270 340))))
      ;; nearest is down, not up
      (pitch= (make-pitch 'ef7) (round-to-nearest (make-pitch 'dqs7)
                                                 :scale 'chromatic-scale))
      (pitch= (make-pitch 'e0) (round-to-nearest (make-pitch 'eqf0)
                                                 :scale 'chromatic-scale))
      (pitch= (pitch-or-chord (round-to-nearest e1)) (make-pitch 'c4))
      (chord= (pitch-or-chord (round-to-nearest e2)) (make-chord '(aqs2 bqs5)))
      (chord= (written-pitch-or-chord e2) (make-chord '(gqs2 aqs5)))
      (pitch= (round-to-nearest (make-pitch 270)) (make-pitch 'cqs4))
      (pitch= (round-to-nearest (make-pitch 270) :scale 'chromatic-scale)
              (make-pitch 'cs4))
      (pitch= (round-to-nearest (make-pitch 63) :scale 'chromatic-scale)
              (make-pitch 'b1))
      (in-scale :twelfth-tone)
      ;; 264 is approx. c 1/12 sharp
      (pitch= (make-pitch 'cts4) (round-to-nearest (make-pitch 264)))
      (in-scale :chromatic)
      (pitch= (round-to-nearest (make-pitch 63)) (make-pitch 'b1))
      (pitch= (round-to-nearest (make-pitch 270)) (make-pitch 'cs4))
      ;; (print e2)
      (chord= (pitch-or-chord (round-to-nearest e2))
              (make-chord '(a2 c6)) (in-scale :quarter-tone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pitch-seq tests

;;; SAR Tue Jan  3 18:23:44 EST 2012
(sc-deftest test-pitch-seq-make-pitch-seq ()
  (let ((ps1 (make-pitch-seq '(pseq1 (1 2 1 1 3))))
        (ps2 (make-pitch-seq '(2 1 1 3 1) 'pseq2)))
    (sc-test-check
      ;; ps1
      (not (notes ps1))
      (= (highest ps1) 3)
      (= (lowest ps1) 1)
      (equalp (id ps1) 'PSEQ1)
      (equalp (data ps1) '(1 2 1 1 3))
      ;; ps2
      (not (notes ps2))
      (= (highest ps2) 3)
      (= (lowest ps2) 1)
      (equalp (id ps2) 'PSEQ2)
      (equalp (data ps2) '(2 1 1 3 1)))))

;;; MDE Wed Mar 27 19:17:39 2013 -- test instruments are recognised too
(sc-deftest test-pitch-seq-instruments ()
  (let ((ps1 (make-pitch-seq '((1 2 1 1 3) violin flute) 'ps1))
        (ps2 (make-pitch-seq '(ps2 ((1 2 1 1 3) violin flute)))))
    (sc-test-check
     (equalp (instruments ps1) '(violin flute))
     (equalp (instruments ps2) '(violin flute)))))


;;; SAR Tue Jan  3 19:30:28 EST 2012
(sc-deftest test-pitch-seq-invert ()
  (let ((ps (make-pitch-seq '(pseq1 (1 2 1 3 4 7)))))
    (sc-test-check
      (equalp (data (invert ps)) '(7 4 7 3 2 1)))))

;;; SAR Tue Aug  7 17:17:33 BST 2012
(sc-deftest test-pitch-seq-lowest-equals-prefers-x ()
  (sc-test-check
    (set-sc-config 'pitch-seq-lowest-equals-prefers-high 6)
    (= 6 (get-sc-config 'pitch-seq-lowest-equals-prefers-high))
    (set-sc-config 'pitch-seq-lowest-equals-prefers-high 5)))

;;; MDE Mon Jan 28 19:55:45 2013 -- make sure that when we share a single psp
;;; object between more than one rthm-seq, that it's not cloned, i.e. we cycle
;;; through the pitch-seqs no matter which rthm-seq we call get-next for 
(sc-deftest test-psp-sharing ()
  (let* ((4s '((0 3 2 6) (0 3 1 4) (0 3 4 6))) ;; a half turn to the right
         (psp4 (make-psp '4s 4 4s))
         (rsp
          (make-rsp
           '6-4
           '((4
              ((a ((((4 4) (w)) (h - +e e - (q)) ((q) (e) e+h)
                    (+h. - +s s - (e))((w)) ((w)))))
               (b ((((4 4) (h) (e) e (q)) ((h) e (e) (q)) ((w))
                    ((h) (q) (s) s (e)) ((e) e (q) (h)) ((w)))))
               (c ((((4 4) (w)) ((h) h) (+q h.) (+h.. e) ((w)) ((h) (e) q.))))
               (d ((((4 4) (w)) ((q) - e e - (e) e (q)) ((w)) ((q) (e) e (h))
                    ((w)) ((w)))))))))))
    (loop for rthm-seq in (data (get-data-data 4 rsp)) do
         (setf (pitch-seq-palette rthm-seq) psp4))
    (sc-test-check
      (equalp (first 4s)
              (data (get-next (pitch-seq-palette (get-data '(4 a) rsp)))))
      (equalp (second 4s)
              (data (get-next (pitch-seq-palette (get-data '(4 b) rsp)))))
      ;; any order...
      (equalp (third 4s)
              (data (get-next (pitch-seq-palette (get-data '(4 d) rsp)))))
      (equalp (first 4s)
              (data (get-next (pitch-seq-palette (get-data '(4 c) rsp))))))))

;;; MDE Wed Jun 10 17:48:39 2020, Heidhausen -- do pitch-seqs assignment to
;;; specific instruments work?
(sc-deftest test-instrument-pitch-seq ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5)))) 
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                  :pitch-seq-palette (((1 2 3) violin)
                                                      (3 1 2)
                                                      ((3 2 1) viola)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (va (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      ;; (cmn-display mini)
      ;; violin (instrument, not player!) gets 1 2 3
      (equalp '(GS3 NIL AS3 NIL B3) (get-pitch-symbols (get-bar mini 1 'vn)))
      (equalp '(GS3 NIL AS3 NIL B3) (get-pitch-symbols (get-bar mini 5 'vn)))
      ;; viola (instrument, not player!) gets 3 2 1
      (equalp '(e4 NIL ds4 NIL cs4) (get-pitch-symbols (get-bar mini 1 'va)))
      (equalp '(e4 NIL ds4 NIL cs4) (get-pitch-symbols (get-bar mini 4 'va)))
      ;; the cello isn't mentioned in the pitc-seq-palette so it can only use 3
      ;; 1 2
      (equalp '(as4 NIL fs4 NIL gs4) (get-pitch-symbols (get-bar mini 2 'vc)))
      (equalp '(as4 NIL fs4 NIL gs4) (get-pitch-symbols
                                      (get-bar mini 3 'vc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instrument tests

;;; MDE Fri Dec  9 13:36:05 2011
(sc-deftest test-instrument ()
  (let ((af (clone
             (get-data 'alto-flute 
                       +slippery-chicken-standard-instrument-palette+))))
    (sc-test-check
      (pitch-p (lowest-written af))
      (pitch-p (highest-written af))
      (pitch-p (lowest-sounding af))     
      (pitch-p (highest-sounding af))
      (not (pitch= (lowest-written af) (highest-written af)))
      (pitch-p (first (missing-notes af)))
      ;; test setf methods work i.e. create pitch objects out of symbols 
      (setf (lowest-written af) 'b3
            (highest-sounding af) 'b7
            (missing-notes af) 'g4)
      ;; should be a list but will be created
      (pitch-p (lowest-written af))
      (pitch-p (highest-sounding af))
      (pitch-p (first (missing-notes af)))
      (pitch= (highest-written af) (make-pitch 'e8))
      (pitch= (lowest-sounding af) (make-pitch 'fs3)))))

;;; SAR Sat Jan  7 11:03:01 EST 2012
(sc-deftest test-instrument-make-instrument ()
  (let ((i1 (make-instrument 'inst1 :staff-name "Instrument One"
                             :staff-short-name "Ins. 1" :lowest-written 'bf3 
                             :highest-written 'a7 :starting-clef 'bass
                             :missing-notes '(bf2 c3 dqs3)))
        (i2 (make-instrument 'inst2 :lowest-sounding 'bf3 
                             :highest-sounding 'a7)) 
        (i3 (make-instrument 'inst3 :lowest-written 'bf3 :highest-written 'a7
                             :transposition 'BF))
        (i4 (make-instrument 'inst4 :lowest-sounding 'af2 :highest-sounding 'g6
                             :transposition-semitones -14))
        (i5 (make-instrument 'inst5 :transposition 'A 
                             :missing-notes '(c4 ds4 eqf4))))
    (sc-test-check
      ;; i1
      (equalp (id i1) 'inst1)
      (equalp (staff-name i1) "Instrument One")
      (equalp (staff-short-name i1) "Ins. 1")
      (equalp (data (lowest-written i1)) 'bf3)
      (equalp (data (lowest-sounding i1)) 'bf3)
      (equalp (data (highest-written i1)) 'a7)
      (equalp (data (highest-sounding i1)) 'a7)
      (equalp (starting-clef i1) 'bass)
      (equalp (clefs i1) '(bass))
      (equalp (clefs-in-c i1) '(bass))
      (equalp (transposition i1) 'C)
      (= (transposition-semitones i1) 0)
      (equalp (loop for p in (missing-notes i1) collect (data p)) 
              '(bf2 c3 dqs3)) 
      ;; i2
      (equalp (data (lowest-sounding i2)) 'bf3)
      (equalp (data (lowest-written i2)) 'bf3)
      (equalp (data (highest-sounding i2)) 'a7)
      (equalp (data (highest-written i2)) 'a7)
      ;; i3
      (equalp (data (lowest-written i3)) 'bf3)
      (equalp (data (lowest-sounding i3)) 'af3)
      (equalp (data (highest-written i3)) 'a7)
      (equalp (data (highest-sounding i3)) 'g7)
      (= (transposition-semitones i3) -2)
      ;; i4
      (equalp (data (lowest-sounding i4)) 'af2)
      (equalp (data (lowest-written i4)) 'bf3)
      (equalp (data (highest-sounding i4)) 'g6)
      (equalp (data (highest-written i4)) 'a7)
      (equalp (transposition i4) 'BF)
      ;; i5
      (equalp (loop for p in (missing-notes i5) collect (data p)) 
              '(A3 C4 CQS4))))) 

;; SAR Sat Jan  7 11:44:42 EST 2012
(sc-deftest test-instrument-transposing-instrument ()
  (let ((i1 (make-instrument 'instrument-one))
        (i2 (make-instrument 'instrument-two :transposition 'bf))
        (i3 (make-instrument 'instrument-two :transposition-semitones -3))
        (i4 (make-instrument 'instrument-two :transposition-semitones -12)))
    (sc-test-check
      (not (transposing-instrument-p i1))
      (transposing-instrument-p i2)
      (transposing-instrument-p i3)
      (not (transposing-instrument-p i4))
      (transposing-instrument-p i4 nil))))

;;; SAR Sat Jan  7 12:01:19 EST 2012
(sc-deftest test-instrument-set-prefers-low ()
  (let ((i1 (make-instrument 'inst))
        (i2 (make-instrument 'inst :prefers-notes 'high)))
    (sc-test-check
      (not (prefers-notes i1))
      (set-prefers-low i1)
      (equalp (prefers-notes i1) 'LOW)
      (set-prefers-low i2)
      (equalp (prefers-notes i2) 'LOW))))

;;; SAR Sat Jan  7 12:06:53 EST 2012
(sc-deftest test-instrument-set-prefers-high ()
  (let ((i1 (make-instrument 'inst))
        (i2 (make-instrument 'inst :prefers-notes 'low)))
    (sc-test-check
      (not (prefers-notes i1))
      (set-prefers-high i1)
      (equalp (prefers-notes i1) 'HIGH)
      (set-prefers-high i2)
      (equalp (prefers-notes i2) 'HIGH))))

;;; SAR Sat Jan  7 12:13:55 EST 2012
(sc-deftest test-instrument-prefers-low ()
  (let ((i1 (make-instrument 'inst :prefers-notes 'low))
        (i2 (make-instrument 'inst1))
        (i3 (make-instrument 'inst2 :prefers-notes 'high)))
    (sc-test-check
      (prefers-low i1)
      (not (prefers-low i2))
      (not (prefers-low i3)))))

;;; SAR Sat Jan  7 12:18:46 EST 2012
(sc-deftest test-instrument-prefers-high ()
  (let ((i1 (make-instrument 'inst :prefers-notes 'high))
        (i2 (make-instrument 'inst1))
        (i3 (make-instrument 'inst2 :prefers-notes 'low)))
    (sc-test-check
      (prefers-high i1)
      (not (prefers-high i2))
      (not (prefers-high i3)))))

;;; SAR Sat Jan  7 12:52:36 EST 2012
(sc-deftest test-instrument-in-range ()
  (let ((i1 (make-instrument 'inst1 :lowest-written 'bf3 :highest-written 'a6))
        (i2 (make-instrument 'inst1 :lowest-written 'fs3 :highest-written 'c6
                             :transposition 'BF)))
    (sc-test-check
      (in-range i1 'c4)
      (not (in-range i1 'a3))
      (= (nth-value 1 (in-range i1 'a3)) 0)
      (not (in-range i1 'as6))
      (= (nth-value 1 (in-range i1 'as6)) 1)
      ;; MDE Wed Nov 14 16:36:28 2018 -- yes, in range
      (in-range (get-standard-ins 'vibraphone) 'cqs4 nil t t)
      ;; no, can't play 1/4 tones
      (not (in-range (get-standard-ins 'vibraphone) 'cqs4 nil nil nil))
      (in-range i1 (make-pitch 'c4))
      (not (in-range i2 'e3))
      (in-range i2 'e3 t)
      ;; MDE Thu Nov  1 18:50:00 2018 -- test artificial harmonics
      (nth-value 1 (in-range (get-standard-ins 'violin) 'e8 t t))
      ;; MDE Thu Dec  6 16:20:51 2018 -- missing notes allowed
      (in-range (get-standard-ins 'bass-clarinet) 'dqf3 nil nil t nil)
      ;; MDE Thu Dec  6 15:31:02 2018 -- no missing notes
      (not (in-range (get-standard-ins 'bass-clarinet) 'dqf3 nil t nil t))
      (not (in-range (get-standard-ins 'bass-clarinet) 'cqf2 t t nil t))
      ;; MDE Thu Dec  6 10:48:17 2018
      (chord-equal
       (make-chord '(d5 g5))
       (nth-value 1 (in-range (get-standard-ins 'double-bass) 'd6 t t)))
      (chord-equal
       (make-chord '(d4 g4)) ; <-- when d6 is 'sounding'
       (nth-value 1 (in-range (get-standard-ins 'double-bass) 'd6 nil t)))
      (= 1 (nth-value 1 (in-range (get-standard-ins 'double-bass)
                                  'dqs6 t t))))))

;;; MDE Wed Oct  9 13:22:55 2013 
(sc-deftest test-pitch-force-in-range ()
  (let ((cl (get-data 'b-flat-clarinet
                      +slippery-chicken-standard-instrument-palette+))
        ;; MDE Sat Nov 20 13:18:34 2021, Heidhausen
        (rec (get-standard-ins 'consort-tenor-recorder)))
    (sc-test-check
      ;; needs to go up 1 octave as cs4 is missing
      (pitch= (make-pitch 'df5) (force-in-range rec (make-pitch 'df4)
                                                :no-missing-notes t))
      ;; ignore missing notes
      (pitch= (make-pitch 'df4) (force-in-range rec (make-pitch 'df4)
                                                :no-missing-notes nil))
      ;; now works with chords too
      (chord= (force-in-range cl (make-chord '(c3 ef4 b8)))
              (make-chord '(c4 ef4 b5)))
      ;; also events
      (chord= (pitch-or-chord
               (force-in-range cl (make-event '(c3 ef4 b8) 'q)))
              (make-chord '(c4 ef4 b5)))
      (pitch= (pitch-or-chord
               (force-in-range cl (make-event 'cs1 'q)))
              (make-pitch 'cs4))
      ;; needs to go down 1 octave
      (pitch= (make-pitch 'e6) (force-in-range cl (make-pitch 'e7)))
      ;; needs to go up 2 octaves
      (pitch= (make-pitch 'g3) (force-in-range cl (make-pitch 'g1)))
      ;; the t indicates we're dealing with sounding pitches so here there's no
      ;; transposition...  
      (pitch= (make-pitch 'd3) (force-in-range cl (make-pitch 'd3) :sounding t))
      ;; ... but here there is
      (pitch= (make-pitch 'd4) (force-in-range cl (make-pitch 'd3))))))

(sc-deftest test-xml-transposition ()
  (flet ((get-it (ins)
           (xml-transposition
            (get-data ins +slippery-chicken-standard-instrument-palette+))))
    (sc-test-check
      (equalp '(-2 0 -3) (get-it 'a-clarinet))
      (equalp '(0 1 0) (get-it 'piccolo))
      (equalp '(0 -1 0) (get-it 'double-bass))
      (equalp '(-1 0 -2) (get-it 'b-flat-clarinet))
      (equalp '(-5 0 -9) (get-it 'alto-sax))
      (equalp '(-3 0 -5) (get-it 'alto-flute))
      (not (get-it 'flute))
      (equalp '(-5 -1 -9) (get-it 'baritone-sax)))))

;;; MDE Mon Sep 24 16:02:23 2018 
(sc-deftest test-instrument-set-standard-range ()
  (load (file-from-sc-dir "src/instruments.lsp"))
  (let ((cl (get-standard-ins 'b-flat-clarinet))
        (fl (get-standard-ins 'flute)))
    (sc-test-check
      (set-standard-range 'b-flat-clarinet 'g3 'c6)
      (eq 'f3 (id (lowest-sounding cl)))
      (eq 'bf5 (id (highest-sounding cl)))
      (set-standard-range 'b-flat-clarinet 'g3 'c6 t)
      (eq 'a3 (id (lowest-written cl)))
      (eq 'd6 (id (highest-written cl)))
      (set-standard-range 'b-flat-clarinet 'd3 nil t)
      (eq 'e3 (id (lowest-written cl)))
      (eq 'd6 (id (highest-written cl))) ; unchanged
      (set-standard-range 'b-flat-clarinet nil 'g6 t)
      (eq 'e3 (id (lowest-written cl))) ; unchanged
      (eq 'a6 (id (highest-written cl)))
      (set-standard-range 'b-flat-clarinet 'c4 nil)
      (eq 'bf3 (id (lowest-sounding cl)))
      (eq 'g6 (id (highest-sounding cl))) 
      (set-standard-range 'b-flat-clarinet nil 'c7)
      (eq 'c4 (id (lowest-written cl)))
      (eq 'bf6 (id (highest-sounding cl)))
      (set-standard-range 'flute 'd4 (make-pitch 'a6))
      (eq 'd4 (id (lowest-written fl)))
      (eq 'd4 (id (lowest-sounding fl)))
      (eq 'a6 (id (highest-sounding fl)))
      (eq 'a6 (id (highest-written fl))))
    (load (file-from-sc-dir "src/instruments.lsp"))))

;;; MDE Tue Jan  8 18:18:08 2019
(sc-deftest test-instrument-harmonics ()
  (let* ((vln (get-standard-ins 'violin))
         (os (open-strings vln))
        h)
    (setf (open-strings vln) '(e5 a4 d4 g3))
    (sc-test-check
      (setq h (natural-harmonic? vln 'b5))
      (pitch= h (make-pitch 'b5))
      (equalp (marks h) '(iv harm))
      ;; with only 10 cents tolerance we fail
      (not (natural-harmonic? vln 'b5 10)) 
      (setq h (natural-harmonic? vln 'gs7))
      (pitch= h (make-pitch 'gs7))
      (equalp (marks h) '(i harm))
      (setq h (natural-harmonic? (get-standard-ins 'double-bass) 'b5))
      (pitch= h (make-pitch 'b5))
      (equalp (marks h) '(i harm))
      (setq h (natural-harmonic? (get-standard-ins 'guitar) 'fs6))
      (pitch= h (make-pitch 'fs6))
      (equalp (marks h) '(c2 harm))
      (not (natural-harmonic? vln 'eqs6))
      (setf (open-strings vln) '(eqs5 a4 d4 g3)) 
      (setq h (natural-harmonic? vln 'eqs6)) ; possible with scordatura
      (pitch= h (make-pitch 'eqs6))
      (equalp (marks h) '(i harm))
      (setq h (natural-harmonic? (get-standard-ins 'harp) 'e6))
      (pitch= h (make-pitch 'e6))
      (equalp (marks h) '(harm))
      (not (natural-harmonic? (get-standard-ins 'harp) 'c0)) ; too low
      (natural-harmonic? (get-standard-ins 'harp) 'b1) ; first available
      (natural-harmonic? (get-standard-ins 'harp) 'gs8) ; last available
      (not (natural-harmonic? (get-standard-ins 'harp) 'a8)) ; too high
      (setf (open-strings vln) '(e5 a4 d4 g3))
      ;; test the node
      (setq h (nth-value 1 (natural-harmonic? vln 'fs6)))
      (pitch= h (make-pitch 'fs4))
      (equalp (marks h) '(flag-head))
      (setf (open-strings vln) os) ; set back to what they were beforehand
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instrument-palette tests

;;; SAR Sat Jan 14 19:29:03 GMT 2012: Optimized via EVERY
;;; SAR Sat Jan  7 14:25:52 EST 2012
(sc-deftest test-instrument-palette-make-instrument-palette ()
  (let ((ip 
         (make-instrument-palette 
          'inst-pal 
          '((piccolo (:transposition-semitones 12 :lowest-written d4
                      :highest-written c6))    
            (bf-clarinet (:transposition-semitones -2 :lowest-written e3
                          :highest-written c6))  
            (horn (:transposition f :transposition-semitones -7 
                   :lowest-written f2 :highest-written c5))   
            (violin (:lowest-written g3 :highest-written c7 :chords t))  
            (viola (:lowest-written c3 :highest-written f6 :chords t)))))) 
    (sc-test-check
      (instrument-palette-p ip)
      (equalp (loop for inst in (data ip) collect (id inst))
              '(PICCOLO BF-CLARINET HORN VIOLIN VIOLA))
      (every #'instrument-p (data ip))
      (equalp (loop for inst in (data ip) 
                 collect (transposition inst)
                 collect (transposition-semitones inst)
                 collect (data (lowest-written inst))
                 collect (data (highest-written inst))
                 collect (chords inst))
              '(C 12 D4 C6 NIL BF -2 E3 C6 NIL F -7 F2 C5 NIL C 0 G3 C7 T C 0
                C3 F6 T)))))

;;; SAR Sat Jan  7 14:38:58 EST 2012
(sc-deftest test-instrument-palette-set-prefers-low ()
  (let ((ip (make-instrument-palette 'inst-pal 
                                     '((piccolo (:transposition-semitones 12 
                                                 :lowest-written d4
                                                 :highest-written c6))   
                                       (viola (:lowest-written c3
                                               :highest-written f6 
                                               :chords t))))))
    (sc-test-check
      (equalp (set-prefers-low ip 'piccolo) 'LOW))))

;;; SAR Sat Jan  7 15:41:42 EST 2012
(sc-deftest test-instrument-palette-set-prefers-high ()
  (let ((ip (make-instrument-palette 'inst-pal 
                                     '((piccolo (:transposition-semitones 12 
                                                 :lowest-written d4
                                                 :highest-written c6))   
                                       (viola (:lowest-written c3
                                               :highest-written f6 
                                               :chords t))))))
    (sc-test-check
     ;; MDE Fri Jul 27 17:18:51 2018
     (auto-set-subset-id ip)
     (eq 'piccolo (subset-id (get-data 'piccolo ip)))
     (eq 'viola (subset-id (get-data 'viola ip)))
     (equalp (set-prefers-high ip 'piccolo) 'HIGH))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; player tests

;;; SAR Sat Jan  7 17:06:59 EST 2012
(sc-deftest test-player-make-player ()
  (let* ((ip (make-instrument-palette 
              'inst-pal 
              '((picc (:transposition-semitones 12 :lowest-written d4
                       :highest-written c6)) 
                (flute (:lowest-written c4 :highest-written d7))  
                (clar (:transposition-semitones -2 :lowest-written e3
                       :highest-written c6))  
                (horn (:transposition f :transposition-semitones -7
                       :lowest-written f2 :highest-written c5))    
                (vln (:lowest-written g3 :highest-written c7 :chords t))  
                (vla (:lowest-written c3 :highest-written f6 :chords t)))))  
         (plr1 (make-player 'player-one ip '(flute picc) 
                            :midi-channel 1
                            :microtones-midi-channel 2))
         (plr2 (make-player 'player-two ip 'flute))
         (plr3 (make-player 'player-three ip '(flute))))
    (sc-test-check
      (player-p plr1)
      (player-p plr2)
      (equalp (loop for i in (data (data plr1)) collect (id i))
              '(FLUTE PICC))
      (equalp (loop for i in (data (data plr1))
                 collect (data (lowest-written i))
                 collect (data (highest-written i))
                 collect (transposition-semitones i))
              '(C4 D7 0 D4 C6 12))
      (= (midi-channel plr1) 1)
      (= (microtones-midi-channel plr1) 2)
      (equalp (id (data plr2)) 'flute)
      ;; MDE Fri Aug 23 09:39:59 2013 
      (reset-instrument-stats plr1)
      (reset-instrument-stats plr2)
      ;; SAR Wed Jan 11 21:22:18 GMT 2012: Added DOUBLES slot tests to see if
      ;; the single-item lists still come out as non-doubling
      (not (doubles plr2))
      (not (doubles plr3))
      (doubles plr1))))

;; SAR Sat Jan  7 18:42:59 EST 2012
(sc-deftest test-player-plays-transposing-instrument ()
  (let* ((ip +slippery-chicken-standard-instrument-palette+)
         (plr1 (make-player 'cl ip 'b-flat-clarinet))
         (plr2 (make-player 'fl ip 'flute))
         (plr3 (make-player 'fl ip '(flute alto-sax)))
         (plr4 (make-player 'db ip 'double-bass)))
    (sc-test-check
      (plays-transposing-instrument plr1)
      (not (plays-transposing-instrument plr2))
      (plays-transposing-instrument plr3)
      (not (plays-transposing-instrument plr4))
      (plays-transposing-instrument plr3 nil))))

;;; SAR Sat Jan  7 19:06:53 EST 2012
(sc-deftest test-player-microtonal-chords-p ()
  (let* ((ip +slippery-chicken-standard-instrument-palette+)
         (plr1 (make-player 'vln ip 'violin :microtones-midi-channel 2)) 
         (plr2 (make-player 'pno ip 'piano)))
    (sc-test-check
      (microtonal-chords-p plr1)
      (not (microtonal-chords-p plr2)))))

;;; SAR Thu Jan 12 18:56:50 GMT 2012
;;; This one is supposed to print one warning
(sc-deftest test-player-get-instrument ()
  (let* ((ip +slippery-chicken-standard-instrument-palette+)
         (plr1 (make-player 'pno ip 'piano))
         (plr2 (make-player 'percussion ip '(marimba vibraphone))))
    (sc-test-check
      (instrument-p (player-get-instrument plr1))
      (instrument-p (player-get-instrument plr2 'marimba))
      (equalp (id (player-get-instrument plr1)) 'piano)
      ;; MDE Fri Mar 17 13:18:56 2017 -- test T special case for ins
      (equalp (id (player-get-instrument plr2 'vibraphone)) 'vibraphone)
      (equalp (id (player-get-instrument plr2 T)) 'marimba)
      (equalp (id (player-get-instrument plr1 'marimba)) 'piano)
      (equalp (id (player-get-instrument plr2 'marimba)) 'marimba))))

;;; SAR Sun Apr 29 15:44:08 BST 2012
(sc-deftest test-player-total-notes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (violin :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5)))) 
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (va (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      ;; MDE Fri Oct 19 16:23:08 2018 -- try this too
      (change-pitch mini 1 1 'vn 'f3)
      (force-in-range mini nil)
      (pitch= (make-pitch 'f4) (pitch-or-chord (get-event mini 1 1 'vn)))
      (= 15 (total-notes (get-data 'vc (ensemble mini)))))))

;;; SAR Sun Apr 29 16:02:49 BST 2012
(sc-deftest test-player-tessitura-degree ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (violin :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (va (1 1 1 1 1))
                              (vc (1 1 1 1 1)))))))
        degs)
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (setf degs
            (loop for ne = (next-event mini 'vn)
               while ne
               unless (not (get-pitch-symbol ne))
               collect (degree (pitch-or-chord ne))))
      ;; (print degs)
      (equal-within-tolerance (tessitura-degree (get-data 'vn (ensemble mini)))
         (/ (apply #'+ degs) (length degs)))
      (not (next-event mini 'va nil 1))
      (setf degs
            (loop for ne = (next-event mini 'va)
               while ne
               unless (not (get-pitch-symbol ne))
               collect (degree (pitch-or-chord ne))))
      (equal-within-tolerance (tessitura-degree (get-data 'va (ensemble mini)))
         (/ (apply #'+ degs) (length degs)))
      (not (next-event mini 'vc nil 1))
      (setf degs
            (loop for ne = (next-event mini 'vc)
               while ne
               unless (not (get-pitch-symbol ne))
               collect (degree (pitch-or-chord ne))))
      (equal-within-tolerance (tessitura-degree (get-data 'vc (ensemble mini)))
         (/ (apply #'+ degs) (length degs))))))

;;; SAR Sun Apr 29 16:08:47 BST 2012
(sc-deftest test-player-total-duration ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (violin :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5)))) 
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                  :pitch-seq-palette ((1 2 3))))
                              (2 ((((2 4) (q) e (s) 32 32))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (va (2 2 2 2 2))
                              (vc (1 2 1 2 1))))))))
    (sc-test-check
      ;; MDE Thu Aug 28 20:03:33 2014 
      (balanced-load? (ensemble mini) :threshold .5)
      (not (balanced-load? (ensemble mini)))
      (= 6.875 (total-duration (get-data 'vn (ensemble mini))))
      (= 3.75 (total-duration (get-data 'va (ensemble mini))))
      (= 5.625 (total-duration (get-data 'vc (ensemble mini)))))))

;;; SAR Wed Aug  8 11:09:03 BST 2012
(sc-deftest test-player-total-degrees ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (violin :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (va (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (= 1730 (total-degrees (first (data (ensemble mini))))))))

;;; SAR Wed Aug  8 11:17:00 BST 2012
(sc-deftest test-player-total-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (violin :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (va (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (player-p (first (data (ensemble mini))))
      (= 5 (total-bars (first (data (ensemble mini))))))))

;;; SAR Wed Aug  8 11:22:34 BST 2012
(sc-deftest test-player-tessitura-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (violin :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (va (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (player-p (first (data (ensemble mini))))
      (equalp 'AQS3 (tessitura-note (first (data (ensemble mini))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sclist tests

(sc-deftest test-sclist-make-sclist ()
  (let ((scl1 (make-sclist '(1 2 3 4 5 6 7)))
        (scl2 (make-sclist '(dog cat cow sheep pig) :id 'animal-list
                           :bounds-alert nil :copy nil)))
    (sc-test-check
      (sclist-p scl1)
      (sclist-p scl2)
      (equalp (data scl1) (loop for i from 1 to 7 collect i))
      (equalp (data scl2) '(dog cat cow sheep pig))
      (equalp (id scl2) 'animal-list)
      ;; MDE Mon May 14 16:16:08 2012 -- 
      (sclist-econs scl1 0)
      (zerop (first (last (data scl1))))
      (not (bounds-alert scl2))
      (not (copy scl2)))))

;;; SAR Thu Jan 12 19:16:11 GMT 2012
(sc-deftest test-sclist-sc-subseq ()
  (let ((scl (make-sclist '(1 2 3 4 5 6 7 8 9))))
    (sc-test-check
      (listp (sc-subseq scl 2 7))
      (equalp (sc-subseq scl 2 7) '(3 4 5 6 7)))))

;;; SAR Thu Jan 12 21:25:55 GMT 2012
(sc-deftest test-sclist-sc-nthcdr ()
  (let ((scl (make-sclist '(0 1 2 3 4 5 6 7 8 9))))
    (sc-test-check
      (listp (sc-nthcdr 4 scl))
      (equalp (sc-nthcdr 2 scl) '(6 7 8 9))
      (not (sc-nthcdr 12 scl)))))

;;; SAR Thu Jan 12 22:25:29 GMT 2012
(sc-deftest test-sclist-sclist-econs ()
  (let ((scl (make-sclist '(0 1 2 3 4))))
    (sc-test-check
      (listp (sclist-econs scl 5))
      (equalp (sclist-econs scl 6) '(0 1 2 3 4 5 6)))))

;;; SAR Thu Jan 12 22:34:24 GMT 2012
;;; This is supposed to print one warning
(sc-deftest test-sclist-get-nth ()
  (let ((scl (make-sclist '(cat dog cow pig sheep))))
    (sc-test-check
      (equalp (get-nth 3 scl) ' PIG)
      (not (get-nth 31 scl)))))

;;; SAR Thu Jan 12 22:45:12 GMT 2012
;;; This is supposed to print one warning
(sc-deftest test-sclist-set-nth ()
  (let ((scl (make-sclist '(cat dog cow pig sheep))))
    (sc-test-check
      (symbolp (set-nth 3 'horse scl))
      (equalp (data scl) '(cat dog cow horse sheep))
      (not (set-nth 31 'goat scl)))))

;;; SAR Thu Jan 12 23:00:00 GMT 2012
(sc-deftest test-sclist-sclist-remove-elements ()
  (let ((scl (make-sclist '(0 1 2 3 4 5 6 7 8 9))))
    (sc-test-check
      (sclist-p (sclist-remove-elements scl 2 3))
      (equalp (data scl) '(0 1 5 6 7 8 9)))))

;;; SAR Thu Jan 12 23:05:31 GMT 2012
(sc-deftest test-sclist-combine ()
  (let ((scl1 (make-sclist '(0 1 2 3 4)))
        (scl2 (make-sclist '(5 6 7 8 9))))
    (sc-test-check
      (sclist-p (combine scl1 scl2))
      (equalp (data (combine scl1 scl2)) '(0 1 2 3 4 5 6 7 8 9)))))

;;; MDE Mon May 14 21:33:20 2012
(sc-deftest test-sclist-copy ()
  (let* ((list (list (make-pitch 'cs4) 1 2 3 4))
         (scl1 (make-sclist list :copy t))
         (scl2 (make-sclist list :copy nil)))
    (sc-test-check
      (setf (data (get-nth 0 scl1)) 'd8)
      (equalp (data (get-nth 0 scl1)) 'd8)
      (equalp 'cs4 (data (first list)))
      (setf (data (get-nth 0 scl2)) 'd6)
      (equalp 'd6 (data (first list))))))

(sc-deftest test-sclist-max-items ()
  (flet ((doit (list result max from)
           (equalp result (data (max-items (make-sclist list) max from)))))
    (sc-test-check
      (doit '(1 2 3 4 5 6 7) '(1 2 3) 3 'start)
      (doit '(1 2 3 4 5 6 7) '(5 6 7) 3 'end)
      (doit '(1 2 3 4 5 6 7) '(3 4 5) 3 'middle)
      (doit '(1 2 3 4 5 6 7 8) '(4 5 6) 3 'middle)
      (doit '(1 2 3 4 5 6 7 8) '(3 4 5 6) 4 'middle)
      (doit '(1 2 3 4 5 6 7 8) '(5 6 7 8) 4 'end)
      (doit '(1 2 3 4 5 6 7 8) '() 0 'start)
      (doit '(1 2 3 4 5 6 7) '(4) 1 'middle)
      (doit '(1 2 3 4 5 6 7) '(7) 1 'end))))

(sc-deftest test-sclist-limits ()
  (let ((scl (make-sclist '(-1.4 5 6 2.3 7.3 -2.1))))
    (multiple-value-bind (min max) (limits scl)
      (sc-test-check
        (equal-within-tolerance -2.1 min)
        (equal-within-tolerance 7.3 max)
        (new-limits scl)
        (multiple-value-bind (minn maxx) (limits scl)
          (equal-within-tolerance 0 minn)
          (equal-within-tolerance 100 maxx))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; circular-sclist tests

(sc-deftest test-circular-sclist-make-cscl ()
  (let ((cscl1 (make-cscl '(1 2 3 4)))
        (cscl2 (make-cscl '((1 (4 5 6))
                            (2 (7 8 9))
                            (3 (10 11 12)))))
        (cscl3 (make-cscl '(1 2 3 4) :id 'test-cscl))
        (cscl4 (make-cscl '(1 2 3 4) :bounds-alert nil)))
    (sc-test-check
      ;; cscl1
      (cscl-p cscl1)
      (equalp (data cscl1) '(1 2 3 4))
      (bounds-alert cscl1)
      (copy cscl1)
      ;; cscl2
      (cscl-p cscl2)
      (equalp (data cscl2) '((1 (4 5 6)) (2 (7 8 9)) (3 (10 11 12))))
      ;; cscl3
      (cscl-p cscl3)
      (equalp (id cscl3) 'test-cscl)
      ;; cscl4
      (cscl-p cscl4)
      (not (bounds-alert cscl4))
      (not (get-nth 41 cscl4)))))

;;; SAR Fri Jan 13 12:44:11 GMT 2012
(sc-deftest test-circular-sclist-get-next ()
  (let ((cscl (make-cscl '(0 1 2 3 4))))
    (sc-test-check
      (equalp (loop repeat 10 collect (get-next cscl)) 
              '(0 1 2 3 4 0 1 2 3 4)))))

;;; SAR Fri Jan 13 12:51:49 GMT 2012
(sc-deftest test-circular-sclist-get-last ()
  (let ((cscl (make-cscl '(0 1 2 3 4))))
    (sc-test-check
      (= (get-last cscl) 4)
      (not (loop repeat 7 do (get-next cscl)))
      (= (get-last cscl) 1))))

;;; SAR Fri Jan 13 13:01:05 GMT 2012
(sc-deftest test-circular-sclist-at-start ()
  (let ((cscl (make-cscl '(0 1 2 3 4))))
    (sc-test-check
      (at-start cscl)
      (not (loop repeat 7 do (get-next cscl)))
      (not (at-start cscl)))))

;;; SAR Fri Jan 13 13:22:29 GMT 2012
(sc-deftest test-circular-sclist-reset ()
  (let ((cscl (make-cscl '(0 1 2 3 4))))
    (sc-test-check
      ;; MDE Mon Dec 17 11:25:59 2012 -- can now ask to reset to too high a
      ;; position and it will be mod'ed 
      (reset cscl 12)
      (= (current cscl) 2)
      (reset cscl)
      (= (get-next cscl) 0)
      (not (loop repeat 8 do (get-next cscl)))
      (= (get-next cscl) 4)
      (reset cscl)
      (= (get-next cscl) 0)
      (not (loop repeat 8 do (get-next cscl)))
      (reset cscl 1)
      (= (get-next cscl) 1)
      (not (loop repeat 8 do (get-next cscl)))
      (reset cscl 2)
      (= (get-last cscl) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l-for-lookup tests

;;; SAR Fri Jan 13 21:47:43 GMT 2012
(sc-deftest test-l-for-lookup-make-l-for-lookup ()
  (let ((lfl (make-l-for-lookup
              'lfl-test
              '((1 ((2 3 4) (5 6 7)))
                (2 ((3 4 5) (6 7 8)))
                (3 ((4 5 6) (7 8 9))))
              '((1 (3 2)) (2 (3 1)) (3 (1 2)))
              :offset 1
              :scaler 0
              :auto-check-redundancy nil)))
    (sc-test-check
      (equalp (id lfl) 'lfl-test)
      (assoc-list-p (rules lfl))
      (equalp (loop for nobj in (data (rules lfl)) collect (data nobj))
              '((3 2) (3 1) (1 2)))
      (listp (data lfl))
      (equalp (loop for nobj in (data lfl) 
                 collect (loop for cscl in (data nobj)
                            collect (cscl-p cscl)))
              '((T T) (T T) (T T)))
      (equalp (loop for cscl in (data lfl) 
                 collect (loop for nobj in (data cscl) collect (data nobj)))
              '(((2 3 4) (5 6 7)) ((3 4 5) (6 7 8)) ((4 5 6) (7 8 9)))))))

;;; SAR Sat Jan 14 13:15:07 GMT 2012
(sc-deftest test-l-for-lookup-get-l-sequence ()
  (let* ((lfl (make-l-for-lookup 'lfl-test
                                 NIL
                                 '((1 (2))
                                   (2 (1 3))
                                   (3 (3 2)))))
         (lflgls (get-l-sequence lfl 1 29)))
    (sc-test-check
      (listp lflgls)
      (= (length lflgls) 29)
      (= (length (nth-value 1 (get-l-sequence lfl 1 29))) 3)
      (equalp lflgls 
              '(2 3 2 3 2 1 3 2 3 2 3 2 1 3 2 3 2 1 3 3 2 1 3 2 3 2 3 2 1))
      (equalp (nth-value 1 (get-l-sequence lfl 1 29)) '(5 12 12)))))

;;; SAR Sat Jan 14 13:50:22 GMT 2012
(sc-deftest test-l-for-lookup-do-lookup ()
  (let ((lfl (make-l-for-lookup 
              'lfl-test
              '((1 ((ax1) (ay1 ay2) (az1 az2 az3)))
                (2 ((bx1) (by1 by2) (bz1 bz2 bz3)))
                (3 ((cx1) (cy2 cy2) (cz1 cz2 cz3))))
              '((1 (1 2 1))
                (2 (2 1 3))
                (3 (2 3 2))))))
    (sc-test-check
      (equalp 
       (nth-value 0 (do-lookup lfl 2 17))
       '(BX1 AX1 CY2 AY1 BY1 AY2 BY2 CY2 BY1 AY1 BY2 AY2 BZ1 AZ1 CZ1 AZ2 BZ2)) 
      ;; MDE Thu Jun 13 12:25:04 2013 -- need to reset now as it's not always
      ;; done in do-lookup-aux 
      (reset lfl)
      (equalp 
       (nth-value 1 (do-lookup lfl 2 17))
       '((CY2 2) (BY1 2) (AY1 2) (BY2 2) (AY2 2) (BX1 1) (AX1 1) (BZ1 1)
         (AZ1 1) (CZ1 1) (AZ2 1) (BZ2 1)))
      (equalp 
       (nth-value 2 (do-lookup lfl 2 17))
       '(2 1 3 1 2 1 2 3 2 1 2 1 2 1 3 1 2)))))

;; SAR Sat Jan 14 14:22:33 GMT 2012
(sc-deftest test-l-for-lookup-do-simple-lookup ()
  (let ((lfl (make-l-for-lookup 
              'lfl-test
              '((1 ((ax1 ax2 ax3) (ay1 ay2 ay3) (az1 az2 az3)))
                (2 ((bx1 bx2 bx3) (by1 by2 by3) (bz1 bz2 bz3)))
                (3 ((cx1 cx2 cx3) (cy2 cy2 cy3) (cz1 cz2 cz3))))
              '((1 (1 2 1))
                (2 (2 1 3))
                (3 (2 3 2))))))
    (sc-test-check
      (equalp (do-simple-lookup lfl 2 17)
              '((BX1 BX2 BX3) (AX1 AX2 AX3) (CX1 CX2 CX3) (AX1 AX2 AX3) 
                (BX1 BX2 BX3) (AX1 AX2 AX3) (BX1 BX2 BX3) (CX1 CX2 CX3) 
                (BX1 BX2 BX3) (AX1 AX2 AX3) (BX1 BX2 BX3) (AX1 AX2 AX3) 
                (BX1 BX2 BX3) (AX1 AX2 AX3) (CX1 CX2 CX3) (AX1 AX2 AX3) 
                (BX1 BX2 BX3))))))

;;; SAR Sat Jan 14 15:00:36 GMT 2012
(sc-deftest test-l-for-lookup-get-linear-sequence ()
  (let ((lfl (make-l-for-lookup 'lfl-test
                                nil
                                '((1 (2 3))
                                  (2 (3 1 2))
                                  (3 (1))))))
    (sc-test-check
      (equalp (get-linear-sequence lfl 1 23)
              '(1 2 3 1 3 1 2 1 3 1 2 2 3 1 3 1 2 1 3 1 2 2 3)))))

;;; SAR Sat Jan 14 16:14:21 GMT 2012
(sc-deftest test-l-for-lookup-count-elements ()
  (let ((l '(1 4 5 7 3 4 1 5 4 8 5 7 3 2 3 6 3 4 5 4 1 4 8 5 7 3 2))
        ;; MDE Thu Jan  3 16:18:19 2013 -- make sure it works with symbols too,
        ;; as claimed 
        (ls '(ba ba black sheep sheep sheep)))
    (sc-test-check
     (equalp (count-elements ls)
             '((sheep 3) (ba 2) (black 1)))
     (equalp (count-elements l)
             '((1 3) (2 2) (3 5) (4 6) (5 5) (6 1) (7 3) (8 2))))))

;;; SAR Sat Jan 14 16:39:16 GMT 2012
(sc-deftest test-l-for-lookup-fibonacci ()
  (sc-test-check
    (= (nth-value 1 (fibonacci 5000)) 
       4181 
       (+ (+ 1597 987) 1597)
       (1+ (+ 1597 987 610 377 233 144 89 55 34 21 13 8 5 3 2 1 1 0)))
    (equalp (fibonacci 5000) '(1597 987 610 377 233 144 89 55 34 21 13 8 5 3 2  
                               1 1 0))))

;;; SAR Mon Jan 16 15:47:23 GMT 2012: Modified
;;; SAR Sat Jan 14 17:25:24 GMT 2012
(sc-deftest test-l-for-lookup-fibonacci-start-at-2 ()
  (sc-test-check
    (= (nth-value 1 (fibonacci-start-at-2 18)) 
       18
       (apply '+ (fibonacci-start-at-2 18)))
    (equalp (fibonacci-start-at-2 18) '(8 5 3 2))))

;;; MDE Mon Aug  5 16:15:03 2019
(sc-deftest test-l-for-lookup-fibonacci-divide ()
  (sc-test-check
   (equalp (fibonacci-divide 400 nil nil)
           '(145 234 289 323 344 357 365 370 373 375 376 377))
   (equalp (fibonacci-divide 400)
           '(154 248 307 343 365 379 387 393 396 398 399 400))
   (equalp (fibonacci-divide 40)
           '(16 26 32 35 38 39 40))
   (equalp (fibonacci-divide 10)
           '(5 8 9 10))
   (equalp (fibonacci-divide 2)
           '(1 2))
   (equalp (fibonacci-divide 20000)
           '(7640 12362 15280 17083 18198 18887 19312 19575 19738 19839 19901
             19939 19963 19977 19986 19992 19995 19998 19999 20000))
   (equalp (fibonacci-divide 200 nil)
           '(56 90 111 124 132 137 140 142 143 144))
   (float-list= (fibonacci-divide 200 t nil)
                 '(77.77777 124.99999 154.16666 172.22221 183.33333 190.27777
                   194.44444 197.22221 198.6111 200.0))))


;;; SAR Sat Jan 14 17:46:05 GMT 2012
(sc-deftest test-l-for-lookup-fibonacci-transition ()
  (sc-test-check
    (equalp (fibonacci-transition 31)
            '(0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 1 1 0 1 0 1 1 0 1 1 1 1 1)) 
    (equalp (fibonacci-transition 23 11 37)
            '(11 11 11 11 37 11 11 37 11 37 11 37 11 37 37 11 37 11 37 11 37 37 
              37))
    ;; MDE Wed May 25 19:45:34 2016 -- test the morphing
    (equalp (fibonacci-transition 50 2.3 5.4 t)
            '(2.3 #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.08333333333333333d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.16666666666666666d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.25d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.3333333333333333d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.41666666666666663d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.49999999999999994d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.5833333333333333d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.6666666666666666d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.8333333333333334d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.9166666666666667d0) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.5d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.25d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.0d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.25d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.5d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75d0) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.8)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.6)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.4)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.7) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.7)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.85) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4 5.4 5.4 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4 5.4 5.4 5.4 5.4
              5.4 5.4 5.4))
    (equalp (fibonacci-transition 50 2.3 5.4  '(0 0 20 1 100 1))
            '(2.3 2.3 2.3 2.3 2.3 2.3 2.3 5.4 5.4 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.8333333333333334d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.9166666666666667d0) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.5d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.25d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.0d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.25d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.5d0)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75d0) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.8)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.6)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.4)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.7) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.7)
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.85) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4 5.4 5.4 5.4
              #S(MORPH :I1 2.3 :I2 5.4 :PROPORTION 0.75) 5.4 5.4 5.4 5.4 5.4
              5.4 5.4 5.4))
    (equalp (fibonacci-transition 27 '(1 2 3) '(5 6 7))
            '((1 2 3) (1 2 3) (1 2 3) (1 2 3) (5 6 7) (1 2 3) (1 2 3) (5 6 7) 
              (1 2 3) (1 2 3) (5 6 7) (1 2 3) (5 6 7) (1 2 3) (5 6 7) (1 2 3) 
              (5 6 7) (5 6 7) (1 2 3) (5 6 7) (5 6 7) (1 2 3) (5 6 7) (5 6 7) 
              (5 6 7) (5 6 7) (5 6 7)))))

;;; SAR Sat Jan 14 18:20:29 GMT 2012
(sc-deftest test-l-for-lookup-fibonacci-transitions ()
  (sc-test-check
    (equalp (fibonacci-transitions 76 4)
            '(0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 2 1
              1 2 1 2 1 2 2 1 2 1 2 2 2 2 2 2 2 3 2 2 3 2 3 2 3 3 2 3 2 3 3 3 2
              3 3 3 3 3 3 3 3 3 3)) 
    (equalp (fibonacci-transitions 152 '(1 2 3 4))
            '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 2 1 1
              2 1 2 1 2 2 1 2 1 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3
              2 2 3 2 2 3 2 3 2 3 3 2 3 2 3 3 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 4
              3 3 3 3 4 3 3 4 3 3 4 3 4 3 4 4 3 4 3 4 4 3 4 4 3 4 4 4 4 4 3 4 4
              4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4))
    (equalp (fibonacci-transitions 52 '(1 2 3 4) t)
            '(1 #S(MORPH :I1 1 :I2 2 :PROPORTION 0.25d0)
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.5d0)
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.75d0) 2
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.9)
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.0d0)
              2 #S(MORPH :I1 1 :I2 2 :PROPORTION 0.75) 2
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.75) 2 2 2
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.75) 2 2 2 2
              #S(MORPH :I1 2 :I2 3 :PROPORTION 0.5d0) 3
              #S(MORPH :I1 2 :I2 3 :PROPORTION 0.75) 3
              #S(MORPH :I1 2 :I2 3 :PROPORTION 0.75) 3 3 3
              #S(MORPH :I1 2 :I2 3 :PROPORTION 0.75) 3 3 3 3
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.5d0) 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4 4 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4 4 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4 4 4 4 4 4 4))
    (equalp (fibonacci-transitions 52 '(1 2 3 4)  '(0 0 10 1 52 1))
            '(1 1 1 2 2 2 1 2 2 2
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.75) 2 2 2
              #S(MORPH :I1 1 :I2 2 :PROPORTION 0.75) 2 2 2 2 2 3
              #S(MORPH :I1 2 :I2 3 :PROPORTION 0.75) 3
              #S(MORPH :I1 2 :I2 3 :PROPORTION 0.75) 3 3 3
              #S(MORPH :I1 2 :I2 3 :PROPORTION 0.75) 3 3 3 3 3 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4 4 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4 4 4
              #S(MORPH :I1 3 :I2 4 :PROPORTION 0.75) 4 4 4 4 4 4
              4))
    (equalp (fibonacci-transitions 45 '((1 2 3) (4 5 4) (3 2 1)))
            '((1 2 3) (1 2 3) (1 2 3) (1 2 3) (1 2 3) (4 5 4) (1 2 3) (1 2 3)
              (4 5 4) (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3) 
              (4 5 4) (1 2 3) (4 5 4) (4 5 4) (4 5 4) (4 5 4) (4 5 4) (3 2 1) 
              (4 5 4) (3 2 1) (4 5 4) (3 2 1) (4 5 4) (3 2 1) (4 5 4) (3 2 1) 
              (4 5 4) (3 2 1) (3 2 1) (3 2 1) (4 5 4) (3 2 1) (3 2 1) (3 2 1) 
              (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1)))))

;;; SAR Fri Mar  2 14:26:04 GMT 2012
(sc-deftest test-l-for-lookup-remix-in ()
  (let* ((fts (fibonacci-transitions 320 '(1 2 3 4 5)))
         (rmir (remix-in fts :replace t)))
  (sc-test-check
    (= 320 (length rmir))
    (equalp 
     rmir
     '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1
       1 2 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 2 1 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 2
       1 2 2 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 1 2 1 2 1
       1 1 2 1 1 1 3 2 1 3 1 2 1 1 1 2 1 1 1 2 3 1 3 1 3 1 1 1 2 1 1 1 3 3 2 3
       1 3 1 1 1 3 1 1 1 3 3 1 3 1 3 1 1 1 3 2 1 1 3 3 1 3 1 3 1 1 1 3 2 1 1 3
       3 1 3 1 4 2 1 1 4 2 1 1 4 3 2 3 1 3 2 1 2 4 1 2 1 4 4 2 3 1 4 2 1 2 4 1
       2 2 4 4 1 4 2 4 2 1 2 4 2 2 2 4 4 1 4 2 4 2 2 2 4 2 2 2 4 5 2 4 2 4 2 2
       2 4 2 2 2 4 5 2 5 2 5 2 2 2 5 3 2 2 5 5 2 5 2 5 2 2 2 5 3 2 2 5 5 2 5 2
       5 3 2 2 5 3 2 2 5 5 3 5 2 5 3 2 3 5 2 3 2 5 5 3 5 2 5 3 2 3 5 2))
    (equalp
     (remix-in fts) 
     '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1
       1 2 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 2 1 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 2
       1 2 2 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 1 2 2 1
       2 2 1 2 1 3 1 2 2 1 2 1 2 1 3 2 2 1 3 2 1 2 3 1 2 1 3 1 2 3 1 2 1 3 1 2
       3 2 1 3 2 1 3 2 1 3 1 3 1 2 3 1 3 1 2 1 3 3 3 2 3 2 1 3 3 1 3 1 3 1 3 3
       1 3 1 3 1 3 3 3 1 3 3 1 3 3 1 3 1 3 1 3 3 2 3 1 4 1 3 3 3 1 3 3 1 3 3 1
       4 1 3 1 3 3 2 3 1 4 1 3 3 4 1 3 3 1 4 3 2 4 1 3 1 4 3 2 4 1 3 1 4 3 4 2
       3 4 1 3 4 2 4 1 3 2 4 4 1 3 2 4 1 4 4 4 2 3 4 1 4 4 2 4 1 4 2 4 4 1 4 2
       4 2 4 4 4 1 4 4 2 4 4 2 4 1 4 2 4 4 2 5 2 4 2 4 4 4 1 4 4 2 4 5 2 4 2 4
       2 4 4 2 5 2 4 2 4 5 4 2 4 5 2 4 5 2 4 2 5 2 4 5 2 4 2 5 2 4 5 4 2 5 4 2
       5 5 2 4 2 5 2 5 4 3 5 2 5 2 5 5 4 2 5 5 2 5 5 2 5 2 5 2 5 5 3 4 2 5 2 5
       5 5 2 5 5 2 5 5 3 5 2 5 2 5 5 3 5 2 5 2 5 5 5 3 5 5 2 5 5 3 5 2 5 3 5 5
       2 5 3 5 2 5 5 5 3 5 5 2 5 5 3 5 2 5 3 5 5 2))
    (equalp
     (remix-in fts)
     '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1
       1 2 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 2 1 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 2
       1 2 2 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 1 2 2 1
       2 2 1 2 1 3 1 2 2 1 2 1 2 1 3 2 2 1 3 2 1 2 3 1 2 1 3 1 2 3 1 2 1 3 1 2
       3 2 1 3 2 1 3 2 1 3 1 3 1 2 3 1 3 1 2 1 3 3 3 2 3 2 1 3 3 1 3 1 3 1 3 3
       1 3 1 3 1 3 3 3 1 3 3 1 3 3 1 3 1 3 1 3 3 2 3 1 4 1 3 3 3 1 3 3 1 3 3 1
       4 1 3 1 3 3 2 3 1 4 1 3 3 4 1 3 3 1 4 3 2 4 1 3 1 4 3 2 4 1 3 1 4 3 4 2
       3 4 1 3 4 2 4 1 3 2 4 4 1 3 2 4 1 4 4 4 2 3 4 1 4 4 2 4 1 4 2 4 4 1 4 2
       4 2 4 4 4 1 4 4 2 4 4 2 4 1 4 2 4 4 2 5 2 4 2 4 4 4 1 4 4 2 4 5 2 4 2 4
       2 4 4 2 5 2 4 2 4 5 4 2 4 5 2 4 5 2 4 2 5 2 4 5 2 4 2 5 2 4 5 4 2 5 4 2
       5 5 2 4 2 5 2 5 4 3 5 2 5 2 5 5 4 2 5 5 2 5 5 2 5 2 5 2 5 5 3 4 2 5 2 5
       5 5 2 5 5 2 5 5 3 5 2 5 2 5 5 3 5 2 5 2 5 5 5 3 5 5 2 5 5 3 5 2 5 3 5 5
       2 5 3 5 2 5 5 5 3 5 5 2 5 5 3 5 2 5 3 5 5 2))
    (equalp
     (remix-in fts :remix-in-fib-seed 3 :mirror t) 
     '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1
       1 2 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 2 1 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 2
       1 2 2 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 1 2 1 2 2
       1 2 1 2 2 1 3 1 2 2 1 2 1 2 3 1 2 1 2 3 1 2 1 2 3 1 2 1 3 2 1 3 1 2 3 1
       2 1 3 2 1 3 1 2 3 1 2 1 3 3 1 2 1 3 3 2 2 1 3 3 1 3 1 3 2 1 3 1 3 3 1 3
       1 3 3 1 3 1 3 3 1 3 1 3 3 1 3 2 3 3 1 3 1 3 3 1 3 1 3 4 1 3 1 3 3 1 3 2
       3 3 1 3 1 4 3 1 3 1 3 3 2 4 1 3 3 1 4 2 3 3 1 4 1 3 4 2 3 1 4 3 2 4 1 3
       4 2 3 1 4 3 2 4 1 3 4 2 4 1 3 4 2 4 1 3 4 2 4 1 4 4 2 3 2 4 4 1 4 2 4 4
       2 4 1 4 4 2 4 2 4 4 2 4 2 4 4 1 4 2 4 4 2 4 2 4 4 2 5 2 4 4 2 4 2 4 4 2
       4 2 4 5 2 4 2 4 4 2 4 2 5 4 2 4 2 5 4 2 4 2 5 4 2 5 2 4 5 2 4 3 5 4 2 5
       2 4 5 2 4 2 5 4 2 5 2 5 4 2 5 3 5 4 2 5 2 5 5 2 5 2 4 5 3 5 2 5 5 2 5 3
       5 5 2 5 2 4 5 3 5 2 5 5 3 5 2 5 5 3 5 2 5 5 3 5 2 5 5 3 5 2 5 5 3 5 2 5
       5 3 5 2 5 5 3 5 3 5 5 2 5 3 5 5 3 5 2 5 5 3 5 3 5 5 3 5 3 5 5 2 5 3 5 5
       3 5 3 5 5 3 5 3 5 5 3 5 3 5 5 3 5 3 5 5 3 5 3 5 5 3 5 3 5 5 3 5 3 5 5 3
       5 3 5 5 3 5 3 5 5 3 5 4 5 5 3 5 3 5 5 3 5 3 5 4 3 5 3 5 5 3 5 4 5 5 3 5
       3 5 4 3 5 3 5 5 4 5 3 4 5 3 5 4 4 5 3 5 3 4 5 4 4 3 5 4 4 5 3 4 5 4 4 3
       5 4 4 5 3 4 5 4 4 3 4 5 4 4 3 4 5 4 4 3 4 4 4 4 4 5 4 3 4 4 4 4 4 4 3 4
       4 4 5 4 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
       4 3 4 4 4 4 4 4 4 3 4 4 4 4 3 4 4 4 4 3 4 4 4 3 4 4 3 5 4 3 4 4 4 3 4 4
       3 4 4 3 4 4 4 3 3 4 4 5 3 3 4 4 4 3 3 4 3 4 3 4 5 3 4 3 3 4 3 5 3 3 4 3
       4 4 3 5 3 4 3 3 5 3 4 3 3 5 3 4 3 3 5 3 4 3 3 5 3 4 3 3 5 3 4 3 3 5 3 4
       2 3 5 3 5 3 3 4 2 5 3 3 5 2 4 3 3 5 2 5 3 2 5 3 5 2 3 4 2 5 3 2 5 3 5 2
       3 5 2 5 3 2 5 2 5 3 2 5 2 4 3 2 5 2 5 2 2 5 3 5 2 2 5 2 5 2 2 5 2 5 2 3
       5 2 5 2 2 5 2 5 2 2 5 2 5 2 2 5 2 5 2 2 5 2 5 2 2 5 2 5 2 2 5 2 5 2 1 5
       2 5 2 2 5 2 5 1 2 5 2 5 1 2 5 2 5 1 2 5 1 5 2 1 5 2 5 1 2 5 1 5 2 1 5 2
       5 1 2 5 1 5 1 2 5 1 5 1 2 5 1 5 1 1 5 1 5 2 1 5 1 5 1 1 5 1 5 1 1 5 2 5
       1 1 5 1 5 1 1 5 1 5 1 1 5 1 5 1 1 5 1 5 2 1 5 1 5 1 1 5 1 5 1 1 5 1 5 1
       1 5 1 5 1 1 5 1 5 1 1 5 1 5 1 1 5 1 5 1 1 5 1 4 1)))))

;;; MDE Fri Mar 29 15:02:39 2013 -- we can now use get-linear-sequence with
;;; lookup so test
(sc-deftest test-l-for-lookup-linear ()
  (let* ((tune1 (make-l-for-lookup
                 'tune1
                 '((1 ((2 1 8)))
                   (2 ((3 4)))
                   (3 ((4 5)))
                   (4 ((5 1 6)))
                   (5 ((6 5 7 4)))
                   (6 ((4 5)))
                   (7 ((4 5 1)))
                   (8 ((1))))
                 '((1 (1 2)) (2 (1 3 2)) (3 (1 4 3)) (4 (1 2 1)) (5 (5 3 1))
                   (6 (2 5 6)) (7 (5 6 4)) (8 (3 2)))))
         (tune2 (make-l-for-lookup
                 'tune2
                 nil
                 '((1 (2 1 8))
                   (2 (3 4))
                   (3 (4 5))
                   (4 (5 1 6))
                   (5 (6 5 7 4))
                   (6 (4 5))
                   (7 (4 5 1))
                   (8 (1)))))
         (result1 (do-lookup-linear tune1 1 100))
         (result2 (get-linear-sequence tune2 1 100)))
    ;; (print result1)
    (sc-test-check
      (equalp result1
              '(2 1 3 8 2 4 4 1 8 3 4 2 1 3 5 5 8 2 4 3 1 8 4 4 5 2 1 3 4 8 2 3
                4 1 4 3 1 8 4 5 4 2 1 3 4 8 2 3 5 6 1 8 4 3 2 1 4 4 5 8 2 3 4 1
                8 3 4 5 2 1 4 3 8 2 4 5 4 1 8 3 4 2 1 3 5 1 4 3 8 2 4 4 5 1 8 3
                4 2 1 3))
      (equalp (l-distribution tune1)
              '(44 35 16 5 0 0 0 0))
      (equalp (ll-distribution tune1)
              '((1 17) (2 15) (3 18) (4 25) (5 10) (6 1) (8 14)))
      (equalp (l-sequence tune1)
              '(1 1 2 1 1 2 3 1 1 2 2 1 1 2 3 4 1 1 2 2 1 1 2 3 3 1 1 2 2 1 1 2
                3 4 2 2 1 1 2 3 3 1 1 2 2 1 1 2 3 4 1 1 2 2 1 1 2 3 3 1 1 2 2 1
                1 2 3 4 1 1 2 2 1 1 2 3 3 1 1 2 2 1 1 2 3 4 2 2 1 1 2 3 3 1 1 2
                2 1 1 2))
      (equalp result2
              '(1 2 3 4 5 6 4 1 1 8 1 2 4 6 5 5 7 4 5 4 1 1 8 1 2 3 5 6 4 6 5 5
                7 5 4 5 6 4 1 1 8 1 2 4 6 5 5 7 1 1 8 1 2 3 4 5 4 1 1 8 1 2 4 6
                4 5 6 5 5 7 4 1 1 8 1 2 3 5 4 6 4 5 6 5 5 7 5 4 1 1 8 1 2 4 6 4
                5 6 5 5)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; permutations tests

;;; SAR Sat Jan 14 22:20:33 GMT 2012
(sc-deftest test-permutations-permutations ()
  (sc-test-check
    (equalp (permutations 4)
            '((0 1 2 3) (1 0 2 3) (0 2 1 3) (2 0 1 3) (1 2 0 3) (2 1 0 3) 
              (0 1 3 2) (1 0 3 2) (0 3 1 2) (3 0 1 2) (1 3 0 2) (3 1 0 2) 
              (0 2 3 1) (2 0 3 1) (0 3 2 1) (3 0 2 1) (2 3 0 1) (3 2 0 1) 
              (1 2 3 0) (2 1 3 0) (1 3 2 0) (3 1 2 0) (2 3 1 0) (3 2 1 0)))))

;;; SAR Sat Jan 14 22:38:20 GMT 2012
(sc-deftest test-permutations-permutate ()
  (sc-test-check
    (equalp (permutate '(a b c)) 
            '((A B C) (B A C) (A C B) (C A B) (B C A) (C B A)))))

;;; SAR Sun Jan 15 19:27:28 GMT 2012
(sc-deftest test-permutations-shuffle ()
  (let ((l1 '(0 1 2 3 4 5 6 7 8 9))
        (l2 '(a b c)))
    (sc-test-check
      (every #'equalp (shuffle l1) (shuffle l1))
      (every #'equalp 
             (subseq l1 0 3)
             (subseq (shuffle l1 :start 3 :end 6 :fix nil) 0 3))
      (every #'equalp
             (subseq l1 6)
             (subseq (shuffle l1 :start 3 :end 6 :fix nil) 6))
      (and 
       (every #'(lambda (x) (equalp (sort x #'string<) l2))
              (loop repeat 100 collect (shuffle l2 :fix nil)))
       (notevery #'(lambda (x) (equalp x l2))
                 (loop repeat 100 collect (shuffle l2 :fix nil)))))))

;;; SAR Sun Jan 15 21:32:46 GMT 2012
(sc-deftest test-permutations-multi-shuffle ()
  (let ((l1 '(a b c d e f g h i j))
        (l2 '(1 2 3)))
    (sc-test-check
      (every #'equalp (multi-shuffle l1 3) (multi-shuffle l1 3))
      (every #'equalp 
             (subseq l1 0 3)
             (subseq (multi-shuffle l1 3 :start 3 :end 6 :fix nil) 0 3))
      (every #'equalp
             (subseq l1 6)
             (subseq (multi-shuffle l1 3 :start 3 :end 6 :fix nil) 6))
      (and 
       (every #'(lambda (x) (equalp (sort x #'<) l2))
              (loop repeat 100 collect (multi-shuffle l2 3 :fix nil)))
       (notevery #'(lambda (x) (equalp x l2))
                 (loop repeat 100 collect (multi-shuffle l2 3 :fix nil)))))))

;;; SAR Mon Jan 16 17:03:16 GMT 2012
(sc-deftest test-permutations-inefficient-permutations ()
  (let ((inp (inefficient-permutations 4)))
    (sc-test-check
      (every #'(lambda (x) (equalp (sort x #'<) '(0 1 2 3)))
             (inefficient-permutations 4))
      (every #'(lambda (x) (member x (permutations 4) :test #'equalp))
             (inefficient-permutations 4))
      (every #'(lambda (x) (member x (inefficient-permutations 4) 
                                   :test #'equalp)) 
             (permutations 4))
      (some #'(lambda (x) (equalp x '(0 1 2 3)))
            (inefficient-permutations 4))
      (notevery #'(lambda (x) (equalp x '(0 1 2 3)))
                (inefficient-permutations 4))
      (every #'(lambda (x) (not x))
             (loop for p in inp
                do (setf inp (append (rest inp) (list (first inp))))
                collect (member (first inp) (rest inp) :test #'equalp))))))

;;; SAR Mon Jan 16 23:13:19 GMT 2012
(sc-deftest test-permutations-inefficiently-permutate ()
  (let ((ipl1 (inefficiently-permutate '(a b c)))
        (ipl2 (inefficiently-permutate '(a b c) :sublists t))
        (ipl3 (inefficiently-permutate '(a b c)))
        (ipl4 (inefficiently-permutate '(a b c) :max 3))
        ;; MDE Sat Nov 16 11:55:05 2013 -- just check that cloning works
        (ipl5 (inefficiently-permutate (map 'list #'make-rhythm '(q e. s))
                                       :clone t)))
    (sc-test-check
      ;; right number of elements, right elements, right number of each element 
      (equalp (sort ipl1 #'string<) 
              (let ((iplc '()))
                (loop for i in '(a b c)
                   do (loop repeat 6 do (push i iplc)))
                (reverse iplc)))
      ;; flat list is all symbols, no lists
      (every #'symbolp ipl1)
      ;; when :sublist T, result is list of lists
      (every #'listp ipl2)
      ;; each consecutive group of 3 in the flat list is a permutation of the 
      ;; original 
      (every #'(lambda (x) (equalp (sort x #'string<) '(a b c)))
             (loop for i from 0 below (/ (length ipl1) 3)
                collect (subseq ipl3 (* i 3) (+ (* i 3) 3))))
      ;; testing the max arg
      (= (length ipl4) 9)
      ;; testing that it always returns same list by default (:fix t)
      (let ((iplcol (loop repeat 4 
                       collect (inefficiently-permutate '(a b c)))))  
        (every #'(lambda (x) (equalp x (first iplcol)))
               (rest iplcol)))

      ;; test that it returns same elements and same number of each
      ;; element...
      (every #'(lambda (x) 
                 (equalp 
                  (sort x #'string<)
                  (sort (inefficiently-permutate '(a b c)) #'string<))) 
             (loop repeat 4 
                collect (inefficiently-permutate '(a b c) :fix nil))) 
      ;; but different orderings each time when :fix is set to NIL
      (let ((ipl-col-no-fix 
             (loop repeat 100 
                collect (inefficiently-permutate '(a b c) :fix nil))))
        (notevery #'(lambda (x) (equalp x (first ipl-col-no-fix)))
                  (rest ipl-col-no-fix))))))

;;; SAR Thu Feb 2 13:36:47 GMT 2012: Changed last test from 
;;; notany to notevery
;;; SAR Tue Jan 17 12:23:28 GMT 2012
(sc-deftest test-permutations-multi-shuffle-with-perms ()
  (let* ((l '(0 1 2 3 4))
         (mswp1 (multi-shuffle-with-perms l 7))
         (mswp-fix (loop repeat 10 collect (multi-shuffle-with-perms l 7)))
         (mswp-dif-num-shuf (loop for i from 0 to 5
                               collect (multi-shuffle-with-perms l i))))
    (sc-test-check
      ;; same number elements, same elements, same number of each element
      (equalp (sort mswp1 #'<) '(0 1 2 3 4))
      ;; always returns same perm for same <num-shuffles>
      (every #'(lambda (x) (equalp x (first mswp-fix)))
             (rest mswp-fix))
      ;; returns different perms for different <num-shuffles>, some of which
      ;; might be the same
      (notevery #'(lambda (x) (equalp x (first mswp-dif-num-shuf)))
              (rest mswp-dif-num-shuf)))))

;;; SAR Tue Jan 17 13:12:32 GMT 2012
(sc-deftest test-permutations-random-rep ()
  (let ((rr1 (loop repeat 100 collect (random-rep 5)))
        (rr2 (loop repeat 100 collect (random-rep 5 t))))
    (sc-test-check 
      ;; most of the returns are different by default
      (notevery #'(lambda (x) (equalp x (first rr1)))
                (rest rr1))
      ;; all of the returns are equal if <reset> is NIL
      (every #'(lambda (x) (equalp x (first rr2)))
             (rest rr2)))))
  
;;; SAR Tue Jan 17 13:48:49 GMT 2012
;;; This test is supposed to print one warning
(sc-deftest test-permutations-move-repeats ()
  (sc-test-check
    (equalp (move-repeats '((a b c) (c a b) (d e f) (a b c) (g h i)))
            '((A B C) (D E F) (C A B) (A B C) (G H I)))
    (equalp (move-repeats '(1 2 3 3 4 5 6 7 8 8 9 10))
            '(1 2 3 4 3 5 6 7 8 9 8 10))
    (equalp (move-repeats '((a b c d) (d c b a) (b c a d) (c a b d)))
            '((A B C D) (B C A D) (C A B D) (D C B A)))))

;;; MDE Tue Dec 12 13:58:19 2017 -- make sure Sebastian Wendt's identified
;;; problem no longer occurs.  
(defun chord-combine2 (ls n)
  (loop for elem in (list-permutations ls n)
     collect (sort elem  #'>)))

(sc-deftest test-list-permutations ()
  (sc-test-check
    (equalp '((3 2) (3 1) (3 2) (2 1) (3 1) (2 1))
            (chord-combine2 '(1 2 3) 2))))

(sc-deftest test-avoid-common-elements ()
  (let (tmp)
    (sc-test-check
      (equalp (avoid-common-elements
               '((1 2 3) (4 5 6) (5 4 6) (4 6 1) (2 3 4) (2 3 1) (1 3 2)))
              '((1 2 3) (4 5 6) (2 3 1) (5 4 6) (1 3 2) (4 6 1) (2 3 4)))
      (equalp (avoid-common-elements
               '((1 2 3) (4 5 6) (5 4 6) (3 2 1) (2 3 1) (6 5 4) (3 1 2)))
              '((1 2 3) (4 5 6) (3 2 1) (5 4 6) (2 3 1) (6 5 4) (3 1 2)))
      (setq tmp (avoid-common-elements
                 (shuffle (list-permutations '(1 2 3 4 5 6 7) 3) :fix t)))
      (= 210 (length tmp))
      ;; (print tmp)
      ;; MDE Fri Jan 22 14:05:14 2021, Heidhausen -- because we shuffled, the
      ;; random states will be different between lisp implementations 
      #+sbcl (equalp (first tmp) '(7 3 1))
      #+ccl (equalp (first tmp) '(6 5 2))
      (setq tmp (avoid-common-elements
                 (list-permutations '(1 2 3 4 5 6 7) 3)
                 :all-on-fail nil))
      (= 161 (length tmp))
      (setq tmp (avoid-common-elements (list-permutations '(1 2 3 4 5 6 7) 3)
                                       :accept 2))
      (equalp (first tmp) '(5 6 7)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recursive-assoc-list tests

;;; SAR Wed Jan 25 13:05:42 GMT 2012
(sc-deftest test-ral-make-ral ()
  (let ((ral-one (make-ral 'ral-1 '((1 one)
                                    (2 ((3 4) (5 6) (7 8)))
                                    (3 three))))
        (ral-two (make-ral 'ral-2 '((1 one)
                                    (2 ((3 4) (5 6) (7 8)))
                                    (3 three))
                           :recurse-simple-data nil))
        (ral-three (make-ral 'ral-3 '((1 one)
                                      (2 ((3 4) (5 6) (7 8)))
                                      (3 three))
                             :tag 'test-tag
                             :full-ref '(7 bob uncle))))
    (sc-test-check
      ;; ral-one
      (is-ral ral-one)
      (not (tag ral-one))
      (not (full-ref ral-one))
      ;; (change-class ral-one 'rthm-seq-map)
      ;; MDE Sat Jan 20 10:50:15 2018 
      (promote ral-one 'rthm-seq-map)
      ;; (print ral-one)
      (typep (get-data-data 2 ral-one) 'rthm-seq-map)
      (equalp (get-all-refs ral-one) '((1) (2 3) (2 5) (2 7) (3)))
      ;; ral-two
      (is-ral ral-two)
      (not (tag ral-two))
      (not (full-ref ral-two))
      (equalp (get-all-refs ral-two) '((1) (2) (3)))
      ;; ral-three
      (is-ral ral-three)
      (equalp (tag ral-three) 'test-tag)
      (equalp (full-ref ral-three) '(7 bob uncle)))))

;;; SAR Thu Jan 26 14:34:13 GMT 2012
(sc-deftest test-ral-get-previous ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon))))))))
        (al (make-assoc-list 'mixed-bag 
                             '((jim beam)
                               (wild turkey)
                               (four roses)))))
    (sc-test-check
      (let ((pno (get-previous ral '(wild))))
        (equalp (list (id pno) (data pno)) '(jim beam)))
      (let ((pno (get-previous ral '(four roses))))
        (equalp (list (id pno) (data pno)) '(wild turkey)))
      (let ((pno (get-previous ral '(four violets white))))
        (equalp (list (id pno) (data pno)) '(fox hole)))
      (let ((pno (get-previous ral '(four violets white) 4)))
        (equalp (list (id pno) (data pno)) '(blue velvet)))
      (equalp (get-previous ral '(four violets white) 14) -7)
      ;; MDE Tue Jun 27 11:17:45 2017
      (equalp '(four) (remove-when al #'(lambda (x) (eq (data x) 'roses))))
      ;; (print (get-all-refs ral))
      (= 8 (num-data ral))
      (= 8 (r-count-elements ral))
      (remove-when ral #'(lambda (x) (eq (id x) 'roses)))
      ;; (print 'here)
      (= 7 (r-count-elements ral))
      (= 7 (num-data ral))
      (equalp (get-all-refs ral)
              '((JIM) (WILD) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
                (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX)
                (FOUR VIOLETS WHITE)))
      (equalp '((JIM) (WILD) (FOUR VIOLETS RED FOX))
              (remove-when ral #'(lambda (x) (or (eq (id x) 'jim)
                                                 (eq (data x) 'hole)
                                                 (eq (data x) 'turkey)))))
      ;; test keys again
      (equalp (get-all-refs ral)
              '((FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
                (FOUR VIOLETS RED VIPER) (FOUR VIOLETS WHITE)))
      ;; (print (get-all-refs ral))
      )))

;;; SAR Thu Jan 26 18:34:48 GMT 2012
(sc-deftest test-ral-link-named-objects ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      ;; before
      (not (linked ral))
      (notany #'linked-named-object-p
              (data (get-data-data 'four ral)))
      (notany #'linked-named-object-p
              (data (get-data-data 'violets 
                                   (get-data-data 'four ral))))
      (notany #'linked-named-object-p
              (data (get-data-data 'red
                                   (get-data-data 'violets 
                                                  (get-data-data 'four ral)))))
      ;; after
      (link-named-objects ral)
      (linked ral)
      (every #'linked-named-object-p
             (data (get-data-data '(four violets red) ral)))
      (every #'(lambda (x) (or (linked-named-object-p x)
                               (is-ral (data x))))
             (data (get-data-data '(four violets) ral)))
      (every #'(lambda (x) (or (linked-named-object-p x)
                               (is-ral (data x))))
             (data (get-data-data 'four ral)))
      (every #'(lambda (x) (or (linked-named-object-p x)
                               (is-ral (data x))))
             (data ral)))))

;;; SAR Thu Jan 26 20:50:31 GMT 2012
(sc-deftest test-ral-relink-named-objects ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      ;; before
      (not (linked ral))
      (notany #'linked-named-object-p
              (data (get-data-data 'four ral)))
      (notany #'linked-named-object-p
              (data (get-data-data 'violets 
                                   (get-data-data 'four ral))))
      (notany #'linked-named-object-p
              (data (get-data-data 'red
                                   (get-data-data 'violets 
                                                  (get-data-data 'four ral)))))
      ;; after
      (relink-named-objects ral)
      (linked ral)
      (every #'linked-named-object-p
             (data (get-data-data '(four violets red) ral)))
      (every #'(lambda (x) (or (linked-named-object-p x)
                               (is-ral (data x))))
             (data (get-data-data '(four violets) ral)))
      (every #'(lambda (x) (or (linked-named-object-p x)
                               (is-ral (data x))))
             (data (get-data-data 'four ral)))
      (every #'(lambda (x) (or (linked-named-object-p x)
                               (is-ral (data x))))
             (data ral)))))

;;; SAR Thu Jan 26 20:56:46 GMT 2012
(sc-deftest test-ral-r-count-elements ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (= (r-count-elements ral) 8))))

;;; SAR Thu Jan 26 21:25:28 GMT 2012
;;; this one is supposed to print two warnings
(sc-deftest test-ral-get-data ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (equalp (list
               (id (get-data 'wild ral))
               (get-data-data 'wild ral))
              '(wild turkey))
      (equalp (list
               (id (get-data '(four violets white) ral))
               (get-data-data '(four violets white) ral))
              '(white ribbon))
      (not (get-data 'johnnie ral))
      (not (get-data 'fox ral)))))

;;; SAR Thu Jan 26 22:36:04 GMT 2012
(sc-deftest test-ral-add ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (= 3 (length (data ral)))
      (add '(makers mark) ral)
      (equalp (data (nth 3 (data ral))) 'mark)
      (add '(yellow sky) ral '(four violets))
      ;; MDE Tue Oct  1 10:56:59 2013 
      (add (make-named-object 'blue 'sky) ral '(four violets red))
      (equal 'sky (get-data-data '(four violets red blue) ral))
      (= 4 (length (data (get-data-data '(four violets) ral))))
      (equalp (data (nth 3 (data (get-data-data '(four violets) ral)))) 
              'sky))))

;;; SAR Fri Jan 27 14:48:02 GMT 2012
(sc-deftest test-ral-get-first ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (equalp (list
               (id (get-first ral))
               (data (get-first ral)))
              '(jim beam)))))

;;; SAR Fri Jan 27 14:58:33 GMT 2012
(sc-deftest test-ral-get-first-ref ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (equalp (get-first-ref ral) '(jim))
      (equalp (get-first-ref (get-data-data '(four violets) ral))
              '(four violets blue)))))

;;; SAR Fri Jan 27 15:04:12 GMT 2012
(sc-deftest test-ral-get-last ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (equalp (list
               (id (get-last ral))
               (data (get-last ral)))
              '(white ribbon)))))

;;; SAR Fri Jan 27 15:15:45 GMT 2012
(sc-deftest test-ral-get-all-refs ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (equalp (get-all-refs ral)
              '((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE) 
                (FOUR VIOLETS RED DRAGON) (FOUR VIOLETS RED VIPER) 
                (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE)))
      (equalp (get-all-refs ral nil)
              '(JIM WILD (FOUR ROSES) (FOUR VIOLETS BLUE) 
                (FOUR VIOLETS RED DRAGON) (FOUR VIOLETS RED VIPER) 
                (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE))))))

;;; SAR Fri Jan 27 15:23:18 GMT 2012
(sc-deftest test-ral-get-last-ref ()
  (let ((ral1 (make-ral 'mixed-bag 
                        '((jim beam)
                          (wild turkey)
                          (four ((roses red)
                                 (violets ((blue velvet)
                                           (red ((dragon den)
                                                 (viper nest)
                                                 (fox hole)))
                                           (white ribbon))))))))
        (ral2 (make-ral 'l-toons 
                        '((bugs bunny)
                          (daffy duck)
                          (porky pig)))))
    (sc-test-check
      (equalp (get-last-ref ral1)
              '(four violets white))
      (equalp (get-last-ref ral2)
              '(porky)))))

;;; SAR Fri Jan 27 16:20:58 GMT 2012
(sc-deftest test-ral-set-data ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon))))))))) 
    (sc-test-check
      (set-data 'jim '(makers mark) ral)
      (equalp (get-all-refs ral)
              '((MAKERS) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE)
                (FOUR VIOLETS RED DRAGON) (FOUR VIOLETS RED VIPER) 
                (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE)))
      (set-data '(wild) '(knob creek) ral)
      (equalp (get-all-refs ral)
              '((MAKERS) (KNOB) (FOUR ROSES) (FOUR VIOLETS BLUE)
                (FOUR VIOLETS RED DRAGON) (FOUR VIOLETS RED VIPER) 
                (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE)))
      (set-data '(four violets red fox) '(bee hive) ral)
      (equalp (get-all-refs ral)
              '((MAKERS) (KNOB) (FOUR ROSES) (FOUR VIOLETS BLUE)
                (FOUR VIOLETS RED DRAGON) (FOUR VIOLETS RED VIPER) 
                (FOUR VIOLETS RED BEE) (FOUR VIOLETS WHITE))))))

(sc-deftest test-ral-parcel-data ()
  (let* ((ral (make-ral 'mixed-bag 
                        '((jim beam)
                          (wild turkey)
                          (four ((roses red)
                                 (violets ((blue velvet)
                                           (red ((dragon den)
                                                 (viper nest)
                                                 (fox hole)))
                                           (white ribbon))))))))
         (pd-ral (parcel-data ral 'potpourri)))
    (sc-test-check
      (equalp (id (first (data pd-ral)))
              'potpourri)
      (equalp (get-all-refs ral)
              (get-all-refs (data (first (data pd-ral))))))))

;;; SAR Mon Jan 30 12:37:22 GMT 2012: Amended
;;; SAR Fri Jan 27 17:30:38 GMT 2012
(sc-deftest test-ral-add-empty-parcel ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon)))))))))
    (sc-test-check
      (add-empty-parcel ral 'bricolage)
      (equalp (id (nth 3 (data ral))) 'bricolage)
      (is-ral (data (nth 3 (data ral))))
      (not (data (data (nth 3 (data ral)))))
      (equalp (get-all-refs ral)
              '((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE) 
                (FOUR VIOLETS RED DRAGON) (FOUR VIOLETS RED VIPER) 
                (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE) (BRICOLAGE))))))

;;; SAR Fri Jan 27 17:44:13 GMT 2012
(sc-deftest test-ral-recursivep ()
  (let ((ral1 (make-ral 'mixed-bag 
                        '((jim beam)
                          (wild turkey)
                          (four ((roses red)
                                 (violets ((blue velvet)
                                           (red ((dragon den)
                                                 (viper nest)
                                                 (fox hole)))
                                           (white ribbon)))))))) 
        (ral2 (make-ral 'mixed-bag 
                        '((jim beam)
                          (wild turkey)
                          (four roses)))))
    (sc-test-check
      (recursivep ral1)
      (not (recursivep ral2)))))

;;; SAR Fri Jan 27 18:09:55 GMT 2012
(sc-deftest test-ral-lisp-assoc-listp ()
  (let ((lal1 '((roses red) (3 "allegro") (5 flute)))
        (lal2 '((1 2) (3 ((4 5) (6 7))) (8 9))))
    (sc-test-check
      (lisp-assoc-listp lal1)
      (lisp-assoc-listp lal2)
      (not (lisp-assoc-listp lal2 nil)))))

;;; SAR Fri Jan 27 18:20:35 GMT 2012
(sc-deftest test-ral-assoc-list-id-list ()
  (let ((alil1 '(jim beam 3 "Allegro" 5 flute))
        (alil2 '(jim beam 3 "Allegro" 5 (flute))))
    (sc-test-check
      (assoc-list-id-list alil1)
      (not (assoc-list-id-list alil2)))))

;;; MDE Mon May 14 19:46:00 2012 
(sc-deftest test-add-parcel ()
  (let ((ral (make-ral 'mixed-bag 
                       '((jim beam)
                         (wild turkey)
                         (four ((roses red)
                                (violets ((blue velvet)
                                          (red ((dragon den)
                                                (viper nest)
                                                (fox hole)))
                                          (white ribbon))))))))
        (refs nil))
    (sc-test-check
      (add-empty-parcel ral 'bricolage)
      (add-empty-parcel ral 'rsp 'rthm-seq-palette)
      (rsp-p (get-data-data 'rsp ral))
      (setf refs (get-all-refs ral))
      ;; (print refs)
      (equalp refs '((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE)
                     (FOUR VIOLETS RED DRAGON)  
                     (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX)
                     (FOUR VIOLETS WHITE) (BRICOLAGE) (rsp))))))

;;; MDE Thu May 17 09:39:23 2012 --   
(sc-deftest test-ral-econs ()
  (let ((ral1 (make-ral 'mixed-bag 
                        '((jim beam)
                          (wild turkey)
                          (four ((roses red)
                                 (violets ((blue velvet)
                                           (red ((dragon den)
                                                 (viper nest)
                                                 (fox hole)))
                                           (white ribbon))))))))
        (ral2 (make-ral 'mixed-bag 
                        '((jim beam)
                          (wild turkey)
                          (four ((roses red))))))
        (ral3 (make-ral nil nil))
        (ral4 (make-ral 'mixed-bag 
                        '((jim (beam))
                          (four ((roses red)
                                 (violets ((blue velvet)
                                           (red ((dragon den)
                                                 (viper nest)
                                                 (fox (hole))))
                                           (white ribbon))))))))
        (ral5 (make-ral nil nil))
        (ral6 (make-ral nil nil)))
    (sc-test-check
      ;; don't forget that the first arg is the data and that it will be put in
      ;; a list 
      (ral-econs 'makers '(mark rob) ral1)
      (equalp (get-all-refs ral1)
              '((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE)
                (FOUR VIOLETS RED DRAGON)
                (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX)
                (FOUR VIOLETS WHITE) (MARK ROB)))
      (ral-econs 'makers '(mark rob) ral2)
      (equalp (get-all-refs ral2) '((JIM) (WILD) (FOUR ROSES) (MARK ROB)) )
      (ral-econs 'mark '(violets makers) ral3)
      (ral-econs 'rose '(violets flower) ral3)
      (equalp '(mark) (get-data-data '(violets makers) ral3))
      (equalp (get-first-ref ral3) '(VIOLETS MAKERS))
      (equalp (get-all-refs ral3) '((VIOLETS MAKERS) (VIOLETS FLOWER)))
      (ral-econs 'down '(four violets red fox) ral4)
      (ral-econs 'whisky 'jim ral4)
      (equalp '(hole down) (get-data-data '(four violets red fox) ral4))
      (equalp '(beam whisky) (get-data-data 'jim ral4))
      ;; now try top-level creation
      (ral-econs 'makers 'mark ral4)
      (equalp '(makers) (get-data-data 'mark ral4))
      (ral-econs 'makers 'mark ral5)
      (equalp '((mark)) (get-all-refs ral5))
      (ral-econs 'beam 'jim ral6)
      (ral-econs 'turkey 'wild ral6)
      (ral-econs 'roses 'four ral6)
      (equalp (get-all-refs ral6) '((JIM) (WILD) (FOUR)))
      (equalp (get-data-data 'jim ral6) '(beam))
      (equalp (get-data-data 'wild ral6) '(turkey))
      (equalp (get-data-data 'four ral6) '(roses)))))

;;; SAR Sat Jun  2 14:32:54 BST 2012
(sc-deftest test-ral-set-slot ()
  (sc-test-check
    (set-slot 'largest-fast-leap 10 'oboe
              +slippery-chicken-standard-instrument-palette+)
    (= 10 (largest-fast-leap 
           (get-data 'oboe
                     +slippery-chicken-standard-instrument-palette+)))
    (set-slot 'largest-fast-leap 19 'oboe
              +slippery-chicken-standard-instrument-palette+)
    (= 19 (largest-fast-leap 
           (get-data 'oboe
                     +slippery-chicken-standard-instrument-palette+)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rthm-seq-palette tests

;;; MDE Sat Jun  6 16:53:58 2020, Heidhausen 
(sc-deftest test-make-rsp-from-unit-multipliers-simp ()
  (let ((rsp (make-rsp-from-unit-multipliers-simp
              'test-unit-mults 's
              '(((3 4 1 2) (2 1 2))
                ((2 1 2 1) (3) (4))
                (((3) 4 1) (3 4 1) (2 3) (2 3) (2)))))
        (rsp2 (make-rsp-from-unit-multipliers-simp
               'test-unit-mults 's
               '(((3 4 1 2) (2 (1) 2))
                 ((2 1 2 1) (3) ((4)) (1.5 1.5 1.5 1.5 1))
                 (((3) 4 1) (3 4 1) ((2) 3) (2 3) (2))))))
    (sc-test-check
      (= 3 (num-data rsp))
      (= 2 (num-bars (get-data 1 rsp)))
      (= 7 (num-notes (get-data 1 rsp)))
      (= 3 (num-bars (get-data 2 rsp)))
      (= 5 (num-bars (get-data 3 rsp)))
      (time-sig-equal (fourth (bars (get-data 2 rsp2)))
                      '(7 16))
      (is-rest (get-first (get-data 3 rsp))))))
          
;;; SAR Sat Jan 28 12:30:26 GMT 2012
(sc-deftest test-rsp-make-rsp ()
  (let* ((mrsp1 (make-rsp 'rsp-test 
                          '((seq1 ((((2 4) q +e. s)
                                    ((s) e (s) q)
                                    (+e. s { 3 (te) te te } ))
                                   :pitch-seq-palette (1 7 3 4 5 2 6))))
                          :psp-inversions nil))
         (mrsp2 (make-rsp 'rsp-test 
                          '((seq1 ((((2 4) q +e. s)
                                    ((s) e (s) q)
                                    (+e. s { 3 (te) te te } ))
                                   :pitch-seq-palette (1 7 3 4 5 2 6))))
                          :psp-inversions t)))
    (sc-test-check
      (every #'rthm-seq-p (data mrsp1))
      (every #'rthm-seq-p (data mrsp2))
      (equalp (data (first (data mrsp1)))
              '((((2 4) Q +E. S) ((S) E (S) Q) (+E. S { 3 (TE) TE TE }))
                :PITCH-SEQ-PALETTE (1 7 3 4 5 2 6)))
      (equalp (data (first (data mrsp2)))
              '((((2 4) Q +E. S) ((S) E (S) Q) (+E. S { 3 (TE) TE TE }))
                :PITCH-SEQ-PALETTE (1 7 3 4 5 2 6)))
      (not (psp-inversions mrsp1))
      (psp-inversions mrsp2)
      (equalp (loop for i in (data (pitch-seq-palette (first (data mrsp1))))
                 collect (data i))
              '((1 7 3 4 5 2 6)))
      (equalp (loop for i in (data (pitch-seq-palette (first (data mrsp2))))
                 collect (data i))
              '((1 7 3 4 5 2 6) (7 1 5 4 3 6 2))))))

;;; SAR Sat Jan 28 15:07:08 GMT 2012
(sc-deftest test-rsp-create-psps ()
  (let* ((mrsp (make-rsp 'rsp-test 
                         '((seq1 ((((2 4) q +e. s)
                                   ((s) e (s) q)
                                   (+e. s { 3 (te) te te } ))
                                  :pitch-seq-palette (1 2 3 4 5 6 7)))
                           (seq2 ((((3 4) (e.) s { 3 te te te } +q)
                                   ({ 3 +te (te) te } e e (q)))
                                  :pitch-seq-palette (2 3 4 5 6 7 8)))
                           (seq3 ((((2 4) e e { 3 te te te })
                                   ((5 8) (e) e e e s s))
                                  :pitch-seq-palette 
                                  ((3 4 5 6 7 8 9 10 1 2)
                                   (1 4 3 6 9 8 9 12 2 1)))))))
         (mrsps-num-notes (loop for i in (data mrsp)
                             collect
                               (loop for j in (data (pitch-seq-palette i))
                                  collect (length (data j)))))
         ;; MDE Tue Sep  3 21:16:55 2013 
         (split (split-into-single-bars mrsp))
         (bar1 (get-data '(seq1 1) split))
         (bar2 (get-data '(seq3 2) split))
         (psp (pitch-seq-palette bar2))
         (cpsps-mrsp '()))
    (sc-test-check
      ;; MDE Tue Sep  3 21:17:54 2013
      (= 1 (num-bars bar1))
      (= 2 (num-notes bar1))
      (zerop (num-rests bar1))
      (= 1 (num-bars bar2))
      (= 5 (num-notes bar2))
      (= 1 (num-rests bar2))
      (= 2 (num-data psp))
      (equalp '(8 9 10 1 2) (data (get-next psp)))
      (equalp '(8 9 12 2 1) (data (get-next psp)))
      ;; default ps-per-rs
      (create-psps mrsp)
      (setf cpsps-mrsp (loop for i in (data mrsp)
                          collect
                            (length (loop for j in (data 
                                                    (pitch-seq-palette i)) 
                                       collect (data j)))))
      (every #'(lambda (x) (= x 3)) cpsps-mrsp)
      (notany #'not 
              (loop for p from 0 to 2 do
                   (every #'(lambda (x) (= x (first (nth p mrsps-num-notes))))
                          (nth p (loop for i in (data mrsp)
                                    collect
                                      (loop for j in (data 
                                                      (pitch-seq-palette i)) 
                                         collect (length (data j))))))))
      (every #'numberp (flatten cpsps-mrsp))
      
      ;; 5 ps-per-rs
      (create-psps mrsp :pitch-seqs-per-rthm-seq 5)
      (setf cpsps-mrsp (loop for i in (data mrsp)
                          collect
                            (length (loop for j in (data 
                                                    (pitch-seq-palette i))
                                       collect (data j)))))
      (every #'(lambda (x) (= x 5)) cpsps-mrsp)
      (notany #'not 
              (loop for p from 0 to 2 do
                   (every #'(lambda (x) (= x (first (nth p mrsps-num-notes))))
                          (nth p (loop for i in (data mrsp)
                                    collect
                                      (loop for j in (data 
                                                      (pitch-seq-palette i)) 
                                         collect (length (data j))))))))
      (every #'numberp (flatten cpsps-mrsp)))))

;;; SAR Sat Jan 28 15:40:52 GMT 2012
(sc-deftest test-rsp-reset-psps ()
  (let ((mrsp
         (make-rsp 'rsp-test 
                   '((seq1 ((((2 4) q +e. s)
                             ((s) e (s) q)
                             (+e. s { 3 (te) te te } ))
                            :pitch-seq-palette ((1 2 3 4 5 6 7)
                                                (1 3 5 7 2 4 6)
                                                (1 4 2 6 3 7 5)
                                                (1 5 2 7 3 2 4))))
                     (seq2 ((((3 4) (e.) s { 3 te te te } +q)
                             ({ 3 +te (te) te } e e (q)))
                            :pitch-seq-palette (2 3 4 5 6 7 8)))
                     (seq3 ((((2 4) e e { 3 te te te })
                             ((5 8) (e) e e e s s))
                            :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2)))))))
    (sc-test-check
      (not (loop repeat 2
              do (get-next (pitch-seq-palette (first (data mrsp))))))
      (= 2 (current (pitch-seq-palette (first (data mrsp)))))
      (equalp (data (get-next (pitch-seq-palette (first (data mrsp)))))
              '(1 4 2 6 3 7 5))
      (reset-psps mrsp)
      (= 0 (current (pitch-seq-palette (first (data mrsp)))))
      (equalp (data (get-next (pitch-seq-palette (first (data mrsp)))))
              '(1 2 3 4 5 6 7)))))

;;; SAR Mon Jan 30 20:49:37 GMT 2012
(sc-deftest test-rsp-scale ()
  (let* ((mrsp
          (make-rsp 'rsp-test 
                    '((seq1 ((((2 4) q +e. s)
                              ((s) e (s) q)
                              (+e. s { 3 (te) te te } ))
                             :pitch-seq-palette ((1 2 3 4 5 6 7)
                                                 (1 3 5 7 2 4 6)
                                                 (1 4 2 6 3 7 5)
                                                 (1 5 2 7 3 2 4))))
                      (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                              ({ 3 +te (te) te } e e (h)))
                             :pitch-seq-palette (2 3 4 5 6 7 8)))
                      (seq3 ((((2 4) e e { 3 te te te })
                              ((4 4) (e) e e e s s (s) s q))
                             :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7)))))) 
         (rests-ties (loop for i in (data mrsp)
                        collect (loop for j in (bars i)
                                   collect (loop for k in (rhythms j)
                                              collect (list 
                                                       (is-rest k)
                                                       (is-tied-to k)))))))
    (sc-test-check
      (scale mrsp .5)
      (equalp (loop for i in (data mrsp)
                 collect (loop for j in (bars i)
                            collect (loop for k in (rhythms j)
                                       collect (list 
                                                (is-rest k)
                                                (is-tied-to k)))))
              rests-ties)
      (equalp (loop for i in (data mrsp)
                 collect (loop for j in (bars i)
                            collect (loop for k in (rhythms j)
                                       collect (list 
                                                (data k)))))
              '((((E) (S.) (32)) ((32) (S) (32) (E)) ((S.) (32) (TS) (TS)
                                                      (TS))) 
                (((S.) (32) (TS) (TS) (TS) (Q)) ((TS) (TS) (TS) (S) (S) (Q))) 
                (((S) (S) (TS) (TS) (TS)) ((S) (S) (S) (S) (32) (32) (32) (32)
                                           (E)))))
      (scale mrsp 6)
      (equalp (loop for i in (data mrsp)
                 collect (loop for j in (bars i)
                            collect (loop for k in (rhythms j)
                                       collect (list 
                                                (is-rest k)
                                                (is-tied-to k)))))
              rests-ties)
      (equalp (loop for i in (data mrsp)
                 collect (loop for j in (bars i)
                            collect (loop for k in (rhythms j)
                                       collect (list 
                                                (data k)))))
              '((((H.) (S.) (E.)) ((E.) (Q.) (E.) (H.)) ((S.) (E.) (Q) (Q)
                                                         (Q))) 
                (((S.) (E.) (Q) (Q) (Q) (W.)) ((Q) (Q) (Q) (Q.) (Q.) (W.)))
                (((Q.) (Q.) (Q) (Q) (Q)) ((Q.) (Q.) (Q.) (Q.) (E.) (E.) (E.)
                                          (E.) (H.))))))))

;;; SAR Tue Jan 31 14:15:32 GMT 2012
(sc-deftest test-rsp-chop ()
  (let* ((rsp-orig (make-rsp
                    'sl-rsp
                    '((1 
                       ((((2 4) (e) e (e) e)) 
                        :pitch-seq-palette (1 8)))
                      (2 
                       ((((2 4) (s) e s e. (s))) 
                        :pitch-seq-palette (3 5 7)))
                      (3
                       ((((3 4) q +s e. +q)) 
                        :pitch-seq-palette (1 7))))))
         (rsp-chopped (chop rsp-orig
                            '((1 1) (1 2) (2 2))
                            'e)))
    (sc-test-check
      (equalp (loop for i in (data rsp-chopped)
                 collect (loop for j in (data (data i))
                            collect (loop for k in (bars j)
                                       collect (loop for l in (rhythms k) 
                                                  collect (list 
                                                           (data l)
                                                           (is-rest l)))))) 
              '(((((8 T))) (((E T) (E NIL))) (((E NIL))) (((8 T))) 
                 (((E T) (E NIL))) (((E NIL)))) 
                ((((S T) (S NIL))) (((S T) (E NIL) (S NIL))) (((S T) (S NIL)))
                 (((E NIL))) (((E. NIL) (S T))) (((8 T)))) 
                ((((E NIL))) (((Q NIL))) (((8 T))) ((("S" T) (S NIL))) 
                 ((("S" T) (E. NIL))) (((8 T))) (((8 T))) (((4 T))) 
                 (((8 T)))))))))

;;; MDE Thu Feb  1 17:32:01 2018 -- 
(sc-deftest test-rsp-create-psps-no-overwrite ()
  (let* ((rsp (make-rsp
               'sl-rsp
               '((1 
                  ((((2 4) (e) e (e) e)) 
                   :pitch-seq-palette (1 8)))
                 (2 
                  ((((2 4) (s) e s e. (s)))))
                 (3
                  ((((3 4) q +s e. +q)) 
                   :pitch-seq-palette (1 7)))))))
    (sc-test-check
      (flat-line (pitch-seq-palette (get-data 2 rsp)))
      (create-psps rsp :overwrite nil :pitch-seqs-per-rthm-seq 5)
      (not (flat-line (pitch-seq-palette (get-data 2 rsp))))
      (= 5 (sclist-length (pitch-seq-palette (get-data 2 rsp))))
      (= 1 (sclist-length (pitch-seq-palette (get-data 1 rsp))))
      (= 1 (sclist-length (pitch-seq-palette (get-data 3 rsp))))
      (equalp '(1 8) (data (get-nth 0 (pitch-seq-palette (get-data 1 rsp)))))
      )))


;;; MDE Fri Mar 25 14:24:26 2016 -- how to combine original with chopped
;;; palettes
(sc-deftest test-rsp-chop2 ()
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
                                's)))                      ; chopping unit
    (setf chopped-palette
          (combine orig-palette (parcel-data chopped-palette 'chopped)))
    (sc-test-check
      (equalp (get-data-data '(1) chopped-palette)
              '((((1 4) - S E S -)) :PITCH-SEQ-PALETTE ((1 2 3))))
      (equalp '(chopped 3 9)
              (previous (get-data '(chopped 3 10) chopped-palette))))))

;;; SAR Thu Feb  2 17:35:32 GMT 2012
(sc-deftest test-rsp-get-multipliers ()
  (let* ((mrsp 
          (make-rsp 'rsp-test 
                    '((seq1 ((((2 4) q +e. s)
                              ((s) e (s) q)
                              (+e. s { 3 (te) te te } ))
                             :pitch-seq-palette ((1 2 3 4 5 6 7)
                                                 (1 3 5 7 2 4 6)
                                                 (1 4 2 6 3 7 5)
                                                 (1 5 2 7 3 2 4))))
                      (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                              ({ 3 +te (te) te } e e (h)))
                             :pitch-seq-palette (2 3 4 5 6 7 8)))
                      (seq3 ((((2 4) e e { 3 te te te })
                              ((4 4) (e) e e e s s (s) s q))
                             :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7))))))
         (mrsp-multipliers (get-multipliers mrsp 'e 'seq1))
         (r1 (make-rhythm 'e))
         (r2 (make-rhythm 's))
         (r3 (make-rhythm 'te)))
    ;; MDE Mon Apr 15 17:37:34 2013 -- 
    (cmn-display mrsp)
    (sc-test-check
      (equal '(seq3) (rsp-id (get-nth-bar 1 (get-data 'seq3 mrsp))))
      ;; MDE Mon Apr 15 17:37:34 2013 -- 
      #+cmn (file-write-ok "/tmp/rsp-test.eps" 23000)
      (equalp (list (first mrsp-multipliers)
                    (second mrsp-multipliers)
                    (third mrsp-multipliers)
                    (fourth mrsp-multipliers))
              '(3.5 1.0 1.5 3.5))
      (equal-within-tolerance (fifth mrsp-multipliers)
                              (rhythm/ (add r2 r3) r1))
      (every #'(lambda (x) (equal-within-tolerance x (rhythm/ r3 r1)))
             (list (sixth mrsp-multipliers)
                   (seventh mrsp-multipliers))))))

;;; MDE Wed Oct 23 17:22:09 2013 
(sc-deftest test-rsp-rsp-subseq ()
  (let ((rsp
         (make-rsp 
          'hyperboles-rsp
          ;; all of these long seqs have 9 bars
          '((long
             ((1
               ((a ((((4 4) - e.. 32 - h.) (+w) (+w) ((w)) ((h) (e) q e)
                     (+q - +s e. - +h) (+w) (+w) ((w)))))
                (b ((((4 4) (q.) q q.) (+q h.) (+h h) (+w) (+s q.. h) (+h.. e)
                     (+q { 3 +te tq } +q. e)
                     (+q h.) (- +s e. - - +e e - +h))))
                (c ((((4 4) (w)) ((w)) ({ 3 tq x 3 } +q q) (+s q.. s q..)
                     (+q - +s e. - +h) (+h - s e. - q) (+w) (+h h)
                     (+h. - s e. -))))
                (d ((((4 4) (w)) (w) (+h - +e e - - +s e. -)
                     (+h { 3 +tq tq tq }) (+w) (w)
                     (+s h...)
                     (+h. - +s e. -) (+q h.))))))
              (2
               ((a ((((4 4) (q) (s) e. +s q..) 
                     (+q - +s e. - +h) (+w) (+h. - +s e s -)
                     (+h - +s e. - - +s e. -) (+w) (+w) ((q) (s) e.+h) (+w))))
                (b ((((4 4) (w)) ((h) h) 
                     (+q. q. q) (+w) (+w) ({ 3 +th th th })
                     (+q h.) (+w) (+q - +s e. - +h))))
                (c ((((4 4) (h) h) (+q. e+h) (+h. - +s e. -) (q. q q.) (+w)
                     (+h h) (+h - e. s - - +e e -) (+h. - +s e. -) (w))))
                (d ((((4 4) (e) h..) (- +s e. - +q - +e. s - - +e e -) (+h h)
                     (+w) (q - +s e. - - +e. s - +q) 
                     (- +e. s - - +e e - +h)
                     (- +e. s - +h.) (+h h) (+h q - +s e. -))))))
              (3
               ((a ((((4 4) (h) - e. s - - +e e -) 
                     (+w) (+h - +s e. - - +e e -) (- +e. s - +h.) (+h. q)
                     (- +e. s - +q. q.) (- +e. s - - +s e s - +h) (+h h)
                     (+h - +s e s - +q))))
                (b ((((4 4) w)
                     (w) (+w) ((w)) ((w))
                     ((q) { 3 te tq } +h) (+h { 3 tq te } +q)
                     (+w) (+h { 3 (te) tq } +q))))
                (c ((((4 4) (w)) ((q) { 3 (te) tq } { 3 +tq tq tq }) (+h h)
                     (+h { 3 +te tq } +q) ({ 3 tq tq tq } +h)
                     (+h. { 3 +te tq }) (+h. - +e. s -) 
                     (+h { 3 +tq th }) ({ 3 +tq tq tq } +h))))
                (d ((((4 4) (w)) ((h) (q) q) (- +e. s - +h.) (+s q.. +q.. s)
                     (+q. e+q - +s e. -) (+w) (+e q. +s q..) (h.. e)
                     (+w))))))))
            (top ((((4 4) q x 4)))))))
        bar)
    ;; (print rsp)
    ;; (print (get-data '(long 1 a) rsp))
    (sc-test-check
     ;; MDE Fri Jul  5 15:40:56 2019
      ;; (not (get-this-refs rsp))
      (equalp '((top)) (get-this-refs rsp))
      (not (get-this-refs (get-data-data 'long rsp)))
      (equalp '((LONG 3 A) (LONG 3 B) (LONG 3 C) (LONG 3 D))
              (get-this-refs (get-data-data '(long 3) rsp)))
      (equal '(long 1 a) (rsp-id (get-nth-bar 0 (get-data '(long 1 a) rsp))))
      (equal '(long 2 a) (rsp-id (get-nth-bar 4 (get-data '(long 2 a) rsp))))
      (equal '(long 3 d) (rsp-id (get-nth-bar 8 (get-data '(long 3 d) rsp))))
      (rsp-subseq (get-data-data 'long rsp) 2 6)
      (= 5 (num-bars (get-data '(long 1 a) rsp)))
      (= 5 (num-bars (get-data '(long 3 d) rsp)))
      (setf bar (get-bar (get-data '(long 3 c) rsp) 1))
      (eq 'q (data (first (rhythms bar))))
      (eq 'te (data (second (rhythms bar))))
      (is-rest (second (rhythms bar))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pitch-seq-palette tests

;;; SAR Tue Jan 31 14:56:34 GMT 2012
(sc-deftest test-psp-make-psp ()
  (let* ((mpsp (make-psp 'mpsp 5 '((2 5 3 1 4)
                                   (1 4 2 5 3)
                                   (5 1 3 2 4)
                                   (2 3 4 5 1)
                                   (3 2 4 1 5))))
         ;; MDE Mon Sep 30 17:56:46 2013 -- so the args here a like Lisp's
         ;; subseq function: 0-based and start = inclusive, end = exclusive
         (subseq (psp-subseq mpsp 1 3)))
    (sc-test-check
      (= 5 (length (data mpsp)))
      ;; MDE Mon Sep 30 17:57:55 2013 
      (equalp (loop for ps in (data subseq) collect (data ps))
              '((5 3) (4 2) (1 3) (3 4) (2 4)))
      (equalp (loop for ps in (data mpsp)
                 collect (data ps))
              '((2 5 3 1 4)
                (1 4 2 5 3)
                (5 1 3 2 4)
                (2 3 4 5 1)
                (3 2 4 1 5))))))

;;; MDE Mon May  3 10:48:23 2021, Heidhausen
(sc-deftest test-psp-subseq-chords ()
  (let* ((mpsp (make-psp 'mpsp 5 '(((2) 5 (3) 1 (4)))))
         (ss1 (psp-subseq mpsp 1 5))
         (ss2 (psp-subseq mpsp 0 3))
         (ss3 (psp-subseq mpsp 2 3)))
    (print ss1) (print ss2) (print ss3)
    (flet ((check-it (psp list)
             (equalp (original-data (get-nth 0 psp)) list)))
      (sc-test-check
        (check-it ss1 '(5 (3) 1 (4)))
        (check-it ss2 '((2) 5 (3)))
        ;; this is the important one: a single-note chord psp retains its ()
        (check-it ss3 '((3)))))))
      

;;; SAR Tue Jan 31 17:39:04 GMT 2012
(sc-deftest test-psp-add-inversions ()
  (let ((mpsp (make-psp 'mpsp 5 '((2 5 3 1 4)
                                  (1 4 2 5 3)
                                  (5 1 3 2 4)
                                  (2 3 4 5 1)
                                  (3 2 4 1 5)))))
    (sc-test-check
      (equalp (loop for ps in (data mpsp)
                 collect (data ps))
              '((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4) (2 3 4 5 1) (3 2 4 1 5))) 
      (add-inversions mpsp)
      (equalp (loop for ps in (data mpsp)
                 collect (data ps))
              '((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4) (2 3 4 5 1) (3 2 4 1 5) 
                (4 1 3 5 2) (5 2 4 1 3) (1 5 3 4 2) (4 3 2 1 5) 
                (3 4 2 5 1))))))

;;; SAR Tue Jan 31 18:07:18 GMT 2012
(sc-deftest test-psp-combine ()
  (let ((mpsp1 (make-psp 'mpsp1 5 '((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4)))) 
        (mpsp2 (make-psp 'mpsp2 5 '((2 3 4 5 1) (3 2 4 1 5) (3 2 1 5 4)))) 
        (mpsp3 (make-psp 'mpsp3 5 '((2 3 4 5 1) (3 2 4 1 5))))
        (mpsp4 (make-psp 'mpsp4 3 '((2 3 4) (3 2 4)))))
    (sc-test-check
      (equalp (loop for ps in (data (combine mpsp1 mpsp2)) collect (data ps))
              '((2 5 3 1 4 2 3 4 5 1) (1 4 2 5 3 3 2 4 1 5) 
                (5 1 3 2 4 3 2 1 5 4)))
      ;; MDE Tue Oct  1 09:52:41 2013 
      (= 5 (num-notes mpsp1))
      (= 10 (num-notes (combine mpsp2 mpsp3)))
      (equalp (loop for ps in (data (combine mpsp1 mpsp3)) collect (data ps))
              '((2 5 3 1 4 2 3 4 5 1) (1 4 2 5 3 3 2 4 1 5) 
                (5 1 3 2 4 2 3 4 5 1)))
      (equalp (loop for ps in (data (combine mpsp1 mpsp4)) collect (data ps))
              '((2 5 3 1 4 2 3 4) (1 4 2 5 3 3 2 4) (5 1 3 2 4 2 3 4))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-set tests

;;; SAR Tue Jan 31 20:48:26 GMT 2012
(sc-deftest test-sc-set-make-sc-set ()
  (let ((mscs1 (make-sc-set '(d2 cs3 fs3 cs4 e4 c5 af5 ef6)))
        (mscs2 (make-sc-set '(d2 cs3 fs3 cs4 e4 c5 af5 ef6)
                            :id 'scs1
                            :subsets '((violin (e4 c5 af5 ef6))
                                       (viola (cs4 e4)))
                            :related-sets '((missing (ds2 e2 b3 cs6 d6))))) 
        (mscs3 (make-sc-set '(af5 cs4 cs3 e4 fs3 ef6 d2 c5)))
        (mscs4 (make-sc-set '(af5 cs4 cs3 e4 fs3 ef6 d2 c5) :auto-sort nil))
        ;; MDE Fri Jun 21 09:52:07 2019
        (s5 (make-sc-set '(F0 AF1 EF2 BF3 DF4 C6 C7 G8 BF8 A10)))
        ;; MDE Fri Mar 12 12:39:19 2021, Heidhausen -- try freqs
        (s6 (make-sc-set '(100 200 300))))
    (sc-test-check
      (equalp '(gqs2 gqs3 dqs4) (get-pitch-symbols s6))
      (equalp (loop for po in (data mscs1) collect (data po))
              '(d2 cs3 fs3 cs4 e4 c5 af5 ef6))
      ;; MDE Fri Jun 21 09:52:16 2019 -- test it works with very high 8ves
      (= 5 (least-used-octave s5 :highest-wins nil))
      (= 9 (least-used-octave s5 :highest-wins t))
      ;; MDE Tue Aug 18 17:23:05 2015 -- 
      (= 2 (least-used-octave mscs1 :highest-wins nil))
      (= 3 (most-used-octave mscs1 :highest-wins nil))
      (= 3 (least-used-octave (make-sc-set '(c2 d2 g3 cs4 gs4 a5))
                              :highest-wins nil))
      (= 5 (least-used-octave (make-sc-set '(c2 d2 g3 cs4 gs4 a5))
                              :highest-wins t))
      (= 5 (least-used-octave (make-sc-set '(c2 d2 g3 cs4 gs4 a5))
                              :highest-wins nil :avoiding 3))
      ;; because there's no note in octave 3!
      (= 3 (least-used-octave (make-sc-set '(c2 d2 g2 cs4 gs4 a5))))
      ;; the lowest octave in the case of a tie wins...
      (= 2 (most-used-octave (make-sc-set '(c2 d2 g2 cs4 gs4 a4 a5))
                             :highest-wins nil))
      ;; ... unless optional arg is T
      (= 4 (most-used-octave (make-sc-set '(c2 d2 g2 cs4 gs4 a4 a5))
                             :highest-wins t))
      (equalp (id mscs2) 'scs1)
      (equalp (loop for i in (data (subsets mscs2)) 
                 collect (id i)
                 collect (loop for j in (data i)
                            collect (data j)))
              '(VIOLIN (E4 C5 AF5 EF6) VIOLA (CS4 E4)))
      (equalp (id (first (data (related-sets mscs2)))) 'MISSING)
      (equalp (loop for i in (data (first (data (related-sets mscs2))))  
                 collect (data i))
              '(DS2 E2 B3 CS6 D6))
      (equalp (loop for po in (data mscs3) collect (data po))
              '(d2 cs3 fs3 cs4 e4 c5 af5 ef6))
      (equalp (loop for po in (data mscs4) collect (data po)) 
              '(af5 cs4 cs3 e4 fs3 ef6 d2 c5))
      ;; MDE Wed Jun 28 10:57:26 2017
      (has-pitches-in-range mscs1 'c3 'g3)
      (not (has-pitches-in-range mscs1 'f4 'b4))
      (not (has-pitches-in-range (make-sc-set '(c4 eqf5 fs5))
                                 'd4 'f5 t))
      ;; MDE Fri Jun  2 12:13:45 2017
      (equalp (get-pitch-symbols (thin (clone mscs1)))
              '(D2 FS3 E4 C5 AF5 EF6))
      (equalp (get-pitch-symbols (thin (clone mscs1) :strength 2))
              '(D2 CS3 FS3 E4 AF5 EF6))
      (equalp (get-pitch-symbols (thin (clone mscs1) :strength 9))
              '(FS3 CS4 E4 C5 AF5 EF6))
      (equalp (get-pitch-symbols (thin (clone mscs1) :strength 10))
              '(FS3 CS4 E4 C5 AF5 EF6))
      (equalp (get-pitch-symbols (thin (clone mscs4) :target 3))
              '(C5 FS3 AF5) )
      (equalp (get-pitch-symbols (thin (clone mscs4) :remove 6))
              '(c5 fs3))
      (equalp (get-pitch-symbols (thin (clone mscs4) :remove 6))
              '(c5 fs3))
      (equalp (get-pitch-symbols
               (thin (make-chord '(af5 cs4 cs3 e4 fs3 ef6 d2 c5)) :remove 7))
              '(e4))
      (equalp '(e4)
              (get-pitch-symbols (thin (make-chord '(c4 d4 e4)) :target 1)))
      (equalp '(c4)
              (get-pitch-symbols (thin (make-chord '(c4 d4 e4))
                                       :target 1 :invert t)))
      (equalp (pitch-list-to-symbols
               (get-data-data 'violin (subsets
                                  (thin (clone mscs2) :remove 2 :invert t))))
              '(c5 ef6)))))

;;; SAR Tue Jan 31 22:08:20 GMT 2012
(sc-deftest test-sc-set-round-inflections ()
  (let ((mscs1 '())
        (mscs2 '()))
    (setf cm::*scale* (cm::find-object 'twelfth-tone))
    (setf mscs1 (make-sc-set '(c4 cts4 css4 cqs4 cssf4 cstf4 cs4)))
    (setf mscs2 (make-sc-set '(c1 cts2 css3 cqs4 cssf5 cstf6 cs7))) 
    (sc-test-check
      (equalp (loop for po in (round-inflections mscs1)
                 collect (data po))
              '(C4 C4 CS4 CS4))
      (equalp (round-inflections mscs1 :as-symbols t)
              '(C4 C4 CS4 CS4))
      (equalp (round-inflections mscs1 
                                 :qtr-tones-also T
                                 :as-symbols t)
              '(C4 C4 C4 CS4 CS4))
      (equalp (round-inflections mscs1 
                                 :qtr-tones-also T
                                 :octave 4
                                 :as-symbols t)
              '(C4 CS4))
      (equalp (round-inflections mscs1 
                                 :qtr-tones-also T
                                 :octave 4
                                 :remove-duplicates NIL
                                 :as-symbols t)
              '(C4 C4 C4 CS4 CS4))
      (setf cm::*scale* (cm::find-object 'quarter-tone)))))

;;; SAR Mon Feb  6 13:12:16 GMT 2012
(sc-deftest test-sc-set-force-micro-tones ()
  (let ((mscs (make-sc-set '(d2 cqs3 fs3 cs4 e4 c5 aqf5 ef6))))
    (sc-test-check
      (equalp (loop for p in (data mscs) collect (micro-tone p))
              '(NIL T NIL NIL NIL NIL T NIL))
      (not (force-micro-tone mscs))
      (every #'not (loop for p in (data mscs) collect (micro-tone p)))
      (not (force-micro-tone mscs 't))
      (notany #'not (loop for p in (data mscs) collect (micro-tone p))))))

;;; SAR Mon Feb  6 13:28:12 GMT 2012
(sc-deftest test-sc-set-get-chromatic ()
  (let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 d6)))) 
    (sc-test-check
      (listp (get-chromatic mscs))
      (every #'pitch-p (get-chromatic mscs))
      (every #'symbolp (get-chromatic mscs :as-symbols t))
      (equalp (get-chromatic mscs :as-symbols t :octave 4)
              '(FS4 CS4 E4 C4 AF4 D4))
      (equalp (get-chromatic mscs 
                             :as-symbols t 
                             :octave 4 
                             :remove-duplicates nil)
              '(D4 FS4 CS4 E4 C4 AF4 D4))
      (equalp (get-chromatic mscs :as-symbols t :invert t)
              '(CQS3 GQF3 FQS4 BQF5)))))

;;; SAR Mon Feb  6 13:45:17 GMT 2012
(sc-deftest test-sc-set-get-non-chromatic ()
  (let ((mscs 
         (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 cqs6 d6)))) 
    (sc-test-check
      (listp (get-non-chromatic mscs))
      (every #'pitch-p (get-non-chromatic mscs))
      (every #'symbolp (get-non-chromatic mscs :as-symbols t))
      (equalp (get-non-chromatic mscs :as-symbols t :octave 4)
              '(GQF4 FQS4 BQF4 CQS4)))))

;;; SAR Mon Feb  6 14:35:06 GMT 2012
(sc-deftest test-sc-set-get-srts ()
  (let ((mscs (make-sc-set '(d2 fs3 cs4 c5 af5 d6))))
    (sc-test-check
      (every #'equal-within-tolerance 
             (get-srts mscs)
             (loop for p in '(d2 fs3 cs4 c5 af5 d6)
                collect (/ (note-to-freq p) (note-to-freq 'c4))))
      (every #'equal-within-tolerance 
             (get-srts mscs 'd4)
             (loop for p in '(d2 fs3 cs4 c5 af5 d6)
                collect (/ (note-to-freq p) (note-to-freq 'd4))))
      ;; MDE Mon Mar 19 19:06:35 2012 
      (every #'equal-within-less-tolerance 
             (get-srts mscs 'c4 2)
             (loop for p in '(e2 gs3 ds4 d5 bf5 e6)
                collect (/ (note-to-freq p) (note-to-freq 'c4)))))))

;;; SAR Mon Feb  6 15:40:03 GMT 2012
(sc-deftest test-sc-set-subset-get-srts ()
  (let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
                           :subsets '((fl (df5 f5 af5 c6))
                                      (va (c3 e3 g3 b3 d4 gf4))))))
    (sc-test-check
      (every #'equal-within-tolerance
             (subset-get-srts mscs 'fl)
             (loop for p in '(df5 f5 af5 c6)
                collect (/ (note-to-freq p) (note-to-freq 'c4))))
      (every #'equal-within-tolerance
             (subset-get-srts mscs 'fl 'd4)
             (loop for p in '(df5 f5 af5 c6)
                collect (/ (note-to-freq p) (note-to-freq 'd4))))
      (every #'equal-within-tolerance
             (subset-get-srts mscs 'fl 'c4 2)
             (loop for p in '(ef5 g5 bf5 d6)
                collect (/ (note-to-freq p) (note-to-freq 'c4)))))))

;;; SAR Mon Feb  6 16:04:59 GMT 2012
(sc-deftest test-sc-set-get-interval-structure ()
  (let* ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)))  
         ;; MDE Sat Feb 11 10:53:01 2012 
         (mset (make-complete-set '(c5 cqs5 g5 c4 d6)))
         ;; MDE Tue Aug 27 20:29:54 2013 -- 
         (simple (make-sc-set '(a3 e4 a4)))
         (is (get-interval-structure simple 'frequencies)))
    (sc-test-check
      ;; MDE Sat Feb 11 11:39:50 2012
      (equalp (get-interval-structure 
               mset t)
              '(12.0f0 12.5f0 19.0f0 26.0f0))
      ;;; MDE Wed Aug 12 17:37:28 2015 
      (equalp (get-interval-structure (make-chord '(c4 e4 g4 b4)) t t)
              '(4.0 3.0 4.0))
      ;; MDE Tue Aug 27 20:25:31 2013 
      (equal-within-tolerance (first is) 110 .5)
      (equal-within-tolerance (second is) 220 .5)
      (equalp (get-interval-structure mscs)
              '(6 14 20 28 34 42 48 56 64 70 78 84 92)))))

;;; SAR Mon Feb  6 16:12:46 GMT 2012
(sc-deftest test-sc-set-set-position ()
  (let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)))) 
    (sc-test-check
      (equalp (loop for n in (data mscs) 
                 collect (set-position (make-pitch n) mscs))
              (loop for i from 0 to 13 collect i)))))

;;; SAR Mon Feb  6 16:17:34 GMT 2012
(sc-deftest test-sc-set-get-degrees ()
  (let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)))) 
    (sc-test-check
      (equalp (get-degrees mscs)
              '(76 82 90 96 104 110 118 124 132 140 146 154 160 168)))))

;;; SAR Mon Feb  6 16:24:14 GMT 2012
(sc-deftest test-sc-set-get-freqs ()
  (let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)))) 
    (sc-test-check
      (every #'(lambda (x y) (equal-within-tolerance x y 0.001))
             (get-freqs mscs) 
             '(73.41618871368837 87.30705289160142
               109.99999810639679 130.8127784729004 164.81377633519514
               195.99771591817216 246.94163930037348 293.6647548547535
               369.99440456398133 466.1637395092839 554.3652698843016
               698.4564231328113 830.6093584209975 1046.5022277832031)))))

;;; SAR Mon Feb  6 16:28:52 GMT 2012
(sc-deftest test-sc-get-midi ()
  (let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
    (sc-test-check
      (equalp (get-midi mscs) '(38 41 45 48 52 55 59 62 66 70 73 77 80 84)))))

;;; SAR Mon Feb  6 16:44:42 GMT 2012
(sc-deftest test-sc-set-get-semitones-from-middle-note ()
  (let ((mscs1 (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))) 
        (mscs2 (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5)))
        (mscs3 (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
                            :subsets '((fl (df5 f5 af5 c6))
                                       (va (c3 e3 g3 b3 d4 gf4))))))
    (sc-test-check
      (equalp (get-semitones-from-middle-note mscs1) 
              '(-24.0 -21.0 -17.0 -14.0 -10.0 -7.0 -3.0 0.0 4.0 8.0 11.0 15.0
                18.0 22.0))
      (equalp (get-semitones-from-middle-note mscs2)
              '(-21.0 -18.0 -14.0 -11.0 -7.0 -4.0 0.0 3.0 7.0 11.0 14.0 18.0
                21.0))
      (equalp (get-semitones-from-middle-note mscs3 'fl)
              '(-7.0 -3.0 0.0 4.0)))))

;;; SAR Mon Feb  6 16:57:32 GMT 2012
(sc-deftest test-sc-set-add ()
  (let* ((mscs1 (make-sc-set '(d2 a2 e3 b3 gf4 df5 af5)))
         (mscs2 (make-sc-set '(f2 c3 g3 d4 bf4 f5 c6)))
         (mscs3 (loop for p in (data (add mscs1 mscs2))
                   collect (data p))))
    (sc-test-check
      (equalp mscs3 '(D2 F2 A2 C3 E3 G3 B3 D4 GF4 BF4 DF5 F5 AF5 C6)))))

;;; SAR Mon Feb  6 17:05:39 GMT 2012
(sc-deftest test-sc-set-contains-pitches ()
  (let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
    (sc-test-check
      ;; MDE Tue Jul 16 13:06:29 2019 -- actually now in chord class but
      ;; makes no difference 
      (contains-pitches mscs (list (make-pitch 'f2)))
      (contains-pitches mscs '(d2 e3 gf4 af5))
      (not (contains-pitches mscs '(d2 e3 gf4 b4 af5))))))

;;; SAR Mon Feb  6 17:41:08 GMT 2012
(sc-deftest test-sc-set-create-chord ()
  (let* ((mscs (make-sc-set '(d2 c3 d4 df5 c6)))
         (mscs-chord (create-chord mscs)))
    (sc-test-check
      (chord-p mscs-chord)
      (every #'pitch-p (data mscs-chord))
      (equalp (loop for p in (data mscs-chord) collect (data p))
              '(d2 c3 d4 df5 c6)))))

;;; SAR Mon Feb  6 17:53:58 GMT 2012
(sc-deftest test-sc-set-create-event ()
  (let* ((mscs (make-sc-set '(d2 c3 d4 df5 c6)))
         (mscs-event (create-event mscs 'q 0.0)))
    (sc-test-check
      (event-p mscs-event)
      (equalp (start-time mscs-event) 0.0)
      (chord-p (pitch-or-chord mscs-event))
      (equalp (loop for p in (data (pitch-or-chord mscs-event))
                 collect (data p))
              '(d2 c3 d4 df5 c6))
      (equalp (data mscs-event) 'q))))

;;; SAR Mon Feb  6 18:04:29 GMT 2012
(sc-deftest test-sc-set-pitch-symbols ()
  (let ((mscs (make-sc-set '(d2 c3 d4 df5 c6))))
    (sc-test-check
      (equalp (pitch-symbols mscs)
              '(d2 c3 d4 df5 c6)))))


;;; SAR Tue Feb  7 18:37:42 GMT 2012
(sc-deftest test-sc-set-get-semitones ()
  (let ((mscs1 (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 fqs4 c5 af5 bqf5 cqs6 d6))) 
        (mscs2 (make-sc-set '(d2 fs3 cs4 c5 af5 d6))))
    (sc-test-check
      (equalp (get-semitones mscs1)
              '(-22.0 -11.5 -6.0 -5.5 1.0 5.5 12.0 20.0 22.5 24.5 26.0)) 
      (equalp (get-semitones mscs2)
              '(-22.0 -6.0 1.0 12.0 20.0 26.0)))))

;;; SAR Wed Feb  8 10:36:34 GMT 2012
(sc-deftest test-sc-set-stack ()
  (let* ((mscs (make-sc-set '(c4 e4 g4) :id 'test :tag "blah blah"))
         (mscs-stack (stack mscs 3))
         ;; MDE Tue Dec 31 09:27:07 2013 -- we can now restrict to just up or
         ;; down 
         (mscs2 (make-sc-set '(c4 ef4 g4)))
         (mscs2-stack (stack mscs2 2 :up nil))
         (mscs3 (make-sc-set '(c4 f4 g4)))
         (mscs3-stack (stack mscs3 4 :down nil)))
    (sc-test-check
      (sc-set-p mscs-stack)
      (eq 'test (id mscs-stack))
      (string= "blah blah" (tag mscs-stack))
      (equalp (pitch-symbols mscs-stack)
              '(EF2 GF2 BF2 DF3 F3 AF3 C4 E4 G4 B4 D5 GF5 A5 DF6 E6))
      (equalp (pitch-symbols mscs3-stack)
              '(C4 f4 G4 c5 D5 G5 A5 D6 E6 a6 b6))
      (equalp (pitch-symbols mscs2-stack)
              '(BF2 D3 F3 A3 C4 Ef4 G4)))))

;;; MDE Fri Mar 30 08:01:31 2012 -- this also tests circular-sclists a little
(sc-deftest test-subsets ()
    (let* ((set (make-sc-set
                 '(f3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5 g5)
                 :subsets '((piano-chords ((pno1 (c4 e4 g4))
                                           (pno2 (d4 f4 a4))
                                           (pno3 (e4 g4 b4))))
                            (other (f3 g3)))))
           (pc (get-data 'piano-chords (subsets set)))
           (pcd (when pc (data pc)))
           (o (get-data 'other (subsets set))))
      (sc-test-check
        (is-ral pcd)
        (and (named-object-p o)
             (listp (data o))
             (pitch-p (first (data o))))
        (and pcd
             (equalp 'pno1 (id (get-next pcd)))
             (equalp 'pno2 (id (get-next pcd)))
             (equalp 'pno3 (id (get-next pcd)))
             (equalp 'pno1 (id (get-next pcd)))))))

;;; MDE Sat Oct 26 16:41:01 2013 
(sc-deftest test-add-harmonics ()
  (let ((s1 (make-sc-set '(c4 e4) :id 'test))
        (s2 (make-sc-set '(c4 e4) :id 'test))
        (c (make-chord '(c4 e4) :id 'test)))
    (sc-test-check
      (add-harmonics s1 :start-freq-is-partial 3 :max-results 3)
      (add-harmonics s2 :start-partial 3 :max-results 3)
      ;; MDE Tue Oct 29 19:44:06 2013 -- try the same thing for a chord
      (add-harmonics c :start-partial 3 :max-results 3)
      (equalp '(C4 E4 G5 B5 C6 E6 E6 AF6) (pitch-list-to-symbols (data s2)))
      (equalp '(C4 E4 G5 B5 C6 E6 E6 AF6) (pitch-list-to-symbols (data c)))
      (equalp '(F2 A2 F3 A3 C4 E4) (pitch-list-to-symbols (data s1))))))

;;; MDE Tue Aug 18 21:41:20 2015
(sc-deftest test-limit-shift-octave ()
  (let* ((pitches (init-pitch-list '(c4 cs4 f4 g4 b4)))
         (changes (list (make-pitch 'cs4) 6 (make-pitch 'g4) 5
                        (make-pitch 'b4) 3))
         (new (ral-change-pitches-aux pitches changes))
         (rss (make-tl-set '(c4 cs4 fs4 gs4 d5)
                           :related-sets
                           '((diads
                              ((1 (c4 cs4)) (2 (fs4 gs4))))
                             (triads
                              ((1 (c4 cs4 fs4)) (2 (fs4 gs4 d5)))))))
         last-set)
    (flet ((lso (notes upper lower)
             (get-pitch-symbols
              (setq last-set
                    (limit-shift-octave
                     (make-tl-set notes)
                     :upper upper :lower lower)))))
      (sc-test-check
        (equalp (pitch-list-to-symbols new) '(c4 cs6 f4 g5 b3))
        (equalp (lso '(c2 e2 cs3 f3 g3 d4 a4 ds5) 'c5 'd2)
                '(ds2 E2 CS3 F3 G3 D4 A4 C5))
        (equalp (lso '(c2 e2 cs3 f3 g3 d4 a4 ds5) 'b4 'd2)
                '(ds2 E2 CS3 F3 G3 C4 D4 A4))
        ;; make sure pitches in the related sets are shifted too
        (limit-shift-octave rss :lower 'd4 :upper 'd6
                            :do-related-sets t)
        (equalp (get-pitch-symbols rss) '(FS4 GS4 C5 CS5 D5))
        (pitch= (make-pitch 'c5) (first (get-data-data '(diads 1)
                                                       (related-sets rss))))
        (pitch= (make-pitch 'cs5) (second (get-data-data '(triads 1)
                                                         (related-sets rss))))
        (pitch= (make-pitch 'd5) (third (get-data-data '(triads 2)
                                                       (related-sets rss))))
        (equalp (lso '(c2 eqs2 cs3 f3 g3 d4 aqf4 ds5) 'b4 'c4)
                '(C4 CS4 D4 DS4 EQS4 F4 G4 AQF4))
        ;; make sure our pitch bends came with transp of the 1/4 tones
        (= 0.5 (pitch-bend (fifth (data last-set))))
        (= 0.5 (pitch-bend (eighth (data last-set))))
        ;; MDE Thu Oct 25 12:02:48 2018 -- make sure it works when only :upper
        ;; or :lower is set, not both
        (setq rss (make-tl-set '(c1 c2 c3 c4 c5 c6 c7)))
        (limit-shift-octave rss :lower 'c3)
        (equalp '(c3 c4 c5 c6 c7) (get-pitch-symbols rss))
        (setq rss (make-tl-set '(c1 c2 c3 c4 c5 c6 c7)))
        (limit-shift-octave rss :upper 'c5)
        (equalp '(c1 c2 c3 c4 c5) (get-pitch-symbols rss))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tl-set tests

;;; SAR Mon Feb  6 20:54:22 GMT 2012
(sc-deftest test-tl-set-make-tl-set ()
  (let ((mtls1 (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))) 
        (mtls2 (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
                            :subsets '((fl (df5 f5 af5))
                                       (vla (e3 g3 b3)))
                            :related-sets '((missing (fs2 b5)))))
        (mtls3 (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6) 
                            :limit-upper 'g5
                            :limit-lower (make-pitch 'd3)))
        (mtls4 (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6) 
                            :transposition 3)))
    (sc-test-check
      (listp (data mtls1))
      (every #'pitch-p (data mtls1))
      (equalp (loop for p in (data mtls1) collect (data p))
              '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))
      (equalp (loop for ss in (data (subsets mtls2)) 
                 collect (id ss)
                 collect (loop for p in (data ss) collect (data p)))
              '(FL (DF5 F5 AF5) VLA (E3 G3 B3)))
      (equalp (loop for ss in (data (related-sets mtls2)) 
                 collect (id ss)
                 collect (loop for p in (data ss) collect (data p)))
              '(MISSING (FS2 B5)))
      ;; MDE Mon Feb 13 15:05:27 2012 -- pitch-list-to-symbols ?
      (equalp 
       ;; (loop for p in (data mtls3) collect (data p))
       ;; (pitch-list-to-symbols (data mtls3))
       ;; init-pitch-list may also be useful
       (pitch-symbols mtls3)
       '(E3 G3 B3 D4 GF4 BF4 DF5 F5))
      (pitch-list= (data mtls4)
                   (init-pitch-list
                    '(F2 AF2 C3 EF3 G3 BF3 D4 F4 A4 CS5 E5 AF5 B5 EF6))))))

;;; SAR Mon Feb  6 21:18:33 GMT 2012 
(sc-deftest test-tl-set-transpose ()
  (let ((mtls (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6) 
                           :subsets '((fl (df5 f5 af5))
                                      (vla (e3 g3 b3)))
                           :related-sets '((missing (fs2 b5)))))
        ;; MDE Wed Aug 22 10:15:13 2018 -- test :lowest and :highest and make
        ;; sure method inheritance works 
        (mtls2 (make-complete-set
                '(d2 f2 a2 c3 e3 gqf4 bf4 df5 fs5 af5 c6) 
                :subsets '((fl (df5 fs5 af5))
                           (vla (e3 bf4 af5)))
                :related-sets '((missing (fs2 b5))))))
    ;; (print mtls2)
    (sc-test-check
      (transpose mtls 3 :destructively t)
      (equalp (pitch-symbols mtls)
              '(F2 AF2 C3 EF3 G3 BF3 D4 F4 A4 CS5 E5 AF5 B5 EF6))
      (equalp (loop for ss in (data (subsets mtls)) 
                 collect (id ss)
                 collect (loop for p in (data ss) collect (data p)))
              '(FL (E5 AF5 B5) VLA (G3 BF3 D4)))
      (equalp (loop for ss in (data (related-sets mtls)) 
                 collect (id ss)
                 collect (loop for p in (data ss) collect (data p)))
              '(MISSING (FS2 B5)))
      ;; MDE Wed Aug 22 10:15:46 2018
      (transpose mtls2 3.5 :do-related-sets t :destructively t
                 :lowest 'c3 :highest 'g5)
      (equalp (pitch-symbols mtls2)
              '(d2 f2 a2 eqf3 gqs3 bf4 dqf5 eqs5 af5 aqs5 c6))
      (transpose mtls 2 :do-related-sets t :destructively t)
      (equalp (loop for ss in (data (related-sets mtls)) 
                 collect (id ss)
                 collect (loop for p in (data ss) collect (data p)))
              '(MISSING (AF2 CS6))))))

;;; SAR Mon Feb  6 21:40:25 GMT 2012
(sc-deftest test-tl-set-limit ()
  (let ((mtls (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6) 
                           :subsets '((fl (df5 f5 af5))
                                      (vla (e3 g3 b3)))
                           :related-sets '((missing (fs2 b5))))))
    (sc-test-check
      (limit mtls :upper 'c6 :lower 'c3)
      (equalp (pitch-symbols mtls)
              '(C3 E3 G3 B3 D4 GF4 BF4 DF5 f5 af5 c6))
      (equalp (loop for ss in (data (related-sets mtls)) 
                 collect (id ss)
                 collect (loop for p in (data ss) collect (data p)))
              '(MISSING (FS2 B5)))
      (limit mtls :upper 'c6 :lower 'c3 :do-related-sets t)
      (equalp (loop for ss in (data (related-sets mtls)) 
                 collect (id ss)
                 collect (loop for p in (data ss) collect (data p)))
              '(MISSING (B5))))))

;;; SAR Wed Feb  8 10:44:19 GMT 2012
(sc-deftest test-tl-set-stack ()
  (let* ((mtls (make-tl-set '(c4 e4 g4)))
         (mtls-stack (stack mtls 3)))
    (sc-test-check
      (equalp (pitch-symbols mtls-stack)
              '(EF2 GF2 BF2 DF3 F3 AF3 C4 E4 G4 B4 D5 GF5 A5 DF6 E6)))))

;;; SAR Sat Feb 11 11:47:33 GMT 2012
(sc-deftest test-tl-set-limit-for-instrument ()
  (let ((mtls (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
                           :subsets '((vc (d2 bf4 c6)))
                           :related-sets '((other-notes (b4 e5 fs5 c6)))))
        (mi (make-instrument 'flute 
                             :staff-name "Flute" :staff-short-name "Fl."
                             :lowest-written 'c4 :highest-written 'd7 
                             :starting-clef 'treble 
                             :midi-program 74 :chords nil 
                             :microtones t :missing-notes '(cqs4 dqf4))))
    (sc-test-check
      (limit-for-instrument mtls mi :do-related-sets nil)
      (print mtls)
      (equalp (pitch-symbols mtls) '(D4 GF4 BF4 DF5 F5 AF5 C6))
      (equalp (loop for nobj in (data (related-sets mtls)) 
                 collect (loop for p in (data nobj) 
                            collect (data p)))
              '((B4 E5 FS5 C6)))
      (limit-for-instrument mtls mi :upper 'b5 :lower 'c5)
      (equalp (pitch-symbols mtls) '(DF5 F5 AF5))
      (limit-for-instrument mtls mi :upper 'b5 :lower 'c5 :do-related-sets t)
      (and 
       (equalp (pitch-symbols mtls) '(DF5 F5 AF5))
       (equalp (loop for nobj in (data (related-sets mtls)) 
                  collect (loop for p in (data nobj) 
                             collect (data p)))
               '((E5 FS5)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set-palette tests

;;; SAR Tue Feb  7 11:57:44 GMT 2012
(sc-deftest test-set-palette-make-set-palette ()
  (let* ((msp (make-set-palette 
              'test
              '((1 ((1
                     ;; MDE Thu Jun 16 17:38:58 2016 -- more some of the
                     ;; octaves and make sure everything still works 
                     ((bf1 ef2 aqf2 c3 e gqf gqs cs4 d g a cqs5 dqf gs b))) 
                    (2
                     ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5
                           gqf5) 
                      :subsets
                      ((tc1 (d2 g3 cs5))
                       (tc2 (eqs2 f3 bqf3))
                       (tc3 (b2 bqs3 gqf5)))))))
                (2 ((1 ((1 1) :transposition 5))
                    (2 ((1 2) :transposition 5))))
                (3 ((1 ((1 1) :transposition -2))
                    (2 ((1 2) :transposition -2)))))))
         (spt (transpose (clone msp) 3)))
    ;; MDE Wed Jul 29 15:01:29 2015
    (multiple-value-bind (dmin dmax cmin cmax)
        (quality-extremes msp)
      ;; (format t "~%~a ~a ~a ~a" dmin dmax cmin cmax)
      (print (print msp))
      (sc-test-check
        ;; MDE Fri Jan 29 17:53:20 2016
        (bass-repeat msp '((1 1) (1 2) (2 2) (3 1) (2 2)))
        (not (bass-repeat msp '((1 1) (2 2) (1 2) (3 1) (2 2))))
        (set-palette-p msp)
        (> dmax dmin 0.0)
        (> cmax cmin 0.0)
        (every #'set-palette-p (loop for i in (data msp) collect (data i)))
        (every #'pitch-p
               (flatten (loop for i in (data msp) 
                           collect (loop for j in (data (data i)) 
                                      collect (loop for p in (data j)
                                                 collect p)))))
        (pitch-list=
         (get-data-data '(2 1) msp)
         (init-pitch-list 
          '(EF2 AF2 DQF3 F3 A3 BQS3 CQS4 FS4 G4 C5 D5 FQS5 GQF5 CS6 E6)))
        (equalp
         (loop for i in (data msp) 
            collect (id i)
            collect (loop for j in (data (data i)) 
                       collect (id j)
                       collect (loop for p in (data j) 
                                  collect (data p))))
         '(1
           (1 (BF1 EF2 AQF2 C3 E3 GQF3 GQS3 CS4 D4 G4 A4 CQS5 DQF5 GS5 B5)  
            2 (BF1 D2 FQF2 FQS2 B2 C3 F3 G3 BQF3 BQS3 FS4 GS4 A4 CS5
               GQF5)) 
           2
           (1 (EF2 AF2 DQF3 F3 A3 BQS3 CQS4 FS4 G4 C5 D5 FQS5 GQF5 CS6 E6) 
            2 (EF2 G2 AQS2 BQF2 E3 F3 BF3 C4 EQF4 EQS4 B4 CS5 D5 FS5
               BQS5)) 
           3
           (1 (AF1 CS2 GQF2 BF2 D3 EQS3 FQS3 B3 C4 F4 G4 BQF4 BQS4 FS5 A5)  
            2 (AF1 C2 DQS2 EQF2 A2 BF2 EF3 F3 AQF3 AQS3 E4 FS4 G4 B4
               EQS5))))
        ;; MDE Wed Aug 19 15:46:17 2015
        ;; (print (get-pitch-symbols (get-data '(3 2) msp)))
        (limit-shift-octave msp :upper '(0 c6 100 c4) :lower '(0 c1 100 e2))
        ;; (print (get-pitch-symbols (get-data '(3 2) msp)))
        ;; MDE Tue Dec 13 10:25:43 2016 -- test new sp transpose method
        (set-palette-p spt)
        (equalp '(cs2 fs2 bqs2 ef3)
                (subseq (pitch-symbols (get-data '(1 1) spt)) 0 4))
        (pitch= (make-pitch 'b5) (highest (get-data '(1 1) msp)))
        (pitch= (make-pitch 'bf1) (lowest (get-data '(1 1) msp)))
        (pitch= (make-pitch 'c4) (highest (get-data '(3 2) msp)))
        (pitch= (make-pitch 'a2) (lowest (get-data '(3 2) msp)))))))

;;; SAR Tue Feb  7 14:12:50 GMT 2012
(sc-deftest test-set-palette-find-sets-with-pitches ()
  (let* ((msp (make-set-palette 
               'test
               '((1 ((1
                      ((g3 c4 e4 g4)))
                     (2
                      ((c4 d4 e4 g4)))))
                 (2 ((1 ((1 1) :transposition 5))
                     (2 ((1 2) :transposition 5))))
                 (3 ((1 ((1 1) :transposition -2))
                     (2 ((1 2) :transposition -2)))))))
         (msp-fswp1 (find-sets-with-pitches msp '(c4)))
         (msp-fswp2 (find-sets-with-pitches msp '(c4 f4))))
    (sc-test-check
      (equalp (loop for cs in msp-fswp1 
                 collect (loop for p in (data cs)
                            collect (data p)))
              '((BF3 C4 D4 F4) (C4 F4 A4 C5) (C4 D4 E4 G4) (G3 C4 E4 G4))) 
      (equalp (loop for cs in msp-fswp2 
                 collect (loop for p in (data cs)
                            collect (data p)))
              '((BF3 C4 D4 F4) (C4 F4 A4 C5))))))

;;; SAR Tue Feb  7 14:50:55 GMT 2012
(sc-deftest test-set-palette-force-micro-tone ()
  (let ((msp (make-set-palette 
              'test
              '((1 ((1
                     ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5 dqf5 gs5
                           b5))) 
                    (2
                     ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5
                           gqf5) 
                      :subsets
                      ((tc1 (d2 g3 cs5))
                       (tc2 (eqs2 f3 bqf3))
                       (tc3 (b2 bqs3 gqf5)))))))
                (2 ((1 ((1 1) :transposition 5))
                    (2 ((1 2) :transposition 5))))
                (3 ((1 ((1 1) :transposition -2))
                    (2 ((1 2) :transposition -2))))))))
    (sc-test-check
      (equalp (loop for i in (data msp) 
                 collect (loop for j in (data (data i))
                            collect (loop for p in (data j)
                                       collect (micro-tone p))))
              '(((NIL NIL T NIL NIL T T NIL NIL NIL NIL T T NIL NIL)
                 (NIL NIL T T NIL NIL NIL NIL T T NIL NIL NIL NIL T))
                ((NIL NIL T NIL NIL T T NIL NIL NIL NIL T T NIL NIL)
                 (NIL NIL T T NIL NIL NIL NIL T T NIL NIL NIL NIL T))
                ((NIL NIL T NIL NIL T T NIL NIL NIL NIL T T NIL NIL)
                 (NIL NIL T T NIL NIL NIL NIL T T NIL NIL NIL NIL T))))
      (force-micro-tone msp 't)
      (equalp (loop for i in (data msp) 
                 collect (loop for j in (data (data i))
                            collect (loop for p in (data j)
                                       collect (micro-tone p))))
              '(((T T T T T T T T T T T T T T T) 
                 (T T T T T T T T T T T T T T T)) 
                ((T T T T T T T T T T T T T T T) 
                 (T T T T T T T T T T T T T T T))
                ((T T T T T T T T T T T T T T T) 
                 (T T T T T T T T T T T T T T T)))))))

;;; SAR Tue Feb  7 14:56:24 GMT 2012
(sc-deftest test-set-palette-set-palette-p ()
  (let ((msp (make-set-palette 
              'test
              '((1 ((1
                     ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5 dqf5 gs5
                           b5))) 
                    (2
                     ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5
                           gqf5) 
                      :subsets
                      ((tc1 (d2 g3 cs5))
                       (tc2 (eqs2 f3 bqf3))
                       (tc3 (b2 bqs3 gqf5)))))))
                (2 ((1 ((1 1) :transposition 5))
                    (2 ((1 2) :transposition 5))))
                (3 ((1 ((1 1) :transposition -2))
                    (2 ((1 2) :transposition -2)))))))
        (mscs (make-sc-set '(d2 cqs3 fs3 cs4 e4 c5 af5 ef6))))
    (sc-test-check
      (set-palette-p msp)
      (not (set-palette-p mscs)))))

;;; SAR Tue Feb  7 18:09:10 GMT 2012
(sc-deftest test-set-palette-ring-mod ()
  (sc-test-check
    (equalp (ring-mod 'c4 'd4)
            '(32 64 198 230 326 491 555 817 849 1079 1111 1372))
    (equalp (ring-mod '261.63 '293.66)
            '(32 64 198 230 326 491 555 817 849 1079 1111 1372))
    (equalp (ring-mod 'c4 'd4 
                      :return-notes t 
                      :scale cm::*chromatic-scale*)
            '(C1 C2 G3 BF3 E4 B4 CS5 AF5 CS6 F6))
    (equalp (ring-mod 'c4 'd4 
                      :return-notes t 
                      :scale (cm::find-object 'cm::quarter-tone))
            '(BQS0 BQS1 G3 AQS3 E4 B4 CS5 GQS5 AQF5 CQS6 CS6 EQS6))
    (equalp (ring-mod '261.63 '293.66 :pitch1-partials 5 :pitch2-partials 3) 
            '(32 64 96 166 198 230 326 358 427 459 491 555 619 721 753 817 849 
              1014 1079 1111 1143 1340 1372 1404 1602 1634 1666 1895 1928
              2189)) 
    (equalp (ring-mod 'c4 'd4 :min-freq 100 :max-freq 1000)
            '(198 230 326 491 555 817 849))
    (every #'floatp (ring-mod 'c4 'd4 :round nil))
    (every #'integerp (ring-mod 'c4 'd4 :round t))
    (let ((l (ring-mod 'c4 'd4 
                       :return-notes t 
                       :scale cm::*chromatic-scale* 
                       :remove-duplicates nil)))
      (not (every #'(lambda (x) (= x 1))
                  (loop for i in (remove-duplicates l) 
                     collect (count i l)))))
    (equalp (ring-mod 'c4 'd4 
                      :return-notes t 
                      :scale cm::*chromatic-scale*
                      :remove-octaves t)
            '(C1 G3 BF3 E4 B4 CS5 AF5 F6))))

;;; SAR Sat Feb 11 15:31:47 GMT 2012
(sc-deftest test-set-palette-ring-mod-bass ()
  (let ((rmb1 (ring-mod-bass '(261.63 293.66 329.63 349.23)))
        (rmb2 (ring-mod-bass '(261.63 293.66 329.63 349.23)
                             :return-notes t))
        (rmb3 (ring-mod-bass '(261.63 293.66 329.63 349.23)
                             :return-notes t
                             :scale cm::*chromatic-scale*))
        (rmb4 (ring-mod-bass '(261.63 293.66 329.63 349.23 392.00) 
                             :return-notes t
                             :scale cm::*chromatic-scale*
                             :bass-octave 1))
        (rmb5 (ring-mod-bass '(261.63 293.66 329.63 349.23 392.00) 
                             :return-notes t
                             :scale cm::*chromatic-scale*
                             :bass-octave 1
                             :low 'e1
                             :high 'a1))
        (rmb6 (ring-mod-bass '(261.63 293.66 329.63 349.23 392.00) 
                             :bass-octave 1
                             :low 'e1
                             :high 'a1
                             :round NIL)))
    (sc-test-check
      ;; MDE Mon Mar 19 19:46:41 2012 
      (= 31 (first (ring-mod-bass '(62.85714177508388 314.2857088754194))))
      (every #'integerp rmb1)
      (equalp rmb1 '(28 29 32))
      (> (first rmb1) (note-to-freq 'a0))
      (< (first (last rmb1)) (note-to-freq 'g3))
      (equalp rmb2 '(A0 BF0 BQS0))
      (equalp rmb3 '(A0 BF0 C1))
      (equalp rmb4 '(CS1 D1 F1 G1 A1 B1))
      (equalp rmb5 '(F1 G1))
      ;; MDE Mon Mar 19 18:48:10 2012
      (every #'(lambda (x y) (equal-within-tolerance x y 0.01))
             rmb6
             '(42.76999999999998 43.45666666666667 43.80000000000001
               49.16999999999999))
      (not (ring-mod-bass '(261.63) :warn nil)))))

;;; SAR Wed Feb 15 10:57:00 GMT 2012
;;; SAR Sat May 19 14:43:10 EDT 2012: Changed the result pitches
(sc-deftest test-set-palette-set-palette-from-ring-mod ()
  (let ((spfrm1 (set-palette-from-ring-mod 'a4 'spfrm-test))
        (spfrm2 (set-palette-from-ring-mod 'a4 'spfrm-test
                                           :partials '(2 4 6 8)))
        (spfrm3 (set-palette-from-ring-mod 'a4 'spfrm-test
                                           :do-bass nil))
        (spfrm4 (set-palette-from-ring-mod 'a4 'spfrm-test
                                           :remove-octaves t)))
    (sc-test-check
      (set-palette-p spfrm1)
      ;; MDE Mon Mar 19 19:50:07 2012 -- changed ring-mod-bass routine to round
      ;; frequencies 
      (equalp (loop for cs in (data spfrm1) collect (pitch-symbols cs))
              '((B0 BQS1 BQS2 GQF3 BQS3 EF4 GQF4 A4 BQS4 DQF5)
                (BQS0 DQF6 DQS6 EF6 F6 FQS6 GQF6 DQS7 EF7 EQF7 EQS7 F7)
                (B0 BQS2 GQF3 BQS3 GQF4 A4 BQS4 DQF5 F5 GQF5 GQS5)
                (BQS0 BQS6 C7 CQS7 DQF7 D7 DQS7)
                (B0 BQS3 EF4 GQF4 BQS4 DQF5 EF5 F5 GQF5 GQS5 BF5 BQS5 CQS6)
                (BQS0 FQS7 FS7 GQF7 G7 GQS7)
                (B0 GQF5 BF5 DQF6 GQF6 AQF6 BF6 C7 DQF7 DQS7 F7 GQF7 GQS7)
                (B0 BQS1 BQS2 BQS3 A4 BQS4 F5 GQS5 A5 BQS5 DQS6)
                (BQS0 GQS6 AQS6 BQS6 DQS7 EQF7 F7 BQF7 BQS7)
                (B0 BQS1 BQS2 BQS3 EF4 BQS4 EF5 F5 GQS5 BQS5 CQS6 EF6 FQS6)
                (BQS0 EF7 EQS7 FQS7 GQS7 AQF7 AQS7)
                (B0 F5 BQS5 GQS6 B6 BQS6 D7 FS7 GQS7 AF7 AQS7) 
                (B0 A4 A5 A6 GQF7 A7)
                (B0 A4 A5 A6 CS7 GQF7 A7) 
                (BQS0 A4 A5 A6 CS7 A7)
                (BQS0 EF6 G6 BF6 EF7 F7 G7 AQF7 BF7 BQS7)
                (B0 BQS1 BQS2 GQF3 BQS3 BQS4 DQF5 GQF5 CQS6 DQS6 F6 GQF6 AQS6) 
                (BQS0 CQS7 DQS7 F7 AQF7 BF7 BQS7) (BQS0 E6 A6 E7 GQF7 A7 B7)
                (B0 A4 A5 E6 A6 A7 B7) 
                (BQS0 A6 CS7 E7 A7 B7)))
      (equalp (loop for cs in (data spfrm2) collect (pitch-symbols cs))
              '((BQS0 CS5 E5 GQF5 B5 CS6 DQS6 FQS6 GQF6 AF6 BF6 B6 C7)
                (BQF0 B0 A2 A3 E4 A4 CS5 E5 GQF5)
                (BQS0 FQS6 GQF6 AF6 BF6 B6 C7 GQS7 AF7 AQF7 AQS7 BF7 BQF7)
                (B0 A2 A3 E4 A4 CS5 E5 GQF5 A5 B5) 
                (BQS0 DQF7 DQS7 EQF7 EQS7 FQS7 FS7)
                (BQF0 A2 A3 E4 CS5 E5 GQF5 B5 CS6 DQS6)
                (AQS0 BQF0 B0 GQS7 AF7 AQF7 AQS7 BF7 BQF7)
                (B0 A4 E5 CS6 E6 GQF6 B6 CS7 DQS7 FQS7 GQF7) 
                (B0 A5 A6 E7 A7)
                (B0 A3 CS5 CS6 DQS6 FQS6 GQF6 B6 C7 DQF7 DQS7 FS7 AF7) 
                (BQS0 A5 A6 E7 A7)
                (B0 A4 A5 E6 A6 CS7 E7 GQF7 A7) 
                (BQS0 A5 A6 E7)
                (BQS0 CS6 E6 GQF6 B6 CS7 DQS7 FQS7 GQF7 AF7 BF7 B7 C8)
                (BQF0 B0 BQS0 A2 A3 E4 A4 CS5 GQF5 A5 B5 CS6 E6)
                (BQS0 B6 CS7 DQS7 FQS7 GQF7 AF7) 
                (B0 A3 A4 E5 A5 CS6 E6 GQF6)
                (B0 BQS0 FQS7 GQF7 AF7 BF7 B7 C8)
                (BQS0 CS6 FQS6 C7 DQS7 FQS7 GQS7 BQF7 C8) 
                (B0 A5 A6 E7 A7)
                (BQS0 A5 E6 CS7 E7 GQF7 B7) 
                (BQS0 A6 A7) 
                (BQS0 AF6 B6 DQF7 FS7 AF7 AQS7)
                (BQF0 B0 BQS0 A2 A3 CS5 GQF5 CS6 DQS6 FQS6 GQF6 BF6)
                (BQS0 EQF7 FQS7 GQS7 BQF7 C8) 
                (BQS0 A6 CS7 GQF7 A7) (B0 A5 A6)
                (BQS0 CS7 E7 GQF7 B7)))
      (equalp (loop for cs in (data spfrm3) collect (pitch-symbols cs))
              '((BQS1 BQS2 GQF3 BQS3 EF4 GQF4 A4 BQS4 DQF5)
                (DQF6 DQS6 EF6 F6 FQS6 GQF6 DQS7 EF7 EQF7 EQS7 F7)
                (BQS2 GQF3 BQS3 GQF4 A4 BQS4 DQF5 F5 GQF5 GQS5)
                (BQS6 C7 CQS7 DQF7 D7 DQS7)
                (BQS3 EF4 GQF4 BQS4 DQF5 EF5 F5 GQF5 GQS5 BF5 BQS5 CQS6)
                (FQS7 FS7 GQF7 G7 GQS7)
                (GQF5 BF5 DQF6 GQF6 AQF6 BF6 C7 DQF7 DQS7 F7 GQF7 GQS7)
                (BQS1 BQS2 BQS3 A4 BQS4 F5 GQS5 A5 BQS5 DQS6)
                (GQS6 AQS6 BQS6 DQS7 EQF7 F7 BQF7 BQS7)
                (BQS1 BQS2 BQS3 EF4 BQS4 EF5 F5 GQS5 BQS5 CQS6 EF6 FQS6)
                (EF7 EQS7 FQS7 GQS7 AQF7 AQS7)
                (F5 BQS5 GQS6 B6 BQS6 D7 FS7 GQS7 AF7 AQS7) 
                (A4 A5 A6 GQF7 A7)
                (A4 A5 A6 CS7 GQF7 A7) 
                (A4 A5 A6 CS7 A7)
                (EF6 G6 BF6 EF7 F7 G7 AQF7 BF7 BQS7)
                (BQS1 BQS2 GQF3 BQS3 BQS4 DQF5 GQF5 CQS6 DQS6 F6 GQF6 AQS6)
                (CQS7 DQS7 F7 AQF7 BF7 BQS7) 
                (E6 A6 E7 GQF7 A7 B7) 
                (A4 A5 E6 A6 A7 B7)
                (A6 CS7 E7 A7 B7)))
      (equalp (loop for cs in (data spfrm4) collect (pitch-symbols cs))
              '((B0 BQS1 GQF3 EF4 A4 DQF5) 
                (BQS0 DQF6 DQS6 EF6 F6 FQS6 GQF6 EQF7 EQS7)
                (B0 BQS2 GQF3 A4 DQF5 F5 GQS5) 
                (BQS0 C7 CQS7 DQF7 D7 DQS7)
                (B0 BQS3 EF4 GQF4 DQF5 F5 GQS5 BF5 CQS6) 
                (BQS0 FQS7 FS7 GQF7 G7 GQS7)
                (B0 GQF5 BF5 DQF6 AQF6 C7 DQS7 F7 GQS7) 
                (B0 BQS1 A4 F5 GQS5 DQS6)
                (BQS0 GQS6 AQS6 DQS7 EQF7 F7 BQF7) 
                (B0 BQS1 EF4 F5 GQS5 CQS6 FQS6)
                (BQS0 EF7 EQS7 FQS7 GQS7 AQF7 AQS7) 
                (B0 F5 BQS5 GQS6 D7 FS7 AF7 AQS7)
                (B0 A4 GQF7) (B0 A4 CS7 GQF7) 
                (BQS0 A4 CS7) 
                (BQS0 EF6 G6 BF6 F7 AQF7)
                (B0 BQS1 GQF3 DQF5 CQS6 DQS6 F6 AQS6) 
                (BQS0 CQS7 DQS7 F7 AQF7 BF7)
                (BQS0 E6 A6 GQF7 B7) 
                (B0 A4 E6) 
                (BQS0 A6 CS7 E7 B7))))))

;;; MDE Fri Aug 24 15:43:18 2018 
#+clm
(sc-deftest test-set-palette-from-spectra ()
  (let ((sp (set-palette-from-spectra 
             (concatenate
              'string cl-user::+slippery-chicken-home-dir+ 
              "tests/test-sndfiles-dir-1/test-sndfile-3.aiff"))))
    (print-simple sp) ; just to make sure it works
    (sc-test-check
      (= 7 (sclist-length sp)))))


;;; SAR Wed Jun 13 15:02:57 BST 2012
(sc-deftest test-set-palette-gen-max-coll-file ()
  (let ((msp (make-set-palette 
              'test
              '((1 ((1
                     ((g3 c4 e4 g4)))
                    (2
                     ((c4 d4 e4 g4)))))
                (2 ((1 ((1 1) :transposition 5))
                    (2 ((1 2) :transposition 5))))
                (3 ((1 ((1 1) :transposition -2))
                    (2 ((1 2) :transposition -2))))))))
    (probe-delete "/tmp/msp-mcf.txt")
    (probe-delete "/tmp/msp-mcfm.txt")
    (sc-test-check
      (gen-max-coll-file msp "/tmp/msp-mcf.txt" 'freq)
      (gen-max-coll-file msp "/tmp/msp-mcfm.txt" 'midi)
      (gen-max-coll-file msp "/tmp/msp-mcft.txt" 'transp)
      (file-write-ok "/tmp/msp-mcf.txt" 45)
      (file-write-ok "/tmp/msp-mcft.txt" 45)
      (file-write-ok "/tmp/msp-mcfm.txt" 45))))

;;; MDE Sat May 18 13:32:24 2013 
(sc-deftest test-ring-mod-piece ()
  (labels
      ((make-rm-rsm-aux (num-seqs num-rthm-seqs ins)
         (list ins (loop repeat num-seqs collect (1+ (random num-rthm-seqs)))))
       (make-rm-rsm (instruments num-seqs num-rthm-seqs)
         (loop for ins in instruments collect
              (make-rm-rsm-aux num-seqs num-rthm-seqs ins))))
    (probe-delete "/tmp/sp.eps")
    (probe-delete "/tmp/_mini-template-score.ly")
    (probe-delete "/tmp/mini-template.mid")
    (let* ((pitches '(c4 gs4 g3 b3))
           (set-palette
            (recursive-set-palette-from-ring-mod 
             pitches 'altogether 
             :ring-mod-bass-octave 2 :min-bass-notes 2
             :remove-octaves 2
             :warn-no-bass nil :partials '(5 6 7 9)))
           (num-seqs 60)
           (mini
            (progn 
              (force-micro-tone set-palette nil)
              (make-slippery-chicken
               '+mini+
               :title "mini template"
               :ensemble '(((cl (bass-clarinet :midi-channel 5))
                            (vn (violin :midi-channel 1))
                            (va (piano :midi-channel 2))
                            (vc (cello :midi-channel 3))
                            (cb (double-bass :midi-channel 4))))
               :tempo-map '((1 (q 180)))
               :set-palette set-palette
               :set-map `((1 ,(loop repeat num-seqs collect
                                   (list 'c4 (1+ (random 7))))))
               :rthm-seq-palette '((1 ((((2 4) (s) e (s) - e e -))
                                       :pitch-seq-palette ((1 2 3))))
                                   (2 ((((2 4) e (s) s - +s e - (s)))
                                       :pitch-seq-palette ((2 3 1))))
                                   (3 ((((2 4) - e e - e (e)))
                                       :pitch-seq-palette ((3 1 2)))))
               :rthm-seq-map
               `((1 ,(make-rm-rsm '(cl vn va vc cb) num-seqs 3)))))))
      (midi-play mini)
      #+cmn
      (cmn-display set-palette :file "/tmp/sp.eps"
                   :include-missing-non-chromatic t
                    :include-missing-chromatic t)
      #+cmn
      (cmn-display mini :display-sets t :write-section-info nil
                   :empty-staves nil :in-c nil)
      (write-lp-data-for-all mini)
      ;; MDE Mon Jun 17 12:12:24 2013 
      (multiple-value-bind
            (midi-events num-midi-events)
          (cm::parse-midi-file "/tmp/mini-template.mid")
        (sc-test-check
          ;; MDE Tue Jun  9 17:52:51 2020, Heidhausen -- this is as good a place
          ;; as any to try out reset-midi-channels
          (= 5 (get-midi-channel (get-note mini 1 1 'cl)))
          (set-midi-channel (get-note mini 1 1 'cl) 1)
          (= 1 (get-midi-channel (get-note mini 1 1 'cl)))
          (reset-midi-channels mini)
          (= 5 (get-midi-channel (get-note mini 1 1 'cl)))
          #+cmn (file-write-ok "/tmp/sp.eps" 900000)
          (file-write-ok "/tmp/_mini-template-score.ly" 160)
          (file-write-ok "/tmp/mini-template.mid" 700)
          ;; MDE Wed Jun 29 13:54:00 2016 -- try import/export here too
          (event-list-to-midi-file
           (midi-file-to-events "/tmp/mini-template.mid")
           :midi-file "/tmp/mftoe.mid")
          (file-write-ok "/tmp/mftoe.mid" 700)
          (= 911 num-midi-events)
          ;; I'd like to test specific event channel writing but importing
          ;; events in CM doesn't result in the same list each time
          (every #'(lambda (x) (member x '(0 1 2 3 4)))
                 (loop for m in midi-events when (typep m 'cm::midi) collect
                      (cm::midi-channel m))))))))

;; MDE Thu Jul 30 13:15:11 2015
(sc-deftest test-auto-sequence ()
  (let* ((sp (recursive-set-palette-from-ring-mod '(a4 b4) 'spfrm-test
                                                  :warn-no-bass nil))
         (first-last nil)
         (num-sets (num-data sp)))
    (flet ((good-refs (list)
             ;; we should get different results with these different calls
             (and (= (length list) num-sets)
                  (prog1 
                      (not (equalp first-last (first list)))
                    (setf first-last (first list)))
                  (every #'(lambda (x)
                             (and (listp x) (= 2 (length x))
                                  (symbolp (first x))
                                  (numberp (second x))))
                         list))))
      ;; (print num-sets)
      ;; (print sp)
      (sc-test-check
        (good-refs (auto-sequence sp :verbose nil :silent t))
        ;; MDE Fri Jan 29 18:38:05 2016 -- try the permutate algorithm now too
        (good-refs (auto-sequence sp :verbose nil :permutate t :silent t
                                  :dissonance-env '(0 1 80 .1 120 .9)
                                  :repeating-bass t))
        (good-refs (auto-sequence sp :verbose nil :centroid-weight 2 :silent t
                                  :dissonance-env '(0 1 80 .1 120 .9)))
        ;; MDE Fri Jan 29 18:46:39 2016 -- again
        (good-refs (auto-sequence sp :verbose nil :permutate t :silent t))
        (good-refs                      ; focussing just on centroid
         (auto-sequence sp :verbose nil :dissonance-env nil :silent t))
        (good-refs
         ;; don't worry about repeating bass notes
         (auto-sequence sp :verbose nil :repeating-bass t
                        :dissonance-env '(0 .7 80 1 120 .3)
                        :silent t))
        (good-refs                      ; focussing just on dissonance
         (auto-sequence sp :verbose nil :dissonance-env '(0 .3 50 0.9 100 .5)
                        :centroid-env nil :repeating-bass t :silent t))
        ;; worry about repeating bass again
        (good-refs (auto-sequence sp :verbose nil
                                  :dissonance-env '(0 1 50 0 100 .5)
                                  :centroid-env nil :repeating-bass nil
                                  :silent t))
        (set-palette-p
         (setq sp (recursive-set-palette-from-ring-mod '(af2 cs3) 'spfrm-test2
                                                       :warn-no-bass nil)))
        (good-refs (auto-sequence
                    sp :dissonance-weight 153
                    :dissonance-env '(0 .3 20 1 30 0 70 .5 80 .1 120 .9)
                    :centroid-env '(0 .5 7 .6 12 .72 20 0 25 1 30 0)
                    :silent t :verbose nil))))))

;;; SAR Wed Jun 13 15:09:28 BST 2012
(sc-deftest test-set-palette-gen-midi-chord-seq ()
  (let ((msp (make-set-palette 
              'test
              '((1 ((1
                     ((bf1 ef2 c3 e3 cs4 d4 g4 a4 gs5 b5)))
                    (2
                     ((bf1 d2 b2 c3 f3 g3 fs4 gs4 a4 cs5)
                      :subsets
                      ((tc1 (d2 g3 cs5))
                       (tc2 (d2 f3 g3))
                       (tc3 (bf1 g3 cs5)))))))
                (2 ((1 ((1 1) :transposition 5))
                    (2 ((1 2) :transposition 5))))
                (3 ((1 ((1 1) :transposition -2))
                    (2 ((1 2) :transposition -2))))))))
    (probe-delete "/tmp/msp-gmchs.mid")
    (sc-test-check
      (gen-midi-chord-seq msp "/tmp/msp-gmchs.mid")
      (file-write-ok "/tmp/msp-gmchs.mid" 51))))

;;; SAR Wed Jun 13 15:12:43 BST 2012
#+cmn
(sc-deftest test-set-palette-cmn-display ()
  (let ((msp (make-set-palette 
              'test
              '((1 ((1
                     ((bf1 ef2 c3 e3 cs4 d4 g4 a4 gs5 b5)))
                    (2
                     ((bf1 d2 b2 c3 f3 g3 fs4 gs4 a4 cs5)
                      :subsets
                      ((tc1 (d2 g3 cs5))
                       (tc2 (d2 f3 g3))
                       (tc3 (bf1 g3 cs5)))))))
                (2 ((1 ((1 1) :transposition 5))
                    (2 ((1 2) :transposition 5))))
                (3 ((1 ((1 1) :transposition -2))
                    (2 ((1 2) :transposition -2))))))))
    (probe-delete "/tmp/sp-output.eps")
    (sc-test-check
      (cmn-display msp
                   :file "/tmp/sp-output.eps"
                   :font-size 8
                   :break-line-each-set nil
                   :size 10
                   :include-missing-chromatic nil
                   :include-missing-non-chromatic nil)
      (file-write-ok "/tmp/sp-output.eps" 30000))))

;;; MDE Fri May 17 19:32:57 2013 -- test creation of set-palette from
;;; individual sets made from a call to stack (along with a couple of other
;;; things) 
(sc-deftest test-set-palette-from-stack ()
  (labels
      ((make-rm-rsm-aux (num-seqs num-rthm-seqs ins)
         (list ins (loop repeat num-seqs collect (1+ (random num-rthm-seqs)))))
       (make-rm-rsm (instruments num-seqs num-rthm-seqs)
         (loop for ins in instruments collect
              (make-rm-rsm-aux num-seqs num-rthm-seqs ins))))
    (let* ((s1 (stack (make-complete-set '(c4 e4 fs4 as4)) 3))
           (s2 (stack (make-complete-set '(c4 ef4 f4 a4)) 3))
           (mini
            (make-slippery-chicken
             '+mini+
             :title "mini template"
             :ensemble '(((cl (bass-clarinet :midi-channel 5))
                          (vn (violin :midi-channel 1))
                          (va (piano :midi-channel 2))
                          (vc (cello :midi-channel 3))
                          (cb (double-bass :midi-channel 4))))
             :tempo-map '((1 (q 180)))
             :set-palette `((s1 ,s1)
                            (s2 ,s2))
             :rthm-seq-palette '((1 ((((2 4) (s) e (s) - e e -))
                                     :pitch-seq-palette ((1 2 3))))
                                 (2 ((((2 4) e (s) s - +s e - (s)))
                                     :pitch-seq-palette ((2 3 1))))
                                 (3 ((((2 4) - e e - e (e)))
                                     :pitch-seq-palette ((3 1 2)))))
             :set-map '((1 (s1 s2 s1 s2 s1)))
             :rthm-seq-map `((1 ,(make-rm-rsm '(cl vn va vc cb) 5 3))))))
      (probe-delete "/tmp/_mini-template-score.ly")
      (midi-play mini)
      #+cmn (cmn-display mini :display-sets t :write-section-info nil
                         :empty-staves nil :in-c nil)
      (write-lp-data-for-all mini)
      (sc-test-check
        (equalp 'fs1 (id (first (data (get-data 's1 (set-palette mini))))))
        (equalp 'a1 (id (first (data (get-data 's2 (set-palette mini))))))
        (file-write-ok "/tmp/_mini-template-score.ly" 170)
        ;; MDE Wed Sep 19 14:24:07 2018 -- test the thin method also,
        ;; deleting all notes and handling the ties also 
        (thin mini :curve '(0 0 100 0))
        (update-slots mini)
        (zerop (num-notes mini))))))


;;; MDE Mon May 20 13:09:46 2013
(sc-deftest test-set-palette-dups ()
  (let* ((pitches '(c4 c4 e4 cs5 cs5 d6))
         (s1 (make-complete-set pitches :rm-dups nil))
         (s2 (make-complete-set pitches :rm-dups t)))
    (sc-test-check
      (equalp '(C4 C4 E4 CS5 CS5 D6) (pitch-list-to-symbols (data s1)))
      (equalp '(C4 E4 CS5 D6) (pitch-list-to-symbols (data s2))))))

;;; MDE Thu Sep  5 11:35:46 2013 
(sc-deftest test-set-palette-combine ()
  (let* ((sp1 (make-set-palette 
               'sp1
              '((1 ((bf1 ef2 c3 e3 cs4 d4 g4 a4 gs5 b5)))
                (2 ((bf1 d2 b2 c3 f3 g3 fs4 gs4 a4 cs5))))))
         (sp2 (make-set-palette 
               'sp2
              '((3 ((c4 e4 g4 b4 d5 f5)))
                (4 ((e4 c4 g4 b4 f5 d5))))))
         (combo (combine sp1 sp2)))
    (sc-test-check
      (equalp '(C4 E4 G4 B4 D5 F5)
              (pitch-list-to-symbols (get-data-data 3 combo))))))

;;; MDE Wed Aug 12 19:13:22 2015 
(sc-deftest test-remove-similar ()
  (let* ((sp1 (make-set-palette 
               'sp1
              '((1 ((c4 e4 g4)))
                (2 ((c4 e4 g4))))))
         (sp2 (remove-similar sp1)))
    ;; (print sp2)
    (sc-test-check
      (= 1 (num-data sp2))
      (= 1 (num-data sp1))
      (chord-p (get-data 2 sp1))
      (chord-p (get-data 2 sp2))
      (not (get-data 1 sp1 nil))
      (not (get-data 1 sp2 nil)))))

;;; MDE Sat Mar  5 17:21:41 2016 
(sc-deftest test-wrap-palette ()
  (let ((sp 
         (make-set-palette
          nil
          '((1 ((c1 a1 ds2 cs3 g3 d4 f4 bf4 e5 b5 gs6 fs7)))
            (2 ((bf1 ef2 af2 g3 cs4 e4 c5 e5 b5 f6 d7 fs7 a7)))
            (3 ((g1 fs2 f3 as3 ds4 a4 gs5 c6 d6 ds6 cs7 e7 g7 b7)))
            (4 ((b0 e1 gs2 ds3 a3 cs4 fs4 as4 g5 c6 d7 f7)))
            (5 ((bf0 c2 f2 d3 b3 e4 a4 cs6 ef7 g7)))
            (6 ((d1 cs2 fs2 e3 c4 ef4 a4 cs6 f7 g6 af6 bf6 cf7)))
            (7 ((gs1 e2 b2 f3 fs3 g3 cs4 d5 c6 ef7 a6 bf7)))
            (8 ((gs1 fs2 a2 df3 f3 b3 e4 b4 g4 c5 d5 ef6 bf6 g7)))
            (9 ((cs1 a2 f3 b4 g5 ef3 bf3 e5 fs6 gs6 c6 d7)))
            (10 ((c3 e3 a4 ds5 f1 cs5 d5 as4 b4 fs5 gs5 g6)))
            (11 ((d3 fs3 gs4 cs5 g1 a2 c4 e6 f6 ds7 as7 b7)))
            (12 ((ef3 g3 f4 df5 bf1 a2 c4 d4 fs4 gs5 e6 b7)))))))
    (sc-test-check
      (equalp '(BF1 F3 E4 BF4 D5 G5 A5 C6 CS6 GS6 DS7 B7)
              (get-pitch-symbols (get-data 12 (wrap (clone sp)))))
      (eq 'b7 (data (highest (get-data 12 (wrap (clone sp) 2)))))
      (eq 'a7 (data (highest (get-data 2 (wrap (clone sp) 17)))))
      (eq 'g1 (data (lowest (get-data 11 (wrap (clone sp) 17))))))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chord tests

;;; SAR Wed Feb 15 11:15:42 GMT 2012
(sc-deftest test-chord-make-chord ()
  (let ((mch1 (make-chord '(c4 e4 g4 b4 d5 f5)))
        (mch2 (make-chord '(e4 c4 g4 b4 f5 d5)))
        (mch3 (make-chord '(e4 c4 g4 b4 f5 d5)
                          :auto-sort nil))
        (mch4 (make-chord '(cqs4 e4 gqf4 b4 dqf5 f5) 
                          :midi-channel 11
                          :microtones-midi-channel 12)))
    (sc-test-check
      ;; MDE Fri Jan 29 17:30:27 2016 
      (bass-repeat mch1 mch2)
      (bass-repeat mch3 mch2)
      (not (bass-repeat mch3 mch4))
      ;; enharmonics are equivalent here
      (bass-repeat mch2 (make-chord '(bs3 fs4)))
      (chord-p mch1)
      ;; MDE Tue May  8 22:07:48 2012 
      (output-midi-note mch1 0 0.5 1)
      (output-midi-note mch4 10 0.5 1)
      ;; MDE Tue Oct 29 19:13:53 2013 -- more flexible types can be
      ;; passed now 
      (chord-p (make-chord mch1))
      (chord-p (make-chord (data mch1)))
      (chord-p (make-chord (make-sc-set '(cs4 d5))))
      (chord-p (make-chord (make-pitch 'cs4)))
      ;; MDE Mon Nov 11 10:57:12 2013 
      (chord= mch1 mch2)
      (not (chord= mch1 mch3))
      (not (chord= mch1 mch4))
      (every #'pitch-p (data mch1))
      (equalp (loop for p in (data mch1) collect (data p))
              '(C4 E4 G4 B4 D5 F5))
      (equalp (loop for p in (data mch2) collect (data p))
              '(C4 E4 G4 B4 D5 F5))
      (equalp (loop for p in (data mch3) collect (data p))
              '(e4 c4 g4 b4 f5 d5))
      (every #'(lambda (x) (= x 1))
             (loop for p in (data mch1) collect (midi-channel p)))
      (equalp (loop for p in (data mch4) collect (midi-channel p))
              '(12 11 12 11 12 11)))))

;;; SAR Wed Feb 22 17:49:48 GMT 2012
(sc-deftest test-chord-set-midi-channel ()
  (let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
                          :midi-channel 11
                          :microtones-midi-channel 12)))
    (sc-test-check
      (equalp (loop for p in (data chrd) collect (midi-channel p))
              '(11 11 12 12 11 11))
      (not (set-midi-channel chrd 3 4))
      (equalp (loop for p in (data chrd) collect (midi-channel p))
              '(3 3 4 4 3 3)))))

;;; SAR Wed Feb 22 17:54:56 GMT 2012
(sc-deftest test-chord-get-midi-channel ()
  (let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
                          :midi-channel 11
                          :microtones-midi-channel 12)))
    (sc-test-check
      (= 11 (get-midi-channel chrd)))))

;;; SAR Wed Feb 22 18:02:57 GMT 2012
(sc-deftest test-chord-get-pitch ()
  (let ((chrd1 (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12))
        (chrd2 (make-chord '(ef4 c4 e4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12
                           :auto-sort nil)))
    (sc-test-check
      (equalp (data (get-pitch chrd1 1)) 'C4)
      (equalp (data (get-pitch chrd2 1)) 'EF4))))

;;; SAR Wed Feb 22 18:08:39 GMT 2012
(sc-deftest test-chord-get-pitch-symbols ()
  (let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
                          :midi-channel 11
                          :microtones-midi-channel 12)))
    (sc-test-check
      (equalp (get-pitch-symbols chrd) '(C4 E4 GQS4 BQF4 D5 F5)))))


;;; SAR Wed Feb 22 18:21:22 GMT 2012
(sc-deftest test-chord-equal ()
  (let ((chrd1 (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12))
        (chrd5 (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12))
        (chrd2 (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12
                           :auto-sort nil))
        (chrd3 (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12
                           :auto-sort t))
        (chrd4 (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
                           :midi-channel 7
                           :microtones-midi-channel 8
                           :auto-sort t)))
    (sc-test-check
      (chord-equal chrd1 chrd5)
      (not (chord-equal chrd1 chrd2))
      (chord-equal chrd1 chrd3)
      (chord-equal chrd1 chrd4))))

;;; SAR Wed Feb 22 18:28:58 GMT 2012
(sc-deftest test-chord-lowest ()
  (let ((chrd1 (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12))
        (chrd2 (make-chord '(e4 c4 gqs4 bqf4 d5 f5 b1)
                           :midi-channel 11
                           :microtones-midi-channel 12
                           :auto-sort nil)))
    (sc-test-check
      (equalp (data (lowest chrd1)) 'c4)
      (equalp (data (lowest chrd2)) 'b1))))

;;; SAR Wed Feb 22 18:38:04 GMT 2012
(sc-deftest test-chord-highest ()
  (let ((chrd1 (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
                           :midi-channel 11
                           :microtones-midi-channel 12))
        (chrd2 (make-chord '(e4 c4 gqs4 bqf4 f5 d5)
                           :midi-channel 11
                           :microtones-midi-channel 12
                           :auto-sort nil)))
    (sc-test-check
      (equalp (data (highest chrd1)) 'F5)
      (and 
       (equalp (data (highest chrd2)) 'f5)))))

;;; SAR Wed Feb 22 18:50:27 GMT 2012
(sc-deftest test-chord-sort-pitches ()
  (let ((chrd (make-chord '(d5 c4 gqs4 bqf5 f5 e4)
                          :midi-channel 11
                          :microtones-midi-channel 12)))
    (sc-test-check
      (equalp (loop for p in (data (sort-pitches chrd)) collect (data p))
              '(C4 E4 GQS4 D5 F5 BQF5))
      (equalp (loop for p in (data (sort-pitches chrd 'descending))
                 collect (data p))
              '(BQF5 F5 D5 GQS4 E4 C4)))))

;;; SAR Mon Apr 16 13:59:32 BST 2012
(sc-deftest test-chord-common-notes ()
  (let ((chrd1 (make-chord '(c4 e4 g4 b4 d5 f5)))
        (chrd2 (make-chord '(d3 f3 a3 c4 e4 g4)))
        (chrd3 (make-chord '(d3 f3 a3 c4 ff4 g4)))
        (chrd4 (make-chord '(d3 f3 a3 ff4 g4 c5))))
    (sc-test-check
      (= 3 (common-notes chrd1 chrd2))
      (= 3 (common-notes chrd1 chrd3))
      (= 2 (common-notes chrd1 chrd3 nil))
      (= 2 (common-notes chrd1 chrd4 t))
      (= 5 (common-notes chrd1 chrd4 t t)))))

;;; SAR Mon Apr 16 14:21:49 BST 2012
(sc-deftest test-chord-has-notes ()
  (let ((chrd1 (make-chord '(c4)))
        (chrd2 (make-chord '(c4 e4 g4)))
        (chrd3 (make-chord nil)))
    (sc-test-check
      (has-notes chrd1)
      (has-notes chrd2)
      (not (has-notes chrd3)))))

;;; SAR Mon Apr 16 14:33:10 BST 2012
(sc-deftest test-chord-pitch- ()
  (let ((chrd-1 (make-chord '(c4 e4 g4)))
        (chrd-2 (make-chord '(d4 e4 fs4 a4))))
    (sc-test-check
      (= -2.0 (pitch- chrd-1 chrd-2))
      (- 2.0 (pitch- chrd-2 chrd-1)))))

;;; SAR Mon Apr 16 14:46:56 BST 2012
(sc-deftest test-chord-transpose ()
  (let ((chrd (make-chord '(c4 e4 g4))))
    (sc-test-check
      (equalp 
       (pitch-list-to-symbols (data (transpose chrd 3 :destructively nil)))
       '(EF4 G4 BF4))
      (equalp
       (pitch-list-to-symbols (data (transpose chrd -3 :destructively nil)))
       '(A3 CS4 E4))
      (equalp
       (pitch-list-to-symbols (data (transpose chrd -3.37 :destructively nil)))
       '(AQF3 CQS4 EQF4))
      ;; MDE Tue Aug 21 20:17:28 2018 -- test the new :lowest and :highest
      ;; args  
      (equalp
       (pitch-list-to-symbols
        (data (transpose chrd 2 :destructively nil :lowest 'd4 :highest 'f4)))
       '(c4 fs4 g4))
      ;; MDE Sun Aug  6 11:01:11 2017 -- test the new :destructive
      ;; functionality  
      (equalp (get-pitch-symbols chrd) '(c4 e4 g4))
      (transpose chrd -0.5 :destructively t)
      (equalp (get-pitch-symbols chrd) '(bqs3 eqf4 gqf4))
      )))

;;; SAR Mon Apr 16 14:56:58 BST 2012
(sc-deftest test-chord-no-accidental ()
  (let ((chrd (make-chord '(cs4 e4 fs4 af4 bf4))))
    (sc-test-check
      (notany #'not (loop for p in (data chrd)
                       collect (show-accidental p)))
      (not (no-accidental chrd))
      (every #'not (loop for p in (data chrd)
                      collect (show-accidental p))))))

;;; SAR Mon Apr 16 15:09:02 BST 2012
;;;; HOW DO I TEST THIS?!:
;; => (#i(midi time 100.0 keynum 61 duration 13.0 amplitude 0.5 channel -1)
;;     #i(midi time 100.0 keynum 64 duration 13.0 amplitude 0.5 channel -1)
;;     #i(midi time 100.0 keynum 66 duration 13.0 amplitude 0.5 channel -1)
;;     #i(midi time 100.0 keynum 68 duration 13.0 amplitude 0.5 channel -1)
;;     #i(midi time 100.0 keynum 70 duration 13.0 amplitude 0.5 channel -1))


;;; SAR Mon Apr 16 16:51:22 BST 2012
(sc-deftest test-chord-add-mark ()
  (let ((chrd (make-chord '(cs4 e4 fs4 af4 bf4))))
    (sc-test-check
      (not (marks chrd))
      (add-mark chrd 'fff)
      (equalp (marks chrd) '(fff))
      (equalp
       (add-mark chrd 'pizz)
       '(pizz fff)))))

;;; SAR Mon Apr 16 16:57:50 BST 2012
(sc-deftest test-chord-delete-marks ()
  (let ((chrd (make-chord '(cs4 e4 fs4 af4 bf4))))
    (sc-test-check
      (not (marks chrd))
      (add-mark chrd 'fff)
      (add-mark chrd 'pizz)
      (equalp (marks chrd) '(pizz fff))
      (not (delete-marks chrd))
      (not (marks chrd)))))

;;; SAR Wed Jun 13 16:04:30 BST 2012
(sc-deftest test-chord-respell-chord ()
  (let ((chrd (make-chord '(a3 ds4 f4 fs5 c6))))
    (sc-test-check
      (equalp
       (pitch-list-to-symbols (data (respell-chord chrd t)))
       '(A3 EF4 F4 GF5 C6)))))

;;; SAR Thu Oct  4 16:09:42 BST 2012
(sc-deftest test-chord-add-pitches ()
  (let ((ch (make-chord '(c4 e4 g4))))
    (sc-test-check
      (equalp (get-pitch-symbols ch) '(c4 e4 g4))
      (add-pitches ch 'bf3)
      (equalp (get-pitch-symbols ch) '(bf3 c4 e4 g4))
      (add-pitches ch 'af3 'a4 'b4)
      (equalp (get-pitch-symbols ch) '(af3 bf3 c4 e4 g4 a4 b4))
      (add-pitches ch '(cs5 ds5 fs5))
      (equalp (get-pitch-symbols ch) '(af3 bf3 c4 e4 g4 a4 b4 cs5 ds5 fs5)))))

;;; SAR Thu Oct  4 16:45:35 BST 2012
(sc-deftest test-chord-rm-pitches ()
  (let ((ch (make-chord '(af3 bf3 c4 e4 g4 a4 b4 cs5 ds5 fs5))))
  (sc-test-check
    (equalp (get-pitch-symbols ch) '(AF3 BF3 C4 E4 G4 A4 B4 CS5 DS5 FS5))
    (rm-pitches ch 'bf3)
    (equalp (get-pitch-symbols ch) '(AF3 C4 E4 G4 A4 B4 CS5 DS5 FS5))
    (rm-pitches ch 'af3 'a4 'b4)
    (equalp (get-pitch-symbols ch) '(C4 E4 G4 CS5 DS5 FS5))
    ;; MDE Sat Mar  5 14:09:36 2016 -- now let's try it in the sc-set class
    (setf ch (make-sc-set '(c4 e4 g4 cs5 ds5 fs5)))
    (rm-pitches ch '(cs5 ds5 fs5))
    (equalp (get-pitch-symbols ch) '(C4 E4 G4)))))

;;; MDE Mon Aug 25 18:26:29 2014 
(sc-deftest test-micro-but-not-quarter-tone-p ()
  (in-scale :twelfth-tone)
  (sc-test-check
    (micro-but-not-quarter-tone-p (make-chord '(cs4 dts4)))
    (not (micro-but-not-quarter-tone-p (make-chord '(cs4 dqs4))))
    (not (micro-but-not-quarter-tone-p (make-chord '(cs4 ds4))))
    (in-scale :quarter-tone)))

;;; MDE Sat Oct  4 17:59:36 2014
(sc-deftest test-artificial-harmonic? ()
  (flet ((ah (notes) 
           (let ((c (make-chord notes)))
             (add-mark (get-nth 1 c) 'flag-head)
             c)))
    (sc-test-check
      (artificial-harmonic? (ah '(c4 f4 e6)))
      (artificial-harmonic? (ah '(c4 ef4 g6)))
      (artificial-harmonic? (ah '(c4 d4 bf6)))
      (artificial-harmonic? (ah '(df1 f1 f3)))
      (not (artificial-harmonic? (ah '(c4 ef4 b6))))
      (not (artificial-harmonic? (ah '(c4 ef4 fs6))))
      (not (artificial-harmonic? (ah '(c4 f4 ef6))))
      ;; MDE Thu Jan 10 15:43:11 2019
      (artificial-harmonic-simple? (ah '(c4 f4)))
      (artificial-harmonic-simple? (ah '(bf0 ef1))))))

(sc-deftest test-calculate-dissonance ()
  (let ((partials '((0.9966666 1.995 3.0 4.005834 5.0208335 6.042501 7.08
                     8.1225 9.185  10.261666 11.45 11.405833)
                    (1.0 0.48279247 0.1664997 0.066108614 0.07859015
                     0.027295936 0.035848983 0.0 0.010107763 0.0027390774
                     0.0040924386 0.0010194636))))
    (flet ((cd (notes) (calculate-dissonance (make-chord notes)))
           (cd-triad (note &optional minor)
             (calculate-dissonance
              (make-chord (list note (transpose-note note (if minor 3 4))
                                (transpose-note note 7)))
              :spectrum partials)))
      (let ((c1 (cd '(c4 e4 g4)))
            (c1a (cd '(c2 e2 g2)))
            (c1b (cd '(c3 e3 g3)))
            (d (make-chord '(d1 fs1 a1)))
            dd dc
            (c2 (cd '(c4 e4 g4 b4)))
            (c3 (cd '(c4 e4 g4 b4 cs4))))
        ;; (print c1a) (print c1b) (print c1)
        (sc-test-check
          ;; default values are nil and are only calculated once we try to
          ;; access the slots  
          (not (slot-value d 'dissonance))
          ;; do centroid here too for ease
          (not (slot-value d 'centroid))
          (floatp (setf dd (dissonance d)))
          (floatp (setf dc (centroid d)))
          (add-pitches d 'cs2)
          ;; don't change slots yet, just check they have the same previous
          ;; values 
          (= dd (dissonance d))
          (= dc (centroid d))
          (> (calculate-dissonance d) dd)
          (> (calculate-spectral-centroid d) dc)
          ;; check minor triads are more dissonant than major, with fixed
          ;; partial data at least
          (> (cd-triad 'c4 t) (cd-triad 'c4))
          (> (cd-triad 'cs4 t) (cd-triad 'cs4))
          (> (cd-triad 'd4 t) (cd-triad 'd4))
          (> (cd-triad 'cs6 t) (cd-triad 'cs6))
          (> (cd-triad 'cs2 t) (cd-triad 'cs2))
          (apply #'< (list c1 c1b c1a))
          (apply #'< (list c1 c2 c3)))))))

;;; MDE Tue Jul 28 16:23:46 2015 
(sc-deftest test-calculate-spectral-centroid ()
  (flet ((csc (notes) (calculate-spectral-centroid (make-chord notes))))
    (let* ((spectra '((0.9982489206168257d0 2.014263461548936d0
                       3.0090767036403947d0 4.025891753079929d0
                       5.0391785223504755d0 6.051163469385689d0
                       7.078870767769303d0 8.097071520583727d0 0.0
                       10.04537485275207d0 0.0 12.001576621760098d0)
                      (1.0d0 0.9110050427095994d0 0.13036526158661896d0
                       0.02384098952910138d0 0.1068663706262658d0
                       0.03364125692496988d0 0.06411071328728492d0
                       0.010625006807649204d0 0.0 0.0016994831031129103d0 0.0
                       0.0018761371417123998d0)))
           (ral (get-data 'clm-piano-spectra +slippery-chicken-spectra+))
           (default (get-sc-config 'default-spectra))
           (c1 (csc '(c4 e4 g4)))
           (c1a (csc '(c2 e2 g2)))
           (c1b (csc '(c3 e3 g3)))
           (c2 (csc '(c4 e4 g4 b4)))
           (c3 (csc '(c4 e4 g4 b4 d5)))
           (c4 (csc '(fs3 c4 cs4 d4 ds4 e4)))
           (c5 (csc '(fs3 c4 cs4 d4 ds4 e4 g6)))
           (c6 (csc '(c1 e2 g3))))
      ;; (print c1a) (print c6)
      (sc-test-check
        (> c5 c4)
        (< c1a c6)
        (apply #'> (list c1 c1b c1a))
        (apply #'< (list c1 c2 c3))
        (set-sc-config 'default-spectra spectra)
        (> (csc '(c4 e4 g4)) (csc '(c3 e3 g3)))
        (set-sc-config 'default-spectra ral)
        (> (csc '(c4 e4 g4)) (csc '(c3 e3 g3)))
        (> (csc '(c2 e2 g2)) (csc '(c1 e2 g3)))
        ;; reset to what it was when we started
        (set-sc-config 'default-spectra default)))))

;;; MDE Wed Jul 11 12:40:39 2018 
#+clm
(sc-deftest test-create-analysis-data ()
  (let ((sf (concatenate 'string cl-user::+slippery-chicken-home-dir+ 
                         "tests/test-sndfiles-dir-2/test-sndfile-4.aiff"))
        (df "/tmp/data.txt"))
    (sc-test-check
      (clm::create-analysis-data sf :outputfile df)
      (file-write-ok df 400000))))

;;; MDE Wed Aug 12 17:36:51 2015 
(sc-deftest test-chord-similarity ()
  (flet ((sim (c1 c2 &optional e o)
           (similarity (make-chord c1) (make-chord c2) e o)))
    (let (s1 s2 s3)
      (sc-test-check
        (= 1.0 (sim '(c4 e4 g4) '(c4 e4 g4)))
        ;; allow enharmonics
        (= 1.0 (sim '(df4 f4 af4) '(cs4 es4 gs4) t))
        ;; allow enharmonics but not octave displacements
        (/= 1.0 (setq s1 (sim '(df4 f4 af4) '(cs4 es4 gs3) t)))
        ;; allow enharmonics and octave displacements to get a higher score
        ;; than with no octave displacements
        (/= 1.0 (setq s2 (sim '(df4 f4 af4) '(cs4 es4 gs3) t t)))
        ;; (print s1) (print s2)
        (> s2 s1)
        ;; don't allow enharmonics
        (/= 1.0 (sim '(df4 f4 af4) '(cs4 es4 gs4) nil))
        (= 1.0 (sim '(c4 e4 g4 b4 d5) '(c4 e4 g4 b4 d5)))
        (= 1.0 (sim '(c4 e4 g4 b4 d5) '(c4 e4 g4 b4 d5)))
        (> (sim '(c4 e4 g4) '(c4 e4 g4 b4))
           (sim '(c4 e4 g4) '(df4 f4 af4)))
        (> (sim '(c4 e4 g4) '(c4 e4 g4 b4))
           (sim '(c4 e4 g4) '(df4 f4 g4)))
        ;; there are no commond notes and no common intervals here
        (zerop (sim '(c1 cs1 f1 b1) '(d1 e1 fs1 gs1 as1)))
        ;; there is one commond note (gs1) but no common intervals here: this
        ;; counts for less than one common interval (of which there are one
        ;; less than number of notes) 
        (not (zerop (setq s1 (sim '(c1 cs1 f1 gs1 b1) '(d1 e1 fs1 gs1 as1)))))
        ;; this has a major 2nd in common
        (not (zerop (setq s2 (sim '(c1 cs1 f1 g1 b1) '(d1 e1 fs1 gs1 as1)))))
        ;; this has a minor and major 2nd in common
        (not (zerop (setq s3 (sim '(c1 cs1 f1 g1 b1) '(d1 ds1 fs1 gs1 as1)))))
        (> s3 s2 s1)
        ))))

;;; MDE Sat Mar  5 12:32:39 2016
(sc-deftest test-chord-wrap ()
  (sc-test-check
    (equalp (get-pitch-symbols (wrap (make-chord '(c4 e4 g4 b4))))
            '(C4 E4 GS4 B4))
    (equalp (get-pitch-symbols (wrap (make-chord '(c4 fs4 b4 f5))))
            '(C4 GF4 C5 F5))
    (equalp (get-pitch-symbols (wrap (make-chord '(df3 c4 fs4 b4 f5)) 2))
            '(DF3 GF3 C4 B4 F5))
    (equalp (get-pitch-symbols (wrap (make-chord '(d4 f4 bf4 e5 b5)) 1 nil))
            '(G3 D4 F4 BF4 E5))
    (equalp (get-pitch-symbols (wrap (make-sc-set '(df3 c4 fs4 b4 f5))))
            '(DF3 G3 FS4 C5 F5))))

;;; MDE Tue May  3 19:09:24 2016
(sc-deftest test-chord-morph ()
  (flet ((test-it (c1 c2 amount expected)
           (equalp expected
                   (get-pitch-symbols
                    ;; (morph (make-chord c1) (make-chord c2) amount)))))
                    (morph (make-complete-set c1)
                           (make-complete-set c2) amount)))))
  (sc-test-check
    (test-it '(c4 e4 g4 b4) '(df4 f4 af4 c5) 0.9 '(DF4 F4 af4 C5))
    (test-it '(c4 e4 g4 b4) '(df4 f4 af4 c5) 0.7 '(DF4 F4 G4 C5))
    (test-it '(c4 e4 g4 b4) '(df4 f4 af4 c5) 0.1 '(c4 e4 G4 c5))
    (test-it '(c4 e4 g4 b4) '(df4 f4 af4 c5) 0 '(c4 e4 G4 b4))
    (test-it '(c4 e4 g4 b4) '(df4 f4 af4 c5) 1 '(df4 f4 af4 c5))
    (test-it '(c4 e4 g4 b4) '(df4 e4 af4 c5) 1 '(df4 e4 af4 c5))
    (test-it '(c4 e4) '(df4 f4 af4 c5) 0 '(c4 e4))
    (test-it '(c4 e4) '(df4 f4 af4 c5) 1 '(df4 f4 af4 c5))
    (test-it '(df4 f4 af4 c5) '(c4 e4) 0 '(df4 f4 af4 c5))
    (test-it '(c4 e4) '(df4 f4 af4 c5) 0.5 '(c4 f4 c5))
    (test-it '(c4 e4) '(df4 f4 af4 c5) 0.3 '(c4 e4 c5))
    (test-it '(c4 e4) '(c5 df4 f4 af4) 0.3 '(c4 e4 c5)))))

;;; MDE Thu Feb  1 15:51:24 2018
(sc-deftest test-chord-collapse ()
  (flet ((test-it (orig collapsed 8ve)
           (equalp collapsed
            (get-pitch-symbols
             (collapse (make-chord orig) 8ve)))))
    (sc-test-check
      (test-it '(c1 d2 e3 f4 g5 a6 b7) '(c4 d4 e4 f4 g4 a4 b4) 4)
      (test-it '(cs1 df2 ef3 fs4 gs5 af6 bs7) '(cs1 df1 ef1 fs1 gs1 af1 bs1)
               1))))

;;; MDE Thu Apr 29 19:27:02 2021, Heidhausen
(sc-deftest test-chord-diatonic-p ()
  (sc-test-check
    (diatonic-p (make-chord '(c4 e g))) ; M
    (diatonic-p (make-chord '(e4 g c5))) ; M 1st inv
    (diatonic-p (make-chord '(g4 c5 e))) ; M 2nd inv
    (not (diatonic-p (make-chord '(c4 e c5))))
    (not (diatonic-p (make-chord '(c4 e g b)))) ; M7
    (diatonic-p (make-chord '(c4 e g b)) t) ; M7
    (diatonic-p (make-chord '(c4 e g bf7)) t) ; dom 7 with high 7th
    (diatonic-p (make-chord '(e4 g bf4 c5)) t) ; dom 7 close, 1st inv
    (diatonic-p (make-chord '(g bf4 c5 e)) t) ; dom 7 2nd inv
    (diatonic-p (make-chord '(bf4 c5 e g)) t) ; dom 7 3rd inv
    (diatonic-p (make-chord '(df1 f gs3))) ; half-dim
    (diatonic-p (make-chord '(c4 ef g b)) t) ; min M7
    (diatonic-p (make-chord '(c4 ef g bf)) t) ; min m7
    (diatonic-p (make-chord '(c4 ef gf bf)) t) ; half dim m7
    (diatonic-p (make-chord '(ef4 gf bf c5)) t) ; half dim m7, inverted
    (not (diatonic-p (make-chord '(c4 ef gf b)) t))
    (diatonic-p (make-chord '(ds4 fs a c1))) ; dim 7
    (diatonic-p (make-chord '(e4 gs c5))) ; aug
    (not (diatonic-p (make-chord '(e4 gs c5 ef)))) ; aug M7
    (diatonic-p (make-chord '(e4 gs c5 ef)) t) ; aug M7
    (diatonic-p (make-chord '(gs c5 ds e)) t) ; aug M7, inverted
    (not (diatonic-p (make-chord '(fs4 as cs d))))
  ))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sndfile tests

;;; MDE Mon Sep  7 11:16:43 2015
(defun get-test-sf-path (sf)
  (concatenate 'string
               cl-user::+slippery-chicken-home-dir+ 
               sf))

;;; SAR Mon Apr 16 17:52:23 BST 2012
#+clm
(sc-deftest test-sndfile-make-sndfile ()
  (let ((sf-1 (make-sndfile 
               (get-test-sf-path "tests/sndfile-1.aiff")
               :start 0.3 :end 1.1 :frequency 653))
        (sf-2 (make-sndfile 
               ;; MDE Wed Dec 26 14:34:53 2012 -- need class name first now
               (list 'sndfile
                     (get-test-sf-path "tests/sndfile-1.aiff")
                     (list nil :start 0.7 :end 1.3 :frequency 'c4))))) 
    (sc-test-check
      (equalp (data sf-1)
              (get-test-sf-path "tests/sndfile-1.aiff"))
      (= 0.3 (start sf-1))
      (= 1.1 (end sf-1))
      (= 653 (frequency sf-1))
      (= 0.7 (start sf-2))
      (= 1.3 (end sf-2))
      (equal-within-tolerance 261.63 (frequency sf-2) 0.01))))

;;; SAR Mon Apr 16 18:35:37 BST 2012
#+clm
(sc-deftest test-sndfile-stereo ()
  (let ((sf-1 (make-sndfile 
               (get-test-sf-path "tests/sndfile-1.aiff"))))
    (sc-test-check
      (not (stereo sf-1))
      (setf (channels sf-1) 8)
      (not (stereo sf-1))
      (setf (channels sf-1) 2)
      (stereo sf-1))))

;;; SAR Mon Apr 16 18:47:49 BST 2012
#+clm
(sc-deftest test-sndfile-reset-usage ()
  (let ((sf-1 (make-sndfile 
               (get-test-sf-path "tests/sndfile-1.aiff"))))
    (sc-test-check
      (= 0 (will-be-used sf-1))
      (= 0 (has-been-used sf-1))
      (setf (will-be-used sf-1) 11)
      (setf (has-been-used sf-1) 13)
      (= 11 (will-be-used sf-1))
      (= 13 (has-been-used sf-1))
      (reset-usage sf-1)
      (= 0 (will-be-used sf-1))
      (= 0 (has-been-used sf-1)))))

;;; MDE Mon Sep  7 11:15:33 2015 -- new autocorrelation routines as default for
;;; freq's: todo: we need to load CLM's autoc, perhaps during the loading of sc
;;; in general
#+clm
(sc-deftest test-sndfile-autoc ()
  ;; MDE Mon Feb 25 20:00:13 2019 -- autoc is as of today part of SC
  ;; (load (compile-file "/Users/michael/ins/autoc.ins"))
  (flet ((msf (dir sf &optional (start 0))
           (make-sndfile 
            (get-test-sf-path
             (format nil "tests/test-sndfiles-dir-~a/test-sndfile-~a.aiff"
                     dir sf))
            :frequency 'detect :start start))
         (check-it (freq sf)
           (= freq (print (round (frequency sf))))))
    (let ((sf1 (msf 1 1 .03))
          (sf2 (msf 1 2))
          (sf3 (msf 2 4 .8))
          (sf4 (msf 1 3 .9))
           ;; MDE Mon May 30 19:15:11 2022, Heidhausen -- we've now changed the
           ;; auotocorrelation routine to sample 200ms. check this works with
           ;; sines
          (matt1 (msf 1 "matt-sines" .1))
          (matt2 (msf 1 "matt-sines" 7.2))
          (matt3 (msf 1 "matt-sines" 12.5))
          (matt4 (msf 1 "matt-sines" 15.7)))
      (sc-test-check
        ;; the lowest partial shown in glisseq is 3x this so the percussive
        ;; nature is confusing the algo. still at least there's a relationship
        (check-it 178 sf1)
        ;; this glissandos so really tough: glisseq really unclear
        (check-it 125 sf2)
        ;; very clear pitch
        (check-it 865 sf3)
        (check-it 637 sf4)
        ;; these are actually harmonics of 200 Hz but we're close enough
        (check-it 201 matt1)
        (check-it 401 matt2)
        (check-it 604 matt3)
        (check-it 802 matt4)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ensemble tests

;;; SAR Wed Apr 18 15:58:46 BST 2012
(sc-deftest test-ensemble-make-ensemble ()
  (let ((ens (make-ensemble 
              'ens
              ;; MDE Wed May 28 17:02:46 2014 -- test that we can specify staff
              ;; names here too  
              '((flt ((flute piccolo) :midi-channel 1
                      :staff-names ("The Flute" "The piccolo")
                      ;; this should still work even though it's not a list and
                      ;; we only specify the one short name
                      :staff-short-names "Flt"))
                (clr ((b-flat-clarinet))))
              :instrument-palette
              +slippery-chicken-standard-instrument-palette+)))
    (sc-test-check
      (string= "The Flute" (staff-name (first (data (get-data-data 'flt
                                                                   ens)))))
      (string= "Flt" (staff-short-name (first (data (get-data-data 'flt ens)))))
      (string= "The piccolo"
               (staff-name (second (data (get-data-data 'flt ens)))))
      ;; MDE Sat Nov  3 09:46:01 2018 -- test cloning of existing objects
      (eq 'blah (id (make-ensemble 'blah ens)))
      (eq 'ens (id (make-ensemble nil ens)))
      (equalp (loop for p in (data ens) 
                 collect (id p)
                 when (assoc-list-p (data p))
                 collect (loop for i in (data (data p))
                            collect (id i))
                 unless (assoc-list-p (data p))
                 collect (id (data p)))
              '(FLT (FLUTE PICCOLO) CLR B-FLAT-CLARINET)))))

;;; SAR Wed Apr 18 16:06:05 BST 2012
(sc-deftest test-ensemble-get-players ()
  (let ((ens (make-ensemble 
              'ens
              '((flt ((flute piccolo) :midi-channel 1))
                (clr ((b-flat-clarinet)))
                (tpt ((b-flat-trumpet c-trumpet) :midi-channel 2))
                (vln ((violin))))
              :instrument-palette
              +slippery-chicken-standard-instrument-palette+)))
    (sc-test-check
      (equalp
       (get-players ens)
       '(FLT CLR TPT VLN)))))

;;; SAR Wed Apr 18 16:45:37 BST 2012
(sc-deftest test-ensemble-num-players ()
  (let ((ens (make-ensemble 
              'ens
              '((flt ((flute piccolo) :midi-channel 1))
                (clr ((b-flat-clarinet)))
                (tpt ((b-flat-trumpet c-trumpet) :midi-channel 2))
                (vln ((violin))))
              :instrument-palette
              +slippery-chicken-standard-instrument-palette+)))
    (sc-test-check
      (= 4 (num-players ens)))))

;;; SAR Wed Apr 18 19:24:50 BST 2012
;;; These use the IGNORE-ERRORS macro to suppress error output and prevent
;;; dropping into the debugger while still checking that an error is
;;; generated. The second test is a BIT of a hack, since all it does is test
;;; that ignore-errors returns a values-list with more than one member, of
;;; which members the second (zero-index 1) is not NIL, meaning that it is most
;;; likely the error message that would be printed. But it doesn't actually
;;; test what that item is, other than that it is not NIL.
(sc-deftest test-ensemble-players-exist ()
  (let ((ens (make-ensemble 
              'ens
              '((flt ((flute piccolo) :midi-channel 1))
                (clr ((b-flat-clarinet)))
                (tpt ((b-flat-trumpet c-trumpet) :midi-channel 2))
                (vln ((violin))))
              :instrument-palette
              +slippery-chicken-standard-instrument-palette+)))
    (sc-test-check
      (and 
       (players-exist ens '(vln))
       (not (nth-value 1 (ignore-errors (players-exist ens '(vln))))))
      (and 
       (not (ignore-errors (players-exist ens '(vla))))
       (nth-value 1 (ignore-errors (players-exist ens '(vla))))))))

;;; MDE Sat Aug 25 16:23:10 2018 -- in the chromatic scale, midi-chann and
;;; microtones-midi-chann will/should be the same:
(sc-deftest test-ensemble-auto-midi-channels ()
  (in-scale :chromatic)
  (let ((ens (make-ensemble 
              'ens
              '((flt ((flute piccolo))); :midi-channel 1))
                (clr ((b-flat-clarinet)))
                (tpt ((b-flat-trumpet c-trumpet))); :midi-channel 1))
                (vln ((violin))))
              :instrument-palette
              +slippery-chicken-standard-instrument-palette+)))
    (sc-test-check
      (= 4 (auto-midi-channels ens t))
      (= 1 (midi-channel (get-player ens 'flt)))
      (= 3 (midi-channel (get-player ens 'tpt)))
      (= 4 (microtones-midi-channel (get-player ens 'vln))))))

(sc-deftest test-ensemble-auto-midi-channels2 ()
  (in-scale :quarter-tone)
  (let ((ens (make-ensemble 
              'ens
              '((flt ((flute piccolo))); :midi-channel 1))
                (clr ((b-flat-clarinet)))
                (tpt ((b-flat-trumpet c-trumpet))); :midi-channel 1))
                (vln ((violin))))
              :instrument-palette
              +slippery-chicken-standard-instrument-palette+)))
    (sc-test-check
      (= 8 (auto-midi-channels ens t))
      (= 1 (midi-channel (get-player ens 'flt)))
      (= 5 (midi-channel (get-player ens 'tpt)))
      (= 8 (microtones-midi-channel (get-player ens 'vln))))))


;;; SAR Wed May  2 12:32:50 BST 2012
(sc-deftest test-ensemble-num-notes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) e e e e))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (= 40 (num-notes (ensemble mini))))))

;;; SAR Wed May  2 13:02:53 BST 2012
;;; two ensemble/tessitura tests:
(sc-deftest test-ensemble-tessitura-ch ()
  (progn
    (in-scale :chromatic)
    (let ((mini-ch
           (make-slippery-chicken
            '+mini-ch+
            :ensemble '(((vn (violin :midi-channel 1))
                         (vc (cello :midi-channel 2))))
            :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4))))
            :set-map '((1 (1 1 1 1 1)))
            :rthm-seq-palette '((1 ((((2 4) e e e e))
                                    :pitch-seq-palette ((1 2 3 4)))))
            :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                                (vc (1 1 1 1 1))))))))
      (sc-test-check
        (equalp (tessitura (ensemble mini-ch)) 'c4)))))

(sc-deftest test-ensemble-tessitura-qt ()
  (progn
    (in-scale :quarter-tone)
    (let ((mini-qt
           (make-slippery-chicken
            '+mini-qt+
            :ensemble '(((vn (violin :midi-channel 1))
                         (vc (cello :midi-channel 2))))
            :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4))))
            :set-map '((1 (1 1 1 1 1)))
            :rthm-seq-palette '((1 ((((2 4) e e e e))
                                    :pitch-seq-palette ((1 2 3 4)))))
            :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                                (vc (1 1 1 1 1))))))))
      (sc-test-check
        (equalp (tessitura (ensemble mini-qt)) 'cqs4)))))

;;; MDE Sat Apr 20 12:38:34 2013
(sc-deftest test-ensemble-add-player ()
  (let ((e (make-ensemble 'e '((flt ((flute piccolo) :midi-channel 1))
                               (clr ((b-flat-clarinet)))))))
    (sc-test-check
      (add-player e 'bsn :instrument 'bassoon)
      (equalp (players e) '(flt clr bsn))
      (= 3 (num-players e))
      (player-p (get-player e 'bsn)))))

;;; MDE Thu Nov  1 11:09:38 2018 
(sc-deftest test-ensemble-combos ()
  (declare (special +mini-qt+))
  (let ((ens (make-ensemble 
              'ens
              '((flt ((flute piccolo))) ; :midi-channel 1))
                (clr ((b-flat-clarinet)))
                (bsn bassoon)
                (tpt ((b-flat-trumpet c-trumpet))) ; :midi-channel 1))
                (trb tenor-trombone)
                (tb tuba)
                (vln ((violin))))))
        oc)
    (flet ((try (chord combo &optional (harms t) event? (chords t))
             (combo-chord-possible?
              (if event? (make-event chord 'q) (make-chord chord))
              (loop for ins in combo collect (get-standard-ins ins))
              harms chords)))
      (sc-test-check
        ;; MDE Thu Nov  8 18:11:24 2018 -- for the orchestrate SC method:
        ;; although shuffle is used make sure we get the same results when
        ;; recalled  
        (equalp (lotsa-combos ens) (lotsa-combos ens))
        (> (length (lotsa-combos ens 11)) (length (lotsa-combos ens 4)))
        (instrument-p (get-instrument ens 'clr))
        (instrument-p (get-instrument ens 'flt 'flute))
        (zerop (nth-value 1 (try '(c4 e4 g4) '(violin violin cello))))
        (zerop (nth-value 1 (try '(c4 e4 g4) '(violin viola cello))))
        ;; try with player ids and thus passing an sc
        (equalp '(1 0)
                (mapcar #'first
                        (combo-chord-possible? (make-event '(cs3 d6) 'q
                                                           :bar-num 3)
                                               '(vn vc) nil +mini-qt+)))
        (every #'(lambda (l) (pitch-p (second l)))
               (combo-chord-possible?
                (make-chord '(bf3 cs4 bqs4))
                (loop for i in '(marimba bass-trombone french-horn) collect
                     (get-standard-ins i))))
        ;; doesn't work completely (but the violins can do it) unless...
        (= 1 (nth-value 1 (try '(c5 e5 g5) '(double-bass violin violin) nil)))
        ;; ... we allow artificial harms
        (zerop (nth-value 1 (try '(c5 e5 g5) '(double-bass violin violin))))
        ;; event class
        (zerop (nth-value 1 (try '(c5 e5 g5) '(double-bass violin violin) t t)))
        ;; can only get e5 and g5
        (= 2 (nth-value 1 (try '(c5 e5 g5) '(crotales piccolo glockenspiel) t)))
        (= 2 (nth-value 1 (try '(c5 e5 g5) '(crotales piccolo glockenspiel)
                               nil)))
        ;; violin and viola can get all the notes but not piccolo
        (= 1 (nth-value 1 (try '(c5 e4 g4) '(piccolo violin viola))))
        (zerop (nth-value 1 (try '(c5 e5 g5) '(piccolo violin viola))))
        (zerop (nth-value 1 (try 'ds3 '(viola) t t)))
        (zerop (nth-value 1 (try '(c8 d8) '(violin viola))))
        (not (try '(c8 d8) '(violin viola) nil)) ; only works with harms
        ;; top note of violin is c7 so will work with harms
        (= 2 (nth-value 1 (try '(b1 g3 cs7) '(cello viola violin) nil)))
        (= 1 (nth-value 1 (try '(b1 g3 cs7) '(cello viola violin))))
        (equalp '(1 0 2) (mapcar #'first ; bassoon on the low b, then vc, vln
                                 (try '(b1 g3 cs7) '(cello bassoon violin))))
        (equalp '(0 1 3 2) (mapcar #'first 
                                   (try '(b1 g3 cs5 cs7)
                                        '(piano bassoon piccolo violin))))
        (equalp '(0 1 2 3) (mapcar #'first 
                                   (try '(b1 g3 d5 cs7)
                                        '(piano bassoon piccolo violin))))
        (assoc-list-p
         (setf oc
               (organise-combos ens '((flt) (flt clr) (flt clr bsn) (clr)
                                      (tpt trb tb) (flt clr tb vln)
                                      (bsn tpt vln tb)
                                      (bsn tpt vln tb flt)
                                      (bsn tpt vln tb clr trb)
                                      (flt clr bsn tpt tb trb vln)))))
        (equalp '(1 2 3 4 5 6 7) (get-keys oc))
        (equalp '((flt clr)) (data (get-data-data 2 oc)))
        (equalp '(bsn tpt vln tb) (second (data (get-data-data 4 oc))))
        (equalp '(bsn tpt vln tb flt) (first (data (get-data-data 5 oc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slippery-chicken-edit tests

;;; MDE Sat Nov  3 10:23:42 2018
(sc-deftest test-sc-orchestrate ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :title "mini"
           :ensemble '(((pno1 piano) (pno2 piano)))
           :tempo-map '((1 (q 60)))
           :set-palette '((1 ((c2 a2 d3 b3 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                          (2 ((cs2 gs2 ds3 b3 fs4 gs4 as4 cs5 ds5 fs5 gs5)))) 
           :set-map '((1 (1 2 1 1 2 1 2 1)))
           :rthm-seq-palette
           '((1 ((((2 4) q +e. s)
                  ((s) e (s) q)
                  (+e. s { 3 (te) te te } ))
                 :pitch-seq-palette (((1) (2) (3) (4) (5) (6) (7)))))
             (2 ((((3 4) (e.) s { 3 te te te } +q)
                  ({ 3 +te (te) te } e e (q)))
                 :pitch-seq-palette (((2) (3) 4 (5) (6) (7) 8))))
             (3 ((((2 4) e e { 3 te te te })
                  ((5 8) (e) e e e s s))
                 :pitch-seq-palette 
                 (((1) 4 (3) (6) 9 8 (9) (12) (2) 1)))))
           :rthm-seq-map '((1 ((pno1 (1 2 2 3 2 3 3 1))
                               (pno2 (1 2 2 3 2 3 3 1)))))))
         (clone (clone mini))
         event)
    (sc-test-check
      (is-chord (get-event mini 1 1 'pno1))
      (is-chord (get-event mini 2 2 'pno2))
      (setq event (find-end-tie mini (get-event mini 4 5 'pno1)))
      (= 5 (bar-num event))
      (= 0 (bar-pos event))
      (chord-p (try-ins-chord (get-standard-ins 'piano)
                              (make-chord '(c4 e4 g4)) 
                              (make-pitch 'c4)))
      (pitch-p (try-ins-chord (get-standard-ins 'piano)
                              (make-chord '(c4 e4 g4))
                              (make-pitch 'cs4)))
      (chord-equal (make-chord '(d5 g5))
                   (try-ins-chord (get-standard-ins 'PIANO)
                                  (make-chord '(d5 g5 gqs5))
                                  (make-pitch 'd5)))
      (orchestrate mini
                   '((v1 violin) (v2 violin) (db double-bass) (fl flute)
                     (vla viola) (ob oboe) (vc cello))
                   'pno2)
      (orchestrate clone
                   '((v1 violin) (v2 violin) (db double-bass) (fl flute)
                     (vla viola) (ob oboe) (vc cello)
                     (bsn bassoon) (org organ) (trb tenor-trombone))
                   '(pno1)
                   :combos '((v1) (db) (fl) (v1 bsn) (trb db) (v2 fl trb)
                             (bsn org db fl) (vla ob trb) (trb vc)
                             (v1 v2 fl) (v1 org db fl) (v1 v2 org fl))
                   :start-bar 1 :end-bar 9)
      (orchestrate clone
                   ;; test
                   '((v1 violin) (v2 violin) (hn french-horn) (tp c-trumpet)
                     (tb tuba))
                   'pno2 :start-bar 10 :end-bar 18))))

;;; SAR Thu Apr 19 11:29:54 BST 2012
(sc-deftest test-sc-edit-add-arrow-to-events ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :title "mini"
          :ensemble '(((pno (piano :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                         (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5)))) 
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q q))
                                  :pitch-seq-palette ((1 (2))))))
          :rthm-seq-map '((1 ((pno (1 1 1 1 1 1))))))))
    (sc-test-check
      (add-arrow-to-events mini "here" "there" '(1 1) '(5 1) 'pno)
      (equalp (marks (get-event mini 1 1 'pno))
              '(start-arrow))
      (equalp (marks-before (get-event mini 1 1 'pno))
              '((ARROW "here" "there")))
      (equalp (marks (get-event mini 5 1 'pno))
              '(end-arrow)))))

;;; SAR Thu Apr 19 11:39:16 BST 2012
(sc-deftest test-sc-edit-add-clef ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :title "mini"
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                         (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5)))) 
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
    (sc-test-check
      (not (marks-before (get-event mini 3 2 'vn)))
      (add-clef mini 'vn 3 2 'alto)
      (equalp (marks-before (get-event mini 3 2 'vn))
              '((CLEF ALTO))))))

;;; SAR Thu Apr 19 12:39:54 BST 2012
(sc-deftest test-sc-edit-add-event-to-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                         (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5 cs6)))) 
          :set-map '((1 (1 1 1 1 1 1))
                     (2 (2 2 2 2 2 2)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))))
                              (2 ((((2 4) e s s q))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))
                          (2 ((vn (2 2 2 2 2 2))))))))
    (sc-test-check
      (equalp (loop for r in (rhythms (first (get-bar mini 2)))
                 collect (get-pitch-symbol r)
                 collect (data r))
              '(C4 Q D4 E F4 S G4 S))
      (add-event-to-bar mini (make-event 'cs4 'e) 2 'vn)
      (equalp (loop for r in (rhythms (first (get-bar mini 2)))
                 collect (get-pitch-symbol r)
                 collect (data r))
              '(C4 Q D4 E F4 S G4 S CS4 E))
      (add-event-to-bar mini (make-event 'c4 'h) '(2 2 1) 'vn :position 2) 
      (equalp (loop for r in (rhythms (first (get-bar mini '(2 2 1))))
                 collect (get-pitch-symbol r)
                 collect (data r))
              '(CS4 E DS4 S C4 H FS4 S GS4 Q)))))

;;; SAR Thu Apr 19 13:05:58 BST 2012
(sc-deftest test-sc-edit-add-mark-all-players ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))) 
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))) 
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (1 1 1 1 1))
                              (hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (every #'not (loop for i in '(cl hn vc)
                      collect (marks (get-event mini 3 1 i))))
      (every #'not (loop for i in '(cl hn vc)
                      for e in '(1 2 3)
                      collect (marks (get-event mini '(2 2 1) e i)))) 
      (add-mark-all-players mini 3 1 'ppp)
      (add-mark-all-players mini '(2 2 1) '(1 2 3) 'fff)
      (every #'(lambda (x) (equalp x '(ppp)))
             (loop for i in '(cl hn vc)
                collect (marks (get-event mini 3 1 i))))
      (every #'(lambda (x) (equalp x '(fff)))
             (loop for i in '(cl hn vc)
                for e in '(1 2 3)
                collect (marks (get-event mini '(2 2 1) e i)))))))

;;; SAR Thu Apr 19 13:15:53 BST 2012
(sc-deftest test-sc-edit-add-mark-before-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
    (sc-test-check
      (not (marks-before (get-event mini 3 3 'vn)))
      (add-mark-before-note mini 3 2 'vn 'ppp)
      (equalp (marks-before (get-event mini 3 3 'vn)) '(ppp)))))

;;; SAR Thu Apr 19 13:32:27 BST 2012
(sc-deftest test-sc-edit-add-mark-to-event ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
    (sc-test-check
      (add-mark-to-event mini 3 2 'vn 'ppp)
      (equalp (marks (get-event mini 3 2 'vn))
              '(ppp)))))

;;; SAR Thu Apr 19 13:41:20 BST 2012
(sc-deftest test-sc-edit-add-mark-to-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                  :pitch-seq-palette ((1 2 3))))
                              ;; MDE Fri Jul 12 18:16:25 2019 -- pedals now work
                              ;; in :marks not just via add-mark post-gen 
                              (2 ((((2 4) q e s s))
                                  :marks (uc 1 ped 2 ped-up 3 tc 4))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 2))))))))
    (flet ((amtn (bar note mark)
             (add-mark-to-note mini bar note 'vn mark))
           (cc= (bar note cc)
             (equalp cc (midi-control-changes (get-note mini bar note 'vn)))))
      (sc-test-check
        (amtn 3 2 'ppp)
        (equalp (marks (get-event mini 3 3 'vn))
                '(ppp))
        ;; MDE Wed May 25 14:12:35 2016 -- test pedal marks too--never mind that
        ;; this is a violin 
        (amtn 1 1 'ped)
        (amtn 1 3 'ped-up)
        (amtn 2 1 'ped)
        (amtn 2 2 'ped^)
        (amtn 2 3 'ped-up)
        (amtn 3 1 'sost)
        (amtn 3 3 'sost-up)
        (amtn 4 1 'uc)
        (amtn 4 3 'tc)
        ;; adding pedal marks adds midi cc also
        (cc= 1 1 '((1 64 127)))
        (cc= 1 3 '((1 64 0)))
        ;; MDE Mon Sep 30 15:15:48 2019 -- changed order of pedals
        (cc= 2 2 '((1 64 127) (1 64 0)))
        (cc= 3 1 '((1 66 127)))
        (cc= 3 3 '((1 66 0)))
        (cc= 4 1 '((1 67 127)))
        (cc= 4 3 '((1 67 0)))
        ;; MDE Fri Jul 12 18:17:00 2019 -- sim. for those added via :marks
        (cc= 6 2 '((1 64 127)))
        (cc= 6 3 '((1 64 0)))
        (cc= 6 1 '((1 67 127)))
        (cc= 6 4 '((1 67 0)))        
        ))))

;;; SAR Thu Apr 19 14:09:46 BST 2012
(sc-deftest test-sc-edit-add-marks-sh ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                              (va (1 1 1 1 1 1))))))))
    (sc-test-check
      (every #'not
             (loop for b in '(1 1 3 2 2 2)
                for n in '(1 2 1 1 2 5)
                collect (marks (get-event mini b n 'vn))))
      (every #'not
             (loop for b in '(1 2 3)
                for n in '(3 3 1)
                collect (marks (get-event mini b n 'va))))
      (not (add-marks-sh mini
                         '((vn a 1 1 1 2 3 1 s 2 1 2 2 2 5)
                           (va pizz 1 3 2 3 sp 3 1))
                         :shorthand '((sp short-pause))))
      (equalp (loop for b in '(1 1 3 2 2 2)
                 for n in '(1 2 1 1 2 5)
                 collect (marks (get-event mini b n 'vn)))
              '((A) (A) (A) (S) (S) (S)))
      (equalp (loop for b in '(1 2 3)
                 for n in '(3 3 1)
                 collect (marks (get-event mini b n 'va)))
              '((PIZZ) (PIZZ) (SHORT-PAUSE))))))

;;; SAR Thu Apr 19 16:24:26 BST 2012
(sc-deftest test-sc-edit-add-marks-to-notes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                              (va (1 1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 2))
      (every #'not (loop for ne = (next-event mini 'vn nil nil 3)
                      while ne
                      collect (marks ne)))
      (not (next-event mini 'vn nil 1))
      (every #'not (loop for ne = (next-event mini 'vn nil nil 2)
                      while ne
                      collect (marks ne)))
      (not (next-event mini 'va nil 1))
      (every #'not (loop for ne = (next-event mini 'va nil nil 2)
                      while ne
                      collect (marks ne)))
      (add-marks-to-notes mini 2 3 'vn nil 'lhp)
      (not (next-event mini 'vn nil 2))
      (every #'(lambda (x) (equalp x '(lhp))) 
             (loop for ne = (next-event mini 'vn nil nil 3)
                while ne
                collect (marks ne)))
      (add-marks-to-notes mini '(1 3) '(2 2) '(vn va) nil 's 'a)
      (not (next-event mini 'vn nil 1))
      (equalp (loop for ne = (next-event mini 'vn nil nil 2)
                 while ne
                 collect (marks ne))
              '(NIL NIL (A S) (A S) (A S) (A S) (A S) (A S) (A S LHP) (A S LHP) 
                (LHP) (LHP) (LHP) (LHP) (LHP) (LHP)))
      (not (next-event mini 'va nil 1))
      (equalp (loop for ne = (next-event mini 'va nil nil 2)
                 while ne
                 collect (marks ne))
              '(NIL NIL (A S) (A S) (A S) (A S) (A S) (A S) (A S) (A S) NIL NIL 
                NIL NIL NIL NIL)))))

;;; SAR Thu Apr 19 16:50:23 BST 2012
(sc-deftest test-sc-edit-add-marks-to-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e (e) e e (e) e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6))))) 
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                              (va (1 1 1 1 1 1))))))))
    (sc-test-check
      (add-marks-to-note mini 2 3 'va 'a 's 'lhp 'pizz)
      (equalp (marks (get-note mini 2 3 'va))
              '(PIZZ LHP S A)))))

;;; SAR Fri Apr 20 09:28:07 BST 2012
(sc-deftest test-sc-edit-auto-beam ()
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((fs4 gs4 as4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (sc-test-check
    (not (next-event mini 'vn nil 1))
    (every #'not
           (loop for ne = (next-event mini 'vn)
              while ne
              collect (beam ne)))
    (not (auto-beam mini 'h))
    (not (next-event mini 'vn nil 1))
    (equalp (loop for ne = (next-event mini 'vn)
               while ne
               collect (beam ne))
            '(1 NIL NIL 0 1 NIL NIL 0 1 NIL NIL 0 1 NIL NIL 0 1 NIL NIL 0 1 NIL 
              NIL 0 1 NIL NIL 0 1 NIL NIL 0)))))

;;; SAR Fri Apr 20 10:35:49 BST 2012
(sc-deftest test-sc-edit-auto-clefs ()
  (set-sc-config 'best-clef-aux-fun #'best-clef-aux)
  (progn
    (setf (starting-clef
           (get-data 'cello +slippery-chicken-standard-instrument-palette+))
          'bass)
    (let ((mini-1
           (make-slippery-chicken
            '+mini-1+
            :ensemble '(((vc (cello :midi-channel 1))))
            :tempo-map '((1 (q 60)))
            :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
            :set-map '((1 (1 1 1 1)))
            :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                    :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
            :rthm-seq-map '((1 ((vc (1 1 1 1)))))))
          (mini-2
           (make-slippery-chicken
            '+mini-2+
            :ensemble '(((hn (french-horn :midi-channel 1))))
            :tempo-map '((1 (q 60)))
            :set-palette '((1 ((c3 d4 b3 c4 d4 a4 f5 g5))))
            :set-map '((1 (1 1 1 1)))
            :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                    :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))  
            :rthm-seq-map '((1 ((hn (1 1 1 1))))))))
      (sc-test-check
        (add-mark-before-note mini-1 1 1 'vc 'ppp)
        (add-mark-to-note mini-1 1 1 'vc 'fff)
        (not (next-event mini-1 'vc nil 1))
        (equalp (loop for ne = (next-event mini-1 'vc)
                   while ne
                   collect (marks-before ne))
                '((PPP) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
                  NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
                  NIL NIL)) 
        ;; MDE Thu Jun 14 11:08:26 2012 -- changed auto-clefs to return T so
        ;; edited all the tests to not be (not (auto-... 
        (auto-clefs mini-1)
        (not (next-event mini-1 'vc nil 1))
        (equalp (loop for ne = (next-event mini-1 'vc)
                   while ne
                   collect (marks-before ne))
                '((PPP) NIL ((CLEF TENOR)) NIL NIL ((CLEF TREBLE)) NIL NIL 
                  ((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL 
                  ((CLEF BASS)) NIL NIL NIL NIL 
                  ((CLEF TREBLE)) NIL NIL ((CLEF BASS)) NIL NIL NIL NIL 
                  ((CLEF TREBLE)) NIL NIL))
        (add-clef mini-1 'vc 1 2 'alto)
        (equalp (marks-before (get-event mini-1 1 2 'vc))
                '((clef alto)))
        (auto-clefs mini-1 :delete-clefs nil)
        (not (next-event mini-1 'vc nil 1))
        (equalp (loop for ne = (next-event mini-1 'vc)
                   while ne
                   collect (marks-before ne))
                '((PPP) ((CLEF ALTO)) ((CLEF TENOR)) NIL NIL ((CLEF TREBLE)) 
                  NIL NIL ((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL
                  ((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL 
                  ((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL)) 
        (auto-clefs mini-1 :delete-marks-before t)
        (not (next-event mini-1 'vc nil 1))
        (equalp (loop for ne = (next-event mini-1 'vc)
                   while ne
                   collect (marks-before ne))
                '(NIL NIL ((CLEF TENOR)) NIL NIL ((CLEF TREBLE)) NIL NIL  
                  ((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL 
                  ((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL 
                  ((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL NIL)) 
        (auto-clefs mini-2 :in-c t)
        (not (next-event mini-2 'hn nil 1))
        ;; MDE Thu Jun 14 11:35:30 2012 -- hmm, this is weird: this should be a
        ;; deterministic algorithm but if we call this test again this next one
        ;; fails as we get (((CLEF BASS)) NIL NIL NIL NIL ((CLEF TREBLE)) NIL
        ;; NIL ((CLEF BASS)) NIL etc. In both CCL and SBCL also.
        #|
        (equalp
         (loop for ne = (next-event mini-2 'hn)
            while ne
            collect (marks-before ne))
         '(((CLEF BASS)) NIL NIL NIL ((CLEF TREBLE)) NIL NIL NIL ((CLEF BASS))
           NIL NIL NIL ((CLEF TREBLE)) NIL NIL NIL ((CLEF BASS)) NIL NIL NIL 
           ((CLEF TREBLE)) NIL NIL NIL ((CLEF BASS)) NIL NIL NIL ((CLEF TREBLE))
           NIL NIL NIL)) |#
        (auto-clefs mini-2 :in-c nil)
        (not (next-event mini-2 'hn nil 1))
        (every #'not (loop for ne = (next-event mini-2 'hn)
                        while ne
                        collect (marks-before ne)))))))


;;; SAR Fri Apr 20 11:09:28 BST 2012
(sc-deftest test-sc-edit-change-pitch ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map '((1 ((vc (1 1 1 1)))))))
        e)
    (sc-test-check
      (equalp (get-pitch-symbol (get-note mini 1 2 'vc)) 'E3)
      (change-pitch mini 1 2 'vc 'fs3)
      (equalp (get-pitch-symbol (get-note mini 1 2 'vc)) 'FS3)
      ;; SW bug again (er ist der Meister!): is something was going on with
      ;; frequency here that makes changing low notes inexact
      (in-scale :twelfth-tone)
      (event-p (setq e (make-event 'ctf1 'q :transposition 12 :written t)))
      (setf (pitch-or-chord e) 'c1)
      (zerop (pitch-bend (written-pitch-or-chord e)))
      (zerop (pitch-bend (written-pitch-or-chord e)))
      (eq 'c0 (data (written-pitch-or-chord e)))
      (in-scale :quarter-tone))))

;;; SAR Fri Apr 20 11:28:43 BST 2012
(sc-deftest test-sc-edit-change-time-sig ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
    (sc-test-check
      (equalp (data (get-time-sig (get-bar mini 2 'vc))) '(4 4))
      (equalp (data (get-time-sig (get-bar mini 1 'vc))) '(4 4))
      (change-time-sig mini 2 '(3 8))
      (equalp (data (get-time-sig (get-bar mini 2 'vc))) '(3 8))
      (change-time-sig mini '(1 1 1) '(5 8))
      (equalp (data (get-time-sig (get-bar mini 1 'vc))) '(5 8)))))

;;; SAR Fri Apr 20 11:39:38 BST 2012
(sc-deftest test-sc-edit-delete-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) q e s s h))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (3 ((((4 4) e s s h q))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (4 ((((4 4) s s h q e))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (5 ((((4 4) s h q e s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((vc (1 2 3 4 5))))))))
    (sc-test-check
      (= 5 (num-bars mini))
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(D3 H E3 Q F3 E G3 S A3 S D3 Q E3 E F3 S 
                G3 S A3 H D3 E E3 S F3 S G3 H A3
                Q D3 S E3 S F3 H G3 Q A3 E D3 S E3 H F3 Q G3 E A3 S))
      (delete-bars mini 2 :end-bar 3)
      (= 3 (num-bars mini))
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(D3 H E3 Q F3 E G3 S A3 S D3 S E3 S F3 H 
                G3 Q A3 E D3 S E3 H F3 Q G3 E A3 S))
      (delete-bars mini 2 :num-bars 1)
      (= 2 (num-bars mini))
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(D3 H E3 Q F3 E G3 S A3 S D3 S E3 H F3 Q G3 E A3 S)))))

;;; SAR Fri Apr 20 11:59:39 BST 2012
(sc-deftest test-sc-edit-delete-clefs ()
  (progn 
    (setf (starting-clef
           (get-data 'cello +slippery-chicken-standard-instrument-palette+))
          'bass)
    (let ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((vc (cello :midi-channel 1))))
            :tempo-map '((1 (q 60)))
            :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
            :set-map '((1 (1 1 1 1)))
            :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                    :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
            :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
      (sc-test-check
        (not (marks-before (get-event mini 1 3 'vc)))
        (auto-clefs mini)
        (equalp (marks-before (get-event mini 1 3 'vc)) '((CLEF TENOR)))
        (not (delete-clefs mini 'vc 1 3))
        (not (marks-before (get-event mini 1 3 'vc)))))))

;;; SAR Fri Apr 20 13:23:32 BST 2012
(sc-deftest test-sc-edit-delete-events ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
          :rthm-seq-map '((1 ((vc (1 1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vc)
                while ne
                collect (is-rest ne)))
      (delete-events mini 2 2 3 3 'vc)
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (is-rest ne))
              '(NIL NIL NIL NIL NIL NIL NIL NIL NIL T T T T T T 
                NIL NIL NIL NIL NIL NIL
                NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
                NIL NIL NIL NIL NIL NIL
                NIL NIL NIL NIL NIL))
      (delete-events mini 4 3 5 nil 'vc t)
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (is-rest ne))
              '(NIL NIL NIL NIL NIL NIL NIL NIL NIL T T T T T T 
                NIL NIL NIL NIL NIL NIL
                NIL T T T T NIL NIL NIL NIL NIL NIL NIL NIL))    
      (delete-events mini 6 1 6 nil 'vc nil)
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (is-rest ne))
              '(NIL NIL NIL NIL NIL NIL NIL NIL NIL T T T T T T
                NIL NIL NIL NIL NIL NIL
                NIL T T T T T T T T T T T T)))))

;;; SAR Fri Apr 20 13:40:54 BST 2012
(sc-deftest test-sc-edit-delete-rehearsal-letter ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
          :rthm-seq-map '((1 ((vc (1 1 1 1 1 1)))))
          :rehearsal-letters '(2 4 6))))
    (sc-test-check
      (equalp (rehearsal-letter (get-bar mini 1 'vc)) "A")
      (equal "A" (delete-rehearsal-letter mini 2 '(vc)))
      (not (rehearsal-letter (get-bar mini 1 'vc))))))

;;; SAR Fri Apr 20 13:49:15 BST 2012
(sc-deftest test-sc-edit-delete-slur ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (slur 1 8))))
          :rthm-seq-map '((1 ((vc (1 1 1 1 1 1))))))))
    (sc-test-check
      (equalp (marks (get-event mini 1 1 'vc)) '(beg-sl))
      (equalp (marks (get-event mini 1 8 'vc)) '(end-sl))
      (equalp (marks (get-event mini 3 1 'vc)) '(beg-sl))
      (equalp (marks (get-event mini 3 8 'vc)) '(end-sl))
      (not (delete-slur mini 1 1 'vc))
      (not (delete-slur mini 3 1 'vc))
      (not (marks (get-event mini 1 1 'vc)))
      (not (marks (get-event mini 1 8 'vc)))
      (not (marks (get-event mini 3 1 'vc)))
      (not (marks (get-event mini 3 8 'vc))))))

;;; SAR Fri Apr 20 14:21:13 BST 2012
(sc-deftest test-sc-edit-double-events ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((bsn (bassoon :midi-channel 1))
                       (tbn (tenor-trombone :midi-channel 2))
                       (vlc (cello :midi-channel 3))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) (w)))))
                              (2 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
          :rthm-seq-map '((1 ((bsn (1 1 1 1 1 1))
                              (tbn (1 1 1 1 1 1))
                              (vlc (2 2 2 2 2 2))))))))
    ;; (cmn-display mini)
    (sc-test-check
      (not (next-event mini 'bsn nil 1))
      (equalp (loop for ne = (next-event mini 'bsn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 1 NIL 1 NIL 1 NIL 1 NIL 1))
      (not (next-event mini 'tbn nil 1))
      (equalp (loop for ne = (next-event mini 'tbn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 1 NIL 1 NIL 1 NIL 1 NIL 1))
      (double-events mini 'vlc '(bsn tbn) 2 3 4 2)
      ;; MDE Fri Nov  9 08:19:05 2018
      (= 4 (bar-num (get-bar mini 4 'tbn)))
      (eq 'tbn (player (get-event mini 2 1 'tbn)))
      (not (next-event mini 'bsn nil 1))
      (equalp (loop for ne = (next-event mini 'bsn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 4 D4 E E4 E F4 E G4 E A4 E F5 E C2 E 
                E2 E D4 E E4 E F4 E G4 E
                A4 E F5 E C2 E E2 E NIL 4 NIL 4 NIL 4 NIL 1 NIL 1)) 
      (not (next-event mini 'tbn nil 1))
      (equalp (loop for ne = (next-event mini 'tbn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 4 D4 E E4 E F4 E G4 E A4 E F5 E C2 E 
                E2 E D4 E E4 E F4 E G4 E
                A4 E F5 E C2 E E2 E NIL 4 NIL 4 NIL 4 NIL 1 NIL 1))
      (double-events mini 'vlc 'bsn 5 1 5 nil :transposition 3.5)
      (not (next-event mini 'bsn nil 1))
      (equalp (loop for ne = (next-event mini 'bsn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 4 D4 E E4 E F4 E G4 E A4 E F5 E C2 E 
                E2 E D4 E E4 E F4 E G4 E
                A4 E F5 E C2 E E2 E NIL 4 NIL 4 NIL 4 EQF2 E 
                GQS2 E FQS4 E GQS4 E AQF4 E
                BQF4 E CQS5 E AQF5 E NIL 1)))))


;;; SAR Tue Apr 24 19:22:16 BST 2012
(sc-deftest test-sc-edit-enharmonics ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (pn (piano :midi-channel 2))
                       (vn (violin :midi-channel 3))))
          :set-palette '((1 ((cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                  :pitch-seq-palette ((1 (2) 3 4 (5) 6 (7)
                                                         8))))) 
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (pn (1 1 1 1 1))
                              (vn (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(GS4 (GS4 AS4) AS4 B4 (AS4 B4) B4 (B4 CS5) CS5 GS4 (GS4 AS4) AS4 B4
         (AS4 B4) B4 (B4 CS5) CS5 GS4 (GS4 AS4) AS4 B4 (AS4 B4) B4 (B4 CS5) CS5   
         GS4 (GS4 AS4) AS4 B4 (AS4 B4) B4 (B4 CS5) CS5 GS4 (GS4 AS4) AS4 B4
         (AS4 B4) B4 (B4 CS5) CS5))
      (not (next-event mini 'pn nil 1))
      (equalp
       (loop for ne = (next-event mini 'pn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4
         CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4
         CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4
         CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4
         CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4))
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl)
          while ne
          collect (get-pitch-symbol ne))
       '(EF4 F4 FS4 AF4 BF4 C5 CS5 EF5 F4 F4 FS4 AF4 BF4 C5 CS5 EF5 F4 F4 FS4
         AF4 BF4 C5 CS5 EF5 F4 F4 FS4 AF4 BF4 C5 CS5 EF5 F4 F4 FS4 AF4 BF4 C5
         CS5 EF5)) 
      (enharmonics mini 1 2 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(AF4 (AF4 BF4) BF4 B4 (BF4 B4) B4 (B4 DF5) DF5 AF4 (AF4 BF4) BF4 B4
         (BF4 B4) B4 (B4 DF5) DF5 GS4 (GS4 AS4) AS4 B4 (AS4 B4) B4 (B4 CS5) CS5
         GS4 (GS4 AS4) AS4 B4 (AS4 B4) B4 (B4 CS5) CS5 GS4 (GS4 AS4) AS4 B4
         (AS4 B4) B4 (B4 CS5) CS5))
      (enharmonics mini 2 3 'pn :pitches '(cs4 ds4))
      (not (next-event mini 'pn nil 1))
      (equal
       (loop for ne = (next-event mini 'pn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4
         DF4 (DF4 EF4 E4 FS4) EF4 E4 (DF4 EF4 E4 FS4) E4 (DF4 EF4 E4 FS4) FS4
         DF4 (DF4 EF4 E4 FS4) EF4 E4 (DF4 EF4 E4 FS4) E4 (DF4 EF4 E4 FS4) FS4
         CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4
         CS4 (CS4 DS4 E4 FS4) DS4 E4 (CS4 DS4 E4 FS4) E4 (CS4 DS4 E4 FS4) FS4))
      (enharmonics mini 3 4 'cl :written nil)
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl)
          while ne
          collect (get-pitch-symbol ne))
       '(EF4 F4 FS4 AF4 BF4 C5 CS5 EF5 F4 F4 FS4 AF4 BF4 C5 CS5 EF5 F4 F4 FS4
         AF4 BF4 C5 CS5 EF5 F4 F4 FS4 AF4 BF4 C5 CS5 EF5 F4 F4 FS4 AF4 BF4 C5
         CS5 EF5))
      ;; MDE Tue May 28 10:57:29 2013 -- check our new :force-naturals works
      ;; (ticket #417) 
      (change-pitches mini 'vn 1 '((bs4)))
      (equalp 'bs4 (data (pitch-or-chord (get-event mini 1 1 'vn))))
      (enharmonics mini 1 1 'vn)
      (equalp 'c5 (data (pitch-or-chord (get-event mini 1 1 'vn))))
      ;; call again to see no change
      (enharmonics mini 1 1 'vn)
      (equalp 'c5 (data (pitch-or-chord (get-event mini 1 1 'vn))))
      ;; now call with force-naturals to go back to bs4
      (enharmonics mini 1 1 'vn :force-naturals t)
      (equalp 'bs4 (data (pitch-or-chord (get-event mini 1 1 'vn)))))))

;;; SAR Fri Apr 20 16:54:45 BST 2012
(sc-deftest test-sc-edit-force-rest-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 e4 g4 b4 d5 f5 a5 c6))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))  
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                              (va (1 1 1 1 1 1))
                              (vc (1 1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (is-rest ne)))
      (not (next-event mini 'vc nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vc)
                while ne
                collect (is-rest ne)))
      ;; MDE Mon Oct 29 12:35:46 2018 -- used to return NIL, now returns
      ;; number of bars processed.
      (= 6 (force-rest-bars mini 3 5 '(vn vc)))
      (not (next-event mini 'vn nil 1))
      (equalp (loop for ne = (next-event mini 'vn)
                 while ne
                 collect (is-rest ne))
              '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
                NIL NIL NIL NIL T T T NIL
                NIL NIL NIL NIL NIL NIL NIL))
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (is-rest ne))
              '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
                NIL NIL NIL NIL T T T NIL
                NIL NIL NIL NIL NIL NIL NIL)))))

(sc-deftest test-sc-edit-move-clef ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map '((1 ((vc (1 1 1 1)))))))
        (vc (get-standard-ins 'cello))
        (acc (get-standard-ins 'accordion))
        (ob (get-standard-ins 'oboe)))
    (sc-test-check
      (set-sc-config 'best-clef-aux-fun #'best-clef-aux)
      (auto-clefs mini)
      (equalp (marks-before (get-event mini 1 6 'vc)) '((clef treble)))
      (move-clef mini 1 6 1 8 'vc)
      (not (marks-before (get-event mini 1 6 'vc)))
      ;; MDE Tue Dec 29 12:40:52 2020, Heidhausen -- test both methods
      (set-sc-config 'best-clef-aux-fun #'best-clef-aux-new)
      ;; MDE Tue Dec 29 16:48:12 2020, Heidhausen -- while we're here, test
      ;; best-clef a bit
      (equalp '(treble bass)
              (best-clef vc (make-chord '(e4 fs4)) nil 'bass nil))
      (equalp '(treble bass)
              (best-clef vc (make-chord '(e4 fs4)) nil 'treble nil))
      (equalp '(tenor bass)
              (best-clef vc (make-chord '(e4 fs4)) nil 'tenor nil))
      (equalp '(tenor bass)
              (best-clef vc (make-chord '(c2 e5)) nil 'tenor nil))
      (equalp '(treble nil)
              ;; this doesn't get to the -aux routine as oboe has only one clef
              ;; (if it did, it should trigger an error because bass is not one
              ;; of its clefs
              (best-clef ob (make-chord '(e4 fs4)) nil 'bass nil))
      (equalp '(treble nil)
              (best-clef ob (make-pitch 'c7) nil 'bass nil))
      (equalp '(treble nil)
              (best-clef ob (make-pitch 'c2) nil 'bass nil))
      (equalp '(treble bass)
              (best-clef acc (make-chord '(e4 fs4)) nil 'treble nil))
      (equalp '(treble bass)
              (best-clef acc (make-chord '(e4 fs4)) nil 'bass nil))
      (equalp '(double-bass bass)
              (best-clef acc (make-chord '(e1 fs1)) nil 'treble nil))
      (equalp '(bass nil)
              (best-clef acc (make-chord '(e2 fs2)) nil 'treble nil))
      (auto-clefs mini)
      ;; MDE Mon Dec 28 18:04:15 2020, Heidhausen -- changes to best-clef-aux
      (equalp (marks-before (get-event mini 1 3 'vc)) '((clef treble)))
      (not (marks-before (get-event mini 1 8 'vc)))
      (move-clef mini 1 3 1 8 'vc)
      (not (marks-before (get-event mini 1 3 'vc)))
      (equalp (marks-before (get-event mini 1 8 'vc)) '((clef treble))))))

;;; SAR Fri Apr 20 17:43:19 BST 2012
(sc-deftest test-sc-edit-move-events ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((bn (bassoon :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))
                              (2 ((((4 4) (w))))))
          :rthm-seq-map '((1 ((bn (1 1 1 1))
                              (vc (2 2 2 2))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 1 NIL 1 NIL 1))
      (not (next-event mini 'bn nil 1))
      (equalp (loop for ne = (next-event mini 'bn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(C2 E E2 E D4 E E4 E F4 E F4 E G4 E A4 E C2 E E2 E 
                D4 E E4 E F4 E F4 E G4
                E A4 E C2 E E2 E D4 E E4 E F4 E F4 E G4 E A4 E 
                C2 E E2 E D4 E E4 E F4 E
                F4 E G4 E A4 E))
      (move-events mini 'bn 'vc 2 3 3 2)
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 4 D4 E E4 E F4 E F4 E G4 E A4 E C2 E 
                E2 E NIL 4 NIL 4 NIL 4 NIL
                1))
      (not (next-event mini 'bn nil 1))
      (equalp (loop for ne = (next-event mini 'bn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(C2 E E2 E D4 E E4 E F4 E F4 E G4 E A4 E C2 E 
                E2 E NIL 4 NIL 4 NIL 4 NIL 4
                D4 E E4 E F4 E F4 E G4 E A4 E C2 E E2 E D4 E 
                E4 E F4 E F4 E G4 E A4 E))
      (move-events mini 'bn 'vc 4 1 4 2 :transposition 4.5)
      (not (next-event mini 'vc nil 1))
      (equalp (loop for ne = (next-event mini 'vc)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(NIL 1 NIL 4 D4 E E4 E F4 E F4 E G4 E A4 E C2 E 
                E2 E NIL 4 NIL 4 NIL 4
                EQS2 E AQF2 E NIL 4 NIL 4 NIL 4))
      (not (next-event mini 'bn nil 1))
      (equalp (loop for ne = (next-event mini 'bn)
                 while ne
                 collect (get-pitch-symbol ne)
                 collect (data ne))
              '(C2 E E2 E D4 E E4 E F4 E F4 E G4 E A4 E C2 E 
                E2 E NIL 4 NIL 4 NIL 4 NIL 4
                D4 E E4 E F4 E F4 E G4 E A4 E NIL 4 D4 E E4 E 
                F4 E F4 E G4 E A4 E)))))

;;; SAR Fri Apr 20 18:30:29 BST 2012
(sc-deftest test-sc-edit-note-add-bracket-offset ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((f3 g3 a3 b3))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((2 4) { 3 te te te } q ))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vc (1))))))))
    (sc-test-check
      (note-add-bracket-offset mini 1 1 'vc 
                               :dx -.1 :dy -.3 
                               :dx0 -.1 :dy0 -.4 
                               :dx1 .3 :dy1 -.1)
      (equalp (nth 0 (bracket (get-event mini 1 1 'vc)))
              '(1 3 -0.1 -0.3 -0.1 -0.4 0.3 -0.1)))))

;;; SAR Sat Apr 21 14:07:59 BST 2012
;;; This routine does not test the :verbose, :update-slots, or :check-ties
;;; arguments!
(sc-deftest test-sc-edit-re-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
    (sc-test-check
      (= 7 (num-bars mini))
      (every #'(lambda (x) (equalp x '(2 4)))
             (loop for b from 1 to (num-bars mini)
                collect (data (get-time-sig (get-bar mini b 'vn)))))
      (every #'(lambda (x) (every #'not x))
             (loop for b from 1 to (num-bars mini)
                collect 
                  (loop for r in (rhythms (get-bar mini b 'vn))
                     collect (beam r))))
      (re-bar mini :start-bar 2 :end-bar 5 :min-time-sig '(4 4) :auto-beam 4) 
      (= 5 (num-bars mini))
      (equalp
       (loop for b from 1 to (num-bars mini)
          collect (data (get-time-sig (get-bar mini b 'vn))))
       '((2 4) (4 4) (4 4) (2 4) (2 4)))
      (equalp
       (loop for b from 1 to (num-bars mini)
          collect 
            (loop for r in (rhythms (get-bar mini b 'vn))
               collect (beam r)))
       '((NIL NIL NIL NIL) (NIL 1 NIL 0 NIL 1 NIL 0) (NIL 1 NIL 0 NIL 1 NIL 0) 
         (NIL NIL NIL NIL) (NIL NIL NIL NIL))))))

;;; SAR Sat Apr 21 14:23:16 BST 2012
(sc-deftest test-sc-edit-remove-extraneous-dynamics ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))
                                  :marks (f 1 f 2 f 3 f 4))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (every #'(lambda (x) (equalp x '(f))) 
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (marks ne)))
      (remove-extraneous-dynamics mini)
      (equalp (marks (get-event mini 1 1 'vn)) '(f))
      (not (next-event mini 'vn nil 2))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (marks ne))))))

;;; SAR Sat Apr 21 16:27:42 BST 2012
(sc-deftest test-sc-edit-replace-events ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
    (sc-test-check
    (not (next-event mini 'vn nil 1))
    (equalp
     (loop for ne = (next-event mini 'vn)
        while ne
        collect (get-pitch-symbol ne)
        collect (data ne))
     '(D4 Q NIL E E4 S F4 S D4 Q NIL E E4 S F4 S D4 Q NIL E E4 S F4 S D4 Q NIL E
       E4 S F4 S))
    (every #'(lambda (x) (every #'not x))
           (loop for b from 1 to (num-bars mini)
              collect 
                (loop for r in (rhythms (get-bar mini b 'vn))
                   collect (beam r))))
    (not (next-event mini 'vn nil 1))
    (every #'not
           (loop for ne = (next-event mini 'vn)
              while ne
              collect (bracket ne)))
    (replace-events mini 'vn 1 2 1 '((nil s) ((ds5 fs5) s)) t)
    (not (next-event mini 'vn nil 1))
    (equalp
     (loop for ne = (next-event mini 'vn)
        while ne
        collect (get-pitch-symbol ne)
        collect (data ne))
     '(D4 Q NIL S (DS5 FS5) S E4 S F4 S D4 Q NIL E E4 S F4 S D4 Q NIL E 
       E4 S F4 S D4 Q NIL E E4 S F4 S))
    (equalp
     (loop for r in (rhythms (get-bar mini 1 'vn))
        collect (beam r))
     '(NIL NIL 1 NIL 0))
    (every #'(lambda (x) (every #'not x)) 
           (loop for b from 2 to (num-bars mini)
              collect 
                (loop for r in (rhythms (get-bar mini b 'vn))
                   collect (beam r))))
    (replace-events mini 'vn 2 2 1 '((cs5 e)))
    (not (next-event mini 'vn nil 1))
    (equalp
     (loop for ne = (next-event mini 'vn)
        while ne
        collect (get-pitch-symbol ne)
        collect (data ne))
     '(D4 Q NIL S (DS5 FS5) S E4 S F4 S D4 Q CS5 E E4 S F4 S D4 Q NIL 
       E E4 S F4 S D4 Q NIL E E4 S F4 S))
    (replace-events mini 'vn '(1 3 1) 3 1 '((df4 s)))
    (not (next-event mini 'vn nil 1))
    (equalp
     (loop for ne = (next-event mini 'vn)
        while ne
        collect (get-pitch-symbol ne)
        collect (data ne))
     '(D4 Q NIL S (DS5 FS5) S E4 S F4 S D4 Q CS5 E E4 S F4 S D4 Q NIL 
       E DF4 S F4 S D4 Q NIL E E4 S F4 S))
    (replace-events mini 'vn 4 1 1 '((ds4 te) (r te) (b3 te)) t '(3 0 2))
    (not (next-event mini 'vn nil 1))
    (equalp
     (loop for ne = (next-event mini 'vn)
        while ne
        collect (get-pitch-symbol ne)
        collect (data ne))
     '(D4 Q NIL S (DS5 FS5) S E4 S F4 S D4 Q CS5 E E4 S F4 S D4 Q NIL 
       E DF4 S F4 S DS4 TE NIL TE B3 TE NIL E E4 S F4 S))
    (equalp
     (loop for r in (rhythms (get-bar mini 1 'vn))
        collect (beam r))
     '(NIL NIL 1 NIL 0))
    (cmn-display mini)
    (equalp
     (loop for r in (rhythms (get-bar mini 4 'vn))
        collect (beam r))
     '(1 NIL 0 NIL 1 0))
    (every #'(lambda (x) (every #'not x)) 
           (loop for b from 2 to 3
              collect 
                (loop for r in (rhythms (get-bar mini b 'vn))
                   collect (beam r))))
    (not (next-event mini 'vn nil 1))
    (equalp
     (loop for ne = (next-event mini 'vn)
        while ne
        collect (bracket ne))
     '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL ((1 3)) (-1) (1) 
       NIL NIL NIL)))))

;;; SAR Sun Apr 22 09:17:36 BST 2012
(sc-deftest test-sc-edit-replace-tempo-map ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((pno (piano :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
          :set-map '((1 (1 1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q q))
                                  :pitch-seq-palette ((1 (2))))))
          :rthm-seq-map '((1 ((pno (1 1 1 1 1 1 1 1))))))))
    (sc-test-check
      (every #'(lambda (x) (equalp x 'q))
             (loop for b from 1 to (num-bars mini)
                collect (beat (get-tempo mini b))))
      (every #'(lambda (x) (= 60 x))
             (loop for b from 1 to (num-bars mini)
                collect (bpm (get-tempo mini b))))
      (replace-tempo-map mini '((1 (q 60 "Andante")) ((1 3 1) (e 80))))
      (every #'(lambda (x) (equalp x 'q))
             (loop for b from 1 to 2
                collect (beat (get-tempo mini b))))
      (every #'(lambda (x) (equalp x 'e))
             (loop for b from 3 to (num-bars mini)
                collect (beat (get-tempo mini b))))
      (every #'(lambda (x) (= 60 x))
             (loop for b from 1 to 2
                collect (bpm (get-tempo mini b))))
      (every #'(lambda (x) (= 80 x))
             (loop for b from 3 to (num-bars mini)
                collect (bpm (get-tempo mini b)))))))

;;; SAR Sun Apr 22 09:49:18 BST 2012
(sc-deftest test-sc-edit-respell-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs4 ds4 df5 ef5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4
         DS4 DF5 EF5))
      (respell-bars mini)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(DF4 EF4 DF5 EF5 DF4 EF4 DF5 EF5 DF4 EF4 DF5 EF5 DF4 EF4 DF5 EF5 DF4 
         EF4 DF5 EF5)))))

;;; SAR Sun Apr 22 10:31:06 BST 2012
(sc-deftest test-sc-edit-respell-notes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs4 ds4 df5 ef5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4
         DS4 DF5 EF5))
      (respell-notes mini)
      (not (next-event mini 'vn nil 1))
      (equalp 
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4
         DS4 CS5 DS5))
      (respell-notes mini '((vn (1 1) (1 4))))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(DF4 DS4 CS5 EF5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4
         DS4 CS5 DS5)))))

;;; SAR Sun Apr 22 11:12:33 BST 2012
(sc-deftest test-sc-edit-respell-notes-for-player ()
  (let ((mini-1
         (make-slippery-chicken
          '+mini-1+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vn (violin :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((b3 cs4 b4 cs5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vn (1 1 1 1 1)))))))
        (mini-2
         (make-slippery-chicken
          '+mini-2+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vn (violin :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs4 ds4 df5 ef5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vn (1 1 1 1 1))))))))
    (sc-test-check
      ;; mini-1 original
      ;; vn
      (not (next-event mini-1 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini-1 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(B3 B3 CS4 CS4 B3 B3 CS4 CS4 B3 B3 CS4 CS4 B3 B3 CS4 CS4 B3 B3 CS4
         CS4)) 
      ;; cl
      (not (next-event mini-1 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini-1 'cl)
          while ne
          collect (get-pitch-symbol ne nil))
       '(B3 CS4 B4 CS5 B3 CS4 B4 CS5 B3 CS4 B4 CS5 B3 CS4 B4 CS5 B3 CS4 B4
         CS5)) 
      (not (next-event mini-1 'cl nil 1))
      (equalp 
       (loop for ne = (next-event mini-1 'cl)
          while ne
          collect (get-pitch-symbol ne t))
       '(CS4 EF4 CS5 EF5 CS4 EF4 CS5 EF5 CS4 EF4 CS5 EF5 CS4 EF4 CS5 EF5 CS4
         EF4 CS5 EF5))
      
      ;; mini-2 original
      ;; vn
      (not (next-event mini-2 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini-2 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 CS4 DS4 DS4 CS4 CS4 DS4 DS4 CS4 CS4 DS4 DS4 CS4 CS4 DS4 DS4 CS4
         CS4 DS4 DS4))
      ;; cl
      (not (next-event mini-2 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini-2 'cl)
          while ne
          collect (get-pitch-symbol ne nil))
       '(CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4 DS4 DF5 EF5 CS4
         DS4 DF5 EF5))
      (not (next-event mini-2 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini-2 'cl)
          while ne
          collect (get-pitch-symbol ne t))
       '(EF4 F4 EF5 F5 EF4 F4 EF5 F5 EF4 F4 EF5 F5 EF4 F4 EF5 F5 EF4 F4 EF5
         F5)) 

    ;;; mini-1 respell written
      (not (respell-notes-for-player mini-1 'cl t))
      (not (next-event mini-1 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini-1 'cl)
          while ne
          collect (get-pitch-symbol ne nil))
       '(B3 CS4 B4 CS5 B3 CS4 B4 CS5 B3 CS4 B4 CS5 B3 CS4 B4 CS5 B3 CS4 B4
         CS5)) 
      (not (next-event mini-1 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini-1 'cl)
          while ne
          collect (get-pitch-symbol ne t))
       '(CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4
         DS4 CS5 DS5))

    ;;; mini-2 respell sounding
      (not (respell-notes-for-player mini-2 'cl nil))
      (not (next-event mini-2 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini-2 'cl)
          while ne
          collect (get-pitch-symbol ne nil))
       '(CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4 DS4 CS5 DS5 CS4
         DS4 CS5 DS5))
      (not (next-event mini-2 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini-2 'cl)
          while ne
          collect (get-pitch-symbol ne t))
       '(EF4 F4 EF5 F5 EF4 F4 EF5 F5 EF4 F4 EF5 F5 EF4 F4 EF5 F5 EF4 F4 EF5
         F5)))))

;;; SAR Sun Apr 22 12:11:24 BST 2012
(sc-deftest test-sc-edit-rest-to-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs4 ds4 fs4))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 NIL DS4 FS4 CS4 NIL DS4 FS4 CS4 NIL DS4 FS4 CS4 NIL DS4 FS4 CS4
         NIL DS4 FS4))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (marks ne)))
      (rest-to-note mini 2 1 'vn 'gs5)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 NIL DS4 FS4 CS4 GS5 DS4 FS4 CS4 NIL DS4 FS4 CS4 NIL DS4 FS4 CS4
         NIL DS4 FS4))
      (rest-to-note mini 3 1 'vn '(gs5 b5))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 NIL DS4 FS4 CS4 GS5 DS4 FS4 CS4 (GS5 B5) DS4 FS4 CS4 NIL DS4 FS4
         CS4 NIL DS4 FS4))
      (rest-to-note mini 4 1 'vn '(gs4 b4) 'ppp)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(CS4 NIL DS4 FS4 CS4 GS5 DS4 FS4 CS4 (GS5 B5) DS4 FS4 CS4 (GS4 B4) DS4
         FS4 CS4 NIL DS4 FS4))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (PPP) NIL NIL NIL
         NIL NIL NIL))
      (rest-to-note mini 5 1 'vn '(gs4 b4) '(fff pizz))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (PPP) NIL NIL NIL
         (PIZZ FFF) NIL NIL)))))

;;; SAR Sun Apr 22 12:24:42 BST 2012
(sc-deftest test-sc-edit-rm-marks-from-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs4 ds4 fs4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))
                                  :marks (a 2 s 2 fff 2 pizz 2))))
          :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL (PIZZ FFF S A) NIL NIL NIL (PIZZ FFF S A) NIL NIL NIL 
         (PIZZ FFF S A)
         NIL NIL NIL (PIZZ FFF S A) NIL NIL))
      (rm-marks-from-note mini 2 2 'vn 'pizz)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL (PIZZ FFF S A) NIL NIL NIL (FFF S A) NIL NIL NIL 
         (PIZZ FFF S A) NIL
         NIL NIL (PIZZ FFF S A) NIL NIL))
      (rm-marks-from-note mini 3 2 'vn '(pizz fff))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL (PIZZ FFF S A) NIL NIL NIL (FFF S A) NIL NIL NIL 
         (S A) NIL NIL NIL
         (PIZZ FFF S A) NIL NIL))
      (rm-marks-from-note mini 3 2 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL (PIZZ FFF S A) NIL NIL NIL (FFF S A) NIL NIL NIL 
         (S A) NIL NIL NIL
         (PIZZ FFF S A) NIL NIL)))))

;;; SAR Sun Apr 22 15:02:24 BST 2012
(sc-deftest test-sc-edit-rm-marks-from-notes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((fl (flute :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vn (violin :midi-channel 3))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs4 ds4 fs4))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))
                                  :marks (a 2 s 2 fff 2))))
          :rthm-seq-map '((1 ((fl (1 1 1 1 1))
                              (hn (1 1 1 1 1))
                              (vn (1 1 1 1 1)))))))
        flm hnm vnm)
    (sc-test-check
      (not (next-event mini 'fl nil 1))
      (setf flm
            (loop for ne = (next-event mini 'fl)
               while ne
               collect (marks ne)))
      (not (next-event mini 'hn nil 1))
      (setf hnm
            (loop for ne = (next-event mini 'hn)
               while ne
               collect (marks ne)))
      (not (next-event mini 'vn nil 1))
      (setf vnm
            (loop for ne = (next-event mini 'vn)
               while ne
               collect (marks ne)))
      (notany #'not
              (loop for i in `(,flm ,hnm ,vnm)
                 collect
                   (equalp i '(NIL (FFF S A) NIL NIL NIL (FFF S A) NIL NIL 
                               NIL (FFF S A) NIL NIL NIL (FFF S A) NIL NIL 
                               NIL (FFF S A) NIL NIL)))) 
      
      (rm-marks-from-notes mini 1 2 'fl 'fff)
      (not (next-event mini 'fl nil 1))
      (equalp
       (loop for ne = (next-event mini 'fl)
          while ne
          collect (marks ne))
       '(NIL (S A) NIL NIL NIL (S A) NIL NIL NIL (FFF S A) NIL NIL NIL 
         (FFF S A) NIL NIL NIL (FFF S A) NIL NIL))

      (rm-marks-from-notes mini '(1 2) '(2 1) 'hn '(fff a))
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (marks ne))
       '(NIL (S) NIL NIL NIL (FFF S A) NIL NIL NIL (FFF S A) NIL NIL NIL 
         (FFF S A) NIL NIL NIL (FFF S A) NIL NIL))

      (rm-marks-from-notes mini 3 '(4 3) '(hn vn) '(fff s a))
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (marks ne))
       '(NIL (S) NIL NIL NIL (FFF S A) NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL (FFF S A) NIL NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL (FFF S A) NIL NIL NIL (FFF S A) NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL (FFF S A) NIL NIL))
      
      (rm-marks-from-notes mini 5 5 nil 'fff)
      (not (next-event mini 'fl nil 1))
      (equalp
       (loop for ne = (next-event mini 'fl)
          while ne
          collect (marks ne))
       '(NIL (S A) NIL NIL NIL (S A) NIL NIL NIL (FFF S A) NIL NIL NIL 
         (FFF S A) NIL NIL NIL (S A) NIL NIL))
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (marks ne))
       '(NIL (S) NIL NIL NIL (FFF S A) NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL (S A) NIL NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL (FFF S A) NIL NIL NIL (FFF S A) NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL (S A) NIL NIL)))))


;;; SAR Sun Apr 22 16:06:48 BST 2012
(sc-deftest test-sc-edit-rm-slurs ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((fl (flute :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vn (violin :midi-channel 3))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs4 ds4 fs4))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (slur 1 2 slur 3 4 slur 5 6 
                                               slur 7 8)))) 
          :rthm-seq-map '((1 ((fl (1 1 1 1 1))
                              (hn (1 1 1 1 1))
                              (vn (1 1 1 1 1)))))))
        fls hns vns)
    (sc-test-check
      (not (next-event mini 'fl nil 1))
      (setf fls
            (loop for ne = (next-event mini 'fl)
               while ne
               collect (marks ne)))
      (not (next-event mini 'hn nil 1))
      (setf hns
            (loop for ne = (next-event mini 'hn)
               while ne
               collect (marks ne)))
      (not (next-event mini 'vn nil 1))
      (setf vns
            (loop for ne = (next-event mini 'vn)
               while ne
               collect (marks ne)))
      (notany #'not
              (loop for i in `(,fls ,hns ,vns)
                 collect
                   (equalp i '((BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)  
                               (END-SL) (BEG-SL) (END-SL) 
                               (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
                               (END-SL) (BEG-SL) (END-SL) 
                               (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
                               (END-SL) (BEG-SL) (END-SL) 
                               (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
                               (END-SL) (BEG-SL) (END-SL) 
                               (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
                               (END-SL) (BEG-SL) (END-SL))))) 
      (rm-slurs mini 1 2 'fl)
      (not (next-event mini 'fl nil 1))
      (equalp
       (loop for ne = (next-event mini 'fl)
          while ne
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
         (END-SL) (BEG-SL) (END-SL))) 
      (rm-slurs mini '(1 2) '(2 1) 'hn)
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (marks ne))
       '((BEG-SL) NIL NIL NIL NIL NIL NIL NIL NIL (END-SL) (BEG-SL) (END-SL) 
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)))
      (rm-slurs mini 3 '(4 3) '(hn vn))
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (marks ne))
       '((BEG-SL) NIL NIL NIL NIL NIL NIL NIL NIL (END-SL) (BEG-SL) (END-SL)
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) 
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '((BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) 
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)
         (BEG-SL) (END-SL) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (END-SL)
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)))
      (rm-slurs mini 5 5 nil)
      (not (next-event mini 'fl nil 1))
      (equalp
       (loop for ne = (next-event mini 'fl)
          while ne
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)
         (BEG-SL) (END-SL) NIL NIL NIL NIL NIL NIL NIL NIL))
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (marks ne))
       '((BEG-SL) NIL NIL NIL NIL NIL NIL NIL NIL (END-SL) (BEG-SL) (END-SL)
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) NIL NIL NIL
         NIL NIL NIL NIL NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '((BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL)
         (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL) (BEG-SL) (END-SL)
         (BEG-SL) (END-SL) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (END-SL)
         (BEG-SL) (END-SL) (BEG-SL) (END-SL) NIL NIL NIL NIL NIL NIL NIL
         NIL)))))


;;; SAR Tue Apr 24 20:32:50 BST 2012
(sc-deftest test-sc-edit-force-artificial-harmonics ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 f4 b4 e5 a5 d6 g7 c8))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
          :rthm-seq-map '((1 ((vn (1 1 1)))))))
        (ins (get-standard-ins 'violin)))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (chord-equal (make-chord '(e2 a2))
                   (force-artificial-harmonic (make-event 'e4 'q)))
      (not (force-artificial-harmonic (make-event 'e4 'q)
                                      (get-standard-ins 'violin)
                                      nil))
      (chord-equal (make-chord '(e2 a2))
                   (force-artificial-harmonic (make-event 'e4 'q)
                                              (get-standard-ins 'cello)))
      ;; MDE Thu Nov  1 18:07:48 2018 -- pitch class too now
      (chord-equal (make-chord '(ef2 af2))
                   (force-artificial-harmonic (make-pitch 'ef4)
                                              (get-standard-ins 'cello)))
      (chord-equal (make-chord '(ds3 gs3))
                   (force-artificial-harmonic (make-pitch 'ds5)
                                              (get-standard-ins 'viola)))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(C4 F4 B4 B4 E5 A5 A5 D6 C4 F4 B4 B4 E5 A5 A5 D6 C4 F4 B4 B4 E5 A5 A5
         D6))
      (not (next-event mini 'vn nil 1))
      (not
       (loop for ne = (next-event mini 'vn)
          while ne
          when (chord-p (pitch-or-chord ne))
          collect (loop for p in (data (pitch-or-chord ne))
                     collect (data p)
                     collect  (marks p))))
      (force-artificial-harmonics mini 'vn 2 3 3 2)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne))
       '(C4 F4 B4 B4 E5 A5 A5 D6 C4 F4 B4 B4 E5 (A3 D4) (A3 D4) (D4 G4) C4 F4 B4
         B4 E5 A5 A5 D6))
      (not (next-event mini 'vn nil 1))
      (equalp 
       (loop for ne = (next-event mini 'vn)
          while ne
          when (chord-p (pitch-or-chord ne))
          collect (loop for p in (data (pitch-or-chord ne))
                     collect (data p)
                     collect  (marks p)))
       '((A3 NIL D4 (FLAG-HEAD)) (A3 NIL D4 (FLAG-HEAD)) 
         (D4 NIL G4 (FLAG-HEAD)))))))

;;; MDE Thu Jan 10 13:54:12 2019 -- 
(sc-deftest test-sc-edit-force-natural-harmonics ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn violin) (db double-bass) (hp harp)))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((e2 e3 b3 c4 f4 b4 e5 a5 d6 e6 fs6 g7 c8))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (hp (1 1 1))
                              (db (1 1 1))))))))
    (sc-test-check
      (force-natural-harmonics mini 'vn 1)
      (has-mark (get-event mini 1 6 'vn) 'harm)
      (not (has-mark (get-event mini 1 4 'vn) 'harm))
      (not (has-mark (get-event mini 1 5 'vn) 'harm))
      (force-natural-harmonics mini 'db 1)
      (force-natural-harmonics mini 'hp 2 1 2)
      (equalp (ml 'harm 8)
              (loop for i from 1 to 8 collect
                   (first (marks (get-event mini 2 i 'hp)))))
      (not (has-mark (get-event mini 1 8 'hp) 'harm))
      (not (has-mark (get-event mini 3 1 'hp) 'harm))
      (equalp '(iv iv iv nil nil nil nil nil)
              (loop for i from 1 to 8 collect
                   (second (marks (get-event mini 3 i 'db))))))))

(sc-deftest test-sc-edit-force-harmonics ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn violin) (db double-bass)))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((e2 e3 b3 c4 f4 b4 e5 a5 d6 e6 fs6 g7 c8))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (db (1 1 1))))))))
    (sc-test-check
      (force-harmonics mini 'vn 1)
      (has-mark (get-event mini 1 6 'vn) 'harm)
      (not (has-mark (get-event mini 1 4 'vn) 'harm))
      (not (has-mark (get-event mini 1 5 'vn) 'harm))
      (force-harmonics mini 'db 1 :warn nil)
      (artificial-harmonic-simple?
       (written-pitch-or-chord (get-event mini 1 4 'db))))))

;;; SAR Wed Apr 25 11:39:04 BST 2012
(sc-deftest test-sc-edit-auto-accidentals ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((fs4 gs4 as4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 2 1 2 3 2))))) 
          :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (notany #'not
              (loop for ne = (next-event mini 'vn)
                 while ne
                 collect (show-accidental (pitch-or-chord ne))))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (accidental-in-parentheses (pitch-or-chord ne)))) 
      (not (auto-accidentals mini))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (show-accidental (pitch-or-chord ne)))
       '(T T T NIL T NIL T NIL T T T NIL T NIL T NIL T T T NIL T NIL T NIL T T
         T NIL T NIL T NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (accidental-in-parentheses (pitch-or-chord ne)))
       '(NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL T NIL T NIL NIL NIL NIL
         NIL T NIL T NIL NIL NIL NIL NIL T NIL T NIL))
      (not (auto-accidentals mini 4))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (show-accidental (pitch-or-chord ne)))
       '(T T T NIL NIL NIL NIL NIL T T T NIL NIL NIL NIL NIL T T T NIL NIL NIL
         NIL NIL T T T NIL NIL NIL NIL NIL))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (accidental-in-parentheses (pitch-or-chord ne)))))))

;;; SAR Wed Apr 25 13:54:49 BST 2012
(sc-deftest test-sc-edit-change-pitches ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))
                       ;; MDE Thu May 30 20:02:27 2013 -- added clarinet to
                       ;; test changing written pitches 
                       (cl (b-flat-clarinet :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map '((1 ((vc (1 1 1 1 1 1))
                              (cl (1 1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne))
       '(D3 E3 F3 G3 A3 B3 C4 E4 D3 E3 F3 G3 A3 B3 C4 E4 D3 E3 F3 G3 A3 B3 C4
         E4 D3 E3 F3 G3 A3 B3 C4 E4 D3 E3 F3 G3 A3 B3 C4 E4 D3 E3 F3 G3 A3 B3
         C4 E4)) 
      (change-pitches mini 'vc 2 '((fs3 gs3 as3)))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne))
       '(D3 E3 F3 G3 A3 B3 C4 E4 FS3 GS3 AS3 G3 A3 B3 C4 E4 D3 E3 F3 G3 A3 B3
         C4 E4 D3 E3 F3 G3 A3 B3 C4 E4 D3 E3 F3 G3 A3 B3 C4 E4 D3 E3 F3 G3 A3
         B3 C4 E4))
      (change-pitches mini 'vc 3 '((nil nil fs3 gs as ds fs gs) 
                                   nil
                                   (cs4 ds fs)))
      ;; MDE Thu May 30 20:02:41 2013
      (change-pitches mini 'cl 3 '((nil nil fs3 gs as ds fs gs) 
                                   nil
                                   (cs4 ds fs))
                      :written nil)
      (equalp '(D3 D3 FS3 GS3 AS3 DS4 FS4 GS4) 
              (get-pitch-symbols (get-bar mini 3 'cl)))
      (change-pitches mini 'cl 3 '((nil nil fs3 gs as ds fs gs) 
                                   nil
                                   (cs4 ds fs))
                      :written t)
      (equalp '(D3 D3 E3 FS3 AF3 CS3 E3 FS3)
              (get-pitch-symbols (get-bar mini 3 'cl)))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne))
       '(D3 E3 F3 G3 A3 B3 C4 E4 FS3 GS3 AS3 G3 A3 B3 C4 E4 D3 E3 FS3 GS3 AS3 
         DS3 FS3 GS3 D3 E3 F3 G3 A3 B3 C4 E4 CS4 DS4 FS4 G3 A3 B3 C4 E4 D3 E3
         F3 G3 A3 B3 C4 E4)))))

;;; SAR Wed Apr 25 15:48:21 BST 2012
(sc-deftest test-sc-edit-sc-delete-marks-before ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vc)
                while ne
                collect (marks-before ne)))
      (not (loop for m in '(fff s lhp)
              do (add-mark-before-note mini 2 3 'vc m)))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (marks-before ne))
       '(NIL NIL NIL NIL NIL NIL (LHP S FFF) NIL NIL NIL NIL NIL))
      (not (sc-delete-marks-before mini 2 3 'vc))
      (not (next-event mini 'vc nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vc)
                while ne
                collect (marks-before ne))))))

;;; SAR Wed Apr 25 16:03:22 BST 2012
(sc-deftest test-sc-edit-sc-delete-marks-from-event ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))
                                  :marks (a 1 4 lhp 4 s 3 4 slur 1 2))))
          :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (sc-test-check
      (equalp (marks (get-event mini 2 4 'vc)) '(s lhp a))
      (not (sc-delete-marks-from-event mini 2 4 'vc))
      (not (marks (get-event mini 2 4 'vc))))))

;;; SAR Wed Apr 25 16:21:03 BST 2012
(sc-deftest test-sc-edit-sc-force-rest ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :set-palette '((1 ((a3 b3 c4 e4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vc)
                while ne
                collect (is-rest ne)))
      (sc-force-rest mini 2 3 'vc)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (is-rest ne))
       '(NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL))
      (sc-force-rest mini 3 3 'vc t)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (is-rest ne))
       '(NIL NIL NIL NIL NIL NIL T NIL NIL NIL T NIL)))))

;;; MDE Thu Aug 22 15:34:58 2013 -- 
(sc-deftest test-sc-edit-sc-force-rest2 ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :set-palette '((1 ((a3 b3 c4 e4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette 
          '((1 ((((4 4) - e.. 32 - h.) (+w) (+w) ((w)) ((h) (e) q e)
                 (+q - +s e. - +h) (+w) (+w) ((w))))))
          :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (sc-test-check
      (= 1 (sc-force-rest2 mini 1 2 'vc))
      (= 3 (sc-force-rest2 mini 1 3 'vc))
      ;; (print (notes-needed (get-bar mini 5 'vc))))
      (= 3 (sc-force-rest2 mini 5 4 'vc))
      (= 1 (notes-needed (get-bar mini 5 'vc)))
      (= 1 (notes-needed (get-bar mini 6 'vc)))
      (zerop (sc-force-rest2 mini 5 8 'vc nil nil)))))

;;; SAR Wed Apr 25 16:38:30 BST 2012
(sc-deftest test-sc-edit-sc-move-dynamic ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :set-palette '((1 ((a3 b3 c4 e4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))
                                  :marks (fff 1))))
          :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (marks ne))
       '((FFF) NIL NIL NIL (FFF) NIL NIL NIL (FFF) NIL NIL NIL))
      (sc-move-dynamic mini 1 'vc 1 3)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (marks ne))
       '(NIL NIL (FFF) NIL (FFF) NIL NIL NIL (FFF) NIL NIL NIL))
      (sc-move-dynamic mini 2 'vc 1 4 3)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (marks ne))
       '(NIL NIL (FFF) NIL NIL NIL NIL NIL (FFF) NIL NIL (FFF))))))

;;; SAR Wed Apr 25 16:56:28 BST 2012
(sc-deftest test-sc-edit-sc-remove-dynamic ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :set-palette '((1 ((a3 b3 c4 e4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))
                                  :marks (fff 1 ppp 3 dim-beg 1 dim-end 3))))
          :rthm-seq-map '((1 ((vc (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc) while ne collect (marks ne))
       '((DIM-BEG FFF) NIL (DIM-END PPP) NIL (DIM-BEG FFF) NIL (DIM-END PPP) NIL
         (DIM-BEG FFF) NIL (DIM-END PPP) NIL))
      (sc-remove-dynamic mini 2 'vc 1)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc) while ne collect (marks ne))
       '((DIM-BEG FFF) NIL (DIM-END PPP) NIL (DIM-BEG) NIL (DIM-END PPP) NIL
         (DIM-BEG FFF) NIL (DIM-END PPP) NIL))
      (sc-remove-dynamic mini 3 'vc '(1 3))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc) while ne collect (marks ne))
      '((DIM-BEG FFF) NIL (DIM-END PPP) NIL (DIM-BEG) NIL (DIM-END PPP) NIL
        (DIM-BEG) NIL (DIM-END) NIL))
      ;; MDE Mon Nov 26 11:35:37 2018 -- remove hairpins too
      (marks (get-event mini 3 1 'vc))
      (not (remove-dynamics (get-event mini 3 1 'vc) t))
      (not (marks (get-event mini 3 1 'vc))))))

;;; SAR Wed Apr 25 17:27:46 BST 2012
(sc-deftest test-sc-edit-sc-remove-dynamics ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4 f4 g4 a4 b4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))
                                  :marks (fff 1 ppp 3))))
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (va (1 1 1))
                              (vc (1 1 1)))))))
        vnm vam vcm)
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (setf vnm
            (loop for ne = (next-event mini 'vn)
               while ne
               collect (marks ne)))
      (not (next-event mini 'va nil 1))
      (setf vam
            (loop for ne = (next-event mini 'va)
               while ne
               collect (marks ne)))
      (not (next-event mini 'vc nil 1))
      (setf vcm
            (loop for ne = (next-event mini 'vc)
               while ne
               collect (marks ne)))
      (notany #'not
              (loop for i in `(,vnm ,vam ,vcm)
                 collect (equalp i
                                 '((FFF) NIL (PPP) NIL (FFF) NIL (PPP) NIL
                                   (FFF) NIL (PPP) NIL))))
      (sc-remove-dynamics mini '(1 2) '(2 2) 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '((FFF) NIL NIL NIL NIL NIL (PPP) NIL (FFF) NIL (PPP) NIL)) 
      (sc-remove-dynamics mini 2 3 '(va vc))  
      (not (next-event mini 'va nil 1))
      (setf vam
            (loop for ne = (next-event mini 'va)
               while ne
               collect (marks ne)))
      (not (next-event mini 'vc nil 1))
      (setf vcm
            (loop for ne = (next-event mini 'vc)
               while ne
               collect (marks ne)))
      (notany #'not
              (loop for i in `(,vam ,vcm)
                 collect (equalp i
                                 '((FFF) NIL (PPP) NIL NIL NIL NIL NIL NIL NIL 
                                   NIL NIL)))))))

;;; SAR Wed Apr 25 18:59:43 BST 2012
(sc-deftest test-sc-edit-set-rehearsal-letter ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((ds3 e3 fs3 af3 bf3 c4 ef4 fs4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1))
                              (va (1 1 1 1))
                              (vc (1 1 1 1))))))))
    (sc-test-check
      (every #'(lambda (x) (every #'not x))
             (loop for i in '(vn va vc)
                collect 
                  (loop for bn from 1 to 4
                     collect (rehearsal-letter (get-bar mini bn i))))) 
      (set-rehearsal-letter mini 2 'A)
      (equalp
       (loop for i in '(vn va vc)
          collect 
            (loop for bn from 1 to 4
               collect (rehearsal-letter (get-bar mini bn i))))
       '((A NIL NIL NIL) (NIL NIL NIL NIL) (NIL NIL NIL NIL)))
      (set-rehearsal-letter mini 3 '2 '(va vc))
      (equalp
       (loop for i in '(vn va vc)
          collect 
            (loop for bn from 1 to 4
               collect (rehearsal-letter (get-bar mini bn i))))
       '((A NIL NIL NIL) (NIL 2 NIL NIL) (NIL 2 NIL NIL)))
      (set-rehearsal-letter mini 4 'Z3)
      (equalp
       (loop for i in '(vn va vc)
          collect 
            (loop for bn from 1 to 4
               collect (rehearsal-letter (get-bar mini bn i))))
       '((A NIL Z3 NIL) (NIL 2 NIL NIL) (NIL 2 NIL NIL))))))

;;; SAR Thu Apr 26 11:29:19 BST 2012
(sc-deftest test-sc-edit-sc-delete-marks ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((cs4 ds4 fs4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                  :pitch-seq-palette ((1 2 3))
                                  :marks (a 2 s 2 fff 2 pizz 2))))
          :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks ne))
       '(NIL NIL (PIZZ FFF S A) NIL NIL NIL (PIZZ FFF S A) NIL NIL NIL
         (PIZZ FFF S A) NIL NIL NIL (PIZZ FFF S A) NIL))
      (sc-delete-marks mini 2 2 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn) 
          while ne 
          collect (marks ne)) 
       '(NIL NIL (PIZZ FFF S A) NIL NIL NIL NIL NIL NIL NIL (PIZZ FFF S A) NIL 
         NIL NIL (PIZZ FFF S A) NIL)))))

;;; SAR Thu Apr 26 11:41:16 BST 2012
(sc-deftest test-sc-edit-replace-multi-bar-events ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((d4 e4 f4 g4))))
          :set-map '((1 (1 1 1 1 1 1))
                     (2 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4))))
                              (2 ((((2 4) e s s q)
                                   (s s e +e e))
                                  :pitch-seq-palette ((1 2 3 4 3 2 4 1)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))
                          (2 ((vn (2 2 2 2 2 2))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(D4 Q E4 E F4 S G4 S D4 Q E4 E F4 S G4 S D4 Q E4 E F4 S G4 S D4 Q E4 E
         F4 S G4 S D4 Q E4 E F4 S G4 S D4 Q E4 E F4 S G4 S D4 E E4 S F4 S G4 Q
         F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E"
         D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S F4 S G4
         Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E"
         D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E))
      ;; MDE Fri Aug 29 12:33:00 2014 -- make sure we're getting different
      ;; numbers of notes when filling empty bars
      (= 72 (total-notes (get-player (ensemble mini) 'vn)))
      (force-rest-bar (get-bar mini 2 'vn))
      (update-instrument-slots mini)
      (= 68 (total-notes (get-player (ensemble mini) 'vn)))
      (replace-multi-bar-events mini 'vn 2 3 
                                '((cs5 h) ((ds5 fs5) h) (nil h)))
      (update-instrument-slots mini)
      (= 62 (total-notes (get-player (ensemble mini) 'vn)))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(D4 Q E4 E F4 S G4 S CS5 H (DS5 FS5) H NIL H D4 Q E4 E F4 S G4 S D4 Q
         E4 E F4 S G4 S D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4
         S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S F4 S G4 Q F4 S E4 S
         G4 E G4 "E" D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E
         E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S F4 S G4 Q F4 S E4
         S G4 E G4 "E" D4 E)) 
      (replace-multi-bar-events mini 'vn '(2 2 2) '3 
                                '((h h h) (cs5 (ds5 fs5) nil))
                                :interleaved nil)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(D4 Q E4 E F4 S G4 S CS5 H (DS5 FS5) H NIL H D4 Q E4 E F4 S G4 S D4 Q
         E4 E F4 S G4 S D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4
         S F4 S G4 Q CS5 H (DS5 FS5) H NIL H D4 E E4 S F4 S G4 Q F4 S E4 S G4 E
         G4 "E" D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S
         F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E))
      (replace-multi-bar-events mini 'vn 1 1
                                '((nil e) (nil e) (nil e) (cs4 e))
                                :consolidate-rests t)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL 4 NIL E CS4 E CS5 H (DS5 FS5) H NIL H D4 Q E4 E F4 S G4 S D4 Q E4
         E F4 S G4 S D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S
         F4 S G4 Q CS5 H (DS5 FS5) H NIL H D4 E E4 S F4 S G4 Q F4 S E4 S G4 E
         G4 "E" D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S
         F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E))
      (replace-multi-bar-events mini 'vn 8 1
                                '((nil q) (b3 e) (cs4 s) (ds4 s))
                                :auto-beam t)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL 4 NIL E CS4 E CS5 H (DS5 FS5) H NIL H D4 Q E4 E F4 S G4 S D4 Q E4
         E F4 S G4 S D4 E E4 S F4 S G4 Q NIL Q B3 E CS4 S DS4 S D4 E E4 S F4 S
         G4 Q CS5 H (DS5 FS5) H NIL H D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E"
         D4 E D4 E E4 S F4 S G4 Q F4 S E4 S G4 E G4 "E" D4 E D4 E E4 S F4 S G4
         Q F4 S E4 S G4 E G4 "E" D4 E)))))

;;; SAR Thu Apr 26 13:11:47 BST 2012
(sc-deftest test-sc-edit-tie ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q s s (s) s))
                                  :pitch-seq-palette ((1 1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (is-tied-from ne)
                collect (is-tied-to ne)))
      (tie mini 2 1 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL)) 
      (tie mini 3 2 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL
         NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL))
      (tie mini 4 2 'vn -.5)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL
         NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL -0.5 NIL NIL T NIL
         NIL NIL NIL)))))

;;; SAR Thu Apr 26 13:46:12 BST 2012
(sc-deftest test-sc-edit-tie-over-all-rests ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4))))
          :set-map '((1 (1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (q) e (s) s))
                                  :pitch-seq-palette ((1 2)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S NIL Q C4 E NIL S D4 S NIL Q C4 E NIL S D4 S NIL
         Q C4 E NIL S D4 S NIL Q C4 E NIL S D4 S NIL Q C4 E NIL S D4 S NIL Q C4
         E NIL S D4 S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-rest ne))
       '(T NIL T NIL T NIL T NIL T NIL T NIL T NIL T NIL T NIL T NIL T NIL T
         NIL T NIL T NIL))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (is-tied-from ne)
                collect (is-tied-to ne)))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (beam ne)))
      (tie-over-all-rests mini 'vn 2 3 :start-note 2 :auto-beam t)
      ;; MDE Wed Feb 4 12:31:29 2015 -- to make sure we're not getting the
      ;; error "slippery-chicken::get-events-sorted-by-time: event must be
      ;; lacking a player slot."
      (write-antescofo mini 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S NIL Q C4 E NIL S D4 S D4 Q C4 E C4 S D4 S D4 Q
         C4 E NIL S D4 S NIL Q C4 E NIL S D4 S NIL Q C4 E NIL S D4 S NIL Q C4 E
         NIL S D4 S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-rest ne))
       '(T NIL T NIL T NIL T NIL NIL NIL NIL NIL NIL NIL T NIL T NIL T NIL T
         NIL T NIL T NIL T NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL T T
         NIL NIL T T NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL)) 
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (beam ne))
       '(NIL NIL NIL NIL NIL 1 NIL 0 NIL 1 NIL 0 NIL 1 NIL 0 NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL))
      (tie-over-all-rests mini 'vn 5 6 :end-note 1 :consolidate-notes t)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S NIL Q C4 E NIL S D4 S D4 Q C4 E C4 S D4 S D4 Q
         C4 E NIL S D4 S NIL Q C4 E. D4 S D4 Q C4 E. D4 S NIL Q C4 E NIL S D4
         S)) 
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-rest ne))
       '(T NIL T NIL T NIL T NIL NIL NIL NIL NIL NIL NIL T NIL T NIL NIL NIL
         NIL NIL T NIL T NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL T T 
         NIL NIL T T NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL
         NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (beam ne))
       '(NIL NIL NIL NIL NIL 1 NIL 0 NIL 1 NIL 0 NIL 1 NIL 0 NIL 1 0 NIL 1 0 NIL
         NIL NIL NIL)))))



;;; SAR Fri Apr 20 16:09:39 BST 2012
;;; SAR Thu Aug  2 13:44:37 BST 2012: Commented out. See Ticket #383
#|
#+cmn
(sc-deftest test-sc-edit-enharmonic-spellings ()
  ;; (print cm::*scale*)
  ;; (print +pitch-seq-lowest-equals-prefers-high+)
  ;; (print +pitch-seq-lowest-equals-prefers-low+)
  (setf (chord-function
         (get-data 'piano-lh +slippery-chicken-standard-instrument-palette+))
        'piano-chord-fun
        (chord-function
         (get-data 'piano +slippery-chicken-standard-instrument-palette+))
        'piano-chord-fun)
  (let ((esmini
         (make-slippery-chicken
          '+esmini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (pn (piano :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((cs2 ds2 e2 fs2 gs2 as2 b2 
                                  cs3 ds3 e3 fs3 gs3 as3 b3
                                  cs4 ds4 e4 fs4 gs4 as4 fs5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 (2) 3 4 (5) 6 7 8)))))
          :rthm-seq-map '((1 ((cl (1 1 1))
                              (pn (1 1 1))
                              (vc (1 1 1))))))))
    (sc-test-check
      (equalp
       (loop for e in '(3 4)
          collect (data 
                   (pitch-or-chord (get-note esmini 3 e 'cl)))
          collect (data 
                   (written-pitch-or-chord (get-note esmini 3 e 'cl)))) 
       '(FS3 GS3 GS3 AS3))              ;(FS3 AF3 GS3 BF3)
      (equalp (pitch-list-to-symbols 
               (data (pitch-or-chord (get-note esmini 2 2 'pn)))) 
              '(CS2 DS2 E2 FS2))
      (equalp (loop for e in '(1 3 4 6)
                 collect (data (pitch-or-chord (get-note esmini 1 e 'vc)))) 
              '(E4 FS4 GS4 AS4))
      (enharmonic-spellings esmini '((cl (3 3 t) (3 4 t))
                                     (pn (2 (2 4)))
                                     (vc (1 1) (1 3) (1 4) (1 6))))
      (equalp
       (loop for e in '(3 4)
          collect (data 
                   (pitch-or-chord (get-note esmini 3 e 'cl)))
          collect (data 
                   (written-pitch-or-chord (get-note esmini 3 e 'cl)))) 
       '(FS3 AF3 GS3 BF3))              ; (FS3 GS3 GS3 AS3)
      (equalp (pitch-list-to-symbols 
               (data (pitch-or-chord (get-note esmini 2 2 'pn)))) 
              '(CS2 DS2 E2 GF2))
      (equalp (loop for e in '(1 3 4 6)
                 collect (data (pitch-or-chord (get-note esmini 1 e 'vc))))
              '(FF4 GF4 AF4 BF4)))))
|#

;;; SAR Wed Jul 18 13:11:55 BST 2012: tie-over-all-rests into last bar
(sc-deftest test-sc-edit-tie-over-all-rests-into-last-bar ()
  (let* ((mini
          (make-slippery-chicken
           '+sc-object+
           :ensemble '(((va (viola :midi-channel 2))))
           :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e (e) e (e) (e) e e e))
                                   :pitch-seq-palette ((1 2 3 4 5)))))
           :rthm-seq-map '((1 ((va (1 1 1))))))))
    (probe-delete "/tmp/slippery-chicken-piece.eps")
    (sc-test-check
      (tie-over-all-rests mini 'va 1 3)
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne)
          collect (is-tied-to ne)
          collect (is-tied-from ne))
       '(C3 E NIL T C3 E T NIL D3 E NIL T D3 E T T D3 E T NIL E3 E NIL
         NIL F3 E NIL NIL G3 E NIL NIL C3 E NIL T C3 E T NIL D3 E NIL T
         D3 E T T D3 E T NIL E3 E NIL NIL F3 E NIL NIL G3 E NIL NIL C3 E
         NIL T C3 E T NIL D3 E NIL T D3 E T T D3 E T NIL E3 E NIL NIL F3
         E NIL NIL G3 E NIL NIL))
      #+cmn (cmn-display mini)
      #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 14000))))

;;; SAR Thu Apr 26 14:56:55 BST 2012
(sc-deftest test-sc-edit-trill ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q. s s))
                                  :pitch-seq-palette ((1 3 2)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (marks-before ne)
                collect (marks ne)))
      (trill mini 'vn 2 1 'e4)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL (BEG-TRILL-A) ((TRILL-NOTE E4)) NIL 
         (END-TRILL-A) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL NIL NIL))
      (trill mini 'vn 3 1 'e4 3)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL (BEG-TRILL-A) ((TRILL-NOTE E4)) NIL 
         (END-TRILL-A) NIL NIL (BEG-TRILL-A) ((TRILL-NOTE E4)) NIL NIL NIL 
         (END-TRILL-A) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (trill mini 'vn 4 1 'e4 3 5)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (marks-before ne)
          collect (marks ne))
       '(NIL NIL NIL NIL NIL NIL (BEG-TRILL-A) ((TRILL-NOTE E4)) NIL 
         (END-TRILL-A) NIL NIL (BEG-TRILL-A) ((TRILL-NOTE E4)) NIL NIL NIL 
         (END-TRILL-A) (BEG-TRILL-A) ((TRILL-NOTE E4)) NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL (END-TRILL-A))))))

;;; SAR Thu Apr 26 15:16:05 BST 2012
(sc-deftest test-sc-edit-unset-cautionary-accidental ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vn (violin :midi-channel 2))))
          :set-palette '((1 ((cs4 ds4 fs4))))
          :set-map '((1 (1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
          :rthm-seq-map '((1 ((cl (1 1))
                              (vn (1 1)))))))
        claip claipw)
    (sc-test-check
      (respell-notes mini)
      (not (next-event mini 'cl nil 1))
      (setf claip
            (loop for ne = (next-event mini 'cl)
               while ne
               collect (accidental-in-parentheses (pitch-or-chord ne))))
      (not (next-event mini 'cl nil 1))
      (setf claipw
            (loop for ne = (next-event mini 'cl)
               while ne
               collect (accidental-in-parentheses 
                        (written-pitch-or-chord ne))))
      (notany #'not
              (loop for i in `(,claip ,claipw)
                 collect 
                   (equalp i '(NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL T 
                               NIL T NIL))))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (accidental-in-parentheses (pitch-or-chord ne)))
       '(NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL))
      (not (unset-cautionary-accidental mini 2 5 'vn))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (accidental-in-parentheses (pitch-or-chord ne)))
       '(NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)) 
      (not (unset-cautionary-accidental mini 2 7 'cl t))
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl)
          while ne
          collect (accidental-in-parentheses (pitch-or-chord ne)))
       '(NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL T NIL T NIL))
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl)
          while ne
          collect (accidental-in-parentheses (written-pitch-or-chord ne))) 
       '(NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL T NIL NIL NIL)))))

;;; SAR Fri Apr 27 10:59:43 BST 2012
(sc-deftest test-sc-edit-tie-all-last-notes-over-rests ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 f4 g4 a4 c5 d5 f5))))
          :set-map '((1 (1 1)))
          :rthm-seq-palette '((1 ((((4 4) e (e) e e (e) (e) e e) 
                                   ((w)) 
                                   ((h.) q) 
                                   ((w))
                                   ((w)) 
                                   ((e) e h.))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 7)))))
          :rthm-seq-map '((1 ((vn (1 1))
                              (va (1 1))
                              (vc (1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(G3 E NIL E A3 E B3 E NIL E NIL E C4 E D4 E NIL 1 NIL H. F4 Q NIL 1 NIL  
         1 NIL E G4 E G4 H. A3 E NIL E A3 E B3 E NIL E NIL E C4 E D4 E NIL 1
         NIL H. F4 Q NIL 1 NIL 1 NIL E G4 E G4 H.))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (is-tied-from ne)
                collect (is-tied-to ne)))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(F3 E NIL E A4 E A4 E NIL E NIL E C5 E D5 E NIL 1 NIL H. F5 Q NIL 1 NIL 
         1 NIL E F5 E F5 H. A4 E NIL E A4 E A4 E NIL E NIL E C5 E D5 E NIL 1
         NIL H. F5 Q NIL 1 NIL 1 NIL E F5 E F5 H.))
      (not (next-event mini 'va nil 1))
      (every #'not
             (loop for ne = (next-event mini 'va)
                while ne
                collect (is-tied-from ne)
                collect (is-tied-to ne)))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(G3 E NIL E A3 E A3 E NIL E NIL E B3 E B3 E NIL 1 NIL H. C4 Q NIL 1 NIL 
         1 NIL E C4 E C4 H. G3 E NIL E A3 E A3 E NIL E NIL E B3 E B3 E NIL 1
         NIL H. C4 Q NIL 1 NIL 1 NIL E C4 E C4 H.))
      (not (next-event mini 'vc nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vc)
                while ne
                collect (is-tied-from ne)
                collect (is-tied-to ne)))
      (tie-all-last-notes-over-rests mini 2 6 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(G3 E NIL E A3 E B3 E NIL E NIL E C4 E D4 E NIL 1 NIL H. F4 Q F4 W F4 W 
         F4 E G4 E G4 H. A3 E NIL E A3 E B3 E NIL E NIL E C4 E D4 E NIL 1 NIL
         H. F4 Q NIL 1 NIL 1 NIL E G4 E G4 H.))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL T NIL T T T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (tie-all-last-notes-over-rests mini 9 12 'vn :auto-beam t)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(G3 E NIL E A3 E B3 E NIL E NIL E C4 E D4 E NIL 1 NIL H. F4 Q F4 W F4 W 
         F4 E G4 E G4 H. A3 E NIL E A3 E B3 E NIL E NIL E C4 E D4 E NIL 1 NIL
         H. F4 Q F4 W F4 W F4 E G4 E G4 H.))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL T NIL T T T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL T T 
         T T NIL T NIL NIL NIL NIL))
      (tie-all-last-notes-over-rests mini 3 5 '(va vc) :to-next-attack nil)
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(F3 E NIL E A4 E A4 E NIL E NIL E C5 E D5 E NIL 1 NIL H. F5 Q F5 W F5 W  
         NIL E F5 E F5 H. A4 E NIL E A4 E A4 E NIL E NIL E C5 E D5 E NIL 1 NIL
         H. F5 Q NIL 1 NIL 1 NIL E F5 E F5 H.))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(G3 E NIL E A3 E A3 E NIL E NIL E B3 E B3 E NIL 1 NIL H. C4 Q C4 W C4 W  
         NIL E C4 E C4 H. G3 E NIL E A3 E A3 E NIL E NIL E B3 E B3 E NIL 1 NIL
         H. C4 Q NIL 1 NIL 1 NIL E C4 E C4 H.))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (tie-all-last-notes-over-rests mini 9 12 'vc :tie-next-attack t)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(G3 E NIL E A3 E A3 E NIL E NIL E B3 E B3 E NIL 1 NIL H. C4 Q C4 W C4 W  
         NIL E C4 E C4 H. G3 E NIL E A3 E A3 E NIL E NIL E B3 E B3 E NIL 1 NIL
         H. C4 Q C4 W C4 W C4 Q C4 H.))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL 
         T T T T NIL T NIL NIL))
      (tie-all-last-notes-over-rests mini 7 9 'va :last-rhythm 'e)
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(F3 E NIL E A4 E A4 E NIL E NIL E C5 E D5 E NIL 1 NIL H. F5 Q F5 W F5 W
         NIL E F5 E F5 H. A4 E NIL E A4 E A4 E NIL E NIL E C5 E D5 E D5 W D5 E
         NIL H NIL E F5 Q NIL 1 NIL 1 NIL E F5 E F5 H.))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL T T NIL T NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))))

;;; SAR Fri Apr 27 11:32:54 BST 2012
(sc-deftest test-sc-edit-tie-over-rests ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (q) e (s) s)
                                   ((h))
                                   ((s) e. (e) e)
                                   ((h))
                                   ((h))
                                   ((e) q s (s)))
                                  :pitch-seq-palette ((1 2 2 3 3 1)))))
          :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. NIL E E4 E NIL 2 NIL 2 NIL E 
         E4 Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. NIL E E4 E NIL  
         2 NIL 2 NIL E E4 Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4
         E. NIL E E4 E NIL 2 NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (is-tied-from ne)
                collect (is-tied-to ne)))
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn)
                while ne
                collect (beam ne)))
      (tie-over-rests mini 1 2 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S D4 H D4 S D4 E. NIL E E4 E NIL 2 NIL 2 NIL E E4 
         Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. NIL E E4 E NIL 2 
         NIL 2 NIL E E4 Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4
         E. NIL E E4 E NIL 2 NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL)) 
      (tie-over-rests mini 7 1 'vn)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S D4 H D4 S D4 E. NIL E E4 E NIL 2 NIL 2 NIL E E4
         Q C4 S NIL S NIL Q C4 E. D4 S NIL 2 NIL S D4 E. NIL E E4 E NIL 2 NIL 2
         NIL E E4 Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. NIL E E4
         E NIL 2 NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL))
      (tie-over-rests mini 9 2 'vn :end-bar 10)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S D4 H D4 S D4 E. NIL E E4 E NIL 2 NIL 2 NIL E E4
         Q C4 S NIL S NIL Q C4 E. D4 S NIL 2 NIL S D4 E. NIL E E4 E E4 H NIL 2
         NIL E E4 Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. NIL E E4
         E NIL 2 NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)) 
      (tie-over-rests mini 13 1 'vn 
                           :auto-beam t 
                           :consolidate-notes nil)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S D4 H D4 S D4 E. NIL E E4 E NIL 2 NIL 2 NIL E E4
         Q C4 S NIL S NIL Q C4 E. D4 S NIL 2 NIL S D4 E. NIL E E4 E E4 H NIL 2
         NIL E E4 Q C4 S NIL S NIL Q C4 E C4 S D4 S NIL 2 NIL S D4 E. NIL E E4
         E NIL 2 NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (beam ne))
       '(NIL 1 NIL 0 NIL 1 0 NIL NIL NIL NIL NIL NIL NIL NIL NIL 1 0 NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 1 NIL 0 NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL)))))

;;; SAR Fri Apr 27 12:12:03 BST 2012
(sc-deftest test-sc-edit-tie-over-rest-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((c4 d4 e4))))
          :set-map '((1 (1 1)))
          :rthm-seq-palette '((1 ((((2 4) (q) e (s) s)
                                   ((h))
                                   ((s) e. e e)
                                   ((h))
                                   ((h))
                                   ((e) q s (s)))
                                  :pitch-seq-palette ((1 2 2 1 3 3 1)))))
          :rthm-seq-map '((1 ((vn (1 1))
                              (va (1 1))
                              (vc (1 1)))))))
        vnd vad vcd)
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. C4 E E4 E NIL 2 NIL 2 NIL E E4
         Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. C4 E E4 E NIL 2
         NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q D4 E NIL S E4 S NIL 2 NIL S E4 E. D4 E E4 E NIL 2 NIL 2 NIL E E4
         Q D4 S NIL S NIL Q D4 E NIL S E4 S NIL 2 NIL S E4 E. D4 E E4 E NIL 2
         NIL 2 NIL E E4 Q D4 S NIL S))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S E4 S NIL 2 NIL S E4 E. C4 E E4 E NIL 2 NIL 2 NIL E E4
         Q C4 S NIL S NIL Q C4 E NIL S E4 S NIL 2 NIL S E4 E. C4 E E4 E NIL 2
         NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (setf vnd
            (loop for ne = (next-event mini 'vn)
               while ne
               collect (is-tied-from ne)
               collect (is-tied-to ne)))
      (not (next-event mini 'va nil 1))
      (setf vad
            (loop for ne = (next-event mini 'va)
               while ne
               collect (is-tied-from ne)
               collect (is-tied-to ne)))
      (not (next-event mini 'vc nil 1))
      (setf vcd
            (loop for ne = (next-event mini 'vc)
               while ne
               collect (is-tied-from ne)
               collect (is-tied-to ne)))
      (notany #'not
              (loop for i in `(,vnd ,vad ,vcd)
                 collect (every #'not i)))
      (not (next-event mini 'vn nil 1))
      (setf vnd
            (loop for ne = (next-event mini 'vn)
               while ne
               collect (beam ne)))
      (not (next-event mini 'va nil 1))
      (setf vad
            (loop for ne = (next-event mini 'va)
               while ne
               collect (beam ne)))
      (not (next-event mini 'vc nil 1))
      (setf vcd
            (loop for ne = (next-event mini 'vc)
               while ne
               collect (beam ne)))
      (notany #'not
              (loop for i in `(,vnd ,vad ,vcd)
                 collect (every #'not i)))
      (tie-over-rest-bars mini 1 'vn :end-bar 2)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S D4 H NIL S D4 E. C4 E E4 E NIL 2 NIL 2 NIL E E4
         Q C4 S NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. C4 E E4 E NIL 2
         NIL 2 NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL))
      (tie-over-rest-bars mini 3 'va :end-bar 5)
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q D4 E NIL S E4 S NIL 2 NIL S E4 E. D4 E E4 E E4 H E4 H NIL E E4 Q
         D4 S NIL S NIL Q D4 E NIL S E4 S NIL 2 NIL S E4 E. D4 E E4 E NIL 2 NIL
         2 NIL E E4 Q D4 S NIL S))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL
         T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL))
      (tie-over-rest-bars mini 3 '(vn vc) :end-bar 6 :tie-next-attack t)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S D4 H NIL S D4 E. C4 E E4 E E4 H E4 H E4 Q. C4 S
         NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. C4 E E4 E NIL 2 NIL 2
         NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL T NIL T T
         T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL))  
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S E4 S NIL 2 NIL S E4 E. C4 E E4 E E4 H E4 H E4 Q. C4 S 
         NIL S NIL Q C4 E NIL S E4 S NIL 2 NIL S E4 E. C4 E E4 E NIL 2 NIL 2
         NIL E E4 Q C4 S NIL S))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL
         T T T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL))  
      (tie-over-rest-bars mini 7 'vc 
                               :end-bar 9
                               :to-next-attack t
                               :auto-beam t)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S E4 S NIL 2 NIL S E4 E. C4 E E4 E E4 H E4 H E4 Q. C4 S 
         NIL S NIL Q C4 E NIL S E4 S E4 H E4 S E4 E. C4 E E4 E NIL 2 NIL 2 NIL
         E E4 Q C4 S NIL S))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL
         T T T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL T T NIL T
         NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL))
      (tie-over-rest-bars mini 9 'vn :end-bar 11 :last-rhythm 'e)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (is-tied-from ne)
          collect (is-tied-to ne))
       '(NIL NIL NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL T NIL T T
         T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL T NIL T T NIL T NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL))
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(NIL Q C4 E NIL S D4 S D4 H NIL S D4 E. C4 E E4 E E4 H E4 H E4 Q. C4 S 
         NIL S NIL Q C4 E NIL S D4 S NIL 2 NIL S D4 E. C4 E E4 E E4 H E4 E NIL 
         Q. NIL E E4 Q C4 S NIL S)))))

;;; SAR Fri Apr 27 13:13:02 BST 2012
(sc-deftest test-sc-edit-set-cautionary-accidental ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (pn (piano :midi-channel 2))))
          :set-palette '((1 ((ds3 e3 fs3 af3 bf3 c4 ef4 fs4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 (3) 4))
                                  :marks (fff 1 ppp 3))))
          :rthm-seq-map '((1 ((cl (1 1 1))
                              (pn (1 1 1))))))))
    (respell-notes mini)
    (sc-test-check
      (not (next-event mini 'cl nil 1))
      (every #'not
             (loop for ne = (next-event mini 'cl)
                while ne
                collect 
                  (accidental-in-parentheses (pitch-or-chord ne))
                collect 
                  (accidental-in-parentheses (written-pitch-or-chord ne)))) 
      (not (next-event mini 'pn nil 1))
      (equalp
       (loop for ne = (next-event mini 'pn)
          while ne
          when (pitch-p (pitch-or-chord ne))
          collect (accidental-in-parentheses (pitch-or-chord ne))
          when (chord-p (pitch-or-chord ne))
          collect (loop for p in (data (pitch-or-chord ne))
                     collect (accidental-in-parentheses p)))
       '(NIL NIL (NIL NIL NIL NIL) NIL NIL NIL (NIL NIL NIL NIL) NIL NIL NIL 
         (NIL NIL NIL NIL) NIL))
      (set-cautionary-accidental mini 2 2 'cl)
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl)
          while ne
          collect (accidental-in-parentheses (pitch-or-chord ne))
          collect (accidental-in-parentheses (written-pitch-or-chord ne)))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL
         NIL NIL NIL NIL NIL))

      (set-cautionary-accidental mini 3 2 'cl t)
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl)
          while ne
          collect (accidental-in-parentheses (pitch-or-chord ne))
          collect (accidental-in-parentheses (written-pitch-or-chord ne)))
       '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL
         NIL T NIL NIL NIL NIL))
      (set-cautionary-accidental mini 2 1 'pn)
      (not (next-event mini 'pn nil 1))
      (equalp
       (loop for ne = (next-event mini 'pn)
          while ne
          when (pitch-p (pitch-or-chord ne))
          collect (accidental-in-parentheses (pitch-or-chord ne))
          when (chord-p (pitch-or-chord ne))
          collect (loop for p in (data (pitch-or-chord ne))
                     collect (accidental-in-parentheses p)))
       '(NIL NIL (NIL NIL NIL NIL) NIL T NIL (NIL NIL NIL NIL) NIL NIL NIL 
         (NIL NIL NIL NIL) NIL))
      (set-cautionary-accidental mini 2 2 'pn)
      (not (next-event mini 'pn nil 1))
      (equalp
       (loop for ne = (next-event mini 'pn)
          while ne
          when (pitch-p (pitch-or-chord ne))
          collect (accidental-in-parentheses (pitch-or-chord ne))
          when (chord-p (pitch-or-chord ne))
          collect (loop for p in (data (pitch-or-chord ne))
                     collect (accidental-in-parentheses p)))
       '(NIL NIL (NIL NIL NIL NIL) NIL T T (NIL NIL NIL NIL) NIL NIL NIL 
         (NIL NIL NIL NIL) NIL))
      (set-cautionary-accidental mini 3 '(3 3) 'pn)
      (not (next-event mini 'pn nil 1))
      (equalp
       (loop for ne = (next-event mini 'pn)
          while ne
          when (pitch-p (pitch-or-chord ne))
          collect (accidental-in-parentheses (pitch-or-chord ne))
          when (chord-p (pitch-or-chord ne))
          collect (loop for p in (data (pitch-or-chord ne))
                     collect (accidental-in-parentheses p)))
       '(NIL NIL (NIL NIL NIL NIL) NIL T T (NIL NIL NIL NIL) NIL NIL NIL 
         (NIL NIL T NIL) NIL)))))

;;; SAR Wed May  2 13:26:55 BST 2012
(sc-deftest test-sc-edit-delete-beams ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
          :set-map '((1 (1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) - e e - - e e - - e e - - e e -))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
          :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (beam ne))
       '(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)) 
      (sc-delete-beams mini 2 'vc)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (beam ne))
       '(1 0 1 0 1 0 1 0 NIL NIL NIL NIL NIL NIL NIL NIL 1 0 1 0 1 0 1 0 1 0 1
         0 1 0 1 0)) 
      (not (sc-delete-beams mini 3 'vc 3 4))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (beam ne))
       '(1 0 1 0 1 0 1 0 NIL NIL NIL NIL NIL NIL NIL NIL 1 0 NIL NIL 1 0 1 0 1
         0 1 0 1 0 1 0)))))

;;; SAR Fri Jun  8 13:39:34 BST 2012
(sc-deftest test-sc-edit-process-events-by-time ()
  (let ((marks-for-sc-edit-test (make-cscl '(a s as te ts at)))
        (mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((3 4) s (e.)  (s) s (e) (e) s (s)))
                                  :pitch-seq-palette ((1 2 3))))
                              (2 ((((3 4) (s) s (e) (e) s (s) s (e.)))
                                  :pitch-seq-palette ((1 2 3))))
                              (3 ((((3 4) (e) s (s) s (e.)  (s) s (e)))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 2 3))
                              (va (2 3 1))
                              (vc (3 1 2))))))))
    (defun add-random-marks-for-sc-edit-test (event)
      (unless (is-rest event) 
        (setf (marks event) (list (get-next marks-for-sc-edit-test)))))  
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vn t)
                while ne
                collect (marks ne)))
      (not (next-event mini 'va nil 1))
      (every #'not
             (loop for ne = (next-event mini 'va t)
                while ne
                collect (marks ne)))
      (not (next-event mini 'vc nil 1))
      (every #'not
             (loop for ne = (next-event mini 'vc t)
                while ne
                collect (marks ne)))
      (process-events-by-time mini #'add-random-marks-for-sc-edit-test 
                              :start-bar 2 :end-bar 2)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn t)
          while ne
          collect (marks ne))
       '(NIL NIL NIL (S) (AT) (A) NIL NIL NIL))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va t)
          while ne
          collect (marks ne))
       '(NIL NIL NIL (AS) (TE) (S) NIL NIL NIL))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc t)
          while ne
          collect (marks ne))
       '(NIL NIL NIL (A) (TS) (AS) NIL NIL NIL)))))

;;; SAR Tue Jun 12 13:12:04 BST 2012
(sc-deftest test-sc-edit-auto-slur ()
  (let* ((mini-1
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vn (violin :midi-channel 1))
                        (vc (cello :midi-channel 2))))
           :tempo-map '((1 (q 60)))
           :set-palette '((1 ((c3 d3 e3 f3 g4 a3 b3
                                  c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) - e e - (s) e.  
                                     - s s e - - s (s) s s -))
                                   :pitch-seq-palette ((1 2 3 4 5 6 7 8 9))
                                   :marks (a 4 s 1 5 slur 1 2 slur 4 6))))
           :rthm-seq-map '((1 ((vn (1 1 1))
                               (vc (1 1 1)))))))
         (mini-2 (clone mini-1))
         (mini-3 (clone mini-1))
         (mini-4 (clone mini-1)))
    (sc-test-check
      (not (next-event mini-1 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini-1 'vn)
          while ne
          collect (marks ne))
       '((BEG-SL S) (END-SL S) NIL (S) (BEG-SL S A) (S) (END-SL) NIL NIL
         NIL NIL (BEG-SL S) (END-SL S) NIL (S) (BEG-SL S A) (S) (END-SL)
         NIL NIL NIL NIL (BEG-SL S) (END-SL S) NIL (S) 
         (BEG-SL S A) (S) (END-SL) NIL NIL NIL NIL))
      (auto-slur mini-1 'vn)
      (not (next-event mini-1 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini-1 'vn)
          while ne
          collect (marks ne))
       '((BEG-SL) (END-SL) NIL (BEG-SL) (A) NIL NIL (END-SL) NIL
         (BEG-SL) NIL NIL (END-SL) NIL (BEG-SL) (A) NIL NIL (END-SL) NIL
         (BEG-SL) NIL NIL (END-SL) NIL (BEG-SL) (A) NIL NIL (END-SL) NIL
         NIL NIL)) 
      (auto-slur mini-2 '(vn vc)
                 :start-bar 2
                 :end-bar 3)
      (not (next-event mini-2 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini-2 'vn)
          while ne
          collect (marks ne))
       '((BEG-SL S) (END-SL S) NIL (S) (BEG-SL S A) (S) (END-SL) NIL NIL
         NIL NIL (BEG-SL) (END-SL) NIL (BEG-SL) (A) NIL NIL (END-SL) NIL
         (BEG-SL) NIL NIL (END-SL) NIL (BEG-SL) (A) NIL NIL (END-SL) NIL
         NIL NIL))
      (not (next-event mini-2 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini-2 'vc)
          while ne
          collect (marks ne))
       '((BEG-SL S) (END-SL S) NIL (S) (BEG-SL S A) (S) (END-SL) NIL NIL
         NIL NIL NIL NIL NIL (BEG-SL) (END-SL A) (S) NIL NIL NIL NIL
         (BEG-SL) (END-SL) (S) NIL (BEG-SL) (END-SL A) (S) NIL NIL NIL
         NIL NIL)) 
      (auto-slur mini-3 'vn
                 :rm-slurs-first nil
                 :rm-staccatos nil)
      (not (next-event mini-3 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini-3 'vn)
          while ne
          collect (marks ne))
       '((BEG-SL S) (END-SL S) NIL (BEG-SL S) (BEG-SL S A) (S) (END-SL)
         (END-SL) NIL NIL NIL (BEG-SL S) (END-SL S) NIL (BEG-SL S)
         (BEG-SL S A) (S) (END-SL) (END-SL) NIL NIL NIL (BEG-SL S)
         (END-SL S) NIL (BEG-SL S) (BEG-SL S A) (S) (END-SL) (END-SL)
         NIL NIL NIL)) 
      (auto-slur mini-4 'vn
                 :over-accents nil)
      (not (next-event mini-4 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini-4 'vn)
          while ne
          collect (marks ne))
       '((BEG-SL) (END-SL) NIL NIL (S A) (BEG-SL) NIL (END-SL) NIL
         (BEG-SL) NIL NIL (END-SL) NIL NIL (S A) (BEG-SL) NIL (END-SL)
         NIL (BEG-SL) NIL NIL (END-SL) NIL NIL (S A) (BEG-SL) NIL
         (END-SL) NIL NIL NIL)))))


;;; SAR Mon Jun 18 12:34:46 BST 2012
(sc-deftest test-sc-edit-copy-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((hn (french-horn :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) h h))
                                  :pitch-seq-palette ((1 2)))))
          :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((hn (2 2 2 2 2))
                              (vc (2 2 2 2 2))))
                          (3 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(C4 H D4 Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4
         S G4 S C4 H D4 Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4 S G4 S C4 H D4 H
         C4 H D4 H C4 H D4 H C4 H D4 H C4 H D4 H C4 H D4 Q E4 E FS4 S G4 S C4 H
         D4 Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4 S G4
         S C4 H D4 Q E4 E FS4 S G4 S))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S
         A4 S D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S A3 H B3 H A3 H
         B3 H A3 H B3 H A3 H B3 H A3 H B3 H D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q
         F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S D4 H
         E4 Q F4 E G4 S A4 S))
      (copy-bars mini 7 2 'vc 'hn 2 t)
      (not (next-event mini 'hn nil 1))
      (equalp
       (loop for ne = (next-event mini 'hn)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(C4 H D4 Q E4 E FS4 S G4 S E4 H FS4 H E4 H FS4 H C4 H D4 Q E4 E FS4 S
         G4 S C4 H D4 Q E4 E FS4 S G4 S C4 H D4 H C4 H D4 H C4 H D4 H C4 H D4 H
         C4 H D4 H C4 H D4 Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4 S G4 S C4 H D4
         Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4 S G4 S C4 H D4 Q E4 E FS4 S G4
         S))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne)
          collect (data ne))
       '(D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S
         A4 S D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S A3 H B3 H A3 H
         B3 H A3 H B3 H A3 H B3 H A3 H B3 H D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q
         F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S D4 H E4 Q F4 E G4 S A4 S D4 H
         E4 Q F4 E G4 S A4 S)))))

(defun test-copy-bars2-aux (sc from-ins to-ins expected-transp pitches-orig)
  (let (event)
    (sc-test-check
      (copy-bars sc 1 1 from-ins to-ins 1)
      (equalp pitches-orig (get-pitch-symbols (get-bar sc 1 to-ins)))
      (setf event (get-event sc 1 1 to-ins))
      (if expected-transp
          (= expected-transp
             (pitch- (pitch-or-chord event)
                     (written-pitch-or-chord event)))
          t))))

;;; MDE Mon Jun 18 16:33:30 2012
(sc-deftest test-sc-edit-copy-bars2 ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                        (hn (french-horn :midi-channel 2))
                        (vn (violin :midi-channel 3))
                        (vc (cello :midi-channel 4))
                        (pc (piccolo :midi-channel 5))
                        (db (double-bass :midi-channel 6))))
           :set-palette '((1 ((e1 f1 g1 a1 b1
                                  c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3 
                                  c4 d4 e4 f4 g4 a4 b4 
                                  c5 d5 e5 f5 g5 a5 b5
                                  c6 d6 e6 f6 g6 a6 b6
                                  c7 d7 e7 f7 g7 a7 b7 c8))))
           :set-map '((1 (1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q - e s 32 32 -))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (2 ((((4 4) q - e s 32 32 - h))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (3 ((((4 4) - e s 32 32 - h q))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (4 ((((4 4) - s 32 32 - h q e))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (5 ((((4 4) - 32 32 - h q - e s -))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (6 ((((4 4) 32 h q - e s 32 -))
                                   :pitch-seq-palette ((1 2 3 4 5 6)))))
           :rthm-seq-map '((1 ((cl (1 1 1 1))
                               (hn (2 2 2 2))
                               (vn (3 3 3 3))
                               (vc (4 4 4 4))
                               (pc (5 5 5 5))
                               (db (6 6 6 6)))))))
         (cl-bar1 (get-pitch-symbols (get-bar mini 1 'cl))))
    (sc-test-check
      (test-copy-bars2-aux mini 'cl 'hn -7 cl-bar1)
      (test-copy-bars2-aux mini 'cl 'vn nil cl-bar1)
      (test-copy-bars2-aux mini 'cl 'pc 12 cl-bar1)
      (test-copy-bars2-aux mini 'cl 'db -12 cl-bar1)
      ;; now non-transposing to transposing
      (test-copy-bars2-aux mini 'vn 'hn -7 cl-bar1)
      ;; (test-ins 'hn -7)
      ;; (test-ins 'vn nil)
      )))

;;; SAR Wed Aug  8 12:58:39 BST 2012
(sc-deftest test-sc-edit-add-tuplet-bracket-to-bar ()
  (let ((mini
         (make-slippery-chicken
          '+sc-object+
          :ensemble '(((va (viola :midi-channel 2))))
          :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((3 4) (te) - te te - { 3 te ts+ts te } 
                                    - fs fs fs fs fs -))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8 9 8))))) 
          :rthm-seq-map '((1 ((va (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (bracket ne))
       '(NIL NIL NIL ((1 3)) (-1) (-1) (1) NIL NIL NIL NIL NIL NIL NIL NIL
         ((1 3)) (-1) (-1) (1) NIL NIL NIL NIL NIL NIL NIL NIL ((1 3)) 
         (-1) (-1) (1) NIL NIL NIL NIL NIL))
      (add-tuplet-bracket-to-bar mini 1 'va '(3 0 2))
      (add-tuplet-bracket-to-bar mini 2 'va '(5 7 11))
      (add-tuplet-bracket-to-bar mini 3 'va '(3 3 4) t)
      (add-tuplet-bracket-to-bar mini 3 'va '(3 5 6))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (bracket ne))
       '(((2 3)) (-2) (2) ((1 3)) (-1) (-1) (1) NIL NIL NIL NIL NIL NIL NIL 
         NIL ((1 3)) (-1) (-1) (1) ((2 5)) (-2) (-2) (-2) (2) NIL NIL NIL 
         ((1 3)) (1) ((2 3)) (2) NIL NIL NIL NIL NIL)))))

;;; SAR Wed Aug  8 13:15:07 BST 2012
(sc-deftest test-sc-edit-add-tuplet-brackets-to-beats ()
  (let ((mini
         (make-slippery-chicken
          '+sc-object+
          :ensemble '(((va (viola :midi-channel 2))))
          :set-palette '((1 ((c3 e3 g3 c4))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((3 4) - te te te - - fs fs fs fs fs -
                                    - 28 28 28 28 28 28 28 -))
                                  :pitch-seq-palette ((1 2 3 4 1 2 3 4 1 2 3 4
                                                         1 2 3)))))
          :rthm-seq-map '((1 ((va (1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'va nil 1))
      (every #'not
             (loop for ne = (next-event mini 'va)
                while ne
                collect (bracket ne)))
      (not (add-tuplet-brackets-to-beats mini 'va 
                                         '((1 3 0 2) (2 5 3 7) (3 7 8 14))))
      (not (next-event mini 'va nil 1))
      (equalp
       (loop for ne = (next-event mini 'va)
          while ne
          collect (bracket ne))
       '(((1 3)) (-1) (1) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL ((1 5)) (-1) (-1) (-1) (1) NIL NIL NIL NIL NIL NIL NIL 
         NIL NIL NIL NIL NIL NIL NIL NIL ((1 7)) (-1) (-1) (-1) (-1) (-1) 
         (1))))))

;;; SAR Thu Oct  4 17:09:00 BST 2012
(sc-deftest test-sc-edit-add-pitches-to-chord ()
  (let* ((ip-clone (clone +slippery-chicken-standard-instrument-palette+)))
    (set-slot 'chord-function 'chord-fun1 'guitar ip-clone)
    (probe-delete-multi "/tmp/"
                        '("slippery-chicken-piece.mid" #+cmn "cmn.eps"
                          "_slippery-chicken-piece-score.ly"
                          "slippery-chicken-piece-def.ly" 
                          "slippery-chicken-piece-gtr-part.ly"
                          "slippery-chicken-piece-gtr.ly"))
    (let* ((mini
            (make-slippery-chicken
             '+mini+
             :instrument-palette ip-clone
             :ensemble '(((gtr (guitar :midi-channel 1))))
             :set-palette '((1 ((e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5
                                    f5 g5 a5 b5 c6 d6 e6))))
             :set-map '((1 (1)))
             :rthm-seq-palette 
             '((1 ((((4 4) e e e e e e e e))
                   :pitch-seq-palette ((1 (2) 3 (4) 5 (6) 7 (8))))))
             :rthm-seq-map '((1 ((gtr (1))))))))
      (sc-test-check
        (equalp (get-pitch-symbols 
                 (pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e3 g3 b3))
        (equalp (get-pitch-symbols 
                 (written-pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e4 g4 b4)) 
        (add-pitches-to-chord mini 'gtr 1 2 'cs4 'ds4)
#|        (equalp (get-pitch-symbols 
                 (pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e3 g3 b3 cs4 ds4))    |#
        (equalp (get-pitch-symbols 
                 (written-pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e4 g4 b4 cs5 ds5)) 
        (midi-play mini)
        (file-write-ok "/tmp/slippery-chicken-piece.mid" 200)
        #+cmn (cmn-display mini)
        #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 10000)
        (write-lp-data-for-all mini)
        (file-write-ok-multi "/tmp/" 
                             '("_slippery-chicken-piece-score.ly"
                               "slippery-chicken-piece-def.ly"
                               "slippery-chicken-piece-gtr-part.ly"
                               "slippery-chicken-piece-gtr.ly")
                             '(190 670 200 220))))))

;;; SAR Fri Oct  5 13:54:01 BST 2012
(sc-deftest test-sc-edit-rm-pitches-from-chord ()
  (let* ((ip-clone (clone +slippery-chicken-standard-instrument-palette+)))
    (set-slot 'chord-function 'chord-fun2 'guitar ip-clone)
    (probe-delete-multi "/tmp/"
                        '("slippery-chicken-piece.mid" #+cmn "cmn.eps"
                          "_slippery-chicken-piece-score.ly"
                          "slippery-chicken-piece-def.ly" 
                          "slippery-chicken-piece-gtr-part.ly"
                          "slippery-chicken-piece-gtr.ly"))
    (let* ((mini
            (make-slippery-chicken
             '+mini+
             :instrument-palette ip-clone
             :ensemble '(((gtr (guitar :midi-channel 1))))
             :set-palette '((1 ((e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5
                                    f5 g5 a5 b5 c6 d6 e6))))
             :set-map '((1 (1)))
             :rthm-seq-palette 
             '((1 ((((4 4) e e e e e e e e))
                   :pitch-seq-palette ((1 (2) 3 (4) 5 (6) 7 (8))))))
             :rthm-seq-map '((1 ((gtr (1))))))))
      (sc-test-check
        (equalp (get-pitch-symbols 
                 (pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e3 a3 d4 g4))
        (equalp (get-pitch-symbols 
                 (written-pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e4 a4 d5 g5)) 
        (rm-pitches-from-chord mini 'gtr 1 2 'a3 'd4)
        #|(equalp (get-pitch-symbols 
                 (pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e3 g4))    |#
        (equalp (get-pitch-symbols 
                 (written-pitch-or-chord (get-event mini 1 2 'gtr)))
                '(e4 g5)) 
        (midi-play mini)
        (file-write-ok "/tmp/slippery-chicken-piece.mid" 200)
        #+cmn (cmn-display mini)
        #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 9000)
        (write-lp-data-for-all mini)
        (file-write-ok-multi "/tmp/" 
                             '("_slippery-chicken-piece-score.ly"
                               "slippery-chicken-piece-def.ly"
                               "slippery-chicken-piece-gtr-part.ly"
                               "slippery-chicken-piece-gtr.ly")
                             '(190 670 200 220))))))

;;; MDE Sat Apr 20 13:06:33 2013 
(sc-deftest test-sc-edit-bars-to-sc ()
  (flet ((loads-of-bars (pitch)
           (loop repeat 100 collect
                (make-rthm-seq-bar 
                 (cons '(3 16)
                       (loop repeat 3 collect
                            (make-event pitch 's :midi-channel 1)))))))
    (let ((sc (bars-to-sc (loads-of-bars 'c4))))
      (bars-to-sc (loads-of-bars 'e4) :sc sc :player 'player-two
                  :instrument 'oboe)
      ;; (get-section-refs sc 1 1)
      ;; (link-named-objects (piece sc))
      (sc-test-check
        (midi-play sc)
        (file-write-ok "/tmp/slippery-chicken-piece.mid" 4000)
        #+cmn (cmn-display sc)
        #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 250000)
        (write-lp-data-for-all sc)
        (file-write-ok-multi "/tmp/" 
                             '("_slippery-chicken-piece-score.ly"
                               "slippery-chicken-piece-def.ly"
                               ;; LP has problem with hyphen so we remove them
                               ;; from player names :/ 
                               "slippery-chicken-piece-playerone-part.ly"
                               "slippery-chicken-piece-playerone.ly"
                               "slippery-chicken-piece-playertwo-part.ly"
                               "slippery-chicken-piece-playertwo.ly")
                             '(190 900 200 2900 200 2900))))))

;;; MDE Fri Jan 10 10:00:50 2020
(sc-deftest test-sc-edit-bars-to-sc-wiki ()
  (let* ((chord '(c1 a1 ds2 cs3 g3 d4 f4 bf4 e5 b5 gs6 fs7))
         (chord-len (length chord))
         ;; generate enough 32nd-note events to fill 16 4/4 bars
         (events (loop repeat (* 16 32) collect
                      (make-event (nth (random chord-len) chord) 32)))
         (bars '())
         (ate 0)
         bar
         sc)
    (loop while events do
         (setq bar (make-rthm-seq-bar '((4 4))) ; make an empty bar
               ;; fill the bar with the events we made. This method will stop
               ;; once the bar is full and will return the number of
               ;; rhythms/events it 'ate'.
               ate (fill-with-rhythms bar events)
               ;; ate should always be 32 but best to check
               events (when ate (nthcdr ate events)))
       ;; we could reverse this after the loop if order was important
         (push bar bars))
    ;; automatically create a slippery-chicken object with the bars we made
    (sc-test-check
      (setq sc (bars-to-sc bars))
      (midi-play sc)
      (cmn-display sc)
      sc)))

;;; MDE Thu Dec  6 14:05:53 2018 
(sc-deftest test-mallet-chord-funs ()
  (let ((c1 (init-pitch-list '(g3 b3 c4 e4)))
        (c2 (init-pitch-list '(g3 b3 c4 e5)))
        (c3 (init-pitch-list '(g3 b3)))
        (c4 (init-pitch-list '(g3)))
        (c5 (init-pitch-list '(g3 e4 c5 b5)))
        (c6 (init-pitch-list '(g3 b3 e4 fs4 c5 d5 f5 b5))))
    (sc-test-check
      (chord-equal (make-chord '(g3 b3))
                   (mallet-chord-fun-narrow nil 0 c1 nil nil nil))
      (chord-equal (make-chord '(g3 c4))
                   (mallet-chord-fun nil 0 c1 nil nil nil))
      (chord-equal (make-chord '(g3 b3 c4 e4))
                   (mallet-chord-fun-both-hands nil 0 c1 nil nil nil))
      (chord-equal (make-chord '(g3 b3 c4))
                   (mallet-chord-fun-both-hands nil 0 c2 nil nil nil))
      (chord-equal (make-chord '(g3 b3))
                   (mallet-chord-fun-both-hands nil 0 c3 nil nil nil))
      (pitch= (make-pitch 'g3)
              (mallet-chord-fun-both-hands nil 0 c4 nil nil nil))
      (chord-equal (make-chord '(g3 e4))
                   (mallet-chord-fun-both-hands nil 0 c5 nil nil nil))
      ;;; MDE Thu Dec  6 15:14:15 2018 -- try piano version too
      (chord-equal (make-chord '(g3 b3 e4 fs4 c5 d5 f5 b5))
                   (piano-chord-fun-both-hands nil 0 c6 nil nil nil)))))

;;; MDE Sat Feb 23 12:46:22 2019 -- test dan's code
(sc-deftest test-pause-last ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                        (hn (french-horn :midi-channel 2))
                        (vn (violin :midi-channel 3))
                        (vc (cello :midi-channel 4))
                        (pc (piccolo :midi-channel 5))
                        (db (double-bass :midi-channel 6))))
           :set-palette '((1 ((e1 f1 g1 a1 b1
                                  c2 d2 e2 f2 g2 a2 b2
                                  c3 d3 e3 f3 g3 a3 b3 
                                  c4 d4 e4 f4 g4 a4 b4 
                                  c5 d5 e5 f5 g5 a5 b5
                                  c6 d6 e6 f6 g6 a6 b6
                                  c7 d7 e7 f7 g7 a7 b7 c8))))
           :set-map '((1 (1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q - e s 32 32 -))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (2 ((((4 4) q - e s 32 32 - h))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (3 ((((4 4) - e s 32 32 - h q))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (4 ((((4 4) - s 32 32 - h q e))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (5 ((((4 4) - 32 32 - h q - e s -))
                                   :pitch-seq-palette ((1 2 3 4 5 6))))
                               (6 ((((4 4) 32 h q - e s 32 -))
                                   :pitch-seq-palette ((1 2 3 4 5 6)))))
           :rthm-seq-map '((1 ((cl (1 1 1 1))
                               (hn (2 2 2 2))
                               (vn (3 3 3 3))
                               (vc (4 4 4 4))
                               (pc (5 5 5 5))
                               (db (6 6 6 6))))))))
    (sc-test-check
      (not (has-mark (get-last-event (get-bar mini 3  'vc)) 'pause))
      (not (has-mark (get-last-event (get-bar mini 4  'pc)) 'pause))
      (= (num-players mini) (pause-last mini :bar-num 2 :bar-line
                                        'begin-repeat))
      (= 3 (bar-line-type (get-bar mini 2 'pc)))
      (= (num-players mini) (pause-last mini :bar-num 3 :pause 'long-pause))
      (has-mark (get-last-event (get-bar mini 3 'vc)) 'long-pause)
      (= (num-players mini) (pause-last mini))
      (every #'(lambda (x) (eq x 'pause))
             (loop for i in (players mini) collect
                  (first (has-mark (get-last-event (get-bar mini 4 i))
                                   'pause)))))))

;;; MDE Fri Jan 10 16:20:51 2020 -- using Dan's new code for generating new
;;; sections  
(sc-deftest test-bars-to-sc-with-section-id ()
  (declare (special *auto*))
  (let* ((ev-list (loop repeat 4 collect (make-event 'a4 'q)))
         (ev-list2 (loop repeat 4 collect (make-event 'c5 'q)))
         (bar (make-rthm-seq-bar '((4 4))))
         (bar2 (make-rthm-seq-bar '((4 4))))
         tmp)
    (sc-test-check
      (fill-with-rhythms bar ev-list)
      (fill-with-rhythms bar2 ev-list2)
      (bars-to-sc (loop repeat 4 collect bar))
      (bars-to-sc (loop repeat 4 collect bar2) :sc *auto* :section-id 2)
      (update-slots *auto*)
      ;; with just one section we should have 252 bytes, with both 460
      ;; MDE Thu Sep 24 19:03:16 2020, Heidhausen -- we've now attached tempo
      ;; changes so file sizes are a little bigger: 280, 516 bytes   
      (setq tmp (nth-value
                 1 (file-write-ok (midi-play *auto* :num-sections 1) 200)))
      (integer-between tmp 200 300)
      (setq tmp (nth-value 1 (file-write-ok (midi-play *auto*) 400)))
      (integer-between tmp 450 550))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rthm-seq-map tests
;;; SAR Thu Apr 26 18:19:12 BST 2012
(sc-deftest test-rthm-seq-map-make-rthm-seq-map ()
  (let ((rsm1 (make-rthm-seq-map 
               'rsm-test-1
               '((1 ((vn (1 2 3 4 5))
                     (va (2 3 4 5 1))
                     (vc (3 4 5 1 2))))
                 (2 ((vn (6 7 8))
                     (va (7 8 6))
                     (vc (8 6 7)))) 
                 (3 ((vn (9))
                     (va (9))
                     (vc (9)))))))
        (rsm2 (make-rthm-seq-map 
               'rsm-test-2
               '((1 ((vn (1 2 3 4 5))
                     (va (2 3 4 5 1))
                     (vc (3 4 5 1 2)))) 
                 (2 ((vn (6 7 8))
                     (va (7 8 6))
                     (vc (8 6 7)))) 
                 (3 ((vn (9))
                     (va (9))
                     (vc (9)))))
               :replacements '(((1 vn) 2 7)
                               ((2 va) 3 1)
                               ((3 vc) 1 0))))
        (rsm3 (make-rthm-seq-map 
               'rsm-test-3
               '((1 ((a (("i" "ii") ("iii" "iv")))
                     (b (("i" "ii") ("iii" "iv")))
                     (c (("i" "ii") ("iii" "iv"))))) 
                 (2 ((a (("i" "ii") ("iii" "iv")))
                     (b (("i" "ii") ("iii" "iv")))
                     (c (("i" "ii") ("iii" "iv"))))) 
                 (3 ((a (("i" "ii") ("iii" "iv")))
                     (b (("i" "ii") ("iii" "iv")))
                     (c (("i" "ii") ("iii" "iv"))))))
               :recurse-simple-data t))
        (rsm4 (make-rthm-seq-map 
               'rsm-test-4
               '((1 ((a (("i" "ii") ("iii" "iv")))
                     (b (("i" "ii") ("iii" "iv")))
                     (c (("i" "ii") ("iii" "iv"))))) 
                 (2 ((a (("i" "ii") ("iii" "iv")))
                     (b (("i" "ii") ("iii" "iv")))
                     (c (("i" "ii") ("iii" "iv"))))) 
                 (3 ((a (("i" "ii") ("iii" "iv")))
                     (b (("i" "ii") ("iii" "iv")))
                     (c (("i" "ii") ("iii" "iv"))))))
               :recurse-simple-data nil))
        (rsm5 (make-rthm-seq-map 
               'rsm-test-5
               '((sec1 ((vn (rs1 rs3 rs2))
                        (va (rs2 rs3 rs1))
                        (vc (rs3 rs1 rs2))))
                 (sec2 ((vn (rs1 rs2 rs1))
                        (va (rs2 rs1 rs3))
                        (vc (rs1 rs3 rs3))))
                 (sec3 ((vn (rs1 rs1 rs3))
                        (va (rs1 rs3 rs2))
                        (vc (rs3 rs2 rs3)))))
               :palette (make-rsp 
                         'rs-pal
                         '((rs1 ((((2 4) q e s s))))
                           (rs2 ((((2 4) e s s q))))
                           (rs3 ((((2 4) s s q e)))))))))
    (sc-test-check
      ;; MDE Sat Jan 27 19:24:22 2018
      (= 9 (num-sequences rsm2))
      ;; MDE Tue Feb 13 14:49:22 2018
      (= 10 (count-ref rsm5 'rs1))
      (rthm-seq-map-p rsm1)
      (equalp
       (loop for i in (data rsm1) 
          collect (id i)
          collect (loop for j in (data (data i)) 
                     collect (id j)
                     collect (data j)))
       '(1 (VN (1 2 3 4 5) 
            VA (2 3 4 5 1) 
            VC (3 4 5 1 2)) 
         2 (VN (6 7 8) 
            VA (7 8 6) 
            VC (8 6 7)) 
         3 (VN (9) 
            VA (9) 
            VC (9))))
      (equalp 
       (loop for i in (data rsm2) 
          collect (id i)
          collect (loop for j in (data (data i)) 
                     collect (id j)
                     collect (data j)))
       '(1 (VN (1 7 3 4 5) 
            VA (2 3 4 5 1) 
            VC (3 4 5 1 2)) 
         2 (VN (6 7 8) 
            VA (7 8 1) 
            VC (8 6 7)) 
         3 (VN (9) 
            VA (9) 
            VC (0))))
      (equalp
       (get-all-refs rsm3)
       '((1 A "i") (1 A "iii") (1 B "i") (1 B "iii") (1 C "i") (1 C "iii")
         (2 A "i") (2 A "iii") (2 B "i") (2 B "iii") (2 C "i") (2 C "iii")
         (3 A "i") (3 A "iii") (3 B "i") (3 B "iii") (3 C "i") (3 C "iii")))  
      (equalp
       (get-all-refs rsm4)
       '((1 A "i") (1 A "iii") (1 B "i") (1 B "iii") (1 C "i") (1 C "iii")
         (2 A "i") (2 A "iii") (2 B "i") (2 B "iii") (2 C "i") (2 C "iii")
         (3 A "i") (3 A "iii") (3 B "i") (3 B "iii") (3 C "i") (3 C "iii"))) 
      (equalp
       (loop for cs in (get-all-data-from-palette rsm5)
          collect (data cs))
       '(((((2 4) Q E S S))) ((((2 4) S S Q E))) ((((2 4) E S S Q)))
         ((((2 4) E S S Q))) ((((2 4) S S Q E))) ((((2 4) Q E S S)))
         ((((2 4) S S Q E))) ((((2 4) Q E S S))) ((((2 4) E S S Q)))
         ((((2 4) Q E S S))) ((((2 4) E S S Q))) ((((2 4) Q E S S)))
         ((((2 4) E S S Q))) ((((2 4) Q E S S))) ((((2 4) S S Q E)))
         ((((2 4) Q E S S))) ((((2 4) S S Q E))) ((((2 4) S S Q E)))
         ((((2 4) Q E S S))) ((((2 4) Q E S S))) ((((2 4) S S Q E)))
         ((((2 4) Q E S S))) ((((2 4) S S Q E))) ((((2 4) E S S Q)))
         ((((2 4) S S Q E))) ((((2 4) E S S Q))) ((((2 4) S S Q E))))))))

;;; SAR Fri Apr 27 13:36:28 BST 2012
(sc-deftest test-rthm-seq-map-get-map-refs ()
  (let ((rsmt (make-rthm-seq-map 
               'rsm-test-5
               '((sec1 ((vn (rs1 rs3 rs2))
                        (va (rs2 rs3 rs1))
                        (vc (rs3 rs1 rs2))))
                 (sec2 ((vn (rs1 rs2 rs1))
                        (va (rs2 rs1 rs3))
                        (vc (rs1 rs3 rs3))))
                 (sec3 ((vn (rs1 rs1 rs3))
                        (va (rs1 rs3 rs2))
                        (vc (rs3 rs2 rs3)))))
               :palette (make-rsp 
                         'rs-pal
                         '((rs1 ((((2 4) q e s s))))
                           (rs2 ((((2 4) e s s q))))
                           (rs3 ((((2 4) s s q e)))))))))
    (sc-test-check
      (equalp (get-map-refs rsmt 'sec1 'vn) '(rs1 rs3 rs2))
      (equalp (get-map-refs rsmt 'sec2 'va) '(rs2 rs1 rs3))
      (equalp (get-map-refs rsmt 'sec3 'vc) '(rs3 rs2 rs3)))))

;;; SAR Fri Apr 27 13:45:58 BST 2012
(sc-deftest test-rthm-seq-set-map-refs ()
  (let ((rsmt (make-rthm-seq-map 
               'rsm-test-5
               '((sec1 ((vn (rs1 rs3 rs2))
                        (va (rs2 rs3 rs1))
                        (vc (rs3 rs1 rs2))))
                 (sec2 ((vn (rs1 rs2 rs1))
                        (va (rs2 rs1 rs3))
                        (vc (rs1 rs3 rs3))))
                 (sec3 ((vn (rs1 rs1 rs3))
                        (va (rs1 rs3 rs2))
                        (vc (rs3 rs2 rs3)))))
               :palette (make-rsp 
                         'rs-pal
                         '((rs1 ((((2 4) q e s s))))
                           (rs2 ((((2 4) e s s q))))
                           (rs3 ((((2 4) s s q e)))))))))
    (sc-test-check
      (equalp (get-map-refs rsmt 'sec2 'vc) '(rs1 rs3 rs3))
      (set-map-refs rsmt 'sec2 'vc '(rs2 rs3 rs2))
      (equalp (get-map-refs rsmt 'sec2 'vc) '(rs2 rs3 rs2)))))

;;; SAR Fri Apr 27 14:30:13 BST 2012
(sc-deftest test-rthm-seq-map-check-num-sequences ()
  (sc-test-check
    (check-num-sequences 
     (make-rthm-seq-map 
      'rsm-test-1
      '((sec1 ((vn (rs1a rs3a rs2a))
               (va (rs1b rs3b rs2b))
               (vc (rs1a rs3b rs2a))))
        (sec2 ((vn (rs1a rs2a rs1a))
               (va (rs1a rs2a rs1b))
               (vc (rs1a rs2b rs1a))))
        (sec3 ((vn (rs1a rs1a rs3a))
               (va (rs1a rs1a rs3b))
               (vc (rs1a rs1b rs3a))))
        (sec4 ((vn (rs1a rs1a rs1a))
               (va (rs1a rs1a rs1b))
               (vc (rs1a rs1b rs1a)))))))
    (and
     (not (ignore-errors 
            (make-rthm-seq-map 
             'rsm-test
             '((sec1 ((vn (rs1a rs3a rs2a))
                      (va (rs1b rs3b))
                      (vc (rs1a rs3b rs2a))))
               (sec2 ((vn (rs1a))
                      (va (rs1a rs2a rs1b))
                      (vc (rs1a rs2b rs1a))))
               (sec3 ((vn (rs1a rs3a))
                      (va (rs1a))
                      (vc (rs1a rs1b rs3a))))
               (sec4 ((vn (rs1a))
                      (va (rs1a rs1a rs1b))
                      (vc (rs1a rs1a))))))))
     (nth-value 1 (ignore-errors 
                    (make-rthm-seq-map 
                     'rsm-test
                     '((sec1 ((vn (rs1a rs3a rs2a))
                              (va (rs1b rs3b))
                              (vc (rs1a rs3b rs2a))))
                       (sec2 ((vn (rs1a))
                              (va (rs1a rs2a rs1b))
                              (vc (rs1a rs2b rs1a))))
                       (sec3 ((vn (rs1a rs3a))
                              (va (rs1a))
                              (vc (rs1a rs1b rs3a))))
                       (sec4 ((vn (rs1a))
                              (va (rs1a rs1a rs1b))
                              (vc (rs1a rs1a)))))))))))

;;; SAR Fri Apr 27 16:21:06 BST 2012
(sc-deftest test-rthm-seq-map-rsm-count-notes ()
  (let ((rsmt (make-rthm-seq-map 
               'rsm-test
               '((sec1 ((vn (rs1 rs3 rs2))
                        (va (rs2 rs3 rs1))
                        (vc (rs3 rs1 rs2))))
                 (sec2 ((vn (rs1 rs2 rs1))
                        (va (rs2 rs1 rs3))
                        (vc (rs1 rs3 rs3))))
                 (sec3 ((vn (rs1 rs1 rs3))
                        (va (rs1 rs3 rs2))
                        (vc (rs3 rs2 rs3)))))))
        (rspt (make-rsp 
               'rs-pal
               '((rs1 ((((2 4) q (e) s s))))
                 (rs2 ((((2 4) e +s (s) q))))
                 (rs3 ((((2 4) (s) s +q e))))))))
    (sc-test-check
      ;; MDE Sat Jan 27 19:22:45 2018
      (= 9 (num-sequences rsmt))
      (= 23 (rsm-count-notes rsmt 'vn rspt))
      (= 27 (rsm-count-notes rsmt 'va rspt nil)))))

;;; SAR Wed Jun 13 12:59:57 BST 2012
;;; MDE Mon Jun 10 22:37:24 2013 -- todo: need a test that uses
;;; :repeat-rest-seqs nil  
(sc-deftest test-rthm-seq-map-add-repeats ()
  (let* ((mrsm-1
          (make-rthm-seq-map 
           'rsm-test
           '((1 ((vn (1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1)))))
           :palette (make-rsp 
                     'rs-pal
                     '((1 ((((2 4) q e s s))))
                       (2 ((((2 4) e s s q))))
                       (3 ((((2 4) s s q e))))))))
         (mrsm-2 (clone mrsm-1)))
    (sc-test-check
      (equalp (get-data-data '(1 vn) mrsm-1)
              '(1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1))
      (add-repeats mrsm-1 '((1 6) (2 6)) '((11 6) (23 3)))
      (equalp (get-data-data '(1 vn) mrsm-1) 
              '(1 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2
                2 2 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1
                1 1 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 1 1 1
                1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1
                1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 3 2 2 2 2 2 2 2 2
                2 2 2 1))
      (equalp (get-data-data '(1 vn) mrsm-2)
              '(1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1))
      (add-repeats mrsm-2 '((1 6) (2 6)) '((11 6) (23 3)) :start 3 :end 11)
      (equalp (get-data-data '(1 vn) mrsm-2)
              '(1 2 3 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3
                3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2
                2 2 2 2 2 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 3
                1 3 2 1)))))

;;; SAR Wed Jun 13 13:42:27 BST 2012
(sc-deftest test-rthm-seq-map-add-repeats-simple ()
  (let ((mrsm
         (make-rthm-seq-map 
          'rsm-test
          '((1 ((vn (1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1)))))
          :palette (make-rsp 
                    'rs-pal
                    '((1 ((((2 4) q e s s))))
                      (2 ((((2 4) e s s q))))
                      (3 ((((2 4) s s q e)))))))))
    (sc-test-check
      (equalp (get-data-data '(1 vn) mrsm)
              '(1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1))
      (add-repeats-simple mrsm 3 13)
      (equalp (get-data-data '(1 vn) mrsm)
              '(1 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 1 3 1 3 2 3 1 2 1 3 1 3
                2 1)))))

;;; MDE Mon Apr 21 16:36:05 2014 -- combine test
(sc-deftest test-rthm-seq-map-combine ()
  (let* ((rsm1 (make-rthm-seq-map 'rsm1 '((1 ((bass (1 1 1 2 1 1 1 2)))))))
         (rsm2 (make-rthm-seq-map 'rsm2 '((2 ((bass (1 2 1 1 1 2 1 1)))))))
         (mini
          (make-slippery-chicken 
           '+mini+ 
           :title "Sing a Song of Songs - Kenny Garret" 
           :tempo-map '((1 (q 170))) 
           :ensemble '(((bass (double-bass :midi-channel 3)))) 
           :set-limits-high '((bass (0 a3 100 a3))) 
           :set-limits-low '((bass (0 c1 100 c1))) 
           :set-palette '((1 ((e1 b1 gs2))) 
                          (2 ((c2 g2 e3))) 
                          (3 ((d2 a2 fs3))) 
                          (4 ((e1 b1 e2)))) 
           :set-map '((1 (1 2 3 4 1 2 3 4))
                      (2 (1 2 3 4 1 2 3 4)))
           :rthm-seq-palette '((1 ((((4 4) 4\. 4\. 4) 
                                    (+q 4\. 4\.)) 
                                   :pitch-seq-palette ((1 2 3 2 1)))) 
                               (2 ((((4 4) 4\. 4\. 4) 
                                    (+w)) 
                                   :pitch-seq-palette ((1 2 3))))) 
           :rthm-seq-map (combine rsm1 rsm2))))
    (sc-test-check 
      (= 2 (get-num-sections mini))
      ;; (print (duplicate-structure (rthm-seq-map mini)))
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-map tests

;;; SAR Sun Mar  4 11:27:03 GMT 2012: Replaced ut
(sc-deftest test-sc-map-make-sc-map ()
  (let ((mscm1 (make-sc-map 'scm-test
                            '((1
                               ((vn (1 2 3 4 5))
                                (va (2 3 4 5 1))
                                (vc (3 4 5 1 2))))
                              (2
                               ((vn (6 7 8))
                                (va (7 8 6))
                                (vc (8 6 7)))) 
                              (3
                               ((vn (9))
                                (va (9))
                                (vc (9)))))))
        (mscm2 (make-sc-map 'scm-test
                            '((1
                               ((vn (1 2 3 4 5))
                                (va (2 3 4 5 1))
                                (vc (3 4 5 1 2)))) 
                              (2
                               ((vn (6 7 8))
                                (va (7 8 6))
                                (vc (8 6 7)))) 
                              (3
                               ((vn (9))
                                (va (9))
                                (vc (9)))))
                            :replacements '(((1 vn) 2 7)
                                            ((2 va) 3 1)
                                            ((3 vc) 1 0))))
        (mscm3 (make-sc-map 'scm-test
                            '((1
                               ((a ((i ii) (iii iv)))
                                (b ((i ii) (iii iv)))
                                (c ((i ii) (iii iv))))) 
                              (2
                               ((a ((i ii) (iii iv)))
                                (b ((i ii) (iii iv)))
                                (c ((i ii) (iii iv))))) 
                              (3
                               ((a ((i ii) (iii iv)))
                                (b ((i ii) (iii iv)))
                                (c ((i ii) (iii iv))))))
                            :recurse-simple-data t))
        (mscm4 (make-sc-map 'scm-test
                            '((1
                               ((a ((i ii) (iii iv)))
                                (b ((i ii) (iii iv)))
                                (c ((i ii) (iii iv))))) 
                              (2
                               ((a ((i ii) (iii iv)))
                                (b ((i ii) (iii iv)))
                                (c ((i ii) (iii iv))))) 
                              (3
                               ((a ((i ii) (iii iv)))
                                (b ((i ii) (iii iv)))
                                (c ((i ii) (iii iv))))))
                            :recurse-simple-data nil))
        (mscm5 (make-sc-map 'sc-m 
                            '((sec1
                               ((vn (set1 set3 set2))
                                (va (set2 set3 set1))
                                (vc (set3 set1 set2))))
                              (sec2
                               ((vn (set1 set2 set1))
                                (va (set2 set1 set3))
                                (vc (set1 set3 set3))))
                              (sec3
                               ((vn (set1 set1 set3))
                                (va (set1 set3 set2))
                                (vc (set3 set2 set3)))))
                            :palette (make-set-palette 
                                      'set-pal 
                                      '((set1 ((c2 b2 a3 g4 f5 e6)))
                                        (set2 ((d2 c3 b3 a4 g5 f6)))
                                        (set3 ((e2 d3 c4 b4 a5 g6))))))))
    (sc-test-check
      (sc-map-p mscm1)
      (= 3 (count-ref mscm1 1))
      (= 3 (count-ref mscm1 9))
      (= 10 (count-ref mscm5 'set1))
      (equalp (loop for i in (data mscm1) 
                 collect (id i)
                 collect (loop for j in (data (data i)) 
                            collect (id j)
                            collect (data j)))
              '(1 (VN (1 2 3 4 5) 
                   VA (2 3 4 5 1) 
                   VC (3 4 5 1 2)) 
                2 (VN (6 7 8) 
                   VA (7 8 6) 
                   VC (8 6 7)) 
                3 (VN (9) 
                   VA (9) 
                   VC (9))))
      (equalp (loop for i in (data mscm2) 
                 collect (id i)
                 collect (loop for j in (data (data i)) 
                            collect (id j)
                            collect (data j)))
              '(1 (VN (1 7 3 4 5) 
                   VA (2 3 4 5 1) 
                   VC (3 4 5 1 2)) 
                2 (VN (6 7 8) 
                   VA (7 8 1) 
                   VC (8 6 7)) 
                3 (VN (9) 
                   VA (9) 
                   VC (0))))
      (equalp (get-all-refs mscm3)
              '((1 A I) (1 A III) (1 B I) (1 B III) (1 C I) (1 C III) (2 A I)  
                (2 A III) (2 B I) (2 B III) (2 C I) (2 C III) (3 A I) (3 A III) 
                (3 B I) (3 B III) (3 C I) (3 C III)))
      (equalp (get-all-refs mscm4)
              '((1 A) (1 B) (1 C) (2 A) (2 B) (2 C) (3 A) (3 B) (3 C)))
      (equalp
       (loop for cs in (get-all-data-from-palette mscm5)
          collect (pitch-list-to-symbols (data cs)))
       '((C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6)
         (D2 C3 B3 A4 G5 F6) (E2 D3 C4 B4 A5 G6) (C2 B2 A3 G4 F5 E6)
         (E2 D3 C4 B4 A5 G6) (C2 B2 A3 G4 F5 E6) (D2 C3 B3 A4 G5 F6)
         (C2 B2 A3 G4 F5 E6) (D2 C3 B3 A4 G5 F6) (C2 B2 A3 G4 F5 E6)
         (D2 C3 B3 A4 G5 F6) (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6)
         (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (E2 D3 C4 B4 A5 G6)
         (C2 B2 A3 G4 F5 E6) (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6)
         (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6)
         (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6) (E2 D3 C4 B4 A5 G6))))))

;;; SAR Sun Mar  4 10:53:47 GMT 2012: Replaced invalid u.t.
(sc-deftest test-sc-map-get-data-from-palette ()
  (let ((sp (make-set-palette 'set-pal '((set1 ((c2 b2 a3 g4 f5 e6)))
                                         (set2 ((d2 c3 b3 a4 g5 f6)))
                                         (set3 ((e2 d3 c4 b4 a5 g6))))))
        (scm (make-sc-map 'sc-m '((sec1
                                   ((vn (set1 set3 set2))
                                    (va (set2 set3 set1))
                                    (vc (set3 set1 set2))))
                                  (sec2
                                   ((vn (set1 set2 set1))
                                    (va (set2 set1 set3))
                                    (vc (set1 set3 set3))))
                                  (sec3
                                   ((vn (set1 set1 set3))
                                    (va (set1 set3 set2))
                                    (vc (set3 set2 set3))))))))
    (sc-test-check
      (equalp 
       (data (get-data-from-palette '(sec1 vn) scm))
       '(SET1 SET3 SET2))
      (bind-palette scm sp)
      (equalp
       (loop for cs in (get-data-from-palette '(sec1 vn) scm)
          collect (pitch-list-to-symbols (data cs)))
       '((C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6))))))

;;; SAR Wed Feb 22 17:16:25 GMT 2012
(sc-deftest test-sc-map-get-nth-from-map ()
  (let ((mscm (make-sc-map 'scm-test
                           '((1
                              ((vn (1 2 3 4 5))
                               (va (2 3 4 5 1))
                               (vc (3 4 5 1 2)))) 
                             (2
                              ((vn (6 7 8))
                               (va (7 8 6))
                               (vc (8 6 7)))) 
                             (3
                              ((vn (9))
                               (va (9))
                               (vc (9))))))))
    (sc-test-check
      (= 2 (get-nth-from-map '(1 vn) 1 mscm))
      (not (get-nth-from-map '(3 vn) 1 mscm)))))

;;; SAR Wed Feb 22 17:31:49 GMT 2012
(sc-deftest test-sc-map-delete-nth-in-map ()
  (let ((mscm (make-sc-map 'scm-test
                           '((1
                              ((vn (1 2 3 4 5))
                               (va (2 3 4 5 1))
                               (vc (3 4 5 1 2)))) 
                             (2
                              ((vn (6 7 8))
                               (va (7 8 6))
                               (vc (8 6 7)))) 
                             (3
                              ((vn (9))
                               (va (9))
                               (vc (9))))))))
    (sc-test-check
      (not (delete-nth-in-map '(1 vn) 1 mscm))
      (equalp (data (get-data-from-palette '(1 vn) mscm)) 
              '(1 NIL 3 4 5)))))

;;; SAR Sat Mar  3 18:54:07 GMT 2012
(sc-deftest test-sc-map-get-all-data-from-palette ()
  (let ((sp (make-set-palette 'set-pal '((set1 ((c2 b2 a3 g4 f5 e6)))
                                         (set2 ((d2 c3 b3 a4 g5 f6)))
                                         (set3 ((e2 d3 c4 b4 a5 g6)))))) 
        (scm (make-sc-map 'sc-m '((sec1
                                   ((vn (set1 set3 set2))
                                    (va (set2 set3 set1))
                                    (vc (set3 set1 set2))))
                                  (sec2
                                   ((vn (set1 set2 set1))
                                    (va (set2 set1 set3))
                                    (vc (set1 set3 set3))))
                                  (sec3
                                   ((vn (set1 set1 set3))
                                    (va (set1 set3 set2))
                                    (vc (set3 set2 set3))))))))
    (sc-test-check
      (not (get-all-data-from-palette scm))
      (bind-palette scm sp)
      (equalp 
       (loop for cs in (get-all-data-from-palette scm)
          collect (pitch-list-to-symbols (data cs)))
       '((C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6)
         (D2 C3 B3 A4 G5 F6) (E2 D3 C4 B4 A5 G6) (C2 B2 A3 G4 F5 E6)
         (E2 D3 C4 B4 A5 G6) (C2 B2 A3 G4 F5 E6) (D2 C3 B3 A4 G5 F6)
         (C2 B2 A3 G4 F5 E6) (D2 C3 B3 A4 G5 F6) (C2 B2 A3 G4 F5 E6)
         (D2 C3 B3 A4 G5 F6) (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6)
         (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (E2 D3 C4 B4 A5 G6)
         (C2 B2 A3 G4 F5 E6) (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6)
         (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6)
         (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6) (E2 D3 C4 B4 A5 G6))))))

;;; SAR Sun Mar  4 11:08:39 GMT 2012
(sc-deftest test-sc-map-get-nth-from-palette ()
  (let ((sp (make-set-palette 'set-pal '((set1 ((c2 b2 a3 g4 f5 e6)))
                                         (set2 ((d2 c3 b3 a4 g5 f6)))
                                         (set3 ((e2 d3 c4 b4 a5 g6))))))
        (scm (make-sc-map 'sc-m '((sec1
                                   ((vn (set1 set3 set2))
                                    (va (set2 set3 set1))
                                    (vc (set3 set1 set2))))
                                  (sec2
                                   ((vn (set1 set2 set1))
                                    (va (set2 set1 set3))
                                    (vc (set1 set3 set3))))
                                  (sec3
                                   ((vn (set1 set1 set3))
                                    (va (set1 set3 set2))
                                    (vc (set3 set2 set3))))))))
    (sc-test-check
      (not (get-nth-from-palette '(sec1 vn) 0 scm))
      (bind-palette scm sp)
      (equalp
       (pitch-list-to-symbols 
        (data (get-nth-from-palette '(sec1 vn) 0 scm)))
       '(C2 B2 A3 G4 F5 E6)))))

;;; SAR Thu Jun 14 18:14:36 BST 2012
(sc-deftest test-sc-map-double ()
  (let ((scm (make-sc-map 'sc-m 
                          '((1
                             ((fl (nil nil nil))
                              (cl (nil nil nil))
                              (vn (set1 set3 set2))
                              (va (set2 set3 set1))
                              (vc (set3 set1 set2))))
                            (2
                             ((vn (set1 set2 set1))
                              (va (set2 set1 set3))
                              (vc (set1 set3 set3))))
                            (3
                             ((vn (set1 set1 set3))
                              (va (set1 set3 set2))
                              (vc (set3 set2 set3))))))))
    (sc-test-check
      (every #'not (get-data-data '(1 fl) scm))
      (every #'not (get-data-data '(1 cl) scm))
      (not (double scm 1 2 3 'vn '(fl cl)))
      (equalp (get-data-data '(1 fl) scm) '(NIL SET3 SET2))
      (equalp (get-data-data '(1 cl) scm) '(NIL SET3 SET2)))))

;;; MDE Thu Jan 18 17:27:07 2018 -- for recursive maps
(sc-deftest test-rthm-seq-map-add-player ()
  (let ((m (make-rthm-seq-map 
            'rmap
            '((sec1 ((vn (rs1 rs3 rs2 rs1))
                     (va (rs2 rs3 rs1 rs3))
                     (vc (rs3 rs1 rs2 rs2))))
              (sec2 ((a ((vn (rs1a rs2a rs1a))
                         (va (rs2a rs1a rs3a))
                         (vc (rs1a rs3a rs3a))))
                     (b ((vn (rs1b rs2b rs1b))
                         (va (rs2b rs1b rs3b))
                         (vc (rs1b rs3b rs3b))))))
              (sec3 ((vn (rs1 rs1))
                     (va (rs1 rs3))
                     (vc (rs3 rs2)))))
            :palette (make-rsp 
                      'rs-pal
                      '((rs1 ((((2 4) q e s s))))
                        (rs2 ((((2 4) e s s q))))
                        (rs3 ((((2 4) s s q e)))))))))
    (sc-test-check
      (add-player m 'cl :data 1)
      (equalp '(1 1 1 1) (get-data-data '(sec1 cl) m))
      (add-player m 'ob :data '(1 2 3 4 5))
      (equalp '(1 2 3 4) (get-data-data '(sec1 ob) m))
      (add-player m 'ob2 :data '(1 2 3))
      (equalp '(1 2 3 nil) (get-data-data '(sec1 ob2) m))
      (add-player m 'ob3 :data '(1 2 3) :cycle t)
      (equalp '(1 2 3 1) (get-data-data '(sec1 ob3) m))
      (add-player m 'bsn :data #'(lambda (rsm section-ref num-refs player)
                                   (ml (case section-ref
                                         (sec1 'rs1)
                                         (sec3 'rs3)
                                         (t 'rs1a))
                                       num-refs)))
      (add-player m 'db)
      (equalp '(nil nil nil nil) (get-data-data '(sec1 db) m))
      (equalp '(nil nil nil) (get-data-data '(sec2 b db) m))
      (equalp '(nil nil) (get-data-data '(sec3 db) m)))))

;;; MDE Fri Aug 24 19:03:07 2018 
(sc-deftest test-sc-map-remove-ref ()
  (let ((scm (make-sc-map 'sc-m 
                          '((1
                             ((fl (nil nil nil))
                              (cl (nil nil nil))
                              (vn (set1 set3 set2))
                              (va (set2 set3 set1))
                              (vc (set3 set1 set2))))
                            (2
                             ((vn (set1 set2 set1))
                              (va (set2 set1 set3))
                              (vc (set1 set3 set3))))
                            (3
                             ((vn (set1 set1 set3))
                              (va (set1 set3 set2))
                              (vc (set3 set2 set3)))))))
        (rsm (make-rthm-seq-map 
              'rmap
              '((sec1 ((vn (rs1 rs3 rs2 rs1))
                       (va (rs2 rs3 rs1 rs3))
                       (vc (rs3 rs1 rs2 rs2))))
                (sec2 ((a ((vn (rs1a rs2a rs1a))
                           (va (rs2a rs1a rs3a))
                           (vc (rs1a rs3a rs3a))))
                       (b ((vn (rs1b rs2b rs1b))
                           (va (rs2b rs1b rs3b))
                           (vc (rs1b rs3b rs3b))))))
                (sec3 ((vn (rs1 rs1))
                       (va (rs1 rs3))
                       (vc (rs3 rs2))))))))
    (sc-test-check
      (remove-ref scm '(2 va) 'set1 'set-r)
      (equalp (get-data-data '(2 va) scm) '(set2 set-r set3))
      (remove-ref scm '(1 vn) 'set1)
      (equalp (get-data-data '(1 vn) scm) '(set3 set2))
      (remove-ref scm '(3 vc) 'set3 '(nil))
      (equalp (print (get-data-data '(3 vc) scm)) '((nil) set2 (nil)))
      (remove-ref rsm '(sec2 a vc) 'rs3a '(rs 3a))
      (equalp (get-data-data '(sec2 a vc) rsm) '(rs1a (rs 3a) (rs 3a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; complete-set tests

;;; SAR Fri Mar  2 14:52:36 GMT 2012
(sc-deftest test-complete-set-make-complete-set ()
  (let* ((mcs1 (make-complete-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5
                                    c6))) 
         (mcs2 (make-complete-set '(d2 f2 a2 e3 g3 b3 d4 gf4 bf4 df5 f5 af5) 
                                  :id 'csset
                                  :subsets '((low (d2 f2 a2)) 
                                             (mid (b3 d4)))
                                  :related-sets '((not-playable (dqs2 eqf3))))) 
         (mcs3 (make-complete-set '(d2 f2 a2 e3 g3 b3 d4 gf4 bf4 df5 f5 af5) 
                                  :transposition 3
                                  :limit-upper 'g5
                                  :limit-lower 'e2))
         ;; MDE Wed Aug 28 19:26:24 2013 -- make sure freqs are retained
         (freqs '(270.0 430.0 441.0 889.0))
         (mcs4 (make-complete-set freqs)))
    (sc-test-check
      ;; MDE Wed Aug 28 19:26:49 2013 
      (equalp freqs (loop for p in (data mcs4) collect (frequency p)))
      ;; MDE Thu Aug 29 10:30:55 2013 -- retains given freqs and extends from
      ;; them? 
      (equalp
       '(99.0d0 110.0d0 270.0d0 430.0d0 441.0d0 889.0d0 1049.0d0 1060.0d0
         1508.0d0) 
       (get-freqs (make-stack 'test '(430 441 889 270) 1 :by-freq t)))
      (equalp (pitch-symbols mcs1)
              '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))
      (not (complete mcs1))
      (= 1 (num-missing-chromatic mcs1))
      (equalp (missing-chromatic mcs1) '(EF))
      (= 0 (transposition mcs1))
      (not (limit-upper mcs1))
      (not (limit-lower mcs1))
      (equalp (loop for nobj in (data (subsets mcs2))
                 collect (pitch-list-to-symbols (data nobj)))
              '((D2 F2 A2) (B3 D4)))
      (equalp (loop for nobj in (data (related-sets mcs2))
                 collect (list
                          (id nobj)
                          (pitch-list-to-symbols (data nobj))))
              '((NOT-PLAYABLE (DQS2 EQF3))))
      ;; MDE Sat Mar 20 12:25:36 2021, Heidhausen
      (chord-equal (make-chord '(d2 f2 a2))
                   (create-chord mcs2 'low))
      (chord-equal (make-chord '(dqs2 eqf3))
                   (create-chord mcs2 nil 'not-playable))
      ;; MDE Fri Jun  2 11:18:45 2017
      (subsets-remove mcs2 'd2 'd4)
      (equalp '(f2 a2) (pitch-list-to-symbols
                        (get-data-data 'low (subsets mcs2))))
      (equalp '(b3) (pitch-list-to-symbols
                     (get-data-data 'mid (subsets mcs2))))
      (equalp (pitch-symbols mcs3)
              '(F2 AF2 C3 G3 BF3 D4 F4 A4 CS5 E5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sc-deftest test-hailstone ()
    (sc-test-check
      (equal (hailstone 6) '(6 3 10 5 16 8 4 2 1))
      (equal (hailstone 11) '(11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Tue Dec 13 01:00:15 2011 -- intra-phrasal looping tests
(sc-deftest test-rs-chop ()
  (let* ((rsa (make-rthm-seq
               '(a ((((2 4) - (s) (32) 32 (s) s - - +s+32 (32) (e) -)
                     ((q) (s) s (e))
                     ((q) (e) (32) 32 (s))
                     ((e) - { 3 48 x 3 } - (s) (e) e)
                     (- (s.) 32 e - - +e e -))
                    :pitch-seq-palette (((2) 8 0  0 0 2 3 (0) -1 11 9)
                                        (5 (4) 4 6 1 3 6 (6) 0 10 2))
                    :marks (as 1 5 6 a 4 te 10 13 14 beg-sl 7 end-sl 9)
                    ))))
         (rsb (make-rthm-seq
               '(b ((((2 4) (q) - 32 32 (32) 32 +e -)
                     ((q) - s (s) s (s) -)
                     (- (s) 32 32 (e) - (e) e)
                     (- s (s) (s) (32) 32 - - 32 32 32 (32) (s) s -)
                     ((32) 32 (s) e (q)))
                    :pitch-seq-palette ((4 0 (6) 0 2 0 3 10 -1 2 0 1 2 6 0 (-4))
                                        (2 5 0 0 1 0 2 8 -1 1 -1 0 1 (5) (-1)
                                           (-2))
                                        ((4 1 (6) (0) (2) (0) 3 (10)
                                            (-1) (2) 0 1 2 (6) (0) (-4))
                                         piano-right-hand piano-left-hand)
                                        ((2 5 (0) (0) (1) (0) 2 (8)
                                            (-1) (1) -1 0 1 (5) (-1) (-2))
                                         piano-right-hand piano-left-hand))))))
         (rsac (chop (scale (clone rsa) 2) nil 'e))
         (rsac1 (first rsac))
         (rsbc (chop rsb))
         (psp (pitch-seq-palette (nth 10 rsbc))))
    (sc-test-check
      (= 100 (length rsac))
      (= 2 (num-notes rsac1))
      (= 3 (num-rests rsac1))
      (= 3 (sclist-length (first (data psp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Dec 13 12:09:21 2011 -- utilities tests
(sc-deftest test-factor () 
  (sc-test-check 
    (handler-case (not (factor 8 0)) (simple-error () 'T)) 
    (not (factor 2 3)) 
    (not (factor 7 3))
    (factor 9 3)
    (factor 0 2) 
    (factor 1 1) 
    (factor 9 1) 
    (factor 9 -1) 
    (factor 16 4) 
    (factor -9 -3) 
    (factor -4 4)))

(sc-deftest test-remove-with-id ()
  (let ((l (loop for i below 100 collect (make-named-object i 'blah)))
        lr)
    (sc-test-check
      (setf lr (remove-with-id l 98))
      (= 99 (length lr))
      (setf l (loop for i below 100 collect
                   (make-named-object (if (= i 50) "dog" i) 'blah))
            lr (remove-with-id l "dog"))
      (= 99 (length lr)))))

(sc-deftest test-partial-freqs ()
  (sc-test-check
    (not (partial-freqs 110 200))
    (partial-freqs 110 220)
    (partial-freqs 110 330)
    (partial-freqs 50.0 10.0)
    (partial-freqs 100 100)))

(sc-deftest test-octave-freqs () 
  (sc-test-check 
    (octave-freqs 110 220)
    (octave-freqs 100 100)
    (not (octave-freqs 110 200))
    (not (octave-freqs 110 330))
    (not (octave-freqs 40 440)) 
    (not (octave-freqs 440 44)) 
    (octave-freqs 44 88) 
    (octave-freqs 440 110) 
    (octave-freqs 55 440)))

;;; MDE Wed Dec 21 16:47:55 2011 -- seems there's a bug in formatting the width
;;; of integers in CCL so don't run this test there 
#-ccl
(sc-deftest test-secs-to-mins-secs ()
  (sc-test-check
    (equal (secs-to-mins-secs 0) "0.000")
    (equal (secs-to-mins-secs 1) "1.000")
    (equal (secs-to-mins-secs 10) "10.000")
    (equal  (secs-to-mins-secs 100) "1:40.000")
    (equal  (secs-to-mins-secs 60 :same-width t) "01:00.000")
    (equal  (secs-to-mins-secs 60.00001) "1:00.000")
    (equal  (secs-to-mins-secs 60.001 :same-width t) "01:00.001")
    (equal  (secs-to-mins-secs 60.00051) "1:00.001")
    (equal  (secs-to-mins-secs 60.000499999) "1:00.000")
    (equal  (secs-to-mins-secs 59.000499999) "59.000")
    (equal  (secs-to-mins-secs 59.00499999) "59.005")
    (equal  (secs-to-mins-secs 60.00499999 :same-width t) "01:00.005")
    (equal  (secs-to-mins-secs 160.00499999) "2:40.005")
    (equal  (secs-to-mins-secs 601) "10:01.000")
    (equal  (secs-to-mins-secs 601 :post-mins '-) "10-01.000")
    (equal  (secs-to-mins-secs 601 :post-secs "s" :post-msecs "ms")
            "10:01s000ms")
    (equal  (secs-to-mins-secs 601 :same-width T) "10:01.000")
    (equal  (secs-to-mins-secs 3599) "59:59.000")))

(sc-deftest test-split-groups ()
  (sc-test-check
    (equal  (split-groups 31 10) '(10 10 10 1))
    (equal  (split-groups 31 5) '(5 5 5 5 5 5 1))
    (equal  (split-groups 31 1) '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                                  1 1 1 1 1 1 1 1 1 1 1))
    (equal  (split-groups 1 1) '(1))
    (equal  (split-groups 10 5) '(5 5))
    (equal  (split-groups 20 4) '(4 4 4 4 4))
    (equal  (split-groups 20 3) '(3 3 3 3 3 3 2))))

;;; MDE Tue Dec  3 11:24:19 2013 
(sc-deftest test-a-weighting ()
  (sc-test-check
    (< (a-weighting 50 :invert nil :linear nil)  -30)
    (> (a-weighting 50 :invert t :linear nil) 30)
    ;; Linear amplitude scalers:
    (> (a-weighting 50) 32)
    (equal-within-tolerance (a-weighting 50 :invert nil) 0.030637344)
    ;; Exaggeration:
    (> (a-weighting 50 :expt 1.1) 46)
    ;; Smoothing:
    (equal-within-tolerance (a-weighting 50 :expt .5) 5.7131343)))

;;; MDE Wed Mar 25 17:58:35 2015
(sc-deftest test-exaggerate-env ()
  (sc-test-check
    (float-list= '(0 .1 50 .5 100 .9) (exaggerate-env '(0 .1 50 .5 100 .9) 2))
    (float-list= '(0 0.0 50 0.5 100 1.0)
                 (exaggerate-env '(0 0 50 .5 100 1) .03))
    (float-list= '(0 0.0 50 0.5 100 1.0) (exaggerate-env '(0 0 50 .5 100 1) 30))
    (float-list= '(0 0.0 50 0.496 100 1.0)
                 (exaggerate-env '(0 0 50 .4 100 1) 3))
    (float-list= '(0 0.0 50 0.496 100 1.0) 
                 (exaggerate-env '(0 0 50 .4 100 1) 3))
    (float-list= '(0 0.0 50 0.19148308 100 1.0)
                 ;; this is right because .4 is on the bottom half of the env
                 ;; so it's pushed further down to .19 
                 (exaggerate-env '(0 0 50 .4 100 1) .3))
    (float-list= '(0 0.0 50 -0.19148308 100 -1.0)
                 ;; this is also right because -.4 is the top half of the env
                 ;; so it's pushed higher to -.19
                 (exaggerate-env '(0 0 50 -.4 100 -1) .3))
    (float-list= '(0 0.0 50 0.9289586 100 1.0) 
                 (exaggerate-env '(0 0 50 .8 100 1) .3))
    (float-list= '(0 0.0 50 -4.125494 100 -10.0) 
                 (exaggerate-env '(0 0 50 -.8 100 -10) 10))))

;;; MDE Mon May 25 11:33:40 2015
(sc-deftest test-remove-pair ()
    (sc-test-check
      (equalp (remove-pair '(a b - - c d - f) '(- -)) '(a b c d - f))
      (equalp (remove-pair '(a b - x c d - f x -) '(- x)) '(a b c d - f x -))
      (equalp (remove-pair '(a b (- x c d) 3 4 - f x -) '(3 4))
              '(a b (- x c d) - f x -))
      (equalp (remove-pair '(a b (- x c d) (3 4) - f x -) '(3 4))
              '(a b (- x c d) (3 4) - f x -))
      (equalp (remove-pair '(a b - - (c d) (x y) - f) '((c d) (x y)) #'equal)
              '(a b - - - f))))

;;; MDE Fri Feb 17 15:13:30 2017 
(sc-deftest test-pos4neg ()
    (sc-test-check
      (equalp '(-1 3) (pos4neg '(-1 -3) '(-1 3)))
      (equalp '(-1 (3 3)) (pos4neg '(-1 (3 3)) '(-1 3)))))

;; MDE Mon Nov  5 13:14:22 2018 
(sc-deftest test-constrain-int ()
  (sc-test-check
   (= 3 (constrain-int 3 2 8))
   (= 2 (constrain-int 9 2 8))
   (= 5 (constrain-int -2 2 8))
   (= 9 (constrain-int 9 0 10))
   (= 10 (constrain-int 10 0 10))
   (= 0 (constrain-int 0 0 10))
   (= 0 (constrain-int 11 0 10))
   (= -10 (constrain-int -10 -10 0))
   (= -10 (constrain-int -10 -10 10))
   (= 0 (constrain-int 0 -10 10))
   (= 10 (constrain-int -11 -10 10))
   (= 0 (constrain-int 0 -10 0))
   (= 0 (constrain-int -11 -10 0))
   (= 1 (constrain-int 55 0 5)) ; because 0 to 5 has a range of 6!
   (= 5 (constrain-int 55 1 5))
   (= 5 (constrain-int 5 0 5))
   (= -11 (constrain-int 0 -11 -1))
   (= -11 (constrain-int -11 -11 -1))
   (= -1 (constrain-int -12 -11 -1))
   (= 0 (constrain-int 6 0 5))))

;; MDE Thu Sep 24 13:56:03 2020, Heidhausen
(sc-deftest test-utilities-positions ()
  (sc-test-check
    (not (positions '9 '(a b c / d e / f / /)))
    (equalp '(3 6 8 9) (positions '/ '(a b c / d e / f / /)))
    (equalp '(1 4) (positions '(1 2) '(0 (1 2) 3 4 (1 2)) :test #'equalp))
    (equalp '((A B C) (D E) (F))
            (subseqs '(a b c / d e / f / /)  '(3 6 8 9) t))
    (equalp (subseqs '(A B C / D E / F / /) '(3 6 8 9))
            '((A B C) (/ D E) (/ F) (/) (/)))
    (equalp (subseqs '(A B C / D E / F / /) '(3 6 8 9) t)
            '((A B C) (D E) (F)))
    (equalp (subseqs '(a b c / d e / f)  '(3 6) t)
            '((A B C) (D E) (F)))
    (equalp (subseqs '(a b c / d e / f)  '(3) t)
            '((A B C) (D E / F)))
    (equalp (subseqs '(a b c / d e / f)  '(3) nil)
            '((A B C) (/ D E / F)))
    (equalp (subseqs '(a b c / d e / f)  '(3 6) nil)
            '((A B C) (/ D E) (/ F)))))


(sc-deftest test-utilities-centre-list ()
  (sc-test-check
    (equalp '(-3 -2 -1 0 1 2 3) (centre-list '(0 1 2 3 4 5 6) t))
    (equalp '(-3 -2 -1 0 1 2 3) (centre-list '(0 1 2 3 4 5 6) nil))
    (equalp '(-3 -2 -1 0 1 2 3) (centre-list '(1 2 3 4 5 6 7) t))
    (equalp '(-3 -2 -1 0 1 2 3) (centre-list '(1 2 3 4 5 6 7) nil))
    (equalp '(-3 -2 -1 0 1 2) (centre-list '(1 2 3 4 5 6) t))
    (equalp '(-2.5 -1.5 -0.5 0.5 1.5 2.5) (centre-list '(1 2 3 4 5 6) NIL))
    (equalp '(-3 -2 -1 0 1 2 3) (centre-list '(11 12 13 14 15 16 17) nil))
    (equalp '(-3 -2 -1 0 1 2 3) (centre-list '(11 12 13 14 15 16 17) t))
    (equalp '(-3.0 -2.0 -1.0 0.0 1.0 2.0 3.0)
            (centre-list '(11 12 13 14 15 16 17) nil))
    (equalp '(-2.5 -1.5 -0.5 0.5 1.5 2.5)
            (centre-list '(11 12 13 14 15 16) nil))
    (equalp '(-13.5 9.7 4.5 10.5 11.5 -4.5 12.5 13.5)
            (centre-list '(-11 12.2 7 13 14 -2 15 16) nil))
    (equalp '(9.7 -13.5 4.5 10.5 11.5 12.5 13.5 -4.5)
            (centre-list '(12.2 -11 7 13 14 15 16 -2) nil))
    (equalp '(-24 -0.8000002 -6 0 1 -15 2 3)
            (centre-list '(-11 12.2 7 13 14 -2 15 16) t))
    (equalp '(-24 -26 -6 0 1 -15 2 3)
            (centre-list '(-11 -13 7 13 14 -2 15 16) t))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Dec 19 19:45:20 2011 -- rthm-chain methods

(sc-deftest test-procession ()
    (sc-test-check
      (equalp (procession 30 '(1 2 3 4 5))
              '(1 2 1 2 3 1 3 1 1 4 2 4 2 5 3 4 3 4 5 2 3 2 2 5 1 4 1 5 3 4))))

;;; SAR Wed May  2 19:08:56 BST 2012
(sc-deftest test-rthm-chain-procession-b ()
  (let ((pr1 (procession 300 30 :peak 0.1))
        (pr2 (procession 300 30 :peak 0.9)))
    (sc-test-check
      (equalp pr1 '(1 2 1 2 3 4 5 4 4 6 7 8 7 9 10 11 10 11 12 13 14 13 13 15
                    16 17 16 18 19 20 19 20 21 22 23 22 22 24 25 26 25 27 28 29 
                    28 29 30 3 5 3 3 6 8 9 8 12 14 15 14 15 17 18 21 18 18 23
                    24 26 24 27 1 2 1 2 30 5 6 5 5 7 9 10 9 11 12 16 12 16 17
                    19 20 19 19 21 23 25 23 26 27 28 27 28 29 4 6 4 4 30 7 8 7
                    10 11 13 11 13 14 15 17 15 15 20 21 22 21 24 25 26 25 26 29 
                    1 2 1 1 30 3 6 3 8 9 10 9 10 12 14 16 14 14 17 18 20 18 22
                    23 24 23 24 27 28 29 28 28 30 2 5 2 6 7 8 7 8 11 12 13 12
                    12 16 17 19 17 20 21 22 21 22 25 26 27 26 26 29 3 4 3 30 5
                    6 5 6 9 10 11 10 10 13 15 16 15 18 19 20 19 20 23 24 25 24
                    24 27 1 29 1 30 2 4 2 4 7 8 9 8 8 11 13 14 13 16 17 18 17
                    18 21 22 23 22 22 25 27 28 27 29 3 5 3 5 30 6 7 6 6 9 11 12
                    11 14 15 16 15 16 19 20 21 20 20 23 25 26 25 28 1 29 1 29
                    30 2 4 2 2 7 9 10 9 12 13 14 13 14 17 18))
      (equalp pr2 '(1 2 1 2 3 1 3 1 1 4 2 3 2 4 3 4 3 4 5 2 4 2 2 5 1 3 1 5 3 4
                    3 4 5 1 5 1 1 6 2 5 2 6 4 5 4 5 6 3 6 3 3 7 5 6 5 7 2 6 2 6
                    7 6 7 6 6 8 4 7 4 8 7 8 7 8 9 7 8 7 7 9 8 9 8 10 8 9 8 9 10
                    8 9 8 8 10 9 10 9 11 9 10 9 10 11 10 11 10 10 12 10 11 10
                    12 11 12 11 12 13 11 12 11 11 13 12 13 12 14 12 13 12 13 14
                    13 14 13 13 15 13 14 13 15 11 14 11 14 15 14 15 14 14 16 15
                    16 15 17 15 16 15 16 17 16 17 16 16 18 16 17 16 18 17 18 17
                    18 19 17 18 17 17 19 18 19 18 20 18 19 18 19 20 19 20 19 19
                    21 15 20 15 21 20 21 20 21 22 20 21 20 20 22 21 22 21 23 21
                    22 21 22 23 22 23 22 22 24 23 24 23 25 23 24 23 24 25 24 25
                    24 24 26 23 25 23 26 25 26 25 26 27 26 27 26 26 28 25 27 25
                    28 27 28 27 28 29 27 28 27 27 29 28 29 28 30 24 29 24 29 30
                    26 29 26 26 30 28 29 28 30 19 29 19 29 30 22 25 22 22 30 12
                    27 12 30 14 16 14 16 30 17)))))

;;; MDE Sat Apr 28 10:52:41 2012 -- 
(sc-deftest test-make-rthm-chain ()
  (let* ((rc (make-rthm-chain 
              'test 100 
              '((((e) e)                ; 8 in total
                 (- s (s) (s) s -) 
                 ({ 3 (te) - te te - })
                 ((e.) s) 
                 (q)
                 ((e) - s s -)
                 ((s) s (e))
                 ((s) s (s) s))
                (({ 3 (te) te (te) })   ; what we transition to
                 ({ 3 - te (te) te - })
                 ({ 3 (te) - te te - })
                 ({ 3 (tq) te })
                 ({ 3 (te) - ts ts te - })
                 (- s s s s -)
                 ({ 3 - te te - (te) })
                 ({ 3 - te te te - })))
              '((((q q)                 ; the 2/4 bars: 10 total
                  ((q) q) 
                  ((q) q) 
                  ((q) (s) e.)
                  (- e e - (e) e)
                  (- e. s - (e) e) 
                  (- e. s - (q)) 
                  ((e.) s (e) e)
                  (- e. s - (e.) s)
                  ((e.) s (q)))
                 (({ 3 tq te +te tq })      ; what we transition to
                  (q - s e. -)
                  (q (s) e.)
                  (q (s) - s e -)
                  ({ 3 tq te +te - te te - })
                  (- e. s - (e) - s s -) 
                  ({ 3 tq te } { 3 (tq) te })
                  ({ 3 - te te te - } { 3 (te) - te te - })
                  (q { 3 (te) tq })
                  (q { 3 (tq) te })))
                ((((e.) s (e) e (s) e.) ; the 3/4 bars: 5 total
                  (- e e - (e) e (q))
                  (- e. s - - +e e - (q)) 
                  ((q) - e. s - (q))
                  (q (e.) s (q)))
                 (({ 3 (tq) tq tq } (q)) ; what we transition to
                  (- e. s - (q) (s) - s e -)
                  ({ 3 tq te } (q) q)
                  ({ 3 - te te te - } { 3 (te) - te te - } { 3 (tq) te })
                  (q (q) { 3 (te) - te te - }))))))
         (cl (clone rc)))
    (print cl)                          ; to check printing works at all!
    (sc-test-check
      (= 26 (num-rthm-seqs cl))
      (= 8 (num-1-beat-rthms cl))
      (= 2 (num-1-beat-groups cl))
      (= 40 (num-slower-bars cl)))))

;;; MDE Sat Jun  8 19:24:50 2013 -- test the rhythm transitions work the other
;;; way too 
(sc-deftest test-make-rthm-chain-reverse ()
  (let* ((rc (make-rthm-chain 
              'test 100 
              '((({ 3 (te) te (te) }) 
                 ({ 3 - te (te) te - })
                 ({ 3 (te) - te te - })
                 ({ 3 (tq) te })
                 ({ 3 (te) - ts ts te - })
                 (- s s s s -)
                 ({ 3 - te te - (te) })
                 ({ 3 - te te te - }))
                (((e) e)              
                 (- s (s) (s) s -) 
                 ({ 3 (te) - te te - })
                 ((e.) s) 
                 (q)
                 ((e) - s s -)
                 ((s) s (e))
                 ((s) s (s) s)))
              '(((({ 3 tq te +te tq }) 
                  (q - s e. -)
                  (q (s) e.)
                  (q (s) - s e -)
                  ({ 3 tq te +te - te te - })
                  (- e. s - (e) - s s -) 
                  ({ 3 tq te } { 3 (tq) te })
                  ({ 3 - te te te - } { 3 (te) - te te - })
                  (q { 3 (te) tq })
                  (q { 3 (tq) te }))
                 ((q q)               
                  ((q) q) 
                  ((q) q) 
                  ((q) (s) e.)
                  (- e e - (e) e)
                  (- e. s - (e) e) 
                  (- e. s - (q)) 
                  ((e.) s (e) e)
                  (- e. s - (e.) s)
                  ((e.) s (q))))
                ((({ 3 (tq) tq tq } (q))
                  (- e. s - (q) (s) - s e -)
                  ({ 3 tq te } (q) q)
                  ({ 3 - te te te - } { 3 (te) - te te - } { 3 (tq) te })
                  (q (q) { 3 (te) - te te - }))
                 (((e.) s (e) e (s) e.) 
                  (- e e - (e) e (q))
                  (- e. s - - +e e - (q)) 
                  ((q) - e. s - (q))
                  (q (e.) s (q)))))))
         (cl (clone rc)))
    (print cl)                          ; to check printing works at all!
    (sc-test-check
      (= 26 (num-rthm-seqs cl))
      (= 8 (num-1-beat-rthms cl))
      (= 2 (num-1-beat-groups cl))
      (= 40 (num-slower-bars cl)))))

;;; MDE Mon May 21 13:16:12 2012 -- 
(sc-deftest test-get-duration-as-beat ()
  (let ((rl1 '(q q))
        (rl2 '(q e e))
        (rl3 '(s s s s))
        (rl4 '(e s s s))
        (rl5 '(ts ts ts e))
        (rl6 '(fs fs fs fs fs h.)))
    (sc-test-check
      (= 2 (get-duration-as-beat rl1))
      (= 2 (get-duration-as-beat rl2))
      (= 4 (get-duration-as-beat rl3))
      (not (get-duration-as-beat rl4 nil))
      (= 4 (get-duration-as-beat rl5))
      (= 1 (get-duration-as-beat rl6)))))


;;; SAR Tue Jun 12 17:18:15 BST 2012
(sc-deftest test-rthm-chain-specified-variables ()
  (let* ((rch
          (make-rthm-chain
           'test-rch 100
           '((((e) e) ; 4 in total
              (- s (s) (s) s -)
              ({ 3 (te) - te te - })
              ((e.) s))
             (({ 3 (te) te (te) }) ; what we transition to
              ({ 3 - te (te) te - })
              ({ 3 (te) - te te - })
              ({ 3 (te) (te) te })))
           '((((q q) ; the 2/4 bars: 5 total
               ((q) q)
               ((q) q)
               ((q) (s) e.)
               (- e e - (e) e))
              (({ 3 te+te te+te te+te }) ; what we transition to
               (q - s e. -)
               (q (s) e.)
               (q (s) - s e -)
               ({ 3 te+te te+te - te te - })))
             ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
               (- e e - (e) e (q))
               (- e. s - - +e e - (q))
               (q (e.) s (q)))
              (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
               (- e. s - (q) (s) - s e -)
               ({ 3 te+te te } (q) q)
               ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
           :players '(fl cl)
           :section-id 1
           :do-rests t
           :rests '(s e e. q q. h w)
           :rest-cycle '((0 3) (1 2) (2 3) (3 1) (4 2) (5 3) (6 1))
           :rest-re '((2 5) (1 7) (3 11))
           :do-rests-curve '(0 1 33 0 67 1 100 0)
           :do-sticking t
           :sticking-rthms '(q q. h w)
           :sticking-repeats '(7 11 3 5 13 19 7)
           :sticking-curve '(0 10 33 7 67 0 100 3)
           :do-sticking-curve '(0 1 33 0 67 1 100 0)
           :activity-curve '(0 7 33 1 67 10 100 5)
           :harmonic-rthm-curve '(0 3 33 1 67 5 100 7)
           :split-data '(4 5)
           :1-beat-fibonacci t
           :slow-fibonacci t)))
    (create-psps (palette rch))
    (let ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((fl (flute :midi-channel 1))
                         (cl (b-flat-clarinet :midi-channel 2))))
            :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5
                                   d5 e5 f5 g5 a5 b5 c6 d6 e6 f4 g6 a6 b6))))
            :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
            :rthm-seq-palette (palette rch)
            :rthm-seq-map rch)))
      (sc-test-check
        (not (next-event mini 'fl nil 1))
        (equalp
         (loop for ne = (next-event mini 'fl)
            while ne
            collect (data ne)
            collect (is-rest ne))
         '(E T E NIL E T E NIL 8 T 8 T S T 8 T E NIL 8 T E NIL S T E T E NIL 4
           T S T Q NIL Q NIL Q NIL Q NIL Q NIL Q NIL Q NIL 8 T E NIL 8 T E NIL
           8 T 4 T 16 T 16 T E NIL 4 T E NIL 16 T 16 T 16 T S NIL S T S T S NIL
           E. T 8 T E NIL E. T Q. NIL Q. NIL Q. NIL Q. NIL Q. NIL Q. NIL Q. NIL
           Q. NIL Q. NIL Q.  NIL Q. NIL 8/9 T 8 T E NIL 8 T 8 T 8/5 T 8 T 8 T 8
           T E NIL Q NIL Q NIL Q NIL Q NIL Q NIL Q NIL Q NIL E T E NIL S NIL S
           T S T S NIL 4 T 4 T 4 T 4 T 4 T 4 T 4 T E T E NIL Q. NIL Q. NIL
           Q. NIL Q. NIL Q. NIL Q. NIL Q. NIL Q.  NIL Q. NIL Q. NIL Q. NIL 4 T
           E T E NIL 4 T 4 T 4 T 4 T S NIL S T S T S NIL 4 T 4 T H NIL H NIL H
           NIL 4 T E T E NIL 4 T TE T TE NIL TE T S NIL S T S T S NIL 4 T 4 T 4
           T E T E NIL 4 T S NIL S T S T S NIL 4/3 T 4/3 T E T E NIL 4 T S NIL
           S T S T S NIL S NIL S T S T S NIL 4 T 4/3 T 4 T E T E NIL 2 T 4/3 T
           4 T S NIL S T S T S NIL 4 T S NIL S T S T S NIL 4/3 T 4/3 T 4/3 T 4
           T E T E NIL S NIL S T S T S NIL 4 T 4 T 2 T 2 T 4/3 T 2 T 4 T S NIL
           S T S T S NIL 4/3 T 4/3 T 4/3 T 2 T 4 T TE NIL TE T TE NIL 4 T 2 T S
           NIL S T S T S NIL 4 T 4/3 T 2 T 2 T 4/3 T 4 T S NIL S T S T S NIL S
           NIL S T S T S NIL 4 T 4 T S NIL S T S T S NIL 2 T 4/3 T S NIL S T S
           T S NIL 4 T 2 T 4/3 T S NIL S T S T S NIL TE T TE NIL TE NIL 2 T 4/3
           T TE NIL TE T TE NIL 4 T 4 T 4 T S NIL S T S T S NIL TE T TE NIL TE
           NIL 4 T S NIL S T S T S NIL S NIL S T S T S NIL 4 T 4 T 2 T 4 T TE T
           TE NIL TE NIL 4/3 T S NIL S T S T S NIL 4 T TE T TE NIL TE NIL 4 T
           TE NIL TE T TE NIL TE T TE NIL TE NIL 4 T 4 T 4 T E. T TE T TE NIL
           TE NIL 4 T S NIL S T S T S NIL E. T 4 T Q NIL Q NIL Q NIL Q NIL Q
           NIL Q NIL Q NIL TE T TE NIL TE NIL TE T TE NIL TE NIL 4 T TE NIL TE
           T TE NIL 4 T TE T TE NIL TE NIL Q. T TE T TE NIL TE NIL TE T TE NIL
           TE NIL H NIL H NIL H NIL TE T TE NIL TE NIL 8 T 8 T 8 T 8 T 8 T TE T
           TE NIL TE NIL TE T TE NIL TE NIL 4 T 4 T TE T TE NIL TE NIL TE T TE
           NIL TE NIL 4 T 4 T 4 T TE T TE NIL TE NIL TE T TE T TE NIL Q NIL Q
           NIL Q NIL Q NIL Q NIL Q NIL Q NIL TE T TE NIL TE NIL 4 T 4 T TE T TE
           NIL TE NIL TE T TE T TE NIL TE T TE NIL TE NIL 4 T 4 T 4 T 4 T TE T
           TE NIL TE NIL E. T S NIL TE T TE NIL TE NIL 16 T TE T TE T TE NIL TE
           T TE NIL TE NIL TE T TE T TE NIL S T TE T TE T TE NIL TE T TE NIL TE
           NIL S T TE T TE T TE NIL TE T TE T TE NIL E T TE T TE NIL TE NIL 8 T
           8 T 8 T TE T TE T TE NIL TE T TE T TE NIL 16 T 16 T 16 T TE T TE T
           TE NIL 4 T TE T TE T TE NIL TE T TE T TE NIL TE T TE NIL TE NIL 4 T
           TE T TE T TE NIL 4 T TE T TE T TE NIL TE T TE T TE NIL 4 T TE T TE T
           TE NIL 4 T TE T TE T TE NIL TE T TE T TE NIL 4 T TE T TE T TE NIL TE
           T TE T TE NIL 4 T TE T TE T TE NIL 4 T TE T TE T TE NIL 4 T 4 T TE T
           TE T TE NIL 4 T 4 T TE T TE T TE NIL 4 T TE T TE T TE NIL 4 T TE T
           TE T TE NIL E T E NIL 4 T E T E NIL E T E NIL E T E NIL 4 T 4 T E T
           E NIL 4 T E T E NIL E T E NIL))))))

;;; SAR Tue Jun 12 17:20:55 BST 2012
(sc-deftest test-rthm-chain-defaults ()
  (let* ((rch
          (make-rthm-chain
           'test-rch 100
           '((((e) e) ; 4 in total
              (- s (s) (s) s -)
              ({ 3 (te) - te te - })
              ((e.) s))
             (({ 3 (te) te (te) }) ; what we transition to
              ({ 3 - te (te) te - })
              ({ 3 (te) - te te - })
              ({ 3 (te) (te) te })))
           '((((q q) ; the 2/4 bars: 5 total
               ((q) q)
               ((q) q)
               ((q) (s) e.)
               (- e e - (e) e))
              (({ 3 te+te te+te te+te }) ; what we transition to
               (q - s e. -)
               (q (s) e.)
               (q (s) - s e -)
               ({ 3 te+te te+te - te te - })))
             ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
               (- e e - (e) e (q))
               (- e. s - - +e e - (q))
               (q (e.) s (q)))
              (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
               (- e. s - (q) (s) - s e -)
               ({ 3 te+te te } (q) q)
               ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
           :players '(fl cl))))
    (create-psps (palette rch))
    (let ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((fl (flute :midi-channel 1))
                         (cl (b-flat-clarinet :midi-channel 2))))
            :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5
                                   d5 e5 f5 g5 a5 b5 c6 d6 e6 f4 g6 a6 b6))))
            :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
            :rthm-seq-palette (palette rch)
            :rthm-seq-map rch)))
      (sc-test-check
        (not (next-event mini 'fl nil 1))
        (equalp
         (loop for ne = (next-event mini 'fl) while ne collect (data ne)
            collect (is-rest ne)) 
         '(E T E NIL S NIL S T S T S NIL E T E NIL E T S NIL S T S T S NIL TE T
           TE NIL TE NIL E T E NIL E NIL E NIL 8 T E NIL TE T TE NIL TE NIL 8 T
           E T E NIL E T E NIL E. T S NIL 4 T S NIL S T S T S NIL TE T TE NIL
           TE NIL S NIL S T S T S NIL 8 T E. T S NIL S NIL S T S T S NIL 8 T TE
           T TE NIL TE NIL S NIL S T S T S NIL Q. T TE T TE NIL TE NIL E. T S
           NIL TE T TE NIL TE T TE T TE NIL TE NIL E T E NIL 4 T E T E NIL E. T
           S NIL S NIL S T S T S NIL 4 T 4/3 T E NIL E NIL E NIL E NIL E NIL TE
           T TE NIL TE NIL S NIL S T S T S NIL E. T S NIL 8 T 8 T E NIL TE T TE
           NIL TE NIL 8 T E NIL 8 T TE T TE NIL TE NIL TE T TE T TE NIL S NIL S
           T S T S NIL TE T TE NIL TE NIL S NIL S T S T S NIL S NIL S T S T S
           NIL E. T S NIL 8 T E NIL 8 T TE T TE NIL TE NIL TE T TE NIL TE T 4 T
           E. T S NIL S NIL S T S T S NIL 8 T TE T TE NIL TE NIL S NIL S T S T
           S NIL 8 T TE T TE NIL TE NIL E. T S NIL E T E NIL Q. T TE NIL TE T
           TE NIL E T E NIL E NIL E NIL E NIL TE T TE NIL TE T 4 T E. T S NIL
           TE NIL TE T TE NIL W T TE T TE NIL TE NIL TE NIL TE T TE NIL E T E
           NIL E NIL E NIL E NIL E NIL TE T TE T TE NIL E T E NIL TE T TE NIL
           TE NIL TE T TE NIL TE T TE T TE NIL TE NIL 8 T TE T TE T TE NIL S
           NIL S T S T S NIL TE T TE NIL TE NIL 8 T TE NIL TE T TE NIL TE NIL
           TE T TE NIL TE T TE T TE NIL 4 T TE T TE NIL TE T TE T TE NIL TE NIL
           TE T TE NIL TE T 8 T TE T TE T TE NIL E T E NIL TE T TE NIL TE NIL
           TE T TE NIL TE T TE T TE NIL TE NIL TE T TE T TE NIL TE NIL TE T TE
           NIL TE T TE NIL TE NIL 8 T TE NIL TE T TE NIL TE NIL TE T TE NIL
           Q. T TE T TE T TE NIL TE T TE NIL TE T 4 T TE T TE NIL TE NIL TE T
           TE NIL TE T 4 T 2 T 4 T TE T TE T TE NIL TE NIL TE T TE NIL TE T TE
           NIL TE NIL 8 T TE NIL TE T TE NIL TE T TE NIL TE NIL TE T TE T TE
           NIL 8 T TE T TE NIL TE T TE NIL TE T TE NIL 8 T TE T TE NIL TE T TE
           T TE NIL TE T 4 T TE T TE T TE NIL E. NIL E. NIL E. NIL TE NIL TE T
           TE NIL TE T TE NIL TE NIL TE NIL TE T TE NIL TE T TE T TE NIL 8 T TE
           T TE NIL TE T TE T TE NIL TE NIL E NIL E NIL E NIL))))))

;;; SAR Tue Jun 12 19:03:36 BST 2012
(sc-deftest test-rthm-chain-split ()
  (let ((rch
         (make-rthm-chain
          'test-rch 150
          '((((e) e) ; 4 in total
             (- s (s) (s) s -)
             ({ 3 (te) - te te - })
             ((e.) s))
            (({ 3 (te) te (te) }) ; what we transition to
             ({ 3 - te (te) te - })
             ({ 3 (te) - te te - })
             ({ 3 (te) (te) te })))
          '((((q q) ; the 2/4 bars: 5 total
              ((q) q)
              ((q) q)
              ((q) (s) e.)
              (- e e - (e) e))
             (({ 3 te+te te+te te+te }) ; what we transition to
              (q - s e. -)
              (q (s) e.)
              (q (s) - s e -)
              ({ 3 te+te te+te - te te - })))
            ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
              (- e e - (e) e (q))
              (- e. s - - +e e - (q))
              (q (e.) s (q)))
             (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
              (- e. s - (q) (s) - s e -)
              ({ 3 te+te te } (q) q)
              ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
          :split-data nil)))
    (sc-test-check
      (equalp
       (loop for rs in (data (get-data-data 1 (palette rch))) 
          collect (num-bars rs)) 
       '(1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2
         2 2 2 2 2 2 2 2 2 1 1 2 2 1 1 2 2 2 2 1 1 2 2 2 2 2 2 2 2 1 1 2 2 2 2
         2 2 2 2 2 2))
      (split rch :min-beats 1 :max-beats 3 :clone nil)
      (equalp
       (loop for rs in (data (get-data-data 1 (palette rch))) 
          collect (num-bars rs)) 
       '(1 1 2 5 2 6 8 8 5 5 3 3 11 11 2 7 6 6 6 6 8 8 1 1 2 8 3 3 4 4 4 4 2 10
         2 9 6 6 2 6 9 9 2 9 3 3 2 8 1 1 2 8 2 4 3 3 5 5 5 5 2 9 8 8 1 1 7 7 2
         7 10 10 2 7 2 8)))))

;;; SAR Tue Jun 12 22:43:28 BST 2012: 
(sc-deftest test-rthm-chain-add-voice ()
  (let ((rch
         (make-rthm-chain
          'test-rch 150
          '((((e) e)                    ; 4 in total
             (- s (s) (s) s -)
             ({ 3 (te) - te te - })
             ((e.) s))
            (({ 3 (te) te (te) })       ; what we transition to
             ({ 3 - te (te) te - })
             ({ 3 (te) - te te - })
             ({ 3 (te) (te) te })))
          '((((q q)                     ; the 2/4 bars: 5 total
              ((q) q)
              ((q) q)
              ((q) (s) e.)
              (- e e - (e) e))
             (({ 3 te+te te+te te+te }) ; what we transition to
              (q - s e. -)
              (q (s) e.)
              (q (s) - s e -)
              ({ 3 te+te te+te - te te - })))
            ((((e.) s (e) e (s) e.)     ; the 3/4 bars: 4 total
              (- e e - (e) e (q))
              (- e. s - - +e e - (q))
              (q (e.) s (q)))
             (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
              (- e. s - (q) (s) - s e -)
              ({ 3 te+te te } (q) q)
              ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
          :players '(fl cl))))
    (add-voice rch '(1 cl) 'ob)
    (create-psps (palette rch))
    (let ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((fl (flute :midi-channel 1))
                         (ob (oboe :midi-channel 2))
                         (cl (b-flat-clarinet :midi-channel 3))))
            :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5
                                   d5 e5 f5 g5 a5 b5 c6 d6 e6 f4 g6 a6 b6))))
            :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
            :rthm-seq-palette (palette rch)
            :rthm-seq-map rch)))
      (sc-test-check
        (not (next-event mini 'ob nil 1))
        (equalp
         (loop for ne = (next-event mini 'ob)
            while ne
            collect (data ne)
            collect (is-rest ne))
         '(Q NIL Q NIL E T E. T S NIL E T E T E NIL S T E. NIL 8 T 8 NIL 8 T 4
           T 8 T Q NIL Q NIL Q NIL 4 T E NIL E NIL E T E NIL 4 T 4 T 8 T Q NIL
           Q NIL 8 T S T S NIL E NIL E. NIL S NIL Q. T "E" T E NIL Q T E NIL E
           NIL E T E NIL 2 T E. NIL S NIL "E" T E NIL 2 T 4/3 T 8 NIL 8 T Q. T
           Q NIL S T S NIL E NIL 8 T TE NIL TE NIL TE NIL 8 T E NIL TE T TE T
           TE NIL 4 T 8 T Q NIL Q NIL Q NIL E. NIL S NIL "E" T E NIL 4 T Q NIL
           8 T Q NIL 4 T 4 T Q NIL E. NIL S NIL 4 T 8 T S T S NIL E NIL 8 T
           E. T S NIL 8 T E NIL S T E. NIL Q. T TE NIL TE NIL TE NIL E T E NIL
           TE T TE T TE NIL 8 NIL 8 T 8 NIL 4 T 4 T Q NIL W T E NIL E NIL E T E
           T E NIL Q T 8 T 8 NIL Q. T 4 T S T E. NIL 4 T Q NIL 8 T "TE"
           NIL "TE" NIL TE NIL 4 T Q NIL 8 T E NIL E NIL 8 T E NIL 4 T 4 T S T
           E. NIL E. T S NIL 4 T E NIL S T E. NIL Q NIL E. T S NIL 4 T E.  NIL
           S NIL 4 T S T S NIL E NIL 4 T 8 T Q NIL Q. T E. T S NIL E T E NIL S
           T E. NIL 4 T Q NIL S T E. NIL 2 T 2 T 4 T S T E. NIL E. NIL S NIL 4
           T 8 T S T S NIL E NIL "TE" NIL "TE" NIL "TE" NIL 8 T "TE" T TE NIL
           TE NIL 4 T 8 T Q NIL "TE" NIL "TE" NIL TE NIL 4 T 4 T Q NIL E. T
           E. T E. NIL TE NIL TE NIL TE NIL E T E NIL TE T TE T TE NIL 8 T TE T
           TE T "TE" NIL "TE" NIL "TE" NIL "TE" NIL 4 T 8 T 8 NIL 8 NIL 8 T Q
           NIL S T E. NIL "TE" NIL "TE" NIL TE NIL Q. T Q T Q NIL TE T TE
           T "TE" NIL "TE" NIL "TE" NIL "TE" NIL 4 T "TE" NIL "TE" NIL "TE"
           NIL "TE" T TE NIL TE NIL E. NIL E. T E. T 4 T 4 T Q NIL Q NIL S NIL
           E. NIL 2 T 2 T "TE" NIL "TE" NIL "TE" NIL "TE" NIL
           "TE" NIL "TE" NIL E T TE T TE T "TE" NIL E T "TE" T "TE" NIL "TE"
           NIL 4 T TE NIL TE NIL TE NIL 4 T E NIL TE T TE T TE NIL E. NIL S NIL
           4 T 4 T S T S NIL E NIL Q NIL 8 T S T S NIL E NIL "TE" NIL "TE" NIL
           TE NIL 4 T 8 T Q NIL 8 T 8 NIL 8 NIL 4 T 8 T "TE" NIL "TE" NIL "TE"
           NIL "TE" NIL "TE" NIL "TE" NIL "TE" NIL "TE" NIL "TE" NIL "TE" T TE
           NIL TE NIL E. NIL S NIL 4 T 4 T S T S NIL E NIL Q NIL 4 T 2 T 4 T S
           T E. NIL Q NIL Q NIL 8 T TE T TE T "TE" NIL "TE" NIL "TE" NIL "TE"
           NIL 4 T 8 T TE T TE T "TE" NIL "TE" T "TE" NIL "TE" NIL 4 T "TE"
           NIL "TE" NIL TE NIL 4 T Q NIL))))))

;;; SAR Wed Jun 13 16:34:16 BST 2012
(sc-deftest test-rthm-chain-reset ()
  (let ((rch
         (make-rthm-chain
          'test-rch 150
          '((((e) e) ; 4 in total
             (- s (s) (s) s -)
             ({ 3 (te) - te te - })
             ((e.) s))
            (({ 3 (te) te (te) }) ; what we transition to
             ({ 3 - te (te) te - })
             ({ 3 (te) - te te - })
             ({ 3 (te) (te) te })))
          '((((q q) ; the 2/4 bars: 5 total
              ((q) q)
              ((q) q)
              ((q) (s) e.)
              (- e e - (e) e))
             (({ 3 te+te te+te te+te }) ; what we transition to
              (q - s e. -)
              (q (s) e.)
              (q (s) - s e -)
              ({ 3 te+te te+te - te te - })))
            ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
              (- e e - (e) e (q))
              (- e. s - - +e e - (q))
              (q (e.) s (q)))
             (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
              (- e. s - (q) (s) - s e -)
              ({ 3 te+te te } (q) q)
              ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))))
        srth1 srth2 srpt1 srpt2 rre1 rre2 rcs1 rcs2)
    (sc-test-check
      (setf srth1 (loop repeat 19
                     collect (data (get-next (sticking-rthms rch)))))
      (setf srth2 (loop repeat 19
                     collect (data (get-next (sticking-rthms rch)))))
      (not (equalp srth1 srth2))
      (reset rch)
      (setf srth2 (loop repeat 19
                     collect (data (get-next (sticking-rthms rch)))))
      (equalp srth1 srth2)
      (setf srpt1
            (loop repeat 19
               collect (get-next (sticking-repeats rch))))
      (setf srpt2
            (loop repeat 19
               collect (get-next (sticking-repeats rch))))
      (not (equalp srpt1 srpt2))
      (reset rch)
      (setf srpt2
            (loop repeat 19
               collect (get-next (sticking-repeats rch))))
      (equalp srpt1 srpt2)
      (setf rre1
            (loop repeat 19
               collect (get-next (rest-re rch))))
      (setf rre2
            (loop repeat 19
               collect (get-next (rest-re rch))))
      (not (equalp rre1 rre2))
      (reset rch)
      (setf rre2
            (loop repeat 19
               collect (get-next (rest-re rch))))
      (equalp rre1 rre2)
      (setf rcs1
            (loop repeat 19 
               collect (loop for i in (get-next (rcs rch)) 
                          collect (loop for r in i collect (data r)))))
      (setf rcs2
            (loop repeat 19 
               collect (loop for i in (get-next (rcs rch)) 
                          collect (loop for r in i collect (data r)))))
      (not (equalp rcs1 rcs2))
      (reset rch)
      (setf rcs2
            (loop repeat 19 
               collect (loop for i in (get-next (rcs rch)) 
                          collect (loop for r in i collect (data r)))))
      (equalp rcs1 rcs2))))

;;; MDE Fri Jun 15 21:41:03 2012 -- 
(sc-deftest test-hash-least-used ()
  (let ((h (make-hash-table)))
    (loop for i below 100 do
         (setf (gethash i h) 10000))
    (setf (gethash 10 h) 50000
          (gethash 11 h) 4
          (gethash 12 h) 3
          (gethash 13 h) 2)
    (sc-test-check
      (= 13 (hash-least-used h :auto-inc nil))
      (= 10 (hash-least-used h :auto-inc nil :invert t))
      (equalp '(13 12) (hash-least-useds h :num 2 :auto-inc nil))
      (equalp '(10 0) (hash-least-useds h :num 2 :auto-inc nil :invert t))
      ;; now try with :ignore
      (equalp '(12 11) (hash-least-useds h :num 2 :auto-inc nil :ignore '(13)))
      (= 13 (hash-least-used h :auto-inc t))
      ;; now 13 and 12 should both have values of 3 but in the case of two keys
      ;; having the same lowest value our algorithm selects the lowest key 
      (= 12 (hash-least-used h :auto-inc t))
      (= 13 (hash-least-used h :auto-inc nil :start 12))
      (setf (gethash 2 h) 0)
      (= 11 (hash-least-used h :auto-inc nil :start 3 :end 11))
      (= 2 (hash-least-used h :auto-inc nil :end 11))
      (= 13 (hash-least-used h :auto-inc nil :ignore '(2))))))

;;; MDE Wed Jun  8 11:13:51 2016
(sc-deftest test-rescale ()
  (= 50 (rescale .5 0 1 0 100))
  (= 50 (rescale 0 -1 1 0 100))
  (= 0 (rescale -1 -1 1 0 100))
  (= 0 (rescale -1 -1 1 0 100))
  (= 1 (rescale 0 0 1 -1 1))
  (= 0 (rescale .5 0 1 -1 1))
  (= 1000 (rescale 1 0 1 -100 1000))
  (= -100 (rescale 0 0 1 -100 1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan 12 12:01:52 2012 -- test adding objects to rthm-seq-palette and
;;; set-palette, as well as notating them with CMN  

(sc-deftest test-palette-add-and-display ()
  (let* ((rs  (make-rthm-seq '(rs1 ((((2 4) (s) s (e) s (e) s )
                                     ((h))
                                     (- e. 32 - (32) (s) - 32 (s) 32 (32) 32 -))
                                    :pitch-seq-palette (9 9 9 (3) 9 9 (3) 5)))))
         (ps (get-next (pitch-seq-palette rs)))
         (fl (get-data 'flute +slippery-chicken-standard-instrument-palette+))
         (bsn (get-data 
               'bassoon +slippery-chicken-standard-instrument-palette+))
         (set (make-complete-set
               '(b3 d4 g4 bf4 e5 af5 dqs6 ef6 a6 f7)))
         (fl-sqz (sc-make-sequenz rs fl set ps nil nil nil 1 nil nil 1 nil
                                  (make-pitch 'c0)))
         (bsn-sqz (sc-make-sequenz rs bsn set ps nil nil nil 1 nil nil 1 nil
                                   (make-pitch 'c0)))
         (sp (make-set-palette 'test nil))
         (rsp (make-rsp 'test nil))
         (rsp-eps "/tmp/rsp.eps")
         (sp-eps "/tmp/sp.eps")
         ;; MDE Wed Jun  8 11:43:42 2016 -- test MIDI notes
         (esp (make-set-palette 'empty nil)))
    (setf (id fl-sqz) 'fl-sqz
          (id bsn-sqz) 'bsn-sqz
          (id set) 'set1)
    (add-clef (get-nth-rhythm 0 fl-sqz) 'treble)
    (add-clef (get-nth-rhythm 0 bsn-sqz) 'tenor)
    (add set sp)
    (add fl-sqz rsp)
    (add bsn-sqz rsp)
    ;; MDE Wed Jun  8 11:44:04 2016 
    (add (make-sc-set '(cs3 d4) :id 1) esp)
    (add (make-sc-set (loop for m in '(10 20 30 40 50 60) collect
                           (midi-to-note m))
                      :id 2)
         esp)
    #+cmn (cmn-display rsp :file rsp-eps)
    #+cmn (cmn-display sp :file sp-eps)
    (sc-test-check
      (= (num-data rsp) 2)
      (= (num-data esp) 2)
      (= (num-data sp) 1)
      #+cmn (probe-file rsp-eps)
      #+cmn (probe-file sp-eps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Mar  8 15:40:10 2012 -- slippery-chicken class

(sc-deftest test-slippery-chicken1 ()
  (let ((min
         (make-slippery-chicken
          '+minimum+
          :instrument-palette +slippery-chicken-standard-instrument-palette+
          :ensemble '(((fl (flute :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -)))))
          :rthm-seq-map '((1 ((fl (1 1 1 1 1))))))))
    (sc-test-check
      (= 40 (num-notes min))
      (= 2 (bar-line-type (get-bar min 5 'fl)))
      (change-bar-line-type min 1 1)
      (= 1 (bar-line-type (get-bar min 1 'fl)))
      (slippery-chicken-p (clone min))
      ;; MDE Wed Mar 21 07:55:22 2012 -- added these to test writing of repeat
      ;; bar-lines 
      (eq 'final-double (change-bar-line-type min 2 2))
      (eq 'begin-repeat (change-bar-line-type min 3 'begin-repeat))
      (change-bar-line-type min 4 4)
      (change-bar-line-type min 5 5)
      #+cmn (cmn-display min :file "/tmp/sc-minimum.eps")
      (write-lp-data-for-all min :base-path "/tmp"))))

;;; SAR Mon Apr 16 13:27:04 BST 2012: I've commented this out until
;;; PHRENOS-BEG.WAV is added to the dir.
;;; MDE Tue Apr 17 10:30:54 2012 -- using another sndfile instead

;;; MDE Mon Apr  2 12:37:53 2012 
(sc-deftest test-slippery-chicken-get-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :title "mini"
          :instrument-palette +slippery-chicken-standard-instrument-palette+
          :snd-output-dir "/tmp"
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((2 4) (e) e+e. 32 (32)))
                                  :pitch-seq-palette (((1) 2)))))
          :rthm-seq-map '((1 ((vn (1))))))))
    (sc-test-check
      (= 32 (data (get-rest mini 1 2 'vn)))
      (string-equal "E." (data (get-note mini 1 2 'vn)))
      (equalp 'c4 (data (get-note mini 1 '(2 1) 'vn)))
      ;; (equalp 'd4 (data (get-note mini 1 '(2 2) 'vn)))
      ;; (print (data (get-note mini 1 '(2 2) 'vn)))
      (is-tied-from (get-note mini 1 1 'vn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Mon Apr  9 15:24:06 2012 -- post-gen editing tests
(sc-deftest test-add-arrow-to-events ()
  (let* ((mini
         (make-slippery-chicken
          '+mini+
          :title "mini"
          :instrument-palette +slippery-chicken-standard-instrument-palette+
          :ensemble '(((pno (piano :midi-channel 1))))
          :tempo-map '((1 (q 69)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                         (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q q))
                                  :pitch-seq-palette ((1 (2))))))
          :rthm-seq-map '((1 ((pno (1 1 1 1 1 1)))))))
         e1 e2)
    (sc-test-check
      (add-arrow-to-events mini "here" "there" '(1 1) '(5 1) 'pno)
      (setf e1 (get-event mini 1 1 'pno)
            e2 (get-event mini 5 1 'pno))
      (equalp (marks-before e1) '((ARROW "here" "there")))
      ;; MDE Thu Apr 19 12:19:27 2012 -- check this too here...why not?
      (equal-within-tolerance 1.739 (sounding-duration (get-bar mini 1 'pno))
                              0.001)
      (equalp (marks e1) '(START-ARROW))
      (not (marks-before e2))
      (equalp (marks e2) '(END-ARROW))
      (write-lp-data-for-all mini))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recurring-event tests

;;; SAR Fri Apr 27 16:57:14 BST 2012
(sc-deftest test-recurring-event-make-re ()
(let ((mre-1 (make-re '((2 3) (3 2) (5 3) (8 2))))
      (mre-2 (make-re '((2 3) (3 2) (5 3) (8 2))
                      :return-data '(a b c d)
                      :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
  (sc-test-check
    (not (return-data mre-1))
    (not (return-data-cycle mre-1))
    (equalp (data mre-1) '((2 3) (3 2) (5 3) (8 2)))
    (not (id mre-1))
    (equalp (return-data mre-2) '(A B C D))
    (equalp (folded (return-data-cycle mre-2))
            '((0 3) (3 2) (2 1) (1 5)))
    (equalp (data (return-data-cycle mre-2)) '(0 0 0 3 3 2 1 1 1 1 1)) 
    (equalp (data mre-2) '((2 3) (3 2) (5 3) (8 2)))
    (not (id mre-2)))))

;;; SAR Fri Apr 27 17:21:08 BST 2012
(sc-deftest test-recurring-event-get-it ()
  (let ((re-1 (make-re '((2 3) (3 2) (5 3) (8 2)) 
                       :return-data '(a b c d)
                       :return-data-cycle '((0 3) (3 2) (2 1) (1 5))))
        (re-2 (make-re '((2 3) (3 2) (5 3) (8 2)) 
                       :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
    (sc-test-check
      (equalp (loop repeat 50 collect (get-it re-1)) 
              '(NIL NIL A NIL A NIL A NIL NIL D NIL NIL D NIL NIL NIL NIL C 
                NIL NIL NIL NIL B NIL NIL NIL NIL B NIL NIL NIL NIL NIL NIL 
                NIL B NIL NIL NIL NIL NIL NIL NIL B NIL B NIL A NIL A))
      (equalp (loop repeat 50 collect (get-it re-2))
              '(NIL NIL 0 NIL 0 NIL 0 NIL NIL 3 NIL NIL 3 NIL NIL NIL NIL 2 NIL 
                NIL NIL NIL 1 NIL NIL NIL NIL 1 NIL NIL NIL NIL NIL NIL NIL 1
                NIL NIL NIL NIL NIL NIL NIL 1 NIL 1 NIL 0 NIL 0)))))

;;; SAR Fri Apr 27 17:28:28 BST 2012
(sc-deftest test-recurring-event-on-it ()
  (let ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
                     :return-data '(a b c d)
                     :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
    (sc-test-check
      (equalp (loop repeat 50 collect (on-it re)) 
              '(NIL NIL T NIL T NIL T NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL 
                NIL NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL T
                NIL NIL NIL NIL NIL NIL NIL T NIL T NIL T NIL T)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popcorn tests

;;; SAR Sat Apr 28 13:59:24 BST 2012
(sc-deftest test-make-popcorn ()
  (let ((ppcn (make-popcorn '(0.01 0.02) :max-spike 4.2 :min-spike 3.7))
        (ttl 0) 
        (avg 0)
        (mx 0))
    (sc-test-check
      (equalp (data ppcn) '(0.01 0.02))
      (not (id ppcn))
      (= (total ppcn) (apply #'+ (kernels ppcn)))
      (= (numk ppcn) (length (kernels ppcn)))
      (= (mink ppcn) (first (sort (kernels ppcn) #'<)))
      (= (maxk ppcn) (first (last (sort (kernels ppcn) #'<))))
      (= (mean ppcn) (/ (total ppcn) (numk ppcn)))
      (notany #'not
              (loop for i from 0 below (numk ppcn)
                 do
                   (setf ttl (+ ttl (nth i (kernels ppcn))))
                   (setf avg (/ ttl (1+ i)))
                   (when (> (nth i (kernels ppcn)) mx) 
                     (setf mx (nth i (kernels ppcn))))
                 collect (and (>= (nth i (kernels ppcn)) avg)
                              (<= (nth i (kernels ppcn)) mx)))))))

;;; SAR Sat Apr 28 19:39:07 BST 2012
(sc-deftest test-popcorn-scale ()
  (let ((ppcn-1 (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)) 
        (ppcn-2 (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)) 
        old-ks
        old-maxk
        old-mink)
    (sc-test-check
      (setf old-ks (kernels ppcn-1))
      (setf old-maxk (maxk ppcn-1))
      (setf old-mink (mink ppcn-1))
      (scale ppcn-1 8.0)
      (every #'= (kernels ppcn-1)
             (loop for k in old-ks
                collect 
                  (+ 0.0 
                     (* (/ (- 8.0 0.0) 
                           (- old-maxk old-mink)) 
                        (- k old-mink)))))
      (setf old-ks (kernels ppcn-2))
      (setf old-maxk (maxk ppcn-2))
      (setf old-mink (mink ppcn-2))
      (scale ppcn-2 8.0 5.0)
      (every #'= (kernels ppcn-2)
             (loop for k in old-ks
                collect 
                  (+ 5.0 
                     (* (/ (- 8.0 5.0) 
                           (- old-maxk old-mink)) 
                        (- k old-mink))))))))

;;; SAR Wed May  2 13:48:02 BST 2012
;;; This only tests that a value greater that (1- (2* (length kernels)))
;;; produces an error, but doesn't test that it is the right error.
(sc-deftest test-popcorn-fit-to-length ()
  (let ((ppcn (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)))
    (sc-test-check
      (fit-to-length ppcn 100)
      (= (length (kernels ppcn)) 100)
      (fit-to-length ppcn (1- (* 2 (length (kernels ppcn)))))
      (= 199 (length (kernels ppcn)))
      (nth-value 1
                 (ignore-errors 
                   (fit-to-length ppcn (* 2 (length (kernels ppcn)))))))))

(sc-deftest test-popcorn-reheat()
  (let ((ppcn (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0))
        numk1 numk2 numk3)
    (probe-delete "/tmp/ppcn.data")
    (probe-delete "/tmp/ppcn.txt")
    (sc-test-check
      (heat ppcn)
      ;; (print (mean ppcn))
      (setf numk1 (numk ppcn))
      (setf (min-spike ppcn) 4.0)
      (heat ppcn)
      ;; (print (mean ppcn))
      (setf numk2 (numk ppcn))
      (setf (max-spike ppcn) 7.0)
      (heat ppcn)
      ;; (print (mean ppcn))
      (setf numk3 (numk ppcn))
      (plot ppcn "/tmp/ppcn" 'kernels)
      (file-write-ok "/tmp/ppcn.data" 1000)
      (file-write-ok "/tmp/ppcn.txt" 50)
      (/= numk1 numk2 numk3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set-map tests

;;; SAR Sun Apr 29 18:26:05 BST 2012
(sc-deftest test-set-map-gen-midi-chord-seq ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '(((1 ((c3 e3 g3 a3 c4 d4 g4 a4 b4 e5)))
                          (2 ((d4 ef5))))
                         :recurse-simple-data nil)
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) q e s s))
                                  :pitch-seq-palette ((1 2 3 4)))))
          :rthm-seq-map '((1 ((vn (1 1 1)))))))
        (mid "/tmp/mchsq.mid"))
    (probe-delete mid)
    (sc-test-check
      (gen-midi-chord-seq (set-map mini) "/tmp/mchsq.mid")
      (= 3 (num-sequences (set-map mini)))
      (file-write-ok mid 27))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sndfile-palette tests

;;; SAR Thu May  3 11:51:29 BST 2012
#+clm
(sc-deftest test-sndfile-palette-make-sfp ()
  (let ((msfp (make-sfp 
               'sfp-test 
               '((sndfile-group-1
                  (test-sndfile-1))
                 (sndfile-group-2
                  (test-sndfile-2 test-sndfile-3 no-file
                   (test-sndfile-4 :frequency 261.61)))
                 (sndfile-group-3
                  ((test-sndfile-5 :start 0.006 :end 0.182) 
                   test-sndfile-6)))
               :paths (list (concatenate 'string
                                         cl-user::+slippery-chicken-home-dir+ 
                                         "tests/test-sndfiles-dir-1/")
                            (concatenate 'string
                                         cl-user::+slippery-chicken-home-dir+ 
                                         "tests/test-sndfiles-dir-2/")))))
    ;; (setf +sfp+ msfp)
    ;; MDE Wed Sep 30 15:25:07 2015 -- test writing and reading back in
    (with-open-file (out "/tmp/test-sndfile-palette.lsp"
                         :direction :output :if-exists :supersede)
      (print-for-init msfp :stream out))
    (setq msfp (eval (read-from-file "/tmp/test-sndfile-palette.lsp")))
    ;; (print msfp)
    (sc-test-check
      ;; (print (get-snds 'sndfile-group-2 msfp))
      (eq 'test-sndfile-4 (id (get-nearest 261.61 msfp 'sndfile-group-2)))
      (eq 'test-sndfile-3 (id (get-nearest 261.62555 msfp 'sndfile-group-2)))
      (random-rep 2 t)
      ;; MDE Fri Dec 21 14:46:17 2018 -- NB this might only work in sbcl: final
      ;; arg is t which means select a random file if they have the same
      ;; :frequency so calling twice should give different results
      #+sbcl
      (eq 'test-sndfile-4 (id (get-nearest 261.62555 msfp 'sndfile-group-2 t)))
      #+sbcl
      (eq 'test-sndfile-3 (id (get-nearest 261.62555 msfp 'sndfile-group-2 t)))
      (random-rep 2 t)
      ;; but this should work in any lisp
      (not (eq (id (get-nearest 261.62555 msfp 'sndfile-group-2 t))
               (id (get-nearest 261.62555 msfp 'sndfile-group-2 t))))
      ;; (print msfp)
      (sndfile-palette-p msfp)
      ;;(print (paths msfp))
      (= 3 (num-data msfp))
      ;; MDE Thu Jan  3 17:06:43 2013 
      (= 6 (count-snds msfp))
      (equalp 
       (loop for sfg in (data msfp)
          collect (id sfg))
       '(SNDFILE-GROUP-1 SNDFILE-GROUP-2 SNDFILE-GROUP-3))
      (equalp
       (loop for sfg in (data msfp)
          collect (loop for sf in (data sfg)
                     collect (id sf)))
       '((TEST-SNDFILE-1) (TEST-SNDFILE-2 TEST-SNDFILE-3 TEST-SNDFILE-4) 
         (TEST-SNDFILE-5 TEST-SNDFILE-6)))
      (every #'(lambda (x y) (equal-within-tolerance x y 0.001))
             (flatten
              (loop for sfg in (data msfp)
                 collect (loop for sf in (data sfg)
                            collect (start sf))))
             (flatten '((0.0) (0.0 0.0 0.0) (0.006 0.0))))
      (every #'(lambda (x y) (equal-within-tolerance x y 0.001))
             (flatten 
              (loop for sfg in (data msfp)
                 collect (loop for sf in (data sfg)
                            collect (end sf))))
             (flatten '((2.1594558) (1.6437188 2.238526 2.13517) 
                        (0.182 0.7942857)))) 
      (every #'(lambda (x y) (equal-within-tolerance x y 0.001))
             (flatten (loop for sfg in (data msfp)
                         collect (loop for sf in (data sfg)
                                    collect (frequency sf))))
             (flatten '((261.62555) (261.62555 261.62555 261.61) 
                        (261.62555 261.62555))))
      ;; MDE Thu Oct  1 19:12:59 2015 -- test get-nearest
      (setf (frequency (first (get-data-data 'sndfile-group-3 msfp))) 100)
      (eq 'test-sndfile-5 (id (get-nearest 103 msfp)))
    )))

;;; SAR Thu May  3 12:59:52 BST 2012
#+clm
(sc-deftest test-sndfile-palette-find-sndfile ()
  (let ((msfp (make-sfp 
               'sfp-test 
               '((sndfile-group-1
                  (test-sndfile-1))
                 (sndfile-group-2
                  (test-sndfile-2 test-sndfile-3 
                   (test-sndfile-4 :frequency 261.61)))
                 (sndfile-group-3
                  ((test-sndfile-5 :start 0.006 :end 0.182) 
                   test-sndfile-6)))
               :paths (list (concatenate 'string
                                         cl-user::+slippery-chicken-home-dir+  
                                         "tests/test-sndfiles-dir-1/")
                            (concatenate 'string
                                         cl-user::+slippery-chicken-home-dir+ 
                                         "tests/test-sndfiles-dir-2/")))))
    (sc-test-check
      (equalp (find-sndfile msfp 'test-sndfile-3)
              (concatenate 
               'string 
               cl-user::+slippery-chicken-home-dir+
               "tests/test-sndfiles-dir-1/test-sndfile-3.aiff")) 
      (equalp (find-sndfile msfp 'test-sndfile-4)
              (concatenate
               'string 
               cl-user::+slippery-chicken-home-dir+
               "tests/test-sndfiles-dir-2/test-sndfile-4.aiff"))))) 

;;; SAR Thu Jun 14 13:14:03 BST 2012
#+clm
(sc-deftest test-sndfile-palette-make-sfp-from-wavelab-marker-file ()
  (let* ((sfpfwmf-1 (make-sfp-from-wavelab-marker-file 
                     (concatenate 
                      'string 
                      cl-user::+slippery-chicken-home-dir+
                      "tests/24-7.mrk")
                     "24-7"
                     :snds-per-group 2
                     ;; MDE Fri Oct  5 14:08:28 2012 
                     :name 'blah
                     :paths (list (concatenate 
                                   'string 
                                   cl-user::+slippery-chicken-home-dir+
                                   "tests"))
                     :sampling-rate 44100
                     :extensions '("wav")))
         (sfpfwmf-2 (make-sfp-from-wavelab-marker-file 
                     (concatenate 
                      'string 
                      cl-user::+slippery-chicken-home-dir+
                      "tests/24-7.mrk")
                     "24-7"
                     :snds-per-group 2
                     :random-every 3
                     :paths (list (concatenate 
                                   'string 
                                   cl-user::+slippery-chicken-home-dir+
                                   "tests"))
                     :sampling-rate 44100
                     :extensions '("wav")))
         ;; MDE Fri Oct  5 14:12:44 2012 -- check we can combine palettes
         (combo (combine sfpfwmf-1 sfpfwmf-2))
         strt-times
         rands
         rands-minus-rands)
    ;; (print (get-all-refs combo))
    ;; (print (num-snds sfpfwmf-1))
    ;; (print (num-snds sfpfwmf-2))
    (sc-test-check
      ;; MDE Thu Jan  3 17:09:21 2013 
      (= 28 (count-snds combo))
      (= 28 (num-snds combo))
      ;; MDE Fri Jan  4 10:18:29 2013 -- remember: the num-data slot counts the
      ;; number of named objects in the ral but  each of these is a list of two
      ;; sndfiles so num-data and num-snds are not the same.
      (= 14 (num-data combo))
      ;; MDE Fri Oct  5 14:09:47 2012 
      (equalp (id sfpfwmf-1) 'blah)
      (string= (id (first (data sfpfwmf-1))) "BLAH1")
      (= 6 (num-data sfpfwmf-1))
      (= 8 (num-data sfpfwmf-2))
      (= 14 (num-data combo))
      (equalp
       (loop for o in (data sfpfwmf-1)
          collect
          (loop for n in (data o)
             collect (start n)))
       '((0.09142857 1.0666667) (2.7530158 5.09678) (6.7628117 8.573968)
         (10.393832 13.878277) (16.337982 18.287165) (19.403175 25.674559))) 
      (setf strt-times 
            (flatten
             (loop for o in (data sfpfwmf-1)
                collect
                (loop for n in (data o)
                   collect (start n)))))
      (equalp strt-times 
              '(0.09142857 1.0666667 2.7530158 5.09678 6.7628117 8.573968
                10.393832 13.878277 16.337982 18.287165 19.403175 25.674559)) 
      (setf rands
            (flatten
             (loop for o from 2 below (length (data sfpfwmf-2)) by 3
                collect
                (loop for n in (data (nth o (data sfpfwmf-2)))
                   collect (start n)))))
      (every #'(lambda (x) (member x strt-times)) rands)
      (setf rands-minus-rands
            (loop for o in (data sfpfwmf-2)
               collect
               (loop for n in (data o)
                  collect (start n))))
      (not 
       (loop for i from 2 below (length rands-minus-rands) by 3
          do
          (setf (nth i rands-minus-rands) nil)))
      (setf rands-minus-rands (flatten (remove nil rands-minus-rands)))
      (equalp rands-minus-rands strt-times))))

;;; SAR Thu Jun 14 13:35:00 BST 2012
#+clm
(sc-deftest test-sndfile-palette-make-sfp-from-groups-in-wavelab-marker-file ()
  (let ((sfpfgiwmf (make-sfp-from-groups-in-wavelab-marker-file 
                    (concatenate 
                     'string 
                     cl-user::+slippery-chicken-home-dir+
                     "tests/24-7.mrk")
                    "24-7"
                    :paths (list (concatenate 
                                  'string 
                                  cl-user::+slippery-chicken-home-dir+
                                  "tests"))
                    :sampling-rate 44100
                    :extensions '("wav")
                    :print nil)))
    (sc-test-check
      (= 2 (length (data sfpfgiwmf)))
      (equalp (id (first (data sfpfgiwmf))) "tapping")
      (equalp (id (second (data sfpfgiwmf))) "splinter")
      (equalp
       (loop for o in (data sfpfgiwmf)
          collect
            (loop for n in (data o)
               collect (start n)))
       '((2.7530158 5.09678 6.7628117 16.337982) (13.878277 19.403175))))))

;;; MDE Thu Mar 15 11:32:48 2018 
#+clm
(sc-deftest test-sndfile-palette-make-sfp-from-folder ()
  (let ((sfp1 (make-sfp-from-folder
               (concatenate 'string
                            cl-user::+slippery-chicken-home-dir+
                            "tests/test-sndfiles-dir-1")
               ;; ignore test-sndfile-matt-sines.aiff (added 30.5.22)
               :resist "matt"))
        (sfp2 (make-sfp-from-folder
               (concatenate 'string
                            cl-user::+slippery-chicken-home-dir+
                            "tests/test-sndfiles-dir-1")
               :auto-freq t :resist "matt")))
    (sc-test-check
      (= 3 (num-snds sfp1))
      (equal-within-tolerance
       2.238526
       (snd-duration (first (data (first (data sfp1))))))
      (= 3 (num-snds sfp2))
      (equal-within-tolerance
       2.1594558
       (snd-duration (third (data (first (data sfp1))))))
      (if (probe-file "/music/hyperboles/snd/cello/samples/11/")
          (progn
            (setq sfp1 (make-sfp-from-folder
                        "/music/hyperboles/snd/cello/samples/11/"
                        :auto-freq
                        #'(lambda (path)
                            (let* ((pnn (pathname-name path))
                                   (dash-pos (position #\- pnn))
                                   (note (subseq pnn 0 dash-pos)))
                              (frequency (make-pitch note))))))
            (equal-within-tolerance
             (frequency (first (data (first (data sfp1)))))
             554.365234))
          t))))

#+clm
(sc-deftest test-sndfile-palette-make-sfp-from-reaper-markers ()
  (let* ((sfp (make-sfp-from-reaper-markers
              (file-from-sc-dir "tests/barbara-markers.RPP")
              (file-from-sc-dir
               "tests/test-sndfiles-dir-1/test-sndfile-1.aiff")))
         (g1 (get-snds 'group1 sfp))
         (g2 (get-snds 'group2 sfp)))
    ;;(print g1)
    (sc-test-check
      (equal-within-tolerance .088 (duration (second g1)) .001)
      (equal-within-tolerance .167 (duration (first g2)) .001)
      (equal-within-tolerance .895 (start (first g1)) .001)
      (equal-within-tolerance 1.338 (end (second g1)) .001)
      (= 2 (length g1))
      (= 1 (length g2)))))

#+clm
(sc-deftest test-parse-reaper-file-for-segment ()
  (sc-test-check
    (equalp (parse-reaper-file-for-segment
             (file-from-sc-dir "tests/barbara-markers2.RPP")
             'gran)
            '((542.9326 548.098) (598.7433 600.6894) (944.8951 945.41925)
              (947.9406 948.8009) (952.61755 959.5987) (971.784 975.9188)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Oct 24 09:33:20 2017 -- sndfilenet tests. Some functionality of
;;; sndfile-palette was extracted and put into the new sndfilenet class. 
;;; MDE Wed Dec 19 14:10:22 2012 -- sndfile-ext and osc related methods
#+(and mac-osx sbcl)
(sc-deftest test-sndfile-palette-osc ()
  (let* ((sfn (make-sfn
               'sfn-test 
               '((sndfile-group-1
                  (test-sndfile-1))
                 (sndfile-group-2
                  (test-sndfile-2 test-sndfile-3
                   ;; so 4 and 5 won't be used in max-play or allocated cues
                   (test-sndfile-4 :use nil :frequency 261.61)))
                 (sndfile-group-3
                  ((test-sndfile-5 :start 0.006 :end 0.182
                                   :followers (test-sndfile-6
                                               ;; just check to make sure 4
                                               ;; isn't used 
                                               (sndfile-group-2 test-sndfile-4))

                                   :use nil)
                   test-sndfile-6)))
               ;; MDE Wed Jan  6 15:15:54 2021, Heidhausen -- no longer have
               ;; this slot
               ;; :with-followers t
               :paths (list (concatenate 'string
                                         cl-user::+slippery-chicken-home-dir+  
                                         "tests/test-sndfiles-dir-1/")
                            (concatenate 'string
                                         cl-user::+slippery-chicken-home-dir+ 
                                         "tests/test-sndfiles-dir-2/"))))
         (sf5 (get-snd 'sndfile-group-3 'test-sndfile-5 sfn)))
    ;; (print sf5)
    (sc-test-check
      ;; (print (get-all-refs sfn))
      (= 5 (auto-cue-nums sfn))
      (= 2 (cue-num (first (data (get-first sfn)))))
      ;; (print sfn)
      ;; 5 because 4 and 5 aren't :use(d)
      (= 5 (cue-num (get-snd 'sndfile-group-3 'test-sndfile-6 sfn)))
      ;; (= 4 (length (osc-send-cue-nums sfn)))
      (= 4 (length (preload-cues sfn)))
      ;; MDE Fri Jan  4 11:24:14 2013 -- remember two of the above have :use
      ;; nil so won't be issued a cue num
      (equalp 'test-sndfile-6 (id (get-snd-with-cue-num sfn 5)))
      (not (use sf5))
      (= 1 (sclist-length (followers sf5)))
      (equalp 'test-sndfile-6 (id (get-next sf5)))
      (setf (followers sf5) '((sndfile-group-1 test-sndfile-1)
                              (sndfile-group-2 test-sndfile-3)))
      (process-followers sfn #'warn)
      (= 2 (sclist-length (followers sf5)))
      (equalp 'test-sndfile-1 (id (get-next sf5)))
      ;; (equalp 'test-sndfile-6 (get-next sf5))
      ;; MDE Mon Oct 23 17:37:30 2017
      ;; MDE Tue Nov 10 20:45:47 2020, Heidhausen -- can no longer setf
      ;; next-sfe(s) directly
      ;; (setf (next-sfe sfn) 3)
      ;; (eq 'test-sndfile-2 (id (next-sfe sfn)))
      ;; (setf (next-sfe sfn) '(sndfile-group-2 test-sndfile-3))
      ;; (= 4 (cue-num (next-sfe sfn)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instruments.lsp tests

;;; SAR Thu May  3 14:09:37 BST 2012
(sc-deftest test-instruments-sc-standard-inst-palette ()
  (let (sc-stip)
    (setf sc-stip +slippery-chicken-standard-instrument-palette+)
    (sc-test-check
      sc-stip
      (every #'(lambda (x) (eq x t))
             (loop for i from 0 below (1- (length (data sc-stip)))
                collect (instrument-p (nth i (data sc-stip))))))))

;;; SAR Thu May  3 14:10:48 BST 2012: Some chord functions are tested above by
;;; MDE  

;;; SAR Thu May  3 15:14:10 BST 2012: These are not very thorough and do not
;;; test all scenarios or that the pitches are correct, just that they're made
;;; and the right number of pitches are in them.

;;; SAR Thu May  3 14:42:42 BST 2012
(sc-deftest test-instruments-chord-fun-aux ()
  (sc-test-check
    (defun new-chord-function (curve-num index pitch-list pitch-seq instrument
                               set) 
      (chord-fun-aux curve-num index pitch-list pitch-seq instrument set 2 3
                     11))
    (functionp #'new-chord-function)
    (let* ((pl (init-pitch-list '(c4 d4 fs4 gs4 as4 b4 ds5 e5 f5 g5 b5 cs6))) 
           (ncf (new-chord-function nil 2 pl nil nil nil))) 
      (chord-p ncf)
      (= 3 (length (data ncf)))
      (equalp
       (loop for p from 1 to (length (data ncf)) 
          collect (data (get-pitch ncf p)))
       '(C4 FS4 AS4)))))

;;; SAR Thu May  3 14:51:00 BST 2012
(sc-deftest test-instruments-piano-chord-fun ()
  (let* ((pl (init-pitch-list '(c4 d4 fs4 gs4 as4 b4 ds5 e5 f5 g5 b5 cs6)))
         (pcf (piano-chord-fun nil 2 pl nil nil nil)))
    (sc-test-check
      (chord-p pcf)
      (= 4 (length (data pcf)))
      (equalp
       (loop for p from 1 to (length (data pcf)) 
          collect (data (get-pitch pcf p)))
       '(C4 D4 FS4 GS4)))))

;;; SAR Thu May  3 15:19:08 BST 2012
;;; strange: if we call this function after running the test suite it fails in
;;; SBCL but not in CCL
(sc-deftest test-instruments-violin-chord-selection-fun ()
  (let* ((pl (init-pitch-list '(g3 a3 b3 
                                c4 d4 e4 fs4 gs4 as4 b4 
                                c5 ds5 e5 f5 g5 a5 b5 
                                cs6 d6 e6 f6 g6 a6 b6 c7)))
         (vncf1 (violin-chord-selection-fun nil 2 pl nil nil nil))
         (vncf2 (violin-chord-selection-fun nil 25 pl nil nil nil)))
    ;;(print vncf1)
    (sc-test-check
      (chord-p vncf1)
      (= 2 (length (data vncf1)))
      (equalp
       (loop for p from 1 to (length (data vncf1)) 
          collect (data (get-pitch vncf1 p)))
       '(b3 gs4))
      (chord-p vncf2)
      (= 2 (length (data vncf2)))
      (equalp
       (loop for p from 1 to (length (data vncf2)) 
          collect (data (get-pitch vncf2 p)))
       '(e6 c7)))))

;;; SAR Thu May  3 15:23:56 BST 2012
(sc-deftest test-instruments-viola-chord-selection-fun ()
  (let* ((pl (init-pitch-list '(c3 d3 e3 f3 g3 a3 b3 
                                c4 d4 e4 fs4 gs4 as4 b4 
                                c5 ds5 e5 f5 g5 a5 b5 
                                cs6 d6 e6 f6 g6 a6 b6 c7)))
         (vacf1 (viola-chord-selection-fun nil 2 pl nil nil nil))
         (vacf2 (viola-chord-selection-fun nil 30 pl nil nil nil)))
    (sc-test-check
      (chord-p vacf1)
      (= 2 (length (data vacf1)))
      (equalp
       (loop for p from 1 to (length (data vacf1)) 
          collect (data (get-pitch vacf1 p)))
       '(e3 c4))
      (chord-p vacf2)
      (= 2 (length (data vacf2)))
      (equalp
       (loop for p from 1 to (length (data vacf2)) 
          collect (data (get-pitch vacf2 p)))
       '(e6 c7)))))

;;; SAR Thu May  3 15:29:06 BST 2012
(sc-deftest test-instruments-cello-chord-selection-fun ()
  (let* ((pl (init-pitch-list '(c2 d2 e3 f3 g2 a3 b3
                                c3 d3 e3 f3 g3 a3 b3 
                                c4 d4 e4 fs4 gs4 as4 b4 
                                c5 ds5 e5 f5 g5 a5 b5 cs6)))
         (vccf1 (cello-chord-selection-fun nil 2 pl nil nil nil))
         (vccf2 (cello-chord-selection-fun nil 30 pl nil nil nil)))
    (sc-test-check
      (chord-p vccf1)
      (= 2 (length (data vccf1)))
      (equalp
       (loop for p from 1 to (length (data vccf1)) 
          collect (data (get-pitch vccf1 p)))
       '(g2 e3))
      (chord-p vccf2)
      (= 2 (length (data vccf2)))
      (equalp
       (loop for p from 1 to (length (data vccf2)) 
          collect (data (get-pitch vccf2 p)))
       '(f5 cs6)))))

;;; SAR Sat May 19 15:22:04 EDT 2012
(sc-deftest test-instruments-guitar-chord-selection-fun ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((gt (guitar :midi-channel 1))))
          :set-palette '((1 ((e2 f2 g2 a2 b2
                                 c3 d3 e3 f3 g3 a3 b3
                                 c4 d4 e4 f4 g4 a4 b4
                                 c5 d5 e5 f5 g5)
                             :subsets ((guitar (e2 b2 e3 g3 b3 e4))))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((2 4) (e) e e e))
                                  :pitch-seq-palette ((1 (2) 3)))))
          :rthm-seq-map '((1 ((gt (1))))))))
    (sc-test-check
      (chord-p (written-pitch-or-chord (get-event mini 1 3 'gt)))
      (equalp
       (get-pitch-symbols (written-pitch-or-chord (get-event mini 1 3 'gt)))
       '(b3 e4 g4 b4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intervals-mapper tests

;;; SAR Thu May  3 17:28:23 BST 2012
(sc-deftest test-intervals-mapper-make-intervals-mapper ()
  (let* ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))) 
         (nnl '(d1 e1 gs1 as1 d2 ef2 g2 a2 bf2 cs3 d3 ef3 gf3))
         (pl (loop for nn in nnl collect (make-pitch nn)))
         (pl-ints (loop for p from 1 below (length pl) 
                     collect (floor (pitch- (nth p pl) (nth (1- p) pl))))))
    (sc-test-check
      (every #'= pl-ints (steps im))
      (equalp 
       (pitch-list-to-symbols (scale-pitches im)) 
       '(C0 D0 FS0 AF0 C1 CS1 F1 G1 AF1 B1 C2 CS2 E2 FS2 BF2 C3 E3 F3 A3 B3 C4
         EF4 E4 F4 AF4 BF4 D5 E5 AF5 A5 CS6 EF6 E6 G6 AF6 A6 C7 D7 FS7 AF7 C8
         CS8 F8 G8 AF8 B8 C9 CS9 E9 FS9))
      (equalp
       (loop for p from 1 below (length (scale-pitches im))
          collect (floor (pitch- (nth p (scale-pitches im)) 
                                 (nth (1- p) (scale-pitches im)))))
       (loop for i from 0 below (1- (length (scale-pitches im)))
          collect (nth (mod i (length pl-ints)) pl-ints))))))

;;; SAR Thu May  3 18:18:13 BST 2012
(sc-deftest test-intervals-mapper-get-pitch-symbols ()
  (let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
    (sc-test-check
      (equalp (get-pitch-symbols im) '(C0 D0 FS0 AF0 C1 CS1 F1 G1 AF1 B1 C2 CS2
              E2 FS2 BF2 C3 E3 F3 A3 B3 C4 EF4 E4 F4 AF4 BF4 D5 E5 AF5 A5 CS6
              EF6 E6 G6 AF6 A6 C7 D7 FS7 AF7 C8 CS8 F8 G8 AF8 B8 C9 CS9 E9
              FS9)))))

;;; SAR Thu May  3 18:25:40 BST 2012
(sc-deftest test-intervals-mapper-get-steps ()
  (let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
    (sc-test-check
      (equalp (get-steps im) '(2 4 2 4 1 4 2 1 3 1 1 3)))))

;;; SAR Thu May  3 18:44:12 BST 2012
(sc-deftest test-intervals-mapper-get-scale ()
  (let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
    (sc-test-check
      (equalp
       (pitch-list-to-symbols (scale-pitches im))
       '(C0 D0 FS0 AF0 C1 CS1 F1 G1 AF1 B1 C2 CS2 E2 FS2 BF2 C3 E3 F3 A3 B3 C4
         EF4 E4 F4 AF4 BF4 D5 E5 AF5 A5 CS6 EF6 E6 G6 AF6 A6 C7 D7 FS7 AF7 C8
         CS8 F8 G8 AF8 B8 C9 CS9 E9 FS9))
      (equalp
       (pitch-list-to-symbols (get-scale im 'd4))
       '(D4 E4 AF4 BF4 D5 EF5 G5 A5 BF5 CS6 D6 EF6 FS6 AF6 C7 D7 FS7 G7 B7 CS8
         D8 F8 FS8 G8 BF8 C9 E9 FS9)))))

;;; SAR Thu May  3 19:16:09 BST 2012
(sc-deftest test-intervals-mapper-note ()
  (let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
    (sc-test-check
      (equalp (data (intervals-mapper-note im 11 5)) 'c7)
      (equalp (data (intervals-mapper-note im -3 2)) 'f1)
      (= 29 (intervals-mapper-note im 11 5 :tonic 'd1 :nth t)))))

;;; SAR Thu May  3 19:42:28 BST 2012
(sc-deftest test-intervals-mapper-intervals-mapper-degree ()
  (let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
    (= 6 (intervals-mapper-degree im (make-pitch 'bf4) 4))
    (= 20 (intervals-mapper-degree im (make-pitch 'b4) 5))
    (= 45 (intervals-mapper-degree im (make-pitch 'b4) 5 t))
    (not (intervals-mapper-degree im (make-pitch 'bf4) 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; section tests

;;; SAR Fri May  4 12:28:38 BST 2012
(sc-deftest test-section-get-sequenz ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s) (q q q q))
                                  :pitch-seq-palette ((1 2 3 4 5 4 3 2 1))))
                              (2 ((((4 4) q e s s h) (h q e e))
                                  :pitch-seq-palette ((1 2 3 4 5 4 3 2 1))))
                              (3 ((((4 4) e s s h q) (e x 8))
                                  :pitch-seq-palette ((1 2 3 4 5 4 3 2 1 2 3 4
                                                         5)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (2 2 2 2 2))
                              (vc (2 2 2 2 2))))
                          (3 ((cl (3 3 3 3 3))
                              (vc (3 3 3 3 3)))))))
        seq)
    (sc-test-check
      (sequenz-p (setq seq (get-sequenz (get-section mini 2) 'vc 2)))
      ;; MDE Sat Feb  3 15:03:22 2018
      (equalp '(e3 f3 g3 a3 b3)
              (change-pitches seq '(c3 d3 e3 f3 g3 a3 b3) 2 3))
      (equalp '(c3 d3)
              (cddr (get-pitch-symbols (get-nth-bar 1 seq))))
      (sequenz-p (setq seq (get-sequenz (get-section mini 1) 'cl 1)))
      (equalp '(b3)
              (change-pitches seq '(c3 d3 e3 f3 g3 a3 b3) 1 4))
      (equalp '(e3 f3 g3 a3)
              (get-pitch-symbols (get-nth-bar 1 seq))))))

;;; SAR Fri May  4 12:34:23 BST 2012
(sc-deftest test-section-num-sequenzes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vc (cello :midi-channel 1))))
          :set-palette '((1 ((f3 g3 a3 b3 c4))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((vc (1 1 1 1 1))))
                          (2 ((vc (1 1 1 1 1))))
                          (3 ((vc (1 1 1 1 1))))))))
    (sc-test-check
      (= 15 (num-sequences (set-map mini)))
      (= 15 (num-sequences (rthm-seq-map mini)))
      (= 5 (num-sequenzes (get-section mini 2))))))

;;; SAR Sat May 19 15:38:50 EDT 2012
(sc-deftest test-section-get-all-players ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (3 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (equalp (get-all-players (get-section mini 1)) '(cl hn vc))
      (equalp (get-all-players (get-section mini 2)) '(cl hn vc))
      (equalp (get-all-players (get-section mini 3)) '(cl hn vc)))))

;;; SAR Fri Jun  1 12:08:23 BST 2012
(sc-deftest test-section-re-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5))))
          :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((3 8) q e)
                                   ((7 8) e s s q e. s e)
                                   ((2 4) s s e q))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8 9 10 11
                                                         12 13))))
                              (2 ((((2 8) e e)
                                   ((6 8) s s e e e. s e)
                                   ((3 4) s s e h))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8 9 10 11
                                                         12 13))))
                              (3 ((((5 8) h e)
                                   ((9 8) q. s s q e. s e)
                                   ((1 4) s s s s))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8 9 10 11
                                                         12 13)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
                          (2 ((cl (2 2 2 2 2))))
                          (3 ((cl (3 3 3 3 3))))))))
    (sc-test-check
      (= 75 (num-bars mini))
      (equalp
       (loop for b from 1 to (num-bars mini)
          collect (data (get-time-sig mini b)))
       '((3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) 
         (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) 
         (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) 
         (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) 
         (2 4) (2 8) (6 8) (3 4) (2 8) (6 8) (3 4) (2 8) (6 8) (3 4) (2 8) 
         (6 8) (3 4) (2 8) (6 8) (3 4) (5 8) (9 8) (1 4) (5 8) (9 8) (1 4) 
         (5 8) (9 8) (1 4) (5 8) (9 8) (1 4) (5 8) (9 8) (1 4)))
      (re-bar (get-section mini 1)
              :start-bar 1
              :end-bar 10
              :min-time-sig '(2 4))
      (update-slots mini)
      (= 72 (num-bars mini))
      (equalp
       (loop for b from 1 to (num-bars mini)
          collect (data (get-time-sig mini b)))
       '((5 4) (2 4) (5 4) (2 4) (5 4) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) 
         (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) 
         (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) 
         (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (3 8) (7 8) (2 4) (2 8) (6 8) 
         (3 4) (2 8) (6 8) (3 4) (2 8) (6 8) (3 4) (2 8) (6 8) (3 4) (2 8) 
         (6 8) (3 4) (5 8) (9 8) (1 4) (5 8) (9 8) (1 4) (5 8) (9 8) (1 4) 
         (5 8) (9 8) (1 4) (5 8) (9 8) (1 4))))))

;;; SAR Fri Jun  1 12:22:56 BST 2012
(sc-deftest test-section-has-subsections ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1
                      ((a (1 1 1))
                       (b (1 1 1))))
                     (2 (1 1 1 1))
                     (3
                      ((a (1 1 1))
                       (b
                        ((x (1 1 1))
                         (y (1 1 1))))))
                     (4
                      ((a (1 1 1))
                       (b (1 1 1))
                       (c (1 1 1 1)))))
          :set-limits-high '((vn (0 c7 100 c7)))
          :rthm-seq-palette '((1 ((((2 4) (q) e (s) s))
                                  :pitch-seq-palette ((1 2)))))
          :rthm-seq-map '((1
                           ((a ((vn (1 1 1))))
                            (b ((vn (1 1 1))))))
                          (2 ((vn (1 1 1 1))))
                          (3
                           ((a ((vn (1 1 1))))
                            (b
                             ((x ((vn (1 1 1))))
                              (y ((vn (1 1 1))))))))
                          (4
                           ((a ((vn (1 1 1))))
                            (b ((vn (1 1 1))))
                            (c ((vn (1 1 1 1))))))))))
    (sc-test-check
      ;; MDE Tue Feb 20 10:53:33 2018
      (equalp
       (doctor-env
        '(1 C5 3 C5 4 C6 6 C6 7 C7 10 C7 11 C5 13 C5 14 C6 16 C6 17 C7 19 C7 20
          C5 22 C5 23 C6 25 C6 26 C7 29 C7))
       ;; replace existing curve
       (set-limits-by-section mini '(c5 c6 c7) 'set-limits-high 'vn))
      ;; check that the result above was stored
      (equalp
       (get-data-data 'vn (set-limits-high mini))
       (doctor-env
        '(1 C5 3 C5 4 C6 6 C6 7 C7 10 C7 11 C5 13 C5 14 C6 16 C6 17 C7 19 C7 20
          C5 22 C5 23 C6 25 C6 26 C7 29 C7)))
      ;; new 'player' in slot with data
      (set-limits-by-section mini '(c6 d6 e6 f6 g6 a6) 'set-limits-high 'all)
      (equalp (get-data-data 'all (set-limits-high mini))
              (doctor-env
               '(1 C6 3 C6 4 D6 6 D6 7 E6 10 E6 11 F6 13 F6 14 G6 16 G6 17 A6
                 19 A6 20 C6 22 C6 23 D6 25 D6 26 E6 29 E6)))
      ;; new 'player' in slot with no data
      (set-limits-by-section mini '(c4 g4) 'set-limits-low 'all)
      (equalp (get-data-data 'all (set-limits-low mini))
              '(1 120 3 120 4 134 6 134 7 120 10 120 11 134 13 134 14 120 16 120
                17 134 19 134 20 120 22 120 23 134 25 134 26 120 29 120))
      ;; (print (set-limits-low mini))
      ;; MDE Tue Feb 13 14:51:49 2018 
      (= 29 (count-ref (rthm-seq-map mini) 1))
      (= 29 (count-ref (set-map mini) 1))
      (= 29 (num-sequences (set-map mini)))
      (= 29 (num-sequences (rthm-seq-map mini)))
      (has-subsections (get-section mini 1))
      (not (has-subsections (get-section mini 2)))
      (not (has-subsections (get-section mini '(1 a))))
      (has-subsections (get-section mini '(3 b)))
      ;; MDE Mon Oct 29 17:29:13 2018 -- now make sure add-player works with new
      ;; piece method
      )))

;;; SAR Fri Jun  1 12:34:05 BST 2012
(sc-deftest test-section-get-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                         (2 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                         (3 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (2 2 2 2 2))
                     (3 (3 3 3 3 3)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) q e s s h))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (3 ((((4 4) e s s h q))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (2 2 2 2 2))
                              (hn (2 2 2 2 2))
                              (vc (2 2 2 2 2))))
                          (3 ((cl (3 3 3 3 3))
                              (hn (3 3 3 3 3))
                              (vc (3 3 3 3 3))))))))
    (sc-test-check
      (equalp (data (get-bar (get-section mini 1) 1 'vc)) 
              '((4 4) H Q E S S))
      (equalp (data (get-bar (get-section mini 3) 11 'hn)) 
              '((4 4) E S S H Q)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; piece tests

;;; SAR Fri May  4 13:07:32 BST 2012
;;; This just tests that it returns a sequenz object, but not if it returns the
;;; right one.
(sc-deftest test-piece-get-nth-sequenz ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((hn (french-horn :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((hn (nil nil nil nil nil))
                              (vc (1 1 1 1 1))))
                          (3 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sequenz-p (get-nth-sequenz (piece mini) 3 'hn 2))))

;;; SAR Fri May  4 13:45:49 BST 2012
(sc-deftest test-piece-get-sequenz-from-bar-num ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((hn (french-horn :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) h h))
                                  :pitch-seq-palette ((1 2)))))
          :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((hn (1 1 1 1 1))
                              (vc (1 2 1 1 1))))
                          (3 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (sequenz-p (get-sequenz-from-bar-num (piece mini) 7 'vc))
      (equalp 
       (data (first (bars (get-sequenz-from-bar-num (piece mini) 7 'vc))))
       '((4 4) H H)))))

;;; SAR Thu May 31 18:13:36 BST 2012
(sc-deftest test-piece-insert-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((hn (french-horn :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) h h))
                                  :pitch-seq-palette ((1 2)))))
          :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((hn (1 1 1 1 1))
                              (vc (1 2 1 1 1))))
                          (3 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1)))))))
        (new-bar (make-rthm-seq-bar '((4 4) (w)))))    
    (fill-with-rhythms new-bar (loop for r in '(h q. e) 
                                  for p in '(c4 e4 g4)
                                  collect (make-event p r)))
    (sc-test-check
      (= 15 (num-bars mini))
      (= 1 (num-bars (get-nth-sequenz (piece mini) 2 'hn 2)))
      (insert-bar (piece mini) new-bar 1 2 'hn 3 '(1 2 3))
      (insert-bar (piece mini) new-bar 1 2 'vc 3 '(1 2 3))
      (= 2 (num-bars (get-nth-sequenz (piece mini) 2 'hn 2)))
      ;; MDE Fri Aug 23 09:56:47 2013 
      ;; (print-simple (get-bar mini 1 'hn))
      ;; (print (total-degrees (get-bar mini 1 'hn)))
      (= 568 (total-degrees (get-bar mini 1 'hn)))
      (update-slots mini)
      (= 16 (num-bars mini)))))

;;; SAR Thu Jun 14 19:03:33 BST 2012
(sc-deftest test-piece-delete-sequenzes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((hn (french-horn :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) h h))
                                  :pitch-seq-palette ((1 2)))))
          :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((hn (2 2 2 2 2))
                              (vc (2 2 2 2 2))))
                          (3 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (= 15 (num-bars mini))
      (= 5 (length (get-data-data 'hn (get-section mini 2))))
      (= 5 (length (get-data-data 'vc (get-section mini 2))))
      (delete-sequenzes (piece mini) 8 'hn 2)
      (delete-sequenzes (piece mini) 8 'vc 2)
      (= 3 (length (get-data-data 'hn (get-section mini 2))))
      (= 3 (length (get-data-data 'vc (get-section mini 2))))
      (update-slots mini)
      (= 13 (num-bars mini)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instrument-change-map tests

;;; SAR Fri May  4 15:06:16 BST 2012
(sc-deftest test-instrument-change-map-get-first-for-player ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                       (db (double-bass :midi-channel 2))))
          :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax))))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))  
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1)))))))
        cd)
    (sc-test-check
      ;; MDE Mon May 14 20:24:43 2012 -- test change-data::get-change-data
      ;; while we're at it
      (setf cd (get-data '(1 sax) (instrument-change-map mini)))
      (equalp 'alto-sax (get-change-data cd 2))
      (equalp 'tenor-sax (get-change-data cd 21))
      (equalp
       (get-first-for-player (instrument-change-map mini) 'sax)
       'ALTO-SAX))))

;;; SAR Fri May  4 17:10:55 BST 2012
(sc-deftest test-instrument-change-map-make-instrument-change-map ()
  (let ((icm
         (make-instrument-change-map 
          'icm-test
          '((1 ((fl ((1 flute) (3 piccolo) (5 flute)))
                (cl ((1 b-flat-clarinet) (2 bass-clarinet) 
                     (6 b-flat-clarinet)))))  
            (2 ((fl ((2 piccolo) (4 flute)))
                (cl ((2 bass-clarinet) (3 b-flat-clarinet)))))))))
    (sc-test-check
      (instrument-change-map-p icm)
      (equalp (get-all-refs icm) '((1 FL) (1 CL) (2 FL) (2 CL)))
      (equalp
       (loop for k in (get-all-refs icm)
          collect (get-data-data k icm))
       '(((1 1 FLUTE) (3 1 PICCOLO) (5 1 FLUTE))
         ((1 1 B-FLAT-CLARINET) (2 1 BASS-CLARINET) (6 1 B-FLAT-CLARINET))
         ((2 1 PICCOLO) (4 1 FLUTE)) 
         ((2 1 BASS-CLARINET) (3 1 B-FLAT-CLARINET)))))))

;;; MDE Sat Nov 14 19:32:28 2015 -- check that different numbers of staff lines
;;; work
(sc-deftest test-staff-lines ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vn (violin :midi-channel 1))
                        (perc ((vibraphone tambourine marimba) :midi-channel 2))
                        (vc (cello :midi-channel 3))))
           :set-palette '((1 ((gs3 as3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
           :set-map '((1 (1 1 1 1 1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                   :pitch-seq-palette ((1 2 3)))))
           :instrument-change-map '((1 ((perc ((2 vibraphone)
                                               (1 tambourine)
                                               (7 tambourine)
                                               (9 marimba))))))
           :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1 1 1))
                               (perc (1 1 1 1 1 1 1 1 1))
                               (vc (1 1 1 1 1 1 1 1 1))))))))
    (flet ((ic-check (bar event should-be)                                      
             (let ((ic (instrument-change (get-event mini bar event 'perc))))
                                        ; 4th element is instrument object
               (equalp (if ic (subseq ic 0 3) ic)
                       should-be))))
      (sc-test-check
        (ic-check 1 1 '(nil nil 1))
        (ic-check 2 1 '("vibraphone" "vib" 5))
        (ic-check 3 1 nil)
        (ic-check 7 1 '("tambourine" "tmb" 1))
        (ic-check 9 1 '("marimba" "mba" 5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple-change-map tests

;;; SAR Fri May  4 17:40:08 BST 2012
(sc-deftest test-simple-change-map-make-simple-change-map ()
  (let ((scm (make-simple-change-map 'bar-map '((1 3) (34 3) (38 4)))))
    (sc-test-check
      (simple-change-map-p scm)
      (equalp (get-all-refs scm) '((1) (34) (38)))
      (equalp
       (loop for r in (get-all-refs scm) collect (get-data-data r scm))
       '(3 3 4)))))

;;; SAR Fri May  4 17:49:09 BST 2012
(sc-deftest test-simple-change-map-scm-get-data ()
  (let ((scm (make-simple-change-map 'bar-map '((1 3) (34 3) (38 4)))))
    (sc-test-check
      (equalp
       (loop for k in '(1 34 38)
          collect (data (scm-get-data k scm)))
       '(3 3 4))
      (notany #'not
              (loop for k in '(1 34 38)
                 collect (named-object-p (scm-get-data k scm)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; change-data tests

;;; SAR Fri May  4 18:15:45 BST 2012
(sc-deftest test-change-data-make-change-data ()
  (let ((mcd1 (make-change-data 'cd-test '((1 3 23) (6 2 28) (18 4 35))))
        (mcd2 (make-change-data 'cd-test '((2 34) (7 39) (19 46)))))
    (sc-test-check
      (make-change-data 'cd-test '((1 1 23) (6 1 28) (18 1 35)))
      (equalp (data mcd1) '((1 3 23) (6 2 28) (18 4 35)))
      (equalp (data mcd2) '((2 1 34) (7 1 39) (19 1 46))))))

;;; SAR Fri Jun 15 12:08:35 BST 2012
(sc-deftest test-change-data-get-change-data ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                       (db (double-bass :midi-channel 2))))
          :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax))))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s)
                                   (w))
                                  :pitch-seq-palette ((1 2 3 4 5 6)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1))))))))
    (sc-test-check
      (equalp
       (get-change-data 
        (get-data '(1 sax) (instrument-change-map mini)) 2 2)
       'alto-sax))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cm tests

;;; SAR Fri May  4 18:46:42 BST 2012
(sc-deftest test-cm-degrees-per-octave ()
  (sc-test-check
    (in-scale :chromatic)
    (= 12 (degrees-per-octave))
    (in-scale :quarter-tone)
    (= 24 (degrees-per-octave))))

;;; SAR Fri May  4 19:04:39 BST 2012
(sc-deftest test-cm-degree-to-note ()
  (sc-test-check
    (in-scale :chromatic)
    (equalp (degree-to-note 127) 'G9)
    (in-scale :twelfth-tone)
    (equalp (degree-to-note 127) 'ATS0)
    (in-scale :quarter-tone)
    (equalp (degree-to-note 127) 'EQF4)))

;;; SAR Fri May  4 19:10:34 BST 2012
(sc-deftest test-cm-midi-to-degree ()
  (sc-test-check
    (in-scale :chromatic)
    (= 64 (midi-to-degree 64))
    (in-scale :twelfth-tone)
    (= 384 (midi-to-degree 64))
    (in-scale :quarter-tone)
    (= 128 (midi-to-degree 64))))

;;; SAR Fri May  4 19:15:44 BST 2012
(sc-deftest test-cm-midi-to-note ()
  (sc-test-check
    (eq 'c4 (midi-to-note 60.5 cm::*chromatic-scale*))
    (eq 'cqs4 (midi-to-note 60.5))
    (equalp (midi-to-note 67) 'G4)))

;;; SAR Fri May  4 19:19:58 BST 2012
(sc-deftest test-cm-midi-to-freq ()
  (sc-test-check
    (equal-within-tolerance (midi-to-freq 67) 391.99 0.01)
    (equal-within-tolerance (midi-to-freq 67.9) 412.91 0.01)))

;;; SAR Fri May  4 19:24:11 BST 2012
(sc-deftest test-cm-note-to-midi ()
  (sc-test-check
    (= 67 (note-to-midi 'g4))))

;;; SAR Fri May  4 19:39:11 BST 2012
(sc-deftest test-cm-degrees-to-notes ()
  (sc-test-check
    (in-scale :chromatic)
    (equalp (degrees-to-notes '(0 143 116 127 38))
            '(C-1 B10 AF8 G9 D2))
    (in-scale :twelfth-tone)
    (equalp (degrees-to-notes '(0 144 116 127 38 287 863))
            '(C-1 C1 GSS0 ATS0 FSSS-1 CTF3 CTF11))
    (in-scale :quarter-tone)
    (equalp (degrees-to-notes '(0 144 116 127 38 287))
            '(C-1 C5 BF3 EQF4 G0 BQS10))))

;;; SAR Fri May  4 19:47:45 BST 2012
(sc-deftest test-cm-freq-to-note ()
  (sc-test-check
    (in-scale :chromatic)
    (equalp (freq-to-note 423) 'AF4)
    (in-scale :twelfth-tone)
    (equalp (freq-to-note 423) 'GSSS4)
    (in-scale :quarter-tone)
    (equalp (freq-to-note 423) 'AQF4)))

;;; SAR Fri May  4 19:53:26 BST 2012
(sc-deftest test-cm-note-to-freq ()
  (sc-test-check
    (in-scale :chromatic)
    (equal-within-tolerance (note-to-freq 'AF4) 415.30 0.01)
    (in-scale :twelfth-tone)
    (equal-within-tolerance (note-to-freq 'GSSS4) 423.37 0.01)
    (in-scale :quarter-tone)
    (equal-within-tolerance (note-to-freq 'AQF4) 427.47 0.01)))

;;; SAR Fri May  4 20:00:00 BST 2012
(sc-deftest test-cm-note-to-degree ()
  (sc-test-check
    (in-scale :chromatic)
    (= 68 (note-to-degree 'AF4))
    (in-scale :twelfth-tone)
    (= 408 (note-to-degree 'AF4))
    (in-scale :quarter-tone)
    (= 136 (note-to-degree 'AF4))))

;;; SAR Fri May  4 20:07:02 BST 2012
(sc-deftest test-cm-freq-to-degree ()
  (sc-test-check
    (in-scale :chromatic)
    (equal-within-tolerance (freq-to-degree 423) 68.31 0.01)
    (in-scale :twelfth-tone)
    (equal-within-tolerance (freq-to-degree 423) 409.90 0.01)
    (in-scale :quarter-tone)
    (equal-within-tolerance (freq-to-degree 423) 136.63 0.01)))

;;; SAR Fri May  4 20:13:17 BST 2012
(sc-deftest test-cm-get-pitch-bend ()
  (sc-test-check
    (equal-within-tolerance (get-pitch-bend 423) 0.32 0.001)))

;;; SAR Sat May  5 11:36:16 BST 2012
(sc-deftest test-cm-degrees-per-semitone ()
  (sc-test-check
    (in-scale :chromatic)
    (= 1 (degrees-per-semitone))
    (in-scale :twelfth-tone)
    (= 6 (degrees-per-semitone))
    (in-scale :quarter-tone)
    (= 2 (degrees-per-semitone))))


;;; SAR Sat May  5 11:55:21 BST 2012
(sc-deftest test-cm-midi-file-high-low ()
  (sc-test-check
    (= 72
       (nth-value 0
                  (cm::midi-file-high-low 
                   (concatenate 'string 
                                cl-user::+slippery-chicken-home-dir+
                                "tests/test-midifile.mid"))))
    (= 60
       (nth-value 1
                  (cm::midi-file-high-low 
                   (concatenate 'string 
                                cl-user::+slippery-chicken-home-dir+
                                "tests/test-midifile.mid"))))))

;;; SAR Mon Jun  4 18:52:16 BST 2012
(sc-deftest test-cm-midi-file-one-note ()
  (probe-delete "/tmp/msp-gmchs-one-note.mid")
  (sc-test-check
    (cm::midi-file-one-note "/tmp/msp-gmchs.mid" 'c4 1)
    (file-write-ok "/tmp/msp-gmchs-one-note.mid" 23)))

;;; SAR Fri Jun 15 12:32:51 BST 2012
(sc-deftest test-cm-parse-midi-file ()
  (sc-test-check
    (= 61 (length (cm::parse-midi-file "/tmp/msp-gmchs.mid")))))

;;; MDE Thu Nov 10 10:40:05 2016 
(sc-deftest test-midi-file-to-events ()
  (let* ((f1 (concatenate 'string 
                          cl-user::+slippery-chicken-home-dir+
                          ;; no tracks in this one
                          "tests/queen.mid"))
         (f2 (concatenate 'string 
                          cl-user::+slippery-chicken-home-dir+
                          "tests/var5.mid"))
         (el1 (midi-file-to-events f1))
         (el2 (midi-file-to-events f2 :track 1))
         (el3 (midi-file-to-events f2 :track 2)))
    (sc-test-check
      ;; MDE Sat Jun 18 10:18:25 2022, Heidhausen -- these numbers all just went
      ;; up by 1 or 2 because we were missing the first events! 
      (= 3803 (length el1))
      ;; on a related note, try this too:
      (= 3803 (midi2qlist f1 nil))
      (= 810 (midi2qlist f2 nil 1 2))
      ;; (print (first el2))
      ;; (print (first el3))
      (= 441 (length el2))
      (= 369 (length el3)))))

(sc-deftest test-diapason ()
  (sc-test-check
    (= 55/8 (set-diapason 440))
    (= 55/8 (set-diapason nil))
    (= 440 (note-to-freq 'a4))
    (= 69 (note-to-midi 'a4))
    (eq 'ef4 (midi-to-note 63))
    ;; assuming quarter-tone scale, which should be right
    (eq 'eqf4 (midi-to-note 63.5))
    (equal-within-tolerance 320.24368 (midi-to-freq 63.5))
    (equal-within-tolerance 261.62555 (midi-to-freq 60))
    ;; 442
    (= 221/32 (set-diapason 442))
    (= 442 (get-sc-config 'diapason))
    (= 442 (note-to-freq 'a4))
    (= 884 (note-to-freq 'a5))
    (= 442 (midi-to-freq 69))
    (equal-within-tolerance 262.81476 (midi-to-freq 60))
    ;; 432
    (= 432 (set-sc-config 'diapason 432)) ; this must call set-diapason!
    (= 432 (get-sc-config 'diapason))
    (= 27/4 (set-diapason nil))
    (= 432 (note-to-freq 'a4))
    (= 864 (note-to-freq 'a5))
    (= 432 (midi-to-freq 69))
    (equal-within-tolerance 256.8687 (midi-to-freq 60))
    (eq 'c4 (freq-to-note (midi-to-freq 60)))
    (= 440 (set-sc-config 'diapason 440))
    (= 55/8 (set-diapason nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities tests

;;; SAR Sat May  5 12:12:49 BST 2012
(sc-deftest test-utilities-mins-secs-to-secs ()
  (sc-test-check
    (= 121.0 (mins-secs-to-secs '(2 1)))
    (equal-within-less-tolerance 1019.534 (mins-secs-to-secs '(16 59 534)))
    ;; MDE Thu Aug 29 13:21:36 2019 -- test case of string
    (equal-within-tolerance 0.0 (mins-secs-to-secs "0:00.000"))
    (equal-within-tolerance 18.118 (mins-secs-to-secs "0:18.118"))
    (equal-within-tolerance 186.829 (mins-secs-to-secs "3:06.829"))
    (equal-within-tolerance 216.29 (mins-secs-to-secs "3-36.29" #\-) 0.0001)
    (equal-within-tolerance 1619.8 (mins-secs-to-secs "26:59.8"))
    (equal-within-tolerance 7440.00002 (mins-secs-to-secs "124:0.00002"))
    ))

;;; SAR Sat May  5 12:17:59 BST 2012
(sc-deftest test-utilities-string-replace ()
  (sc-test-check
    (equalp (string-replace "flat" "\\flat" "bflat clarinet")
            "b\\flat clarinet")))

;;; SAR Sat May  5 12:22:19 BST 2012
(sc-deftest test-utilities-econs ()
  (sc-test-check
    (equalp (econs '(1 2 3 4) 5) '(1 2 3 4 5))))

;;; SAR Sat May  5 12:30:30 BST 2012
(sc-deftest test-utilities-get-sublist-indices ()
  (sc-test-check
    (equalp 
     (get-sublist-indices '((1 2) (3 4 5 6) (7 8 9) (10 11 12 13 14) (15))) 
     '(0 2 6 9 14))))

;;; SAR Sat May  5 12:39:36 BST 2012
(sc-deftest test-utilities-get-sublist-lengths ()
  (sc-test-check
    (equalp
     (get-sublist-lengths '((1 2) (3 4 5 6) (7 8 9) (10 11 12 13 14) ()))
     '(2 4 3 5 0))
    (equalp
     (get-sublist-lengths '((1 2) (3 4 5 6) (7 8 9) (10 11 12 13 14) ()) t)
     '(2 4 3 5))))

;;; SAR Sat May  5 12:44:05 BST 2012
(sc-deftest test-utilities-all-members ()
  (sc-test-check
    (all-members '(1 2 3 4 5 6 7) '(1 2 3 7))
    (not (all-members '(1 2 3 4 5 6 7) '(1 2 3 7 11)))))

;;; SAR Sat May  5 12:52:35 BST 2012
(sc-deftest test-utilities-split-into-subgroups ()
  (sc-test-check
    (equalp
     (split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 2 3 2 1))
     '((1 2) (3 4) (5 6 7) (8 9) (10)))
    (equalp
     (split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 1))
     '((1 2) (3)))
    (equalp 
     (split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 3 17))
     '((1 2) (3 4 5) (6 7 8 9 10)))))

;;; SAR Sat May  5 14:02:20 BST 2012
(sc-deftest test-utilities-split-into-subgroups2 ()
  (sc-test-check
    (equalp
     (split-into-sub-groups2 '(1 2 3 4 5 6 7 8 9 10 11 12) 3)
     '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
    (equalp
     (split-into-sub-groups2 '(1 2 3 4 5 6 7 8 9 10 11 12) 5)
     '((1 2 3 4 5) (6 7 8 9 10) (11 12)))))

;;; SAR Sat May  5 14:07:59 BST 2012
(sc-deftest test-utilities-split-into-subgroups3 ()
  (sc-test-check
    (equalp
     (split-into-sub-groups3 '(1 2 3 4 5 6 7 8 9 10 11 12) 3)
     '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
    (equalp
     (split-into-sub-groups3 '(1 2 3 4 5 6 7 8 9 10 11 12) 5)
     '((1 2 3 4 5) (6 7 8 9 10 11 12)))))

;;; MDE Tue Aug 28 09:11:19 2018 
(sc-deftest test-utilities-split-into-subgroups4 ()
    (sc-test-check
    (equalp
     (split-into-sub-groups4 '(1 2 3 4 5 6 7 8 9 10 11 12) '(3 4))
     '((1 2 3) (4 5 6 7) (8 9 10) (11 12)))
    (equalp
     (split-into-sub-groups4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14) '(3 4))
     '((1 2 3) (4 5 6 7) (8 9 10) (11 12 13 14)))
    (equalp
     (split-into-sub-groups4 '(1 2 3 4 5 6 7 8 9 10 11 12) '(1 2 3 4))
     '((1) (2 3) (4 5 6) (7 8 9 10) (11) (12)))))


;;; SAR Sat May  5 14:15:24 BST 2012
(sc-deftest test-utilities-power-of-2 ()
  (sc-test-check
    (not (power-of-2 17.3))
    (equal-within-tolerance (nth-value 1 (power-of-2 17.3)) 4.1127 0.001)
    (power-of-2 16)
    (equal-within-tolerance (nth-value 1 (power-of-2 16)) 4.0 0.1)))

;;; SAR Sat May  5 14:26:49 BST 2012
(sc-deftest test-utilities-nearest-power-of-2 ()
  (sc-test-check
    (= 16 (nearest-power-of-2 31))
    (= 32 (nearest-power-of-2 31 t))
    (= 512 (nearest-power-of-2 769))
    (= 1024 (nearest-power-of-2 769 t))
    (= 32 (nearest-power-of-2 32))
    (= 32 (nearest-power-of-2 33))))

;;; SAR Sat May  5 14:30:36 BST 2012
(sc-deftest test-utilities-flatten ()
  (sc-test-check
    (equalp
     (flatten '((1 (2 3 4) (5 (6 7) (8 9 10 (11) 12)) 13) 14 15 (16 17))) 
     (loop for i from 1 to 17 collect i))))

;;; SAR Sat May  5 15:05:52 BST 2012
(sc-deftest test-utilities-equal-within-tolerance ()
  (sc-test-check
    (equal-within-tolerance 261.626 261.62555 0.001)))

;;; SAR Sat May  5 15:21:09 BST 2012
(sc-deftest test-utilities-decimal-places ()
  (sc-test-check
    (= 1.15 (decimal-places 1.1478349092347 2))))

;;; SAR Sat May  5 15:26:38 BST 2012
(sc-deftest test-utilities-almost-zero ()
  (sc-test-check
    (not (almost-zero 0.0000011))
    (almost-zero 0.0000009)))

;;; SAR Sat May  5 15:35:04 BST 2012
(sc-deftest test-utilities-round-if-close ()
  (sc-test-check
  (= 2 (round-if-close 1.999999))
  (= 1.999998 (round-if-close 1.999998))))

;;; SAR Sat May  5 15:40:17 BST 2012
(sc-deftest test-utilities-sort-symbol-list ()
  (sc-test-check
    (equalp
     (sort-symbol-list '(Lorem ipsum dolor sit amet consectetur adipiscing)) 
     '(ADIPISCING AMET CONSECTETUR DOLOR IPSUM LOREM SIT))))

;;; SAR Sat May  5 15:50:36 BST 2012
(sc-deftest test-utilities-list-to-string ()
  (sc-test-check
    (equalp (list-to-string '(1 2 3 4 5)) "1 2 3 4 5")
    (equalp (list-to-string '(1 2 3 4 5) "-") "1-2-3-4-5")
    (equalp (list-to-string NIL) "nil")
    (not (list-to-string NIL "" nil))))

;;; SAR Sat May  5 16:00:55 BST 2012
(sc-deftest test-utilities-semitones ()
  (sc-test-check
    (equal-within-tolerance (semitones 3) 1.1892 0.0001)
    (equal-within-tolerance (semitones 3 2.0 13) 1.1734 0.0001)
    (equal-within-tolerance (semitones 3 4.0) 1.4142 0.0001)
    (equal-within-tolerance (semitones 3.72) 1.2397 0.0001)
    (equal-within-tolerance (semitones -3.72) 0.8066 0.0001)))

;;; SAR Sat May  5 16:08:13 BST 2012
(sc-deftest test-utilities-srt ()
  (sc-test-check
    (equal-within-tolerance (srt 1.73) 9.4893 0.0001)
    (equal-within-tolerance (srt 1.73 4.0 13) 5.14 0.01)))

;;; SAR Sat May  5 16:14:16 BST 2012
(sc-deftest test-utilities-replace-elements ()
  (sc-test-check
    (equalp
     (replace-elements '(1 2 3 4 5 6 7 8 9) 3 7 '(dog cat goldfish))
     '(1 2 3 DOG CAT GOLDFISH 9))))

;;; SAR Sat May  5 16:19:36 BST 2012
(sc-deftest test-utilities-splice ()
  (sc-test-check
    (equalp 
     (splice '(dog cat goldfish) '(1 2 3 4 5 6 7 8 9) 3)
     '(1 2 3 DOG CAT GOLDFISH 4 5 6 7 8 9))))

;;; SAR Sat May  5 16:25:37 BST 2012
(sc-deftest test-utilities-remove-elements ()
  (sc-test-check
    (equalp
     (remove-elements '(1 2 3 4 5 6 7) 2 4)
     '(1 2 7))))

;;; SAR Mon May  7 09:49:24 BST 2012
(sc-deftest test-utilities-nconc-sublists ()
  (sc-test-check
    (equalp
     (nconc-sublists '(((1 2) (a b) (cat dog)) 
                       ((3 4) (c d) (bird fish)) 
                       ((5 6) (e f) (pig cow))))
     '((1 2 3 4 5 6) (A B C D E F) (CAT DOG BIRD FISH PIG COW)))))

;;; SAR Mon May  7 09:57:06 BST 2012
(sc-deftest test-utilities-logarithmic-steps ()
  (sc-test-check
    (every #'equal-within-tolerance
           (logarithmic-steps 1 100 19)
           '(1.0 1.3055556 2.2222223 3.75 5.888889 8.638889 12.0 15.972222
             20.555555 25.75 31.555555 37.97222 45.0 52.63889 60.88889 69.75
             79.22222 89.30556 100.0))))

;;; SAR Mon May  7 10:08:36 BST 2012
(sc-deftest test-utilities-interpolate ()
  (sc-test-check
    (= 1.0 (interpolate 50 '(0 0 4 10 15 22 27 3 43 9 50 1 60 2 84 5 100 0)))
    (= 8.3 (interpolate 50 '(0 0 4 10 15 22 27 3 43 9 53 8 60 2 84 5 100 0)))
    (zerop (interpolate 150 '(0 0 4 10 15 22 27 3 43 9 53 8 60 2 84 5 100 0)))
    (= 1 (interpolate 2 '(5 1 8 10 15 22 27 3 43 9 53 8 60 2 84 5 100 0)))
    (= 0.5 (interpolate 50 '(0 0 100 1)))
    (= 1.0 (interpolate 50 '(0 0 100 1) :scaler 2))
    (= 0.25 (interpolate 50 '(0 0 100 1) :exp 2))))

;;; SAR Mon May  7 10:30:52 BST 2012
(sc-deftest test-utilities-scale-env ()
  (sc-test-check
    (equalp
     (scale-env '(0 53 25 189 50 7 75 200 100 3) 0.5)
     '(0 26.5 25 94.5 50 3.5 75 100.0 100 1.5))
    (equalp
     (scale-env '(0 53 25 189 50 7 75 200 100 3) 0.5 :y-min 20 :y-max 100)
     '(0 26.5 25 94.5 50 20 75 100 100 20))
    (equalp
     (scale-env '(0 53 25 189 50 7 75 200 100 3) 1.0 :x-scaler 2)
     '(0 53.0 50 189.0 100 7.0 150 200.0 200 3.0))
    (equalp
     (scale-env '(0 53 25 189 50 7 75 200 100 3) 1.0 
                :x-scaler 2 :x-min 9 :x-max 90)
     '(9 53.0 50 189.0 90 7.0 90 200.0 90 3.0))))

;;; MDE Thu Sep  5 19:29:54 2013 
(sc-deftest test-utilities-decimate-env ()
  (sc-test-check
    (float-list=
     (decimate-env '(0 0 4 4 5 5 5.1 5.1 5.3 1 5.6 5.6 6 6 10 10) 6)
     '(0.0 0.0 1 2.0 2 4.5 3 4.425 4 8.0 5.0 10.0))
    (float-list=
     (decimate-env '(0 0 4 4 5 5 5.1 5.1 5.3 1 5.6 5.6 6 6 10 10) 6
                   'interpolate)
     '(0 0.0 1 2.0 2 4.0 3 6.0 4 8.0 5 10.0))
    (float-list=
     (decimate-env '(0 0 4 4 5 5 5.1 5.1 5.3 1 5.6 5.6 6 6 10 10) 6 'average)
     '(0.0 0.0 1 1.9899993 2 3.9899993 3 5.452499 4 7.990005 5.0 10.0))))


;;; MDE Thu Aug 29 15:37:04 2013 
(sc-deftest test-utilities-auto-scale-env ()
  (sc-test-check
    (equalp
     (auto-scale-env '(-1 0 .3 -3 1 1) :y-min 5 :y-max 6 :x-min 2)
     '(2.0 5.75 65.7 5.0 100.0 6.0))
    (equalp
     (auto-scale-env '(-1 0 1 1) :y-min 5 :y-max 6 :x-min 2)
     '(2.0 5.0 100.0 6.0))
    (equalp (auto-scale-env '(0 .5 100 .5) :y-min 1 :y-max 2)
            '(0.0 1.0 100.0 1.0))
    (equalp (auto-scale-env '(0 .5 100 .5) :y-min 1 :y-max 2
                            :orig-y-range '(0 1))
            '(0.0 1.5 100.0 1.5))
    (equalp (AUTO-SCALE-ENV '(0 0 10 1))
            '(0.0 0.0 100.0 10.0))
    (equalp
     (AUTO-SCALE-ENV '(-1 0 1 1) :y-min 5 :x-min 2)
     '(2.0 5.0 100.0 10.0))
    (equalp
     (AUTO-SCALE-ENV '(0 0 100 1) :y-min -5)
     '(0.0 -5.0 100.0 10.0))
    (equalp
     (AUTO-SCALE-ENV '(0 1 5 1.5 10 1) :y-min -.1)
     '(0.0 -0.1 50.0 10.0 100.0 -0.1))
    (equalp
     (AUTO-SCALE-ENV '(0 1 5 1.5 10 1) :y-min -15)
     '(0.0 -15.0 50.0 10.0 100.0 -15.0))
    (equalp
     (AUTO-SCALE-ENV '(0 1 5 1.5 10 1) :y-min -15 :y-max -4)
     '(0.0 -15.0 50.0 -4.0 100.0 -15.0))
    (equalp
     (AUTO-SCALE-ENV '(0 1 5 1.5 7 0 10 1) :y-min -15 :y-max -4)
     '(0.0 -7.6666665 50.0 -4.0 70.0 -15.0 100.0 -7.6666665))
    (equalp
     (AUTO-SCALE-ENV '(0 1 5 1.5 7 0 10 1) :y-min -15 :y-max -4 :x-min 10
                     :x-max 11)
     '(10.0 -7.6666665 10.5 -4.0 10.7 -15.0 11.0 -7.6666665))
    (equalp
     (AUTO-SCALE-ENV '(-1 0 .3 -3 1 1) :y-min 5 :y-max 6 :x-min 2)
     '(2.0 5.75 65.7 5.0 100.0 6.0))))

#|
;;; MDE Thu Dec 19 10:50:56 2013 -- these are no longer in the sc package as
;;; they're now in CLM. 
;;; SAR Mon May  7 10:35:33 BST 2012
(sc-deftest test-utilities-reverse-env ()
  (sc-test-check
    (equalp
     (reverse-env '(0 0 25 11 50 13 75 19 100 23))
     '(0 23 25 19 50 13 75 11 100 0))))

;;; SAR Mon May  7 10:49:16 BST 2012
(sc-deftest test-utilities-repeat-env ()
  (sc-test-check
    (every #'(lambda (x y) (equal-within-tolerance x y))
           (repeat-env '(0 1 50 2 100 3) 3)
           '(0.0 1 16.666666 2 33.333332 3 34.333332 1 50.0 2 66.666664 3
             67.666664 1 83.33333 2 100.0 3))
    (every #'(lambda (x y) (equal-within-tolerance x y))
           (repeat-env '(0 1 50 2 100 3) 3 t)
           '(0.0 1 16.666666 2 33.333332 3 50.0 2 66.666664 1 83.33333 2 100.0
             3))))

;;; SAR Mon May  7 11:04:29 BST 2012
(sc-deftest test-utilities-env-plus ()
  (sc-test-check
    (every #'=
           (env-plus '(0 0 25 11 50 13 75 19 100 23) 7.1)
           '(0 7.1 25 18.1 50 20.1 75 26.1 100 30.1))))
|#
;;; SAR Mon May  7 11:25:16 BST 2012
(sc-deftest test-utilities-env-symmetrical ()
  (sc-test-check
    (every #'(lambda (x y) (equal-within-tolerance x y))
           (env-symmetrical '(0 0 25 11 50 13 75 19 100 23))
           '(0 1.0 25 -10.0 50 -12.0 75 -18.0 100 -22.0))
    (every #'(lambda (x y) (equal-within-tolerance x y))
           (env-symmetrical '(0 0 25 11 50 13 75 19 100 23) 0)
           '(0 0.0 25 -11.0 50 -13.0 75 -19.0 100 -23.0))
    (every #'(lambda (x y) (equal-within-tolerance x y))
           (env-symmetrical '(0 0 25 11 50 13 75 19 100 23) 0 -20 -7)
           '(0 -7 25 -11.0 50 -13.0 75 -19.0 100 -20))))

(sc-deftest test-utilities-invert-env ()
  (flet ((ok (l1 l2)
           (every #'(lambda (x y) (equal-within-tolerance x y .00001))
                  l1 l2)))
    (sc-test-check
      (ok (invert-env '(0 .3 40 .6 100 .9))
          '(0 0.9 40 0.6 100 0.3))
      (ok (invert-env '(0 .3 40 .4 100 .9))
          '(0 0.9 40 0.8 100 0.3))
      (ok (invert-env '(0 -.9 40 .4 100 .9))
          '(0 0.9 40 -0.4 100 -0.9))
      (ok (invert-env '(0 -10 40 0 60 .3 100 20))
          '(0 20.0 40 10 60 9.6999999 100 -10.0))
      (ok (invert-env '(0 -.9 40 0 100 .9))
          '(0 0.9 40 0.0 100 -0.9)))))


;;; SAR Mon May  7 15:07:28 BST 2012
(sc-deftest test-utilities-middle ()
  (sc-test-check
    (= 49.5 (middle 7 92))))

;;; SAR Mon May  7 15:12:41 BST 2012
(sc-deftest test-utilities-hz2ms ()
  (sc-test-check
    (equal-within-tolerance 3.8221915 (hz2ms 261.63))))

;;; SAR Mon May  7 15:26:13 BST 2012
(sc-deftest test-utilities-split-groups ()
  (sc-test-check
    (every #'(lambda (x) (= x 17))
           (split-groups 102 17))
    (equalp (split-groups 101 17)
            '(17 17 17 17 17 16))))

;;; SAR Mon May  7 19:18:45 BST 2012
(sc-deftest test-utilities-pts2cm ()
  (sc-test-check
    (equal-within-tolerance (pts2cm 150) 5.2916665)))

;;; SAR Mon May  7 19:31:28 BST 2012
(sc-deftest test-utilities-between ()
  (sc-test-check
    (every #'(lambda (x) (and (>= x 1) (<= x 100)))
           (loop repeat 10 collect (between 1 100)))
    (every #'equalp
           (loop repeat 5 
              collect 
                (loop for i from 0 to 9 
                   collect (between 1 100 t (zerop i))))
           (loop repeat 5 
              collect 
                (loop for i from 0 to 9 
                   collect (between 1 100 t (zerop i)))))))

;;; SAR Mon May  7 19:45:49 BST 2012
(sc-deftest test-utilities-randomise ()
  (sc-test-check
    (every #'(lambda (x) (<= (abs (- x 100)) 5.0))
           (loop repeat 10 collect (randomise 100)))
    (every #'(lambda (x) (<= (abs (- x 87)) (* 0.05 87)))
           (loop repeat 10 collect (randomise 87)))
    (every #'(lambda (x) (<= (abs (- x 67)) (* 0.03 67)))
           (loop repeat 10 collect (randomise 67 3)))))

;;; SAR Mon May  7 20:07:02 BST 2012
(sc-deftest test-utilities-random-from-list ()
  (let ((lst '(3 5 7 11 13 17 19 23 29)))
    (sc-test-check
      (member (random-from-list lst) lst))))

;;; SAR Mon May  7 22:52:29 BST 2012
(sc-deftest test-utilities-wrap-list ()
  (sc-test-check
    (equalp (wrap-list '(1 2 3 4 5 6 7 8 9) 4) '(5 6 7 8 9 1 2 3 4))))

;;; SAR Mon May  7 22:57:07 BST 2012
(sc-deftest test-utilities-combine-into-symbol ()
  (sc-test-check
    (equalp (combine-into-symbol "test" 1 'a) 'test1a)
    (= 6 (nth-value 1 (combine-into-symbol "test" 1 'a)))))

;;; SAR Mon May  7 23:10:26 BST 2012
(sc-deftest test-utilities-factor ()
  (sc-test-check
    (factor 14 7)
    (not (factor 15 7))))

;;; SAR Mon May  7 23:40:39 BST 2012
(sc-deftest test-utilities-get-harmonics ()
  (sc-test-check
    (equalp 
     (get-harmonics 63 :start-partial 2 :max-freq 1010)
     '(126 189 252 315 378 441 504 567 630 693 756 819 882 945 1008))
    ;; MDE Tue May 28 20:59:54 2013 -- 
    (equalp 
     (get-harmonics 63 :start-partial 2 :max-freq 10100 :max-results 10)
     '(126.0 189.0 252.0 315.0 378.0 441.0 504.0 567.0 630.0 693.0))
    ;; MDE Thu Dec 12 17:19:41 2019 -- test new keywords
    (equalp (get-harmonics (note-to-freq 'c3) :notes t
                           :max-freq (note-to-freq 'f6))
            '((C3 0) (C4 0) (G4 2) (C5 0) (E5 -14) (G5 2) (BF5 -31) (C6 0)
              (D6 4) (E6 -14)))
    (pitch-p (first (get-harmonics (note-to-freq 'c3)
                                   :pitches t :max-results 15)))
    (equalp
     (get-harmonics 100 :start-partial 2 :max-results 10
                    :start-freq-is-partial 2)
     '(100.0 150.0 200.0 250.0 300.0 350.0 400.0 450.0 500.0 550.0))))

;;; SAR Mon May  7 23:48:13 BST 2012
(sc-deftest test-utilities-db2amp ()
  (sc-test-check
    (equal-within-tolerance 0.70794576 (db2amp -3))))

;;; SAR Mon May  7 23:52:25 BST 2012
(sc-deftest test-utilities-amp2db ()
  (sc-test-check
    (equal-within-tolerance -10.457575 (amp2db 0.3))))

;;; SAR Mon May  7 23:58:30 BST 2012
(sc-deftest test-utilities-remove-all ()
  (sc-test-check
    (equalp
     (remove-all '(3 5 8 13) '(1 2 3 4 5 6 7 8 9 10 11 12 13))
     '(1 2 4 6 7 9 10 11 12))))

;;; SAR Tue May  8 00:08:04 BST 2012
(sc-deftest test-utilities-amplitude-to-dynamic ()
  (sc-test-check
    (equalp
     (loop for a from 0 to 1 by (/ 1 10) 
        collect (float a)
        collect (amplitude-to-dynamic a))
     '(0.0 NIENTE 0.1 PPPP 0.2 PPP 0.3 PP 0.4 P 0.5 MP 0.6 MF 0.7 F 0.8 FF 0.9  
       FFF 1.0 FFFF))))

;;; SAR Tue May  8 00:15:32 BST 2012
(sc-deftest test-utilities-dynamic-to-amplitude ()
  (sc-test-check
    (equalp
     (loop for d in '(niente pppp ppp pp p mp mf f ff fff ffff)
        collect (dynamic-to-amplitude d))
     '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))))

;;; SAR Tue May  8 00:25:02 BST 2012
(sc-deftest test-utilities-move-elements ()
  (sc-test-check
    (equalp 
     (move-elements '(3 5 8) '(1 2 3 4 5 6 7 8 9) '(a b c d e))
     '(1 2 4 6 7 9))
    (equalp 
     (nth-value 1 (move-elements '(3 5 8) 
                                 '(1 2 3 4 5 6 7 8 9) 
                                 '(a b c d e)))
     '(8 5 3 A B C D E))))

;;; SAR Tue May  8 00:31:05 BST 2012
(sc-deftest test-utilities-move-to-end ()
  (sc-test-check
    (equalp (move-to-end 2 '(1 2 3 4 5))
            '(1 3 4 5 2))
    (equalp (move-to-end 2 '(1 2 3 2 4 2 5))
            '(1 3 4 5 2))
    ;; MDE Mon Jan 18 11:46:36 2016
    (equalp (move-all-to-end '(1 2 4) '(1 2 3 4 5))
                '(3 5 1 2 4))))

;;; SAR Tue May  8 00:54:42 BST 2012
(sc-deftest test-utilities-hailstone ()
  (sc-test-check
    (equalp (hailstone 11)
            '(11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))))

;;; SAR Sun May 20 14:28:23 EDT 2012
(sc-deftest test-utilities-setf-last ()
  (let ((test-list (list 1 2 3 4 5)))
    (sc-test-check
      (setf-last test-list 'dog)
      (equalp test-list '(1 2 3 4 dog)))))

;;; SAR Sun May 20 15:08:54 EDT 2012
(sc-deftest test-utilities-force-length ()
  (sc-test-check
    (equalp
     (force-length (loop for i from 1 to 100 collect i) 17)
     '(1 7 13 20 26 32 39 45 51 57 63 70 76 82 89 95 100))
    (equalp
     (force-length (loop for i from 1 to 100 collect i) 199) 
     '(1 1.5 2 2.5 3 3.5 4 4.5 5 5.5 6 6.5 7 7.5 8 8.5 9 9.5 10 10.5 11 11.5 12
       12.5 13 13.5 14 14.5 15 15.5 16 16.5 17 17.5 18 18.5 19 19.5 20 20.5 21
       21.5 22 22.5 23 23.5 24 24.5 25 25.5 26 26.5 27 27.5 28 28.5 29 29.5 30
       30.5 31 31.5 32 32.5 33 33.5 34 34.5 35 35.5 36 36.5 37 37.5 38 38.5 39
       39.5 40 40.5 41 41.5 42 42.5 43 43.5 44 44.5 45 45.5 46 46.5 47 47.5 48
       48.5 49 49.5 50 50.5 51 51.5 52 52.5 53 53.5 54 54.5 55 55.5 56 56.5 57
       57.5 58 58.5 59 59.5 60 60.5 61 61.5 62 62.5 63 63.5 64 64.5 65 65.5 66
       66.5 67 67.5 68 68.5 69 69.5 70 70.5 71 71.5 72 72.5 73 73.5 74 74.5 75
       75.5 76 76.5 77 77.5 78 78.5 79 79.5 80 80.5 81 81.5 82 82.5 83 83.5 84
       84.5 85 85.5 86 86.5 87 87.5 88 88.5 89 89.5 90 90.5 91 91.5 92 92.5 93
       93.5 94 94.5 95 95.5 96 96.5 97 97.5 98 98.5 99 99.5 100))))

;;; SAR Sun May 20 15:19:20 EDT 2012
(sc-deftest test-utilities-reflect-list ()
  (sc-test-check
    (equalp 
     (reflect-list '(1 4 3 5 9 6 2 7 8 8 9))
     '(9 6 7 5 1 4 8 3 2 2 1))))

;;; SAR Sun May 20 17:08:44 EDT 2012
(sc-deftest test-utilities-remove-more ()
  (sc-test-check
    (equalp (remove-more '(1 2 3 4 5 5 5 6 7 8) #'= 5 7 2)
            '(1 3 4 6 8))
    (equalp (remove-more '(1 2 3 4 5 5 5 6 7 8) #'eq 5.0 7.0 2.0)
            '(1 2 3 4 5 5 5 6 7 8))
    (equalp (remove-more '(1 2 3 4 5 5 5 6 7 8) #'equalp 5.0 7.0 2.0)
            '(1 3 4 6 8))))

;;; SAR Sun May 20 17:24:11 EDT 2012
;;; As with all tests of random results, this is not completely effective, but
;;; will always pass.  
(sc-deftest test-utilities-random-amount ()
  (sc-test-check
    (every #'(lambda (x) (and (>= x -2.5) (<= x 2.5)))
           (loop repeat 10 collect (random-amount 100)))
    (every #'(lambda (x) (and (>= x -4.0) (<= x 4.0)))
           (loop repeat 10 collect (random-amount 80 10)))))

;;; SAR Mon May 21 09:34:54 EDT 2012
(sc-deftest test-utilities-partial-freqs ()
  (sc-test-check
    (partial-freqs 300 900)
    (not (partial-freqs 300 700))
    (partial-freqs 300 300)
    (not (partial-freqs 300 300 nil))))

;;; SAR Mon May 21 10:01:18 EDT 2012
(sc-deftest test-utilities-octave-freqs ()
  (sc-test-check
    (octave-freqs 261.63 2093.04)
    (not (octave-freqs 261.63 3000.00))
    (octave-freqs 261.63 261.63)
    (not (octave-freqs 261.63 261.63 nil))))

;;; SAR Mon May 21 10:07:55 EDT 2012
(sc-deftest test-utilities-swap-elements ()
  (sc-test-check
    (equalp (swap-elements '(1 2 3 4 5 6 7 8 9 10))
            '(2 1 4 3 6 5 8 7 10 9))
    (equalp (swap-elements '(1 2 3 4 5 6 7 8 9))
            '(2 1 4 3 6 5 8 7 9))))

;;; SAR Mon May 21 10:21:41 EDT 2012
(sc-deftest test-utilities-read-from-file ()
  (sc-test-check
    (equalp
     (read-from-file
      (concatenate 'string 
                   cl-user::+slippery-chicken-home-dir+
                   "tests/lisp-lorem-ipsum.txt")) 
     '(Lorem ipsum dolor sit amet consectetur adipiscing elit Cras
       consequat convallis justo vitae consectetur Mauris in nibh vel
       est tempus lobortis Suspendisse potenti Sed mauris massa
       adipiscing vitae dignissim condimentum volutpat vel felis Fusce
       augue dui pulvinar ultricies imperdiet sed pharetra eu quam
       Integer in vulputate velit Aliquam erat volutpat Vivamus sit
       amet orci eget eros consequat tincidunt Nunc elementum
       adipiscing lobortis Morbi at lorem est eget mattis erat Donec
       ac risus a dui malesuada lobortis ac at est Integer at interdum
       tortor Vivamus hendrerit consequat augue Quisque aliquam tellus
       nec vestibulum lobortis risus turpis luctus ligula in bibendum
       felis sem pulvinar dolor Vivamus rhoncus nisi gravida porta
       vulputate ipsum lacus porta risus a vulputate magna justo a
       est))))

;;; SAR Fri Jun 15 12:49:41 BST 2012
(sc-deftest test-utilities-parse-wavelab-marker-file-for-loops ()
  (sc-test-check
    (equalp
     (parse-wavelab-marker-file-for-loops
      (concatenate 
       'string 
       cl-user::+slippery-chicken-home-dir+
       "tests/24-7loops1.mrk"))
     '((25.674559 25.829296 26.116327 26.649048 27.038843)
       (32.211884 32.33669 32.481815 32.618233 32.716915 32.902676 33.227757
        33.61959)
       (36.893604 37.059048 37.160633 37.27383 37.439274 37.4683 37.627937)
       (39.52907 39.81932 39.999275 40.2634 40.338867 40.605896)
       (45.612698 45.818775 46.050976 46.145306 46.275192)
       (46.4566 46.644535 46.76934 46.886894 46.971066 47.16553)
       (84.15927 84.260864 84.292786 84.355194 84.47274 84.52789 84.556915
        84.65415)
       (85.10694 85.227394 85.36236 85.48281 85.5873)
       (91.270386 91.521454 91.627396 91.78993 91.910385 92.04681)
       (121.0224 121.16608 121.26476 121.45197 121.650795 121.882996)
       (159.98549 160.1727 160.4107 160.52681 160.61533 160.74304 160.99411
        161.05505 161.24953 161.50784)
       (169.48535 169.57097 169.76979 169.84961 170.19937 170.29515)
       (170.50122 170.72182 171.11655 171.41551 171.68254)
       (218.33723 218.50703 218.6391 218.79582 218.89597 219.14413)
       (219.82767 220.02359 220.13388 220.29787 220.46912 220.61424)
       (220.86386 221.01913 221.2165 221.41823 221.58658 221.76363)
       (259.27692 259.32916 259.58893 259.9677 260.078 260.23618)
       (274.25018 274.38586 274.51355 274.724 274.8967 275.11728)
       (275.34802 275.44672 275.53525 275.75436 275.92126 276.0664)
       (276.4292 276.57578 276.73398 276.90958 277.0402 277.10114)
       (277.66858 277.9037 277.98206 278.0488 278.12427 278.34195 278.52625
        278.79037)
       (279.22433 279.5044 279.59293 279.72498 279.9006 280.0312)
       (282.38083 282.5883 282.77988 282.82776 282.95258 283.08173)
       (283.28055 283.40244 283.50403 283.60126 283.70142 283.87122 284.1092) 
       (296.73215 296.83954 297.10657 297.21832 297.55646 297.8177)
       (297.93088 298.23856 298.5999 298.86115 298.92352 299.0977)
       (299.7101 300.02792 300.2645 300.34866 300.48654)
       (300.72888 300.86096 301.0177 301.15265 301.2644 301.57495)
       (301.70413 301.88843 302.09305 302.4196 302.7316)
       (357.40878 357.56262 357.69324 357.75275 357.95883)
       (364.94077 365.09314 365.20346 365.33698 365.59674 365.70413)
       (365.9073 366.0379 366.17725 366.2643 366.60098 366.7316 366.91736)
       (367.90277 368.02612 368.1074 368.18866 368.2525 368.39474 368.5297
        368.71692)
       (418.9025 419.29868 419.3843 419.49023 419.72534 419.81244 419.9764)
       (441.9962 442.19647 442.3358 442.3924 442.59555 442.66812 442.87854)
       (472.27356 472.48834 472.61606 472.68716 472.83228 472.97885 473.06015) 
       (474.20227 474.33722 474.47366 474.53604 474.7436 474.81613 474.9482
        475.31247)
       (490.34595 490.5796 490.72617 490.96854 491.09332 491.26312)
       (522.01794 522.2008 522.40546 522.4635 522.71454 522.89014 523.1398
        523.38794 523.5998)
       (523.83057 524.0541 524.13824 524.30804 524.41833 524.5301 524.7405)
       (547.6397 548.07935 548.57764 548.8325 549.11066)
       (588.2641 588.53064 588.7392 589.11 589.44604 589.87476)
       (595.7032 596.37524 596.90826 597.4297 597.9048)
       (598.47253 599.04034 599.7703 600.4076 601.18396)
       (610.70874 611.25336 611.7632 612.33093 613.2126 614.4746)
       (633.22284 634.4859 635.15796 635.7605 636.4557 637.5217)
       (655.91077 656.4554 656.80304 657.4519 658.04285 658.8192)
       (676.1075 676.79114 677.1503 677.57904 678.12366)
       (799.29205 799.8019 800.58984 800.96063 801.13446 801.45886)
       (804.98145 805.2016 805.5724 805.83887 806.31396)))))

;;; SAR Fri Jun 15 13:06:47 BST 2012
(sc-deftest test-utilities-parse-audacity-label-file-for-loops ()
  (sc-test-check
    (equalp
     (parse-audacity-label-file-for-loops 
      (concatenate 
       'string 
       cl-user::+slippery-chicken-home-dir+
       "tests/24-7loops1.txt"))
     '((25.674559 25.829296 26.116327 26.649048 27.038843)
       (32.211884 32.33669 32.481815 32.618233 32.716915 32.902676 33.227757
        33.61959)
       (36.893604 37.059048 37.160633 37.27383 37.439274 37.4683 37.627937)
       (39.52907 39.81932 39.999275 40.2634 40.338867 40.605896)
       (45.612698 45.818775 46.050976 46.145306 46.275192)
       (46.4566 46.644535 46.76934 46.886894 46.971066 47.16553)
       (84.15927 84.260864 84.292786 84.355194 84.47274 84.52789 84.556915
        84.65415)  
       (85.10694 85.227394 85.36236 85.48281 85.5873)
       (91.270386 91.521454 91.627396 91.78993 91.910385 92.04681)
       (121.0224 121.16608 121.26476 121.45197 121.650795 121.882996)
       (159.98549 160.1727 160.4107 160.52681 160.61533 160.74304 160.99411
        161.05505 161.24953 161.50784)
       (169.48535 169.57097 169.76979 169.84961 170.19937 170.29515)
       (170.50122 170.72182 171.11655 171.41551 171.68254)
       (218.33723 218.50703 218.6391 218.79582 218.89597 219.14413)
       (219.82767 220.02359 220.13388 220.29787 220.46912 220.61424)
       (220.86386 221.01913 221.2165 221.41823 221.58658 221.76363)
       (259.27692 259.32916 259.58893 259.9677 260.078 260.23618)
       (274.25018 274.38586 274.51355 274.724 274.8967 275.11728)
       (275.34802 275.44672 275.53525 275.75436 275.92126 276.0664)
       (276.4292 276.57578 276.73398 276.90958 277.0402 277.10114)
       (277.66858 277.9037 277.98206 278.0488 278.12427 278.34195 278.52625
        278.79037)
       (279.22433 279.5044 279.59293 279.72498 279.9006 280.0312)
       (282.38083 282.5883 282.77988 282.82776 282.95258 283.08173)
       (283.28055 283.40244 283.50403 283.60126 283.70142 283.87122 284.1092)
       (296.73215 296.83954 297.10657 297.21832 297.55646 297.8177)
       (297.93088 298.23856 298.5999 298.86115 298.92352 299.0977)
       (299.7101 300.02792 300.2645 300.34866 300.48654)
       (300.72888 300.86096 301.0177 301.15265 301.2644 301.57495)
       (301.70413 301.88843 302.09305 302.4196 302.7316)
       (357.40878 357.56262 357.69324 357.75275 357.95883)
       (364.94077 365.09314 365.20346 365.33698 365.59674 365.70413)
       (365.9073 366.0379 366.17725 366.2643 366.60098 366.7316 366.91736)
       (367.90277 368.02612 368.1074 368.18866 368.2525 368.39474 368.5297
        368.71692) 
       (418.9025 419.29868 419.3843 419.49023 419.72534 419.81244 419.9764)
       (441.9962 442.19647 442.3358 442.3924 442.59555 442.66812 442.87854)
       (472.27356 472.48834 472.61606 472.68716 472.83228 472.97885 473.06015)
       (474.20227 474.33722 474.47366 474.53604 474.7436 474.81613 474.9482
        475.31247)
       (490.34595 490.5796 490.72617 490.96854 491.09332 491.26312)
       (522.01794 522.2008 522.40546 522.4635 522.71454 522.89014 523.1398
        523.38794 523.5998)
       (523.83057 524.0541 524.13824 524.30804 524.41833 524.5301 524.7405)
       (547.6397 548.07935 548.57764 548.8325 549.11066)
       (588.2641 588.53064 588.7392 589.11 589.44604 589.87476)
       (595.7032 596.37524 596.90826 597.4297 597.9048)
       (598.47253 599.04034 599.7703 600.4076 601.18396)
       (610.70874 611.25336 611.7632 612.33093 613.2126 614.4746)
       (633.22284 634.4859 635.15796 635.7605 636.4557 637.5217)
       (655.91077 656.4554 656.80304 657.4519 658.04285 658.8192)
       (676.1075 676.79114 677.1503 677.57904 678.12366)
       (799.29205 799.8019 800.58984 800.96063 801.13446 801.45886)
       (804.98145 805.2016 805.5724 805.83887 806.31396)))))
     
;;; SAR Fri Jun 15 13:18:25 BST 2012
(sc-deftest test-utilities-wavelab-to-audacity-marker-file ()
  (let ((out (concatenate 'string
                  cl-user::+slippery-chicken-home-dir+ 
                  "tests/24-7loops3.txt")))
    (probe-delete out)
    (sc-test-check
      (wavelab-to-audacity-marker-file 
       (concatenate 'string
                    cl-user::+slippery-chicken-home-dir+ 
                    "tests/24-7loops3.mrk")
       44100)
      (file-write-ok out 635))
    (probe-delete out)))

(sc-deftest test-envelope-boundaries ()
  (let ((env1 '(0 10 20 10 21 3 25 4 26 9 50 7 51 1 55 2 56 7 70 10 100 10))
        (env2 '(0 0 4 3.1 100 10)))
    (sc-test-check
      (equalp (envelope-boundaries env1) '(21 26 51 56))
      (equalp (envelope-boundaries env1 30 20) '(21 26 51 56 70))
      (equalp (envelope-boundaries env1 10) '(21 25 26 51 55 56))
      (equalp (envelope-boundaries env2) '(4)) 
      (equalp (envelope-boundaries env2 20) '(4))
      (equalp (envelope-boundaries env2 30 3) nil)
      (equalp (envelope-boundaries env2 32) nil))))

(sc-deftest test-get-clusters ()
  (sc-test-check
    (equalp 
     (get-clusters '(24 55 58 59 60 81 97 102 106 116 118 119 145 149 151 200
                     210 211 214 217 226 233 235 236 237 238 239 383 411 415
                     419))
     '(24 (55 58 59 60) 81 (97 102 106) (116 118 119) (145 149 151) 200
       (210 211 214 217) 226 (233 235 236 237 238 239) 383 (411 415 419)))
    (equalp (get-clusters '(0 .1 .3 .7 1.5 1.55 2 4.3 6.3 6.4) 1)
            '((0 0.1 0.3 0.7 1.5 1.55 2) 4.3 (6.3 6.4)))
    (equalp (get-clusters '(0 .1 .3 .7 1.5 1.55 2 4.3 6.3 6.4) 0.5)
            '((0 0.1 0.3 0.7) (1.5 1.55 2) 4.3 (6.3 6.4)))))

;;; MDE Wed Mar 25 16:54:03 2015
(sc-deftest test-fscale ()
  (sc-test-check
    (= 1.0 (fscale 20 10 110 0 10))
    (= 0.1 (fscale 20 10 110 0 1))
    (= -1.0 (fscale 20 10 110 0 -10))
    (= -9.0 (fscale 20 10 110 -10 0))
    (= -8.0 (fscale 20 10 110 -10 10))
    ;; now some weird scalings where either min > max -- should also work
    (equal-within-tolerance -7.142857 (fscale 0 -.5 3 -10 10))
    7.142857 (fscale 0 -.5 3 10 -10) 
    (equal-within-tolerance -7.1428566 (fscale 0 3 -.5 10 -10))
    (equal-within-tolerance 7.1428566 (fscale 0 3 -.5 -10 10))
    (equal-within-tolerance 8.571428 (fscale 0 3 -.5 0 10))))

(sc-deftest test-down-up ()
  (sc-test-check
    (float-list= (down-up 10)
                 '(0.8 0.6 0.4 0.2
                   5.551115123125783d-17 0.166666666666 0.333333333333 0.5
                   0.66666666666 0.83333333333))
    (float-list= (down-up 10 :up nil)
                 '(0.9090909090909091d0 0.8181818181818181d0
                   0.7272727272727272d0 0.6363636363636362d0
                   0.5454545454545453d0 0.45454545454545436d0
                   0.3636363636363634d0 0.2727272727272725d0
                   0.18181818181818157d0 0.09090909090909066d0))
    (float-list= (down-up 10 :down nil)
                 '(0.09090909090909091d0 0.18181818181818182d0
                   0.2727272727272727d0
                   0.36363636363636365d0 0.4545454545454546d0
                   0.5454545454545455d0
                   0.6363636363636365d0 0.7272727272727274d0
                   0.8181818181818183d0 0.9090909090909093d0))
    (float-list=
     (down-up 10 :down nil :cons t)
     '(0.0d0 0.1d0 0.2d0 0.30000000000000004d0 0.4d0 0.5d0 0.6d0 0.7d0 
       0.7999999999999999d0 0.8999999999999999d0))
    (float-list=
     (print (down-up 10 :start 3.5 :target .5))
     '(2.9 2.3000002 1.7000002 1.1000001 0.5000001 1.0 1.5 2.0 2.5 3.0))
    ;; '(3.4 2.8000002 2.2000003 1.6000001 1.0000001 1.0 1.5 2.0 2.5 3.0))
    (float-list=
     (down-up 10 :down nil :butlast nil)
     '(0.1d0 0.2d0 0.30000000000000004d0 0.4d0 0.5d0 0.6d0 0.7d0
       0.7999999999999999d0 0.8999999999999999d0 0.9999999999999999d0))
    ))

;;; MDE Thu Sep  6 08:20:30 2018
(sc-deftest test-things-from-title ()
  (sc-test-check
    (string= "1-2-3" (filename-from-title "1 2 3"))
    (string= "more---spaces-than_planned"
             (filename-from-title "more   spaces than_planned"))
    (eq '+1-2-3+ (sc-name-from-title "1 2 3"))
    (eq '+the-dog-and-bone+ (sc-name-from-title "the dog and bone"))))


;;; MDE Tue May 21 07:56:14 2019
(sc-deftest test-middle-out ()
  (sc-test-check
    (equalp (middle-out '(1 2 3 4 5 6)) '(4 3 5 2 6 1))
    (equalp (middle-out '(1 2 3 4 5 6 7)) '(4 3 5 2 6 1 7))))

;;; MDE Tue Jun 23 15:23:27 2020, Heidhausen
(sc-deftest test-one-to-many ()
  (sc-test-check 
    (float-list= (one-to-many .8 7)
                 '(0.045112778 0.082706764 0.12030074 0.15789473 0.19548872
                   0.21804512 0.18045112))
    (float-list=  (one-to-many .8 7 .7)
                  '(0.06509622 0.09950039 0.1293403 0.15646003 0.18169008
                    0.19612299 0.17178996))
    (float-list= (one-to-many .8 7 1.3) 
                 '(0.030845987 0.06782856 0.11039718 0.15721251 0.20752355
                   0.23917702 0.1870151))
    ;; passing 5 points: these don't have to have min/max of 0 and 1 ...
    (float-list= (one-to-many .8 '(0 .1 .35 .7 .92))
                 '(0.07067137 0.10600708 0.19434628 0.3180212 0.31095406))
    ;; ... and they don't have to be in ascending order either
    (float-list= (one-to-many .8 '(0 .1 .35 .7 .2))
                 '(0.08510638 0.12765959 0.23404254 0.38297874 0.17021276))))

;;; MDE Mon Feb  7 16:39:40 2022, Heidhausen
(sc-deftest test-utilities-average ()
  (sc-test-check
    (= 3 (average '(2 3 4)))
    (= 50.0 (average (loop for i to 100 collect i)))
    (= -50.0 (average (loop for i from 0 downto -100 collect i)))
    (> (average (loop repeat 10000000 collect (random 1000000000.0)))
       490000000.0)))

;;; MDE Tue Feb  8 18:02:47 2022, Heidhausen
(sc-deftest test-utilities-force-symmetrical ()
  (flet ((fsanok (mid min max list &optional (tolerance 0.00001d0))
           (let ((result (force-symmetrical-and-normalise
                          list :min min :max max :verbose t))
                 av smin smax)
             (loop for s in result maximize s into lmax minimize s into lmin
                   finally (setq smin lmin smax lmax))
             (setq av (average result))
             (print av) (print smin) (print smax)
             (and (equal-within-tolerance mid av tolerance)
                  ;; don't forget that after normalising probably only one of
                  ;; min and max will have been achieved 
                  (or (equal-within-tolerance max smax tolerance)
                      (equal-within-tolerance min smin tolerance))))))
    (sc-test-check
      (fsanok 0 -1 1 (loop repeat 10000 collect (+ .1 (random .4))))
      (fsanok -1 -2 0 (loop repeat 100000 collect (- (random 1.7) .1)))
      (fsanok 15.1 10.1 20.1 
              (loop repeat 1000000 collect (- (random 40.0) 50.0)) .001)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue May  8 21:14:56 2012 -- other random tests

(sc-deftest test-get-time-sig-ral ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax (alto-sax :midi-channel 1))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
           :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s s) ((2 4) h))))
                               (2 ((((4 4) h q e s s))))
                               (3 ((((4 4) h q e s s))))
                               (4 ((((4 4) h q q) ((2 4) q q)))))
           :rthm-seq-map '((1 ((sax (1 2 3 2 4 2 3 1 3 2 3 2 1 3 2)))))))
         (tsral (get-time-sig-ral (rthm-seq-map mini) (rthm-seq-palette mini)))
         (sax (get-data-data 'sax (get-data-data 1 tsral))))
    (sc-test-check
      (equalp '(4 1) (data (first sax)))
      (equalp '(3 2) (data (second sax))))))

(sc-deftest test-cmn-staff-args ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1
                                      :cmn-staff-args
                                      (staff-size .8 staff-lines 3)))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))))
                              (2 ((((4 4) h q e s s))))
                              (3 ((((4 4) h q e s s)))))
          :rthm-seq-map '((1 ((sax (1 2 3 2 3 2 3 1 3 2 3 2 1 3
                                      2))))))))
    #+cmn (probe-delete "/tmp/slippery-chicken-piece.eps")
    #+cmn (cmn-display mini)
    (sc-test-check
      ;; these will be cmn objects: just check there's something there 
      (cmn-staff-args (get-data 'sax (ensemble mini)))
      #+cmn (file-write-ok "/tmp/slippery-chicken-piece.eps" 30000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slippery-chicken tests

;;; SAR Wed May  9 13:10:38 BST 2012
(sc-deftest test-sc-num-notes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((fl (flute :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -)))))
          :rthm-seq-map '((1 ((fl (1))))))))
    (sc-test-check
      (= 8 (num-notes mini)))))

;;; SAR Wed May  9 14:42:27 BST 2012
;;; This test only checks that cmn is able to produce a file with the specified
;;; arguments, not that they work correctly.
#+cmn 
(sc-deftest test-sc-cmn-display-a ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                         (2 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                         (3 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (2 2 2 2 2))
                     (3 (3 3 3 3 3)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) q e s s h))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (3 ((((4 4) e s s h q))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((cl (1 3 2 1 2))
                              (hn (3 1 1 2 2))
                              (vc (1 1 3 2 2))))
                          (2 ((cl (3 1 1 2 2))
                              (hn (1 3 1 2 2))
                              (vc (3 2 2 1 1))))
                          (3 ((cl (1 1 3 2 2))
                              (hn (2 1 1 2 3))
                              (vc (3 1 1 2 2))))))))
    (sc-test-check
      (cmn-display mini 
                   :file "/tmp/slippery-chicken-piece.eps"
                   :players '(cl vc)
                   :in-c nil
                   :respell-notes nil
                   :auto-clefs nil
                   :start-bar 8
                   :end-bar 13
                   :title "CMN Fragment"
                   :size 13
                   :page-nums nil
                   :empty-staves t
                   :display-sets t
                   :write-section-info t
                   :display-time t
                   :staff-separation 2
                   :line-separation 3))))

;;; SAR Wed May  9 15:24:01 BST 2012
;;; This test only checks that cmn is able to produce a file with the specified
;;; arguments, not that they work correctly.
#+cmn 
(sc-deftest test-sc-cmn-display-b ()
  (progn 
    (defun kill-marks (event) (setf (marks event) nil))
    (let ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((fl (flute :midi-channel 1))
                         (cl (b-flat-clarinet :midi-channel 2))
                         (hn (french-horn :midi-channel 3))
                         (tp (b-flat-trumpet :midi-channel 4))
                         (vn (violin :midi-channel 5))
                         (vc (cello :midi-channel 6))))
            :staff-groupings '(2 2 2)
            :tempo-map '((1 (q 60)))
            :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                           (2 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                           (3 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
            :set-map '((1 (1 1 1 1 1))
                       (2 (2 2 2 2 2))
                       (3 (3 3 3 3 3)))
            :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                    :pitch-seq-palette ((1 2 3 4 5))
                                    :marks (a 1 s 2 slur 3 4 lhp 5)))
                                (2 ((((4 4) q e s s h))
                                    :pitch-seq-palette ((1 2 3 4 5))
                                    :marks (a 1 s 2 slur 3 4 lhp 5)))
                                (3 ((((4 4) e s s h q))
                                    :pitch-seq-palette ((1 2 3 4 5))
                                    :marks (a 1 s 2 slur 3 4 lhp 5))))
            :rthm-seq-map '((1 ((fl (3 1 1 2 2))
                                (cl (1 3 2 1 2))
                                (hn (3 1 1 2 2))
                                (tp (1 3 1 2 2))
                                (vn (3 2 2 1 1))
                                (vc (1 1 3 2 2))))
                            (2 ((fl (1 1 3 2 2))
                                (cl (3 1 1 2 2))
                                (hn (1 3 1 2 2))
                                (tp (2 1 1 2 3))
                                (vn (3 1 1 2 2))
                                (vc (3 2 2 1 1))))
                            (3 ((fl (1 3 2 1 2))
                                (cl (1 1 3 2 2))
                                (hn (2 1 1 2 3))
                                (tp (3 1 1 2 2))
                                (vn (3 2 2 1 1))
                                (vc (3 1 1 2 2)))))
            :rehearsal-letters '(3 5 8 13))))
      (probe-delete-multi "/tmp/" '("slippery-chicken-piece.eps" 
                                    "slippery-chicken-piece-1.eps"
                                    "slippery-chicken-piece-2.eps"
                                    "slippery-chicken-piece-3.eps"
                                    "slippery-chicken-piece-4.eps"))
      (sc-test-check
        (cmn-display mini 
                     :group-separation 3
                     :system-separation cmn::page-mark
                     :page-height 14.85
                     :page-width 10.5
                     :size 9
                     :all-output-in-one-file nil
                     :one-line-per-page t
                     :start-bar-numbering 7
                     :auto-bar-nums 3
                     :rehearsal-letters-all-players t
                     :tempi-all-players t
                     :process-event-fun #'kill-marks)
        (file-write-ok "/tmp/slippery-chicken-piece.eps" 64000)
        (file-write-ok "/tmp/slippery-chicken-piece-1.eps" 53000)
        (file-write-ok "/tmp/slippery-chicken-piece-2.eps" 53000)
        (file-write-ok "/tmp/slippery-chicken-piece-3.eps" 42000)
        (file-write-ok "/tmp/slippery-chicken-piece-4.eps" 412)))))

;;; SAR Wed May  9 15:37:24 BST 2012
;;; This test only checks that cmn is able to produce a file with the specified
;;; arguments, not that they work correctly.
#+cmn 
(sc-deftest test-sc-cmn-display-c ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((fl (flute :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((g6 a6 b6 c7 d7))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))
                                  :marks (a 1 s 2 slur 3 4 lhp 5)))
                              (2 ((((4 4) (w))))))
          :rthm-seq-map '((1 ((fl (1 2 2 2 1))))))))
    (sc-test-check
      (cmn-display mini
                   :automatic-octave-signs t
                   :multi-bar-rests t))))

;;; SAR Thu Jun  7 17:38:39 BST 2012
;;; This only checks that the :add-postscript functions without an error, but
;;; doesn't test that it adds the correct result
#+cmn 
(sc-deftest test-sc-cmn-display-d-add-postscript ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c3 d3 e3 f3 g4 a3 b3
                                 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) - e e - (s) e. - s s e -
                                    - s (s) s s -))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8 9)))))
          :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (probe-delete "/tmp/slippery-chicken-piece.eps")
    (sc-test-check
      (cmn-display 
       mini
       :add-postscript 
       `((1 
          ,(format nil "0.00 0.00 moveto ~%/Verdana findfont 9 scalefont ~
                        setfont ~
                        ~%(added by slippery-chicken) show"))))
      (file-write-ok "/tmp/slippery-chicken-piece.eps" 15000))))

;;; SAR Thu Jun  7 17:47:04 BST 2012
;;; This only checks that the :display-marks-in-part functions without an
;;; error, but doesn't test that it adds the correct result
#+cmn 
(sc-deftest test-sc-cmn-display-display-marks-in-part ()
            (let ((mini
                   (make-slippery-chicken
                    '+mini+
                    :title "mini"
                    :ensemble '(((vn (violin :midi-channel 1))))
                    :tempo-map '((1 (q 60)))
                    :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
                    :set-map '((1 (1)))
                    :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                            :pitch-seq-palette ((1 2 3)))))
                    :rthm-seq-map '((1 ((vn (1))))))))
              (add-mark-in-part (get-note mini 1 1 'vn) "with viola")
              (probe-delete "/tmp/minip.eps")
              (probe-delete "/tmp/mini.eps")
              (sc-test-check
               (cmn-display mini :file "/tmp/minip.eps" 
                            :display-marks-in-part t)
               (file-write-ok "/tmp/minip.eps" 7000)
               (cmn-display mini :file "/tmp/mini.eps")
               (file-write-ok "/tmp/minip.eps" 7000))))

;;; SAR Wed May  9 15:45:53 BST 2012
(sc-deftest test-sc-get-player ()
            (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((fl (flute :midi-channel 1))
                       (tp (b-flat-trumpet :midi-channel 2))
                       (vn (violin :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((fl (1 1 1 1 1))
                              (tp (1 1 1 1 1))
                              (vn (1 1 1 1 1))))))))
    (sc-test-check
      (player-p (get-player mini 'vn))
      (equalp (id (get-player mini 'vn)) 'vn))))

;;; SAR Wed May  9 17:49:35 BST 2012
(sc-deftest test-sc-num-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((fl (flute :midi-channel 1))
                       (tp (b-flat-trumpet :midi-channel 2))
                       (vn (violin :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((fl (1 1 1 1 1))
                              (tp (1 1 1 1 1))
                              (vn (1 1 1 1 1))))))))
    (sc-test-check
      (= 5 (num-bars mini)))))

;;; SAR Wed May  9 18:06:27 BST 2012
(sc-deftest test-sc-get-bar-from-ref ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s)
                                   (q e s s h)
                                   (e s s q h))
                                  :pitch-seq-palette ((1 2 3 4 5 
                                                         1 3 2 4 5 
                                                         3 5 2 4 1))))) 
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (3 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (get-bar-from-ref mini 2 'vc 3 2)
      (rthm-seq-bar-p (get-bar-from-ref mini 2 'vc 3 2))
      (equalp (rhythms-as-symbols 
               (rhythms (get-bar-from-ref mini 2 'vc 3 2)))
              '(q e s s h))
      (equalp
       (loop for e in (rhythms (get-bar-from-ref mini 2 'vc 3 2))
          collect (data (pitch-or-chord e)))
       '(D4 F4 E4 G4 A4)))))

;;; SAR Wed May  9 18:11:34 BST 2012
(sc-deftest test-sc-get-bar-num-from-ref ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s)
                                   (q e s s h)
                                   (e s s q h))
                                  :pitch-seq-palette ((1 2 3 4 5 
                                                         1 3 2 4 5 
                                                         3 5 2 4 1))))) 
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (3 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (= 27 (get-bar-num-from-ref mini 2 4 3)))))

;;; SAR Wed May  9 18:24:48 BST 2012
(sc-deftest test-sc-get-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s)
                                   (q e s s h)
                                   (e s s q h))
                                  :pitch-seq-palette ((1 2 3 4 5 
                                                         1 3 2 4 5 
                                                         3 5 2 4 1))))) 
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (3 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (get-bar mini 18 'cl)
      (rthm-seq-bar-p (get-bar mini 18 'cl))
      (equalp
       (rhythms-as-symbols 
        (rhythms (get-bar mini 18 'cl)))
       '(E S S Q H))
      (equalp
       (loop for e in (rhythms (get-bar mini 18 'cl))
          collect (data (pitch-or-chord e)))
       '(A3 C4 G3 B3 F3)))))

;;; SAR Wed May  9 18:42:31 BST 2012
(sc-deftest test-sc-count-notes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s)
                                   (q (e) s +s h)
                                   ((e) s (s) (q) h))
                                  :pitch-seq-palette ((1 2 3 4 5 1 3 2)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (3 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (= 62 (count-notes mini 2 11))
      (= 31 (count-notes mini 2 11 nil 'vc))
      (= 27 (count-notes mini 2 11 t 'vc)))))

;;; SAR Wed May  9 19:09:27 BST 2012
(sc-deftest test-sc-players ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((cl (1 1 1))
                              (hn (1 1 1))
                              (vc (1 1 1))))))))
    (sc-test-check
      (equalp (players mini) '(cl hn vc)))))

;;; SAR Wed May  9 19:25:19 BST 2012
(sc-deftest test-sc-next-event ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((cl (1 1 1))
                              (hn (1 1 1))
                              (vc (1 1 1)))))))
        tmp)
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (equalp 
       (loop for ne = (next-event mini 'vc)
          while ne
          collect (get-pitch-symbol ne))
       '(E4 NIL F4 NIL G4 E4 NIL F4 NIL G4 E4 NIL F4 NIL G4))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc t)
          while ne
          collect (get-pitch-symbol ne))
       '(E4 F4 G4 E4 F4 G4 E4 F4 G4))
      ;; MDE Tue Sep 22 16:10:09 2015 -- make sure starting on a rest bar gets
      ;; the first even of the next bar
      (force-rest-bar (get-bar mini 2 'hn))
      (not (next-event mini 'hn t 2))   ; start at bar 2
      (setq tmp (next-event mini 'hn t))
      (eq 'h (data tmp))
      (= 3 (bar-num tmp))
      ;; DJR Sun  1 Sep 2019 10:17:39 BST -- let's test that next-event ends
      ;; where it's supposed to
      (not (next-event mini 'cl nil 2))
      (equalp
       (loop for ne = (next-event mini 'cl nil nil 2)
          while ne
          collect (get-pitch-symbol ne))
       '(G3 NIL A3 NIL B3)))))

;;; SAR Wed May  9 21:20:39 BST 2012
(sc-deftest test-sc-get-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1)))
          :rthm-seq-palette '((1 ((((2 4) (e) e+e. 32 (32)))
                                  :pitch-seq-palette (((1) 2)))))
          :rthm-seq-map '((1 ((vn (1))))))))
    (sc-test-check
      (= 32 (data (get-rest mini 1 2 'vn)))
      (equalp (data (get-note mini 1 2 'vn)) "E.")
      (equalp (data (get-note mini 1 '(2 1) 'vn)) 'C4)
      (equalp (data (get-note mini 1 '(2 2) 'vn)) 'A4)
      (is-tied-from (get-note mini 1 1 'vn)))))

;;; SAR Wed May  9 21:27:44 BST 2012
(sc-deftest test-sc-get-rest ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (e) e+e. 32 (32)))
                                  :pitch-seq-palette (((1) 2)))))
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (vc (1 1 1))))))))
    (sc-test-check
      (event-p (get-rest mini 2 1 'vc))
      (is-rest (get-rest mini 2 1 'vc))
      (equalp (data (get-rest mini 2 1 'vc)) 'e))))

;;; SAR Wed May  9 21:35:47 BST 2012
(sc-deftest test-sc-get-event ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (e) e+e. 32 (32)))
                                  :pitch-seq-palette (((1) 2)))))
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (vc (1 1 1))))))))
    (sc-test-check
      (get-event mini 2 4 'vn)
      (event-p (get-event mini 2 4 'vn))
      (equalp (get-pitch-symbol (get-event mini 1 2 'vn)) '(c4 a4))
      (equalp (get-pitch-symbol (get-event mini 2 4 'vn)) 'd4)
      (equalp (data (get-event mini 2 4 'vn)) 32))))

;;; SAR Wed May  9 21:48:33 BST 2012: 
(sc-deftest test-sc-get-current-instrument-for-player ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                        (db (double-bass :midi-channel 2))))
           :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                    (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                   :pitch-seq-palette ((1 2 3 4 5)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                               (db (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))
                               (db (1 1 1 1 1)))))))
         (mgcifp1 (get-current-instrument-for-player 2 'sax 3 mini))
         (mgcifp2 (get-current-instrument-for-player 2 'sax 5 mini)))
    (sc-test-check
      (instrument-p mgcifp1)
      (equalp (id mgcifp1) 'alto-sax)
      (instrument-p mgcifp2)
      (equalp (id mgcifp2) 'tenor-sax))))

;;; SAR Wed May  9 21:56:14 BST 2012
(sc-deftest test-sc-get-instrument-for-player-at-bar ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                        (db (double-bass :midi-channel 2))))
           :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                    (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                   :pitch-seq-palette ((1 2 3 4 5)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                               (db (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))
                               (db (1 1 1 1 1)))))))
         (mgifpab1 (get-instrument-for-player-at-bar 'sax 3 mini))
         (mgifpab2 (get-instrument-for-player-at-bar 'sax 7 mini)))
    (sc-test-check
      (instrument-p mgifpab1)
      (equalp (id mgifpab1) 'tenor-sax)
      (instrument-p mgifpab2)
      (equalp (id mgifpab2) 'alto-sax))))

;;; SAR Wed May  9 22:05:26 BST 2012
(sc-deftest test-sc-get-transposition-at-bar ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1))))))))
    (sc-test-check
      (= -9 (get-transposition-at-bar 'sax 2 mini)))))

;;; SAR Wed May  9 22:11:22 BST 2012
(sc-deftest test-sc-num-seqs ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
          :set-map '((1 (1 1 1 1))
                     (2 (1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1))))
                          (2 ((sax (1 1 1))))
                          (3 ((sax (1 1 1 1 1))))))))
    (sc-test-check
      (equalp (loop for s from 1 to 3 collect (num-seqs mini s)) '(4 3 5)))))

;;; SAR Wed May  9 22:25:06 BST 2012
(sc-deftest test-sc-get-section-refs ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))
                         (2 ((ef3 fs3 b3 cs4 fs4 gs4 ds5 f5))))
          :set-map '((1 (1 1 2))
                     (2 (1 1 2))
                     (3 ((a (1 2 1))
                         (b ((x (1 2 1))
                             (y (1 1 2))))))
                     (4 ((a (1 1 1))
                         (b (1 1 1))
                         (c (1 1 1 1))))
                     (5 (1 1 1))
                     (6 (1 1 2))
                     (7 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1))))
                          (2 ((sax (1 1 1))))
                          (3 ((a ((sax (1 1 1))))
                              (b ((x ((sax (1 1 1))))
                                  (y ((sax (1 1 1))))))))
                          (4 ((a ((sax (1 1 1))))
                              (b ((sax (1 1 1))))
                              (c ((sax (1 1 1 1))))))
                          (5 ((sax (1 1 1))))
                          (6 ((sax (1 1 1))))
                          (7 ((sax (1 1 1))))))))
    (sc-test-check
      (= 1 (id (get-set-for-bar-num mini 13)))
      (= 2 (id (get-set-for-bar-num mini 11)))
      (= 34 (num-sequences (rthm-seq-map mini)))
      (= 34 (num-sequences (set-map mini)))
      (= 34 (num-sequences (set-map mini)))
      ;; MDE Tue Feb 13 14:55:00 2018
      (= 34 (count-ref (rthm-seq-map mini) 1))
      (= 28 (count-ref (set-map mini) 1))
      (= 6 (count-ref (set-map mini) 2))
      (equalp
       (get-section-refs mini 2 4)
       '((2) (3 A) (3 B X) (3 B Y) (4 A) (4 B) (4 C) (5))))))

;;; SAR Wed May  9 22:28:02 BST 2012
(sc-deftest test-sc-get-num-sections ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
          :set-map '((1 (1 1 1 1))
                     (2 (1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1))))
                          (2 ((sax (1 1 1))))
                          (3 ((sax (1 1 1 1 1))))))))
    (sc-test-check
      (= 3 (get-num-sections mini)))))

;;; SARThu May 10 11:40:04 BST 2012
(sc-deftest test-sc-get-all-section-refs ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
          :set-map '((1 (1 1 1))
                     (2 (1 1 1))
                     (3 ((a (1 1 1))
                         (b ((x (1 1 1))
                             (y (1 1 1))))))
                     (4 ((a (1 1 1))
                         (b (1 1 1))
                         (c (1 1 1 1))))
                     (5 (1 1 1))
                     (6 (1 1 1))
                     (7 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1))))
                          (2 ((sax (1 1 1))))
                          (3 ((a ((sax (1 1 1))))
                              (b ((x ((sax (1 1 1))))
                                  (y ((sax (1 1 1))))))))
                          (4 ((a ((sax (1 1 1))))
                              (b ((sax (1 1 1))))
                              (c ((sax (1 1 1 1))))))
                          (5 ((sax (1 1 1))))
                          (6 ((sax (1 1 1))))
                          (7 ((sax (1 1 1))))))))
    (sc-test-check
      (equalp (get-all-section-refs mini)
              '((1) (2) (3 A) (3 B X) (3 B Y) (4 A) (4 B) (4 C) (5) (6)
                (7)))))) 

;;; SAR Thu May 10 11:48:43 BST 2012
(sc-deftest test-sc-statistics ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
          :set-map '((1 (1 1 1))
                     (2 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1))))
                          (2 ((sax (1 1 1))))))))
    (sc-test-check
      (equalp (statistics mini nil)
              "            start-bar: 1
            end-bar: 6
            num-bars: 6
            start-time: 0.0
            end-time: 24.0
            start-time-qtrs: 0
            end-time-qtrs: 24.0
            num-notes (attacked notes, not tied): 30
            num-score-notes (tied notes counted separately): 30 
            num-rests: 0
            duration-qtrs: 24.0 
            duration: 24.0 (24.000)

"))))


;;; SAR Thu May 10 12:01:58 BST 2012
(sc-deftest test-sc-get-tempo ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :tempo-map '((1 (q 60)) (5 (e 72)) (7 (q. 176 "prestissimo")))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
          :set-map '((1 (1 1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1 1 1 1))))))))
    (sc-test-check
      (tempo-p (get-tempo mini 2))
      (= 60 (data (get-tempo mini 2)))
      (equalp (beat (get-tempo mini 2)) 'q)
      (tempo-p (get-tempo mini 6))
      (= 72 (data (get-tempo mini 6)))
      (equalp (beat (get-tempo mini 6)) 'e)
      (tempo-p (get-tempo mini 8))
      (= 176 (data (get-tempo mini 8)))
      (equalp (beat (get-tempo mini 8)) 'q.))))


;;; SAR Thu May 10 12:11:02 BST 2012
(sc-deftest test-sc-get-time-sig ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s)
                                   ((5 8) q e s s e)
                                   ((3 16) s e))
                                  :pitch-seq-palette ((1 2 3 4 5 1 2 3 4 5 1 
                                                         2))))) 
          :rthm-seq-map '((1 ((sax (1 1 1))))))))
    (sc-test-check
      (time-sig-p (get-time-sig mini 2))
      (equalp (data (get-time-sig mini 2)) '(5 8))
      (time-sig-p (get-time-sig mini 4))
      (equalp (data (get-time-sig mini 4)) '(4 4))
      (time-sig-p (get-time-sig mini 9))
      (equalp (data (get-time-sig mini 9)) '(3 16)))))

;;; SAR Thu May 10 12:36:05 BST 2012
(sc-deftest test-sc-shorten-large-fast-leaps ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 96)))
          :set-palette '((1 ((g3 a5 b6))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) e s 32 64 64 e s 32 64 64))
                                  :pitch-seq-palette ((1 5 1 5 1 5 1 5 1 5)))))  
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn t)
          while ne
                 collect (get-pitch-symbol ne))
       '(G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3
          B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6
          G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6))
      (shorten-large-fast-leaps mini :threshold 0.25 :verbose nil)
      (not (next-event mini 'vn nil 1))
      (equalp
       (loop for ne = (next-event mini 'vn t)
          while ne
                 collect (get-pitch-symbol ne))
       '(G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6
         G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6
         G3 B6 G3 B6 G3 B6 G3 B6 G3 B6 G3 B6)))))

;;; SAR Thu May 10 13:50:31 BST 2012
;;; UT'S FOR MIDI FILE ARGUMENTS GO HERE ONCE TICKETS 299-302 HAVE BEEN
;;; ADDRESSED 
(sc-deftest test-sc-midi-play-num-seqs-from-seq ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (hn (french-horn :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))) 
          :set-map '((1 (1 1 1 1 1 1 1))
                     (2 (1 1 1 1 1 1 1))
                     (3 (1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                  :pitch-seq-palette ((1 2 3))))
                              (2 ((((4 4) (q) e (s) s h))
                                  :pitch-seq-palette ((1 2 3))))
                              (3 ((((4 4) e (s) s h (q)))
                                  :pitch-seq-palette ((2 3 3))))
                              (4 ((((4 4) (s) s h (q) e))
                                  :pitch-seq-palette ((3 1 2)))))
          :rthm-seq-map '((1 ((cl (1 2 1 2 1 2 1))
                              (hn (1 2 1 2 1 2 1))
                              (vc (1 2 1 2 1 2 1))))
                          (2 ((cl (3 4 3 4 3 4 3))
                              (hn (3 4 3 4 3 4 3))
                              (vc (3 4 3 4 3 4 3))))
                          (3 ((cl (1 2 1 2 1 2 1))
                              (hn (1 2 1 2 1 2 1))
                              (vc (1 2 1 2 1 2 1))))))))
    (probe-delete "/tmp/md-test.mid")
    (sc-test-check
      (midi-play mini
                 :midi-file "/tmp/md-test.mid"
                 :start-section 2
                 :num-sections 1
                 :voices '(cl vc)
                 ;; MDE Thu Sep 17 17:34:02 2015 -- test the new functionality
                 ;; of allowing force-velocity to be a function
                 :force-velocity #'(lambda (event)
                                     (randomise (amplitude event) 10))
                 :from-sequence 2
                 :num-sequences 3)
      (file-write-ok "/tmp/md-test.mid" 19.2)
      (add-half-beat-rest mini 4)
      (time-sig-equal (get-time-sig mini 4) (make-time-sig '(9 8)))
      (= 8 (value (get-last-event (get-bar mini 4 'cl))))
      (is-rest (get-last-event (get-bar mini 4 'vc))))))

;;; DJR Sun  1 Sep 2019 10:34:29 BST
(sc-deftest test-map-over-notes ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
           :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                    (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                    (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1))
                      (3 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h e (s) (s) e+s+s))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))))
                           (3 ((sax (1 1 1 1 1)))))))
         (mon (map-over-notes mini 1 nil nil #'add-pitches 'c4 'd4)))
    (sc-test-check
      (equalp (num-notes mini)
              (first mon))
      (is-chord (get-event mini 1 1 'sax))
      (is-chord (get-note mini (num-bars mini) 3 'sax))
      (setf mon (map-over-notes mini 1 2 nil #'force-rest))
      (equalp 6 (first mon))
      (is-rest (get-event mini 1 1 'sax))
      (is-rest (get-event mini 2 5 'sax)))))

;;; DJR Sun  1 Sep 2019 11:04:46 BST
(sc-deftest test-map-over-events ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
           :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                    (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                    (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1))
                      (3 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h e (s) (s) e+s+s))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))))
                           (3 ((sax (1 1 1 1 1)))))))
         (moe (map-over-events mini 1 nil nil
                               #'(lambda (event)
                                   (if (is-rest event)
                                       (add-mark event 'pause)
                                       (add-pitches event 'c4 'd4))))))
    (sc-test-check
      (equalp (+ (num-score-notes (piece mini))
                 (num-rests (piece mini)))
              (first moe))
      (is-chord (get-event mini 1 1 'sax))
      (has-mark (get-event mini (num-bars mini) 3 'sax) 'pause)
      (setf moe (map-over-notes mini 1 2 nil #'force-rest))
      (equalp 6 (first moe))
      (is-rest (get-event mini 1 1 'sax))
      (is-rest (get-event mini 2 5 'sax)))))

;;; MDE Mon Jun 11 18:26:47 2012 -- 
(sc-deftest test-map-over-bars ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
           :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                    (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                    (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1))
                      (3 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h e (s) (s) e+s+s))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))))
                           (3 ((sax (1 1 1 1 1)))))))
         (mob (map-over-bars mini 1 nil nil #'consolidate-notes nil 'q))
         (cor (consolidate-all-rests mini 1 nil nil t)))
    ;; (print cor)
    ;; (print (first mob))
    (sc-test-check
      (= 15 (length mob))
      (= 4 (num-rhythms (first mob)))
      ;; MDE Sun Sep  7 18:05:58 2014 -- consolidate rests now returns rsb
      ;; object but following test didn't do much for us anyway as it always
      ;; used to return T whether it consolidated or not, so leave this. 
      ;; (equalp cor (ml t 15))
      (equalp '(h e e q) (loop for r in (rhythms (first mob))
                            collect (data r))))))

;;; MDE Tue Feb  6 15:51:42 2018
(sc-deftest test-map-over-sequenzes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((hn (french-horn :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) h h))
                                  :pitch-seq-palette ((1 2)))))
          :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1))))
                          (2 ((hn (2 2 2 2 2))
                              (vc (2 2 2 2 2))))
                          (3 ((hn (1 1 1 1 1))
                              (vc (1 1 1 1 1)))))))
        (list '()))
    (sc-test-check
      (map-over-sequenzes
       mini 'hn 
       #'(lambda (seq) (push (player-section-ref seq) list)))
      (equalp
       (reverse list)
       '((1 HN) (1 HN) (1 HN) (1 HN) (1 HN) (2 HN) (2 HN) (2 HN) (2 HN) (2 HN) 
         (3 HN) (3 HN) (3 HN) (3 HN) (3 HN)))
      (not (setq list nil))
      (map-over-sequenzes
       mini nil
       #'(lambda (seq) (push (num-bars seq) list)))
      (every #'(lambda (x) (= x 1)) list)
      (= 30 (length list)))))

(sc-deftest test-make-minimal-sc ()
  (let ((sc (make-minimal-sc 'test 'solo 'piano)))
    (sc-test-check
      (= 1 (num-bars sc))
      (equalp '(solo) (players sc)))))

;;; MDE Fri Jul 27 19:18:09 2018 -- test that subsets work properly
(sc-deftest test-subsets ()
  (let* ((lflu (make-l-for-lookup 'rsm nil
                                  '((1 (1 1 2 1 3 6))
                                    (2 (3))
                                    (3 (3 3 3 3 4 4 5))
                                    (4 (4 4 4 4 4 5 5 5))
                                    (5 (5 5 5 5 5 5 5 5 1 5 3))
                                    (6 (6 6 6 7))
                                    (7 (7 1 1 1 6 7 2)))))
         (num-seqs 20)
         (seq (get-linear-sequence lflu 1 num-seqs))
         (mini
          (make-slippery-chicken
           '+mini+
           :title "fm3"
           :ensemble '(((vln-one violin)
                        (vln-two violin)
                        (vla viola)
                        (vc cello)
                        (db double-bass)))
           :set-palette '((1 ((f2 a2 b2 cs3 e3 fs3 as3 b3 ds4 f4 g4 a4 b4
                                  d5 fs5 bf5 e6)
                              ;; mix up player IDs with instrument IDs to make
                              ;; sure both work 
                              :subsets ((cello (cs3)) (viola (fs3))
                                        (vln-one (ds4)) (vln-two (as3))
                                        (db (f2))))))
           :set-map `((1 ,(ml 1 num-seqs)))
           :tempo-map '((1 (q 160)))
           :rthm-seq-palette '((1 ((((5 8) q q e))))
                               (2 ((((2 8) q))))
                               (3 ((((5 8) q q.))))
                               (4 ((((5 8) q. q))))
                               (5 ((((4 8) q q))))
                               (6 ((((6 8) q. q.))))
                               (7 ((((6 8) q. q e)))))
           :rthm-seq-map `((1 ,(loop for ins in '(vln-one vln-two vla vc db)
                                  collect (list ins seq)))))))
    (sc-test-check
      (eq 'fs3 (get-pitch-symbol (get-note mini 1 1 'vla)))
      (eq 'cs3 (get-pitch-symbol (get-note mini 1 1 'vc)))
      (eq 'f2 (get-pitch-symbol (get-note mini 1 1 'db) nil))
      (eq 'ds4 (get-pitch-symbol (get-note mini 1 1 'vln-one)))
      (eq 'as3 (get-pitch-symbol (get-note mini 1 1 'vln-two))))))
    
;;; MDE Thu Sep  6 14:37:21 2018
(sc-deftest test-transposition-curve ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((f4))))
           :set-map '((1 (1 1 1 1 1 1 1 1 1 1)))
           :transposition-curve '(0 0 50 1 100 0)
           :rthm-seq-palette '((1 ((((4 4) e x 6 s x 4)))))
           :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1 1 1 1))))))))
    (sc-test-check
      (eq 'f4 (get-pitch-symbol (get-event mini 1 1 'vn)))
      (eq 'fs4 (get-pitch-symbol (get-event mini 5 1 'vn)))
      (eq 'fqs4 (get-pitch-symbol (get-event mini 3 1 'vn)))
      (eq 'fqs4 (get-pitch-symbol (get-event mini 7 1 'vn)))
      (eq 'f4 (get-pitch-symbol (get-event mini 10 1 'vn))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Apr  2 09:48:29 2012 

(in-scale :quarter-tone)

#+clm
(sc-deftest test-clm-play ()
  (declare (special cl-user::+slippery-chicken-home-dir+))
  (in-scale :chromatic)
  (let* ((tsdir (format nil "~atests"
                        cl-user::+slippery-chicken-home-dir+))
         (mini
          (make-slippery-chicken
           '+mini+
           :title "mini"
           :instrument-palette +slippery-chicken-standard-instrument-palette+
           :snd-output-dir "/tmp"
           :sndfile-palette
           ;; MDE Sat Dec 17 13:46:06 2016 -- make sure this works without the
           ;; .wav extension, i.e. that :extensions isn't set to nil when
           ;; make-sc calls make-sfp
           ;; `(((audio-1 (pink5s.wav)))
           `(((audio-1 (pink5s)))
             (,tsdir))
           :ensemble '(((vn (violin :midi-channel 1))
                        (vc (cello :midi-channel 2))))
           :tempo-map '((1 (q 60)))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1))
                      (2 (1 1 1))
                      (3 (1 1 1)))
           :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((vn (1 nil 1))
                               (vc (1 1 1))))
                           (2 ((vn (1 1 1))))
                           (3 ((vn (1 1 1))
                               (vc (1 1 1))))))))
    (probe-delete "/tmp/mini.mid")
    (probe-delete "/tmp/mini.eps")
    (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
    (sc-test-check
      (midi-play mini :midi-file "/tmp/mini.mid")
      (file-write-ok "/tmp/mini.mid" 400)
      #+cmn (cmn-display mini :file "/tmp/mini.eps")
      #+cmn (file-write-ok "/tmp/mini.eps" 25000)
      (clm-play mini 1 nil 'audio-1 :num-sections 3 :check-overwrite nil
                :src-width 5
                :play nil :header-type clm::mus-aiff)
      ;; (not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif" 6000000)
      ;; MDE Thu Mar 6 10:54:54 2014 -- make sure do-src works with numbers and
      ;; note-name symbols
      (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
      (clm-play mini 1 nil 'audio-1 :num-sections 3 :check-overwrite nil
                :src-width 5 :do-src 100
                :play nil :header-type clm::mus-aiff)
      ;; (not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif" 6000000)
      (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
      (clm-play mini 1 nil 'audio-1 :num-sections 3 :check-overwrite nil
                :src-width 5 :do-src 'c3
                :play nil :header-type clm::mus-aiff)
      ;; (not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif" 6000000)
      (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
      ;; DJR Thu 22 Aug 2019 15:17:56 BST
      ;; test decay-time arg
      (clm-play mini 1 'vn 'audio-1 :num-sections 2 :check-overwrite nil
                :do-src nil
                :decay-time 10
                :play nil :header-type clm::mus-aiff)
      (file-write-ok "/tmp/mini-1-vn-audio-1-seq1-3.aif" 6000000)
      (probe-delete "/tmp/mini-1-vn-audio-1-seq1-3.aif")
      ;; and should work with nil
      (clm-play mini 1 'vn 'audio-1 :num-sections 2 :check-overwrite nil
                :do-src nil
                :play nil :header-type clm::mus-aiff)
      ;;(not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-audio-1-seq1-3.aif" 5000000)
      (probe-delete "/tmp/mini-1-vn-audio-1-seq1-3.aif")
      ;; LMF Wed 21 Jul 2021
      ;; envelope argument for duration-scaler
      (clm-play mini 1 'vn 'audio-1 :num-sections 2 :check-overwrite nil
                :duration-scaler '(0 .1  100 2)
                :do-src nil
                :play nil :header-type clm::mus-aiff)
      (file-write-ok "/tmp/mini-1-vn-audio-1-seq1-3.aif" 5000000)
      (probe-delete "/tmp/mini-1-vn-audio-1-seq1-3.aif")
      ;; and for src src-scaler
      (clm-play mini 1 'vn 'audio-1 :num-sections 2 :check-overwrite nil
                :src-scaler '(0 .5  100 2) :src-width 5
                :play nil :header-type clm::mus-aiff)
      ;;(not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-audio-1-seq1-3.aif" 5000000))))

;;; MDE Wed Feb 24 20:07:17 2016
#+clm
(sc-deftest test-clm-play-1-section-and-nils ()
  (declare (special cl-user::+slippery-chicken-home-dir+))
  (in-scale :chromatic)
  (let* ((tsdir (format nil "~atests"
                        cl-user::+slippery-chicken-home-dir+))
         (mini
          (make-slippery-chicken
           '+mini+
           :title "mini"
           :instrument-palette +slippery-chicken-standard-instrument-palette+
           :snd-output-dir "/tmp"
           :sndfile-palette
           `(((audio-1 (pink5s.wav)))
             (,tsdir))
           :ensemble '(((vn (violin :midi-channel 1))
                        (vc (cello :midi-channel 2))))
           :tempo-map '((1 (q 60)))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((vn (1 nil 1))
                               (vc (nil 1 nil))))))))
    (probe-delete "/tmp/mini.mid")
    (probe-delete "/tmp/mini.eps")
    (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
    (sc-test-check
      (clm-play mini 1 nil 'audio-1 :num-sections 3 :check-overwrite nil
                :src-width 5
                :play nil :header-type clm::mus-aiff)
      ;; (not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif" 2500000)
      ;; MDE Thu Mar 6 10:54:54 2014 -- make sure do-src works with numbers and
      ;; note-name symbols
      (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
      (clm-play mini 1 nil 'audio-1 :num-sections 3 :check-overwrite nil
                :src-width 5 :do-src 100
                :snd-selector #'(lambda (sflist pitch event)
                                  (declare (ignore event))
                                  (get-nearest-by-freq
                                   (frequency pitch) (data sflist)))
                :pan-fun #'(lambda (event)
                             (declare (ignore event))
                             (random 90))
                :play nil :header-type clm::mus-aiff)
      ;; (not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif" 2500000)
      (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
      (clm-play mini 1 nil 'audio-1 :num-sections 3 :check-overwrite nil
                :src-width 5 :do-src 'c3 :pan-min-max '(30 60)
                :play nil :header-type clm::mus-aiff)
      ;; (not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif" 2500000)
      (probe-delete "/tmp/mini-1-vn-vc-audio-1-seq1-3.aif")
      ;; and should work with nil
      (clm-play mini 1 'vn 'audio-1 :num-sections 2 :check-overwrite nil
                :do-src nil
                :play nil :header-type clm::mus-aiff)
      ;;(not (sleep 1))
      (file-write-ok "/tmp/mini-1-vn-audio-1-seq1-3.aif" 2500000))))


;;; MDE Tue Apr 17 11:55:59 2012
#+clm 
(sc-deftest test-clm-play-psynch ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :title "mini"
          ;; :instrument-palette +slippery-chicken-standard-instrument-palette+
          :snd-output-dir "/tmp"
          :sndfile-palette
          `(((sines ((sine-a4.wav :frequency 440)
                     (sine-250.wav :frequency 250)))
             (sines2 ((sine-a4.wav :frequency 440)
                      (sine-250.wav :frequency 250))))
            (,(concatenate 'string
                           cl-user::+slippery-chicken-home-dir+ 
                           "tests")))
          :ensemble '(((vn (violin :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1))
                     (2 (1 1 1))
                     (3 (1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                  :pitch-seq-palette (((1) 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (vc (1 1 1))))
                          (2 ((vn (1 1 1))))
                          (3 ((vn (1 1 1))
                              (vc (1 1 1))))))))
    (probe-delete-multi "/tmp/" 
                        '("mini.eps"
                          "mini-1-vn-sines-to-sines2-seq1-3-psync.wav"
                          "mini-1-vn-vc-sines-seq1-3.wav"
                          "mini-1-vn-vc-sines-to-sines2-seq1-3-psync.wav"
                          "sine1mini-1-vn-vc-seq1-3.wav"
                          "sine2mini-2-vn-vc-seq1-3.wav"))
    #+cmn (cmn-display mini)
    (clm-play mini 1 nil 'sines :num-sections 3 :play nil :check-overwrite nil
              :data-format clm::mus-l24int :src-width 5
              :header-type clm::mus-riff)
    ;; test the output name when transitioning from sndfile group to group
    (clm-play mini 1 nil 'sines :num-sections 1 :play nil :pitch-synchronous t
              :data-format clm::mus-l24int
              ;; check extension guessing works
              :header-type clm::mus-riff  :src-width 5
              :sound-file-palette-ref2 'sines2 :check-overwrite nil)
    (clm-play mini 1 'vn 'sines :num-sections 1 :play nil :pitch-synchronous t
              :data-format clm::mus-l24int
              :sound-file-palette-ref2 'sines2 :check-overwrite nil
              :src-width 5 :header-type clm::mus-riff)
    (setf (sndfile-palette mini) nil)
    (clm-play mini 1 nil nil :num-sections 3 :play nil :check-overwrite nil
              :data-format clm::mus-l24int :src-width 5 :clm-ins #'clm::sine
              :header-type clm::mus-riff
              :output-name-uniquifier "sine1"
              :clm-ins-args '(:unused-arg-for-testing t))
    (clm-play mini 2 nil nil :num-sections 3 :play nil :check-overwrite nil
              :data-format clm::mus-l24int :src-width 5 :clm-ins #'clm::sine
              :header-type clm::mus-riff
              :output-name-uniquifier "sine2"
              :clm-ins-args #'(lambda (event event-num)
                                (print-simple event)
                                (print event-num)
                                (terpri)
                                nil))
    (sc-test-check
      ;; it would be best to verify these by eye and ear.  the pitch
      ;; synchronous files should match the score
      #+cmn (file-write-ok "/tmp/mini.eps" 30000)
      (file-write-ok "/tmp/mini-1-vn-sines-to-sines2-seq1-3-psync.wav" 2000000)
      (file-write-ok "/tmp/mini-1-vn-vc-sines-seq1-3.wav" 2000000)
      (file-write-ok "/tmp/mini-1-vn-vc-sines-to-sines2-seq1-3-psync.wav"
                     2000000)
      (file-write-ok "/tmp/sine1mini-1-vn-vc-seq1-3.wav" 2000000)
      (file-write-ok "/tmp/sine2mini-2-vn-vc-seq1-3.wav" 2000000))))

;;; MDE Sat Jun  2 12:52:38 2012 
#+clm
(sc-deftest test-clm-play-with-subsections ()
  (let* ((rsm (make-rthm-seq-map
               'test
               '((1
                  ((a ((vn (1 1 1))))
                   (b ((vn (1 1 1))))))
                 (2 ((vn (1 1 1 1))))
                 (3
                  ((a ((vn (1 1 1))))
                   (b
                    ((x ((vn (1 1 1))))
                     (y ((vn (1 1 1))))))))
                 (4
                  ((a ((vn (1 1 1))))
                   (b ((vn (1 1 1))))
                   (c ((vn (1 1 1 1)))))))))
         (mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1
                       ((a (1 1 1))
                        (b (1 1 1))))
                      (2 (1 1 1 1))
                      (3
                       ((a (1 1 1))
                        (b
                         ((x (1 1 1))
                          (y (1 1 1))))))
                      (4
                       ((a (1 1 1))
                        (b (1 1 1))
                        (c (1 1 1 1)))))
           :rthm-seq-palette '((1 ((((2 4) (q) e (s) s)))))
           :rthm-seq-map rsm
           :snd-output-dir "/tmp/"
           :sndfile-palette
           `(((grp-1
               (test-sndfile-1.aiff test-sndfile-2.aiff test-sndfile-3.aiff)))
             (,(file-from-sc-dir "tests/test-sndfiles-dir-1/"))))))
    (sc-test-check
      (= 29 (num-sequences (set-map mini)) (num-sequences (rthm-seq-map mini)))
      (= 6 (num-seqs mini 1))
      (= 4 (num-seqs mini 2))
      (= 9 (num-seqs mini 3))
      (= 10 (num-seqs mini 4))
      ;; 29 total seqs, each with 2 notes
      (= 58 (clm-play mini 1 nil 'grp-1 :check-overwrite nil :src-width 5))
      (= 46 (clm-play mini 2 nil 'grp-1 :check-overwrite nil :src-width 5))
      (= 12 (clm-play mini 3 nil 'grp-1 :num-sections 2 :check-overwrite nil
                      :src-width 5))
      (= 20 (clm-play mini 4 nil 'grp-1 :check-overwrite nil :src-width 5))
      (= 6 (clm-play mini 2 nil 'grp-1 :num-sequences 3 :num-sections 1
                     :src-width 5 :check-overwrite nil))
      ;; MDE Sat Jan 20 10:31:49 2018
      ;; (print rsm)
      (rthm-seq-map-p rsm)
      (rthm-seq-map-p (get-data-data 1 rsm))
      (rthm-seq-map-p (get-data-data '(1 a) rsm))
      (rthm-seq-map-p (get-data-data 2 rsm))
      (rthm-seq-map-p (get-data-data 3 rsm))
      (rthm-seq-map-p (get-data-data '(3 a) rsm))
      (rthm-seq-map-p (get-data-data '(3 b) rsm))
      (rthm-seq-map-p (get-data-data 4 rsm))
      (rthm-seq-map-p (get-data-data '(4 a) rsm))
      (add-player rsm 'tp)
      ;; make sure the :after method works
      (equalp (players rsm) '(vn tp))
      (equalp '(nil nil nil)
              (get-data-data '(1 a tp) rsm))
      ;; test add-player in the sc class
      (player-p (add-player mini 'trb :instrument 'tenor-trombone))
      (eq 'trb (player (get-event mini 1 1 'trb)))
      (equalp '(vn trb) (players mini))
      ;; MDE Sat Nov  3 10:13:54 2018
      (equalp '(vn trb vln vla)
              (add-ensemble-players
               mini (make-ensemble 'test '((vln violin) (vla viola)))))
      (is-rest-bar (get-bar mini 20 'vln))
      (is-rest-bar (get-bar mini 1 'vla))
      (is-rest-bar (get-bar mini 1 'trb))
      (is-rest-bar (get-bar mini (num-bars mini) 'trb))
      (not (empty-bars? mini 3 7 'vn))
      (force-rest-bars mini 3 7 'vn)
      (empty-bars? mini 3 7 'vn)
      (empty-bars? mini 3 7) ; both
      (empty-bars? mini 4 6 '(trb vn)) ; both explicit
      (not (empty-bars? mini 2 7)) ; both
      (not (empty-bars? mini 3 8 'vn))
      (empty-bars? mini 29 29 'trb) ; last bar
      (eq 'trb (player (get-event mini 1 1 'trb)))
      ;; MDE Fri Nov 23 19:00:34 2018 -- test the new all-rests arg
      (add-marks (get-event mini 1 2 'vn) '(p cresc-beg))
      ;; (print (get-event mini 1 2 'vn))
      (force-all-rests (get-bar mini 1 'vn) t)
      (not (empty-bars? mini 1 1 'vn))
      (empty-bars? mini 1 1 'vn t)
      (equalp (marks (get-event mini 1 2 'vn)) '(cresc-beg p))
      )))


;;; SAR Thu May 10 17:38:32 BST 2012
(sc-deftest test-sc-find-rehearsal-letters ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :rehearsal-letters '(2 5 7)
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
    (sc-test-check
      (equalp (find-rehearsal-letters mini) '(2 5 7)))))

;;; SAR Thu May 10 17:59:17 BST 2012
;;; SAR Thu May 10 17:59:17 BST 2012
(sc-deftest test-sc-check-slurs ()
  (let ((mini-1
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (beg-sl 1 end-sl 2))))
          :rthm-seq-map '((1 ((vn (1 1 1)))))))
        (mini-2
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (beg-sl 1 end-sl 4 beg-sl 2 end-sl 3
                                                 beg-sl 4))))
          :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (sc-test-check
      (check-slurs mini-1)
      (not (check-slurs mini-2 t))
      ;; MDE Fri Dec  3 11:11:38 2021, Heidhausen
      (print '1***********)
      (not (check-slurs mini-2 t)) ; this should fail but pass next time
      (print '2***********)
      (check-slurs mini-2 t)
      (print '3***********)
      (force-rest (get-event mini-2 3 2 'vn))
      ;;(add-mark-to-note mini-2 3 2 'vn 'beg-sl)
      (not (check-slurs mini-2 t))
      (not (check-slurs mini-2 t))
      (check-slurs mini-2)
      )))

;;; SAR Mon May 14 18:14:37 BST 2012
(sc-deftest test-sc-check-phrases ()
  (let ((mini-1
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (beg-phrase 1 end-phrase 2))))
          :rthm-seq-map '((1 ((vn (1 1 1)))))))
        (mini-2
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                  :marks (beg-phrase 1 end-phrase 4
                                                     beg-phrase 2 end-phrase
                                                     3 beg-phrase 4))))
          :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (sc-test-check
      (check-phrases mini-1)
      (not (check-phrases mini-2)))))

;;; MDE Thu May 26 13:14:26 2016 -- test functionality of auxiliary note
;;; functions 
(sc-deftest test-add-auxiliary-notes ()
  (let* ((h (pitch-list-stats '(cs4 d5 cs4 cs4 d5 f5)))
         (more '(cs4 d5 cs4 d5 gs5 cs4 f5 d5 g5 d5 f5))
         (cs4 (frequency (make-pitch 'cs4)))
         (d5 (frequency (make-pitch 'd5)))
         (f5 (frequency (make-pitch 'f5))))
    (sc-test-check
      (= cs4 (hash-least-used h :invert t :auto-inc nil))
      (= 3 (gethash cs4 h))
      (= 1 (gethash f5 h))
      (= 2 (gethash d5 h))
      (setf h (pitch-list-stats more))
      (= 3 (gethash cs4 h))
      (= 2 (gethash f5 h))
      (= 4 (gethash d5 h))
      (equalp
       '(cs4 d5 cs4 d5 gs5 cs4 f5 ef5 g5 d5 f5)
       (pitch-list-to-symbols
        (add-auxiliary-notes-aux (init-pitch-list more) :ignore '(cs4)))))))

;;; SAR Thu May 10 18:20:35 BST 2012
(sc-deftest test-sc-player-doubles ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                       (db (double-bass :midi-channel 2))))
          :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                   (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1))))
                          (2 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1))))))))
    (sc-test-check
      (player-doubles mini 'sax)
      (not (player-doubles mini 'db)))))


;;; SAR Thu May 10 18:24:27 BST 2012
(sc-deftest test-sc-get-starting-ins ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                       (db (double-bass :midi-channel 2))))
          :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                   (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1))))
                          (2 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1))))))))
    (sc-test-check
      (get-starting-ins mini 'sax)
      (instrument-p (get-starting-ins mini 'sax))
      (equalp (id (get-starting-ins mini 'sax)) 'alto-sax))))

;;; SAR Thu May 10 18:32:55 BST 2012
(sc-deftest test-sc-get-events-from-to ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                       (db (double-bass :midi-channel 2))))
          :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                   (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))  
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1))))
                          (2 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1))))))))
    (sc-test-check
      (every #'event-p (get-events-from-to mini 'sax 3 2 5 3))
      (equalp
       (loop for e in (get-events-from-to mini 'sax 3 2 5 3)
          collect (get-pitch-symbol e)
          collect (data e))
       '(FS4 Q AF4 E CS5 S EF5 S B3 H FS4 Q AF4 E CS5 S EF5 S B3 H FS4 Q AF4 
         E)))))

;;; SAR Thu May 10 18:50:55 BST 2012
(sc-deftest test-sc-transpose-events ()
  (let* ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))
                       (db (double-bass :midi-channel 2))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                              (db (1 1 1 1 1)))))))
         tes)
    (sc-test-check
      (equalp
       (loop for e in (get-events-from-to mini 'sax 3 2 5 3)
          collect (get-pitch-symbol e))
       '(EF4 AF4 BF4 EF5 CS4 EF4 AF4 BF4 EF5 CS4 EF4 AF4))
      (setf tes (transpose-events mini 'sax 3 2 5 3 11))
      (and (listp tes)
           (event-p (first tes)))
      (equalp
       (loop for e in (get-events-from-to mini 'sax 3 2 5 3)
          collect (get-pitch-symbol e))
       '(D5 G5 A5 D6 C5 D5 G5 A5 D6 C5 D5 G5)))))

;;; SAR Thu May 10 18:59:30 BST 2012
(sc-deftest test-sc-get-section ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax (alto-sax :midi-channel 1))
                        (db (double-bass :midi-channel 2))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))  
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                   :pitch-seq-palette ((1 2 3 4 5)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                               (db (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))
                               (db (1 1 1 1 1)))))))
         (mini-sec (get-section mini 2)))
    (sc-test-check
      (get-section mini 2)
      (equalp
       (loop for i in '(start-bar end-bar start-time end-time num-notes
                        duration) 
          collect (funcall i mini-sec))
       '(6 10 20.0 40.0 50 20.0)))))

;;; SAR Mon May 14 17:14:21 BST 2012
(sc-deftest test-sc-get-clefs ()
  (set-sc-config 'best-clef-aux-fun #'best-clef-aux)
  (progn
    (setf (starting-clef
           (get-data 'cello +slippery-chicken-standard-instrument-palette+))
          'bass)
    (let ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((vc (cello :midi-channel 1))))
            :tempo-map '((1 (q 96)))
            :set-palette '((1 ((g2 f4 e5))))
            :set-map '((1 (1 1 1)))
            :rthm-seq-palette '((1 ((((5 4) e e e e e e e e e e))
                                    :pitch-seq-palette
                                    ((1 1 2 2 2 2 3 3 3 1)))))
            :rthm-seq-map '((1 ((vc (1 1 1))))))))
      (auto-clefs mini)
      (sc-test-check
        (equalp
         (loop for i from 1 to 10 collect (get-clef mini 1 i 'vc))
         '(NIL NIL TENOR NIL NIL NIL TREBLE NIL NIL BASS))))))

(sc-deftest test-check-tuplets-ties ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((cl (b-flat-clarinet :midi-channel 1))))
           :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1)))
           :rthm-seq-palette '((1 ((((4 4) { 3 tq tq tq } +q e (s) s)))))
           :rthm-seq-map '((1 ((cl (1)))))))
         (e1 (get-event mini 1 1 'cl))
         (e4 (get-event mini 1 4 'cl)))
    (sc-test-check
      (equalp (bracket e1) '((1 3)))
      (is-tied-to e4)
      (check-tuplets mini #'warn)
      (check-ties mini nil #'warn)
      (not (setf (is-tied-to e4) nil))
      (not (check-ties mini t nil))
      (not (setf (bracket e1) nil))
      ;; MDE Fri May 18 13:43:37 2012 -- this should now return T as
      ;; check-tuplets now attempts to fix before failing
      (check-tuplets mini #'warn))))

#+clm
(sc-deftest test-wavelab-sfp ()
  (let* ((marker (test-suite-file "24-7.mrk"))
         (sndfile (test-suite-file "24-7.wav"))
         (sfp (make-sfp-from-wavelab-marker-file 
               marker sndfile
               :snds-per-group 8
               :random-every 5
               :paths '("/music/24-7/")))
         (sfpg (make-sfp-from-groups-in-wavelab-marker-file
                marker sndfile 
                :paths '("/music/24-7/")))
         (loops1 (parse-wavelab-marker-file-for-loops 
                  (test-suite-file "24-7loops1.mrk") :max-length 5))
         (loops2 (parse-wavelab-marker-file-for-loops 
                  (test-suite-file "24-7loops2.mrk") :max-length 5))
         (loops3 (parse-wavelab-marker-file-for-loops 
                  (test-suite-file "24-7loops3.mrk") :max-length 5))
         ;; a utilities fun which will also be used above but what the hell
         (sections (parse-wavelab-marker-file-for-sections
                    marker sndfile))
         (tapping (get-data-data "tapping" sfpg))
         (splinter (get-data-data "splinter" sfpg))
         (a1 (get-data-data "AUTO1" sfp)))
    (sc-test-check
      (equalp (get-all-refs sfpg) '(("tapping") ("splinter")))
      (= 2 (num-data sfp))              ; 2 groups made
      (= 8 (length a1))
      (= 4 (length tapping))
      (= 2 (length splinter))
      (= 50 (length loops1))
      (= 5 (length (first loops1)))
      (= 25 (length loops2))
      (= 7 (length loops3))
      (= 12 (length sections))
      (equal-within-tolerance 0.0914286 (start (first a1))))))

#+clm
(sc-deftest test-audacity ()
  (let* ((limine-loops (parse-audacity-label-file-for-loops
                        (test-suite-file "limine-audacity-loops.txt")))
         (af (test-suite-file "24-7loops1.txt")))
    ;; (print limine-loops)
    (when (probe-file af)
      (delete-file af))
    (wavelab-to-audacity-marker-file (test-suite-file "24-7loops1.mrk"))
    (sc-test-check
      (file-write-ok af 800)
      (= 540 (print (length limine-loops)))
      (= 7 (print (length (first limine-loops))))
      t)))

;;; SAR Wed May 16 17:17:04 EDT 2012
(sc-deftest test-sc-write-lp-data-for-all ()
  (sc-test-check
    (let ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((fl (flute :midi-channel 1))
                         (cl (b-flat-clarinet :midi-channel 2))
                         (vc (cello :midi-channel 3))))
            :staff-groupings '(2 1)
            :tempo-map '((1 (q 84)) (9 (q 72)))
            :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
            :set-map '((1 (1 1 1 1 1 1 1 1))
                       (2 (1 1 1 1 1 1 1 1))
                       (3 (1 1 1 1 1 1 1 1)))
            :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                    :pitch-seq-palette ((1 2 3))
                                    :marks (bartok 1)))
                                (2 ((((4 4) (q) e (s) s h))
                                    :pitch-seq-palette ((1 2 3)))))
            :rthm-seq-map '((1 ((fl (1 2 1 2 1 2 1 2))
                                (cl (1 2 1 2 1 2 1 2))
                                (vc (1 2 1 2 1 2 1 2))))
                            (2 ((fl (1 2 1 2 1 2 1 2))
                                (cl (1 2 1 2 1 2 1 2))
                                (vc (1 2 1 2 1 2 1 2))))
                            (3 ((fl (1 2 1 2 1 2 1 2))
                                (cl (1 2 1 2 1 2 1 2))
                                (vc (1 2 1 2 1 2 1 2)))))
            :rehearsal-letters '(3 11 19))))
      (write-lp-data-for-all mini 
                             :start-bar 7
                             ;; MDE Tue May 29 21:48:05 2012 -- I've updated
                             ;; this method to use the start-bar as the
                             ;; start-bar when numbering but just for thrills
                             ;; let's change it here
                             :start-bar-numbering 567
                             :end-bar 23
                             :paper "letter"
                             :landscape t
                             :respell-notes nil
                             :auto-clefs nil
                             :staff-size 17
                             :in-c nil
                             :barline-thickness 3.7
                             :top-margin 40
                             :bottom-margin 60
                             :left-margin 40
                             :line-width 22
                             :page-nums t
                             :all-bar-nums t
                             :use-custom-markup t
                             :rehearsal-letters-font-size 24
                             :lp-version "2.12.1"
                             :group-barlines nil
                             :page-turns t
                             :players '(fl cl)
                             :tempi-all-players t))))

;;; SAR Thu May 17 10:46:19 EDT 2012
(sc-deftest test-sc-make-sc ()
  (let ((mini 
         (make-slippery-chicken
          '+mini+
          :title "A Little Piece"
          :composer "Joe Green"
          :ensemble '(((fl ((flute piccolo) :midi-channel 1))
                       (cl (b-flat-clarinet :midi-channel 2))
                       (hn (french-horn :midi-channel 3))
                       (tp (b-flat-trumpet :midi-channel 4))
                       (vn (violin :midi-channel 5))
                       (va (viola :midi-channel 6))
                       (vc (cello :midi-channel 7))))
          :set-palette '((1 ((fs2 b2 d4 a4 d5 e5 a5 d6)))
                         (2 ((b2 fs2 d4 e4 a4 d5 e5 a5 d6)))
                         (3 ((cs3 fs3 e4 a4 e5 a5 e6))))
          :set-map '((1 (2 1 2 3 1 3 1))
                     (2 (1 1 3 2 2 3 1))
                     (3 (2 3 1 3 1 1 2)))
          :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                  :pitch-seq-palette ((1 2 3))))
                              (2 ((((4 4) (q) e (s) s h))
                                  :pitch-seq-palette ((2 1 3))))
                              (3 ((((4 4) e (s) s h (q)))
                                  :pitch-seq-palette ((3 2 1)))))
          :rthm-seq-map '((1 ((fl (2 3 3 1 1 1 2))
                              (cl (3 2 1 1 2 1 3))
                              (hn (1 2 3 1 1 3 2))
                              (tp (2 1 1 3 3 2 1))
                              (vn (3 1 3 2 1 1 2))
                              (va (2 1 1 1 3 2 3))
                              (vc (1 2 3 1 3 2 1))))
                          (2 ((fl (3 1 3 2 2 1 1))
                              (cl (1 1 2 3 1 3 2))
                              (hn (1 3 2 1 3 1 2))
                              (tp (1 1 1 3 3 2 2))
                              (vn (2 1 3 1 3 1 2))
                              (va (2 2 3 1 1 3 1))
                              (vc (1 3 1 2 2 1 3))))
                          (3 ((fl (1 1 3 2 1 3 2))
                              (cl (2 1 2 3 3 1 1))
                              (hn (3 2 1 1 1 3 2))
                              (tp (3 3 1 1 2 1 2))
                              (vn (3 1 3 2 1 1 2))
                              (va (3 2 1 1 3 2 1))
                              (vc (1 3 2 1 2 3 1)))))
          :tempo-curve '(5 q (0 40 25 60 50 80 75 100 100 120))
          :staff-groupings '(2 2 3)
          :instrument-change-map '((1 ((fl ((1 flute) (3 piccolo) (5 flute))))))
          :set-limits-low '((fl (0 c5 50 g5 100 c5))
                            (cl (0 c4 50 f4 100 c4))
                            (hn (0 f3 50 c4 100 f3))
                            (tp (0 c4 50 f4 100 c4))
                            (vn (0 e5 50 a5 100 e5))
                            (va (0 c3 50 f3 100 c3))
                            (vc (0 c2 50 f3 100 c2)))
          :set-limits-high '((fl (0 d6 50 a6 100 d6))
                             (cl (0 c5 50 a5 100 c5))
                             (hn (0 f4 50 c5 100 f4))
                             (tp (0 f5 50 c5 100 f5))
                             (vn (0 c6 50 e6 100 c6))
                             (va (0 g4 50 d5 100 g4))
                             (vc (0 c4 50 f4 100 c4)))
          :fast-leap-threshold 0.5
          :instruments-hierarchy '(fl vn cl tp va hn vc)
          :rehearsal-letters '(3 11 19)
          :avoid-melodic-octaves nil
          :instruments-write-bar-nums '(fl cl hn tp)
          :pitch-seq-index-scaler-min 0.1
          :bars-per-system-map '((1 1) (2 2) (3 3) (7 4) (11 5))
          :rthm-seq-map-replacements '(((1 va) 3 1) ((2 fl) 4 3))
          :set-map-replacements '((1 2 2) (3 3 1)))))
  (sc-test-check
    ;; MDE Tue May 27 17:49:52 2014 
    (= 21 (num-bars mini))
    (equalp "A" (rehearsal-letter (get-bar mini 2 'fl)))
    ;; MDE Thu Aug 28 18:40:11 2014 -- also just test our new ensemble method
    (equalp '(FL CL HN TP VN VA VC) (sort-players (ensemble mini)))
    (equalp '(CL HN TP VA VC) (sort-players (ensemble mini)
                                            :ignore '(fl vn)))
    (equalp '(VN FL TP CL HN VA VC)
            (sort-players (ensemble mini) :stats-fun #'total-degrees))
    ;; letters only on instruments at top of groups
    (not (rehearsal-letter (get-bar mini 2 'cl)))
    (setf (rehearsal-letters mini) '(4 12 18))
    (not (rehearsal-letter (get-bar mini 2 'fl)))
    (equalp "A" (rehearsal-letter (get-bar mini 3 'fl)))
    (equalp "B" (rehearsal-letter (get-bar mini 11 'hn)))
    (equalp "C" (rehearsal-letter (get-bar mini 17 'vn)))
    (not (rehearsal-letter (get-bar mini 17 'vc))))))

(sc-deftest test-sc-find-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((pno (piano :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s)
                                   (q (e) s +s h)
                                   ((e) s (s) (q) h))
                                  :pitch-seq-palette ((1 2 3 4 5 1 3 2))))
                              (2 ((((4 4) h (q) e s s)
                                   (q e s +s h)
                                   ((e) q. h))
                                  :pitch-seq-palette
                                  ((1 2 (3) 4 5 (1) 3 2 1 4)))))
          :rthm-seq-map '((1 ((pno (2 2 2 1 2))
                              (vc (1 1 1 1 1))))
                          (2 ((pno (2 2 2 1 2))
                              (vc (1 1 1 1 1))))
                          (3 ((pno (2 2 2 1 2))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (listp (find-note mini 'vc 'f4))
      (= 24 (length (find-note mini 'pno '(f3 g3 a3 b3))))
      (equalp
       (loop for e in (find-note mini 'vc 'f4) 
          collect (get-pitch-symbol e)
          collect (start-time e))
       '(F4 3.75 F4 8.5 F4 15.75 F4 20.5 F4 27.75 F4 32.5 F4 39.75 F4
         44.5 F4 51.75 F4 56.5 F4 63.75 F4 68.5 F4 75.75 F4 80.5 F4
         87.75 F4 92.5 F4 99.75 F4 104.5 F4 111.75 F4 116.5 F4 123.75 F4
         128.5 F4 135.75 F4 140.5 F4 147.75 F4 152.5 F4 159.75 F4 164.5
         F4 171.75 F4 176.5)))))

;;; SAR Thu May 17 14:21:47 EDT 2012
;;; A simple test for sc::update-slots that only spot-checks the start-time of
;;; one event after calling update-slots with a specified start-time
(sc-deftest test-sc-update-slots ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                  :pitch-seq-palette ((1 2 3)))))
          :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
    (sc-test-check
      (= 6.0 (start-time (get-event mini 4 1 'vn)))
      (update-slots mini nil 10.0)
      (= 16.0 (start-time (get-event mini 4 1 'vn))))))

;;; MDE Tue May 29 22:48:51 2012 -- test that we can have beams over rests and
;;; stemlets too!  
(sc-deftest test-rest-stems-beams ()
  (let* ((mini
         (make-slippery-chicken
          '+mini+
          :title "mini"
          :instrument-palette +slippery-chicken-standard-instrument-palette+
          :ensemble '(((pno (piano :midi-channel 1))))
          :tempo-map '((1 (q 69)))
          :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                         (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5))))
          :set-map '((1 (1 1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((2 4) - (s) e. - - 32 (32) (s) (e) -)))))
          :rthm-seq-map '((1 ((pno (1 1 1 1 1 1))))))))
    (sc-test-check
      (= 1 (beam (get-event mini 1 1 'pno)))
      (zerop (beam (get-event mini 1 2 'pno)))
      (write-lp-data-for-all mini :stemlet-length .75))))

;;; SAR Thu Jun  7 18:04:27 BST 2012
(sc-deftest test-sc-check-time-sigs ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vn (violin :midi-channel 1))
                       (va (viola :midi-channel 2))
                       (vc (cello :midi-channel 3))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) { 3 tq tq tq } +q e (s) s)))))
          :rthm-seq-map '((1 ((vn (1 1 1))
                              (va (1 1 1))
                              (vc (1 1 1))))))))
    (sc-test-check
      (check-time-sigs mini)
      (setf (time-sig (get-bar mini 1 'vn)) '(3 4))
      (and 
       (not (ignore-errors (check-time-sigs mini)))
       (nth-value 1 (ignore-errors (check-time-sigs mini)))))))


;;; SAR Mon Jun 18 13:33:04 BST 2012
(sc-deftest test-sc-auto-set-written ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((hn (french-horn :midi-channel 1))
                       (cl (b-flat-clarinet :midi-channel 2))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                              (cl (1 1 1 1 1))))
                          (2 ((hn (1 1 1 1 1))
                              (cl (1 1 1 1 1))))
                          (3 ((hn (1 1 1 1 1))
                              (cl (1 1 1 1 1))))))))
    ;; original state hn
    (not (next-event mini 'hn nil 1))
    (equalp
     (loop for ne = (next-event mini 'hn)
        while ne
        collect (data (written-pitch-or-chord ne)))
     '(C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4
       FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4
       C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4
       FS4 G4 C4 D4 E4 FS4 G4))
    ;; original state cl
    (not (next-event mini 'cl nil 1))
    (equalp
     (loop for ne = (next-event mini 'cl)
        while ne
        collect (data (written-pitch-or-chord ne)))
     '(E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4
       G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 
       E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4
       G4 A4 B4 E4 FS4 G4 A4 B4))
    ;; set hn to nil
    (not (next-event mini 'hn nil 1))
    (loop for ne = (next-event mini 'hn)
       while ne
       do (setf (written-pitch-or-chord ne) nil))
    ;; set cl to nil
    (not (next-event mini 'cl nil 1))
    (loop for ne = (next-event mini 'cl)
       while ne
       do (setf (written-pitch-or-chord ne) nil))
    ;; nil state hn
    (not (next-event mini 'hn nil 1))
    (every #'not
           (loop for ne = (next-event mini 'hn)
              while ne
              collect (written-pitch-or-chord ne)))
    ;; nil state cl
    (not (next-event mini 'cl nil 1))
    (every #'not
           (loop for ne = (next-event mini 'cl)
              while ne
              collect (written-pitch-or-chord ne)))
    ;; asw default
    (auto-set-written mini)
    ;; print change hn
    (not (next-event mini 'hn nil 1))
    (equalp
     (loop for ne = (next-event mini 'hn)
        while ne
        collect (data (written-pitch-or-chord ne)))
     '(C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4
       FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4
       C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 
       FS4 G4 C4 D4 E4 FS4 G4))
    ;; print change cl
    (not (next-event mini 'cl nil 1))
    (equalp
     (loop for ne = (next-event mini 'cl)
        while ne
        collect (data (written-pitch-or-chord ne)))
     '(E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4
       G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4
       E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4
       G4 A4 B4 E4 FS4 G4 A4 B4))
    ;; set to nil hn
    (not (next-event mini 'hn nil 1))
    (loop for ne = (next-event mini 'hn)
       while ne
       do (setf (written-pitch-or-chord ne) nil))
    ;; set to nil cl
    (not (next-event mini 'cl nil 1))
    (loop for ne = (next-event mini 'cl)
       while ne
       do (setf (written-pitch-or-chord ne) nil))
    ;; asw bar nums, all players
    (auto-set-written mini :start-bar 4 :end-bar 13) 
    ;; print change hn
    (not (next-event mini 'hn nil 1))
    (equalp
     (loop for ne = (next-event mini 'hn)
        while ne
        when (not (written-pitch-or-chord ne))
        collect (written-pitch-or-chord ne)
        when (written-pitch-or-chord ne)
        collect (data (written-pitch-or-chord ne)))
     '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL C4 D4 E4 FS4 
       G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4
       E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4
       G4 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
    ;; print change cl
    (not (next-event mini 'cl nil 1))
    (equalp
     (loop for ne = (next-event mini 'cl)
        while ne
        when (not (written-pitch-or-chord ne))
        collect (written-pitch-or-chord ne)
        when (written-pitch-or-chord ne)
        collect (data (written-pitch-or-chord ne)))
     '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL E4 FS4 G4 A4 
       B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4
       FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4
       A4 B4 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
    ;; set to nil hn
    (not (next-event mini 'hn nil 1))
    (loop for ne = (next-event mini 'hn)
       while ne
       do (setf (written-pitch-or-chord ne) nil))
    ;; set to nil cl
    (not (next-event mini 'cl nil 1))
    (loop for ne = (next-event mini 'cl)
       while ne
       do (setf (written-pitch-or-chord ne) nil))
    ;; asw bar nums, one player only
    (auto-set-written mini :start-bar 4 :end-bar 13 :players 'cl) 
    ;; print change hn
    (not (next-event mini 'hn nil 1))
    (every #'not
           (loop for ne = (next-event mini 'hn)
              while ne
              when (not (written-pitch-or-chord ne))
              collect (written-pitch-or-chord ne)
              when (written-pitch-or-chord ne)
              collect (data (written-pitch-or-chord ne))))
    ;; print change cl
    (not (next-event mini 'cl nil 1))
    (equalp
     (loop for ne = (next-event mini 'cl)
        while ne
        when (not (written-pitch-or-chord ne))
        collect (written-pitch-or-chord ne)
        when (written-pitch-or-chord ne)
        collect (data (written-pitch-or-chord ne)))
     '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL E4 FS4 G4 A4
       B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4
       FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4 A4 B4 E4 FS4 G4
       A4 B4 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clm.lsp tests

;;; SAR Thu May 10 21:45:19 BST 2012
#+clm
(sc-deftest test-clm-clm-loops ()
  (probe-delete "/tmp/test-sndfile-3-loops-from-00m00s180ms.aif")
  (sc-test-check
    (clm-loops 
     (concatenate 'string 
                  cl-user::+slippery-chicken-home-dir+
                  "tests/test-sndfiles-dir-1/test-sndfile-3.aiff")
     '(0.180 2.164 4.371 7.575 9.4 10.864)
     :fibonacci-transitions '(1 2 3 4 5)
     :max-perms 7
     :output-dir "/tmp/"
     :header-type clm::mus-aifc
     :channels 1
     :transpositions '(1 12 -12)
     :num-shuffles 3
     :src-width 20)
    ;; MDE Fri May 11 16:29:00 2012 -- changed name to reflect new scheme
    (file-write-ok
     "/tmp/test-sndfile-3-loops-from-00m00s180ms.aif"
     8600000)))

#+clm
(sc-deftest test-clm-clm-loops2 ()
  (sc-test-check
   (clm-loops
    (concatenate 'string
                 cl-user::+slippery-chicken-home-dir+ 
                 "tests/test-sndfiles-dir-1/test-sndfile-1.aiff")
    '(0.028 .288 .56 .83 1.103 1.652 1.927)
    ;; :stop-after 0.5
    :num-shuffles 4
    :srate 44100
    ;; :data-format +spl-data-format+
    :header-type clm::mus-riff
    ;;:sndfile-extension ".blah"
    :channels 2
    ;; :suffix "blah"
    ;; :src-width +spl-src-width+
    :max-perms 100
    :max-start-time 30
    :fibonacci-transitions '(13 8 5 3)
    ;; if transps are added, shuffling will take forever!
    :transpositions '(0 0.5 0.25 -0.25 -0.5))))

;;; SAR Thu May 17 13:49:22 EDT 2012
#+clm
(sc-deftest test-clm-clm-loops-all ()
  (probe-delete "/tmp/test-sndfile-3-loops-from-00m00s794ms-test.wav")
  (probe-delete "/tmp/test-sndfile-3-loops-from-00m00s787ms-test.wav")
  (probe-delete "/tmp/test-sndfile-3-loops-from-00m00s749ms-test.wav")
  (probe-delete "/tmp/test-sndfile-3-loops-from-00m00s744ms-test.wav")
  (sc-test-check
    (not (clm-loops-all
          (concatenate 'string 
                       cl-user::+slippery-chicken-home-dir+
                       "tests/test-sndfiles-dir-1/test-sndfile-3.aiff")
          '((0.794 0.961 1.061 1.161 1.318 1.436 1.536 )
            (0.787 0.887 0.987 1.153 1.310 1.510 )
            (0.749 0.889 1.056 1.213 1.413 )
            (0.311 0.411 0.611 0.729 )
            (0.744 0.884 1.002 ))
          :max-perms 6
          :fibonacci-transitions '(31 8 21 13)
          :max-start-time 1.0
          :output-dir "/tmp/"
          :srate 48000
          :data-format clm::mus-lfloat
          :header-type clm::mus-riff
          :channels 1
          :do-shuffles nil
          :start-after 0.7
          :stop-after 0.8
          :suffix "test"
          :transpositions '(1 12 -12)
          :transposition-offset -3
          :src-width 20))
    (file-write-ok "/tmp/test-sndfile-3-loops-from-00m00s794ms-test.wav" 2.09)
    (file-write-ok "/tmp/test-sndfile-3-loops-from-00m00s787ms-test.wav" 2.07)
    (file-write-ok "/tmp/test-sndfile-3-loops-from-00m00s749ms-test.wav" 2.19)
    (file-write-ok "/tmp/test-sndfile-3-loops-from-00m00s744ms-test.wav" 2.12)))

;;; SAR Mon Jun  4 12:50:56 BST 2012
#+clm
(sc-deftest test-clm-clm-random-loop-points ()
  (let* ((rlps (random-loop-points 
                "/tmp/outfile" 
                (concatenate
                 'string 
                 cl-user::+slippery-chicken-home-dir+
                 "tests/test-sndfiles-dir-1/test-sndfile-3.aiff")
                :min-points 3
                :max-points 7
                :min-dur 0.1
                :num-loop-sets 5
                :scalers '(1/1 2/1 3/2 5/3 7/5 11/7 13/11)))
         (rlps-durs (loop for rlpl in rlps
                       collect (loop for ep from 0 below (1- (length rlpl))
                                  collect (- (nth (1+ ep) rlpl) 
                                             (nth ep rlpl)))))
         (rlps-scaled (loop for sclr in '(1/1 2/1 3/2 5/3 7/5 11/7 13/11)
                         collect (* 0.1 sclr))))
    (sc-test-check
      (every #'(lambda (x) (and (>= (length x) 3)
                                (<= (length x) 7))) 
             rlps)
      (every #'(lambda (x) 
                 (every #'(lambda (y) 
                            (or (> y 0.1)
                                (equal-within-tolerance y 0.1))) 
                        x)) 
             rlps-durs)
      (= 5 (length rlps))
      (every #'(lambda (x) (notany #'not x))
             (loop for i in rlps-durs
                collect 
                (loop for j in i
                   collect 
                   (member j rlps-scaled
                           :test #'(lambda (x y) 
                                     (equal-within-tolerance x y
                                                             0.001)))))))))


;;; SAR Mon Jun  4 17:45:10 BST 2012: Adding MDE's ut
#+clm
(sc-deftest test-clm-random-loop-points-2 ()
  (sc-test-check
    (let* ((min-durs '(0.01 0.02 0.03 0.05 0.1 0.2))
           (rlps 
            (loop for min-dur in min-durs collect
                 (random-loop-points 
                  "/tmp/outfile.txt" 
                  (concatenate 'string 
                               cl-user::+slippery-chicken-home-dir+
                               "tests/pink5s.wav")
                  :min-dur min-dur :if-outfile-exists :overwrite)))
           (ok t))
      (loop for min-dur in min-durs and rlp in rlps 
         do
           (loop for seg in rlp do
                (loop for s1 in seg 
                   for s2 in (cdr seg) 
                   for diff = (- s2 s1)
                   do
                     (when (and (< diff min-dur)
                                (not (equal-within-tolerance diff min-dur)))
                       (warn "~&~a to ~a (~a) is < ~a" s1 s2 diff min-dur)
                       (setf ok nil))))
         finally (return ok)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; change-map tests

(sc-deftest test-change-map-cm-get-data ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
          :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                   (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                   (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                          (2 ((sax (1 1 1 1 1))))
                          (3 ((sax (1 1 1 1 1))))))))
    (sc-test-check
      (equalp (cm-get-data (instrument-change-map mini) '(1 sax) 2) 'alto-sax) 
      (equalp (cm-get-data (instrument-change-map mini) '(1 sax) 4) 'tenor-sax) 
      (equalp (cm-get-data (instrument-change-map mini) '(2 sax) 3) 'alto-sax) 
      (equalp (cm-get-data (instrument-change-map mini) '(2 sax) 6) 'tenor-sax) 
      (equalp (cm-get-data (instrument-change-map mini) '(3 sax) 3) 'alto-sax) 
      (equalp (cm-get-data (instrument-change-map mini) '(3 sax) 5)
              'tenor-sax))))

;;; SAR Tue May 22 10:53:13 EDT 2012
(sc-deftest test-change-map-find-nearest ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
          :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                   (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                   (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                          (2 ((sax (1 1 1 1 1))))
                          (3 ((sax (1 1 1 1 1))))))))
    (sc-test-check
      (equalp (data (find-nearest '(4 sax) (instrument-change-map mini)))
              '((3 1 ALTO-SAX) (4 1 TENOR-SAX))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bar-holder tests

;;; SAR Thu Jun 14 14:35:48 BST 2012
(sc-deftest test-bar-holder-get-note ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2 
                                 c3 d3 e3 f3 g3 a3 b3 
                                 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 (2) 3 4 5)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (pitch-p (get-note (piece mini) 3 '(2 1) 'vc))
      (equalp (data (get-note (piece mini) 3 '(2 1) 'vc)) 'd2)
      (chord-p (pitch-or-chord (get-note (piece mini) 3 2 'vc)))
      (equalp
       (loop for p in (data (pitch-or-chord (get-note (piece mini) 3 2 'vc)))
          collect (data p))
       '(d2 b2))
      (equalp (data (pitch-or-chord (get-note (piece mini) 5 3 'cl))) 'f3))))

;;; SAR Thu Jun 14 15:09:39 BST 2012
(sc-deftest test-bar-holder-change-pitches ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2 
                                 c3 d3 e3 f3 g3 a3 b3 
                                 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e (s) s))
                                  :pitch-seq-palette ((1 (2) 3 4)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl) 
          while ne 
          collect (get-pitch-symbol ne)) 
       '(E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 
         FS3 G3 NIL A3))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc) 
          while ne 
          collect (get-pitch-symbol ne)) 
       '(C2 (D2 B2) E2 NIL F2 C2 (D2 B2) E2 NIL F2 C2 (D2 B2) E2 NIL F2 C2 
         (D2 B2) E2 NIL F2 C2 (D2 B2) E2 NIL F2))
      (change-pitches (piece mini) 'cl 2 '((c4 d4 e4 f4)))
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl) 
          while ne 
          collect (get-pitch-symbol ne)) 
       '(E3 FS3 G3 NIL A3 D4 E4 FS4 NIL G4 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 
         FS3 G3 NIL A3))
      (change-pitches (piece mini) 'vc 3 '((c3 d e f) nil (g3 nil b c4)))
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc) 
          while ne
          collect (get-pitch-symbol ne))
       '(C2 (D2 B2) E2 NIL F2 C2 (D2 B2) E2 NIL F2 C3 D3 E3 NIL F3 C2 (D2 B2) E2  
         NIL F2 G3 (D2 B2) B3 NIL C4)))))

;;; SAR Thu Jun 14 15:24:17 BST 2012
(sc-deftest test-bar-holder-delete-all-marks ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2 
                                 c3 d3 e3 f3 g3 a3 b3 
                                 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e (s) s))
                                  :pitch-seq-palette ((1 (2) 3 4))
                                  :marks (a 1 s 2 te 3 as 4))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc) 
          while ne
          collect (marks ne))
       '((A) (S) (TE) NIL (AS) (A) (S) (TE) NIL (AS) (A) (S) (TE) NIL (AS) (A)
         (S) (TE) NIL (AS) (A) (S) (TE) NIL (AS)))
      (delete-all-marks (piece mini) 2 2 'vc)
      (not (next-event mini 'vc nil 1))
      (equalp
       (loop for ne = (next-event mini 'vc) 
          while ne
          collect (marks ne))
       '((A) (S) (TE) NIL (AS) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (A) (S) 
         (TE) NIL (AS) (A) (S) (TE) NIL (AS))))))

;;; SAR Thu Jun 14 15:58:41 BST 2012
(sc-deftest test-bar-holder-transpose-bars ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                       (vc (cello :midi-channel 2))))
          :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2 
                                 c3 d3 e3 f3 g3 a3 b3 
                                 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e (s) s))
                                  :pitch-seq-palette ((1 (2) 3 4)))))
          :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                              (vc (1 1 1 1 1))))))))
    (sc-test-check
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl) 
          while ne
          collect (get-pitch-symbol ne))
       '(E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 
         FS3 G3 NIL A3))
      (transpose-bars (piece mini) 11 1 2 'cl :destructively nil)
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl) 
          while ne
          collect (get-pitch-symbol ne))
       '(E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL A3 E3 
         FS3 G3 NIL A3))
      (equalp
       (loop for rsb in
            (transpose-bars (piece mini) 11 1 2 'cl :destructively nil)
          collect
            (loop for r in (rhythms rsb)
               when (written-pitch-or-chord r)
               collect (data (written-pitch-or-chord r))))
       '((EF4 F4 FS4 AF4) (EF4 F4 FS4 AF4)))

      (transpose-bars (piece mini) 11 1 2 'cl :destructively t)
      (not (next-event mini 'cl nil 1))
      (equalp
       (loop for ne = (next-event mini 'cl) 
          while ne
          collect (get-pitch-symbol ne))
       '(EF4 F4 FS4 NIL AF4 EF4 F4 FS4 NIL AF4 E3 FS3 G3 NIL A3 E3 FS3 G3 NIL
         A3 E3 FS3 G3 NIL A3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define this as a separate normal function so we can also test it from
;;; within maxmsp 
(defun max-play-test ()
  (let* ((sf1 (make-sndfile-ext 
              (concatenate 'string
                           cl-user::+slippery-chicken-home-dir+ 
                           "tests/sndfile-1.aiff")
              :start 0.3 :end 1.1 :frequency 653)))
    (max-play sf1 20 100 10)))

(defun max-cue-test ()
  (let* ((sf1 (make-sndfile-ext 
              (concatenate 'string
                           cl-user::+slippery-chicken-home-dir+ 
                           "tests/sndfile-1.aiff")
              :cue-num 2 :start 0.3 :end 1.1 :frequency 653
              :followers '(blah wah))))
    ;; (print sf1)
    (max-cue sf1)))


;;; MDE Sun Dec 16 13:37:20 2012, Koh Mak, Thailand :)
#+clm
(sc-deftest test-sndfile-ext ()
  (let ((sf1 (make-sndfile-ext 
              (concatenate 'string
                           cl-user::+slippery-chicken-home-dir+ 
                           "tests/sndfile-1.aiff")
              :start 0.3 :end 1.1 :frequency 653))
        (sf2 (make-sndfile-ext 
              (concatenate 
               'string
               cl-user::+slippery-chicken-home-dir+ 
               "tests/test-sndfiles-dir-1/test-sndfile-1.aiff")))
        (sf3 (make-sndfile-ext 
              (concatenate 'string
                           cl-user::+slippery-chicken-home-dir+ 
                           "tests/sndfile-1.aiff")
              :pitch 3 :pitch-curve 4 :bandwidth 10 :energy 2
              :harmonicity-curve 0))
        (sf4 (make-sndfile-ext 
              (concatenate 'string
                           cl-user::+slippery-chicken-home-dir+ 
                           "tests/sndfile-1.aiff")
              :followers '(blah blah blah) :pitch 3 :pitch-curve 4
              :bandwidth 10 :energy 2 :harmonicity-curve 1))
        (mct (max-cue-test))
        (mpt (max-play-test)))
    ;; (print sf4)
    (sc-test-check
      ;; first some stuff from test-sndfile-make-sndfile ()
      (equalp (data sf1)
              (concatenate 'string
                           cl-user::+slippery-chicken-home-dir+ 
                           "tests/sndfile-1.aiff"))
      (= 0.3 (start sf1))
      (= 1.1 (end sf1))
      (= 653 (frequency sf1))
      (= 1 (channels sf1))
      (= 24 (bitrate sf1))
      (= 44100 (srate sf1))
      (= 16 (bitrate sf2))
      (= 190518 (bytes sf2))
      (= 95232 (num-frames sf2))
      (string= "curve: 3: rising then falling"
               (get-data-data '(curve 3) (characteristics sf1)))
      (= 4 (set-characteristic sf3 'volume 4))
      (not (set-characteristic sf3 'volume 14 nil))
      (= 0 (set-characteristic sf3 'volume-curve 0))
      (equal-within-tolerance 0.12857 (proximity sf3 sf4) .00001)
      ;; should make no difference
      (= 2 (set-characteristic sf4 'bandwidth-curve 2))
      ;; will now match so proximity should be closer
      (equal-within-tolerance 0.12857 (proximity sf3 sf4) .00001)
      (= 2 (set-characteristic sf3 'bandwidth-curve 2))
      (equal-within-tolerance 0.09524 (proximity sf3 sf4) .00001)
      ;; (print (proximity sf3 sf4))
      ;; will no longer match so proximity should be further
      (zerop (set-characteristic sf3 'bandwidth-curve 0))
      ;; (print (proximity sf3 sf4))
      (equal-within-tolerance 0.28571 (proximity sf3 sf4) .00001)      
      (= 2 (second mct))
      (= 300.0 (fourth mct))
      (= 1100.0 (fifth mct))
      (= -1 (first (first mpt)))
      (equal-within-tolerance 320.0 (fifth (first mpt)) .0001) ;fade duration
      (equal-within-tolerance 480.0 (sixth (first mpt)) .0001))));begin fade out

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri May 17 21:55:39 2013 -- arising from the Essen workshops: make sure
;;; that setting a dynamic in an rsm results in amplitudes being set.  Also
;;; tests that we can algorithmically generate pitch-seqs

(sc-deftest test-rsp-dynamics ()
  ;; in-scale call has to be inside function so that it's set when it's called.
  (in-scale :quarter-tone)
  (flet ((auto-ps (num-notes &optional (kurve '(0 1 66 5 100 2)))
           (let ((skaliert (new-lastx kurve (1- num-notes))))
             (move-repeats (loop for i below num-notes collect
                                (round (interpolate i skaliert)))))))
    ;; MDE Tue Apr 19 18:39:57 2016 
    (set-sc-config 'verbose-pitch-selection t)
    (let* 
        ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vla (viola :midi-channel 1 :microtones-midi-channel 2))
                        (fag (bassoon :midi-channel 3
                                      :microtones-midi-channel 4))))
           :set-palette '((1 ((c4 e4 gs4 b4 ds5 d4)))
                          (2 ((cs4 gqf4 cqf5 fqs4 cs3 bf3 e5)))
                          (3 (2 :transposition -12)))
           ;; slow tempo otherwise the grace notes end up coming before
           ;; previous note  
           :tempo-map '((1 30))
           :rthm-seq-palette `((1 ((((2 4) (e.) s { 5  fs f32 fs f32
                                     fs f32 f32  })
                                    (q q))
                                   :pitch-seq-palette ((1 3 4 3 2 1 2 3 2 1)
                                                       (4 3 2 2 3 4 3 1 3 6))
                                   :marks (ffff 1 mp 4)))
                               (komp1 ((((5 8) - s. 32 - - +s 32 32 -
                                         ;; todo: this breaks the rendering of
                                         ;; of the lilypond file. 
                                         - { 5 +f32 fs { 3 60 x 3 } } -
                                         - { 3 +ts t32 t32 ts } - - +s s -))
                                       :pitch-seq-palette ,(auto-ps 12)))
                               (komp2 ((((5 8) - s. 32 - - +s 32 32 -  - +s s -
                                         - { 3 +ts t32 t32 ts } -
                                         ;; MDE Wed May 14 18:30:31 2014 --
                                         ;; added some grace notes here to make
                                         ;; sure we can write them in lilypond
                                         ;; and antescofo 
                                         - { 5 +f32 fs f32 g g g f32 } -))
                                       :pitch-seq-palette ,(auto-ps 14))))
           :set-map `((1 ,(fibonacci-transitions 99 '(1 2 3)))) 
           :rthm-seq-map `((1 ((vla ,(fibonacci-transitions 99 '(1 komp1 1))) 
                               (fag ,(fibonacci-transitions 99
                                                            '(1 komp2 1)))))))))
      (set-sc-config 'verbose-pitch-selection nil)
      (sc-test-check
        (write-lp-data-for-all mini :base-path "/tmp/")
        ;; MDE Wed May 14 20:04:38 2014 -- following the viola, we can see 3
        ;; grace notes with duration 0.025 at the end of bar 103. following the
        ;; bassoon we should see 0 duration notes at many points. See note in
        ;; write-antescofo for why we don't 'steal' duration from previous
        ;; note. 
        (= 2354 (write-antescofo mini 'vla :file "/tmp/rsp-dynamics.asco.txt"))
        (midi-play mini)
        (equalp '(1.0 1.0 1.0 0.5 0.5 0.5 0.5 0.5)
                (loop for i from 1 to 8 collect
                     (amplitude (get-note mini 1 i 'vla))))
        ;; MDE Fri Oct 19 12:34:45 2018 -- give this a crack too, while we're
        ;; at it.
        (eq 'fqs4 (get-pitch-symbol (get-note mini 25 8 'vla)))
        (in-scale :chromatic)
        (round-to-nearest mini)
        (eq 'f4 (get-pitch-symbol (get-note mini 25 8 'vla)))
        (in-scale :quarter-tone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Aug 28 13:46:55 2013 
(sc-deftest test-proportions ()
  (let ((pexp (pexpand 3 3 2 1))
        ;; MDE Wed Jul 22 19:46:03 2015 -- to check pexpand-section-length
        (pexp2 (rest (nth-value 1 (pexpand 2 3 6 4 5))))
        (lens '((a 972) (b 1944) ((b a d) 90) ((b a d e) 18) ((b d) 324)
                (c 1296) ((c a) 324) ((c a a c) 18) ((c a b) 108) ((c d c) 72)
                (d 1620) ((d b) 324) ((b f d e) 18) ((d e c) 72))))
    (sc-test-check
      (every #'not
             (loop for args in lens collect
                  (/= (second args)
                      (pexpand-section-length pexp2 (first args)))))
      (float-list=
       (pdivide 3/2 4 :duration 35 :increment t :halves t)
       '(0.0 3.4449766 5.595868 8.696347 10.6936035 13.550747 15.428142
         18.025545 19.77778 22.30621 24.0064 26.324125 27.918053 30.078053
         31.58647 33.580315 35.0))
      (float-list=
       (pdivide 3/2 4 :duration 35 :increment t :halves nil)
       '(0.0 3.4449766 6.5454555 9.402599 12.000002 14.52843 16.846155
         19.006155 21.000002 23.150894 25.148151 27.025547 28.777782
         30.477972 32.0719 33.58032 35.000004))
      (equalp (pdivide 3/2 4 :duration 35 :halves nil)
              (pdivide 3/2 4 :duration 35 :halves t))
      (float-list= (pdivide 9/2 1 :increment t)
                   '(0.0 0.8181818 1.0))
      (float-list=
       (pdivide 7/4 5 :increment t)    
       '(0.0 0.06921974 0.12900043 0.18321489 0.2303579 0.27813584
         0.31994152 0.3580622 0.39160842 0.43309143 0.46978796 0.50341666
         0.5333088 0.56316525 0.58982277 0.61436254 0.6363637 0.6721557
         0.7043685 0.73414457 0.7610391 0.78766763 0.81179976 0.8341904
         0.8545456 0.877791 0.8989853 0.91871786 0.93675905 0.95452577
         0.97081196 0.9860227 1.0000001)) 
      ;; MDE Tue Sep  3 10:59:14 2013 
      (not (pexpand-find 'd pexp nil))
      (= 1 (pexpand-find 'a pexp))
      (= 649 (pexpand-find 'b pexp))
      (= 1081 (pexpand-find 'c pexp))
      ;; MDE Tue Sep  3 14:49:17 2013 
      (every #'(lambda (x) (eq x T))
             (loop repeat 10 collect
                  (let* ((args 
                          (cons (1+ (random 4))
                                (loop repeat (+ 3 (random 4)) collect
                                     (1+ (random 6)))))
                         (pxlen (nth-value 2 (apply #'pexpand args))))
                    ;; the number we produce will be the sum of the
                    ;; proportions ^ (1+ generations)
                    (= pxlen (expt (loop for n in (rest args) sum n)
                                   (1+ (first args)))))))
      ;; MDE Tue Sep  3 10:46:07 2013
      (equalp (pexpand 2 3 2)
              '(1 (A) 6 (A A A B) 11 (A A A C) 16 (A A B) 21 (A A B B) 26
                (A B) 31 (A B A B)
                36 (A B A C) 41 (A B B) 46 (A B B B) 51 (A C) 56 (A C A B) 61
                (A C A C) 66
                (A C B) 71 (A C B B) 76 (B) 81 (B A A B) 86 (B A A C) 91 
                (B A B) 96 (B A B B)
                101 (B B) 106 (B B A B) 111 (B B A C) 116 (B B B) 121 
                (B B B B)))
      ;; MDE Fri Oct 11 19:16:34 2013 -- numbers not letters
      (equalp (pexpand 2 4 3 2 t) '(1 (1) 10 (1 1 1 2) 19 (1 1 1 3) 28 (1 1 1 4)
              37 (1 1 2) 46 (1 1 2 2) 55 (1 1 2 3) 64 (1 1 3) 73 (1 1 3 2) 82
              (1 2) 91 (1 2 1 2) 100 (1 2 1 3) 109 (1 2 1 4) 118 (1 2 2) 127 (1
              2 2 2) 136 (1 2 2 3) 145 (1 2 3) 154 (1 2 3 2) 163 (1 3) 172 (1 3
              1 2) 181 (1 3 1 3) 190 (1 3 1 4) 199 (1 3 2) 208 (1 3 2 2) 217 (1
              3 2 3) 226 (1 3 3) 235 (1 3 3 2) 244 (1 4) 253 (1 4 1 2) 262 (1 4
              1 3) 271 (1 4 1 4) 280 (1 4 2) 289 (1 4 2 2) 298 (1 4 2 3) 307 (1
              4 3) 316 (1 4 3 2) 325 (2) 334 (2 1 1 2) 343 (2 1 1 3) 352 (2 1 1
              4) 361 (2 1 2) 370 (2 1 2 2) 379 (2 1 2 3) 388 (2 1 3) 397 (2 1 3
              2) 406 (2 2) 415 (2 2 1 2) 424 (2 2 1 3) 433 (2 2 1 4) 442 (2 2
              2) 451 (2 2 2 2) 460 (2 2 2 3) 469 (2 2 3) 478 (2 2 3 2) 487 (2
              3) 496 (2 3 1 2) 505 (2 3 1 3) 514 (2 3 1 4) 523 (2 3 2) 532 (2 3
              2 2) 541 (2 3 2 3) 550 (2 3 3) 559 (2 3 3 2) 568 (3) 577 (3 1 1
              2) 586 (3 1 1 3) 595 (3 1 1 4) 604 (3 1 2) 613 (3 1 2 2) 622 (3 1
              2 3) 631 (3 1 3) 640 (3 1 3 2) 649 (3 2) 658 (3 2 1 2) 667 (3 2 1
              3) 676 (3 2 1 4) 685 (3 2 2) 694 (3 2 2 2) 703 (3 2 2 3) 712 (3 2
              3) 721 (3 2 3 2)))
      (equalp (pexpand 1 2 5) 
              '(1 (A) 8 (A B) 15 (B) 22 (B B) 29 (B C) 36 (B D) 43 (B E)))
      (equalp (nth-value 1 (pexpand 2 3 2 4))
              '(729
                (((A) 243)
                 (((A A) 81) (((A A A) 27) ((A A A A) 9) ((A A A B) 9)
                              ((A A A C) 9))
                  (((A A B) 18) ((A A B A) 9) ((A A B B) 9))
                  (((A A C) 36) ((A A C A) 9) ((A A C B) 9) ((A A C C) 9)
                   ((A A C D) 9)))
                 (((A B) 81) (((A B A) 27) ((A B A A) 9) ((A B A B) 9)
                              ((A B A C) 9))
                  (((A B B) 18) ((A B B A) 9) ((A B B B) 9))
                  (((A B C) 36) ((A B C A) 9) ((A B C B) 9) ((A B C C) 9) 
                   ((A B C D) 9)))
                 (((A C) 81) (((A C A) 27) ((A C A A) 9) ((A C A B) 9)
                              ((A C A C) 9))
                  (((A C B) 18) ((A C B A) 9) ((A C B B) 9))
                  (((A C C) 36) ((A C C A) 9) ((A C C B) 9) ((A C C C) 9) 
                   ((A C C D) 9))))
                (((B) 162)
                 (((B A) 81) (((B A A) 27) ((B A A A) 9) ((B A A B) 9)
                              ((B A A C) 9))
                  (((B A B) 18) ((B A B A) 9) ((B A B B) 9))
                  (((B A C) 36) ((B A C A) 9) ((B A C B) 9) ((B A C C) 9) 
                   ((B A C D) 9)))
                 (((B B) 81) (((B B A) 27) ((B B A A) 9) ((B B A B) 9) 
                              ((B B A C) 9))
                  (((B B B) 18) ((B B B A) 9) ((B B B B) 9))
                  (((B B C) 36) ((B B C A) 9) ((B B C B) 9) ((B B C C) 9) 
                   ((B B C D) 9))))
                (((C) 324)
                 (((C A) 81) (((C A A) 27) ((C A A A) 9) ((C A A B) 9)
                              ((C A A C) 9))
                  (((C A B) 18) ((C A B A) 9) ((C A B B) 9))
                  (((C A C) 36) ((C A C A) 9) ((C A C B) 9) ((C A C C) 9)
                   ((C A C D) 9)))
                 (((C B) 81) (((C B A) 27) ((C B A A) 9) ((C B A B) 9)
                              ((C B A C) 9))
                  (((C B B) 18) ((C B B A) 9) ((C B B B) 9))
                  (((C B C) 36) ((C B C A) 9) ((C B C B) 9) ((C B C C) 9)
                   ((C B C D) 9)))
                 (((C C) 81) (((C C A) 27) ((C C A A) 9) ((C C A B) 9)
                              ((C C A C) 9))
                  (((C C B) 18) ((C C B A) 9) ((C C B B) 9))
                  (((C C C) 36) ((C C C A) 9) ((C C C B) 9) ((C C C C) 9)
                   ((C C C D) 9)))
                 (((C D) 81) (((C D A) 27) ((C D A A) 9) ((C D A B) 9)
                              ((C D A C) 9))
                  (((C D B) 18) ((C D B A) 9) ((C D B B) 9))
                  (((C D C) 36) ((C D C A) 9) ((C D C B) 9) ((C D C C) 9)
                   ((C D C D) 9)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jun 28 15:11:45 2014 -- this is from Dan Ross's code, which hit
;;; errors until bug was fixed.
(sc-deftest test-get-events-sorted-by-time ()
  (let* ((rch
          (make-rthm-chain
           'test-rch 143
           '((((e) e)                   ; 4 in total
              (- s (s) (s) s -)
              ({ 3 (te) - te te - })
              ((e.) s)))
           '((((q q)                    ; the 2/4 bars: 4 total
               ((q) q)
               ((q) q)
               ((q) (s) e.)))
             ((((e.) s (e) e (s) e.)    ; the 3/4 bars: 4 total
               (- e e - (e) e (q))
               (- e. s - - +e e - (q))
               (q (e.) s (q)))))
           :players '(fl cl)))
         (mini
          (progn
            (create-psps (palette rch))
            (make-slippery-chicken
             '+mini+
             :ensemble '(((fl (flute :midi-channel 1))
                          (cl (b-flat-clarinet :midi-channel 2))))
             :set-palette '((1 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
             :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
             :tempo-map '((1 (q 120)))
             :rthm-seq-palette (palette rch)
             :rthm-seq-map rch))))
    (tie-all-last-notes-over-rests mini 1 60 '(fl cl))
    (get-events-sorted-by-time mini)
    (sc-test-check
      ;; MDE Sat Jan 20 10:27:29 2018
      (add-player (rthm-seq-map mini) 'tp)
      (equalp (ml nil (num-rthm-seqs rch))
              (get-data-data '(1 tp) (rthm-seq-map mini)))
      (= (num-notes (piece mini)) 376)
      (= (num-score-notes (piece mini))) 481)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sc-deftest test-activity-levels ()
  (let ((al (make-al))
        (ale (make-ale))
        (ale2 (make-ale '(0 1 100 10) 50))
        (ale3 (make-ale '(0 0 2 10 3 1) 10 2)))
    (sc-test-check
      (every #'(lambda (x) (eq x T))
             (loop repeat 201 collect (eq (active al) (active ale))))
      (reset al 2)
      (equalp (loop repeat 35 collect (active al 1))
              '(NIL NIL NIL NIL NIL NIL T NIL NIL NIL T NIL NIL NIL NIL
                NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL
                NIL NIL NIL NIL NIL NIL))
      (reset al 0)
      (equalp (loop repeat 35 collect (active al 1))
              '(T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL
                NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL
                T NIL NIL NIL NIL))
      (reset al 1)
      (equalp (loop repeat 35 collect (active al 1))
              '(NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
                NIL NIL T NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL
                NIL NIL NIL NIL T NIL))
      (equalp (loop repeat 101 collect (active ale2))
              '(T NIL NIL T NIL NIL NIL NIL NIL T NIL NIL NIL T T NIL
                NIL NIL T NIL T T NIL NIL T T T NIL T T T T NIL T T NIL
                T T NIL T T T T NIL T T T T T T NIL NIL NIL T NIL NIL
                NIL NIL NIL NIL T NIL NIL NIL T T NIL NIL NIL T NIL T T
                NIL NIL NIL T T NIL NIL T T NIL T T T T T T NIL T T T T
                T T T T T T NIL))
      (equalp (loop repeat 31 collect (active ale3))
              '(NIL NIL NIL NIL T T T T NIL NIL NIL NIL NIL NIL T T T
                NIL NIL NIL NIL NIL T T T T T NIL T NIL NIL))
      (reset al)
      (equalp (loop for i from 0 to 1 by 0.01 collect (flicker-round al i))
      '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0
        0 0 1 0 0 0 1 1 0 0 0 1 1 1 0 0 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1
        1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
      (reset al)
      (equalp (loop for i from 0 to 1 by 0.01 collect
                   (flicker-round al i .1 .9))
      '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 0 0
        1 0 0 0 1 0 1 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1 0 1 1 0 1 1 0 0 1 1 0 1 1 0
        1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1))
      (reset al)
      (equalp (loop for i from 0 to 1 by 0.01 collect
                   (flicker-round al i .1 .9))
              '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 1 0 1
                0 0 0 0 1 0 0 0 1 0 1 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1 0 1 1 0 1 1
                0 0 1 1 0 1 1 0 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1
                1 1 1 1 1))
      (reset al)
      (equalp (loop for i from 0 to 1 by 0.01 collect
                   ;; this should be the same as pure rounding
                   (flicker-round al i .5 .5))
              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
                1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                1 1 1 1 1))
      (equalp (loop for i from 1010 to 1011 by 0.01 collect
                   (flicker-round al i))
              '(1010 1010 1010 1010 1010 1010 1010 1010 1010 1010 1010 1010 1010
                1010 1010 1010 1010 1010 1010 1010 1010 1010 1010 1010 1010 1010
                1010 1010 1010 1010 1011 1010 1010 1011 1010 1010 1010 1010 1011
                1010 1010 1010 1011 1011 1010 1010 1010 1011 1011 1011 1010 1010
                1011 1011 1011 1010 1011 1011 1011 1011 1010 1011 1011 1011 1011
                1010 1011 1011 1011 1011 1011 1011 1011 1011 1011 1011 1011 1011
                1011 1011 1011 1011 1011 1011 1011 1011 1011 1011 1011 1011 1011
                1011 1011 1011 1011 1011 1011 1011 1011 1011)))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jan 18 12:11:49 2016 -- no need for sc-test-check here as problems
;;; will throw errors into the debugger
(sc-deftest test-octave-signs ()
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e x 6 s x 4))
                                   :marks (beg-sl 1 end-sl 2
                                                  beg-8va 1 end-8va 2
                                                  beg-8vb 9 end-8vb 10
                                                  beg-15ma 3 end-15ma 5
                                                  beg-15mb 7 end-15mb 8))))
           :rthm-seq-map '((1 ((vn (1 1 1)))))))
         (e1 (get-event mini 1 1 'vn))
         (e2 (get-event mini 1 2 'vn))
         (e3 (get-event mini 1 3 'vn))
         (p1 (make-pitch 'c5))
         (p2 (make-pitch 'e5))
         (p3 (make-pitch 'd6))
         (p4 (make-pitch 'fs6)))
    ;; MDE Tue Jun 16 12:26:55 2020, Heidhausen -- make sure changing one
    ;; notehead works, particularly x-head which seems to be the only lilypond
    ;; notehead that __needs__ to be case sensitive 
    ;; #+cmn (cmn-display mini)
    (add-mark p1 'circled-x)
    (add-mark p2 'x-head)
    (setf (pitch-or-chord e1) (make-chord (list p1 p2)))
    (add-mark e2 'triangle)
    (add-mark p3 'wedge)
    (add-mark p4 'square)
    (setf (pitch-or-chord e3) (make-chord (list p3 p4)))
    ;; so this should make the whole chord blue
    (add-mark e3 '(rgb (0 0 1)))
    (change-pitch mini 1 4 'vn '(c6 ef7))
    ;; access chord notes
    (add-mark-to-note mini 1 '(4 1) 'vn 'triangle-up)
    ;; but these make individual chord notes different colours
    (add-mark-to-note mini 1 '(4 1) 'vn '(rgb (.5 .5 .5)))
    (add-mark-to-note mini 1 '(4 2) 'vn '(rgb (0 1 0)))
    (add-mark-to-note mini 1 5 'vn 'flag-head)
    (add-mark-to-note mini 1 6 'vn '(rgb (0 0 1)))
    (write-xml mini)
    (lp-display mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jun 13 14:43:21 2016 -- handles hairpins and updating amps
(sc-deftest test-events-amplitudes ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((fl (flute :midi-channel 1))
                       (cl (b-flat-clarinet :midi-channel 2))
                       (hn (french-horn :midi-channel 3))
                       (tp (b-flat-trumpet :midi-channel 4))
                       (vn (violin :midi-channel 5))
                       (vc (cello :midi-channel 6))))
          :set-palette '((1 ((f3 g3 a3 b3 c4 dqs4 e4 f4 g4 a4 b4 c5)))
                         (2 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                         (3 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1)))
          ;; dynamics marks will affect following event's amplitudes when the
          ;; sequenz is created but this won't carry over sequenzes 
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))
                                  :marks (pp 1 f 4)))
                              (2 ((((4 4) q e s s h))
                                  :pitch-seq-palette ((1 2 3 4 5))
                                  :marks (ppp 1 mp 2 mf 5)))
                              (3 ((((4 4) e s s h q))
                                  :pitch-seq-palette ((1 2 3 4 5))
                                  :marks (ff 1 p 3 fff 4)))
                              (3a ((((4 4) e s s h q))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((fl (3 1 1 2 2))
                              (cl (1 3 2 1 2))
                              (hn (3 1 1 2 3a))
                              (tp (1 3 1 2 2))
                              (vn (3 2 2 1 1))
                              (vc (1 1 3 2 2)))))))
        a1 a2 a3)
    (flet ((get-amps (bar player)
             (loop for event in (rhythms (get-bar mini bar player)) collect
                  (amplitude event))))
      (setq a1 (get-amps 1 'fl)
            a2 (get-amps 2 'fl)
            a3 (get-amps 3 'cl))
      (sc-test-check
        (= (dynamic-to-amplitude 'ff) (first a1))
        (float-list= '(0.8 0.8 0.4 0.9 0.9) a1)
        (float-list= '(0.3 0.3 0.3 0.7 0.7) a2)
        (float-list= '(0.2 0.5 0.5 0.5 0.6) a3)
        ;; adding a dynamic mark does not affect the amplitude of following
        ;; events
        (add-mark-to-note mini 3 1 'cl 'f)
        (setq a3 (get-amps 3 'cl))
        (float-list= '(0.7 0.5 0.5 0.5 0.6) a3)
        (remove-dynamics (get-bar mini 3 'cl))
        (add-mark-to-note mini 3 1 'cl 'mf)
        (update-amplitudes mini)
        (float-list= (ml .6 5) (get-amps 3 'cl))
        ;; 3a has no dynamics so that mf at the end of seq 2 should prevail
        (float-list= (ml .6 5) (get-amps 5 'hn))
        (remove-dynamics (get-bar mini 5 'tp))
        (add-mark-to-note mini 1 1 'tp 'cresc-beg)
        (add-marks-to-note mini 3 1 'tp 'cresc-end 'f)
        (add-mark-to-note mini 3 2 'tp 'dim-beg)
        (add-marks-to-note mini 5 1 'tp 'dim-end 'pp)
        (update-amplitudes mini)
        (handle-hairpins mini)  
        (float-list= '(0.3 0.34 0.38 0.42 0.46 0.5 0.54 0.58 0.62 0.66 0.7 0.7
                       0.6555 0.6111111 0.56666666 0.5222222 0.47777778
                       0.43333334 0.3888889 0.34444445 0.3 0.3 0.3 0.3 0.3)
                      (loop for i from 1 to 5 appending (get-amps i 'tp))
                      .001)
        ;; MDE Thu Oct 25 11:05:32 2018 -- give this a go too
        (set-midi-channels mini '((fl 9 16) (tp 10)) 2 4)
        (= 1 (midi-channel (pitch-or-chord (get-note mini 1 1 'fl))))
        (= 4 (midi-channel (pitch-or-chord (get-note mini 1 1 'tp))))
        (= 9 (midi-channel (pitch-or-chord (get-note mini 2 1 'fl))))
        (= 10 (midi-channel (pitch-or-chord (get-note mini 4 1 'tp))))
        (= 4 (midi-channel (pitch-or-chord (get-note mini 5 1 'tp))))
        (= 1 (midi-channel (pitch-or-chord (get-note mini 5 1 'fl))))
        ;; this is dqs4 but we didn't change bar 5 so it's on the only channel
        ;; given in the ensemble definition : 1 
        (= 1 (midi-channel (pitch-or-chord (get-note mini 5 2 'fl))))
        ;; this we did change:
        (= 16 (midi-channel (pitch-or-chord (get-note mini 4 2 'fl))))
        ;; MDE Sat Jan 20 10:24:30 2018
        (add-player (rthm-seq-map mini) 'blah)
        (equalp '(nil nil nil nil nil)
                (get-data-data '(1 blah) (rthm-seq-map mini)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jul 11 21:38:43 2016  -- control wave tests
#+clm
(sc-deftest test-control-wave1 ()
  (let ((s (make-control-sine :period 14 :minimum 0 :maximum 3))
        (c (make-control-cosine :frequency '(0 1/4 50 1/2 100 5)
                                :minimum -.5 :maximum 2.5 :amp-env '(0 .1 100 1)
                                :duration 60
                                :sndfile "/tmp/cosine.wav"))
        ;; (s2 (make-control-sine :frequency 2 :minimum -3 :maximum 7))
        (saw (make-control-sawtooth :frequency '(0 4 80 1/4 100 1/4) :minimum 0
                                    :maximum 1 :duration 60))
        ;; we couldn't have a freq of 1000 unless rate was >= 2000
        (saw2 (make-control-sawtooth :frequency 1000 :duration 180 :rate 2000))
        (tri (make-control-triangle :frequency '(0 4 80 1/4 100 1/4) :minimum 0
                                    :maximum 1 :duration 30))
        (sq (make-control-square :frequency '(0 4 100 40) :duration 6
                                 :initial-phase pi))
        (triws (make-control-triangle :frequency '(0 4 100 40) :duration 6 
                                      :transfer '(-1 1 1 -1) ; phase inversion
                                      :amp-env '(0 .1 50 1 100 .5) :amp .7
                                      :sndfile "/tmp/triws.wav")))
    (sc-test-check
      (= 14 (period s))
      (eq 'sine (type s))
      (eq 'cosine (type c))
      (eq 'sawtooth (type saw))
      (eq 'triangle (type tri))
      (eq 'square (type sq))
      (eq 'triangle (type triws))
      (file-write-ok "/tmp/control-wave.wav" 23000)
      (file-write-ok "/tmp/triws.wav" 23000)
      (equal-within-tolerance 1.5 (get-data 0 s))
      (equal-within-tolerance 1.15 (get-data 0 c))
      ;; (print saw)
      ;; vorletzter
      (equal-within-tolerance .99975 (get-data 59999 saw nil))
      (equal-within-tolerance 0.0 (get-last saw))
      (= (1+ (* (rate saw) (duration saw))) (array-dimension (data saw) 0))
      (equal-within-tolerance 0.5 (get-data 0 tri))
      (equal-within-tolerance 0.0 (get-data 0 sq)) ; because phase = pi
      ;; (print triws)
      (equal-within-tolerance 0.0 (get-last triws)) ; because transfer
      (equal-within-tolerance 0.055999999 (get-data 5999 triws nil))
      (not (setf (transfer triws) nil))
      ;; transfer no longer done so no phase inversion
      (equal-within-tolerance -0.055999999 (get-data 5999 triws nil))
      ;; (print triws)
      (equal-within-tolerance 0.0 (get-data 6 triws)) ; full duration works
      (equal-within-tolerance 0.0 (get-last triws)) ; ditto
      (equalp -1.0 (get-data 0 saw2))
      (zerop (get-data 1 saw2 nil)) ; index, not seconds
      (equalp -1.0 (get-data 2 saw2 nil))))) ; ditto

;;; MDE Tue Jul 12 14:07:47 2016 -- a little piece perhaps
#+clm
(sc-deftest test-control-wave2 ()
  (let* ((duration (* 3 60))
         (fundamental 100)
         (harmonics (make-control-sine 
                     :frequency '(0 .1 100 1)
                     :minimum 1 :maximum 21 :duration duration
                     :amp-env '(0 .5 20 .8 30 .4 50 .9 70 .5 100 1)))
         (delta (make-control-sawtooth
                 :frequency '(0 .1 30 .2 50 .1 100 4)
                 :minimum 0.05 :maximum .3 :duration duration
                 :amp-env '(0 1 100 .1)))
         d h e
         (time 0.0)
         (events
          (loop while (<= time duration) do
               (setf d (get-data time delta)
                     h (get-data time harmonics)
                     e (make-event (freq-to-note (* (round h) fundamental))
                                   (* d .7) :duration t :start-time time))
               (incf time d)
             collect e)))
    (event-list-to-midi-file events :start-tempo 60)
    (sc-test-check
      (file-write-ok "/tmp/tmp.mid" 13000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May 23 12:01:49 2019 -- afu tests
(sc-deftest test-afu ()
  (let ((afu (make-afu :level 3 :minimum -3 :maximum 3)))
    (sc-test-check
      (= 3 (maximum afu))
      (= -3 (minimum afu))
      (= 113 (period afu))
      (= 32 (num-unique afu))
      (setf (minimum afu) 0.0)
      (setf (maximum afu) 1.0)
      (listp (setf (binlist afu) '(1 0 0 1 0 1 0 1 0 0 0)))
      (= 12 (num-unique afu))
      (= 11 (period afu))
      (equal-within-tolerance 1.0 (progn (get-next afu) (get-next afu)))
      (equalp (binlist-to-proportions '(1 0 0 1 0 1 0 1 0 0 0))
              '(3 2 2 4))
      (equalp (binlist-to-proportions '(1 0 0 1 0 1 0 1 0 0 0 1))
              '(3 2 2 4 1))
      (equalp (binlist-to-proportions '(0 0 0 1 0 0 1 0 1 0 1 0 0 0 1))
              '(3 2 2 4 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jul 16 19:06:02 2016 -- utilities macro
(sc-deftest test-popnew ()
  (let* ((l1 '(1 2 3 4 5 6 7))
         (l2 '("dog" "cat" "ant" "rat" "mouse" "chicken"))
         (l3 '())
         (l4 '(1 1 2 3 4 5)))
    (sc-test-check
      (not (popnew l3 'blah))
      (eq 1 (popnew l1 3))
      (equalp l1 '(2 3 4 5 6 7))
      (eq 3 (popnew l1 2))
      (equalp l1 '(2 4 5 6 7))
      (eq 2 (popnew l1 15))
      (equalp l1 '(4 5 6 7))
      (eq 2 (popnew l4 1))
      (equal l4 '(1 1 3 4 5))
      (string= "cat" (popnew l2 "dog" :test #'string=))
      (equalp l2 '("dog" "ant" "rat" "mouse" "chicken")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sc-deftest test-no-pitches-fine ()
  (set-sc-config 'pitch-seq-no-pitches-error nil)
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2 c3))))
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e x 6 s x 4)))))
           :rthm-seq-map '((1 ((vn (1 1 1))))))))
    (sc-test-check
      (is-rest-bar (get-bar mini 1 'vn)))
    (set-sc-config 'pitch-seq-no-pitches-error t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Thu 22 Aug 2019 15:48:52 BST
;;; sc-test-swap-marks
(sc-deftest test-swap-marks ()
  (let* ((mini (make-slippery-chicken  
                '+mini+ 
                :ensemble '(((flt (flute :midi-channel 1))))
                :staff-groupings '(1)
                :tempo-map '((1 (q 60)))
                :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                               (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
                :set-map '((1 (set1 set1 set2 set1 set1 set2)))
                :rthm-seq-palette
                '((seq1 ((((4 4) (q) (q) q q))   
                         :pitch-seq-palette (1 2)
                         :marks (pp 1)))  
                  (seq2 ((((4 4) (e) e q h)) 
                         :pitch-seq-palette (1 2 3)
                         :marks (p 1 a 1 s 1))))
                :rthm-seq-map '((1 ((flt (seq1 seq1 seq2 seq1 seq1 seq2))))))))
    (sc-test-check
      (not (has-mark (get-note mini 1 1 'flt) 'fff))
      (not (has-mark (get-note mini 6 1 'flt) 'f))
      (not (has-mark (get-note mini 6 1 'flt) 'te))
      (not (has-mark (get-note mini 6 1 'flt) 'pause))
      (= (swap-marks mini nil nil nil 'pp 'fff) 4)
      (zerop (swap-marks mini nil nil nil 'pp 'fff))
      (= (swap-marks mini 1 6 'flt '(p a s) '(f te pause)) 6)
      (zerop (swap-marks mini 1 6 'flt '(p a s) '(f te pause)))
      (has-mark (get-note mini 1 1 'flt) 'fff)
      (has-mark (get-note mini 6 1 'flt) 'f)
      (has-mark (get-note mini 6 1 'flt) 'te)
      (has-mark (get-note mini 6 1 'flt) 'pause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Wed  4 Sep 2019 18:11:36 BST
;;; test fast-microtone-to-chromatic
(sc-deftest test-fast-microtone-to-chromatic ()
  (set-standard-instrument-slot 'chords t 'computer)
  (set-standard-instrument-slot 'chord-function 'piano-chord-fun 'computer)
  (let ((mini (make-slippery-chicken  
               '+mini+
               :ensemble '(((one (computer :midi-channel 1))
                            (two (computer :midi-channel 2))))
               :set-limits-high '((one (0 f5 100 f5)))
               :set-limits-low '((two (0 f4 100 f4)))
               :avoid-used-notes t
               :staff-groupings '(2)
               :tempo-map '((1 (q 120)))
               :set-palette '((set1 ((C2 CQS2 D2 DQS2 E2 F2 FQS2 G2 GQS2 A2 AQS2
                                         B2 C3 CQS3 D3 DQS3 E3 F3 FQS3 G3 GQS3
                                         A3 AQS3 B3 C4 CQS4 D4 DQS4 E4 F4 FQS4
                                         G4 GQS4 A4 AQS4 B4 C5))) 
                              (set2 ((D2 EF2 E2 F2 GF2 G2 AF2 A2 BF2 B2 C3 CQS3
                                         D3 EF3 E3 F3 GF3 G3 AF3 A3 BF3 B3 C4
                                         CQS4 D4 EF4 E4 F4 GF4 G4 AF4 A4 BF4 B4
                                         C5 CQS5 D5))))  
               :set-map `((1 ,(fibonacci-transitions 10 '(set1 set2))))
               :rthm-seq-palette
               '((seq1 ((((4 4) { 3 - te (te) te - - te te te -
                          - te (te) te - - te te te - })
                         ({ 3 - te te te - - te te te -
                            - te te te - - te te te - }))
                        :pitch-seq-palette (((1) 3 (1) 2 3 (1) 2 3 (1) 2 3
                                             (1) 2 3 (1) 2 3 (1) 3 (1) 2 3))))
                 (seq2 ((((4 4) q (q) q q)(e. (s) s s e q. e))
                        :pitch-seq-palette (((1) (3) 4 1 (2) 4 5 6 7)))))
               :rthm-seq-map `((1 ((one ,(fibonacci-transitions
                                          10
                                          '(seq1 seq1)))
                                   (two ,(fibonacci-transitions
                                          10
                                          '(seq2 seq2)))))))))
    (sc-test-check
      (equalp
       (let* ((count 0))
         (map-over-notes mini nil nil nil
                         #'(lambda (ev)
                             (if (is-chord ev)
                                 (loop for p in (data (pitch-or-chord ev))
                                    do
                                      (when (micro-tone p)
                                        (incf count)))
                                 (progn
                                   (when (micro-tone (pitch-or-chord ev))
                                     (incf count))))))
         count)
       93)
      (equalp '(66 27) (fast-microtone-to-chromatic mini nil :threshold 10))
      (zerop
       (let* ((count 0))
         (map-over-notes mini nil nil nil
                         #'(lambda (ev)
                             (if (is-chord ev)
                                 (loop for p in (data (pitch-or-chord ev)) do
                                      (when (micro-tone p)
                                        (incf count)))
                                 (progn
                                   (when (micro-tone (pitch-or-chord ev))
                                     (incf count))))))
         count))
      (equalp '(0 0) (fast-microtone-to-chromatic mini nil :threshold 10)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Wed 18 Sep 2019 18:35:36 BST
;;; test-pitch-or-chord=
(sc-deftest test-pitch-or-chord= ()
  (let ((p1 (make-pitch 'c4))
        (p2 (make-pitch 'bs3))
        (p3 (make-pitch 'a4))
        (c1 (make-chord '(c4 e4 g4)))
        (c2 (make-chord '(c4 e4)))
        (c3 (make-chord '(bs3 ff4 g4)))
        (c4 (make-chord '(a4)))
        (e1 (make-event '(a4) 'e))
        (e2 (make-event 'a4 'e))
        (e3 (make-event 'as4 'e))
        (e4 (make-event 'bf4 'e)))
    (sc-test-check
      (null (pitch-or-chord= p1 p2))
      (pitch-or-chord= p1 p2 t)
      (null (pitch-or-chord= c1 c2))
      (null (pitch-or-chord= c1 c3))
      (null (pitch-or-chord= c1 c3))
      (pitch-or-chord= p3 c4)
      ;; DJR Mon 10 Feb 2020 16:28:07 GMT
      ;; test eharmonics again
      (pitch-or-chord= e3 e4 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Wed 18 Sep 2019 18:53:41 BST
;;; test-tie-repeated-notes
(sc-deftest test-tie-repeated-notes ()
  (let* ((mini (make-slippery-chicken  
                '+mini+ 
                :ensemble '(((pno (piano :midi-channel 1))))
                :staff-groupings '(1)
                :tempo-map '((1 (q 60)))
                :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                               (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
                :set-map '((1 (set1 set1 set2 set1 set1 set2)))
                :rthm-seq-palette
                '((seq1 ((((4 4) q (q) q q))   
                         :pitch-seq-palette (1 1 1)))  
                  (seq2 ((((4 4) (e) e q e (e) e e)) 
                         :pitch-seq-palette (1 1 1 (1) (1)))))
                :rthm-seq-map '((1 ((pno (seq1 seq1 seq2 seq1 seq1 seq2))))))))
    (sc-test-check
      (equalp (tie-repeated-notes mini nil nil nil) '(12)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sc-deftest test-hammer-friendly ()
  (let* ((mini (make-slippery-chicken  
                '+mini+ 
                :ensemble '(((pno (piano :midi-channel 1))))
                :staff-groupings '(1)
                :tempo-map '((1 (q 60)))
                :set-palette '((set1 ((fs2))) 
                               (set2 ((b2))))
                :set-map '((1 (set1 set2 set1 set2)))
                :rthm-seq-palette
                '((seq1 ((((4 4) s x 16)))))
                :rthm-seq-map '((1 ((pno (seq1 seq1 seq1 seq1))))))))
    (flet ((check (bnum enum &optional invert)
             (let ((e1 (get-event mini bnum enum 'pno))
                   (e2 (get-event mini bnum (1+ enum) 'pno)))
               (funcall (if invert #'> #'<)
                        (- (start-time e2) (end-time e1))) .03)))
      (sc-test-check
        (check 1 1)
        (check 1 2)
        (check 2 5)
        (check 4 15)
        (midi-play mini :midi-file "/tmp/before.mid")
        (equalp '(60) (make-hammer-friendly mini 'pno))
        (midi-play mini :midi-file "/tmp/after.mid")
        (check 3 15 t)
        (check 2 2 t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Mon 13 Jan 2020 17:07:53 GMT
(sc-deftest test-get-section-bar-nums ()
  (let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                          (2 ((sax (1 1 1 1 1))))
                          (3 ((sax (1 1 1 1 1))))))))
    (sc-test-check
     (get-section-bar-nums mini :end nil :start 2)
     (get-section-bar-nums mini)
     (null (ignore-errors (get-section-bar-nums mini :end -1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Thu 6 Feb 2020 13:33:33 GMT
;;; MDE Tue Apr  7 14:40:40 2020 -- now defunct
#|
(sc-deftest test-list-member ()
            (sc-test-check
             (list-member '(a b c) '(a b c))
             (null (list-member '(a b c) '(1 2 3)))
             (list-member '(a b c) '(1 2 c))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Mon 10 Feb 2020 18:27:15 GMT
(sc-deftest test-sharp-to-flat ()
            (let ((e1 (make-event 'bf4 'e))
                  (e2 (make-event 'as4 'q))
                  (c1 (make-chord '(bf4 bf5)))
                  (c2 (make-chord '(as4 bf5)))
                  (p1 (make-pitch 'df3))
                  (p2 (make-pitch 'cs3)))
              (sc-test-check
               (pitch-or-chord= e1 (sharp-to-flat e2))
               (chord= c1 (sharp-to-flat c2))
               (pitch= p1 (sharp-to-flat p2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Mon 10 Feb 2020 14:22:22 GMT

(sc-deftest test-flat-to-sharp ()
            (let ((e1 (make-event 'bf4 'e))
                  (e2 (make-event 'as4 'q))
                  (c1 (make-chord '(as4 bf5)))
                  (c2 (make-chord '(as4 as5)))
                  (p1 (make-pitch 'df3))
                  (p2 (make-pitch 'cs3)))
              (sc-test-check
               (pitch-or-chord= e2 (flat-to-sharp e1))
               (chord= c2 (flat-to-sharp c1))
               (pitch= p2 (flat-to-sharp p1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test write-list-to-coll
;;; DJR Tue 18 Feb 2020 15:58:07 GMT

(sc-deftest test-write-list-to-coll ()
  (let ((l '((hello!)(how are you?)(very well thank you.)(1 2 3 4))))
    (probe-delete "/tmp/sc-max-coll.txt")
    (probe-delete "/tmp/sc-max-coll-1.txt")
    (sc-test-check
      (write-list-to-coll l :base 6)
      (file-write-ok "/tmp/sc-max-coll.txt" 64)
      ;; Update DJR Tue 3 Mar 2020 13:56:30 GMT
      (write-list-to-coll l :base 15 
                          :alt-label #'(lambda (count)
                                         (let ((l '(foo bar)))
                                           (nth (mod count 2) l)))
                          :prefix "yes_"
                          :file "/tmp/sc-max-coll-1.txt")
      (file-write-ok "/tmp/sc-max-coll-1.txt" 64))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan 28 16:40:13 2021, Heidhausen -- reaper tests
(sc-deftest test-reaper ()
  (let ((sndfiles (get-sndfiles
                   (concatenate 'string
                                cl-user::+slippery-chicken-home-dir+
                                ;; only three in here
                                "tests/test-sndfiles-dir-2"))))
    (multiple-value-bind
          (items1 end-time1)
        (make-reaper-items1 sndfiles
                            '(w (w) q h.+h+e (h) (e) h (q.) w (w) (w) (e))
                            60
                            :input-start '(0 .1 .2)
                            :play-rate '(1 1.02 1 .98 1.01 1 1.02)
                            :preserve-pitch t)
      (multiple-value-bind
            (items2 end-time2)
          (make-reaper-items2 (append sndfiles sndfiles sndfiles)
                              '(w (w) q h.+h+e (h)) ; 3 attacks
                              63
                              :input-start .9
                              :play-rate 1.04
                              :preserve-pitch t)
        (let* ((rf1 (make-reaper-file 'otest1 items1 :cursor end-time1))
               (rf2  (make-reaper-file 'otest2 items2 :cursor end-time2))
               (items3 (make-reaper-items3 (append sndfiles sndfiles) .1))
               (rf3  (make-reaper-file 'otest3 items3 :cursor end-time2)))
          (probe-delete "/tmp/reaper-test.rpp")
          (probe-delete "/tmp/reaper-test2.rpp")
          (probe-delete "/tmp/reaper-test3.rpp")
          (sc-test-check
            ;; We'll get warnings about durations but ignore these for test
            ;; purposes 
            (write-reaper-file rf1 :file "/tmp/reaper-test.rpp")
            (write-reaper-file rf2 :file "/tmp/reaper-test2.rpp")
            (write-reaper-file rf3 :file "/tmp/reaper-test3.rpp")
            (assoc-list-p (tracks rf1))
            (assoc-list-p (tracks rf2))
            (assoc-list-p (tracks rf3))
            (file-write-ok "/tmp/reaper-test.rpp" 4200)
            (file-write-ok "/tmp/reaper-test2.rpp" 4200)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Dec 20 12:10:05 2021, Heidhausen -- an example from Simon Bahr that
;;; was causing problems for handle-ties

(sc-deftest test-simons-remote ()
  (let ((+remote-control+
         (make-slippery-chicken  
          '+remote-control+ 
          :title "Remote Control" 
          :instrument-palette
          '((rh (:lowest-written f3 :highest-written c6
                 :starting-clef treble
                 :chords t))
            (lh (:lowest-written c2 :highest-written g4
                 :starting-clef bass
                 :chords t)))
          :ensemble '(((rh (rh :midi-channel 1))
                       (lh (lh :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((A ((f2 c4 e4 f4 fs4 g4 gs4 a4 ds5 gs5 as5 b5)))
                         (A1 ((f2 c4 e4 f4 g4 a4)))
                         (A2 ((e4 fs4 gs4 ds5 gs5 as5 b5)))
                         (B ((c2 g2 bf2 df3 f3 g3 bf3 df4 d4 bf4 f5 g5)))
                         (B1 ((c2 g2 f3 g3 d4 f5 g5)))
                         (B2 ((bf2 df3 f3 bf3 df4 bf4)))
                         (C ((cs2 e2 d3 ds3 e3 gs3 a3 b4 c5 d5 e5 fs5)))
                         (C1 ((cs2 e2 ds3 e3 gs3 a3 b4 e5 fs5)))
                         (C2 ((d3 e3 a3 c5 d5 e5 fs5)))
                         (D ((d2 ds2 fs2 gs2 a2 b2 c3 fs3 b3 ds4 cs5 a5)))
                         (D1 ((fs2 a2 fs3 cs5 a5)))
                         (D2 ((b2 fs3 b3 ds4 a5))) 
                         (D3 ((ef2 af2 c3 ef4)))
                         (D4 ((d2 fs2 a2 c3 a5))))
          :set-map '((A (A1 A1 A2 A1 A2 A2 A1 A2 A))
                     (B (B1 B1 B2 B1 B2 B2 B1 B2 B))
                     (C (C1 C1 C2 C1 C2 C2 C1 C2 C))
                     (D (D4 D2 D1 D3 D2 D4 D3 D1 D)))
          :avoid-melodic-octaves nil
          :rthm-seq-palette
          '(;; 4x Intro/Outro
            (IO1
             ((((4 4) - e (32) 32 s - (s) s { 3 - te ts - } - s. 32 - { 3 - (ts)
                ts (ts) - } - (32) 32 s - { 3 - ts (ts) ts - })
               (w)
               (h (h))
               ( - e (32) 32 s - - (s) s - { 3 - te ts - } - s. 32 - { 3 - (ts)
                ts (ts) - } - (32) 32 s - { 3 - ts (ts) ts - })
               (- e (e) - - (s) s (e) -
                - 32 s (32) (32) s (32) - - s (s) (e) - )
               ({ 3 ts ts ts } s s - s s e - (h))
               (- e (e) - - (s) s (e) -
                - 32 s (32) (32) s (32) - - s (s) (e) - )
               ({ 3 ts ts ts } s s - s s e - - s s s s -
                { 3 ts ts ts } { 3 ts ts ts } ))
              :pitch-seq-palette (2 2 2 2 2 2 2 2 2 2 2 2 2
                                  2 2
                                  2 2 2 2 2 2 2 2 2 2 2 2 2
                                  3 3 3 3 3 3
                                  1 2 3 4 5 6 7 8
                                  3 3 3 3 3 3
                                  3 4 5 6 7 8 7 6 5 4 3 4 3 2 1 3 2 1)))
            (IO2
             ((((4 4) q. e q e e) (e e e e q e e)
               (q (e) e q e e) (q e e e e e e)
               (q. e q e e) (e e e e e e e e)
               (q. e q q) (q q h))
              :pitch-seq-palette (6 6 7 6 6 5 4 3 7 5 4 3
                                  5 6 7 8 7 3 2 1 2 3 4 5
                                  5 6 5 4 3 2 3 4 5 1 2 3 4
                                  7 6 7 8 6 7 8)))
            (IO3
             ((((4 4) { 3 - te (te) te - } { 3 - te (te) te - }
                (q) { 3 - te (te) te - } )
               (q { 3 - te (te) te - } { 3 - (te) te (te) - } (q))
               ( { 6 - ts ts ts ts ts ts - } q
                { 6 - ts ts ts ts ts ts - } q )
               ( q (q) q. { 3 - ts ts ts - } )
               ( q (q) q. { 3 - ts ts ts - } )
               ( { 3 - ts (ts) ts (ts) ts (ts) - } q
                { 6 - ts ts ts ts ts ts - } q )
               ( q (q) q. { 3 - ts ts ts - } )
               ( { 3 - te (te) te - } { 3 - te (te) te - }
                (q) { 3 - te (te) te - } ))
              :pitch-seq-palette (1 3 2 4 3 5
                                  10 9 8 9
                                  2 3 4 3 4 5 6 3 4 5 4 5 6 7
                                  4 9 2 8 5
                                  4 6 9 3 2
                                  1 3 6 2 4 6 8 9 10
                                  5 3 0 1 4
                                  9 7 8 6 7 5 6 4)))
            (IO4
             ((((4 4) w ) ( (q.) s (s) (s) s (q.) )
               ( (h) (e.) s (q) ) ( w )
               ( w ) ( (q.) s (s) (s) s (q.) )
               ( (h) (e.) s (q) ) ( w ))
              :pitch-seq-palette (6 5 7 4 8 3 6 9 1 9)))
            ;; 9x Main
            (M1
             ((((4 4) - e e - - s s s s - (s) s (s) s { 3 - te te te - } )
               ( - s s s (s) -  { 3 - te te (te) - } q. (e) ))
              :pitch-seq-palette
              (((11) 10 6 5 (11) 10 9 8 7 11 (10) 9 (11) (10) 8 (7) (11))
               (11 10 9 11 10 (8) 7 11 10 6 (5) 11 (10) 9 (8) (7) 11))))
            (M2
             ((((3 4) { 3 - (te) tq - } { 3 - (tq) te - } { 5 - (fe.) fe - } )
               ( { 5 - (fe.) fe - } { 5 - (fe.) fe - } q )
               ( s (s) s (s) e (e) (s) s (e) )
               ((5 4) q (q) q (q) q)
               ( (q) q (q) q (q)))
              :pitch-seq-palette
              ((1 2 3 4 (5) 6
                5 4 (7) 2
                3 4 (5) 6 7)
               (1 2 (8) 4 5
                4 3 2 (6)
                2 3 4 5 (6) 7))))
            (M3
             ((((4 4) { 3 - tq te - } (q) 
                { 3 - (te) tq - } (q) )
               ((3 4) q { 5 - (fe.) fe - } (q) )
               ( { 5 - fe. fe - } +h )
               ( { 3 - ts (ts) ts (ts) ts (ts) - } - s (s) s (s) - q)
               ( - s (s) s (s) - { 5 - (fe.) fe - } { 3 - te te te - } ))
              :pitch-seq-palette
              ((1 2 (6) 4 (5) 6
                (5)
                6 7 (8) 6 7 (8) 6 7 (8) 6 7 (8))
               (7
                (8) 9 10 (8) 9 10 (8) 9 10 (8) 9 10
                9 (8) 7 6 (6) 5))))
            (M4
             ((((3 4) { 5 - fe. fe - } { 5 - fe. fe - } q )
               ( { 5 - fe. fe - } { 3 - tq te - } q )
               ((5 4) q q q q q)
               ( h. { 3 - +tq te - } { 3 - te te te - })
               ((4 4) { 3 - ts ts ts - } e - s s s (s) - (q)
                { 5 - fs fs fs (fe) - } ))
              :pitch-seq-palette
              (((10) (11) (12)
                (7) 8 10 1 3 5 (7) 8 10
                (13) (11) (12) 5 5
                9 7 5 1 3 5 9 7 5 7 9 5 3)
               (10 9 7 6 (4) 3
                1 2 (4) 5 (7) 8 1 2 (5) 6 7 8
                (9) (9)
                10 9 7 6 4 3 10 9 7 9))))
            (M5
             ((((1 4) q )
               ((6 8) - e e e - q. )
               ( - e e s s - q. )
               ((3 4) - e e - q (q))
               ( h. ))
              :pitch-seq-palette
              ((2 3 4 5
                (13) 4 5 6 7 (8)
                2 (13) 4 5)
               (1 4 5 (12) 9 7
                8 5 2 (10)
                (11) 4 8 7))))
            (M6
             ((((2 4) h ) ( h ) ( h )
               ((4 4) { 3 - te te te - } q h )
               ((3 4) { 6 - ts ts ts ts te - } +h )
               ((4 4) - s s s s - q +h ))
              :pitch-seq-palette
              (((10)
                (10) (12)
                1 2 3 4 (5) 1 2 3 4 (5) 10 9 8 7 (6))
               ((10)
                (10) (9)
                10 9 8 7 (6) 10 9 8 7 (6) 1 2 3 4 (5)))))
            (M7
             ((((3 4) { 3 - (tq) te - } - (s) s (e) - (q) )
               ( { 3 - (tq) te - } - (s) s (e) - (q) )
               ( { 3 - (tq) te - } - (s) s s (s) - q )
               ( { 5 - (fe.) fe - } - (s) s (e) - (q) )
               ( { 3 - (tq) te - } - (e) s (s) - (q) )
               ( - e e - - s s (e) - (q)))
              :pitch-seq-palette
              ((6 7 (5) 4 (8) 9 3 2 9 8 (11) 2 3 4 5 (6))
               (12 11 10 9 8 (7) 1 3 4 7 (8) 3 4 5 6 7))))
            (M8
             ((((4 4) - e (e) - { 3 - te (te) te - } { 3 - tq te - } (q) )
               ( { 5 - (fe.) fs fs - } { 5 - fs fs fe. - } +h )
               ( { 6 - ts ts ts ts te - } +q +h )
               ( - s s s s - +q +h )
               ( - s s (e) - { 5 - fs fs fe. - } +h ))
              :pitch-seq-palette
              ((2 3 (14) 5 9 8 7 (6) 5 9 8 7 (6) 5
                3 (11) 4 (5) (9) 9 7 5 3 (11))
               (2 3 (14) 5 9 8 7 (6) 5 9 8 7 (6) 5
                9 7 5 3 (11) 3 (11) 4 (5) (9)))))
            (M9
             ((((4 4) q \+8 (e) - e e - { 3 - te te te - } )
               ( { 3 - te te (te) - } (s) s (s) s - s s s s - - s s s (s) - ))
              :pitch-seq-palette
              (((11) (10) 9 11 10 8 (7) 11 (10) 6 5 11 (10) 9 (8) 7 (11))
               (11 10 9 (11) 11 10 8 (7) 11 10 (6) 5 10 (9) 8 (7) 11))))
            ;; 1x ZAPP!
            (zapp-up
             ((((12 4) q q q q q q q q q q q q))
              :pitch-seq-palette (1 2 3 4 5 6 7 8 9 10 11 12)))
            (zapp-down
             ((((12 4) q q q q q q q q q q q q))
              :pitch-seq-palette (12 11 10 9 8 7 6 5 4 3 2 1))))
          :rthm-seq-map
          '((A ((rh (IO1 M1 M2 M3 M4 M5 M6 IO2 zapp-down)) 
                (lh (IO2 M1 M2 M3 M4 M5 M6 IO1 zapp-down))))
            (B ((rh (IO2 M2 M3 M4 M6 M5 M7 IO3 zapp-up))
                (lh (IO3 M2 M3 M4 M6 M5 M7 IO2 zapp-up))))
            (C ((rh (IO3 M3 M6 M4 M5 M7 M8 IO4 zapp-up))
                (lh (IO4 M3 M6 M4 M5 M7 M8 IO3 zapp-up))))
            (D ((rh (IO4 M5 M4 M6 M8 M7 M9 IO1 zapp-down))
                (lh (IO1 M5 M4 M6 M8 M7 M9 IO4 zapp-down))))))))
    (write-xml +remote-control+ :respell-notes nil)
    (sc-test-check
      (file-write-ok "/tmp/remote-control.xml" 1040000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *sc-test-all-tests*
;;; (setf *sc-test-all-tests* (remove 'test-rs-chop *sc-test-all-tests*)) 

;;; SAR Mon Dec 26 11:00:09 EST 2011: Changed the WARN to an ERROR so that the
;;; process is interrupted before the piece tests are run if something fails
;;; here. 

;;; SAR 08.12.11 TODO: Figure out the confounded macros enough to be able to
;;; automatically generate sublists of tests, so that we can, for example, test
;;; only the methods/functions for just one individual class etc.
(if (sc-test-test-all)
    (progn 
      (setf *sc-test-meth-and-func-tests-state*
            "- ALL METHOD AND FUNCTION TESTS PASSED.")
      (format t "~%~%~a~%~%" *sc-test-meth-and-func-tests-state*))
    (progn
      (setf *sc-test-meth-and-func-tests-state*
            "- METHOD AND FUNCTION TEST-SUITE FAILED.")
      (error "METHOD AND FUNCTION TEST-SUITE FAILED.")))

(in-scale :quarter-tone)

;;; MDE Thu May 30 16:21:23 2013 
(set-sc-config 'cmn-display-auto-open #+sbcl T #-sbcl nil)
(set-sc-config 'midi-play-auto-open #+sbcl T #-sbcl nil)
;;; DJR Thu 26 Sep 2019 11:12:55 BST
(set-sc-config 'lp-display-auto-open #+sbcl T #-sbcl nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sc-test-suite.ls
