;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             cheat-sheet.lsp
;;;
;;; Project:          cheat sheet:
;;;                   composition for solo electric guitar, alto flute/piccolo,
;;;                   e-flat clarinet/bass clarinet, marimba, piano, violin,
;;;                   viola, cello, and double bass.
;;;
;;; Purpose:          Defining the data to be used in the composition: harmonic
;;;                   material, rhythmic sequences, sequence order etc. etc.
;;;                   NB This modified version of the file is for sc testing
;;;                   purposes only.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    17th February 2007 (Edinburgh)
;;;
;;; $$ Last modified: 09:06:01 Tue Jan 14 2014 GMT
;;;
;;; SVN ID: $Id: rthm-seq-bar.lsp 509 2011-12-14 20:35:27Z reed@seanreed.ie $
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

(setf cm::*scale* (cm::find-object 'quarter-tone))

(defun get-loops-seqs-aux (seq-order
                           &key 
                           (fibonacci-transitions '(21 13 8)))
  (let ((fts (make-cscl fibonacci-transitions))
        (transition nil)
        (insert nil)
        (top-refs '()))
    (loop for s1 in seq-order and s2 in (cdr seq-order) do
          (setf transition (fibonacci-transition (get-next fts)))
          ;; we transition from the first to the second seq, then from the
          ;; second to the third; each transition starts with repetitions of
          ;; the first and ends with repetitions of the second, e.g.
          ;; (fibonacci-transition 13) -> (0 0 1 0 1 0 1 1 1 0 1 1 1)
          ;; To avoid having two series of repetitions of the same thing,
          ;; insert one more of the first seq.
          (when insert
            (push insert top-refs))
          (loop for i in transition do
                (if (zerop i)
                    (push s1 top-refs)
                  (push s2 top-refs)))
          (setf insert s1))
    (nreverse top-refs)))

(defun get-loops-seqs (seq-order
                       &key 
                       (fibonacci-transitions '(21 13 8))
                       ;; when t, then when we've gone a certain distance, then
                       ;; we start mixing in the same result again so that we
                       ;; don't simply proceed 1,2,3,4.. rather after getting
                       ;; to 3 we reintroduce 1 etc. 
                       (remix-in t)
                       ;; when we start to remix-in, use a fibonacci sequence
                       ;; to choose when to do so or not ; this simply cycles
                       ;; around and uses the following arg to seed the
                       ;; sequence. 
                       (remix-in-fib-seed 34)
                       ;; when t, we go backwards to the first seq too :o)
                       (mirror t)
                       (verbose t))
  (let* ((simple (get-loops-seqs-aux
                  (if mirror
                      (append seq-order (reverse (butlast seq-order)))
                    seq-order)
                  :fibonacci-transitions fibonacci-transitions))
         (fib-tran (when remix-in
                     (make-cscl (fibonacci-transition remix-in-fib-seed))))
         (first-third (position (third seq-order) simple))
         (result (if remix-in
                     (loop 
                         with copy = (copy-list simple)
                         for i from 1
                         for p1 in simple
                         if (and (> i first-third)
                                 (= 1 (get-next fib-tran)))
                         collect p1 and collect (pop copy)
                         else collect p1)
                   simple)))
    (when verbose
      (format t "~%~a sequences produced" (length result))
      (loop for el in (remove-duplicates seq-order) do
            (format t "~%~a: ~a" el (count el result))))
    result))

(defun guitar-chord (lowest-fret fingerings
                     &key (tuning '(eqf3 a3 d4 g4 bqs4 e5))
                          print-octaves)
  (unless (= 6 (length fingerings))
    (error "second argument must be a list of 6 fingerings (1-4): ~a"
           fingerings))
  ;; if we're doing a barre, we can't have an open string except on VI
  (when (and (> (count 1 fingerings) 1)
             (member 0 (rest fingerings)))
    (error "It's not possible to have an open string other than VI when ~
            using barre: ~a" fingerings))
  (when (> (+ lowest-fret (loop for f in fingerings maximize f))
           19)
    (error "The guitar only has 19 frets! lowest-fret: ~a, fingerings: ~a"
           lowest-fret fingerings))
  (let* ((result (loop for finger in fingerings and open in tuning collect
                       (if (zerop finger)
                           open
                         ;; guitar sounds an octave lower than written
                         (transpose-note open (+ lowest-fret finger -12)))))
         (pitches (loop for p in result collect (make-pitch p)))
         ;; for the purpose of getting extra pitches for the ensemble extend
         ;; the range up and down by the intervallic relationship of the
         ;; resultant chord mapped onto the highest and lowest notes.
         (intervals (loop 
                        with lowest = (first pitches)
                        for p in (rest pitches) 
                        collect (pitch- p lowest)))
         (reverse-intervals (loop 
                                with highest = (sixth pitches)
                                for p in (rest (reverse pitches))
                                collect (pitch- p highest)))         
         (lowest (first result))
         (highest (sixth result))
         ;; contract by a qtr tone to avoid having almost exclusively quarter
         ;; tone notes (i.e. adjust scordatura back) 
         (high-extension (loop for interval in intervals collect
                               (transpose-note highest (- interval 0.5))))
         (low-extension (reverse
                         (loop for interval in reverse-intervals collect
                               (transpose-note lowest (- interval 0.5)))))
         (num-octaves 0))
    (loop 
        for g in result 
        for gp = (make-pitch g)
        do
          (loop 
              for hp in high-extension
              for lp in low-extension
              do
                (when (is-octave gp (make-pitch hp))
                  (incf num-octaves))
                (when (is-octave gp (make-pitch lp))
                  (incf num-octaves)))
        finally (when print-octaves
                  (format t "~&~a octaves" num-octaves)))
    (values result high-extension low-extension num-octaves)))

;;; This results in three top-level references: low, medium, high (low means
;;; we'll finger the lowest 4 strings leaving the top two barred with finger 1;
;;; sim for medium and high); 15 second-level refs (numbered 1-15),
;;; representing ascending fret positions; and 6 3rd level refs (numbered 1-6)
;;; representing 6 different fingerings.

(defparameter +cheat-sheet-set-palette+
    ;; these chosen after finding out which fingerings produce the least
    ;; octaves  
    (let* ((finger-perms '((4 2 1 3) (4 1 2 3) (4 1 3 2) (3 2 1 4) (3 4 1 2)
                           (4 3 1 2)))
           (sp
            (make-set-palette 
             'cheat-sheet-set-palette
             (loop for label in '(low medium high) collect
                   (list 
                    label
                    (loop for fret from 1 to 15 collect
                          (list 
                           fret
                           (loop 
                               for fingers in finger-perms and i from 1 
                               for fingering =
                                 (flatten
                                  (case label
                                    (low (list fingers 1 1))
                                    (medium (list 1 fingers 1))
                                    (high (list 1 1 fingers))))
                               collect
                                 (multiple-value-bind
                                     (guitar high low)
                                     (guitar-chord fret fingering)
                                   (list 
                                    i
                                    (list (append guitar high low)
                                          :subsets
                                          (list 
                                           (list 
                                            'guitar
                                            (list 
                                             guitar
                                             ;;(list-to-string fingering)))
                                             fingering))
                                           (list 'high high)
                                           (list 'low low)))))))))))))
      (link-named-objects sp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;; use the sc chords now we have them
(defun string-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                   instrument set string-III) 
  (let* ((default (default-chord-function curve-num index pitch-list pitch-seq 
                                          instrument set))
         (high (when (> (sclist-length default) 1)
                 (second (data default)))))
    (if (and high
             ;; can't have any 2-note chords where highest note is < open III
             ;; MDE Sat Dec 17 09:07:36 2011 -- this should already be a pitch
             ;; object but SBCL keeps coughing when it is  
             (pitch< high (make-pitch string-III)))
        ;; just return the highest note
        ;; (make-chord (list high))
        high
      default)))

(defun violin-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                     instrument set) 
  (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                              instrument set 'd4))

(defun viola-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                  instrument set) 
    (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                                instrument set 'g3))

(defun cello-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                    instrument set) 
  (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                              instrument set 'g2))

(defun piano-chord-fun (curve-num index pitch-list pitch-seq instrument
                        set) 
  (declare (ignore set instrument pitch-seq curve-num))
  (let* ((start (if (> index 2) ;; try and get 4 notes
                    (- index 3)
                  0))
         (at-start (nth start pitch-list))
         (result (list at-start)))
    (loop 
        for i from (1+ start) to (+ start 3) 
        for p = (nth i pitch-list)
        do
          (when (and p
                     (<= (pitch- p at-start) 12))
            (push p result)))
    (if (> (length result) 1)
        (make-chord result)
      (first result))))

(let ((last-chord '()))
  (defun guitar-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                     instrument set) 
    (declare (ignore index instrument pitch-list))
    (let* ((subset (get-data 'guitar (subsets set)))
           (subset-pitches (data subset))
           (tag (tag subset))
           ;; find out where the current number from the pitch curve is
           ;; relative to the lowest/highest in the curve; using this we can
           ;; decide whether to play all or only some of the 6-note guitar
           ;; chord.
           (range (1+ (- (highest pitch-seq) (lowest pitch-seq))))
           (tessitura (/ curve-num range))
           (strings '(VI V IV III II I))
           nth1 nth2 fingering notes chord show-fingering)
      (cond ((< tessitura 0.3) (setf nth1 0 nth2 3))
            ((< tessitura 0.5) (setf nth1 0 nth2 4))
            ((< tessitura 0.7) (setf nth1 1 nth2 5))
            ((>= tessitura 0.7) (setf nth1 0 nth2 6)))
      (setf notes (subseq subset-pitches nth1 nth2)
            fingering (subseq tag nth1 nth2)
            strings (subseq strings nth1 nth2)
            show-fingering (not (equal (pitch-list-to-symbols last-chord)
                                       (pitch-list-to-symbols notes)))
            last-chord (copy-list notes)
            chord (make-chord notes))
      (add-mark chord (when show-fingering
                            (apply #'cmn::fingering 
                                   (append
                                    (reverse 
                                     (loop 
                                         for s in strings 
                                         for f in fingering 
                                         collect
                                           (format nil "~3a ~a" s f)))
                                    ;;'(:direction :up)))))
                                    ))))
      chord)))
|#    

;; (print (loop for i below 20 collect (random-rep 1.0 (zerop i))))
;; (print (loop for i below 20 collect (random-rep 100 (zerop i))))

(defparameter +cheat-sheet-rsp-orig+
  (make-rsp
   'cheat-rthm-seq-palette
   '((1 
      ((a ((((2 4) (s) (32) - 32 (s) s - - +s+32 - (32) (e))
            ((q) (s) s (e))
            ((q) (e) (32) 32 (s))
            ((e) - { 3 48 x 3 } - (s) (e) e)
            ((s.) - 32 e - - +e e -))
           :pitch-seq-palette (((2) 8 
                                0 
                                0 
                                0 2 3 (0) 
                                -1 11 9)
                               (5 (4) 
                                  4 
                                  6 
                                  1 3 6 (6) 
                                  0 10 2))
           :marks (as 1 5 6 a 4 te 10 13 14 beg-sl 7 end-sl 9)))
       (b ((((2 4) (q) - 32 32 (32) 32 +e -)
            ((q) - s (s) s - (s))
            ((s) - 32 32 - (e) (e) e)
            (- s (s) (s) (32) 32 - - 32 32 32 (32) (s) s -)
            ((32) 32 (s) e (q)))
           :pitch-seq-palette ((4 0 (6) 0 2 0 3 10 -1 2 0 1 2 6 0 (-4))
                               (2 5 0 0 1 0 2 8 -1 1 -1 0 1 (5) (-1) (-2))
                               ((4 1 (6) (0) (2) (0) 3 (10)
                                   (-1) (2) 0 1 2 (6) (0) (-4))
                                piano-right-hand piano-left-hand)
                               ((2 5 (0) (0) (1) (0) 2 (8)
                                   (-1) (1) -1 0 1 (5) (-1) (-2))
                                piano-right-hand piano-left-hand))
           :marks (as 5 6 s 7 8 a 9 as 10 s 11 15 as 16 at 17)))
       (c ((((2 4) h)
            (- +\32 32 (32) 32 (32) 32 - (s) (q))
            (- e 32 32 (32) 32 - (32) 32 (s) (e))
            ((q) - s+32 - (32) (e))
            ((q) - s+32 - (32) (e)))
           :pitch-seq-palette (((0) 4 0 6 11 -1 3  2 -1 0 0)
                               (4 3 4 5 8 1   4 3 (-1) -1 1)
                               (((0) (4) (0) (6)
                                 (11) -1 3  2 -1 (0) (0))
                                piano-right-hand piano-left-hand
                                (((4) (3) (4) (5)
                                  (8) 1   4 3 (-1) (-1) (1))
                                 piano-right-hand piano-left-hand)))
           :marks (s 3 4 as 5 s 7 10 a 11 14)))
       (d ((((2 4) (q) (e) (s) s)
            (- +\32 s. s. 32 - - +s e { 3 48 x 3 } -)
            ((q) (s) g - s g s { 3 48 x 3 } -)
            ((s) - e+s - (q))
            ((e) (s) s - +\32 32 (32) 32 (32) s. -))
           :pitch-seq-palette ((11 10 6 13 11 (5) 7 9 
                                   8 10 9 12 (7) (7) (7) 
                                   (9) (10) 9 6 (13))
                               ((3) 4 5 (3) 4 6 7 9   
                                2 3 4 5 8 8 8
                                0 (0) 1 5 (-2))
                               (((11) (10) (6) (13) (11) 5 7 9 
                                 8 10 9 12 7 13 7
                                 (9) 
                                 (10) (9) (6) 13)
                                piano-right-hand piano-left-hand)
                               (((3) (4) (5) (3) (4) 6 7 9   
                                 1 3 2 5 8 1 8
                                 (0)
                                 (0) (1) (5) (-2))
                                piano-right-hand piano-left-hand))
           :marks (te 3 4 beg-sl 8 end-sl 10 a 15 17 as 19 at 24))))))
   :psp-inversions t))

;;; MDE Mon Apr 15 18:00:53 2013 -- 
(cmn-display +cheat-sheet-rsp-orig+)

(defparameter +cheat-sheet-rsp+
  (scale (clone +cheat-sheet-rsp-orig+) 2))
  ;; (clone +cheat-sheet-rsp-orig+))

;; (get-data 85 +cheat-sheet-rsp-chopped+)

(defparameter +cheat-sheet-rsp-chopped+
    (chop +cheat-sheet-rsp+ nil 'e))

(defparameter +cheat-sheet-rehearsal-letters+
    '(a 76 b 148 c 193 d 288 e 422 f 458 g 557 h 643 i 716 j 791 k 858 
      l 1020 m 1086 n 1198 o 1270 p 1356 q 1456 r 1521 s 1596 t 1655 
      u 1705 v 1853 w 1916 x 1997 y 2076 z 2121))

(defun cheat-sheet-letter-bars ()
  (loop for bar-num in (rest +cheat-sheet-rehearsal-letters+) by #'cddr
      collect bar-num))

(defun cheat-sheet-letter-bar-num (letter)
  (if (equal letter 'beg)
      1
    (nth (1+ (position letter +cheat-sheet-rehearsal-letters+))
         +cheat-sheet-rehearsal-letters+)))

;;; the upper-case letters are the rehearsal numbers, the small case letters
;;; refer to the a-d rthm-seqs above (or rather in the chopped seqs).
(defparameter +cheat-sheet-ins-groups+
    '((beg ((a (fl cl))
            (b (perc pno-rh pno-lh))
            (c (vln vla vc db))
            (d (gtr))))
      (A ((a (fl cl))
          (b (perc pno-rh pno-lh))
          (c (vln vla vc db))
          (d (gtr))))
      (D ((a (cl))
          (b (perc pno-rh pno-lh))
          (c (fl vln vla vc db))
          (d (gtr))))
      (E ((a (cl perc))
          (b (pno-rh pno-lh))
          (c (vln vla vc db))
          (d (fl gtr))))
      (F ((a (cl perc))
          (b (pno-rh pno-lh))
          (c (vln vla vc db))
          (d (fl gtr))))
      (G ((a ())
          (b (fl cl pno-rh pno-lh))
          (c (vln vla vc db))
          (d (perc gtr))))
      (H ((a (vla vc fl cl))
          (b (perc db pno-rh pno-lh))
          (c ())
          (d (vln gtr))))
      (I ((a (vla vc fl))
          (b (db perc pno-rh pno-lh))
          (c ())
          (d (gtr vln cl))))
      (J ((a (vln vla vc fl))
          (b (db cl perc pno-rh pno-lh))
          (c ())
          (d (gtr))))
      (K ((a (fl))
          (b (perc cl))
          (c (vln vla vc))
          (d (gtr))))
      (L ((a (fl))
          (b (perc cl pno-rh pno-lh))
          (c (vln vla vc db))
          (d (gtr fl))))
      (M ((a (fl cl db))
          (b (perc))
          (c (vln vla vc))
          (d (gtr pno-rh pno-lh))))
      (O ((a (fl cl vln))
          (b (perc))
          (c (db vla vc))
          (d (gtr))))
      (Q ((a (fl cl vln))
          (b (perc pno-rh pno-lh))
          (c (vla vc db))
          (d (gtr))))
      (R ((a (fl cl))
          (b (perc pno-rh pno-lh))
          (c (vln vla vc db))
          (d (gtr))))
      (T ((a (vln fl cl))
          (b (pno-rh pno-lh))
          (c (vla vc db))
          (d (gtr))))
      (U ((a (fl cl vln))
          (b (db pno-rh pno-lh))
          (c ())
          (d (gtr vc vla))))
      (V ((a (perc fl cl))
          (b (pno-rh pno-lh))
          (c (vln vla vc db))
          (d (gtr))))
      (Z ((a (fl cl))
          (b ())
          (c (vln vla vc db pno-lh pno-rh perc))
          (d (gtr))))))

(defparameter +cheat-sheet-ins-change-bar-nums+
    (loop for sec in +cheat-sheet-ins-groups+ collect
          (cheat-sheet-letter-bar-num (first sec))))

(defparameter +cheat-sheet-chopped-seq-numbers+
    (get-loops-seqs (loop for i from 1 to 100 collect i)
                    :mirror nil :remix-in t :remix-in-fib-seed 21
                    :fibonacci-transitions '(21 13 8) 
                    :verbose nil))

;;; This actually gets set below
(defparameter +cheat-sheet-num-rthm-seqs+ 
    (length +cheat-sheet-chopped-seq-numbers+))


;;; specify an instrument and return the map from the loops. We need references
;;; like '(1 a 23) where the last number ranges from 1-100 because the chopping
;;; of a single seq-letter (e.g. 1b) of +cheat-sheet-rsp+ results in 100 new
;;; seqs (because we specify 10 chop points per quarter note (see
;;; rthm-seq-bar::chop)) and there are 5 2/4 bars (= 10 quarters) per rthm-seq.
(defun cheat-rsm-ins (ins)
  ;; first get the seq-letter that this instrument is playing at
  ;; each rehearsal letter (including the beginning)
  (let ((ins-seq-letters
         (loop
             for section in +cheat-sheet-ins-groups+
            ;; for reh-letter = (first section)
             for seq-letters = (second section)
                               ;; each seq is a bar long so
                               ;; seq-num==bar-num  
            ;; for seq-num = (cheat-sheet-letter-bar-num reh-letter)
             collect
               (loop 
                   for sl in seq-letters
                   for l = (first sl)
                   for inss = (second sl)
                   do
                     (when (member ins inss)
                       (return l)))))
        (bnums (econs +cheat-sheet-ins-change-bar-nums+
                      (1+ +cheat-sheet-num-rthm-seqs+))))
    (list ins
          (loop 
              for this in bnums
              for next in (rest bnums)
              for isl in ins-seq-letters 
              appending
                (loop for i from (1- this) to (- next 2) collect
                      (if isl
                          (list 1 isl 
                                (nth i +cheat-sheet-chopped-seq-numbers+))
                        nil))))))


(defparameter +cheat-sheet-rsm+
    (make-rthm-seq-map
     'cheat-sheet-rsm
     `((1 (,(cheat-rsm-ins 'fl)
           ,(cheat-rsm-ins 'cl)
           ,(cheat-rsm-ins 'perc)
           ,(cheat-rsm-ins 'pno-rh)
           ,(cheat-rsm-ins 'pno-lh)
           ,(cheat-rsm-ins 'gtr)
           ,(cheat-rsm-ins 'vln)
           ,(cheat-rsm-ins 'vla)
           ,(cheat-rsm-ins 'vc)
           ,(cheat-rsm-ins 'db))))))

(defparameter +cheat-sheet-set-map+
    (let* ((top-llu
            ;; Make a general transition from low->high, i.e. from fingerings
            ;; strings VI-III to fingering strings IV-I 
            (make-l-for-lookup 
             'ternary-lfl
             '((1 ((low low low medium low low) 
                   (low medium low low medium medium)
                   (medium low medium high high low high)))
               (2 ((low low medium low low high)
                   (low medium medium low medium high high)
                   (high medium high medium low high high)))
               (3 ((medium medium high medium high high)
                   (medium high high medium high high high)
                   (high medium high high medium high high high high low))))
             '((1 (1 2 2 2 1 1))
               (2 (2 1 2 3 2 1))
               (3 (2 3 2 2 2 3 3)))))
           (top-refs (do-lookup top-llu 1 +cheat-sheet-num-rthm-seqs+))
           (fingering-llu
            (make-l-for-lookup 
             'fingering-lfl
             nil
             '((1 (2 3 2 4))
               (2 (3 1 4 1))
               (3 (4 5 2 1))
               (4 (5 3 6))
               (5 (6 4 6 2))
               (6 (5 5 3)))))
           (fingering-refs (move-repeats 
                            (get-l-sequence fingering-llu 1 
                                            +cheat-sheet-num-rthm-seqs+)))
           (fret-curve 
            '(0.000 1.000 10.615 1.000 16.387 5.848 20.764 2.661 27.188 7.864
              33.054 4.949 39.479 10.206 41.713 1.000 43.855 10.996 49.069
              1.000 52.793 11.895 54.562 4.214 68.622 12.521 83.054 13.665
              83.520 1.000 88.175 15.000 91.527 1.000 94.879 15.000 97.393
              1.000 100.000 15.000))
           (stretched-fret-curve (new-lastx fret-curve 
                                            +cheat-sheet-num-rthm-seqs+)))
      (loop 
          for top in top-refs
          for fingering in fingering-refs
          and i from 1 collect
            (list top 
                  (round (interpolate i stretched-fret-curve))
                  fingering))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +cheat-sheet-set-limits-high+
    '(1 82.073 179.464 91 193 100 295.708 87.258 458.857 82.937 575.1
      93.884 636.281 82 709.698 105.983 834.099 93.308 960.54 80.632
      1029.878 98.781 1129.807 88.987 1239.932 104.831 1333.743 87.258 1435.711
      88.699 1521.365 100.51 1639.648 92.156 1770.167 102.238 1845.623
      85.242 1919.04 99.934 2065.875 93.884 2177 108))

(defparameter +cheat-sheet-set-limits-low+
    '(1 40.99 193 55 371.164 28.32 619.966 43.524 748.446 30.563 795.351 41.272
      838.178 50.282 960.54 41.835 1070.665 56.476 1131.846 31.136 1186.909
      44.65 1229.736 60.699 1331.704 26.068 1551.955 31.417 1639.648 50.563
      1704.907 36.204 1772.206 47.748 1827.269 23.252 1899 21 1984.3
      33.388 2029.166 21 2177 21))

(defparameter +cheat-sheet-pno-high+
    '(1 100 193 100 236.566 80 295.708 87.258 458.857 82.937 575.1 93.884
      636.281 78 668.911 105.983 713.777 96.189 789.233 102.815 834.099
      93.308 893.241 74.007 960.54 80.632 1019.681 98.781 1070.665 79.768
      1127.768 92.156 1199.145 83.801 1239.932 104.831 1341.901 57.01 1409.2
      78.04 1456 55 1461 65 1484.656 71.414 1500 100 1521.365 100.51 1511.388
      91.003 1604.979 
      98.493 1639.648 92.156 1672.277 98.205 1725.301 82.937 1770.167 102.238
      1845.623 85.242 1890 60 1891 100 1900 100 1905 62 2065.875 93.884 
      2177 108))

(defparameter +cheat-sheet-pno-low+
    '(1 60 193 55 236.566 48 295.708 35 458.857 52.937 575.1 78
      636.281 48.616 691.344 28.778 730.092 45 834.099 63.308 893.241
      34.007 960.54 50.632 1011.524 32.523 1020 70 1070.665 39.768 1127.768
      62.156 1199.145 53.801 1341.901 27.01 1423.475 24.745 1456 25 1484.656
      41.414 1511.388 61.003 1725.301 52.937 1776.285 55.858 1841.545 34.252
      1900 21 2177 21))

#|
(setf +cheat-sheet-pno-high+
      (loop for x in +cheat-sheet-pno-high+ by #'cddr 
         and y in +cheat-sheet-pno-high+ by #'cddr
         collect x collect (min 120 (+ y 10)))
      +cheat-sheet-pno-low+
      (loop for x in +cheat-sheet-pno-low+ by #'cddr 
         and y in +cheat-sheet-pno-low+ by #'cddr
         collect x collect (max 21 (- y 10))))
|#
(defparameter +cheat-sheet-pno-lh-high+
    (loop 
        for x in +cheat-sheet-pno-high+ by #'cddr 
        for high-y in (cdr +cheat-sheet-pno-high+) by #'cddr
        for low-y = (interpolate x +cheat-sheet-pno-low+)
        for diff = (- high-y low-y)
        for halfway = (round (+ low-y (/ diff 2.0)))
        collect x collect halfway))

(defparameter +cheat-sheet-pno-rh-low+
    (env-plus +cheat-sheet-pno-lh-high+ 1))

(make-slippery-chicken
 '+cheat-sheet+
 :instrument-palette 
 '((piccolo
    (:staff-name "piccolo" :lowest-written d4 :highest-written c7 :chords nil
                 :missing-notes nil :midi-program 73 :starting-clef treble 
                 :transposition-semitones 12 :microtones t))
   (flute 
    (:staff-name "flute" :lowest-written c4 :highest-written d7 :chords nil 
     :missing-notes (cqs4 dqf4) :midi-program 74 :starting-clef treble
     :microtones t))
   (alto-flute 
    (:staff-name "alto flute" :lowest-written c4 :highest-written c7
     :chords nil :missing-notes (cqs4 dqf4) :midi-program 74 
     :starting-clef treble :microtones t :transposition-semitones -5))
   (bass-clarinet 
    (:staff-name "bass clarinet" :lowest-written c3 :highest-written g6
     :chords nil :midi-program 72 :starting-clef treble :microtones t
     :prefers-notes low
     :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3 eqf3 dqs3 dqf3 
                     cqs3)
     :clefs (treble) :clefs-in-c (treble bass) :transposition-semitones -14)) 
   (e-flat-clarinet 
    (:staff-name "e-flat clarinet" :lowest-written e3 :highest-written a6
     :chords nil :midi-program 72 :starting-clef treble :microtones t
     :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3)
     :transposition-semitones 3)) 
   (marimba 
    (:staff-name "marimba" :lowest-written c3 :highest-written c7 :chords t
     :midi-program 13 :starting-clef treble :clefs (treble)
     :microtones nil))
   (piano-right-hand 
    (:staff-name "piano" :lowest-written a0 :highest-written c8 :chords t
     :midi-program 1 :starting-clef treble :prefers-notes high
     :clefs (treble bass) ;; double-treble double-bass)
     :microtones nil :largest-fast-leap 9 :chord-function piano-chord-fun))
   (piano-left-hand 
    (:staff-name "" :lowest-written a0 :highest-written c8 :chords t
     :starting-clef bass :clefs (bass treble) ;; double-treble double-bass)
     :midi-program 1 :microtones nil :largest-fast-leap 9 :prefers-notes low
     :chord-function piano-chord-fun))
   (guitar 
    (:staff-name "electric guitar" :lowest-written eqf3 :highest-written b6 
     :chords t :midi-program 28 :starting-clef treble 
     :transposition-semitones -12 :microtones t :subset-id guitar 
     :chord-function guitar-chord-selection-fun :largest-fast-leap 31))
   (violin 
    (:staff-name "violin" :lowest-written g3 :highest-written c7 
     ;; :prefers-notes high
     :starting-clef treble :chords t :midi-program 41 :microtones t
     :chord-function violin-chord-selection-fun :largest-fast-leap 13))
   ;; viola might be conducting....
   (viola (:staff-name "viola" :lowest-written c3 :starting-clef alto 
           :highest-written f6 :chords t :midi-program 42 :microtones t
           :chord-function viola-chord-selection-fun :largest-fast-leap 13
           :clefs (alto treble)))
   (cello 
    (:staff-name "cello" :lowest-written c2 :starting-clef bass
     :clefs (bass tenor treble)
     :highest-written c7 :chords t :midi-program 43 :microtones t
     :chord-function cello-chord-selection-fun :largest-fast-leap 12))
   (double-bass 
    (:staff-name "double bass" :lowest-written e2 :starting-clef bass
     :clefs (bass tenor treble) :prefers-notes low
     :highest-written g5 :chords nil :midi-program 44
     :transposition-semitones -12 :microtones t :largest-fast-leap 10)))
 ;; MDE Mon Dec 12 10:50:30 2011 -- I think the original had microtones t for
 ;; each instrument but we don't have enough MIDI channels, so for testing
 ;; purposes set to same channel as chromatic notes.
 :ensemble '(((fl ((alto-flute piccolo) :midi-channel 1
                   :microtones-midi-channel 1))
              (cl ((bass-clarinet e-flat-clarinet) :midi-channel 2
                   :microtones-midi-channel 2))
              (perc (marimba :midi-channel 3))
              (pno-rh (piano-right-hand :midi-channel 4))
              (pno-lh (piano-left-hand :midi-channel 5))
              (gtr (guitar :midi-channel 6 :microtones-midi-channel 7))
              (vln (violin :midi-channel 8 :microtones-midi-channel 9))
              (vla (viola :midi-channel 11 :microtones-midi-channel 12))
              (vc (cello :midi-channel 13 :microtones-midi-channel 14))
              (db (double-bass :midi-channel 15 :microtones-midi-channel 16))))
 :instruments-hierarchy '(gtr vln fl pno-rh cl db vc perc pno-lh vla)
 ;; these always refer to sounding pitches
 :set-limits-high `((all ,+cheat-sheet-set-limits-high+)
                    (vc (0 f4 100 c6))
                    (db (0 bf3 70 d4 100 cs3))
                    ;; no matter that he's sometimes playing bass clarinet,
                    ;; that instrument's limit will override this
                    (cl (0 c7 716 f6 789 f6 2177 c7))
                    (vla (0 fs5 100 fs5))
                    (gtr (0 100 270 100 420 f3 474 100 2177 100))
                    ;; MDE Thu Dec 15 15:50:28 2011 -- not working at the mo
                    ;; but not important for testing purposes (i.e. not a bug)
                    ;; (pno-rh ,+cheat-sheet-pno-high+)
                    ;; (pno-lh ,+cheat-sheet-pno-lh-high+)
                    (perc (0 c6 25 c6 40 d5  60 e6 75 g5 90 c7 100 c7)))
 :set-limits-low `((all ,+cheat-sheet-set-limits-low+)
                   (fl (0 0 1704 0 1705 b5 1800 b5 2177 0))
                   (cl (0 0 1704 0 1705 b5 1800 b5 2177 0))
                   (perc (0 bf3 25 bf3 40 c3 60 d4 75 f3 90 bf4 100 bf3))
                   ;; MDE Thu Dec 15 15:50:57 2011 -- sim.
                   ;;(pno-rh ,+cheat-sheet-pno-rh-low+)
                   (pno-lh ,+cheat-sheet-pno-low+))
 :set-palette +cheat-sheet-set-palette+
 :rthm-seq-palette +cheat-sheet-rsp-chopped+
 :set-map `((1 ,+cheat-sheet-set-map+))
 :rthm-seq-map +cheat-sheet-rsm+
 :staff-groupings '(2 1 2 1 4)
 ;; need to set this if we want sc to write bar nums instead of cmn
 ;; also need to add players when writing parts.
 :instruments-write-bar-nums '(fl pno-rh vln)
 :rehearsal-letters (cheat-sheet-letter-bars)
 :bars-per-system-map '((1 12) (96 11) (128 9) (172 11) (213 13)
                        (226 8) (323 8) (344 7) (391 7) (405 8)
                        (436 10) (451 6) (470 8) (487 9) (505 10) (515 8)
                        (523 6) (540 8) (577 7) (606 8) (628 7) (658 6) 
                        (666 7) (670 8)
                        (688 6) (701 8) (718 9) (758 6) (771 8) (784 7) (805 8)
                        (852 7) (865 7) (900 8) (1018 6) (1040 7) (1055 7)
                        (1090 7) (1130 8))
 :instrument-change-map
 ;; bars 643, 1086, 1705 (make all instruments play high until strgs entry in
 ;; 1853--they should be low!)  
 ;; (fl ef-cl) (picc bcl) (fl bcl) (picc ef-cl) 
 ;; (section ((player ((seq-num ins) ...))))
 ;; in our case every seq is 1 bar long so no problem
 '((1 ((cl ((1 e-flat-clarinet) (557 bass-clarinet) (1705 e-flat-clarinet)))
       ;; 21/5/07: the following results in us never having bass cl and picc,
       ;; so change to bcl earlier 
       ;;(cl ((1 e-flat-clarinet) (1020 bass-clarinet) (1705 e-flat-clarinet)))
       (fl ((1 alto-flute) (422 piccolo) (1086 alto-flute) (1705 piccolo))))))
 :tempo-map '((1 (q 208 "Prestissimo"))))

;; (change-bar-line-type +cheat-sheet+ 1167 2)

(midi-play +cheat-sheet+ :midi-file "/tmp/cheat-sheet.mid")

(midi-play +cheat-sheet+  
           :num-sequences 10
           :midi-file "/tmp/cheat-sheet-beg.mid")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF cheat-sheet.lsp
