;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             skin.lsp
;;;
;;; Project:          skin
;;;
;;; Purpose:          composition for 7-string bass viol (gamba) and live
;;;                   electronics  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    14th August 2003
;;;
;;; $$ Last modified: 14:38:41 Sun Jan  1 2012 ICT
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Syllables per line in the five verses of Eugenio Montale's "What you knew
;;; of me / was only a whitened skin" (NB clisp doesn't like Italian accents
;;; so ` is used instead):


;;; Ci`o che di me sapeste           (7) 
;;; non fu che la scialbatura,       (8)
;;; la tonaca che riveste            (8)
;;; la nostra umana ventura.         (9) [32]
;;; 
;;; Ed era forse oltre il telo       (10)
;;; l'azzurro tranquillo;            (6)
;;; vietava il limpido cielo         (10)
;;; solo un sigillo.                 (6) [32]
;;;
;;; O vero c'era il fal`otico        (10)
;;; mutarsi della mia vita,          (9)
;;; lo schiudersi d'un'ignita        (8)
;;; zolla che mai vedr`o.            (6) [33]
;;; 
;;; Rest`o cos`i questa scorza       (8)
;;; la vera mia sostanza;            (8)
;;; il fuoco che non si smorza       (8)
;;; per me si chiam`o: l'ignoranza.  (9) [33]
;;; 
;;; Se un'ombra scorgete, non `e     (9)
;;; un'ombra--ma quella io sono.     (9)
;;; Potessi spiccarla da me,         (8)
;;; offrirvela in dono.              (7) [33] [163]


;;; William Arrowsmith's translation:

;;; What you knew of me
;;; was only a whitened skin,
;;; the cowl that cloaks
;;; our human destiny.
;;; 
;;; And perhaps behind the blue veil
;;; the air was blue and still;
;;; between me and the clear sky
;;; lay a simple seal.
;;; 
;;; Or else it was that wildfire
;;; changing of my life,
;;; the disclosure of the kindled clod
;;; I'll never see.
;;; 
;;; So then this husk remained
;;; my true substance;
;;; the name of unquenched fire
;;; for me was--ignorance.
;;; 
;;; If you glimpse a shade,
;;; it's not a shade--it's me.
;;; If I could strip that shade away,
;;; I'd give it to you, gladly.

(defparameter +skin-syllables+
    (make-assoc-list 'skin-syllables
                     '((1 (7 8 8 9))
                       (2 (10 6 10 6))
                       (3 (10 9 8 6))
                       (4 (8 8 8 9))
                       (5 (9 9 8 7)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unit is in units and the scalers are the units with unit removed all divided
;; by 4 

(defun get-scalers-for-unit (units unit) 
  (mapcar #'(lambda (x) (/ x 4))
          (remove unit units)))

;;; Make a rthm-seq using each of the 5 +skin-syllables+ as multipliers to
;;; basic rhythmic units of 32nd, septuplet 16th and quintuplet 16th etc.

(defparameter +skin-multiplier-rthm-seqs+
  ;; with 5 rhythmic units we'll get five seqs for each unit (=25) and 20
  ;; scaled seqs (=100): total 125
  ;; NB if we change this we have to change skin-get-rthm-seq-index-long below!
  (let ((rthm-units '(36 32 28 24 20)) 
        (tuplets '(9 nil 7 6 5))
        (count 0)
        (rsp (make-rsp 'skin-multiplier-rthm-seqs nil)))
    (loop for unit in rthm-units 
          for tuplet in tuplets do
          (reset +skin-syllables+)
          (loop for i below (sclist-length +skin-syllables+)
                for mults = (data (get-next +skin-syllables+))
                for rs = (make-rthm-seq-from-unit-multipliers 
                          unit mults '(4 4) :tuplet tuplet
                          :id (incf count)
                          :tag
                          (format nil "skin(unit:~a)(verse:~a)(mulipliers:~a)" 
                                  unit (1+ i) (list-to-string mults ",")))
                do (add rs rsp)))
    ;; Now take the 5 +skin-syllables+ and make rthm-seqs again using the
    ;; rhythmic units above but multiply them by the other units, i.e. when
    ;; we're doing the 32nds we multiply by 9, 7, 6, and 5 (all units divided
    ;; by 4), when doing the 28ths, multiply by 9, 8, 6, and 5 etc.
    (labels 
     ((scale-list (list scaler)
                  (loop for i in list collect (* i scaler)))
      (do-one (unit scaler tuplet)
              (reset +skin-syllables+)
              (loop for i below (sclist-length +skin-syllables+) 
                    for mults = (data (get-next +skin-syllables+))
                    for scaled = (scale-list mults scaler)
                    for rs = (make-rthm-seq-from-unit-multipliers 
                              unit scaled '(4 4) 
                              :tuplet tuplet
                              ;; the id is the unit, the scaler for the
                              ;; multipliers and the scaled multipliers
                              :id (incf count)
                              :auto-beam nil
                              :tag
                              (format nil "skin(unit:~a)(scaler:~a)(verse:~a) ~
                                           (unscaled:~a)(scaled:~a)"
                                      unit scaler (1+ i)
                                      (list-to-string mults ",")
                                      (list-to-string scaled ",")))
                    do (add rs rsp)))
      (do-all (unit tuplet scalers)
              (loop for scaler in scalers do
                    (do-one unit scaler tuplet))))
            (loop for ru in rthm-units 
                  for tuplet in tuplets
                  for scalers = (get-scalers-for-unit rthm-units ru)
                  do (do-all ru tuplet scalers)))
    rsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The normal tunings of the 7-bass viol in questions
(defparameter +skin-viol-tunings-normal+ '(a1 d2 g2 c3 e3 a3 d4))

;;; The tunings we will use
(defparameter +skin-viol-tunings+ +skin-viol-tunings-normal+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The viol can play 3 strings simultaneously, which means 5 string
;;; groups (convenient given the 5 verses).  Producing the 5th, 6th, and
;;; 7th partials on each of these 3 string combos, in each of the six
;;; permutations of the three nodes, will produce the following pitches
;;; (but note that triple-stop harmonics are NOT possible!):

(defparameter +skin-harmonics+
  (let ((partials '(7 6 5));; the partials we want to produce on each string
        (fingerings '(2 3 4));; semitones above open string to produce them
        (string-names '(VII VI V IV III II I))
        (num-3-strg-combos (- (length +skin-viol-tunings+) 2))
        (perms (permutations 3))
        (count 0)
        (sp (make-set-palette 'skin-harmonics nil)))
    (labels ((note-for-partial (fundamental partial)
                               (freq-to-note (* partial 
                                                (note-to-freq fundamental))))
             (nearest-chromatic (note)
                                (freq-to-note 
                                 (note-to-freq note) 
                                 cm::*chromatic-scale*))
             (get-fingering-and-partial-notes
              (open-string-note ref)
              (list (nearest-chromatic
                     (transpose-note open-string-note (nth ref fingerings)))
                    (note-for-partial open-string-note 
                                      (nth ref partials)))))
            (loop for i below num-3-strg-combos 
                  for iplus2 from 3
                  ;; for open-strings = (subseq +skin-viol-tunings+ i iplus2)
                  for names = (subseq string-names i iplus2)
                  for str1 = (nth i +skin-viol-tunings+)
                  for str2 = (nth (1+ i) +skin-viol-tunings+)
                  for str3 = (nth (+ 2 i) +skin-viol-tunings+)
                  do
                  (loop for j from 1
                        for perm in perms
                        for s1 = (get-fingering-and-partial-notes 
                                  str1 (first perm))
                        for s2 = (get-fingering-and-partial-notes
                                  str2 (second perm))
                        for s3 = (get-fingering-and-partial-notes
                                  str3 (third perm))
                        do (add (make-complete-set 
                                 (append s1 s2 s3) 
                                 :subsets (list (list (first names)
                                                      s1)
                                                (list (second names)
                                                      s2)
                                                (list (third names)
                                                      s3))
                                 :id (incf count)
                                 :tag (format 
                                       nil "~a-~a"
                                       (1+ i) (list-to-string perm ".")))
                                sp)))
            ;; do this for sections 1 and 2
            (add (make-complete-set '(c4) :id 0 :tag "no pitches!") sp)
            sp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The rthm-seqs are just thrown into the palette above one after the other at
;;; top level, so we have to fake some kind of reasonable structure.
;;; unit: of one <units> below
;;; scaler: of <units> below / 4
;;; verse: 1-5

(defun skin-get-rthm-seq-index-long (unit scaler verse)
  (let* ((start 26) ;; the first 25 are the short seqs
         (units '(36 32 28 24 20))
         (the-unit (position unit units))
         (the-scaler (position (* 4 scaler) (remove unit units)))
         ;; there are 20 for each unit
         (units (+ start (* 20 the-unit))) 
          ;; the scaler is used once for each of the 5 verses
         (scalers (+ units (* 5 the-scaler))))
    (+ scalers (1- verse))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The viol has 7 strings and can play 3 simultaneously (not harmonics!),
;;; these are numbered as follows:
;;;     1: VII,VI,V
;;;     2: VI,V,IV
;;;     3: V,IV,III
;;;     4: IV,III,II
;;;     5: III,II,I
;;; We're going to play these combos in the different structures/verses in the
;;; following sequence:

(defparameter +skin-strg-combos+
  '((1 (0 0 0 0))
    (2 (0 0 0 0)) ;; the first two pre-sections don't use strings!
    (3 (1 2 1 2))
    (4 (3 2 1 2))
    (5 (3 2 3 4))
    (6 (3 2 1 3))
    (7 (4 3 4 5))))

(defun skin-strg-combos-tally ()
  ;; stunning clisp bug: this doesn't get reinitialised between calls!!!!!!!!!
  (let ((result (ml 0 5)) ;;'(0 0 0 0 0))
        (combos (loop for i in +skin-strg-combos+
                      appending (second i))))
    (loop for i in combos do
          (unless (zerop i)
            (incf (nth (1- i) result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skin-get-skin-harmonics-ref (strg-combo perm)
  (+ perm (* 6 (1- strg-combo))))


;;; We need (4 6 6 3 1) (from skin-strg-combos-tally) sets from each string
;;; combo This selects the sets from the +skin-harmonics+ that we will use.
;;; Calling skin-get-combo-next will give us the next set in the list for the
;;; required string combo.

(defparameter +skin-strg-combo+ 
  ;; we've chosen these as the preferred sets and order
  (let* ((refs '(((1 1) (1 2) (1 4) (1 5))
                 ((2 1) (2 2) (2 3) (2 4) (2 5) (2 6))
                 ((3 1) (3 2) (3 3) (3 4) (3 5) (3 6))
                 ((4 1) (4 4) (4 5))
                 ((5 5))))
         (refs-transl (loop for section in refs and i from 1 collect
                            (list i 
                                  (make-cscl
                                   (loop for sub-section in section
                                         collect (skin-get-skin-harmonics-ref
                                                  (first sub-section)
                                                  (second sub-section))))))))
    (make-assoc-list 'skin-strg-combos refs-transl)))

(defun skin-get-combo-next (combo)
  (if (zerop combo)
      0
    (get-next (get-data-data combo +skin-strg-combo+ ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +skin-set-map+
  (loop for section in +skin-strg-combos+ collect
        (list (first section) (loop for combo in (second section) collect
                                    (skin-get-combo-next combo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; in the 25 shorter sequences we have about 35 bars which will do for section
;;; 6 (clm ric: 33 bars).  Counting the 2nd pre-section, we have 5 sections
;;; with the longer seqs, this is 20 seqs total.  We have 100 longer seqs in
;;; the palette so need to reject a lot!  We have each of the five verses four
;;; times (1 for each scaler) with each rhythmic unit:

;;; units: 5 (they get longer as unit decreases)
;;; verses: 5
;;; scalers: 4
;;; so for each scaler, 5 verses

;;; for each unit we can have only 4 verses, so: go through each unit in
;;; permutations, each scaler in sequence but exhausted before moving on, and
;;; through four of the five verses, remembering where we left off last time:

(defparameter +skin-long-rthm-seq-refs+
  (let* ((units '(20 24 28 32 36)) ;; backwards so we get the longest first
         (verses (make-cscl '(1 2 3 4 5)))
         ;; take the units in a randomly permutated order
         (unit-order (inefficient-permutations 5 :max 4)) ;; 4 verses
         (first-pass
          ;; this will be a list of 5 elements, one for each section but in
          ;; each of the sections the rhythmic unit is the same....
          (loop for unit in units ;; verse=section
                for scalers = (reverse (get-scalers-for-unit units unit))
                collect
                (loop for scaler in scalers collect
                      (list unit scaler))))
         ;; ... so we want to mix them up a bit: go from the highest scaler
         ;; down getting a different rhythmic-unit and verse each time
         (second-pass (loop for perm in unit-order and i from 0 appending
                            (loop for unit in perm collect
                                  (econs
                                   (nth i (nth unit first-pass))
                                   (get-next verses)))))
         (third-pass (loop for i in second-pass collect
                           (apply #'skin-get-rthm-seq-index-long i))))
    ;; (print first-pass)
    ;; (print second-pass)
    third-pass))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These are the refs to the 6th section, using the first 25 rthm-seqs, which
;;; are the short ones. 

(defparameter +skin-short-rthm-seq-refs+
  ;;  these are listed as sublists because 6 or 7 of these short seqs make up a
  ;;  theoretical long seq
  '((1 7 13 19 25 2 8) 
    (14 20 3 9 15 21)
    (4 10 16 22 5 11)
    (17 23 6 12 18 24)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB Although there are five verses we have 7 (= number of viol strings)
;;; sections.  The first two sections are seen as preludes and in fact there is
;;; no pitch material in these.   The first section even involves no playing,
;;; it is purely theatrical, the second section involves pitchless bowing onthe
;;; bridge. 

(defparameter +skin-rthm-seq-map+
  ;; the seqs in the palette are in groups of 5 
  (labels 
   ((cptr (viol add)
          (loop for i in viol collect
                ;; there's only 46 and 47 with 9 bars!
                (cond ((= i 47) 46)
                      ((= i 52) (if (= add 1) 51 58)) ;; 58 is closest
                      ((= i 77) (if (= add 1) 76 60)) ;; 60 is closest
                      ;; the seqs in the palette are in groups of 5 so
                      ;; try to get other seqs from within the 5, but
                      ;; if not, solve on an individual basis as above
                      (t (if (or (zerop (mod i 5))
                                 (zerop (mod (1+ i) 5)))
                             (- i add)
                           (+ i add))))))
    ;; section 6 is made from the first 25 seqs which are much shorter rhythmic
    ;; units so several are combined into longer seqs.  Just use the same
    ;; sub-seqs and order as the viol but start further along in the sublist of
    ;; subseqs. 
    (cptr-section6 (viol add)
                   (loop for sublist in viol
                         for len = (length sublist) 
                         with start = (* 2 add)
                         collect
                         (loop for i from start to (+ start (1- len))
                               collect (nth (mod i len) sublist)))))
   (let* ((last-four (subseq +skin-long-rthm-seq-refs+ 16))
          (4sections (split-into-sub-groups
                      (subseq +skin-long-rthm-seq-refs+ 0 16)
                      '(4 4 4 4)))
          (viol1 last-four)
          (viol2 (first 4sections))
          (viol3 (second 4sections))
          (viol4 (third 4sections))
          (viol5 (fourth 4sections))
          (viol6 +skin-short-rthm-seq-refs+)
          (viol7 last-four))
     `((1 ((viol ,viol1)
           (cptr1 ,(cptr viol1 1))
           (cptr2 ,(cptr viol1 2))))
       (2 ((viol ,viol2)
           (cptr1 ,(cptr viol2 1))
           (cptr2 ,(cptr viol2 2))))
       (3 ((viol ,viol3)
           (cptr1 ,(cptr viol3 1))
           (cptr2 ,(cptr viol3 2))))
       (4 ((viol ,viol4)
           (cptr1 ,(cptr viol4 1))
           (cptr2 ,(cptr viol4 2))))
       (5 ((viol ,viol5)
           (cptr1 ,(cptr viol5 1))
           (cptr2 ,(cptr viol5 2))))
       (6 ((viol ,viol6)
           ;; todo: start here, viol here is a list of lists!  and get the
           ;; individual voices to start with different sounds
           (cptr1 ,(cptr-section6 viol6 1))
           (cptr2 ,(cptr-section6 viol6 2))
           ))
       (7 ((viol ,viol7)
           (cptr1 ,(cptr viol7 1))
           (cptr2 ,(cptr viol7 2))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(make-slippery-chicken 
 '+skin+
 :set-palette +skin-harmonics+
 :set-map +skin-set-map+
 :tempo-map '((1 52))
 :instrument-palette '((viol (:lowest-written a1
                                              :starting-clef bass
                                              :highest-written d6
                                              :chords t))
                       (computer (:starting-clef percussion)))
 :ensemble '(((viol (viol))
              (cptr1 (computer))
              (cptr2 (computer))))
 :staff-groupings '(3)
 :bars-per-system-map '((1 8) (177 6) (183 4) (223 6))
 :rthm-seq-map +skin-rthm-seq-map+
 :rthm-seq-palette +skin-multiplier-rthm-seqs+
 :snd-output-dir "/winxp/snd/skin/sc"
 :sndfile-palette 
 '(((raw (neumann_09.wav  neumann_24.wav
                          neumann_10.wav  neumann_25.wav
                          neumann_11.wav  neumann_26.wav
                          neumann_01.wav  neumann_16.wav
                          neumann_28.wav  
                          neumann_02.wav  neumann_17.wav
                          neumann_30.wav 
                          neumann_03.wav  neumann_19.wav
                          neumann_33.wav 
                          neumann_06.wav  neumann_21.wav
                          neumann_08.wav  neumann_22.wav))
    (p-long-continuous1
     ( ;; bowed bridge
      (skin-processed-24-48-mono.wav :start 0.341 :end 16.373)
      ;; spe
      (skin-processed-24-48-mono.wav :start (2 1 749) :end (2 10 389))
      ;; click start then bowed bridge
      (skin-processed-24-48-mono.wav :start (1 20 848) :end (1 37 152))
      ;; clb spe with battuto attack and spectral development
      (skin-processed-24-48-mono.wav :start (4 22 489) :end (4 58 752))
      ;; high clicks bowed bridge, louder
      (skin-processed-24-48-mono.wav :start (1 37 152) :end (1 46 331))
      ;; bowed bridge small ric attack
      (skin-processed-24-48-mono.wav :start (3 25 119) :end (3 34 241))
      ))
    (p-long-continuous2
     ( ;; bowed bridge
      (skin-processed-24-48-mono.wav :start 16.373 :end 34.688)
      ;; bowed bridge, bounce start
      (skin-processed-24-48-mono.wav :start (2 24 877) :end (2 40 43))
      ;; crackling bowed bridge
      (skin-processed-24-48-mono.wav :start (2 40 43) :end (2 53 995))
      ;; bowed bridge creaking
      (skin-processed-24-48-mono.wav :start (2 53 995) :end (3 6 944))
      ;; bowed bridge
      (skin-processed-24-48-mono.wav :start 53.568 :end (1 12 75))
      ;; bowed bridge biggish ric attack with string noises, clb
      ;; re-attack etc. nicely varied
      (skin-processed-24-48-mono.wav :start (3 34 241) :end (3 58 358))
      ))
    (p-long-continuous3
     ( ;; clb spe
      (skin-processed-24-48-mono.wav :start (4 1 132) :end (4 22 489))
      ;; creaking bowed bridge
      (skin-processed-24-48-mono.wav :start (1 54 549) :end (2 1 749))
      ;; clb spe with attack
      (skin-processed-24-48-mono.wav :start (5 23 373) :end (5 42 255))
      ;; nervous bridge trem
      (skin-processed-24-48-mono.wav :start (15 57 10) (16 22 187))
      ;; clb spe with open string at end
      (skin-processed-24-48-mono.wav :start (6 25 27) :end (6 31 301))
      ;; clb spe
      (skin-processed-24-48-mono.wav :start (6 31 301) :end (6 54 422))
      ))
    (p-long-continuous4
     ( ;; clb spe with clacks at end
      (skin-processed-24-48-mono.wav :start (7 12 816) :end (7 44 726))
      ;; vertical bowing with trem attempt at end
      (skin-processed-24-48-mono.wav :start (8 31 202) :end (8 41 494))
      ;; clb spe with quick cresc at end
      (skin-processed-24-48-mono.wav :start (6 9 767) :end (6 25 27))
      ;; two open strings attack and clt
      (skin-processed-24-48-mono.wav :start (8 45 914) :end (9 1 601))
      ;; 1/2cl hair, groans, harmonics, m6th
      (skin-processed-24-48-mono.wav :start (9 17 250) :end (9 39 179))
      ;; harmonics, wispy, groany
      (skin-processed-24-48-mono.wav :start (9 46 693) :end (10 8 608))
      ;; nervous bridge trem
      (skin-processed-24-48-mono.wav :start (12 8 658) :end (12 21 6))      
      ))
    (f-long-continuous1
     ( ;; attack with lh waggling
      (skin-processed-24-48-mono.wav :start (5 42 255) :end (5 53 584))
      ;; clb spe with open string + slight cresc
      (skin-processed-24-48-mono.wav :start (5 54 433) :end (6 9 767))
      ;; harmonic double stop ff with clb ric at end
      (skin-processed-24-48-mono.wav :start (8 6 528) :end (8 17 707))
      ;; harmonic double stop p re-stroked cresc
      (skin-processed-24-48-mono.wav :start (8 19 898) :end (8 31 202))
      ;; gliss, trem, vtrem
      (skin-processed-24-48-mono.wav :start (9 1 601) :end (9 17 250))
      ;; ric attack, harmonic gliss
      (skin-processed-24-48-mono.wav :start (11 15 483) :end (11 28 717))
      ))
    (f-long-continuous2
     ( ;; ric attack harmonics
      (skin-processed-24-48-mono.wav :start (11 28 717) :end (11 40 181))
      ;; trem, vtrem
      (skin-processed-24-48-mono.wav :start (11 40 181) :end (11 55 8))
      ;; attack, harmonics, stroking
      (skin-processed-24-48-mono.wav :start (11 55 8) :end (12 8 658))
      ;; ric open+harmonics ff
      (skin-processed-24-48-mono.wav :start (14 25 802) :end (14 45 419))
      ;; tailpiece trem
      (skin-processed-24-48-mono.wav :start (12 35 691) :end (12 46 818))
      ;; trem behind bridge
      (skin-processed-24-48-mono.wav :start (12 46 818) :end (12 56 579))
      ))
    (f-long-continuous3
     ( ;; trem on bridge
      (skin-processed-24-48-mono.wav :start (12 56 579) :end (13 3 723))
      ;; trem spe
      (skin-processed-24-48-mono.wav :start (12 56 579) :end (13 44 580))
      ;; ric open+harmonics ff
      (skin-processed-24-48-mono.wav :start (14 6 11) :end (14 25 802))
      ;; loud trem behind bridge
      (skin-processed-24-48-mono.wav :start (12 21 6) :end (12 35 691))
      ;; trem harmonics
      (skin-processed-24-48-mono.wav :start (14 47 512) :end (15 11 883))
      ;; trem bridge
      (skin-processed-24-48-mono.wav :start (15 11 883) :end (15 28 105))
      ))
    (f-long-continuous4
     ( ;; subharmonic
      (skin-processed-24-48-mono.wav :start (16 52 780) :end (17 13 472))
      ;; subharmonic
      (skin-processed-24-48-mono.wav :start (17 19 10) :end (17 33 653))
      ;; bartok pizz, trem
      (skin-processed-24-48-mono.wav :start (15 28 105) :end (15 40 229))
      ;; subharmonic
      (skin-processed-24-48-mono.wav :start (17 37 116) :end (17 50 169))
      ;; harmonics
      ;; few pizzes
      (skin-processed-24-48-mono.wav :start (15 40 229) :end (15 57 10))
      (skin-processed-24-48-mono.wav :start (17 50 169) :end (18 2 112))
      ;; harmonic -> scratch
      (skin-processed-24-48-mono.wav :start (18 4 305) :end (19 3 940))
      ))
    (short
     ( ;; short trem on bridge
      (skin-processed-24-48-mono.wav :start (1 10 667) (1 12 75))
      ;; short bounce on bridge
      (skin-processed-24-48-mono.wav :start (2 24 877) :end (2 25 522))
      ;; clb bridge attack
      (skin-processed-24-48-mono.wav :start (3 16 815) :end (3 18 986))
      ;; short uneven trem on bridge
      (skin-processed-24-48-mono.wav :start (3 4 390) :end (3 6 944))
      ;; two short attacks
      (skin-processed-24-48-mono.wav :start (10 34 368) :end (10 36 971))
      )))
   ("/winxp/snd/skin/Audio"
    "/winxp/snd/skin"))
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Data generation for MAX/PD
;;; Get the sounding pitches for the harmonics in the set palette.
;;; NB These will be exact to the nearest 12th tone which should be just fine.

(defparameter +skin-set-sounding-pitches+
  (nthcdr 8 ;; the first 8 are just c4
          (loop for set in
                (get-all-data-from-palette (set-map +skin+))
                collect 
                ;; the first 3 notes are the nodes, the second 3 the sounding
                ;; pitches  
                (loop for pitch in (nthcdr 3 (data set))
                      collect (data pitch)))))
 |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF skin.lsp
