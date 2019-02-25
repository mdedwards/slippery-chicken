;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             coming.lsp
;;;
;;; Project:          you are coming into us who cannot withstand you
;;;                   for Ensemble Aventure, Freiburg
;;;
;;; Purpose:          Code for generation of score
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    27th December 2010 (Wals, Austria)
;;;
;;; $$ Last modified: 12:57:49 Wed Jul  4 2012 BST
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The title is taken from the poem, Final Notions, by Adrienne Rich (1929-):
;;; 
;;; It will not be simple, it will not take long
;;; It will take little time, it will take all your thought
;;; It will take all your heart, it will take all your breath
;;; It will be short, it will not be simple 
;;; 
;;; It will touch through your ribs, it will take all your heart
;;; It will not take long, it will occupy all your thought
;;; As a city is occupied, as a bed is occupied
;;; It will take your flesh, it will not be simple 
;;;
;;; You are coming into us who cannot withstand you
;;; You are coming into us who never wanted to withstand you
;;; You are taking parts of us into places never planned
;;; You are going far away with pieces of our lives
;;;
;;;
;;; It will be short, it will take all your breath
;;; It will not be simple, it will become your will
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sc)
(in-scale :quarter-tone)

;;; The rthm-chains data and object
(defparameter +coming-rthm-chain-main+
  (let ((1-beat-rthms1                  ; 8 in total
         '(((e) e) 
           (- s (s) (s) s -) 
           ({ 3 (te) - te te - })
           ((e.) s) 
           (q)
           ((e) - s s -)
           ((s) s (e))
           ((s) s (s) s)))
        (1-beat-rthms2                  ; what we transition to
         '(({ 3 (te) te (te) })
           ({ 3 - te (te) te - })
           ({ 3 (te) - te te - })
           ({ 3 (tq) te })
           ({ 3 (te) - ts ts te - })
           (- s s s s -)
           ({ 3 - te te - (te) })
           ({ 3 - te te te - })))
        (slower-rthms1                  ; the 2/4 bars: 10 total
         '(((q q) 
            ((q) q) 
            ((q) q) 
            ((q) (s) e.)
            (- e e - (e) e)
            (- e. s - (e) e) 
            (- e. s - (q)) 
            ((e.) s (e) e)
            (- e. s - (e.) s)
            ((e.) s (q)))
           (({ 3 tq tq tq })            ; what we transition to
            (q - s e. -)
            (q (s) e.)
            (q (s) - s e -)
            ({ 3 tq tq - te te - })
            (- e. s - (e) - s s -) 
            ({ 3 tq te } { 3 (tq) te })
            ({ 3 - te te te - } { 3 (te) - te te - })
            (q { 3 (te) tq })
            (q { 3 (tq) te }))))
        (slower-rthms2                  ; the 3/4 bars: 5 total
         '((((e.) s (e) e (s) e.)
            (- e e - (e) e (q))
            (- e. s - - +e e - (q)) 
            ((q) - e. s - (q))
            (q (e.) s (q)))
           (({ 3 (tq) tq tq } (q))      ; what we transition to
            (- e. s - (q) (s) - s e -)
            ({ 3 tq te } (q) q)
            ({ 3 - te te te - } { 3 (te) - te te - } { 3 (tq) te })
            (q (q) { 3 (te) - te te - })))))
    (make-instance 'rthm-chain
                   :num-beats (nth-value 1 (fibonacci 400))
                   ;; so the flute voice will be the 'fast' player, clarinet
                   ;; the 'slow' 
                   :players '(flute clarinet) ; to start with
                   :1-beat-rthms (list 1-beat-rthms1 1-beat-rthms2)
                   :section-id 1
                   :rests '(q h q w) 
                   :1-beat-fibonacci nil
                   :slow-fibonacci t
                   :slower-rthms (list slower-rthms1 slower-rthms2)
                   :do-rests t
                   ;; the recurring-event object data: every 5 events 3x, then
                   ;; every 8 2x...
                   :rest-re '((5 3) (8 2) (13 1))
                   :activity-curve  '(0 7 50 10 100 10) 
                   :do-sticking t
                   :do-sticking-curve '(0 0 100 1)
                   :sticking-rthms '(e e. e s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.2.11 now we've got two voices of counterpoint (flute and clarinet), add
;;; the rest based on their rthm-seqs and create the ensemble groupings for the
;;; different sections.

(let* ((flute-voices '(flute violin piano-lh viola))
       (clarinet-voices '(clarinet cello bassoon piano-rh marimba))
       ;; the sequence numbers (starting from 1) at which we'll change the
       ;; ensemble (and maybe have a longer pause, but that's for later).
       ;; Determined by ear. See coming.mm for details
       ;; when I decided these the piece was considerably longer than it turned
       ;; out to be so in calculating the actual seq to change at we have to
       ;; scale it: see csmult below
       (change-seqs '(1 19 29 67 79 91 129 143 160 210 211 218 252 274 309 332
                      ;; 422 is the last seq
                      343 353 355 363 387 410 422)))
  (flet ((add-voices (voices based-on)
           ;; from rthm-chain.lsp: The challenge here is that each rthm-seq has
           ;; potentially its own time signature structure: there could be a
           ;; 2/4 bar followed by 5/4 then 3/16; or any other combination of
           ;; any meter.  So first of all, we analyse the time-signature
           ;; structure of the existing rthm-seqs and save those with the same
           ;; bar/meter structure together in the order in which they occurred.
           ;; When creating the extra voice then, we actually start ahead of
           ;; the main voice, by choosing <offset> similar rthm-seqs in advance
           (loop for v in (rest voices) and offset from 1 do
                (add-voice +coming-rthm-chain-main+
                           based-on v offset)))
         ;; check that we've got a 'flute' and a 'clarinet' voice in every
         ;; ensemble (i.e. we should have the 1-beat and slower rthms always
         ;; present.)
         (both-types (ensemble)
           (let ((check '()))
             (loop for v in ensemble do
                  (cond ((member v flute-voices) (pushnew 'fl check))
                        ((member v clarinet-voices) (pushnew 'cl check))
                        (t (error "coming: huh?: ~a" v))))
             (= 2 (length check)))))
    ;; first of all, add the groups: this will set up the rthm-chain's
    ;; rthm-seq-map so that all voices are playing all the time--we deal with
    ;; this below.
    (add-voices flute-voices '(1 flute))
    (add-voices clarinet-voices '(1 clarinet))
    ;; from the rthm-seq-map class, this adds rthm-seq repeats (so rhythms only,
    ;; not notes) at recurring intervals
    (add-repeats +coming-rthm-chain-main+ 
                 ;; of course this means we have 1+ seq items of these numbers
                 ;; all together, as these are the number of _repeats_
                 '((3 3) (2 2) (1 1)) ; when to repeat
                 '((3 3) (5 2) (8 1) (13 1)) ; now many to repeat when repeating
                 :print nil)
    ;; we're now going to algorithmically generate the scoring, moving in
    ;; general from 4 instruments playing at the beginning to 8 by the end,
    ;; keeping track of who's played and how much and trying to balance this
    ;; out to maximize timbral variety and rests for the players.
                                        ; -1 because we have the last seq in
                                        ; the list too 
    (let* ((num-ins-procession (procession (1- (length change-seqs)) 
                                   '(4 5 6 7 8)))
           (total-entries (loop for num-ins in num-ins-procession sum num-ins))
           ;; we don't want to just append flute-voices and clarinet-voices as
           ;; we determine an ordering here
           (ins '(violin clarinet flute viola cello bassoon marimba piano-rh
                  piano-lh))
           (num-ins (length ins))
           ;; the hash table to keep track of how often each ins has played
           (hash (let ((ht (make-hash-table)))
                   (loop for i below num-ins do
                        (setf (gethash i ht) 0))
                   ht))
           (csmult (/ (first (last change-seqs))
                      (length (get-data-data '(1 flute)
                                             +coming-rthm-chain-main+))))
           ;; first of all we're going to spread out the instruments so
           (ins-order-procession (procession total-entries num-ins))
           ;; create the instrumental groupings (scoring) we'll want so that we
           ;; can make those not playing silent in the now full/everyone
           ;; constantly playing rthm-seq-map
           (groups (loop with ins-order-procession-cp =
                        (copy-list ins-order-procession)
                      with group with ngroup
                      for num-ins in num-ins-procession 
                      do (setf group '()   ; the instrument symbols
                               ngroup '()) ; their position in <ins>
                      collect
                      (loop repeat num-ins 
                                        ; procession min is 1
                         for ni = (1- (pop ins-order-procession-cp))
                         for i = (nth ni ins)
                         do
                         ;; this messes with the results of the procession
                         ;; algorithm but we really can't have the same voice
                         ;; twice in a single ensemble
                         (loop with happy = nil
                            with ignore = (copy-list ngroup) ; once
                            for count from 0 
                            for try = (cons i group)
                            until happy do
                            (when (= count 1000) ; no infinite loop
                              (error "woah!: 1000 tries already!"))
                            (if (member ni ngroup)
                                (progn 
                                  ;; so if this instrument is already in the
                                  ;; current scoring we have to get another
                                  (push ni ignore)
                                  ;; from the rthm-chain class: ignoring the
                                  ;; insuments in the ignore list, get the
                                  ;; least-used element--here the instrument
                                  ;; number--auto-incrementing its count
                                  (setf ni (hash-least-used hash :ignore ignore)
                                        i (nth ni ins)))
                                ;; else clause!
                                (setf happy t))
                            (when (and (= (length group) num-ins)
                                       happy)
                              ;; if we don't have at least one 1-beat and one
                              ;; slower-rthms group in there, keep going
                              (setf happy (both-types try))))
                         ;; keep tally
                         (incf (gethash (position i ins) hash))
                         (push ni ngroup)
                         (push i group)
                         finally (return (nreverse group))))))
      ;; just print the groups and the spread of instruments
      (print groups)
      (maphash #'(lambda (key val) (format t "~&~a: ~a" (nth key ins) val))
               hash)
      ;; now all instruments are playing all the time, so sculpt them out
      ;; according to the groupings we've just made
      (loop with all-players = (append clarinet-voices flute-voices)
         for start in change-seqs and end in (rest change-seqs)
         for group1 in groups and group2 in (rest groups)
         do
         ;; see change-seqs above for why we have to scale the change-seq
         ;; number 
         (setf end (floor end csmult)
               start (floor start csmult))
         (loop for not-playing in (remove-all group1 all-players) do
              (delete-from-to-in-map (list 1 not-playing)
                                     (max 0 (1- start))
                                     ;; we don't go up to end itself as
                                     ;; that change seq needs both groups
                                     ;; playing 
                                     (- end 2)
                                     +coming-rthm-chain-main+))
         ;; now the change-seq itself
         (loop for not-playing in
              (remove-all (append group1 group2 all-players) all-players) do
              (delete-nth-in-map (list 1 not-playing) (1- end)
                                 +coming-rthm-chain-main+))))))

;; some more repeats                         seq no. (determined by ear)
(add-repeats-simple +coming-rthm-chain-main+ 228 5 :print nil)

;; and now some instrumental rhythmic doublings
(defparameter +coming-doublings+
  '((11 16 violin viola) ; viola was silent but now doubles violin
    (72 76 violin clarinet) ; added 22.7.11 (Pula)
    (102 116 cello (violin viola))
    (151 162 violin  viola)
    (117 125 cello violin)
    (102 125 bassoon flute)
    (138 150 clarinet flute)
    (180 198 clarinet bassoon)
    (237 250 bassoon flute)
    (17 46 piano-rh piano-lh)
    (54 68 piano-lh piano-rh)
    (87 87 piano-rh piano-lh)
    (102 125 piano-lh piano-rh)
    (131 150 piano-rh piano-lh)
    (218 236 piano-lh piano-rh)))

(loop for double in +coming-doublings+ do
     (apply #'double (append (list +coming-rthm-chain-main+ 1) double)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; modify our default piano chord function somewhat so that chords are 3rd
;;; instead of 2nd based
(let ((last-chord (make-chord '(d8 e8)))
      (cs2 (make-pitch 'cs2)))
  (defun coming-piano-chord (curve-num index pitch-list pitch-seq instrument
                             set) 
    (declare (ignore set instrument pitch-seq curve-num))
    (let* ((start (max 0 (- index 3))) 
           (at-start (nth start pitch-list))
           (nump (length pitch-list))
           (lots (> nump 4))
           (down-low (pitch< at-start cs2))
           (result (list at-start)))
      (loop 
         ;; try and get every other note
         for i from (if lots 
                        (if down-low
                            (+ 3 start) ;; 4ths if low 
                            (+ 2 start))
                        (1+ start))
         by (if lots 2 1) 
         ;; 26.2.11 don't get more than 2 notes if we're down low (remember
         ;; at-start is already in there, so we try for 4 notes if > c2
         ;; otherwise 2
         repeat (if (not down-low) 3 2)
         for p = (nth i pitch-list)
         for interval = (when p (pitch- p at-start))
         do
         (when (and p (<= interval 12)
                    (not (member p result :test #'note=))
                    ;; 24.7.11 no low M7ths
                    (and down-low (/= interval 11)))
           (push p result)))
      (if (> (length result) 1)
          (progn 
            (setf result (make-chord result))
            ;; 24.7.11 (Pula) don't have repeated chords or new chords with
            ;; more than 1 common note 
            (if (> (common-notes result last-chord) 1)
                (coming-piano-chord nil (1+ index) pitch-list nil nil nil)
                (setf last-chord result)))
          (first result)))))

;;; ditto for marimba: prefer thirds
(defun coming-marimba-chord (curve-num index pitch-list pitch-seq instrument
                             set)
  (declare (ignore set instrument pitch-seq curve-num))
  (let ((at-index (nth index pitch-list))
        p1 p2)
    (cond ((> index 1) (setf p1 (nth (- index 2) pitch-list)
                             p2 at-index))
          ((> index 0) (setf p1 (nth (1- index) pitch-list)
                            p2 at-index))
          ((> (length pitch-list) 2) (setf p1 at-index 
                                           p2 (nth (+ index 2) pitch-list)))
          ((= (length pitch-list) 2) (setf p1 at-index 
                                           p2 (nth (1+ index) pitch-list)))
          (t (setf p1 at-index
                   p2 nil)))
    ;; don't create diads > 8ve
    (when (and p2 (> (pitch- p2 p1) 12)) ; assuming pitch-list is sorted
      (setf p2 nil))
    (if p2
        (make-chord (list p1 p2))
        (make-chord (list p1)))))

(defparameter +coming-instrument-palette+
  (clone +slippery-chicken-standard-instrument-palette+))

(setf (chord-function
       (get-data 'piano-lh +coming-instrument-palette+))
      'coming-piano-chord
      (chord-function
       (get-data 'piano +coming-instrument-palette+))
      'coming-piano-chord
      (chord-function
       (get-data 'marimba +coming-instrument-palette+))
      'coming-marimba-chord)

;;; although we want sc to choose notes for marimba all the way through, we
;;; want it called percussion as we use cymbals too (hand-entered) 
(setf (staff-name 
       (get-data 'marimba +coming-instrument-palette+))
      "percussion"
      (staff-short-name 
       (get-data 'marimba +coming-instrument-palette+))
      "perc"
      (starting-clef
       (get-data 'cello +coming-instrument-palette+))
      'treble
      (starting-clef
       (get-data 'viola +coming-instrument-palette+))
      'treble)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use the default 'melodic' lines, now we've modified them to include chords.
(create-psps (palette +coming-rthm-chain-main+))

(make-slippery-chicken
 '+coming-rthm-chain+
 :title "coming"
 ;; increase from 0.125 to become a little more angular
 :fast-leap-threshold 0.15
 :instrument-palette +coming-instrument-palette+
 :ensemble '(((flute ((flute piccolo)
                      :midi-channel 1 :microtones-midi-channel 2))
              (clarinet ((b-flat-clarinet bass-clarinet)
                         :midi-channel 3 :microtones-midi-channel 4))
              (bassoon (bassoon :midi-channel 5 :microtones-midi-channel 6))
              (marimba (marimba :midi-channel 7))
              (piano-rh (piano :midi-channel 8))
              (piano-lh (piano-lh :midi-channel 9))
              (violin (violin :midi-channel 10 :microtones-midi-channel 11))
              (viola (viola :midi-channel 12 :microtones-midi-channel 13))
              (cello (cello :midi-channel 14 :microtones-midi-channel 15))))
 ;; remember here that we have to use the bar numbers--if not using seq
 ;; numbers--before we deleted bars (see coming-no-delete-bars.pdf)
 :set-limits-high '((piano-rh (0 c4 40 c4  96 d7  97 c8 643 c8))
                    (piano-lh (0 g2 40 g2  96 c5  97 c4 643 c4))
                    (violin (0 c7 217 c7 218 b4 222 b4 223 c7 259 c7)) 
                    ;; hmm, though bar numbers should work, it's actually best
                    ;; to express x axis in seq numbers.
                    (flute (0 c9 129 c9 130 a5 131 a5 136 c9 259 c9))
                    (clarinet (0 g6 105 g6 109 bf5 121 bf5 127 g6 134 c5 153 c5
                               157 g6 232 g6 237 c6 242 c6 247 g6 259 g6))) 
 :set-limits-low '((piano-rh (0 gs2 40 gs2 96 cs5 97 cs4 643 c4))
                   (piano-lh (0 a0  40 a0  96 b3  97 a0 137 f2 170 c3 171 a0
                              280 a0 334 c3 335 a0 643 a0))
                   (bassoon (0 f2 35 f2 36 bf1 202 bf1 220 e3 230 bf1 280 bf1
                             285 b2 300 c4 330 e3 357 e3 385 bf1 500 bf1 510 e3
                             525 bf1 643 bf1))
                   (violin (0 g3 643 g3))
                   (clarinet (0 bf1 102 bf1 107 c5 115 c5 120 d3 162 bf1
                              232 bf1 237 d4 242 d3 255 c5 259 c5)))
 ;; remember we're entering sequence numbers here, not bars.
 :instrument-change-map '((1 ((clarinet ((1 bass-clarinet)
                                         (96 b-flat-clarinet)
                                         (164 bass-clarinet)
                                         (237 b-flat-clarinet)))
                              (flute ((1 flute) (164 piccolo))))))
 :rehearsal-letters '(22 40 96 117 137 153 164 202 231 256 280 333 343 385 421
                      ;; we've added 10 from 583
                      461 471 500 534 544 575 593 622 630) 
 :staff-groupings '(3 1 2 3) 
 ;; for the first time I'm using more of a scale than a chord but of course
 ;; this scale has harmonic properties (e.g. microtones and something a little
 ;; octophonic like).  I also only change chord once--yikes!
 :set-palette (make-set-palette 
               'coming-set-palette
               (let* ((pitches '(c4 d4 ef4 f4 gf4 af4 bqf4 cqf5 df5))
                      (ipitches (invert-pitch-list pitches))
                      (s1 (make-stack 's1 pitches 3))
                      (s2 (transpose (make-stack 's2 ipitches 3) 13)))
                 (list s1 s2)))
 :set-map `((1 ,(append
                 (ml 's1 101)
                 ;; chord/scale change
                 (ml 's2 78)          ; K = seq102, p56 (before deleting bars)
                 (ml 's1 80))))       ; P = seq 180, p92 (before deleting bars)
 :bars-per-system-map '((1 7))
 :rthm-seq-palette (palette +coming-rthm-chain-main+)
 :rthm-seq-map +coming-rthm-chain-main+
 :tempo-map 
 ;; set tempi by hand.  we have a max tempo of start-tempo * 4/3 and as we have
 ;; max 4 steps from start to max tempo our inc is 1/4 of this
 (let* ((start-tempo 140)
        (inc (/ (- (* start-tempo 4/3) 140) 4))
        ;; bar number, inc steps NB bar nums before deleting!
        (changes '(96 1 117 -1 164 1 202 1 231 -1 280 1 385 -1 421 -1 461 1
                   500 1 544 -2 593 1 622 1 630 1 637 1)))
   (append
    (list (list 1 start-tempo))
    (loop with current-steps = 0
       for bar in changes by #'cddr
       for steps in (cdr changes) by #'cddr
       do (incf current-steps steps)
       collect (list bar
                     (list 'q (round (+ start-tempo
                                        (* current-steps inc)))
                           (if (< steps 0) "Meno Mosso" "Piu Mosso")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(proclaim '(special +coming-rthm-chain+))

;;; using the doubling data, add a textual indication to the player in the
;;; score as to who they're playing with.
(labels ((write-double (ins with start-bar)
           (add-marks-to-note 
            +coming-rthm-chain+ start-bar 1 ins 
            (format nil "with ~a"
                    (string-downcase (list-to-string with ", ")))))
         (write-doubles (instruments start-bar)
           (loop for ins in instruments
              for others = (remove ins instruments)
              do
              (write-double ins others start-bar))))
  (loop for double in +coming-doublings+
     for start-bar = (get-bar-num-from-ref
                      +coming-rthm-chain+ 1 (first double) 1)
     for ins = (flatten (list (third double) (fourth double)))
     do
     (unless (or (equal ins '(piano-lh piano-rh))
                 (equal ins '(piano-rh piano-lh)))
       (write-doubles ins start-bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now set the popcorn dynamics

(let (amp-list last)
  (defun add-popcorn-dynamic (event &optional amps)
    (if (and (not event) amps)
        (setf amp-list amps
              last 0.0)
        (when (needs-new-note event)
          (setf (amplitude event) (pop amp-list))))))

(let ((pc (make-popcorn '(0.02 0.03) :min-spike 2 :max-spike 2.4)))
  (heat pc)
  (flet ((set-amps (start-bar end-bar start-dynamic end-dynamic)
           ;; start and end bars are inclusive
           (let ((start-amp (dynamic-to-amplitude start-dynamic))
                 (end-amp (dynamic-to-amplitude end-dynamic))
                 (num-notes (count-notes +coming-rthm-chain+
                                         start-bar end-bar t))
                 (pc-clone (clone pc)))
             (fit-to-length pc-clone num-notes)
             (scale pc-clone (max start-amp end-amp) (min start-amp end-amp))
             (add-popcorn-dynamic nil (if (< end-amp start-amp)
                                          (reverse (kernels pc-clone))
                                          (kernels pc-clone)))
             (unless (= (numk pc-clone) num-notes)
               (error "numk = ~a, num-notes = ~a" (numk pc-clone) num-notes))
             (process-events-by-time +coming-rthm-chain+
                                     #'add-popcorn-dynamic
                                     :start-bar start-bar :end-bar end-bar))))
    (set-amps 1 117 'pp 'mp)
    (set-amps 118 163 'pp 'p)
    (set-amps 164 201 'mf 'pp)
    (set-amps 202 230 'mf 'ff)
    (set-amps 231 279 'pp 'p)
    (set-amps 280 342 'f 'p)
    (set-amps 343 384 'f 'ff)
    (set-amps 385 420 'p 'ff)
    (set-amps 421 460 'pp 'mf)
    (set-amps 461 470 'ff 'mf)
    (set-amps 471 499 'p 'f)
    (set-amps 500 524 'f 'ff)
    (set-amps 525 533 'p 'mf)
    (set-amps 534 543 'f 'ff)
    (set-amps 544 620 'pp 'f)))
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(midi-play +coming-rthm-chain+ :midi-file "/tmp/coming.mid")
(write-lp-data-for-all +coming-rthm-chain+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF coming.lsp
