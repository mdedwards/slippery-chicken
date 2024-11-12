;;; ============================================================================
;;;  MINI EXAMPLES - TWO SIMPLE SC EXAMPLES
;;; ============================================================================

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 e4 g4))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

(let* ((random-1-2 (loop repeat 30 collect (1+ (random 2))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 e4 g4)))
                        (2 ((fs4 b4 ds5))))
         :tempo-map '((1 (q 180)))
         :rthm-seq-palette '((1 ((((2 4) q e e))
                                 :pitch-seq-palette ((1 2 3))))
                             (2 ((((2 4) s s s s e s s))
                                 :pitch-seq-palette ((1 2 3 2 1 3 2)))))
         :set-map (list (list 1 random-1-2))
         :rthm-seq-map (list (list 1 (list (list 'vn random-1-2)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

(let* ((random-1-2 (loop repeat 30 collect (1+ (random 2))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 e4 g4)))
                        (2 ((fs4 b4 ds5))))
         :tempo-map '((1 (q 180)))
         :rthm-seq-palette '((1 ((((2 4) q - e e -))
                                 :pitch-seq-palette ((1 2 3))))
                             (2 ((((2 4) - s s s s - - e s s -))
                                 :pitch-seq-palette ((1 2 3 2 1 3 2)))))
         :set-map (list (list 1 random-1-2))
         :rthm-seq-map (list (list 1 (list (list 'vn random-1-2)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

(let* ((x 1)
       (y 2)
       (z -5))
  (+ x y z))


(transpose (make-pitch 'c4) 1)

(let* ((scale '(c4 d4 e4 f4 g4 a4 b4))
       (length (length scale))
       (chord1 '(c4 e4 g4))
       (positions (loop for note in chord1 collect (position note scale)))
       (new-position 6)
       (chord2 (loop for position in positions collect 
                    (nth (mod (+ position new-position) length) scale))))
  chord2)
  


(let ((sp (make-set-palette 'test nil))
      (tlist '()))
  (loop for i from 1 to 12 do
       (setf tlist (loop repeat 7 collect (+ -5 (random 12))))
       (add 
        (make-complete-set 
         (loop with centre = 'c4
            for transp in tlist
            collect (transpose (make-pitch centre) transp))
         :id i)
        sp)))

(print-simple (transpose (make-chord '(c4 e4 g4)) 3))



;;; algorithmic generation of random sets from a limited set of pitches
(defun get-set-pitches (pitches)
  (let* ((len (length pitches))
         (half-len (/ len 2)))
    (subseq pitches (random half-len) (+ half-len (random half-len)))))

(let* ((pitches '(c4 cs7 d4 ds4 e4 f4 fs4 g4)))
  `((set1 ,(get-set-pitches pitches))
    (set2 ,(get-set-pitches pitches))
    (set3 ,(get-set-pitches pitches))))


(let* ((mini
           (make-slippery-chicken
            '+mini+
            :ensemble '(((pno (piano :midi-channel 1))))
            :set-palette '((1 ((c4 e4 fs4 a4 d5 f5 g5)
                               :subsets ((pno (c4 fs4 a4))))))
            :set-map '((1 (1)))
            :rthm-seq-palette '((1 ((((2 4) q e e))
                                    :pitch-seq-palette ((1 (2) 3)))))
            :rthm-seq-map '((1 ((pno (1))))))))
      (midi-play mini)
      (cmn-display mini)
      (write-lp-data-for-all mini))


(let* ((x 1)
       (y 2))
  (+ x y))



(let* ((random-1-2 (loop repeat 30 collect (1+ (random 2))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 e4 g4 b4)))
                        (2 ((fs4 b4 ds5 es5))))
         :tempo-map '((1 (q 180)))
         :rthm-seq-palette '((1 ((((2 4) q - e e -))
                                 :pitch-seq-palette ((1 2 3) (1 3 2) (1 4 3))))
                             (2 ((((2 4) - s s s s - - e s s -))
                                 :pitch-seq-palette ((1 2 3 2 1 3 2)
                                                     (1 2 3 2 4 3 2)))))
         :set-map (list (list 1 random-1-2))
         :rthm-seq-map (list (list 1 (list (list 'vn random-1-2)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  MINI EXAMPLES - OBJECTS SLOTS ETC.
;;; ============================================================================

;;; ============================================================================
;;;  CLOS'S MAKE-INSTANCE
;;; ============================================================================

;;;  just as an example. focus on make- functions below.
;;; ----------------------------------------------------------------------------

(make-instance 'assoc-list :id 'al-object-1 :data '((3 17) (ob bf3) (c4 q)))

;;; ============================================================================
;;;  USING A METHOD: GET-FIRST METHOD OF ASSOC-LIST CLASS
;;; ============================================================================

;;;  (must first make object, though make-functions not explained till later)
;;; ----------------------------------------------------------------------------

(let* ((al-object-1 
        (make-instance 'assoc-list 
                       :id 'al-object-1 :data '((3 17) (ob bf3) (c4 q)))))
  (get-first al-object-1))

;;; ============================================================================
;;;  FUNCTION DEF AND USAGE
;;; ============================================================================

(defun my-get-harmonics (fundamental &key (start-at 1) (min-freq 20)
                      (max-freq 20000))
  (loop for h from start-at
     for freq = (* fundamental h)
     while (<= freq max-freq)
     if (>= freq min-freq)
     collect freq))

(my-get-harmonics 63 :start-at 2 :max-freq 1010)

;;; ============================================================================
;;;  MAKE- FUNCTIONS
;;; ============================================================================

#|
;;; just an example: don't evaluate this as it'll override the existing fun
(defun make-rhythm (rthm &key (is-rest nil) (is-tied-to nil) (duration nil)
                    (tempo 60.0))
  (cond ((rhythm-p rthm) (clone rthm))
        ((and rthm (not duration))
         (make-instance 'rhythm :data rthm :is-rest is-rest 
                        :is-tied-to is-tied-to))
        ((and (numberp duration) rthm)
         (error "rhythm::make-rhythm: can't process both a <rthm> (~a) and ~
                 <duration> (~a) (duration should be T or NIL)" rthm duration))
        ((and rthm duration)
         (let* ((rthm-letter
                 (get-rhythm-letter-for-duration 
                  rthm :tempo tempo :warn nil :error-on-fail nil)))
           ;; MDE Mon Mar 19 22:14:20 2012 
           (unless rthm-letter
             (setf rthm-letter (rationalize-if-necessary rthm
                                                         :error-on-fail nil)
                   rthm-letter 
                   (if (and rthm-letter (= 1 (length rthm-letter)))
                       (data (first rthm-letter))
                       nil)))
           (make-instance 'rhythm 
                          :data rthm-letter ;(when rthm-letter rthm-letter)
                          :duration (if rthm-letter -1 rthm)
                          :is-rest is-rest :is-tied-to is-tied-to)))
        (t nil)))

(make-rhythm 16 :is-rest t)
|#
;;; ============================================================================
;;;  MAKE-SLIPPERY-CHICKEN
;;; ============================================================================

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 e4 g4))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) q e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  MINI EXAMPLES - CORE USAGE
;;; ============================================================================

;;; ============================================================================
;;;  SETTING THE ENVIRONMENT
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;;  TUNING SCALES
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ----------------------------------------------------------------------------

(in-scale :quarter-tone)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1
                                  :microtones-midi-channel 2))))
         :set-palette '((1 ((c4 cqs4 gqf4 af4 bqf4 cs5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ----------------------------------------------------------------------------

(in-scale :twelfth-tone) ; cmn only

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1
                                  :microtones-midi-channel 2))))
         :set-palette '((1 ((css4 dsf4 fts4 gtf4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (midi-play mini)
  (cmn-display mini))

;;; ============================================================================
;;;  - THE CALL TO MAKE-SLIPPERY-CHICKEN
;;;  - INSTRUMENT-PALETTE
;;;  - ENSEMBLE
;;; ============================================================================

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :instrument-palette +slippery-chicken-standard-instrument-palette+
         :ensemble '(((flt (flute :midi-channel 1))
                      (clr (b-flat-clarinet :midi-channel 2))
                      (vln (violin :midi-channel 3))
                      (vla (viola :midi-channel 4))
                      (vlc (cello :midi-channel 5))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((flt (1))
                             (clr (1))
                             (vln (1))
                             (vla (1))
                             (vlc (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  STAFF GROUPINGS
;;; ============================================================================

;;;  grouping a
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((flt (flute :midi-channel 1))
                      (clr (b-flat-clarinet :midi-channel 2))
                      (vln (violin :midi-channel 3))
                      (vla (viola :midi-channel 4))
                      (vlc (cello :midi-channel 5))))
         :staff-groupings '(2 3)
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((flt (1))
                             (clr (1))
                             (vln (1))
                             (vla (1))
                             (vlc (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  grouping b
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((flt (flute :midi-channel 1))
                      (clr (b-flat-clarinet :midi-channel 2))
                      (vln (violin :midi-channel 3))
                      (vla (viola :midi-channel 4))
                      (vlc (cello :midi-channel 5))))
         :staff-groupings '(1 1 3)
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((flt (1))
                             (clr (1))
                             (vln (1))
                             (vla (1))
                             (vlc (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  TEMPO-MAP
;;; ============================================================================

;;;  defaut = q 60
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  first bar only
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  several tempi in the map
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)) (3 (e 44)) (5 (q. 144 "Allegro")))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  SET-PALETTE and SET-MAP
;;; ============================================================================

;;;  standard example
;;; ----------------------------------------------------------------------------

(in-scale :quarter-tone)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1
                                   :microtones-midi-channel 2))))
         :set-palette '((1 ((c4 eqf4 fqs4 a4 bqf4 d5)))
                        (2 ((c4 d4 fqs4 g4 bqf4 cqs5 d5)))
                        (3 ((d4 eqf4 g4 a4 cqs5 d5))))
         :set-map '((1 (1 2 3 1 2 3 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  using non-numbers as IDs
;;; ----------------------------------------------------------------------------

(in-scale :quarter-tone)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1
                                   :microtones-midi-channel 2))))
         :set-palette '((set1 ((c4 eqf4 fqs4 a4 bqf4 d5)))
                        (set2 ((c4 d4 fqs4 g4 bqf4 cqs5 d5)))
                        (set3 ((d4 eqf4 g4 a4 cqs5 d5))))
         :set-map '((1 (set1 set2 set3 set1 set2 set3 set1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  SET-LIMITS-HIGH and SET-LIMITS-LOW
;;; ============================================================================

;;;  original
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))
                      (vla (viola :midi-channel 2))
                      (vlc (cello :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1 1 1 1))
                             (vla (1 1 1 1 1 1 1 1 1 1))
                             (vlc (1 1 1 1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  individual
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))
                      (vla (viola :midi-channel 2))
                      (vlc (cello :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-limits-high '((vln (0 a4 50 c6 100 c5))
                            (vla (0 d4 50 e5 100 g4))
                            (vlc (0 d3 50 g4 100 d4)))
         :set-limits-low '((vln (0 g3 50 c5 100 c4))
                           (vla (0 d3 50 e4 100 g3))
                           (vlc (0 c2 50 g3 100 d3)))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1 1 1 1))
                             (vla (1 1 1 1 1 1 1 1 1 1))
                             (vlc (1 1 1 1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  all
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))
                      (vla (viola :midi-channel 2))
                      (vlc (cello :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-limits-high '((all (0 a4 50 c6 100 c5)))
         :set-limits-low '((all (0 c2 50 g3 100 d3)))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1 1 1 1))
                             (vla (1 1 1 1 1 1 1 1 1 1))
                             (vlc (1 1 1 1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  RHYTHMS
;;; ============================================================================

;;;  numeric representation
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) 1)
                                  (2 4 8 16 32 32))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  alphabetic representation
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) w)
                                  (h q e s s))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  rests
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) h (q) e (16) 16))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  dots
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) e. s (e..) 32 (8\.) 16 8\.. 32))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  ties
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) q+e e 4+8 8) ; ties from rhythm to the
                                        ; next 
                                  (+q +e e 4 \+8 8)) ; ties to the previous
                                 :pitch-seq-palette ((1 2 3 4 5 6 7)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  tuplets - numeric
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) - 12 12 - (12) - 20 20 (10) 20 -))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  tuplets - alphabetic
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) - te t8 - (te) - fs fs (f8) f16 -))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  tuplets - partial beat
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) te e ts s ts s ts ts))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  tuplets - brackets and numbers
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) { 3 te te (te) } 
                                   { 5 - fs fs (f8) f16 - }))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  tuplets - nested
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) { 3 12 12 12 } 
                                   { 3  12 12 { 3 36 36 36 } } ))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  beams
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e - - s s s s - e - s s - 
                                   (s) - s (s) s -))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8 7 6 5)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  repeat rhythms shorthand
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) s x 16))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8 
                                                        8 7 6 5 4 3 2 1)))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  RTHM-SEQ-PALETTE and RTHM-SEQ-MAP and RTHM-SEQ
;;; ============================================================================

;;;  straightforward usage, single psp, no marks, multiple bars
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((5 5 3 3 3 4 3 3 1 1 1 1 1))))
           (2 ((((3 4) { 3 - te (te) te - } +q s (e.))
                ((2 4) (q.) e)
                (e q e))
               :pitch-seq-palette ((6 3 5 5 2 2 2))))
           (3 ((((7 8) - s s s s - +q - +e (s) s - (e)))
               :pitch-seq-palette ((6 6 3 5 7)))))
         :rthm-seq-map '((1 ((vln (1 2 3 1 2 3 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  MARKS
;;; ============================================================================

;;;  not including slurs
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e s s -))
               :pitch-seq-palette ((5 3 4 3 1))
               :marks (ppp 1 s 2 "dolce" 4)))
           (2 ((((2 4) - e e - - s s s - (s)))
               :pitch-seq-palette ((5 3 4 3 1))
               :marks (a 1 3 5))))
         :rthm-seq-map '((1 ((vln (1 2 1 2 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  two ways to draw slurs/phrases
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) - e e e e - - e e e e -))
               :pitch-seq-palette ((1 2 3 4 5 6 7 8))
               :marks (beg-sl 1 end-sl 2 slur 5 6 beg-phrase 1 end-phrase 8))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  RTHM-SEQ-MAP
;;; ============================================================================

;;;  straightforward usage
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))
                      (vla (viola :midi-channel 2))
                      (vlc (cello :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((vln (3 2 1 1 3 3 3 2 1 1 2))
                             (vla (1 1 3 2 2 3 2 1 3 3 1))
                             (vlc (1 2 2 1 3 3 1 1 3 2 3))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))


;;; ============================================================================
;;;  OUTPUT
;;; ============================================================================

;;;  with defaults
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) - e e e e - - e e e e -))
               :pitch-seq-palette ((1 2 3 4 5 6 7 8))
               :marks (beg-sl 1 end-sl 2 slur 5 6 beg-phrase 1 end-phrase 8))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  specifying output file
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) - e e e e - - e e e e -))
               :pitch-seq-palette ((1 2 3 4 5 6 7 8))
               :marks (beg-sl 1 end-sl 2 slur 5 6 beg-phrase 1 end-phrase 8))))
         :rthm-seq-map '((1 ((vln (1))))))))
  (midi-play mini :midi-file "/tmp/midi-out.mid")
  (cmn-display mini :file "/tmp/cmn-out.eps")
  (write-lp-data-for-all mini :base-path "/tmp"))

;;; ============================================================================
;;;  MINI EXAMPLES - FEATURED ALGORITHMS 1
;;; ============================================================================

;;; ============================================================================
;;;  FIBONACCI-TRANSITION
;;; ============================================================================

;;;  one argument
;;; ----------------------------------------------------------------------------

(fibonacci-transition 50)

;;;  three arguments
;;; ----------------------------------------------------------------------------

(fibonacci-transition 50 's 'e)

;;; ============================================================================
;;;  FIBONACCI-TRANSITIONS
;;; ============================================================================

;;;  two integers
;;; ----------------------------------------------------------------------------

(fibonacci-transitions 76 4)

;;;  one integer one list
;;; ----------------------------------------------------------------------------

(fibonacci-transitions 304 '(s e q h))

;;; ============================================================================
;;;  REMIX-IN
;;; ============================================================================

;;;  simple usage
;;; ----------------------------------------------------------------------------

(remix-in '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))

;;;  "seed" argument
;;; ----------------------------------------------------------------------------

(remix-in '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23) 
          :remix-in-fib-seed 1)

;;; mirror argument
;;; ----------------------------------------------------------------------------

(remix-in '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23) 
          :mirror t)

;;; ============================================================================
;;;  CHOP
;;; ============================================================================

;;;  straightforward example, no re-bar
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((orig-palette (make-rsp 'orig ; original rthm-seq-palette
                               '((1 ((((1 4) - s e s - ))
                                     :pitch-seq-palette ((1 2 3))))
                                 (2 ((((1 4) - e. s - ))
                                     :pitch-seq-palette ((1 2))))
                                 (3 ((((1 4) - (e.) s - ))
                                     :pitch-seq-palette ((1)))))))
       (chopped-palette (chop orig-palette ; chopped rthm-seq-palette
                              '((1 4) 
                                (1 3) (2 4) 
                                (1 2) (2 3) (3 4) 
                                (1 1) (2 2) (3 3) (4 4)) ; chop points  
                              's)) ; chopping unit
       (chop-examp
        (make-slippery-chicken
         '+chop-examp+
         :ensemble '(((vn (violin :midi-channel 1))))
         :bars-per-system-map '((1 10))
         :set-palette '((1 ((c4 d4 e4))))
         :set-map '((1 (1 1 1 1 1)))
         :rthm-seq-palette chopped-palette
         :rthm-seq-map '((1 ((vn ((1 1) (1 2) (1 3) (2 1) (3 2)))))))))
  (midi-play chop-examp)
  (cmn-display chop-examp)
  (write-lp-data-for-all chop-examp))

;;;  chop with re-bar
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((orig-palette (make-rsp 'orig ; original rthm-seq-palette
                               '((1 ((((1 4) - s e s - ))
                                     :pitch-seq-palette ((1 2 3))))
                                 (2 ((((1 4) - e. s - ))
                                     :pitch-seq-palette ((1 2))))
                                 (3 ((((1 4) - (e.) s - ))
                                     :pitch-seq-palette ((1)))))))
       (chopped-palette (chop orig-palette ; chopped rthm-seq-palette
                              '((1 4) 
                                (1 3) (2 4) 
                                (1 2) (2 3) (3 4) 
                                (1 1) (2 2) (3 3) (4 4)) ; chop points  
                              's)) ; chopping unit
       (chop-examp
        (make-slippery-chicken
         '+chop-examp+
         :ensemble '(((vn (violin :midi-channel 1))))
         :bars-per-system-map '((1 10))
         :set-palette '((1 ((c4 d4 e4))))
         :set-map '((1 (1 1 1 1 1)))
         :rthm-seq-palette chopped-palette
         :rthm-seq-map '((1 ((vn ((1 1) (1 2) (1 3) (2 1) (3 2)))))))))
  (re-bar chop-examp :min-time-sig '(1 4))
  (midi-play chop-examp)
  (cmn-display chop-examp)
  (write-lp-data-for-all chop-examp))

;;; ============================================================================
;;;  MINI EXAMPLES - INTRAPHRASAL-LOOPING
;;; ============================================================================

;;; ============================================================================
;;;  ORIGINAL RTHM-SEQ-PALETTE
;;; ============================================================================

;;;  see example code for featured-algorithms 1

;;; ============================================================================
;;;  MULTIPLE CURVES (PITCH-SEQS) IN ONE PSP
;;; ============================================================================

;;;  straightforward example
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+multi-ps+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((8 7 8 7 8 7 8 7)
                                                     (5 4 3 4 5 4 3 4)
                                                     (1 2 1 2 1 2 1 2)))))
         :avoid-used-notes nil
         :rthm-seq-map '((1 ((fl (1))
                             (ob (1))
                             (cl (1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  CREATING CHOPPED RSP
;;; ============================================================================

;;;  see example code for featured-algorithms 1

;;; ============================================================================
;;;  CHANGING VALUES IN INSTRUMENT PALETTE USING LOOP AND SET-SLOT
;;; ============================================================================

;;;  before setting slots
;;; ----------------------------------------------------------------------------

(print
 (loop for i in
      '(flute oboe b-flat-clarinet bassoon french-horn b-flat-trumpet
        tenor-trombone double-bass)   
    collect
      (largest-fast-leap
       (get-data i +slippery-chicken-standard-instrument-palette+))))

;;;  setting slots
;;; ----------------------------------------------------------------------------

(loop for i in
     '((flute 11)
       (oboe 6)
       (b-flat-clarinet 8)
       (bassoon 6)
       (french-horn 4)
       (b-flat-trumpet 6)
       (tenor-trombone 4)
       (double-bass 4))
   do (set-slot 'largest-fast-leap
                (second i)
                (first i)
                +slippery-chicken-standard-instrument-palette+))

;;; after setting slots
;;; ----------------------------------------------------------------------------

(print
 (loop for i in
      '(flute oboe b-flat-clarinet bassoon french-horn b-flat-trumpet
        tenor-trombone double-bass)   
    collect
      (largest-fast-leap
       (get-data i +slippery-chicken-standard-instrument-palette+))))

;;; ============================================================================
;;;  CALL TO MAKE-SLIPPERY-CHICKEN
;;; ============================================================================

;;;  see core usage slides

;;; ============================================================================
;;;  WELL-CONSIDERED SET-PALETTES
;;; ============================================================================

;;;  all leaps = jagged lines
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln-one (violin :midi-channel 1))
                      (vln-two (violin :midi-channel 2))))
         :set-palette '((1 ((g3 d4 a4 e5 b5 fs6 cs7 gs7))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) - s s s s - - s s s s - - s s s s - - s s s s -))
               :pitch-seq-palette ((1 2 6 5 4 8 6 1 3 2 3 4 7 5 8 7)))))
         :rthm-seq-map '((1 ((vln-one (1))
                             (vln-two (1))))))))
  (midi-play mini :midi-file "/tmp/midi-out.mid")
  (cmn-display mini :file "/tmp/cmn-out.eps")
  (write-lp-data-for-all mini :base-path "/tmp"))

;;;  smaller intervals = smoother lines
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln-one (violin :midi-channel 1))
                      (vln-two (violin :midi-channel 2))))
         :set-palette '((1 ((g3 b3 cs4 e4 fs4 a4 b4 d5))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) - s s s s - - s s s s - - s s s s - - s s s s -))
               :pitch-seq-palette ((1 2 6 5 4 8 6 1 3 2 3 4 7 5 8 7)))))
         :rthm-seq-map '((1 ((vln-one (1))
                             (vln-two (1))))))))
  (midi-play mini :midi-file "/tmp/midi-out.mid")
  (cmn-display mini :file "/tmp/cmn-out.eps")
  (write-lp-data-for-all mini :base-path "/tmp"))

;;; ============================================================================
;;;  GENERATING SET-MAP ALGORITHMICALLY
;;; ============================================================================

;;;  using fibonacci-transitions and a loop within the call to make-sc
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-seqs-list '(11 13 11))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln-one (violin :midi-channel 1))
                      (vln-two (violin :midi-channel 2))))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6))) 
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6))) 
                        (4 ((e4 a4 b4 e5 a5 b5 e6))))
         :set-map (loop for section in
                       '((1 (1 2 3))
                         (2 (2 3 4 1))
                         (3 (4 3 1)))
                     collect
                       (list (first section)
                             (fibonacci-transitions
                              (nth (1- (first section)) num-seqs-list)
                              (second section))))
         :rthm-seq-palette 
         '((1 ((((4 4) - s s s s - - s s s s - - s s s s - - s s s s -))
               :pitch-seq-palette ((1 2 6 5 4 8 6 1 3 2 3 4 7 5 8 7)))))
         :rthm-seq-map '((1 ((vln-one (1 1 1 1 1 1 1 1 1 1 1))
                             (vln-two (1 1 1 1 1 1 1 1 1 1 1))))
                         (2 ((vln-one (1 1 1 1 1 1 1 1 1 1 1 1 1))
                             (vln-two (1 1 1 1 1 1 1 1 1 1 1 1 1))))
                         (3 ((vln-one (1 1 1 1 1 1 1 1 1 1 1))
                             (vln-two (1 1 1 1 1 1 1 1 1 1 1))))))))
  (midi-play mini :midi-file "/tmp/midi-out.mid")
  (cmn-display mini :file "/tmp/cmn-out.eps")
  (write-lp-data-for-all mini :base-path "/tmp"))

;;;  replaces:
;;; ----------------------------------------------------------------------------

(list
 (list 1 (fibonacci-transitions 11 '(1 2 3))) 
 (list 2 (fibonacci-transitions 13 '(2 3 4 1))) 
 (list 3 (fibonacci-transitions 11 '(4 3 1))))

;;; ============================================================================
;;;  USING CHOP
;;; ============================================================================

;;;  orig rsp, chopped rsp, print-simple
;;; ----------------------------------------------------------------------------

(let* ((rsp-orig 
        (make-rsp
         'sl-rsp
         '((1 
            ((((4 4) - (e.) s - - \+32 32 32 32 (e) - 
               - 32 32 (s) 32 32 32 (32) - - (s) s s (s) - )
              (- (e..) 32 - +q q (q))
              (h. (q)))
             :pitch-seq-palette ((1 2 1 2 3 2 3 3 1 2 5 5 7 6)))))))
       (rsp-chopped (chop rsp-orig 
                          '((1 2) (1 1) (2 2))
                          'e)))
  (print-simple rsp-chopped))

;;; ============================================================================
;;;  PUTTING THE CHOPPED MATERIAL INTO THE CALL TO MAKE-SC
;;; ============================================================================

;;;  - put variable in :rthm-seq-palette
;;;  - hand-coding rsm with refs in list-form into nested rsp of chop object
;;; ----------------------------------------------------------------------------

(let* ((num-seqs-list '(31 37 29))
       (rsp-orig 
        (make-rsp
         'sl-rsp
         '((1 
            ((((4 4) - (e.) s - - \+32 32 32 32 (e) - 
               - 32 32 (s) 32 32 32 (32) - - (s) s s (s) - )
              (- (e..) 32 - +q q (q))
              (h. (q)))
             :pitch-seq-palette ((1 2 6 5 4 8 6 1 3 2 3 4 7 5)))))))
       (rsp-chopped (chop rsp-orig 
                          '((1 2) (1 1) (2 2))
                          'e))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))
                      (bn (bassoon :midi-channel 4))))
         :set-palette '((1 ((fs2 b3 d4 a4 d5 e5 a5 d6))) 
                        (2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((cs3 fs3 e4 a4 e5 a5 e6))) 
                        (4 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e6))))
         :set-map (loop for section in
                       '((1 (1 2 3))
                         (2 (2 3 4 1))
                         (3 (4 3 1)))
                     collect
                       (list (first section)
                             (fibonacci-transitions
                              (nth (1- (first section)) num-seqs-list)
                              (second section))))
         :rthm-seq-palette rsp-chopped
         :rthm-seq-map 
         `((1 
            ((fl ,(fibonacci-transitions 31 '((1 1) (1 3) (1 4))))))
           (2 
            ((fl ,(fibonacci-transitions 37 '((1 15) (1 16) (1 25) (1 26))))
             (ob ,(fibonacci-transitions 37 '((1 15) (1 16) (1 25) (1 26))))
             (cl ,(fibonacci-transitions 37 '((1 9) (1 10) (1 13) (1 14))))
             (bn ,(fibonacci-transitions 37 '((1 12) (1 13) (1 10) (1 11))))))
           (3 
            ((fl ,(fibonacci-transitions 29 '((1 6) (1 7) (1 8))))
             (ob ,(fibonacci-transitions 29 '((1 6) (1 7) (1 8))))))))))
  (midi-play mini :midi-file "/tmp/midi-out.mid")
  (cmn-display mini :file "/tmp/cmn-out.eps")
  (write-lp-data-for-all mini :base-path "/tmp"))

;;; ============================================================================
;;;  ALGORITHMICALLY GENERATING RTHM-SEQ-MAP WITH CHOPPED RSP
;;; ============================================================================

;;;  - using loop and fibonacci-transitions in rsm
;;;  - NB: put variable in :rthm-seq-palette
;;; ----------------------------------------------------------------------------

(let* ((num-seqs-list '(31 37 29))
       (rsp-orig 
        (make-rsp
         'sl-rsp
         '((1 
            ((((4 4) - (e.) s - - \+32 32 32 32 (e) - 
               - 32 32 (s) 32 32 32 (32) - - (s) s s (s) - )
              (- (e..) 32 - +q q (q))
              (h. (q)))
             :pitch-seq-palette ((1 2 6 5 4 8 6 1 3 2 3 4 7 5)))))))
       (rsp-chopped (chop rsp-orig 
                          '((1 2) (1 1) (2 2))
                          'e))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))
                      (bn (bassoon :midi-channel 4))))
         :set-palette '((1 ((fs2 b3 d4 a4 d5 e5 a5 d6))) 
                        (2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((cs3 fs3 e4 a4 e5 a5 e6))) 
                        (4 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e6))))
         :set-map (loop for section in
                       '((1 (1 2 3))
                         (2 (2 3 4 1))
                         (3 (4 3 1)))
                     collect
                       (list (first section)
                             (fibonacci-transitions
                              (nth (1- (first section)) num-seqs-list)
                              (second section))))
         :rthm-seq-palette rsp-chopped
         :rthm-seq-map (loop for section in
                            '((((1 3 4) fl))
                              (((15 16 25 26) fl ob)
                               ((9 10 13 14) cl)
                               ((12 13 10 11) bn))
                              (((6 7 8) fl ob)))
                          for section-num from 1
                          collect 
                            (list section-num
                                  (loop for ins-group in section 
                                     appending
                                       (loop with fts =
                                            (loop for ch in (first ins-group) 
                                               collect
                                                 (list 1 ch))
                                          for ins in (rest ins-group) 
                                          collect
                                            (list ins
                                                  (fibonacci-transitions
                                                   (nth (1- section-num)
                                                        num-seqs-list) 
                                                   fts)))))))))
  (midi-play mini :midi-file "/tmp/midi-out.mid")
  (cmn-display mini :file "/tmp/cmn-out.eps")
  (write-lp-data-for-all mini :base-path "/tmp"))

;;; ============================================================================
;;;  RE-BAR
;;; ============================================================================

;;;  straightforward usage
;;; ----------------------------------------------------------------------------

(let* ((num-seqs-list '(31 37 29))
       (rsp-orig 
        (make-rsp
         'sl-rsp
         '((1 
            ((((4 4) - (e.) s - - \+32 32 32 32 (e) - 
               - 32 32 (s) 32 32 32 (32) - - (s) s s (s) - )
              (- (e..) 32 - +q q (q))
              (h. (q)))
             :pitch-seq-palette ((1 2 6 5 4 8 6 1 3 2 3 4 7 5)))))))
       (rsp-chopped (chop rsp-orig 
                          '((1 2) (1 1) (2 2))
                          'e))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))
                      (bn (bassoon :midi-channel 4))))
         :set-palette '((1 ((fs2 b3 d4 a4 d5 e5 a5 d6))) 
                        (2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((cs3 fs3 e4 a4 e5 a5 e6))) 
                        (4 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e6))))
         :set-map (loop for section in
                       '((1 (1 2 3))
                         (2 (2 3 4 1))
                         (3 (4 3 1)))
                     collect
                       (list (first section)
                             (fibonacci-transitions
                              (nth (1- (first section)) num-seqs-list)
                              (second section))))
         :rthm-seq-palette rsp-chopped
         :rthm-seq-map (loop for section in
                            '((((1 3 4) fl))
                              (((15 16 25 26) fl ob)
                               ((9 10 13 14) cl)
                               ((12 13 10 11) bn))
                              (((6 7 8) fl ob)))
                          for section-num from 1
                          collect 
                            (list section-num
                                  (loop for ins-group in section 
                                     appending
                                       (loop with fts =
                                            (loop for ch in (first ins-group) 
                                               collect
                                                 (list 1 ch))
                                          for ins in (rest ins-group) 
                                          collect
                                            (list ins
                                                  (fibonacci-transitions
                                                   (nth (1- section-num)
                                                        num-seqs-list) 
                                                   fts)))))))))
  (re-bar mini :min-time-sig '(4 4))
  (midi-play mini :midi-file "/tmp/midi-out.mid")
  (cmn-display mini :file "/tmp/cmn-out.eps")
  (write-lp-data-for-all mini :base-path "/tmp"))

;;; ============================================================================
;;;  MINI EXAMPLES - SC AND CLM
;;; ============================================================================

;;; ============================================================================
;;;  SETTING THE OUTPUT DIRECTORY
;;; ============================================================================

;;;  straightforward example
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1)))))
         :snd-output-dir "/tmp/")))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  DEFINING THE SNDFILE-PALETTE
;;; ============================================================================

;;;  straightforward example
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((developing-group
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (percussive-group
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfiles-dir-1 sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  SIMPLE CALL TO CLM-PLAY
;;; ============================================================================

;;;  straightforward example,  players = nil
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((developing-group
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (percussive-group
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfiles-dir-1 sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 nil 'percussive-group :check-overwrite nil
                  :src-width 5))

;;;  straightforward example,  specifying players
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((developing-group
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (percussive-group
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfiles-dir-1 sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 '(fl ob) 'percussive-group :check-overwrite nil
                  :src-width 5))

;;; ============================================================================
;;;  INC-START, DURATION-SCALER, IGNORE-RESTS
;;; ============================================================================

;;;  using all three arguments in same call, one player
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((developing-group
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (percussive-group
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfiles-dir-1 sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'developing-group :check-overwrite nil  :src-width 5
                  :inc-start t :duration-scaler 1.3 :ignore-rests nil))

;;; ============================================================================
;;;  :START, :END, :DURATION WITHIN SNDFILE-PALETTE
;;; ============================================================================

;;;  using all arguments in same call, one player
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((developing-group
                              ((test-sndfile-1.aiff :start 0.000 :end 2.100) 
                               (test-sndfile-1.aiff :start 0.000 
                                                    :duration 0.308) 
                               (test-sndfile-1.aiff :start (0 1 000))
                               (test-sndfile-1.aiff :end 1.308)
                               (test-sndfile-1.aiff :duration 1.736))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'developing-group :check-overwrite nil :src-width 5))

;;; ============================================================================
;;;  BASE FREQUENCY
;;; ============================================================================

;;;  not pitch-synchronous
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((percussive-group
                              ((test-sndfile-4.aiff :start 0.000 :end 2.100
                                                    :frequency 860)
                               (test-sndfile-4.aiff :end 1.1
                                                    :frequency a5))))
                            ,(list sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'percussive-group :check-overwrite nil :src-width 5))

;;;  pitch-synchronous
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((percussive-group
                              ((test-sndfile-4.aiff :start 0.000 :end 2.100
                                                    :frequency 860)
                               (test-sndfile-4.aiff :end 1.1
                                                    :frequency a5))))
                            ,(list sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'percussive-group :pitch-synchronous t
                  :check-overwrite nil :src-width 5))

;;; ============================================================================
;;;  ONE GROUP PER CALL TO CLM-PLAY
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)))
                             (group-2
                              ((test-sndfile-3.aiff)
                               (test-sndfile-4.aiff)))
                             (group-3
                              ((test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfiles-dir-1 sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 :check-overwrite nil :src-width 5)
  #+clm (clm-play mini 1 'fl 'group-2 :check-overwrite nil :src-width 5)
  #+clm (clm-play mini 1 'fl 'group-3 :check-overwrite nil :src-width 5))


;;; ==============================================
;;;  MORE ELABORATE EXAMPLE
;;; ==============================================

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (num-seqs 17)
       (mini
        (make-slippery-chicken
         '+mini+
         :title "mini"
         :instrument-palette +slippery-chicken-standard-instrument-palette+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))))
         :tempo-map '((1 (q 60)))
         :set-palette '((1 ((d3 c4 d4 e4 f5))))
         :set-map (list (list 1 (loop repeat num-seqs collect 1)))
         :rthm-seq-palette '((1 ((((4 4) h (q) - e (s) s -))
                                 :pitch-seq-palette ((1 2 3))))
                             (2 ((((4 4) s s s (s) (q) (e) 32 32 s s e.))
                                 :pitch-seq-palette ((1 6 5 1 2 6 5 3))))
                             (3 ((((4 4) e e +q h))
                                 :pitch-seq-palette ((5 5 5)))))
         :rthm-seq-map (list 
                        (list 1 
                              (loop for ins in '(fl ob cl) 
                                 collect 
                                   (list ins (loop repeat num-seqs
                                                collect (1+ (random 3))))))) 
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-1.aiff :start 0.021 :end 0.289 
                                                    :frequency 524)   
                               (test-sndfile-1.aiff :start 0.287 
                                                    :duration 0.536 
                                                    :frequency c3)  
                               (test-sndfile-1.aiff :start 0.831 :frequency c5)  
                               (test-sndfile-1.aiff :end 1.921 :frequency d4)  
                               (test-sndfile-1.aiff :duration 1.921 
                                                    :frequency e4)))) 
                            ,(list sndfiles-dir-1)))))
  (midi-play mini :midi-file "/tmp/mini.mid")
  (cmn-display mini :file "/tmp/mini.eps")
  (write-lp-data-for-all mini :base-path "/tmp/")
  #+clm (clm-play mini 1 nil 'source-sndfile-grp-1 
                  :check-overwrite nil
                  :src-width 5
                  :play nil
                  :rev-amt 0.05
                  :inc-start t
                  :duration-scaler 1.7
                  :reset-snds-each-rs nil))

;;; ============================================================================
;;;  FIBONACCI-TRANSITION BETWEEN TWO GROUPS
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (sndfiles-dir-2
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-2/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)))
                             (group-2
                              ((test-sndfile-3.aiff)
                               (test-sndfile-4.aiff))))
                            ,(list sndfiles-dir-1 sndfiles-dir-2)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 :sound-file-palette-ref2 'group-2
                  :check-overwrite nil :src-width 5))

;;; ============================================================================
;;;  ARBITRARY :AMPLITUDE VALUE IN SNDFILE OBJECT
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 :check-overwrite nil :src-width 5))

;;; ============================================================================
;;;  FROM-SEQUENCE AND NUM-SEQUENCES
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 :from-sequence 2 :num-sequences 2
                  :check-overwrite nil :src-width 5))

;;; ============================================================================
;;;  NUM-SECTIONS
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 2 'fl 'group-1 :num-sections 2 :check-overwrite nil
                  :src-width 5))

;;; ============================================================================
;;;  RESET-SNDS-EACH-RS AND -EACH-PLAYER
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 
                  :check-overwrite nil
                  :src-width 5
                  :reset-snds-each-rs nil
                  :reset-snds-each-player nil))

;;; ============================================================================
;;;  TIME-SCALER
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 :time-scaler 1.7 :check-overwrite nil
                  :src-width 5))


;;; ============================================================================
;;;  SRC-SCALER
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 :src-scaler 1.9 :check-overwrite nil
                  :src-width 5))

;;; ============================================================================
;;;  REV-AMT
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1 :rev-amt 0.1 :check-overwrite nil
                  :src-width 5))

;;; ============================================================================
;;;  CLM-PLAY OUTPUT ARGUMENTS
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (bn (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'fl 'group-1
                  :check-overwrite nil
                  :src-width 5
                  :channels 8
                  :srate 44100
                  :header-type clm::mus-riff
                  :data-format clm::mus-bshort 
                  :sndfile-extension ".aiff"))

;;; ============================================================================
;;;  INDEPENDENT COMPUTER PART
;;; ============================================================================

;;;  also setting set-limits-
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cp (computer))))
         :set-limits-high '((cp (0 c6 100 c6)))
         :set-limits-low '((cp (0 f3 100 f3)))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (cp (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (cp (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (cp (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  #+clm (clm-play mini 1 'cp 'group-1 :check-overwrite nil :src-width 5))

;;; ============================================================================
;;;  EXCLUDING COMPUTER PART FROM MIDI AND SCORE OUTPUT
;;; ============================================================================

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "tests/test-sndfiles-dir-1/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cp (computer))))
         :set-limits-high '((cp (0 c6 100 c6)))
         :set-limits-low '((cp (0 f3 100 f3)))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((1 1 3 5 4 6 3 5 5 7 9 6 5))))
           (2 ((((2 4) - e e - - e (s) s -)
                (- e s s - q)
                ((5 8)  - s s e - q e))
               :pitch-seq-palette ((1 1 3 2 3 3 4 2 3 2 2 4 6))))
           (3 ((((2 4) - e e - (s) - s e - )
                (- s s e - - +e e -)
                ((5 8)  - e e - - +e e - - s s -))
               :pitch-seq-palette ((4 1 1 1 1 3 5 7 9 10 11 10 7)))))
         :rthm-seq-map '((1 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (cp (1 2 2 1 3 3 1 1 3 2 3))))
                         (2 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (cp (1 2 2 1 3 3 1 1 3 2 3))))
                         (3 ((fl (3 2 1 1 3 3 3 2 1 1 2))
                             (ob (1 1 3 2 2 3 2 1 3 3 1))
                             (cp (1 2 2 1 3 3 1 1 3 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((group-1
                              ((test-sndfile-1.aiff :amplitude 11)
                               (test-sndfile-2.aiff :amplitude 1))))
                            ,(list sndfiles-dir-1)))))
  (midi-play mini :voices '(fl ob))
  (cmn-display mini :players '(fl ob))
  (write-lp-data-for-all mini :players '(fl ob))
  #+clm (clm-play mini 1 'cp 'group-1 :check-overwrite nil :src-width 5))

;;; ============================================================================
;;;  POST-GENERATION DATA EDITING MINI EXAMPLES
;;; ============================================================================

;;; ============================================================================
;;;  EVENTS VS NOTES
;;; ============================================================================

;;;  get-event
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (data (get-event mini 1 1 'vn)))

;;;  get-note
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (data (get-note mini 1 1 'vn)))

;;; ============================================================================
;;;  BASIC USAGE
;;; ============================================================================

;;;  add-mark-to-note
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (add-mark-to-note mini 2 4 'va "_whispered")
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  SPECIFIC EXAMPLES
;;; ============================================================================

;;;  change pitch
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (change-pitch mini 3 2 'va 'cs4)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  add-mark-to-note
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (add-mark-to-note mini 3 2 'va 'ppp)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  rm-marks-from-note
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6))
                                 :marks (ppp 2))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (rm-marks-from-note mini 3 2 'va 'ppp)
  (cmn-display mini)
  (write-lp-data-for-all mini))


;;;  trill
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) q (e) e h))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (trill mini 'vc 2 4 'e4 1 3)
  (write-lp-data-for-all mini))

;;;  tie-over-rests
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) q (e) e s (e) s q))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (tie-over-rests mini 2 3 'va :auto-beam t :consolidate-notes t)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  tie-over-all-rests
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

#|
curently creates bad-tie error
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) q (e) e s (e) s (q)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (tie-over-all-rests mini 'va 2 3 
                      :start-note 2 :auto-beam t :consolidate-notes t)
  (cmn-display mini)
  (write-lp-data-for-all mini))
|#

;;;  re-bar
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) q (e) e))
                                 :pitch-seq-palette ((1 2)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1))
                             (va (1 1 1 1))
                             (vc (1 1 1 1))))))))
  (re-bar mini :min-time-sig '(4 4))
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  move-events
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) q (e) e))
                                 :pitch-seq-palette ((1 2)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1))
                             (va (nil nil nil nil))
                             (vc (1 1 1 1))))))))
  (move-events mini 'vn 'va 2 1 3 1 :consolidate-rests t :transposition -3)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  double-events
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) q (e) e))
                                 :pitch-seq-palette ((1 2)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1))
                             (va (nil nil nil nil))
                             (vc (nil nil nil nil))))))))
  (double-events mini 'vn '(va vc) 2 1 3 1 
                 :consolidate-rests t :transposition -3)  
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  SCORE LAYOUT MINI CODE EXAMPLES
;;; ============================================================================

;;; ============================================================================
;;;  Header
;;; ============================================================================

;;;  :composer has no affect on CMN output
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A Slippery Chicken Piece"
         :composer "Joe Green"   
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((f4 g4 a4))))
         :set-map '((1 (1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  Score Order
;;; ============================================================================

;;;  using setf to change inst name, using set-limits- for sensible pitches
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A New Piece"
         :composer "Joe Green"   
         :ensemble '(((vn-solo (violin :midi-channel 1))
                      (fl (flute :midi-channel 2))
                      (ob (oboe :midi-channel 3))
                      (hn (french-horn :midi-channel 4))
                      (tp (b-flat-trumpet :midi-channel 5))
                      (vn (violin :midi-channel 6))
                      (va (viola :midi-channel 7))
                      (vc (cello :midi-channel 8))))
         :staff-groupings '(1 2 2 3)
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 
                                c5 d5 e5 f5 g5 a5 b5
                                c6 d6 e6 f6 g6 a6 b6 c7))))
         :set-limits-high '((vn-solo (0 c7 100 c7))
                            (fl (0 a5 100 a5))
                            (ob (0 d5 100 d5))
                            (hn (0 a4 100 a4))
                            (tp (0 g5 100 g5))
                            (vn (0 a5 100 a5))
                            (va (0 a4 100 a4))
                            (vc (0 a3 100 a3)))
         :set-limits-low '((vn-solo (0 b5 100 b5))
                           (fl (0 d5 100 d5) )
                           (ob (0 g4 100 g4))
                           (hn (0 c4 100 c4))
                           (tp (0 a4 100 a4))
                           (vn (0 e5 100 e5))
                           (va (0 d4 100 d4))
                           (vc (0 d3 100 d3)))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 4)))))
         :rthm-seq-map '((1 ((vn-solo (1 1 1 1))
                             (fl (1 1 1 1))
                             (ob (1 1 1 1))
                             (hn (1 1 1 1))
                             (tp (1 1 1 1))
                             (vn (1 1 1 1))
                             (va (1 1 1 1))
                             (vc (1 1 1 1))))))))
  (setf (staff-name (get-data-data 'vn-solo (ensemble mini))) "violin solo")
  (setf (staff-short-name (get-data-data 'vn-solo (ensemble mini))) "vn solo")
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  Score Groups
;;; ============================================================================

;;;  straightforward example, using set-limits- for sensible pitches
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A New Piece"
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
                                c5 d5 e5 f5 g5 a5 b5
                                c6 d6 e6 f6 g6 a6 b6 c7))))
         :set-limits-high '((fl (0 c6 100 c6))
                            (ob (0 b4 100 b4))
                            (hn (0 e4 100 e4))
                            (tp (0 d5 100 d5))
                            (vn (0 g5 100 g5))
                            (va (0 e3 100 e3))
                            (vc (0 e2 100 e3)))
         :set-limits-low '((fl (0 a5 100 a5) )
                           (ob (0 g4 100 g4))
                           (hn (0 c4 100 c4))
                           (tp (0 f4 100 f4))
                           (vn (0 e5 100 e5))
                           (va (0 c3 100 c3))
                           (vc (0 c2 100 c3)))
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
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  BARS PER SYSTEM
;;; ============================================================================

;;;  CMN only
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "A New Piece"
         :composer "Joe Green"   
         :ensemble '(((fl (flute :midi-channel 1))))
         :bars-per-system-map '((1 1) (2 2) (3 3) (7 4) (11 5))
         :set-palette '((1 ((a5 b5 c6))))
         :set-map (list (list 1 (loop repeat 15 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
         :rthm-seq-map `((1 ((fl ,(loop repeat 15 collect 1))))))))
  (midi-play mini)
  (cmn-display mini))

;;; ============================================================================
;;;  BAR LINE TYPES
;;; ============================================================================

;;;  straightforward example
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+bar-lines-piece+
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((f4 g4 a4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 1 1 1 1 1 1 1)))))
         :rthm-seq-map '((1 ((fl (1 1 1))))))))
  (change-bar-line-type mini 1 1)
  (change-bar-line-type mini 3 5)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini :base-path "/tmp/"))

;;; ============================================================================
;;;  REHEARSAL-LETTERS
;;; ============================================================================

;;;  :rehearsal-letters
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))))
         :rehearsal-letters '(3 6 10)
         :set-palette '((1 ((a5 b5 c6))))
         :set-map (list (list 1 (loop repeat 12 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
         :rthm-seq-map `((1 ((fl ,(loop repeat 12 collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  post-gen, using number instead
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
         :rthm-seq-map '((1 ((fl (1 1 1 1 1))))))))
  (set-rehearsal-letter mini 3 1)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  place in all players parts; 
;;;  NB: LilyPond still groups them in the score, though they're now in parts
;;;  too!  
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-seqs 12)
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :rehearsal-letters '(3 6 10)
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map (list (list 1 (loop repeat num-seqs collect 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
         :rthm-seq-map `((1 ((fl ,(loop repeat num-seqs collect 1))
                             (ob ,(loop repeat num-seqs collect 1))
                             (bn ,(loop repeat num-seqs collect 1))))))))
  (midi-play mini)
  (cmn-display mini :rehearsal-letters-all-players t)
  (write-lp-data-for-all mini :rehearsal-letters-all-players t))

;;; ============================================================================
;;;  ADD/DELETE CLEFS
;;; ============================================================================

;;;  add-clefs
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vc (cello :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((vc (1 1 1)))))))) 
  (add-clef mini 'vc 2 2 'tenor)
  (add-clef mini 'vc 3 3 'treble)
  (cmn-display mini :auto-clefs nil)
  (write-lp-data-for-all mini :auto-clefs nil))

;;;  delete-clefs
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vc (cello :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((vc (1 1 1)))))))) 
  (add-clef mini 'vc 2 2 'tenor)
  (add-clef mini 'vc 3 3 'treble)
  (delete-clefs mini 'vc 2 2)
  (delete-clefs mini 'vc 3 3)
  (cmn-display mini :auto-clefs nil)
  (write-lp-data-for-all mini :auto-clefs nil))

;;; ============================================================================
;;;  NON-C SCORES
;;; ============================================================================

;;;  straightforward example
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (cl (b-flat-clarinet :midi-channel 2))))
         :set-limits-high '((fl (0 g6 100 g6))
                            (cl (0 g5 100 g5)))
         :set-limits-low '((fl (0 a5 100 a5))
                           (cl (0 b4 100 b4)))
         :set-palette '((1 ((b4 c5 d5 e5 f5 g5 a5 b5 c6 d6 e6 f6 g6))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((fl (1 1 1 1))
                             (cl (1 1 1 1)))))))) 
  (cmn-display mini :in-c nil)
  (write-lp-data-for-all mini :in-c nil))

;;; ============================================================================
;;;  PARTS
;;; ============================================================================

;;;  straightforward example
;;; ----------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (cl (b-flat-clarinet :midi-channel 2))))
         :set-limits-high '((fl (0 g6 100 g6))
                            (cl (0 g5 100 g5)))
         :set-limits-low '((fl (0 a5 100 a5))
                           (cl (0 b4 100 b4)))
         :set-palette '((1 ((b4 c5 d5 e5 f5 g5 a5 b5 c6 d6 e6 f6 g6))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((fl (1 1 1 1))
                             (cl (1 1 1 1)))))))) 
  (cmn-display mini :players '(cl) :in-c nil)
  (write-lp-data-for-all mini :players '(cl) :in-c nil))

;;; ===========================================================================
;;;  FEATURED ALGORITHMS 2 MINI CODE EXAMPLES
;;; ===========================================================================

;;; ===========================================================================
;;;  PERMUTATIONS
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(permutations 4)

;;;  using the function to generate set-map and rthm-seq-map
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (permutations 4))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map `((1 ,(flatten (loop for p in perms
                                    collect p))))
         :rthm-seq-palette '((0 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (1 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (2 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (3 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map `((1 ((vn 
                              ,(flatten (loop for p in perms
                                           collect p))))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  inefficient-permutations
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(inefficient-permutations 4)
(inefficient-permutations 4 :max 7)
(inefficient-permutations 4 :max 7 :skip 2)
(loop repeat 5 collect (inefficient-permutations 4 :max 4 :fix nil))
(loop repeat 5 collect (inefficient-permutations 4 :max 4 :fix t))

;;;  using the function to generate set-map and rthm-seq-map
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (inefficient-permutations 4 :max 11))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map `((1 ,(flatten perms)))
         :rthm-seq-palette '((0 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (1 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (2 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (3 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map `((1 ((vn ,(flatten perms))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  permutate
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(permutate '(a b c d))

;;;  using the function to generate rthm-seq-palette
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (permutate '(e (e) s (e.))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 (loop repeat (length perms) collect 1)))
         :rthm-seq-palette (loop for p in perms
                              for rs-id from 0
                              collect `(,rs-id
                                        ((((2 4) ,@(nth rs-id perms)))
                                         :pitch-seq-palette ((1 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn 
                                     (loop for rs from 0 below (length perms)
                                        collect rs)))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  inefficiently-permutate
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(inefficiently-permutate '(a b c d))
(inefficiently-permutate '(a b c d) :sublists t)
(inefficiently-permutate '(a b c d) :max 7)
(inefficiently-permutate '(a b c d) :max 7 :skip 2)
(loop repeat 5 collect (inefficiently-permutate '(a b c d) :max 5 :fix nil))
(loop repeat 5 collect (inefficiently-permutate '(a b c d) :max 5 :fix t))

;;;  using the function to generate rthm-seq-palette
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (inefficiently-permutate '(e (e) s (e.) q s s (e)) 
                                       :max 91 
                                       :sublists t))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((fs2 b2 d4 a4 d5 e5 a5 d6)))
                        (1 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((cs3 fs3 e4 a4 e5 a5 e6)))
                        (3 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e5)))
                        (4 ((d2 a2 e4 fs4 gs4 b4 e5 b5)))
                        (5 ((a2 e3 e4 fs4 gs4 b4 cs5 e5 b5)))
                        (6 ((cs3 fs3 fs4 gs4 a4 cs5 a5 cs6)))
                        (7 ((fs2 cs3 fs4 gs4 a4 b4 cs5 fs5)))
                        (8 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6)))
                        (9 ((d2 a2 fs4 gs4 a4 e5 a5 e6)))
                        (10 ((a2 d2 e4 fs4 a4 e5 a5))))
         :set-limits-high '((ob (0 a5 100 a5))
                            (bn (0 g3 100 g3)))
         :set-limits-low '((fl (0 d5 100 d5)))
         :set-map (list (list 1 (loop for sn from 0 below (length perms) 
                                   collect (mod sn 11)))) ; mod 11 = 11 sets 
         :rthm-seq-palette (loop for p in perms
                              for rs-id from 0
                              collect `(,rs-id
                                        ((((4 4) ,@(nth rs-id perms)))
                                         :pitch-seq-palette ((1 3 2 5 6)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'fl 
                                     (loop for rs from 0 below (length perms)
                                        collect rs))
                               (list 'ob 
                                     (loop for rs from 0 below (length perms)
                                        collect 
                                          (mod (1+ rs) (length perms))))
                               (list 'bn
                                     (loop for rs from 0 below (length perms)
                                        collect 
                                          (mod (+ rs 2) (length perms))))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  shuffle
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(shuffle '(1 2 3 4 5 6 7))
(shuffle '(1 2 3 4 5 6 7) :start 1 :end 5)
(let* ((l '(1 2 3 4 5 6 7)))
  (shuffle l :copy t)
  (print l)
  (shuffle l :copy nil)
  (print l))
(loop repeat 5 collect (shuffle '(1 2 3 4 5 6 7) :fix t))
(loop repeat 5 collect (shuffle '(1 2 3 4 5 6 7) :fix nil))
(loop repeat 3
   collect
     (loop for i below 3 
        collect (shuffle '(1 2 3 4 5 6 7) :fix t :reset (zerop i))))

;;;  using the function to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (flatten 
         (loop repeat 11 
            collect 
              (shuffle '(1 2 3 4) :fix nil))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  multi-shuffle
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(multi-shuffle '(1 2 3 4 5 6 7) 11)
(multi-shuffle '(1 2 3 4 5 6 7) 11 :start 1 :end 5)
(let* ((l '(1 2 3 4 5 6 7)))
  (multi-shuffle l 11 :copy t)
  (print l)
  (multi-shuffle l 11 :copy nil)
  (print l))
(loop repeat 5 collect (multi-shuffle '(1 2 3 4 5 6 7) 11 :fix t))
(loop repeat 5 collect (multi-shuffle '(1 2 3 4 5 6 7) 11 :fix nil))
(loop repeat 3
   collect
     (loop for i below 3 
        collect (multi-shuffle '(1 2 3 4 5 6 7) 11 :fix t :reset (zerop i))))


;;;  using the function to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (flatten 
         (loop repeat 11 
            collect 
              (multi-shuffle '(1 2 3 4) 11 :fix nil))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  multi-shuffle-with-perms
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(multi-shuffle-with-perms '(0 1 2 3 4) 7)

;;;  using the function to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (flatten 
         (loop for i from 1 to 11 
            collect 
              (multi-shuffle-with-perms '(1 2 3 4) 11))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  move-repeats
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(move-repeats '(1 2 3 3 4 5 6 7 8 8 9 10))
(move-repeats '((a b c) (c a b) (c a b) (d e f) (a b c) (g h i)))

;;;  using the function together with inefficiently-permutate
;;;  to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (move-repeats (inefficiently-permutate '(1 2 3 4) :max 11)))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  random-rep
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(loop repeat 10 collect (random-rep 5))
(loop repeat 10 collect (random-rep 5 t))

;;;  using the function together with inefficiently-permutate
;;;  to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-seqs 91)
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 (loop repeat num-seqs collect (random-rep 4))))
         :rthm-seq-palette '((0 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (1 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (2 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (3 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn (loop repeat num-seqs 
                                            collect (random-rep 4))))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  L-SYSTEMS
;;; ===========================================================================

;;; ===========================================================================
;;;  make-l-for-lookup
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(make-l-for-lookup 'l-sys
                   '((1 ((a)))
                     (2 ((b))))
                   '((1 (1 2)) (2 (1))))

;;; ===========================================================================
;;;  do-simple-lookup
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'l-sys
                               '((1 ((a)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1))))))
  (flatten (do-simple-lookup lfl 1 29)))

;;;  simple piece using do-simple-lookup to make set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-bars 37)
       (lfl (make-l-for-lookup 'l-sys
                               '((1 ((a)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1)))))
       (l-sys-list (flatten (do-simple-lookup lfl 1 num-bars)))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((a ((e4 a4 b4 e5 a5 b5 e5)))
                        (b ((e4 fs4 gs4 b4 cs5 e5 b5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 l-sys-list))
         :rthm-seq-palette '((a ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (b ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn l-sys-list)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  get-l-sequence
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'l-sys
                               nil
                               '((1 (1 2)) (2 (1))))))
  (get-l-sequence lfl 1 29))

;;;  simple piece using get-l-sequence to make set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-bars 37)
       (lfl (make-l-for-lookup 'l-sys
                               nil
                               '((1 (1 2)) (2 (1)))))
       (l-sys-list (get-l-sequence lfl 1 num-bars))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((e4 a4 b4 e5 a5 b5 e5)))
                        (2 ((e4 fs4 gs4 b4 cs5 e5 b5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 l-sys-list))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn l-sys-list)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  do-lookup
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

;;; do-lookup with single-item data returns simple l-sequence
(let* ((lfl (make-l-for-lookup 'l-sys-a
                               '((1 ((a)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1))))))
  (print (do-lookup lfl 1 73)))

;;; do-lookup with two-item sublists as data for at least one of the items
;;; makes fib-based trans evident.
;;; returns l-sequence, but the end: the (a) is fully replaced by the (c).
(let* ((lfl (make-l-for-lookup 'l-sys-a
                               '((1 ((a) (c)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1))))))
  (print  (do-lookup lfl 1 73)))

;;;  simple piece using do-lookup to make set-map and rsm
;;;  demonstrates transitions, gradually replacing a with c
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-bars 73)
       (lfl (make-l-for-lookup 'l-sys
                               '((1 ((a) (c)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1)))))
       (l-sys-list (do-lookup lfl 1 num-bars))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((a ((e4 a4 b4 e5 a5 b5 e5)))
                        (b ((e4 fs4 gs4 b4 cs5 e5 b5)))
                        (c ((fs4 gs4 a4 e5 a5 e6))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 l-sys-list))
         :rthm-seq-palette '((a ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (b ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (c ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn l-sys-list)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  get-linear-sequence
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'lfl-test
                               nil
                               '((1 (2 3))
                                 (2 (3 1 2))
                                 (3 (1))))))
  (print (get-linear-sequence lfl 1 23)))

;;;  simple piece using get-linear-sequence to make set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-bars 37)
       (lfl (make-l-for-lookup 'l-sys
                               nil
                               '((1 (2 3))
                                 (2 (3 1 2))
                                 (3 (1)))))
       (l-sys-list (get-linear-sequence lfl 1 num-bars))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((e4 a4 b4 e5 a5 b5 e5)))
                        (2 ((e4 fs4 gs4 b4 cs5 e5 b5)))
                        (3 ((fs4 gs4 a4 e5 a5 e6))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 l-sys-list))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn l-sys-list)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  RTHM-CHAINS
;;; ===========================================================================

;;; ===========================================================================
;;;  PROCESSION
;;; ==========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(procession 500 '(a b c d e f g h i j k l m n o p q r s t u v w x y z) 
            :peak 0.9 :expt 0.7 
            :orders '((1 3 2 2 3 1) (3 1 1 2 3 3) (1 2 1 3 2 2)))

;;;  used to generate the set map
;;;  an amalgamation of reich's 18-musicians chords and piano phase
;;; ---------------------------------------------------------------------------
(in-scale :chromatic)

(set-slot 'largest-fast-leap 999 'piano 
          +slippery-chicken-standard-instrument-palette+)

(let* 
    ((num-bars 113)
     (mini
      (make-slippery-chicken
       '+mini+
       :ensemble '(((pn-one (piano :midi-channel 1))
                    (pn-two (piano :midi-channel 2))))
       :tempo-map '((1 (q. 72)))
       :set-palette '((1 ((fs2 b2 d4 a4 d5 e5 a5 d6 a7)))
                      (2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                      (3 ((cs3 fs3 e4 a4 e5 a5 e6)))
                      (4 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e5)))
                      (5 ((d2 a2 e4 fs4 gs4 b4 e5 b5)))
                      (6 ((a2 e3 e4 fs4 gs4 b4 cs5 e5 b5)))
                      (7 ((cs3 fs3 fs4 gs4 a4 cs5 a5 cs6)))
                      (8 ((fs2 cs3 fs4 gs4 a4 b4 cs5 fs5)))
                      (9 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6)))
                      (10 ((d2 a2 fs4 gs4 a4 e5 a5 e6)))
                      (11 ((a2 d2 e4 fs4 a4 e5 a5))))
       :set-map (print (list (list 1 (procession num-bars 11))))
       :rthm-seq-palette '((1 ((((6 8) - s s s s s s - - s s s s s s -))  
                               :pitch-seq-palette ((1 2 3 4 5 10 1 4 3 2 5 4))))
                           (2 ((((6 8) - s s s s s s - - s s s s s s -))  
                               :pitch-seq-palette ((2 3 4 5 2 1 4 3 2 5 4 1)
                                                   (3 4 5 2 1 4 3 2 5 4 1 2)
                                                   (4 5 2 1 4 3 2 5 4 1 2 3)
                                                   (5 2 1 4 3 2 5 4 1 2 3 4)
                                                   (2 1 4 3 2 5 4 1 2 3 4 5)
                                                   (1 4 3 2 5 4 1 2 3 4 5 2)
                                                   (4 3 2 5 4 1 2 3 4 5 2 1)
                                                   (3 2 5 4 1 2 3 4 5 2 1 4)
                                                   (2 5 4 1 2 3 4 5 2 1 4 3)
                                                   (5 4 1 2 3 4 5 2 1 4 3 2)
                                                   (4 1 2 3 4 5 2 1 4 3 2 5)))))
       :rthm-seq-map (list 
                      (list 1 
                            (list 
                             (list 'pn-one
                                   (loop repeat num-bars collect 1))
                             (list 'pn-two
                                   (loop repeat num-bars collect 2))))))))
  (midi-play mini)
  ;; (cmn-display mini :display-sets t :write-section-info t)
  (cmn-display mini :display-sets t :write-section-info nil)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  MAKE-RTHM-CHAIN
;;; ===========================================================================

;;;  both the function and a piece
;;; ---------------------------------------------------------------------------

(let* ((rch
        (make-rthm-chain
         'test-rch 143
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s))
           (({ 3 (te) te (te) }) ; what we transition to
            ({ 3 - te (te) te - })
            ({ 3 (te) - te te - })
            ({ 3 (te) (te) te })))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.))
            (({ 3 te+te te+te te+te }) ; what we transition to
             (q - s e. -)
             (q (s) e.)
             (q (s) - s e -)))
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
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (cl (b-flat-clarinet :midi-channel 2))))
           :set-palette '((1 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :tempo-map '((1 (q 120)))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;; ===========================================================================
;;;  make-rthm-chain with fib trans instead of procession
;;; ===========================================================================

;;;  both the method and a piece
;;; ---------------------------------------------------------------------------

(let* ((rch
        (make-rthm-chain
         'test-rch 143
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s))
           (({ 3 (te) te (te) }) ; what we transition to
            ({ 3 - te (te) te - })
            ({ 3 (te) - te te - })
            ({ 3 (te) (te) te })))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.))
            (({ 3 te+te te+te te+te }) ; what we transition to
             (q - s e. -)
             (q (s) e.)
             (q (s) - s e -)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))
            (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
             (- e. s - (q) (s) - s e -)
             ({ 3 te+te te } (q) q)
             ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
         :players '(fl cl)
         :1-beat-fibonacci t
         :slow-fibonacci t)))
  (create-psps (palette rch))
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (cl (b-flat-clarinet :midi-channel 2))))
           :set-limits-high '((fl (0 a5 50 c7 100 a5))
                              (cl (0 d5 50 a5 100 d5)))
           :set-limits-low '((fl (0 a4 50 a5 100 a4))
                             (cl (0 e3 50 d5 100 c4)))
           :tempo-map '((1 (q 72)))
           :set-palette '((1 ((cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    (re-bar mini :min-time-sig '(4 4) :auto-beam nil) ; we've already beamed
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;; ===========================================================================
;;;  rhtm-chain together with post-gen editing
;;; ===========================================================================

;;;  Same code as first make-rthm-chain above, but with a loop for adding ties
;;;  over rests, then using handle-ties, map-over-bars with consolidate-rests
;;;  ---------------------------------------------------------------------------

(let* ((rch
        (make-rthm-chain
         'test-rch 137
         '((((e) e) ; 4 in total
            (s (s) (s) s)
            ({ 3 (te) - te te - })
            ((e.) s))
           (({ 3 (te) te (te) }) ; what we transition to
            ({ 3 te (te) te })
            ({ 3 (te) - te te - })
            ({ 3 (te) (te) te })))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.))
            (({ 3 te+te te+te te+te }) ; what we transition to
             (q - s e. -)
             (q (s) e.)
             (q (s) - s e -)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))
            (({ 3 (tq) tq tq } (q)) ; what we transition to
             (- e. s - (q) (s) - s e -)
             ({ 3 te+te te } (q) q)
             ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
         :players '(fl cl))))
  (create-psps (palette rch))
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (cl (b-flat-clarinet :midi-channel 2))))
           :set-limits-high '((fl (0 a5 50 c7 100 a5))
                              (cl (0 d5 50 a5 100 d5)))
           :set-limits-low '((fl (0 a4 50 a5 100 a4))
                             (cl (0 e3 50 d5 100 c4)))
           :tempo-map '((1 (q 72)))
           :set-palette '((1 ((cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    ;; Loop through the list of bar-nums paired with lists of note-nums and
    ;; tie-over-rests for each. Must be backwards otherwise ties into next bars
    ;; will change the number of notes in that bar. consolidate-notes must be
    ;; set to NIL for the same reason.
    (loop for p in '(fl cl)
       for ator in '(((31 (1)) (29 (5 3)) (27 (2)) (25 (2)) (24 (1)) (18 (1))
                      (16 (1)) (15 (3)) (12 (2)) (10 (2)) (7 (1)) (6 (2))
                      (5 (1)) (3 (1)) (2 (2)))
                     ((30 (3)) (25 (1)) (24 (1)) (22 (5)) (20 (1)) (17 (3))
                      (13 (1)) (9 (1)) (7 (5)) (4 (2)) (2 (1)) (1 (2))))
       do (loop for b in ator
             do (loop for n in (second b)
                   do (tie-over-rests mini (first b) n p
                                      :consolidate-notes nil))))
    ;; Reflect new ties in MIDI output
    (handle-ties mini)
    ;; Might as well go for -max right away
    (map-over-bars mini nil nil nil #'consolidate-rests-max)
    (midi-play mini)
    (write-xml mini)
    (cmn-display mini)))

;;; ===========================================================================
;;;  add-voice
;;; ===========================================================================

;;;  both the method and a piece
;;;  NB: There may be an issue with the offset at the moment, as the new voice
;;;  is very often very much in rhythmic unison with the source voice.
;;; ---------------------------------------------------------------------------

(let* ((rch
        (make-rthm-chain
         'test-rch 173
         '((((e) e) ; 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s))
           (({ 3 (te) te (te) }) ; what we transition to
            ({ 3 - te (te) te - })
            ({ 3 (te) - te te - })
            ({ 3 (te) (te) te })))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.))
            (({ 3 te+te te+te te+te }) ; what we transition to
             (q - s e. -)
             (q (s) e.)
             (q (s) - s e -)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))
            (({ 3 (te) (te) te+te te+te } (q)) ; what we transition to
             (- e. s - (q) (s) - s e -)
             ({ 3 te+te te } (q) q)
             ({ 3 - te te te - } (e) e { 3 (te) (te) te }))))
         :players '(fl cl))))
  (add-voice rch '(1 fl) 'ob)
  (create-psps (palette rch))
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))
                        (cl (b-flat-clarinet :midi-channel 3))))
           :set-limits-high '((fl (0 a5 50 c7 100 a5))
                              (cl (0 d5 50 a5 100 d5)))
           :set-limits-low '((fl (0 a4 50 a5 100 a4))
                             (cl (0 e3 50 d5 100 c4)))
           :tempo-map '((1 (q 72)))
           :set-palette '((1 ((cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6))))
           :set-map `((1 ,(ml 1 (num-rthm-seqs rch))))
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    (midi-play mini)
    (cmn-display mini)
    (write-lp-data-for-all mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF mini-examples-all.lsp
