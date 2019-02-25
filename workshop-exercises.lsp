;;; ============================================================================
;;;  EXERCISES TO ACCOMPANY THE SLIPPERY CHICKEN WORKSHOPS
;;; ============================================================================

;;; ============================================================================
;;;  CORE USAGE
;;; ============================================================================

;;;  Using the "mini" template below, modify the following manually (no loops
;;;  or algorithms) to generate your own short 1-section piece:
;;;  - :title
;;;  - :ensemble
;;;  - :staff-groupings
;;;  - :tempo-map
;;;  - :set-limits-high
;;;  - :set-limits-low
;;;  - :set-palette (perhaps changing the number of sets)
;;;  - :set-map (changing the length of the map)
;;;  - :rthm-seq-palette (perhaps changing the number of rthm-seqs)
;;;  - :rthm-seq-map (changing the length of the map)
;;;  - output file names


(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "my mini template"
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-limits-high '((vn (0 c7 100 c7))
                            (va (0 d5 100 d5))
                            (vc (0 a4 100 a4)))
         :set-limits-low '((vn (0 a4 100 a4))
                           (va (0 d4 100 d4))
                           (vc (0 g2 100 g2)))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 bf4 c5)))
                        (2 ((cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
         :set-map '((1 (1 2 1 2 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e (s) - e e -))
                                 :pitch-seq-palette ((1 2 3))))
                             (2 ((((2 4) e (s) s - +s e - (s)))
                                 :pitch-seq-palette ((2 3 1))))
                             (3 ((((2 4) - e e - e (e)))
                                 :pitch-seq-palette ((3 1 2)))))
         :rthm-seq-map '((1 ((vn (1 2 3 2 1))
                             (va (2 3 2 1 2))
                             (vc (3 2 1 2 3))))))))
  (midi-play mini)
  (cmn-display mini :display-sets t :write-section-info t :empty-staves t)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  FIBONACCI TRANSITIONS AND CHOP
;;; ============================================================================

;;; Using the template below and referencing the examples before it
;;; a) Generate the set-map using fibonacci-transitions or remix-in.
;;; b) 1) Assign your own rthm-seq-palette to one variable and a chopped
;;;       version of that palette to another. 
;;;    2) Replace the rthm-seq-palette with the variable assigned to the
;;;       chopped palette.
;;;    3) Use the print-simple method to print out the bars contained in chop.
;;;    4) Use two-item references, consisting of the rthm-seq-palette number
;;;       paired with the rthm-seq number, as the contents of the rthm-seq-map. 
;;;    5) Apply re-bar and generate output.
;;; c) Stay with only one player for now.
;;;    -------------------------------------------------------------------------

;;; print-simple example
;;; ----------------------------------------------------------------------------

(let* ((orig-palette (make-rsp 'orig ; original rthm-seq-palette
                               '((1 ((((1 4) - s e s - ))
                                     :pitch-seq-palette ((1 2 3))))
                                 (2 ((((1 4) - e. s - ))
                                     :pitch-seq-palette ((1 2))))
                                 (3 ((((1 4) - (e.) s - ))
                                     :pitch-seq-palette ((1)))))))
       (chopped-palette (chop orig-palette ; chopped rthm-seq-palette
                              '((1 4)                    ; \
                                (1 3) (2 4)              ; - chop points  
                                (1 2) (2 3) (3 4)        ; /|
                                (1 1) (2 2) (3 3) (4 4)) ; /
                              's)))
  (print-simple chopped-palette))

;;; chopped example 
;;; ----------------------------------------------------------------------------

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


;;; template
;;; ----------------------------------------------------------------------------

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "mini template"
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5)))
                        (2 ((cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
         :set-map '((1 (1 2 1 2 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e (s) - e e -))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  SC AND CLM
;;; ============================================================================

;;;  Using the template below and soundfiles available from
;;;  michael-edwards.org/sc/media/test-sndfiles.zip 

;;;  1) Modify the set-palette and -map, rthm-seq-palette and -map to your
;;;     liking. 
;;;  2) Modify the sndfile-palette to reflect the location of the given
;;;     or your own soundfiles.
;;;  3) Generate a series of sound files with calls to clm-play, using
;;;     different parameters and keywords.
;;;     a) Based on one voice only, with one group
;;;     b) Based on all voices
;;;     c) Using various degrees of reverb
;;;     d) By setting start- and end-times within the sndfile-palette
;;;     e) Once pitch-synchronous, once not
;;;     f) Modifying base-frequency settings
;;;     g) Choosing a second group for fibonacci transitioning
;;;     h) Resetting the sounds each seq/player
;;;     i) using the :time-scaler argument
;;;
;;;  See the manual webpage and robodoc for arguments
;;; ----------------------------------------------------------------------------

#+clm
(let* ((mini
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
         :sndfile-palette 
         '(((developing-group
             ((test-sndfile-1.aiff)
              (test-sndfile-2.aiff)
              (test-sndfile-3.aiff)))
            (percussive-group
             ((test-sndfile-4.aiff)
              (test-sndfile-5.aiff)
              (test-sndfile-6.aiff))))
           ("/Users/michael/sc/test-suite/test-sndfiles-dir-1/"
            "/Users/michael/sc/test-suite/test-sndfiles-dir-2/")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play mini 1 nil 'percussive-group :check-overwrite nil))

;;; ============================================================================
;;;  POST-GENERATION DATA EDITING
;;; ============================================================================

;;;  Using the template below, add dynamics and other marks, trills, and ties,
;;;  change individual pitches, and move/copy events using the following
;;;  post-generation methods: 
;;;  1) add-mark-to-note
;;;  2) trill
;;;  3) tie-over-rests
;;;  4) tie-over-all-rests
;;;  5) change-pitch
;;;  6) move-events
;;;  7) double-events
;;; ----------------------------------------------------------------------------

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "mini template"
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-limits-high '((vn (0 c7 100 c7))
                            (va (0 d5 100 d5))
                            (vc (0 a4 100 a4)))
         :set-limits-low '((vn (0 a4 100 a4))
                           (va (0 d4 100 d4))
                           (vc (0 g2 100 g2)))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5)))
                        (2 ((cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
         :set-map '((1 (1 2 1 2 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e (s) - e e -))
                                 :pitch-seq-palette ((1 2 3))))
                             (2 ((((2 4) e (s) s - +s e - (s)))
                                 :pitch-seq-palette ((2 3 1))))
                             (3 ((((2 4) - e e - e (e)))
                                 :pitch-seq-palette ((3 1 2)))))
         :rthm-seq-map '((1 ((vn (1 2 3 2 1))
                             (va (2 3 2 1 2))
                             (vc (3 2 1 2 3))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  SCORE LAYOUT
;;; ============================================================================

;;; Using the following template, add code to change the header, alter score
;;; order, change staff groupings, modify bars-per-system in CMN, change
;;; bar-line types, add rehearsal letters, change clefs, and extract parts.
;;; ----------------------------------------------------------------------------

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-limits-high '((vn (0 c7 100 c7))
                            (va (0 d5 100 d5))
                            (vc (0 a4 100 a4)))
         :set-limits-low '((vn (0 a4 100 a4))
                           (va (0 d4 100 d4))
                           (vc (0 g2 100 g2)))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5)))
                        (2 ((cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
         :set-map '((1 (1 2 1 2 1 2 1 2 1 2 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e (s) - e e -))
                                 :pitch-seq-palette ((1 2 3))))
                             (2 ((((2 4) e (s) s - +s e - (s)))
                                 :pitch-seq-palette ((2 3 1))))
                             (3 ((((2 4) - e e - e (e)))
                                 :pitch-seq-palette ((3 1 2)))))
         :rthm-seq-map '((1 ((vn (1 2 3 2 1 2 3 1 2 3 1))
                             (va (2 3 2 1 2 3 1 2 3 1 2))
                             (vc (3 2 1 2 3 1 2 3 1 2 3))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ============================================================================
;;;  PERMUTATIONS
;;; ============================================================================

;;;  Based on the following example, use a number of the permutations functions
;;;  to generate the set-map and rthm-seq-maps.
;;;  1) permutations
;;;  2) inefficient-permutations
;;;  3) permutate
;;;  4) inefficiently-permutate
;;;  5) shuffle
;;; ----------------------------------------------------------------------------

(let* ((sp (make-set-palette 'blah
                             '((1 ((d4 a4 d5 e5 a5 d6)))
                               (blah ((ds4 e4 a4 d5 e5 as5 d6)))
                               (3 ((e4 af4 e5 a5 e6)))
                               (4 ((e4 a4 b4 e5 a5 b5 e5))))))
       (sp-refs (get-all-refs sp))
       (sp-perms (flatten (permutate sp-refs)))
       (perms (permutations 4))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette sp
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map `((1 ,sp-perms))
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

;;; ============================================================================
;;;  L-SYSTEMS
;;; ============================================================================

;;;  Based on the following example, use make-l-for-lookup to make your own
;;;  l-for-lookup object and do-simple-lookup to generate the set-map and
;;;  rthm-seq-map for a slippery-chicken piece.
;;;  ---------------------------------------------------------------------------

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

;;; ============================================================================
;;;  RHYTHM CHAINS
;;; ============================================================================

;;;  Based on the following example, make your own rthm-chain object and use it
;;;  as the source material for manually determined rthm-seq map. Apply re-bar
;;;  before generating output.
;;;  ---------------------------------------------------------------------------

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
    (cmn-display mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF workshop-exercises.lsp
