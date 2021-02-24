;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             tramontana.lsp
;;;
;;; Project:          tramontana, for solo viola and computer.
;;;
;;; Purpose:          Defining the data to be used in the composition: harmonic
;;;                   material, rhythmic sequences, sequence order etc. etc.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    23rd August 2002 (Graz)
;;;
;;; $$ Last modified: 20:14:17 Thu Jun 16 2016 WEST
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (in-package :cm)

;;; (setf *scale* (find-object 'twelfth-tone)) 

(in-package :sc)

(in-scale :twelfth-tone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Say we want to repeat three notes in each possible permutation of those
;;; three notes, <level> is 3, max is, say, 20, so we get 360 notes:
;;; (permutate-permutations 3 20) ->
;;; ((0 1 2) (0 2 1) (1 2 0) (2 0 1) (1 0 2) (2 1 0) (1 2 0) (0 1 2) (0 2 1)
;;;  (2 1 0) (2 0 1) (1 0 2) (0 2 1) (0 1 2) (2 1 0) (1 2 0) (1 0 2) (2 0 1)
;;;  (2 0 1) (1 0 2) (0 2 1) (1 2 0) (0 1 2) (2 1 0) (1 0 2) (2 0 1) (0 2 1)
;;;  (2 1 0) (0 1 2) (1 2 0) (1 0 2) (2 1 0) (0 2 1) (1 2 0) (2 0 1) (0 1 2)
;;;  .......

(defun permutate-permutations (level max &optional (skip 0))
  (let* ((perms (permutations level))
         (len (length perms))
         ;; 20 permutations of the 6 possibilities for the perms, each of
         ;; which is length three so we end up with 6 x 20 x 3 = 360 numbers.
         (order (flatten (inefficient-permutations len :max max :skip skip))))
    (move-repeats
     (loop for i in order collect (nth i perms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tramontana-transpose-pitch (pitch semitones)
  (declare (ignore semitones))
  (make-pitch (get-sounding-note (id pitch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tramontana-transpose-chord (chord semitones)
  (declare (ignore semitones))
  (cond ((chord-equal chord +hc1+)
                (simple-chord-transpose chord))
               ((chord-equal chord +hc2+)
                (make-chord '(c3 assf5 assf5)))
               ((chord-equal chord +hc3+)
                (make-chord (list 'assf5 
                                  (get-sounding-note 'g3 'iii)
                                  'assf5)))
               ((chord-equal chord +hc4+)
                (make-chord (list 'assf5 'assf5 
                                  (get-sounding-note 'd4 'ii))))
               ((chord-equal chord +hc5+)
                (simple-chord-transpose chord))
               ((chord-equal chord +hc6+)
                (simple-chord-transpose chord))
               ((chord-equal chord +hc7+)
                (simple-chord-transpose chord))
               ((chord-equal chord +hc8+)
                (make-chord (list 'g3 
                                  (get-sounding-note 'ef4 'iii)
                                  (get-sounding-note 'c5 'ii)
                                  ;; harmonic and non-harmonic sound same
                                  (get-sounding-note 'a5))))
               ((chord-equal chord +hc9t+)
                (make-chord (list 'assf5
                                  (get-sounding-note 'g3 'iii)
                                  (get-sounding-note 'd4 'ii))))
               (t (error "Unhandled chord: ~a" chord))
               ))

(defun simple-chord-transpose (chord)
  (make-chord
   (loop 
       for p in (data chord) 
       for new = (tramontana-transpose-pitch p nil)
       do (setf (marks new) (marks p))
       collect new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-sounding-note (note &optional (force-non-harm nil))
  ;; meaning 7/6 of a semitone lower = 1 st and a 12th tone lower!
  (flet ((iii (harmonic) (freq-to-note (* +viola-III+ harmonic)))
         ;; a semitone higher minus a 6th tone, i.e. 2/3 of st higher
         (ii (harmonic) (freq-to-note (* +viola-II+ harmonic)))
         (i (harmonic) (freq-to-note (* +viola-I+ harmonic)))
         (iv (harmonic) (freq-to-note (* +viola-IV+ harmonic)))
         (iii-non-harm (note) (transpose-note note -7/6))
         (ii-non-harm (note) (transpose-note note 2/3))
         (i-non-harm (note) (transpose-note note 2/3)))
    (if (eq force-non-harm 'iii)
        (iii-non-harm note)
      (if (eq force-non-harm 'ii)
          (ii-non-harm note)
        (when note
          (case note 
            ;; I-harm
            (a5 (i 2))
            ;; II-harm
            (fs4 (ii 5))
            (g4 (ii 4))
            (a4 (ii 3))
            ;; III-harm
            (b3 (iii 5))
            (c4 (iii 4))
            (d4 (iii 3))
            ;; IV-harm
            (dssf3 (iv 7))
            (e3 (iv 5))
            (f3 (iv 4))
            (g3 (iv 3))
            ;; III-non-harm
            (df4 (iii-non-harm 'df4))
            (ef4 (iii-non-harm 'ef4))
            (f4 (iii-non-harm 'f4))
            ;; IV-non-harm
            (fs3 'gf3)
            (gs3 'af3)
            (bf3 'bf3)
            (t (error "Unhandled note: ~a" note))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This gets indices for those notes that should be "real notes" i.e. not
;;; harmonics, not "dead harmonics" but normal, fingered, sounding notes.  
;;; The number of real notes will increase as the algorithm progresses.

(defun get-real-note-indices ()
  (let* ((non-harms
          ;; We use +tramontana-harms-with-rests+, the final form of the
          ;; algorithm (that starts in the last section (3 3) bar 290), find
          ;; out which notes in that list are "dead harmonics" ...
          (loop 
              for note in +tramontana-harms-with-rests+
              for i from 0
              when (or (member note +tramontana-III-non-harm+)
                       (member note +tramontana-IV-non-harm+))
              collect i))
         (lsys (do-lookup +tramontana-binary-lfl+ 2 (length non-harms))))
    ;; ... then use +tramontana-binary-lfl+ (the l-system also used for string
    ;; selection (just G and C) and selection of harmonics or dead harmonics--
    ;; where we proceed from mainly 1s to mainly 2s) to decide whether this
    ;; should be a real note or not: yes when we have a 2, no when it's a 1
    (loop for i in non-harms and bin in lsys 
        when (= bin 2)
             collect i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This takes a list of notes (symbols) and rests (nil) and creates sc::events
;;; The actual notes have diamond heads as they are harmonics and non-harmonics
;;; played as harmonics (dead harmonics--diamond is filled with gray).  If
;;; <do-real-notes> is t then some of the non-harmonics will be changed to
;;; real, normally-fingered notes according to the algorithm defined in
;;; get-real-note-indices above. 

(defun make-tramontana-harm-events (notes &optional (do-real-notes nil))
  (loop with real-notes = (when do-real-notes (get-real-note-indices))
     with stem-up
     for note in notes
     for e = (make-event note 't32 :is-rest (unless note t))
     for fingering = (position note +tramontana-III-non-harm+)
     for i from 0
     do
     #-cmn (warn "No CMN")
     #+cmn 
     (when note
       ;; notes on the G string are all stem up, C string (and D string) are
       ;; stem down
       (setf stem-up (or (member note +tramontana-III-harm+)
                         (member note +tramontana-III-non-harm+)))
       ;; put an accent on the "real notes" (i.e. normal note head)
       (if (member i real-notes)
           (add-mark e (cmn::accent))
           ;; make the dead-harmonics (grayed in diamond) ...
           (if (or (member note +tramontana-III-non-harm+)
                   (member note +tramontana-IV-non-harm+))
               (dead-harmonic e stem-up)
               ;; ... or real harmonics
               (add-mark e (cmn::note-head :artificial-harmonic))))
       (add-mark e (if stem-up
                       cmn::stem-up
                       cmn::stem-down)))
     collect e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-sounding-notes (notes)
  (loop 
     for n in (get-sounding-notes notes)
     for e = (make-event n 't32 :is-rest (unless n t))
     do
       #-cmn (warn "No CMN")
       #+cmn (add-mark e (if n 
                              cmn::no-stem
                              ;; rests
                              cmn::invisible))
      collect e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dh ()
  #+cmn (cmn::note-head :gray-filled-diamond))

(defun dead-harmonic (event stem-up)
  (add-mark event (dh))
  ;; this makes the whole note and stem gray
  ;; (add-mark event (cmn::note-head :diamond (cmn::gray-scale .5)))
  ;; (add-mark event (cmn::note-head (cmn::draw-filled-diamond)))
  ;; (add-mark event (cmn::dead-harmonic
  ;;                  (cmn::dy (if stem-up -.2 .2))))
  ;;(add-mark event (cmn::natural-harmonic))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tramontana-symbol (name)
  ;; (format nil "/winxp/music/tramontana/score/symbols/~a.eps" name))
  (format nil "/music/tramontana/score/symbols/~a.eps" name))

(defun no-head ()
  #+cmn (cmn::note-head :none))

(defun fist (&optional (dx -.2) (dy 1.5))
  #+cmn (cmn::graphics 
   ;; there's a problem with photoshop eps files so try body1 instead
     ;; (cmn::file (tramontana-symbol "fist"))
     (cmn::file (tramontana-symbol "body1"))
     (cmn::scale .55 .55)
     (cmn::dx dx) (cmn::dy dy)))

(defun tailpiece (&optional (dx -.2) (dy 1.5))
  #+cmn (cmn::graphics 
     ;; (cmn::file (tramontana-symbol "tailpiece"))
     (cmn::file (tramontana-symbol "body1"))
     (cmn::scale .7 .7)
     (cmn::dx dx) (cmn::dy dy)))

(defun body1 (&optional (dx -.6) (dy 1.5))
  #+cmn (cmn::graphics 
     (cmn::file (tramontana-symbol "body1"))
     (cmn::scale .7 .7)
     (cmn::dx dx) (cmn::dy dy)))

(defun body2 (&optional (dx -.6) (dy 1.5))
  #+cmn (cmn::graphics 
         (cmn::file (tramontana-symbol "body2"))
         (cmn::scale .7 .7)
         (cmn::dx dx) (cmn::dy dy)))

(defun bridge (&optional (dx 0.0) (dy 1.5))
  #+cmn (cmn::on-bridge (cmn::dx dx) (cmn::dy dy)))

(defun ntxt (text &key (dx 0.0) (dy 1.5))
  #+cmn (cmn::text text (cmn::dx dx) (cmn::dy dy) (cmn::font-name "Times-Roman")
             (cmn::font-size 8)))

(defun open-string ()
  (first (mk 0)))

(defun clb ()
  (ntxt "clb"))

(defun cl ()
  (ntxt "cl"))

(defun hair ()
  (ntxt "hair"))

(defun spe ()
  (ntxt "spe"))

(defun ste ()
  (ntxt "ste"))

(defun vlb ()
  (ntxt "vlb"))

(defun ord ()
  (ntxt "ord"))

(defun sp ()
  (ntxt "sp"))

(defun st ()
  (ntxt "st"))

(defun pizz ()
  (ntxt "pizz"))

(defun arco ()
  (ntxt "arco"))

(defun pizz-nail ()
  (ntxt "pizz nail"))

(defun br (bar note dy &optional (index 0))
  (note-add-bracket-offset +tramontana+ bar note 'vla :dy dy :index index))

(defun tr3 ()
  #+cmn (cmn::unmeasured-tremolo (cmn::tremolo-slashes 3)))

(defun tr4 ()
  #+cmn (cmn::unmeasured-tremolo (cmn::tremolo-slashes 4)))

(defun text1 (x y text)
  (ps-text x y text :font "Verdana-Italic" :font-size 10.0))

(defmacro acmtn (bar note &rest marks)
  `(add-marks-to-note +tramontana+ ,bar ,note 'vla ,@marks))

(defun acmtns (mark-function notes)
  ;; (add-mark-to-notes +tramontana+ mark-function 'vla notes)
  )

(defun acmtnsft (mark-function notes)
  (add-mark-to-notes-from-to +tramontana+ mark-function 'vla notes))

(defmacro rmbe (bar num-bars new-events &rest keys)
  `(replace-multi-bar-events +tramontana+ 'vla ,bar ,num-bars ,new-events
                             :auto-beam nil
                             ,@keys))

(defmacro re (bar start-event replace-num-events new-events &rest keys)
  `(replace-events +tramontana+ 'vla ,bar ,start-event ,replace-num-events
                   ,new-events ,@keys))

(defun treble (bar-num note-num)
  #+cmn (add-mark-before +tramontana+ bar-num note-num 'vla cmn::treble))

(defun alto (bar-num note-num)
  #+cmn (add-mark-before +tramontana+ bar-num note-num 'vla cmn::alto))

(defun perc (bar-num note-num)
  #+cmn (add-mark-before +tramontana+ bar-num note-num 'vla 
                              cmn::percussion))

;; (defun bnum (section seq bar)
;;   (get-bar-num-from-ref +tramontana+ section seq bar))

(defun mf-d ()
  (mk mf :dy -0.5))

(defun pp-d ()
  (mk pp :dy -0.5))

(defun f-d ()
  (mk f :dy -0.5))

(defun pp-sub (&optional (dy 0.0))
  (mk pp :text "sub" :dy dy))

(defun p-sub (&optional (dy 0.0))
  (mk p :text "sub" :dy dy))

(defun mp-sub (&optional (dy 0.0))
  (mk mp :text "sub" :text-dx .8 :dy dy))

(defun mf-sub (&optional (dy 0.0))
  (mk mf :text "sub" :text-dx .8 :dy dy))

(defun f-sub (&optional (dy 0.0))
  (mk f :text "sub" :dy dy))

(defun up-bow (&optional (dy 0.0))
  #+cmn (cmn::up-bow (cmn::dy dy) (cmn::scale 1.7 1.7)))

(defun down-bow (&optional (dy 0.0))
  #+cmn (cmn::down-bow (cmn::dy dy) (cmn::scale 1.7 1.7)))

(defun battuto ()
  #+cmn (cmn::sc-sprechstimme))

(defun ah ()
  #+cmn (cmn::note-head :artificial-harmonic))

(defun aux-note (note)
  #+cmn (cmn::auxiliary-note note
                       cmn::no-stem cmn::in-parentheses))

(defun qnp (&key (dx 0.0) (dy 0.5))
  #+cmn (cmn::quarter-note-in-parentheses 
         (cmn::dx dx) (cmn::dy dy)))

(defun tup (ref info &optional (delete t))
  (add-tuplet-bracket (get-bar +tramontana+ ref 'vla)
                      info
                      ;; delete all tuplet brackets in the bar first?
                      delete))

;; todo: add this to the piece class and update the beams info of the rsb!
(defun bm (ref start-note end-note)
  (let ((bar (get-bar +tramontana+ ref 'vla)))
    ;; (format t "~&re-beaming bar ~a" (bar-num bar))
    (start-beam (get-nth-non-rest-rhythm (1- start-note) bar))
    (end-beam (get-nth-non-rest-rhythm (1- end-note) bar))
    t))

(defun ttie (bar-num note-num)
  (tie +tramontana+ bar-num note-num 'vla))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Create running t32s on harmonic and non-harmonic points of the string, but
;;; both played as flageolets.  Start off with the non-harm points (1) and
;;; transition to the harmonic points (2) by using an l-system (binary-lfl).
;;; Use the same l-sys to select whether we play the notes on the III or IV
;;; string--should be fairly even all through.  On each string four harmonic
;;; and three non-harmonic nodes are available.  The order in which these are
;;; chosen is simply permutational.

;;; harmonic nodes on D string
(defparameter +tramontana-II-harm+ '(fs4 g4 a4))
;;; harmonic nodes on G string
(defparameter +tramontana-III-harm+ '(b3 c4 d4))
;;; non-harmonic nodes ("dead harmonics") on G string
(defparameter +tramontana-III-non-harm+ '(df4 ef4 f4))
;;; harmonic nodes on C string
(defparameter +tramontana-IV-harm+ '(e3 f3 g3))
;;; non-harmonic nodes ("dead harmonics") on G string
(defparameter +tramontana-IV-non-harm+ '(fs3 gs3 bf3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; the l-sequence for transition from non-harmonic to harmonic nodes and
;;; selection of G or C string.  A transition will actually be made here when
;;; do-lookup is called from mainly 1s to mainly 2s

(defparameter +tramontana-binary-lfl+
    (make-l-for-lookup 
     'binary-lfl
     ;; the transition sequences:
     ;; when we do lookup with the l-sequence, the 1 will cause lookup into the
     ;; first sequence (line), the 2, lookup into the second sequence.  The two
     ;; lists in each of these sequences represent the list to cycle around at
     ;; the beginning and the list to cycle around at the end.  (Beginning and
     ;; end here refer to the number of events requested when do-lookup is
     ;; called.) 
     '((1 ((1 1 1 2 1 1) (2 1 2 2 2 2)))
       (2 ((1 1 2 1 1 1) (1 2 2 2 2 2))))
     ;; the rules:
     ;; A simple l-sequence will here return a sequence of fairly evenly
     ;; distributed 1s and 2s, 
     '((1 (1 2 2 2 1 1)) (2 (2 1 2 1 1 1 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; On each string we have 3 harmonic and 3 non-harmonic nodes to play (see
;;; above). We want to play each of these 3 notes in permutation, not repeating
;;; one until the other 2 are already played, but always with a different order
;;; e.g.
;;; ((1 2 0) (2 0 1) (2 1 0) (1 0 2) (0 1 2) (0 2 1) (2 1 0) (2 0 1) (0 2 1)
;;;  (0 1 2) (1 0 2) (1 2 0) (1 2 0) (2 0 1) (0 1 2) (0 2 1) (2 1 0) (1 0 2)
;;;  (1 2 0) (1 0 2) (0 2 1) (2 0 1) (2 1 0) (1 2 0) (2 1 0) (2 0 1) (0 1 2)
;;;  (0 1 2) (1 0 2) (0 2 1) (0 1 2) (0 2 1) (2 0 1) (2 1 0) (1 2 0) (1 0 2)
;;;  .....

(defparameter +tramontana-3perms+ (permutate-permutations 3 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A list of 768 "notes" on strings C and G (fairly evenly distributed) each
;;; note being either a harmonic or a non-harmonic (but played as a harmonic)
;;; and transitioning gradually from almost exclusively non-harmonics to almost
;;; exclusively harmonics.

(defparameter +tramontana-harms+
    (let* (;; 8 sequences, each 4 bars of 2/4, rhythm is t32
           (total-notes (* 8 4 2 12))
           ;; whether we're going to play a harmonic or non-harmonic node--this
           ;; will transition from mainly 1 (non-harmonic) to mainly 2
           ;; (harmonic) 
           (binary-transition-type (do-lookup +tramontana-binary-lfl+ 1
                                              total-notes))
           ;; which string we're going to play on: this is taken from the same
           ;; l-sequence as that used to choose harmonic or non-harmonic nodes
           ;; but note that we don't do lookup (which causes the transition)
           ;; rather we just get the l-sequence which will have roughly the
           ;; same number of 1s and 2s distributed evenly.  So, we alternate
           ;; string all the way through but transition from non-harmonic to
           ;; harmonic nodes. 
           (binary-transition-string (get-l-sequence +tramontana-binary-lfl+ 1
                                                     total-notes))
           ;; on each string there is a group of 3 harmonic and 3 non-harmonic
           ;; nodes.  These will be permutated as documented in the
           ;; +tramontana-3perms+ parameter above.  We start reading a
           ;; flattened version of that list at 4 different points (0, 9, 18,
           ;; 27), one each for the harmonic and non-harmonic nodes of each of
           ;; the two strings.
           (permutated-harms1 (flatten (subseq +tramontana-3perms+ 0 
                                               (ceiling total-notes 3))))
           (permutated-harms2 (flatten (subseq +tramontana-3perms+ 9 
                                               (ceiling total-notes 3))))
           (permutated-non-harms1 (flatten (subseq +tramontana-3perms+ 18 
                                                   (ceiling total-notes 3))))
           (permutated-non-harms2 (flatten (subseq +tramontana-3perms+ 27
                                                   (ceiling total-notes 3)))))
      (loop 
          for type in binary-transition-type 
          for string in binary-transition-string
          with note-group
          with note 
          with note-index
          do
            ;; signal an error if we run out of notes in the permutations
            (when (or (not permutated-non-harms1) 
                      (not permutated-non-harms2)
                      (not permutated-harms1)
                      (not permutated-harms2))
              (error "Permutations!"))
            ;; using the string (1=G, 2=C) and type (1=non-harmonic,
            ;; 2=harmonic) variables, get the group we are to select the note
            ;; from: +tramontana-III-non-harm+, +tramontana-III-harm+,
            ;; +tramontana-IV-non-harm+ or +tramontana-IV-harm+
            (setf note-group (if (= string 1)
                                 (if (= type 1)
                                     +tramontana-III-non-harm+
                                   +tramontana-III-harm+)
                               (if (= type 1)
                                   +tramontana-IV-non-harm+
                                 +tramontana-IV-harm+))
                  note-index (if (= type 1) ;; non-harmonic
                                 (if (= string 1) ;; G string
                                     (pop permutated-non-harms1)
                                   (pop permutated-non-harms2)) ;; C string
                               ;; harmonic
                               (if (= string 1) ;; G string
                                   (pop permutated-harms1)
                                 (pop permutated-harms2))) ;; C string
                  ;; now we have the group to read from, and the index into the
                  ;; group, we can get the note
                  note (nth note-index note-group))
          collect note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Bring string II into play: We're going to modify +tramontana-harms+ by
;;; gradually introducing more and more harmonics from the D-string overwriting
;;; the C and G-string harmonics that were already there.

;;; Split the 768 +tramontana-harms+ into 24 groups of 32 notes.  These 24
;;; groups take the following number of harmonics from string II: 4x0, 4x1,
;;; 4x2, 2x3, 2x4, 2x5, 1x6, 1x7, 4x8

(defparameter +tramontana-harms-with-II+
    ;; the indices are where the harmonics on the D string come in in the
    ;; relevant 32-note group.
    ;; the first 128 have no D harmonics (4x0)
    ;; each sublist of indices refers to one of the 24 groups of 32 notes,
    ;; hence the indices range from 0 to 31.
    ;; the length of the sublist reflects the number of D harmonics that will
    ;; be introduced
    ;; the actual indices given here were chosen at will, not really according
    ;; to any system.
    (let* ((indices '((8) (14) (20) (26) ;; 4x1
                      (9 10) (3 29) (13 19) (29 30) ;; 4x2
                      (16 17 18) (11 12 21) ;; 2x3
                      (16 17 18 26) (5 6 24 25) ;; 2x4
                      (9 10 11 22 23) (13 14 18 19 20) ;; 2x5
                      (2 3 8 15 16 17) ;; 1x6
                      (5 6 10 11 12 18 19) ;; 1x7
                      ;; 2 2 3 1, 2 3 2 1, 3 2 2 1, 1 2 3 2
                      (2 3 9 10 15 16 17 24) (4 5 11 12 13 19 20 31) ;; 4x8
                      (3 4 5 11 12 17 18 25) (2 5 6 15 16 17 28 29)))
           (real-indices (loop 
                             for list in indices
                             with index = 128 ;; first 128 have no D harmonics
                             with result = '()
                             do
                               (loop for i in list do
                                     (push (+ index i) result))
                               ;; move onto the next group of 32 notes
                               (incf index 32) 
                             finally (return (nreverse result))))
           ;; the harmonics and non-harmonics on the G string
           ;; these are only used to find out if a note is played on the
           ;; g-string when avoiding string jumps below
           (string-III (append +tramontana-III-harm+
                               +tramontana-III-non-harm+)) 
           ;; ditto on the C string
           (string-IV (append +tramontana-IV-harm+ +tramontana-IV-non-harm+))
           (flattened-3perms (flatten +tramontana-3perms+))
           ;; get a whole load of G-string harmonics for avoiding string jumps
           (III-harms (loop 
                          for i in flattened-3perms
                          collect (nth i +tramontana-III-harm+)))
           ;; get a whole load of D-string harmonics for inserting (not just to
           ;; avoid string jumps!)
           (II-harms (loop 
                         for i in flattened-3perms
                         repeat (length real-indices)
                         collect (nth i +tramontana-II-harm+)))
           ;; this is where we copy our original data to modify by introducing
           ;; the D-string harmonics
           (result (copy-list +tramontana-harms+)))
      ;; and this is where we actually pop the D-string harmonics in,
      ;; overwriting any C or G-string harmonics that were already there.
      (loop for i in real-indices do 
            (setf (nth i result) (pop II-harms)))
      ;; here we simply avoid string-jumping:
      ;; make sure that any notes on string II are preceded by string III
      (loop for n1 in result and n2 in (cdr result) and i from 0 do
            (if (and (member n1 string-IV)
                     (member n2 +tramontana-II-harm+))
                (setf (nth i result) (pop III-harms))
              (when (and (member n1 +tramontana-II-harm+)
                         (member n2 string-IV))
                (setf (nth (1+ i) result) (pop III-harms)))))
      result))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now we modify +tramontana-harms-with-II+ to put occasional rests into the
;;; constant flow of t32 notes.  This is actually the final form of the
;;; algorithm that will be inserted in to the piece starting at section (3 3)
;;; (bar 290)--the 768 t32 notes are 32 bars long, taking us to the end of the
;;; piece at 321
;;; This global is used by make-tramontana-harm-events (see functions.lsp) to
;;; actually make the sc::events for the sc structure.

(defparameter +tramontana-harms-with-rests+
    (let* ((result (copy-list +tramontana-harms-with-II+))
           ;; this determines the number of t32 rests when we insert the rest,
           ;; varies from 1 to 5 with more occuring at the beginning and only 1
           ;; occuring at the end.
           (num-rests-lfl 
            (make-l-for-lookup 
             'num-rests
             '((1 ((3 1 2) (1 1 2) (1)))
               (2 ((4 3 4) (2 1 2) (1)))
               (3 ((5 2 4) (1 3 2) (1))))
             '((1 (3)) (2 (3 1)) (3 (1 2)))
             :auto-check-redundancy nil))
           ;; should be 768 but let's be sure
           (total-notes (length result))
           ;; this determines how many notes will occur before a rest or rests
           ;; are inserted.  N.B. there is no transition here, we just use the
           ;; l-sequence. 
           (num-notes-lfl 
            (make-l-for-lookup 
             'num-notes
             nil
             '((13 (11 17)) (25 (17 13)) (17 (13)) (11 (25 17)))))
           (num-rests (do-lookup num-rests-lfl 2 50))
           (num-notes (get-l-sequence num-notes-lfl 17 50)))
      ;; (print num-rests)
      ;; (print num-notes)
      (loop 
          for nn in num-notes 
          for nr in num-rests
          for count from 1 
          with i = 0 
          while (< i total-notes)
          ;; collect nr into used
          do
            (incf i nn)
            ;; insert num-rests nils (i.e. rests) into our copy of
            ;; +tramontana-harms-with-II+ after num-notes 
            (loop for j from i repeat nr do
                  (when (< j total-notes)
                    (setf (nth j result) nil))))
      result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is the algorithm that starts at section (3 1) (bar 222).
;;; It makes sporadic but ever more often little bursts of notes (separated by
;;; long but becoming shorter rests).   
;;; It returns a list of notes and nils (rests).
;;; This global is used by make-tramontana-harm-events (see functions.lsp) to
;;; actually make the sc::events for the sc structure.

(defparameter +tramontana-harms-transition+
    (let* ((l-distance 
            ;; how many ts notes between harms--becomes less and less, starting
            ;; at a max of 50 and decreasing to 1 but in fact twice this
            ;; because of the scaler!
            (make-l-for-lookup 
             'distance
             '((1 ((50 35 42) (20 29 17) (6 8 9) (2 1 4)))
               (2 ((48 36 42) (29 16 22) (7 9 10) (3 2 1)))
               (3 ((30 42 39) (18 23 27) (8 10 7) (4 3 1))))
             '((1 (3)) (2 (3 1)) (3 (1 2)))
             :scaler 2
             :auto-check-redundancy nil))
           ;; how many harms per appearance---increases.
           (l-num-harms
            (make-l-for-lookup
             'num-harms
             '((1 ((2 3 2) (4 5 6)))
               (2 ((3 4 2) (4 7 6)))
               (3 ((4 3 2) (7 5 8))))
             '((1 (3)) (2 (3 1)) (3 (1 2)))
             :offset 1
             :auto-check-redundancy nil))
           ;; 3.1 and 3.2 are 68 bars long = (* 68 2 12) = 1632 t32 notes
           ;; N.B. both use transitions
           (distance (do-lookup l-distance 2 49))
           (num-harms (do-lookup l-num-harms 2 49))
           ;; actually 257 last time I looked
           (total-harms (loop for h in num-harms sum h))
           (result
            ;; this is where we use the 768 harmonics and non-harmonics on C
            ;; and G strings as calculated above but reversed so that we
            ;; actually start with mainly harmonics and gradually progress to
            ;; more and more non-harmonics so that when
            ;; +tramontana-harms-with-rests+ at bar 290 we can start the
            ;; progression back again.
            ;; N.B. because so few harmonics are presented at a time at the
            ;; beginning, they will not dominate as at the end of the piece
            ;; where they run uninterrupted.
            (loop
                for dist in distance 
                for num in num-harms 
                           ;; so total-harms has to be less the 768-300!!!
                with harms = (reverse (subseq +tramontana-harms+ 300
                                              (+ 300 total-harms)))
                with return = '()
                              ;; it could be that we want to have more harms
                              ;; than we've got space for...
                for do-notes = (min dist num)
                for do-nil = (- dist do-notes)
                do
                  ;; ran out of notes?
                  (unless harms
                    (error "Need more notes (harms)!"))
                  ;; the notes/harmonics
                  (loop repeat do-notes do
                        (push (pop harms) return))
                  ;; the rests
                  (loop repeat do-nil do
                        (push nil return))
                finally (return (nreverse return)))))
      ;; (print (loop for i in distance sum i))
      ;; (print (length result))
      result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +c3+ 130.8128)
(defparameter +c3-7th-harmonic+ (* +c3+ 7))
(defparameter +viola-IV+ +c3+)
(defparameter +viola-III+ (/ +c3-7th-harmonic+ 5))
(defparameter +viola-II+ (/ +c3-7th-harmonic+ 3))
(defparameter +viola-I+ (/ +c3-7th-harmonic+ 2))
(defparameter +tramontana-string-freqs+
    (list +viola-IV+ +viola-III+ +viola-II+ +viola-I+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-tr-chord (notes)
  (make-chord notes :midi-channel 1 :microtones-midi-channel 2))

(defparameter +h-iv+ 'dssf3)
(defparameter +hc1+ (make-tr-chord (list +h-iv+ 'b3 'a4)))
(defparameter +hc2+ (make-tr-chord '(c3 b3 a4)))
(defparameter +hc3+ (make-tr-chord (list +h-iv+ 'g3 'a4)))
(defparameter +hc4+ (make-tr-chord (list +h-iv+ 'b3 'dbn4))) ; d4 with nat in ()
(defparameter +hc5+ (make-tr-chord '(b3 a4)))
(defparameter +hc6+ (make-tr-chord (list +h-iv+ 'b3)))
(defparameter +hc7+ (make-tr-chord '(a4 a5)))
(defparameter +hc8+ (make-tr-chord '(g3 ef4 c5 a5)))
(defparameter +a4h+ (make-pitch 'a4))

;;; This really does belong here as it is part of the global and should be
;;; added before the global is used. 
(add-mark (1st +hc1+) (ah))
(add-mark (2nd +hc1+) (ah))
(add-mark (3rd +hc1+) (ah))
(add-mark (2nd +hc2+) (ah))
(add-mark (3rd +hc2+) (ah))
(add-mark (1st +hc3+) (ah))
(add-mark (3rd +hc3+) (ah))
(add-mark (1st +hc4+) (ah))
(add-mark (2nd +hc4+) (ah))
(add-mark (1st +hc5+) (ah))
(add-mark (2nd +hc5+) (ah))
(add-mark (1st +hc6+) (ah))
(add-mark (2nd +hc6+) (ah))
(add-mark (1st +hc7+) (ah))
(add-mark (2nd +hc7+) (ah))
(add-mark +a4h+ (ah))
#+cmn (add-mark +hc5+ (cmn::fingering "II" "III"))

;;; Same as above but tremolo
(defparameter +hc1t+ (clone +hc1+))
(defparameter +hc2t+ (clone +hc2+))
(defparameter +hc3t+ (clone +hc3+))
(defparameter +hc4t+ (clone +hc4+))
(defparameter +hc8t+ (clone +hc8+))
(defparameter +hc9t+ (make-tr-chord (list +h-iv+ 'g3 'd4)))

(add-mark +hc1t+ (tr4))
(add-mark +hc2t+ (tr4))
(add-mark +hc3t+ (tr4))
(add-mark +hc4t+ (tr4))
(add-mark +hc8t+ (tr4))
(add-mark (1st +hc9t+) (ah))
(add-mark +hc9t+ (tr4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This will decide which of the three transpositions of the three basic
;;; chords will be used.  Given a call to do-lookup, this will simply create a
;;; transition from 1 to 3.

(defparameter +tramontana-harmonic-lfl+
    (make-l-for-lookup
     'ternary-lfl
     '((1 ((1) (2) (3))))
     '((1 (1 1)))
     ))

(defparameter +tramontana-chord-order+
    (let* ((perms (loop for i in 
                        (flatten (subseq +tramontana-3perms+ 0 30))
                      collect (1+ i)))
           (1-to-3-transition (do-lookup +tramontana-harmonic-lfl+ 1 76))
           (refs (loop for i in 1-to-3-transition and j in perms collect
                       (list i j)))
           (sections (split-into-sub-groups refs '(25 25 26)))
           (sub-sections 
            (loop for section in sections 
                for i from 1 
                for sub-secs1 = 
                  ;; the sections are actually (8 9 8) (8 9 8) (8 9 9)
                  ;; but the (8 9 9) will yield (8 9 8) when there's only 25 in
                  ;; the list.
                  (split-into-sub-groups section '(8 9 9))
                for sub-secs2 = 
                  (loop for ss in sub-secs1 and i from 1 collect 
                        (list i ss))
                collect
                  (list i sub-secs2))))
      sub-sections))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +tramontana-loops+
    '((14.21644 14.717097 15.155375 15.326848)
      (37.45814 37.877552 38.27229 38.73669)
      (67.29288 67.6644 68.25796 69.01406)
      (72.27647 73.35329 74.4693 75.07592 76.83338 77.937775 78.80272)
      (111.81424 112.74884 113.220184)
      (123.09914 123.94521 124.985756 126.22803 127.2468)
      (130.00853 131.22757 131.81387 132.0098)
      (135.3883 135.87012 136.42885 136.91211)
      (151.8643 153.18059 153.87283 154.75374)
      (163.55556 164.03882 164.53514 165.68889)
      (165.9951 166.50594 166.81796)
      (168.62767 169.33456 169.58984 170.45769 170.95111 172.05696 172.30948)
      (183.844 184.21841 185.45197 186.35754)
      (215.02548 215.68726 216.05588 216.60445 217.05434 217.38521)
      (217.91637 218.23274 218.95256 219.24861 219.41696 219.73624)
      (221.9131 222.58359 223.21342 223.55592 223.84616 224.25844)
      (224.30766 224.63855 224.7111 224.78368 225.09424 225.40771)
      (225.98358 226.35973 226.66739 227.32045 227.78485)
      (230.6293 230.81215 231.23882 232.10957)
      (266.925 267.2733 267.66513 268.17596 268.7768 269.33987 270.0916
       270.38187 270.6431 271.1278) 
      (272.13788 272.44553 272.7706 273.2002)
      (302.4109 302.91013 303.2439 303.73734)
      (368.21335 368.74158 369.38596 370.02158 371.1129)
      (376.89758 377.99564 378.79874 379.1267 379.50693 379.84653 380.33997
       381.5445) 
      (381.74475 382.2672 382.69388 382.97833)
      (389.67148 390.02267 390.8383 391.48553 392.16763 392.6088)
      (411.58847 412.1745 412.6273 413.35583 414.20337 414.81287)
      (433.57462 434.36987 434.9678 435.62958 436.23038 437.0837)
      (439.0458 439.57407 440.16037 441.0166)
      (448.2438 449.65442 451.0331 452.1767 452.69913)
      (452.97778 453.2245 453.40735 453.67438 453.9066)
      (507.03674 507.29797 507.73044 508.0352 508.53152 509.37323 510.273)
      (515.2624 515.8429 516.38855 516.71075)
      (578.65576 578.89667 579.0273 579.38434 579.66614 579.79645)
      (581.1752 581.27673 581.3928 581.5525 581.65405 581.77594 581.90656
       582.0459 582.342 582.4174 582.60315) 
      (588.0976 588.29205 588.6171 588.9103 589.10474 589.2586 589.3312
       589.40955 589.7375 590.1206) 
      (590.2803 590.46893 590.63727 590.73303 590.8056 591.1597 591.2178)
      (596.7151 597.00824 597.22595 597.3827 597.522 597.84705)
      (611.9735 612.0954 612.197 612.2986 612.4002 612.48724 612.6469 612.87036
       613.05334) 
      (648.1734 648.54785 648.6872 649.0065 649.0645 649.1951 649.5376)
      (659.4148 659.90564 660.2855 660.4858 661.02277 661.37396 661.87317)
      (711.62775 711.8832 712.05444 712.246 712.41724 712.6669)
      (762.38947 762.6072 762.7436 762.8684 763.028 763.1064 763.3125)
      (785.1305 785.27563 785.4846 785.61816 785.82715 786.14056 786.55273
       786.97363 787.3306) 
      (844.7507 845.2092 845.47046 845.6359 845.8478 846.13806)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
MDE Sun Jan  1 14:37:30 2012 -- similar to skin, this piece used a version of
slippery chicken without pitch selection routines in detail so can no longer
run.  
(make-slippery-chicken 
 '+tramontana+
 ;; 16/6/04
 ;;
 ;; At the time of composing the viola part, things like the harmonic structure
 ;; weren't fixed.  Work this out here for the generation of the sound files. 
 ;; The notes here are taken from the sounding harmonics on strings ii-iv
 :set-palette
 (let ((tramontana-diff-iv-iii 
        (- (midi-note-float (make-pitch +VIOLA-III+))
           (midi-note-float (make-pitch +VIOLA-IV+))))
       (tramontana-diff-iv-ii
        (- (midi-note-float (make-pitch +VIOLA-II+))
           (midi-note-float (make-pitch +VIOLA-IV+)))))
   `((1 ((1 ((c3 d3 e3 f3 g3 c5 d5 e5 f5 g4 fssf5 assf5)))
         (2 ((c3 d3 e3 f3 g3 c5 d5 e5 f5 cstf5 etf5 dssf6)))
         (3 ((c3 d3 e3 f3 g3 c5 d5 e5 f5 assf5 gqf6)))))
     (2 ((1 ((1 1) :transposition ,tramontana-diff-iv-iii))
         (2 ((1 2) :transposition ,tramontana-diff-iv-iii))
         (3 ((1 3) :transposition ,tramontana-diff-iv-iii))))
     (3 ((1 ((1 1) :transposition ,tramontana-diff-iv-ii))
         (2 ((1 2) :transposition ,tramontana-diff-iv-ii))
         (3 ((1 3) :transposition ,tramontana-diff-iv-ii))))))
 
 
 ;; 
 :set-map
 +tramontana-chord-order+

 :tempo-map 
 '((((1 1) 1 1) 44)
   (27 52) (29 44) (60 48) (72 44) (80 48) (85 44) (99 48) (116 44)
   (135 48) (150 50) (199 52) (222 54) (256 52) (267 50) (275 48)
   (281 46) (289 44))
   
 :instrument-palette 
 '((viola (:lowest-written c3
           :staff-name "Viola"
           :starting-clef percussion
           :highest-written f6
           :chords t))
   (viola-sounding (:lowest-written c3 :highest-written f6 :chords t
                    :starting-clef treble :staff-name "Viola Sounding"))
   (computer (:starting-clef percussion :staff-name "Computer")))
   
 :ensemble
 '(((vla-snds (viola-sounding 
               :midi-channel 3 :microtones-midi-channel 4
               :cmn-staff-args (staff-size .7)))
    (vla (viola :midi-channel 1 :microtones-midi-channel 2))
    (cptr1 (computer :midi-channel 5 :microtones-midi-channel 6))
    (cptr2 (computer :midi-channel 7 :microtones-midi-channel 8))))
 
 :staff-groupings '(4)

 :instruments-hierarchy '(vla cptr1 cptr2 vla-snds)
                    
 ;; :instruments-write-bar-nums '(vla)
 
 :bars-per-system-map
 '((1 9) (7 12) (82 10) 
   (102 14) (158 12) (182 10) (200 11) (203 10) (213 9) 
   (222 9) (234 9) (240 8) (249 6) (256 5) (261 6)
   (267 4) (275 3) (286 2))

 ;; 25 seqs in all.
 :rthm-seq-map
 (let* ((seqs1 '(2 20  1  9 10 22 16 25  6 14 21 17  4  9 13 24 19  3 11 15  1
                 18 11 16 12))
        (seqs2 '(2 23  3  7 13 22 19  3  8 12 23 14  2 10 15  4 20 25  9 16  5
                 17 14 18 9))
        (seqs3 '(2 21  3 12 11 22 16  1  8 17 23 20 24  9 12  2 18 21  6 15  5
                 19 11 18 15 26))
        ;; seq 26 is only one bar...
        (3butlast (butlast seqs3))
        (seqs1-rev (reverse seqs1))
        (seqs2-rev (reverse seqs2))
        (seqs3-rev (econs (reverse 3butlast) 26))
        (seqs1-reflect (reflect-list seqs1))
        (seqs2-reflect (reflect-list seqs2))
        (seqs3-reflect (econs (reflect-list 3butlast) 26)))
   `((1
      ((1 ((vla ,(subseq seqs1 0 8))
           (vla-snds ,(make-list 8 :initial-element nil))
           (cptr1 ,(subseq seqs1-rev 0 8))
           (cptr2 ,(subseq seqs1-reflect 0 8))
           ))
       (2 ((vla ,(subseq seqs1 8 17))
           (cptr1 ,(subseq seqs1-rev 8 17))
           (cptr2 ,(subseq seqs1-reflect 8 17))))
       (3 ((vla ,(subseq seqs1 17 25))
           (cptr1 ,(subseq seqs1-rev 17 25))
           (cptr2 ,(subseq seqs1-reflect 17 25))))))
     (2
      ((1 ((vla ,(subseq seqs2 0 8))
           (cptr1 ,(subseq seqs2-rev 0 8))
           (cptr2 ,(subseq seqs2-reflect 0 8))))
       (2 ((vla ,(subseq seqs2 8 17))
           (cptr1 ,(subseq seqs2-rev 8 17))
           (cptr2 ,(subseq seqs2-reflect 8 17))))
       (3 ((vla ,(subseq seqs2 17 25))
           (cptr1 ,(subseq seqs2-rev 17 25))
           (cptr2 ,(subseq seqs2-reflect 17 25))))))
     (3
      ((1 ((vla ,(subseq seqs3 0 8))
           (cptr1 ,(subseq seqs3-rev 0 8))
           (cptr2 ,(subseq seqs3-reflect 0 8))))
       (2 ((vla ,(subseq seqs3 8 17))
           (cptr1 ,(subseq seqs3-rev 8 17))
           (cptr2 ,(subseq seqs3-reflect 8 17))))
       (3 ((vla ,(subseq seqs3 17 26))
           (cptr1 ,(subseq seqs3-rev 17 26))
           (cptr2 ,(subseq seqs3-reflect 17 26))))))))
   
 :snd-output-dir "/music/tramontana/sc"
 :sndfile-palette
 '((
    (short-delicate1
     (
      (tramontana-mono :description "body trem" :start 0.005 :end 0.819)
      (tramontana-mono :description "on bridge" :start 204.428 :end 205.903)
      (tramontana-mono :description "tailpiece stroke" :start 175.848 :end
                       176.800)      
      (tramontana-mono :description "body stroke" :start 23.888 :end 24.192)
      (tramontana-mono :description "2 body strokes" :start 38.269 :end 38.893)
      (tramontana-mono :description "short tailpiece trem" :start 114.482 :end
                       115.554) 
      (tramontana-mono :description "c 7th harm" :start 551.491 :end 552.377)
      (tramontana-mono :description "short tailpiece trem swell" :start 130.000
                       :end 130.746) 
      (tramontana-mono :description "body trem" :start 29.312 :end 30.090)
      (tramontana-mono :description "short body trem" :start 135.372 :end
                       135.692) 
      ))
    (short-delicate2
     (
      (tramontana-mono :description "short body trem pp" :start 141.679 :end
                       142.727) 
      (tramontana-mono :description "body trem" :start 148.222 :end 149.464)
      (tramontana-mono :description "tailpiece trem with clb" :start 164.508
                       :end 165.872) 
      (tramontana-mono :description "short talipiece trem" :start 167.413 :end
                       168.133) 
      (tramontana-mono :description "body trem" :start 4.087 :end 5.010)
      (tramontana-mono :description "body stroke" :start 22.804 :end 23.196)
      (tramontana-mono :description "c 7th harm" :start 542.653 :end 543.766)
      (tramontana-mono :description "bridge trem" :start 240.540 :end 241.879)
      (tramontana-mono :description "short bridge high freqs" :start 258.232
                       :end 259.187) 
      (tramontana-mono :description "bridge waggle" :start 282.630 :end 283.417)
      (tramontana-mono :description "c + bridge trem" :start 314.131 :end
                       315.159) 
      ))
    (short-delicate3
     (
      (tramontana-mono :description "c+ bridge trem" :start 318.328 :end
                       319.031)  
      (tramontana-mono :description "short body trem" :start 139.369 :end
                       140.333) 
      (tramontana-mono :description "g spe arco" :start 361.639 :end 362.439)
      (tramontana-mono :description "short tailpiece trem" :start 532.254 :end
                       532.605) 
      (tramontana-mono :description "unwanted noise, good loop" :start 532.741
                       :end 533.150) 
      (tramontana-mono :description "on bridge" :start 196.197 :end 197.666)
      (tramontana-mono :description "body trem" :start 131.184 :end 132.014)
      (tramontana-mono :description "short talipiece trem" :start 535.277 :end
                       536.070)      
      (tramontana-mono :description "short body trem" :start 135.860 :end
                       136.292) 
      (tramontana-mono :description "c 7th harm" :start 539.254 :end 540.224)
      ))
    (long-delicate1
     (
      (tramontana-mono :description "body trem & hold" :start 9.434 :end 15.421)
      (tramontana-mono :description "body repeated strokes" :start 72.198 :end
                       77.372) 
      (tramontana-mono :description "stroke tap" :start 77.392 :end 79.816)
      (tramontana-mono :description "long tailpiece trem" :start 123.008 :end
                       127.121) 
      (tramontana-mono :description "aggressive body strokes" :start 79.897 :end
                       86.117) 
      (tramontana-mono :description "half flag gliss trem" :start 457.120 :end
                       462.479) 
      (tramontana-mono :description "body trem longer" :start 90.506 :end
                       94.801) 
      (tramontana-mono :description "body trem" :start 95.254 :end 96.715)
      (tramontana-mono :description "spe arco trem" :start 368.210 :end 372.629)
      (tramontana-mono :description "tailpiece trem fp" :start 127.251 :end
                       129.965) 
      ))
    (long-delicate2
     (
      (tramontana-mono :description "tailpiece trem" :start 136.429 :end
                       139.160)      
      (tramontana-mono :description "body trem rit" :start 151.824 :end 157.098)
      (tramontana-mono :description "spe trem" :start 375.374 :end 383.094)
      (tramontana-mono :description "body long strokes" :start 168.620 :end
                       172.295) 
      (tramontana-mono :description "tailpiece trem dim" :start 172.498 :end
                       175.779) 
      (tramontana-mono :description "spe trem final accents" :start 421.445 :end
                       424.557) 
      (tramontana-mono :description "tailpiece trem with  tap" :start 184.210
                       :end 185.919) 
      (tramontana-mono :description "on bridge long" :start 209.177 :end
                       214.610) 
      (tramontana-mono :description "tailpiece trem swell" :start 186.903 :end
                       191.211) 
      (tramontana-mono :description "taps then bridge hold" :start 230.618 :end
                       236.220) 
      ))
    (long-delicate3
     (
      (tramontana-mono :description "tailpiece trem" :start 246.251 :end
                       249.919)
      (tramontana-mono :description "bridge hold" :start 250.143 :end 252.610)
      (tramontana-mono :description "tailpiece trem" :start 252.729 :end
                       254.145) 
      (tramontana-mono :description "bridge trem" :start 254.299 :end 257.834)
      (tramontana-mono :description "spe trem" :start 388.383 :end 394.948)
      (tramontana-mono :description "bridge trem ppp" :start 263.349 :end
                       266.702 :amplitude 2) 
      (tramontana-mono :description "bridge hold" :start 297.160 :end 301.807)
      (tramontana-mono :description "repeated bridge strokes" :start 303.737
                       :end 314.111) 
      (tramontana-mono :description "c+ bridge trem + hold" :start 323.677 :end
                       327.883)      
      (tramontana-mono :description "c spe hold" :start 337.334 :end 341.240
                       :amplitude 2)
      (tramontana-mono :description "c+g strings spe" :start 346.244 :end
                       355.045) 
      ))
    (long-delicate4
     (
      (tramontana-mono :description "body trem" :start 96.763 :end 100.740)
      (tramontana-mono :description "spe arco repeat" :start 372.644 :end
                       375.301) 
      (tramontana-mono :description "half flag trem" :start 467.058 :end
                       472.487) 
      (tramontana-mono :description 
                       "spe trem with little click opening + half flag gliss"
                       :start 406.140 :end 420.151)      
      (tramontana-mono :description "tailpiece trem" :start 177.276 :end
                       180.699)      
      (tramontana-mono :description "half flag gliss trem" :start 430.155 :end
                       440.956) 
      (tramontana-mono :description "spe trem" :start 440.974 :end 444.006)
      (tramontana-mono :description "half flag gliss trem" :start 448.231 :end
                       452.873) 
      (tramontana-mono :description "c 7th harm" :start 491.810 :end 493.682)
      (tramontana-mono :description "body trem" :start 540.320 :end 542.154)
      ))
    (short-percussive1
     (
      (tramontana-mono :description "body clb" :start 40.109 :end 40.842)
      (tramontana-mono :description "tail clb" :start 102.980 :end 103.816)
      (tramontana-mono :description "c pizz spe" :start 333.230 :end 333.653)
      (tramontana-mono :description "tailpiece clb" :start 158.803 :end 159.610)
      (tramontana-mono :description "c 7th harm arco ff" :start 574.550 :end
                       574.906) 
      (tramontana-mono :description "g pizz" :start 359.896 :end 360.305)
      (tramontana-mono :description "tailpiece clb rhythm" :start 163.550 :end
                       164.455) 
      (tramontana-mono :description "body clb double hits" :start 166.000 :end
                       166.969)
      (tramontana-mono :description "c 7th harm bartok pizz" :start 574.913 :end
                       575.212) 
      (tramontana-mono :description "body trem + click" :start 186.396 :end
                       186.860) 
      ))
    (short-percussive2
     (
      (tramontana-mono :description "bridge clb" :start 328.084 :end 328.510)
      (tramontana-mono :description "c pizz spe" :start 328.912 :end 329.311)
      (tramontana-mono :description "g clb" :start 333.798 :end 334.182)
      (tramontana-mono :description "body clb" :start 157.317 :end 158.077)
      (tramontana-mono :description "g pizz spe" :start 341.310 :end 341.798)
      (tramontana-mono :description "c 7th harm pizz" :start 546.982 :end
                       547.503) 
      (tramontana-mono :description "tap" :start 383.311 :end 383.678)
      (tramontana-mono :description "clb" :start 395.675 :end 396.103)
      (tramontana-mono :description "body clb" :start 52.823 :end 53.539)
      (tramontana-mono :description "c 7th harm clb" :start 571.040 :end
                       571.494) 

      ))
    (short-percussive3
     (
      (tramontana-mono :description "c 7th harm pizz" :start 544.657 :end
                       545.257) 
     
      (tramontana-mono :description "g clb" :start 357.193 :end 357.471)
      (tramontana-mono :description "c 7th harm pizz" :start 552.430 :end
                       553.596) 
      (tramontana-mono :description "3 clbs" :start 398.133 :end 399.241)
      (tramontana-mono :description "c 7th harm bartok pizz" :start 572.180 :end
                       573.042) 
      (tramontana-mono :description "body clb" :start 112.750 :end 113.324)
      (tramontana-mono :description "c 7th harm arco ff" :start 575.238 :end
                       575.728) 
      (tramontana-mono :description "body clb" :start 53.921 :end 54.739)
      (tramontana-mono :description "tail clb slap" :start 111.815 :end 112.704)
      (tramontana-mono :description "c 7th harm bartok pizz" :start 575.741 :end
                       576.447) 
      ))
    (short-percussive4
     (
      (tramontana-mono :description "tail clb" :start 108.333 :end 109.203)
      (tramontana-mono :description "tap" :start 399.413 :end 400.091)
      (tramontana-mono :description "pizz spe" :start 428.107 :end 428.611)
      (tramontana-mono :description "clb" :start 395.082 :end 395.664)
      (tramontana-mono :description "jete ff" :start 444.508 :end 445.624)
      (tramontana-mono :description "c 7th harm f attack" :start 557.264 :end
                       557.731) 
      (tramontana-mono :description "clb gliss" :start 466.657 :end 467.039)
      (tramontana-mono :description "c 7th harm 3 fff" :start 493.729 :end
                       494.401)      
      ))
    (long-percussive
     (
      (tramontana-mono :description "rhythmic taps" :start 225.980 :end 228.049)
      (tramontana-mono :description "body clb + trem (longer)" :start 67.295
                       :end 72.065)  
      (tramontana-mono :description "rhythmic taps" :start 215.034 :end 219.835)
      (tramontana-mono :description "rhythmic taps" :start 221.925 :end 225.538)
      (tramontana-mono :description "rhythmic taps" :start 266.925 :end 271.273)
      (tramontana-mono :description "rhythmic taps" :start 271.791 :end 276.077)
      (tramontana-mono :description "rhythmic taps" :start 278.779 :end 280.674)

      ))
    (perms1
     (
      (tramontana-mono :description "perms" :start 728.529 :end 733.013)
      (tramontana-mono :description "perms" :start 733.021 :end 734.200)
      (tramontana-mono :description "perms" :start 735.069 :end 736.416)
      (tramontana-mono :description "perms" :start 742.116 :end 746.928)
      (tramontana-mono :description "perms" :start 747.051 :end 753.139)
      (tramontana-mono :description "perms" :start 759.039 :end 761.308)
      (tramontana-mono :description "perms" :start 761.407 :end 765.655)
      (tramontana-mono :description "perms" :start 766.347 :end 776.441)
      (tramontana-mono :description "perms" :start 776.453 :end 779.493)
      (tramontana-mono :description "perms" :start 779.518 :end 781.218)
      ))
    (perms2
     (
      (tramontana-mono :description "perms" :start 781.290 :end 785.072)
      (tramontana-mono :description "perms" :start 785.139 :end 787.657)
      (tramontana-mono :description "perms" :start 788.218 :end 793.859)
      (tramontana-mono :description "perms" :start 794.087 :end 795.668)
      (tramontana-mono :description "perms" :start 797.258 :end 799.725)
      (tramontana-mono :description "perms" :start 799.907 :end 803.947)
      (tramontana-mono :description "perms" :start 803.959 :end 806.466)
      (tramontana-mono :description "perms" :start 806.542 :end 808.047)
      (tramontana-mono :description "perms" :start 808.060 :end 810.659)
      (tramontana-mono :description "perms" :start 810.708 :end 811.836)
      ))
    (perms3
     (
      (tramontana-mono :description "perms" :start 811.921 :end 814.194)
      (tramontana-mono :description "perms" :start 814.279 :end 816.178)
      (tramontana-mono :description "perms" :start 816.209 :end 818.681)
      (tramontana-mono :description "perms" :start 818.835 :end 820.774)
      (tramontana-mono :description "perms" :start 820.871 :end 822.700)
      (tramontana-mono :description "perms" :start 822.799 :end 824.139)
      (tramontana-mono :description "perms" :start 824.234 :end 826.517)
      (tramontana-mono :description "perms" :start 826.526 :end 828.312)
      (tramontana-mono :description "perms" :start 828.323 :end 830.703)
      (tramontana-mono :description "perms" :start 830.980 :end 834.395)
      (tramontana-mono :description "perms" :start 834.402 :end 837.005)
      ))
    (perms4
     (
      (tramontana-mono :description "perms" :start 837.229 :end 838.597)
      (tramontana-mono :description "perms (clicks?)" :start 838.642 :end
                       842.191) 
      (tramontana-mono :description "perms" :start 842.218 :end 844.629)
      (tramontana-mono :description "perms" :start 844.730 :end 846.709)
      (tramontana-mono :description "perms" :start 846.735 :end 849.132)
      (tramontana-mono :description "perms (click?)" :start 849.177 :end
                       851.158) 
      (tramontana-mono :description "perms" :start 851.354 :end 852.844)
      (tramontana-mono :description "perms" :start 852.893 :end 854.558)
      (tramontana-mono :description "perms" :start 854.564 :end 857.118)
      (tramontana-mono :description "perms" :start 857.131 :end 860.827)
      (tramontana-mono :description "perms" :start 860.844 :end 863.476)
      ))
    (perms5
     (
      (tramontana-mono :description "perms" :start 863.487 :end 865.346)
      (tramontana-mono :description "perms" :start 865.396 :end 869.923)
      (tramontana-mono :description "perms" :start 869.943 :end 871.712)
      (tramontana-mono :description "perms" :start 871.724 :end 873.385)
      (tramontana-mono :description "perms" :start 873.391 :end 875.353)
      (tramontana-mono :description "perms" :start 875.361 :end 878.180)
      (tramontana-mono :description "perms" :start 878.359 :end 880.104)
      (tramontana-mono :description "perms" :start 880.112 :end 882.152)
      (tramontana-mono :description "perms" :start 882.169 :end 886.091)
      (tramontana-mono :description "perms" :start 886.105 :end 887.968)
      (tramontana-mono :description "perms" :start 887.972 :end 889.538)
      (tramontana-mono :description "perms" :start 889.547 :end 890.339)
      ))
    (harm-trem1
     (
      (tramontana-mono :description "c 7th harm ff trem" :start 578.662 :end
                       581.180) 
      (tramontana-mono :description "c 7th harm ff trem" :start 581.182 :end
                       583.033) 
      (tramontana-mono :description "c 7th harm ff trem" :start 583.040 :end
                       586.714) 
      (tramontana-mono :description "c 7th harm ff trem" :start 586.718 :end
                       588.610)  
      (tramontana-mono :description "c 7th harm ff trem" :start 588.617 :end
                       593.592) 
      (tramontana-mono :description "c 7th harm ff trem" :start 596.722 :end
                       600.895) 
      (tramontana-mono :description "c 7th harm ff trem" :start 600.896 :end
                       607.486) 
      (tramontana-mono :description "c 7th harm ff trem" :start 611.244 :end
                       614.013) 
      (tramontana-mono :description "c 7th harm ff trem" :start 614.014 :end
                       616.707) 
      ))
    (harm-trem2
     (     
      (tramontana-mono :description "c 7th harm ff trem" :start 616.710 :end
                       618.217) 
      (tramontana-mono :description "c 7th harm ff trem" :start 618.221 :end
                       622.214) 
      (tramontana-mono :description "c 7th harm ff trem" :start 622.219 :end
                       625.075) 
      (tramontana-mono :description "c 7th harm ff trem" :start 625.085 :end
                       626.318) 
      (tramontana-mono :description "c 7th harm ff trem" :start 626.325 :end
                       627.490) 
      (tramontana-mono :description "c 7th harm ff trem" :start 627.496 :end
                       630.438) 
      (tramontana-mono :description "c 7th harm ff trem" :start 630.446 :end
                       631.407) 
      (tramontana-mono :description "c 7th harm ff trem" :start 635.756 :end
                       637.613) 
      (tramontana-mono :description "c 7th harm ff trem" :start 637.623 :end
                       642.099) 
      (tramontana-mono :description "c 7th harm ff trem" :start 642.159 :end
                       645.853) 
      ))
    (harm-trem3
     (     
      (tramontana-mono :description "c 7th harm ff trem" :start 645.868 :end
                       650.826) 
      (tramontana-mono :description "c 7th harm ff trem" :start 650.860 :end
                       654.039) 
      (tramontana-mono :description "c 7th harm ff trem" :start 654.045 :end
                       656.627) 
      (tramontana-mono :description "c 7th harm ff trem mp beg" :start 657.701
                       :end 663.533) 
      (tramontana-mono :description "c 7th harm ff trem" :start 664.601 :end
                       675.633) 
      (tramontana-mono :description "c 7th harm ff trem" :start 675.658 :end
                       679.600) 
      (tramontana-mono :description "c 7th harm ff trem" :start 679.610 :end
                       683.153) 
      (tramontana-mono :description "c 7th harm ff trem" :start 683.163 :end
                       685.702) 
      (tramontana-mono :description "c 7th harm ff trem" :start 685.719 :end
                       693.472) 
      (tramontana-mono :description "c 7th harm ff trem" :start 693.475 :end
                       699.408) 
      ))
    (harm-trem4
     (     
      (tramontana-mono :description "c 7th harm ff trem" :start 705.232 :end
                       712.413) 
      (tramontana-mono :description "c 7th harm ff trem" :start 712.416 :end
                       717.547) 
      (tramontana-mono :description "c 7th harm ff trem" :start 717.549 :end
                       721.092) 
      (tramontana-mono :description "c 7th harm ff trem" :start 721.113 :end
                       722.925) 
      (tramontana-mono :description "c 7th harm ff trem" :start 722.930 :end
                       724.116) 
      (tramontana-mono :description "c 7th harm ff trem" :start 724.131 :end
                       725.683) 
      (tramontana-mono :description "c 7th harm ff trem" :start 727.403 :end
                       728.512) 
      (tramontana-mono :description "c 7th harm ff trem" :start 734.204 :end
                       735.063)
      (tramontana-mono :description "c 7th harm ff trem" :start 736.421 :end
                       737.736) 
      (tramontana-mono :description "c 7th harm ff trem" :start 890.417 :end
                       891.931) 
      ))
   
    (mixed-p
     (
      (tramontana-mono :description "tailpiece clb + trem" :start 289.773 :end
                       293.799)  
      (tramontana-mono :description "half flag gliss trem + arco flag gliss"
                       :start 473.622 :end 486.519) 
      (tramontana-mono :description "c 7th harms (3)" :start 490.777 :end
                       491.799) 
      (tramontana-mono :description "c 7th harm trem, half flag gliss" :start
                       507.031 :end 518.726) 

      (tramontana-mono :description "c 7th harm" :start 521.358 :end 524.072)
      (tramontana-mono :description "c 7th harm various attacks" :start 553.617
                       :end 556.022)      
      ))
    (mixed-f
     (
      (tramontana-mono :description "c 7th harm pesante scratch" :start 524.079
                       :end 531.309)  
      (tramontana-mono :description "c 7th harm 3x ff" :start 570.024 :end
                       571.038) 
      (tramontana-mono :description "tap + bridge squeaks" :start 302.106 :end
                       303.732)     
      (tramontana-mono :description "3 trichords" :start 737.956 :end 739.062)
      (tramontana-mono :description "jete ff + half flag gliss trem" :start
                       452.973 :end 457.105) 
      (tramontana-mono :description "pressing" :start 557.755 :end 561.106)
      (tramontana-mono :description "batt + half flag gliss" :start 486.531 :end
                       489.448) 
      (tramontana-mono :description "half flag gliss + cresc, 2 strings" :start
                       494.404 :end 501.935) 
      (tramontana-mono :description "trichords 2x" :start 741.116 :end 742.099)
      (tramontana-mono :description "pressing" :start 561.200 :end 566.967)
      ))
    (mix1
     (
      (tramontana-mono :description "body trem" :start 37.424 :end 38.174)
      (tramontana-mono :description "clb gliss" :start 462.679 :end 464.180)
      (tramontana-mono :description "c 7th harm ff trem" :start 593.596 :end
                       596.713) 
      (tramontana-mono :description "tailpiece resonant stroke" :start 180.881
                       :end 181.687) 
      (tramontana-mono :description "perms" :start 754.443 :end 758.967)
      (tramontana-mono :description "c 7th harm" :start 533.256 :end 535.237)
      (tramontana-mono :description "perms" :start 795.739 :end 797.205)
      ))
    )
   ("/music/tramontana"))
  
 ;; no replacements when we're generating sound files otherwise we have to make
 ;; replacements for the two computer voices too and that's too much hassle. 
 #|
 :rthm-seq-map-replacements
 '(((1 1 vla) 2 20a) ((1 1 vla) 3 1a) ((1 1 vla) 4 9a) ((1 1 vla) 5 10a) 
   ((1 1 vla) 6 empty1) ((1 1 vla) 7 16a) ((1 1 vla) 8 25a) 
   ((1 2 vla) 2 14a) ((1 2 vla) 3 21a) ((1 2 vla) 4 17a) ((1 2 vla) 5 4a) 
   ((1 2 vla) 6 9b) ((1 2 vla) 7 13a) ((1 2 vla) 8 24a) ((1 2 vla) 9 19a) 
   ((1 3 vla) 1 3a) ((1 3 vla) 2 11a) ((1 3 vla) 3 15a) ((1 3 vla) 4 1b) 
   ((1 3 vla) 5 18a) ((1 3 vla) 6 11b) ((1 3 vla) 7 16b) ((1 3 vla) 8 12a) 
   ((2 1 vla) 1 2a) ((2 1 vla) 2 23a) ((2 1 vla) 3 3b) ((2 1 vla) 4 7a) 
   ((2 1 vla) 5 13b) ((2 1 vla) 6 22a) ((2 1 vla) 7 19b) ((2 1 vla) 8 3c) 
   ((2 2 vla) 1 8a) ((2 2 vla) 2 12b) ((2 2 vla) 3 23b) ((2 2 vla) 4 14b) 
   ((2 2 vla) 5 2b) ((2 2 vla) 6 10b) ((2 2 vla) 7 15b) ((2 2 vla) 8 4b) 
   ((2 2 vla) 9 20b) ((2 3 vla) 1 25b) ((2 3 vla) 2 9c) ((2 3 vla) 3 16c) 
   ((2 3 vla) 4 5a) ((2 3 vla) 5 17b) ((2 3 vla) 6 14c) ((2 3 vla) 7 18b) 
   ((2 3 vla) 8 9d) 
   ;; ((3 1 vla) 8 empty2)              ;
   )
 |#
 :rthm-seq-palette
 '((1 ((((2 4) (s) e. 32 (32) (s) (e))
        ((e) q.)
        (+q { 3 - +te +te - (te) })
        ((q) (s) e.))
       ;;((3 4)(q) (s) e. q))
       :pitch-seq-palette (1 1 1 1)
       ;;:pitch-seq-palette (1 1 1 1 1)
       ;; :marks "a 1 2 3 6;"
       ))
   (1a ((((2 4) (s) 32 (32) (e) 32 (32) (s) (e))
         ((h))
         ;; ((q) (1 ((1 (1)) (1 (1 (1) (2))) (1 ((1))))))
         ((q) { 3 - te +t32 - (t32) (ts) (te) })
         ((h)))))
   (1b ((((2 4) (s) e. 32 (32) (s) (e))
         ((h))
         ((e) q.)
         (+q { 3 +t32 (t32) (ts) tq })
         (+q - +32 (32) e. -))))
   (2 ((((2 4) 
         ;; (1 ((1 (1)) (1 (\+1 (1) (2))) (1 ((1))))) (q))
         { 3 - te +t32 - (t32) (ts) (te) } (q))
        ((q) { 3 - te +t32 - (t32) (ts) (te) })
        ;; ((q) (1 ((1 (1)) (1 (\+1 (1) (2))) (1 ((1))))))
        ((h))
        ((q) { 3 te +tq }))
       :pitch-seq-palette ((1 1 1) (2 1 2))
       ;; :marks "s 2 4/a 6;"
       ))
   (2a ((((2 4) ;; (1 ((1 (1)) (1 (\+1 (1) (2))) (1 ((1))))) (q))
          { 3 - te +t32 - (t32) (ts) (te) } (q)) 
         ((q) { 3 - te +t32 - (t32) (ts) (te) })
         ;; ((q) (1 ((1 (1)) (1 (\+1 (1) (2))) (1 ((1))))))
         ((h))
         ((q) { 3 te +tq })
         (+h))))
   (2b ((((2 4) ;; (1 ((1 (1)) (1 (\+1 (1) (2))) (1 ((1))))) (q))
          { 3 - te +t32 - (t32) (ts) (te) } (q)) 
         ((q) { 3 te tq })
         (+h)
         (q { 3 te tq }))))
   (3 ((((2 4) { 7 - 28 x 3 (28) 28 - (14) } q)
        ((q) q)
        ((q) q)
        ((e) q.))
       :pitch-seq-palette (3 5 4 2 1 1 1 1)
       ;; :marks "t 5:8;"
       ))
   (3a ((((2 4) (q) q)
         ((h))
         ((h))
         ((q) q)
         ((q) q)
         (\+32 (32) (s) q.)
         ((5 8) +h+e))))
   (3b ((((2 4) (h))
         ((q) q)
         (+q q)
         (+e +q.))))
   (3c ((((2 4) { 7 - 28 x 3 (28) 28 - (14) } (q))
         ((h))
         ((3 8) (q.))
         ((2 4) (e) q.))))
   (4 ((((2 4) (s) e. 32 (32) (s) (e))
        ((h))
        ((q) (s) e.)
        ({ 5 - +fe f32 (f32) (fe) - } (q)))
       :pitch-seq-palette (1 1 1 1)
       ;; :marks "at 1 4/a 2 5/;"
       ))
   (4a ((((2 4) (s) e. 32 (32) (s) (e))
         ((h))
         ((q) (s) e.)
         ({ 5 +fe fh }))))
   (4b ((((2 4) (s) e. q)
         (q q)
         (q - s e. -)
         ({ 5 - +fe fe. - } +q))))
   (5 ((((2 4) (q) (s) e.)
        (+h)
        (+h)
        ({ 3 - +te te - (te) } (q)))
       ;; :marks "t 4 5;"
       :pitch-seq-palette (2 1)))
   (5a ((((2 4) q - s e. -)
         (+q +q)
         (+q +q)
         ({ 3 - +te te - (te) } (q)))))
   (6 ((((2 4) h)
        (+q { 3 +tq te })
        ({ 3 +te tq } +q)
        (+q \+32 (32) (s) (e)))
       :pitch-seq-palette (1 2 1)))
   (7 ((((2 4) (q) s (s) (e))
        ((q) s (s) (e))
        ((q) s (s) (e))
        ((s) s (e) (q)))
       ;; :marks "ts 1:4;"
       :pitch-seq-palette ((ps1 (1 1 1 1))
                           (ps2 (2 1 1 2))
                           (ps3 (1 2 1 1)))))
   (7a ((((2 4) q+32 (32) (s) (e))
         ((q) s (s) (e))
         ((q) s (s) (e))
         ((s) s (e) (q))
         ((h)))))
   (8 ((((2 4) (h))
        ((h))
        ((q) { - 3 te +te +te - })
        ((q) { - 5 fe +fs - (fe) }))
       :pitch-seq-palette (1 1)
       ;; :marks "t 1:3/a 2 3/as 5;"
       ))
   (8a ((((2 4) h)
         (+q { - 3 te te te - })
         (+q { - 5 fe fe. - }))))
   (9 ((((2 4) (h))
        ((q) { 5 - fe fe { 3 30 60 - } })
        ((q) 32 (32) (s) (e))
        ((q) { 7 - 28 x 3 (28) (14) 28 - }))
       ;; :marks "t 5;"
       :pitch-seq-palette (3 4 1 2 7 7 6 5 1)))
   (9a ((((2 4) (h))
         ((q) { 5 - fe f32 (f32) (fs) { 3 60 (60) 60 - } })
         ((q) 32 (32) (s) (e))
         ((h)))))
   (9b ((((2 4) h)
         (- s x 4 - { 5 - fe fe { 3 60 (60) 60 - } })
         ((q) 32 (32) (s) (e))
         ((3 8) (q.))
         ((2 4) (q) { 7 - 28 x 3 (28) (14) 28 - }))))
   (9c ((((2 4) (h))
         ((q) { 5 - fe fe { 3 60 (60) 60 - } })
         (+q +q)
         (q { 7 - 28 28 - 7 28 }))))
   (9d ((((2 4) (h))
         ((q) { 5 - fe fe { 3 60 (60) 60 - } })
         ((q) s (s) (e))
         ((q) { 7 - 28 x 3 (28) (14) 28 - })
         ((h)))))
   (10 ((((2 4) (q) { 5 (fe) - fe fs - })
         ({ 5 +fs fq } +q)
         (+h)
         (- +s s - (e) s (s) (e)))
        ;; :marks "a 8/as 9;"
        :pitch-seq-palette (4 7 1 1 1)))
   (10a ((((2 4) (h))
          ((h))
          ((h))
          ((s) s (e) s (s) (e)))))
   (10b ((((2 4) q { 5 - fe fe fs - })
          ({ 5 +f32 (f32) fq } q)
          (q q)
          (- s 32 (32) - (e) s 32 (32) (e)))))
   (11 ((((2 4) { 7 - 28 x 3 (28) 28 (28) 28 - } 
          { 7 - 28 28 (28) 28 (28) 28 28 - })
         ({ 7 (28) - 28 (28) 28 14\. - } { 7 - 28 28 (14) 28 (28) 28 - })
         ({ 7 (28) - 28 14 28 (28) 28 - } 
            { 7 (28) - 28 (28) 28 (28) 28 - (28) })
         ({ 7 (28) 7 28 (28) } { 7 - 28 28 (28) 14 (28) 28 - }))
        ;; :marks "a 13/t 19/a 20:24/a 28/pp 29;"
        :pitch-seq-palette (5 4 3 2 1 2 3 6 8 7 6 3 1 8 1 3 5 9 5 1 1 1 1 1 1
                              6 4 1 1 4)))
   (11a ((((2 4) { 7 - 28 x 3 (28) 28 (28) 28 - } 
           { 7 - 28 28 (28) 28 (28) 28 28 - })
          ({ 7 (28) - 28 (28) 28 14\. - } { 7 - 28 28 (14) 28 (28) 28 - })
          ((3 8) (q.))
          ((2 4) { 7 (28) - 28 14 28 (28) 28 - } 
           { 7 (28) - 28 (28) 28 (28) 28 - (28) })
          ((5 8) { 7 (28) 7 28 (28) } (e) { 7 - 28 28 (28) \14. 28 - }))))
   (11b ((((2 4) { 7 - 28 28 14 14 28 - }
           { 7 - 28 14 14 28 28 - })
          ((3 4) { 7 - 28 14 28 \14. - } (q) { 7 - 28 \14. 14 28 - })
          ((2 4) { 7 - 28 28 14 28 (28) 28 - }
           { 7 (28) - 28 (28) 28 (28) 28 - (28) })
          ((h))
          ({ 7 (28) 7 28 (28) } { 7 - 28 14 \14. 28 - }))))
   (12 ((((2 4) (h))
         ((q) { 5 - fe fe fs - })
         ((q) (s) e.)
         (+q { 7 - \+28 28 28 - (7) }))
        ;; :marks "t 3;"
        :pitch-seq-palette (3 5 1 7 6 5)))
   (12a ((((2 4) h)
          (+q { 5 - fe fs (fs) fs - })
          (+q - +32 (32) e. -)
          (+h)
          (+q { 7 - \+28 28 28 - \+7 })
          ((3 4) q q q)
          (q q q)
          (q q q))))
   (12b ((((2 4) h)
          (+q { 5 - fe fe f32 - (f32) })
          ((q) (s) e.)
          (+q { 7 - \+28 28 28 - (7) }))))
   (13 ((((2 4) (q) { 7 - 14 14 (14) 28 - })
         ({ 7 (28) 28 (28) (7) } (q))
         ((h))
         ((32) e.. +q))
        ;; :marks "t 2/s 3 4/a 5;"
        :pitch-seq-palette (3 5 1 1 1)))
   (13a ((((2 4) q { 7 - 14 14 (14) 28 - })
          ({ 7 \+56 (56) \7. } +q)
          (+q+q)
          ((32) q...))))
   (13b ((((2 4) (q) { 7 - 14 7 28 - })
          ({ 7 \+28 \7. } +q)
          (+h)
          (\+32 q...))))
   (14 ((((2 4) (q) s (s) (e))
         ((q) s (s) (e))
         ((q) { 7 - 28 28 (28) 28 (28) (14) - })
         ((q) - s \+32 - (32) (e)))
        ;; :marks "ts 1 2/s 3:5/as 7;"
        :pitch-seq-palette (1 1 6 5 4 1)))
   (14a ((((2 4) (q) s (s) (e))
          ((h))
          ((5 8) (q) s (s) (e) (e))
          ((2 4) (q) { 7 - 28 28 (28) 28 (28) (14) - })
          ((q) - s \+32 - (32) (e)))))
   (14b ((((2 4) q q)
          (q q)
          (+q { 7 - \+28 28 (28) 7 - })
          ((5 8) +q - s \+32 - (32) (e) (e)))))
   (14c ((((2 4) (q) s (s) (e))
          ((q) s (s) (e))
          ((h))
          ((q) { 7 - 28 28 (28) 28 (28) (14) - })
          ((q) - s 32 - (32) (e))
          ((h)))))
   (15 ((((2 4) (h))
         ({ 3 (tq) - t32 t32 (t32) t32 - } +q)
         (+h)
         (- { 3 +te +te (te) } - (q)))
        ;; :marks "a 1 2 7;"
        :pitch-seq-palette (1 2 5)))
   (15a ((((2 4) (h))
          ({ 3 (tq) - t32 t32 (t32) t32 - } +q)
          (+h)
          (- { 3 +te +t32 (t32) (ts) (te) } - (q))
          ((3 8) (q.)))))
   (15b ((((2 4) (h))
          ({ 3 (tq) - t32 t32 (t32) t32 - } +q)
          (+h)
          (- { 3 +te +t32 (t32) (ts) (te) } - (q)))))
   (16 ((((2 4) (q) { 3 - te x 3 - })
         (h)
         (+s (s) (e) (q))
         ((q) { 3 - te x 3 - }))
        :pitch-seq-palette (1 2 1 2 1 2 1)))
   (16a ((((2 4) (q) { 3 - t32 (t32) (ts) te  t32 (t32) (ts) - })
          ((5 8) h+e)
          ((2 4) - s x 4 - - s x 4 -)
          (- s x 4 - - s x 4 -)
          ((9 16) - e e - { 3 - te t32 (t32) (ts) te - } (s)))))
   (16b ((((2 4) (q) { 3 - te t32 (t32) (ts) te - })
          ((h))
          ((h))
          ((q) { 3 - te t32 (t32) (ts) te - }))))
   (16c ((((2 4) q { 3 - te x 3 - })
          (h)
          (+32 (32) (s) (e) (q))
          ((q) { 3 - te x 3 - }))))
   (17 ((((2 4) { 5 (fe) fe fq. })
         (- +e e - (s) e (s))
         ({ 5 (fe) - fe fe - fq })
         (+q - s e - (s)))
        ;; :marks "t 4 5/s 10;"
        :pitch-seq-palette (3 4 3 3 3 4 1 1 1)))
   (17a ((((2 4) { 5 - fe fe - fq. })
          (- +e e - (s) - e 32 - (32))
          ((h))
          ({ 5 (fe) - fe fe - fq })
          ((5 8) +q - \+32 (32) e 32 - (32) (e)))))
   (17b ((((2 4) { 5 (fe) fe fq. })
          (- +e e - (s) e (s))
          ((h))
          ({ 5 (fe) - fe fe - fq })
          (+q - +32 (32) e 32 - (32)))))
   (18 ((((2 4) - 32 (32) e. - +q)
         (+q - \+32 (32) 32 (32) - (e))
         ((h))
         (h))
        :pitch-seq-palette (1 1 1 1)
        ;; :marks "f 1/a 1/p 2/at 6/f 6/a 7/fp 7;"
        ))
   (18a ((((2 4) 32 (32) q..)
          (+q - \+32 (32) e (s) -)
          ((3 4) (h.))
          ((5 8) h+e))))
   (18b ((((2 4) - 32 (32) e. - q)
          (q - 32 (32) e. -)
          (q q)
          (h))))
   (19 ((((2 4) { 5 - (fe) fe (fe) fe fe - })
         (+h)
         (+q - +s s (s) s -)
         ((h)))
        :pitch-seq-palette (4 6 2 6 1)
        ;; :marks "t 1 2/s 7;"
        ))
   (19a ((((2 4) { 5 - fe fe (fe) fe fe - })
          (+h)
          (+q - +32 (32) s (s) s -)
          ((h)))))
   (19b ((((2 4) { 5 - (fe) fq fe fe - })
          (+h)
          (+q - +s s (s) s -)
          ((3 8) (q.)))))
   (20 ((((2 4) (h))
         ({ 3 (te) - t32 t32 (ts) t32 ts. - } +q)
         ({ 5 - +fs fs x 3 (fs) - } (q))
         ({ 5 - fs x 3 fe - } +q))
        :pitch-seq-palette (4 4 4 4 7 6 1 5 7 6 1)
        ;; :marks "a 1 2;"
        ))
   (20a ((((5 8) q+q+e)
          ((2 4) ;; (1 ((1 (\+1)) (1 (1 1 (2))) (1 (1 1 (2))))) (q))
           { 3 - +te t32 t32 (ts) t32 t32 - (ts) } (q))
          ((h))
          ((h)))))
   (20b ((((2 4) h)
          ({ 3 - te t32 t32 (ts) t32 ts. - } q)
          ((5 8) { 5 - fs x 4 (fs) - } (q) (e))
          ({ 5 - fs fs f32 (f32) fe - } +q.))))
   (21 ((((2 4) (q) { 3 - t32 t32 (t32) t32 (t32) t32 (t32) t32 (te) - })
         ((q) q)
         ({ 3 +tq (te) } (s) e.)
         (+h))
        :pitch-seq-palette (1 2 4 3 6 6 9)))
   (21a ((((2 4) (h))
          ((h))
          ((q) (s) e.)
          (+h))))
   (22 ((((2 4) { 7 - (28) 28 x 6 - } +q)
         (+q { 7 - 28 x 3 - (7) })
         (32 (32) (s) (e) (q))
         ({ 7 - 28 x 3 (14) 28 28 - } +q))
        :pitch-seq-palette (5 4 6 7 4 2 10 9 8 8 7 10 5 3 1)
        ;; :marks "ff 1/s 3:5/pp 7/ff 9/mp 12;"
        ))
   (22a ((((2 4) h)
          (+q { 7 - \+28 28 28 - (7) })
          (32 (32) (s) (e) (q))
          ((3 8) (q.)))))
   (23 ((((2 4) - 32 (32) (32) 32 (s) s - +q)
         (- \+32 (32) (32) 32 (s) s - +q)
         (- \+32 (32) e. - +q)
         (+q +s. 32 (e)))
        :pitch-seq-palette (3 3 3 3 3 3 8)
        ;; :marks "a 1 2 3 6 14/ff 1/p 4/ff 6/p 7/ff 14;"
        ))
   (23a ((((2 4) - 32 (32) (32) 32 (s) s - (q))
          ((3 8) (q.))
          ((2 4) (s) (32) - 32 (s) s - (q))
          ((3 4) (q) (s) q..)
          ((2 4) +q - +s. 32 - (e)))))
   (23b ((((2 4) (h))
          ((e) (s) s (q))
          ((s) q..)
          (q - s. 32+e - ))))
   (24 ((((2 4) (q) { 3 - te (te) te - })
         (h)
         ({ 3 +te (te) (te) } { 3 - te (te) te - })
         ((q) { 3 - (te) te te - }))
        :pitch-seq-palette (2 1 2 2 1 2 1)
        ;; :marks "s 1/t 4/s 5/at 6;"
        ))
   (24a ((((2 4) q { 3 - te (te) te - })
          (h)
          (+q { 3 - te (te) te - })
          (q { 3 - te te te - }))))
   (25 ((((2 4) { 3 { 3 - 36 x 3 - } tq } +q)
         ({ 5 - +fe fe fe - fq })
         (+h)
         (+s (s) (e) (q)))
        :pitch-seq-palette (1 2 1 2 2 2 2)
        ;; :marks "t 6:9;"
        ))
   (25a ((((2 4) { 3 { 3 - 36 x 3 - } tq } { 6 - ts x 6 - })
          ({ 5 - fe fe fe - fq })
          ((h))
          ((h)))))
   (25b ((((2 4) { 3 { 3 - 36 x 3 - } tq } q)
          ({ 5 - fe fe fe - fq })
          (+h)
          (+32 (32) (s) (e) (q)))))
   (26 ((((2 4) { 3 (te) te t32 (t32) (ts) } (q)))))
   (empty1 ((((2 4) (h))
             ((h))
             ((h))
             ((h)))))
   (empty2 ((((3 4) (h.))
             ((2 4) (h))
             ((h))
             ((h)))))
   ))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; only do this if we use the rthm-seq replacements!
;;; (load "/user/michael/mus/tramontana/src/edits.lsp")

;;; (load "~/dos/mus/tramontana/src/functions.lsp")
;;; (load "~/dos/mus/tramontana/src/globals.lsp")
;;; (load "~/dos/mus/tramontana/src/edits.lsp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF tramontana.lsp

