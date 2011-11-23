;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/set-palette
;;; NAME 
;;; set-palette
;;; 
;;; File:             set-palette.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> set-palette
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the set-palette class which extends the
;;;                   palette class by simply instantiating the sets given in
;;;                   the palette.  
;;;
;;;                   Note that the sets in this palette may refer to
;;;                   previously defined sets in order to obviate retyping note
;;;                   lists.  Hence the reference to bcl-chord2 in the
;;;                   bcl-chord3 set of the example below  will instantiate a
;;;                   set based on a transposed clone of that set previously
;;;                   stored as bcl-chord2.  
;;;
;;;                     (make-set-palette 
;;;                      'test
;;;                      '((bcl-chord1
;;;                         ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5
;;;                               dqf5 gs5 b5) 
;;;                          :subsets
;;;                          ((tc1 (ds2 e3 a4))
;;;                           (tc2 (bf1 d4 cqs5))
;;;                           (qc1 (aqf2 e3 a4 dqf5 b5))
;;;                           (qc2 (bf1 c3 gqs3 cs4 cqs5)))
;;;                          :related-sets
;;;                          ((missing (bqs0 eqs1 f5 aqs5 eqf6 fqs6 
;;;                                          bqf6 dqs7 fs7)))))
;;;                        (bcl-chord2
;;;                         ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4
;;;                           cs5 gqf5) 
;;;                          :subsets
;;;                          ((tc1 (d2 g3 cs5))
;;;                           (tc2 (eqs2 f3 bqf3))
;;;                           (qc1 (eqs2 c3 f3 fs4 gqf5))
;;;                           (qc2 (d2 fqs2 bqs3 gs4 a4)))
;;;                          :related-sets
;;;                          ((missing (aqs0 dqs1 ds5 gqs5 dqf6 eqf6 aqf6 cqs7
;;;                                          e7))))) 
;;;                        (bcl-chord3 
;;;                         (bcl-chord2 :transposition 13))))
;;;
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    August 14th 2001
;;;
;;; $$ Last modified: 17:47:30 Sat Jan 29 2011 ICT
;;;
;;; SVN ID: $Id$
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; todo: in general, recursive set palettes shouldn't have
;;; named-objects whose data are set-palettes, rather just a set
;;; palette (which has an id anyway). 
(defclass set-palette (palette)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sp set-palette) stream)
  (format stream "~%SET-PALETTE: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((sp set-palette))
  (unless (and (simple-listp (data sp))
               (sc-set-p (first (data sp))))
    (ral-to-set-palette sp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sp set-palette))
  (clone-with-new-class sp 'set-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; todo: there's a bug here that tries to put all sets on one page!
#+cmn
(defmethod cmn-display ((sp set-palette)
                        &key
                        ;; 10.3.10: display on 4 staves (treble+15 bass-15)?
                        (4stave nil)
                        (file "/tmp/cmn.eps")
                        (text-x-offset -0.5)
                        (text-y-offset nil)
                        (font-size 10.0)
                        (break-line-each-set t)
                        (line-separation 3)
                        (staff-separation nil)
                        (transposition nil) ;; in semitones
                        (size 20)
                        (use-octave-signs nil)
                        (automatic-octave-signs nil)
                        (include-missing-chromatic t)
                        (include-missing-non-chromatic t))
  ;; some defaults above are good for 2-staff display but not 4...
  (unless text-y-offset
      (setf text-y-offset (if 4stave 1.9 2.1)))
  (unless staff-separation
      (setf staff-separation (if 4stave 1.5 3.0)))
  (let* ((aux (cmn-display-aux sp 4stave text-x-offset text-y-offset
                               break-line-each-set font-size
                               include-missing-chromatic
                               include-missing-non-chromatic transposition
                               use-octave-signs))
         (aux2 (loop for set in aux 
                     appending (first set) into treble
                     appending (second set) into bass
                     appending (third set) into quad-treble
                     appending (fourth set) into quad-bass
                     finally (return (list treble bass quad-treble 
                                           quad-bass)))))
    (cmn::cmn-display 
     (if 4stave 
         (cmn::cmn-treble-bass-quad-system (first aux2) (second aux2)
                                           (third aux2) (fourth aux2))
         (cmn::cmn-treble-bass-system (first aux2) (second aux2)))
     :file file :size size :line-separation line-separation
     :staff-separation staff-separation
     :automatic-octave-signs automatic-octave-signs
     :automatic-line-breaks (not break-line-each-set))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod cmn-display-aux ((sp set-palette) 
                            &optional 
                            (4stave nil)
                            (text-x-offset -0.5)
                            (text-y-offset 2.0)
                            (break-line-each-set t)
                            (font-size 10.0)
                            include-missing-chromatic
                            include-missing-non-chromatic
                            transposition
                            (use-octave-signs t)
                            ;; leave parents alone, used recursively 
                            parents)
  ;; (print parents)
  (loop for i below (sclist-length sp)
     for current = (get-nth i sp)
     if (typep (data current) 'set-palette)
     ;; keep track of the levels of recursion in the set-palette
     do (push (id current) parents)
     and append (cmn-display-aux (data current) 4stave text-x-offset 
                                 text-y-offset
                                 break-line-each-set font-size
                                 include-missing-chromatic
                                 include-missing-non-chromatic
                                 transposition use-octave-signs 
                                 parents)
     into result
     and do (pop parents)
     ;; cmn-treble-bass-system is part of the complete-set class
     ;; returns a list: treble-clef notes, bass-clef notes
     else collect (cmn-treble-bass-system 
                   (if transposition
                       (transpose (clone current) transposition)
                       current)
                   4stave
                   (make-sp-name parents
                                 (id current)
                                 (tag current))
                   text-x-offset text-y-offset
                   break-line-each-set
                   font-size
                   include-missing-chromatic
                   include-missing-non-chromatic
                   use-octave-signs)
     into result
     ;; todo: 10.3.10: what's this here? is it or isn't it? I think it's
     ;; confused by the recursive aspect: could be a sub set-palette in there.
     ;; This is a mistake correction so not pretty: result will not be a
     ;; list of lists, each sublist containing a list of treble and a
     ;; list of bass notes.
     finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* set-palette/find-sets-with-pitches
;;; FUNCTION
;;; find-sets-with-pitches
;;; return a list of sets from a a set-palette that have specific pitches in
;;; them.  NB Only those sets which contain all the given pitches will match.
;;; 
;;; ARGUMENTS
;;; - the set-palette
;;; - a list of pitches (either pitch objects or note symbols)
;;; - (optional) whether to print set notes as we examine them
;;; 
;;; RETURN VALUE
;;; a list of sets from the set palette
;;; 
;;; EXAMPLE
;;; (find-sets-with-pitches (set-palette +cheat-sheet+) '(d3 fs3 a3) t) 
;;;
;;; SYNOPSIS
(defmethod find-sets-with-pitches ((sp set-palette) pitches &optional print)
;;; ****
  (loop 
      for ref in (get-all-refs sp)
      for set = (get-data ref sp)
      with result = '()
      do
        (when (contains-pitches set pitches)
          (when print
            (format t "~&~a: ~a" 
                    (this set) (pitch-list-to-symbols (data set))))
          (push set result))
      finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* set-palette/gen-max-coll-file
;;; FUNCTION
;;; gen-max-coll-file: write a text file suitable for reading into MaxMSP's
;;; coll object. The text file has a line for each set in the palette; the coll
;;; index is the ID of the set; the rest of the line is a list of
;;; frequency/amplitude pairs (or MIDI note numbers if required).
;;; 
;;; CREATION DATE
;;; 26/12/09
;;; 
;;; ARGUMENTS
;;; - the set-palette
;;; - the .txt file to write
;;; - If <midi> (optional) then midi note numbers will be generated, otherwise
;;;   it will be frequencies.
;;; 
;;; RETURN VALUE
;;; 
;;; EXAMPLE
;;; 
;;; SYNOPSIS
(defmethod gen-max-coll-file ((sp set-palette) file &optional midi)
;;; ****
  (with-open-file
      (stream file
              :direction :output :if-exists :overwrite 
              :if-does-not-exist :create)
    (gen-max-coll-file-aux sp stream midi 0)))

(defmethod gen-max-coll-file-aux ((sp set-palette) stream midi index)
  (reset sp)
  (loop with data 
     for s = (get-next sp)
     for i below (sclist-length sp) 
     do
     (if (set-palette-p (data s))
         (incf index (1- (gen-max-coll-file-aux (data s) stream midi 
                                                (+ index i))))
         (progn
           (setf data (if midi 
                          ;; nslider expects note/velocity pairs
                          (loop for n in (get-midi s) collect n collect 80)
                          ;; ioscbank~ in max expects freq/amp pairs
                          (loop for f in (get-freqs s) 
                             collect f collect 0.1)))
           (when data
             (format stream "~&~a,~{ ~,3f~^~};" ;;(+ 1 index i) 
                     (combine-into-symbol (id sp) "-" (id s))
                     data))))
     finally (return i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* set-palette/gen-midi-chord-seq
;;; FUNCTION
;;; gen-midi-chord-seq:
;;; write a midi file with each chord in the palette played at 1 second
;;; intervals. 
;;; 
;;; ARGUMENTS:
;;; - the set-palette
;;; - the path for the midi file
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
(defmethod gen-midi-chord-seq ((sp set-palette) midi-file)
;;; ****
  (let ((events (gen-midi-chord-seq-aux sp 0)))
    (cm::process-voices (list (list events))
                        midi-file (make-tempo 60) nil 0)
    t))

(defmethod gen-midi-chord-seq-aux ((sp set-palette) time)
  (reset sp)
  (loop with rthm = (make-rhythm 'q)
     for i below (sclist-length sp) 
     for set = (get-next sp)
     if (set-palette-p (data set))
     append 
     (multiple-value-bind
           (events sub-time)
         (gen-midi-chord-seq-aux (data set) time)
       (incf time sub-time)
       events)
     into result
     else collect (create-event set rthm time time)
     into result
     and do (incf time)
     finally (return (values result i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* set-palette/force-micro-tone
;;; FUNCTION
;;; force-micro-tone:
;;; change the micro-tone slot of all pitches in all the sets to <value>.
;;; 
;;; ARGUMENTS:
;;; - a set-palette 
;;; - t or nil
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
(defmethod force-micro-tone ((sp set-palette) &optional value)
;;; ****
  (loop for set in (data sp) do
       (cond ((or (set-palette-p set) 
                  (sc-set-p set))
              (force-micro-tone set value))
             ((set-palette-p (data set))
              (force-micro-tone (data set) value))
             (t (error "set-palette::force-micro-tone: can't operate on ~a"
                       set))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-sp-name (parents current tag)
  ;; NB parents is the wrong way round
  (setf parents (reverse parents))
  (format nil "~{~a.~}~a   ~a" parents current (if tag tag "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* set-palette/make-set-palette
;;; FUNCTION
;;; make-set-palette:
;;; create a set-palette object.
;;; 
;;; ARGUMENTS:
;;; - the id
;;; - the data
;;; - recurse-simple-data: whether to interpret two-element data lists as
;;;   recursive palettes
;;; - warn-note-found: whether to print warnings when data is not found with
;;;   subsequent calls to get-data.
;;; 
;;; RETURN VALUE: 
;;; a set-palette
;;; 
;;; EXAMPLE
;;; (make-set-palette 
;;;        'test
;;;        '((1 ((1
;;;               ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5 dqf5 gs5 b5)
;;;                :subsets
;;;                ((tc1 ((ds2 e3 a4) "a-tag"))
;;;                 (tc2 (bf1 d4 cqs5))
;;;                 (tc3 (c3 cs4 gs5))
;;;                 (tc4 (gqf3 g4 dqf5))
;;;                 (tc5 (aqf2 gqs3 b5))
;;;                 (qc1 (aqf2 e3 a4 dqf5 b5))
;;;                 (qc2 (ef2 gqf3 d4 g4 gs5))
;;;                 (qc3 (bf1 c3 gqs3 cs4 cqs5)))))
;;;              (2
;;;               ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5 gqf5)
;;;                :subsets
;;;                ((tc1 (d2 g3 cs5))
;;;                 (tc2 (eqs2 f3 bqf3))
;;;                 (tc3 (b2 bqs3 gqf5))
;;;                 (tc4 (fqs2 fs4 gs4))
;;;                 (tc5 (bf1 c3 a4))
;;;                 (qc1 (eqs2 c3 f3 fs4 gqf5))
;;;                 (qc2 (bf1 b2 g3 bqs3 cs5))
;;;                 (qc3 (d2 fqs2 bqs3 gs4 a4)))))
;;;              (3
;;;               ((cqs2 fs2 g2 c3 d3 fqs3 gqf3 cs4 ds4 e4 gs4 dqf5 f5 a5 bqs5)
;;;                :subsets
;;;                ((tc1 (cqs2 c3 f5))
;;;                 (tc2 (fs2 e4 bqs5))
;;;                 (tc3 (d3 ef4 a5))
;;;                 (tc4 (gqf3 af4 dqf5))
;;;                 (tc5 (g2 fqs3 cs4))
;;;                 (qc1 (c3 gqf3 ds4 gs4 dqf5))
;;;                 (qc2 (fs2 g2 fqs3 e4 a5))
;;;                 (qc3 (cqs2 d3 cs4 f5 bqs5)))))
;;;              (4
;;;               ((bf1 c2 eqf2 fqf2 b2 cs3 d3 fs3 bqs3 ef4 g4 aqs4 bqf4 e5 f5)
;;;                :subsets
;;;                ((tc1 (bf1 bqs3 e5))
;;;                 (tc2 (eqs2 fs3 aqs4))
;;;                 (tc3 (c2 d3 bqf4))
;;;                 (tc4 (b2 ef4 f5))
;;;                 (tc5 (eqf2 cs3 g4))
;;;                 (qc1 (eqf2 b2 ef4 aqs4 e5))
;;;                 (qc2 (c2 eqs2 d3 fs3 bqf4))
;;;                 (qc3 (bf1 cs3 bqs3 g4 f5)))))
;;;              (5
;;;               ((aqs1 e2 fs2 g2 b2 fqf3 af3 c4 dqs4 eqf4 a4 bf4 ef5 f5 aqf5)
;;;                :subsets
;;;                ((tc1 (e2 fqf3 a4))
;;;                 (tc2 (fs2 bf4 aqf5))
;;;                 (tc3 (g2 af3 ef5))
;;;                 (tc4 (aqs1 eqf4 f5))
;;;                 (tc5 (b2 c4 dqs4))
;;;                 (qc1 (e2 fqf3 dqs4 a4 f5))
;;;                 (qc2 (fs2 c4 eqf4 bf4 aqf5))
;;;                 (qc3 (aqs1 g2 b2 af3 ef5)))))))
;;;          (2 ((1 ((1 1) :transposition 5))
;;;              (2 ((1 2) :transposition 5))
;;;              (3 ((1 3) :transposition 5))
;;;              (4 ((1 4) :transposition 5))
;;;              (5 ((1 5) :transposition 5))))
;;;          (3 ((1 ((1 1) :transposition -2))
;;;              (2 ((1 2) :transposition -2))
;;;              (3 ((1 3) :transposition -2))
;;;              (4 ((1 4) :transposition -2))
;;;              (5 ((1 5) :transposition -2))))
;;;          (4 ((1 ((1 1) :transposition 3))
;;;              (2 ((1 2) :transposition 3))
;;;              (3 ((1 3) :transposition 3))
;;;              (4 ((1 4) :transposition 3))
;;;              (5 ((1 5) :transposition 3))))
;;;          (5 ((1 ((1 1) :transposition -4))
;;;              (2 ((1 2) :transposition -4))
;;;              (3 ((1 3) :transposition -4))
;;;              (4 ((1 4) :transposition -4))
;;;              (5 ((1 5) :transposition -4))))))
;;;
;;; NB A simple list of sets (with unique id slots) can also be passed.
;;; 
;;; SYNOPSIS
(defun make-set-palette (id palette 
                         &key (recurse-simple-data t) (warn-not-found t))
;;; ****
  (make-instance 'set-palette 
    :id id :data palette :recurse-simple-data recurse-simple-data
    :warn-not-found warn-not-found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ral-to-set-palette (ral &optional god)
  (when (data ral)
    ;; we need to be able to reference the outermost ral when this function is
    ;; called recursively.  This is not the parent, rather the very first ral,
    ;; hence it's called god.
    (unless god
      (setf god ral))
    (loop for i in (data ral) and j from 0 do
         (if (is-ral (data i))
             (setf (data (nth j (data ral)))
                   (ral-to-set-palette (data i) god))
             (unless (sc-set-p i)
               (let* ((id (id i))
                      (data (data i))
                      (set (first data))
                      (set-object (and (or (assoc-list-id-p set)
                                           (assoc-list-id-list set))
                                       (get-data set god nil))))
                 (setf (nth j (data ral))
                       ;; a reference to a previously defined set was given
                       (if set-object
                           (apply #'make-complete-set 
                                  ;; we allow the initialization of a set from
                                  ;; a previous one just given in this palette!
                                  ;; In order to allow this, we have to now do
                                  ;; a lookup of the set-id in the assoc-list
                                  ;; (palette) we're currently processing!
                                  (append (list set-object :id id)
                                          (rest data)))
                           ;; this is a new set with a list of notes.
                           (apply #'make-complete-set 
                                  (append (list set :id id)
                                          (rest data)))))))))
    (sc-change-class ral 'set-palette)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* set-palette/set-palette-p
;;; FUNCTION
;;; set-palette-p:
;;; test whether an object is a set-palette
;;; 
;;; ARGUMENTS:
;;; a lisp object
;;; 
;;; RETURN VALUE: 
;;; t or nil
;;; SYNOPSIS
(defun set-palette-p (thing)
;;; ****
  (typep thing 'set-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Ring Modulation routines
;;; todo: make the sp's proper linked-named-objects so we can (get-all-refs)
;;; with a list of notes
;;; 
;;; ****f* set-palette/recursive-set-palette-from-ring-mod
;;; FUNCTION
;;; recursive-set-palette-from-ring-mod:
;;; Create a set-palette containing sub palettes based on ring modulation
;;; routines applied to the given notes.
;;; 
;;; ARGUMENTS:
;;; - a list of notes, each of which we'll make a sub-set-palette from using
;;;   set-palette-from-ring-mod 
;;; - the id for the top-level set-palette; ids of sub-set-palettes are the
;;;   notes and then numbers
;;; - (key :partials) the partials are used to ring modulate frequencies from
;;;   reference-note but also from the bass note that would have reference-note
;;;   as the 7th partial (or whatever is the highest in this list)
;;; - (key :warn-no-bass) issue a warning when ring-mod-bass fails to find
;;;    suitable bass notes for the generated sets.
;;; - (key :do-bass) whether to add notes created by the ring-mod-bass function
;;; 
;;; RETURN VALUE: the recursive set-palette
;;; EXAMPLE
;;;       (recursive-set-palette-from-ring-mod '(a4 cs5 g5 b5) 
;;;       'altogether '(5 6 7 9)))
;;; SYNOPSIS
(defun recursive-set-palette-from-ring-mod (reference-notes id &key
                                            (warn-no-bass t)
                                            (ring-mod-bass-octave 0)
                                            (do-bass t)
                                            remove-octaves
                                            (min-bass-notes 1)
                                            (partials '(1 3 5 7)))
;;; ****
  (unless (listp reference-notes)
    (error "set-palette::recursive-set-palette-from-ring-mod: need a list ~
            of notes: ~a" reference-notes))
  (let ((sp (make-set-palette id nil)))
    (loop for rn in reference-notes do
         (add 
          (make-named-object 
           rn (set-palette-from-ring-mod 
               rn rn :partials partials :do-bass do-bass 
               :min-bass-notes min-bass-notes
               :remove-octaves remove-octaves
               :ring-mod-bass-octave ring-mod-bass-octave
               :warn-no-bass warn-no-bass))
          sp))
    sp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* set-palette/set-palette-from-ring-mod
;;; FUNCTION
;;; set-palette-from-ring-mod
;;; create a set-palette by applying ring modulation procedures (difference and
;;; sum tones of partials).
;;; 
;;; ARGUMENTS
;;; - the starting note from which we do ring-modulation
;;; - the id for the palette
;;; - (key :partials) we ring mod partials 1,3,4,7 by default, starting from
;;;    reference-note but also from the bass note that would have reference-note
;;;    as the 7th partial (or the highest in the given list).
;;; - (key :warn-no-bass) issue a warning when ring-mod-bass fails to find
;;;    suitable bass notes for the generated sets.
;;; - (key :do-bass) whether to add notes created by the ring-mod-bass function
;;; 
;;; RETURN VALUE
;;; a set-palette
;;; EXAMPLE
;;; (set-palette-from-ring-mod 'a4 'altogether '(5 6 7 9))
;;; SYNOPSIS
(defun set-palette-from-ring-mod (reference-note id &key
                                  (warn-no-bass t)
                                  (do-bass t)
                                  remove-octaves
                                  (min-bass-notes 1)
                                  (ring-mod-bass-octave 0)
                                  (partials '(1 3 5 7)))
;;; ****
  (let* ((freq (note-to-freq reference-note))
         (highest-partial (loop for p in partials maximize p))
         ;; find the low note that would have reference-note as it's
         ;; highest-partial (according to our partials list)  
         (fundamental (/ freq highest-partial))
         ;; get all the partials of the fundamental and the reference-note
         (freqs
          (loop for p in partials
             collect (* p fundamental)
             when (> p 1) ;; so as to avoid duplication of reference-note
             collect (* p freq)))
         ;; all possible permutations of our frequencies in pairs
         (pairs (get-all-pairs freqs))
         (sp (make-set-palette id nil)))
    ;; so now we'll ring modulate each possible pair
    (loop for pair in pairs
       for left = (first pair) for right = (second pair)
       for rm = (ring-mod left right 
                          :print nil :return-notes nil
                          :remove-octaves remove-octaves
                          :min-freq (note-to-freq 'a0)
                          :max-freq (note-to-freq 'c8))
       ;; find bass notes too as we're often top heavy
       for rm-bass = (when do-bass 
                       (ring-mod-bass 
                        pair 
                        :bass-octave ring-mod-bass-octave
                        :warn warn-no-bass))
       for i from 1
       with set
       do 
       ;; if we can't get a bass from the pair, try it with the whole freq
       ;; set from the ring-modulation
       (when (and do-bass (< (length rm-bass) min-bass-notes))
         (let ((rmb (ring-mod-bass 
                     rm :bass-octave ring-mod-bass-octave :warn warn-no-bass)))
           (when (> (length rmb) (length rm-bass))
             (setf rm-bass rmb)))
         (when (and warn-no-bass
                    (< (length rm-bass) min-bass-notes))
           (warn "set-palette::set-palette-from-ring-mod: can't get bass ~
                  notes even after 2nd attempt with ~a" rm)))
       (setf rm-bass (when rm-bass
                       ;; max three bass notes
                       (list (first rm-bass)
                             (nth (floor (length rm-bass) 2) rm-bass)
                             (first (last rm-bass))))
             set (remove-duplicates (append rm rm-bass)))
       (add (make-complete-set set :id i :subsets `((rm-bass ,rm-bass))
                               :tag (combine-into-symbol 
                                     (freq-to-note left) '-ringmod- 
                                     (freq-to-note right)))
            sp))
    sp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* set-palette/ring-mod
;;; FUNCTION
;;; ring-mod
;;; ring modulate two frequencies and harmonic partials thereof
;;; 
;;; ARGUMENTS
;;; - the first pitch (hertz or note symbol)
;;; - the second pitch (hertz or note symbol). Needn't be higher than first.
;;; - (return-notes nil) 
;;; - (key :pitch1-partials default 3): how many harmonic partials of pitch1 to
;;; include in the modulation.
;;; - (key :pitch2-partials default 2): how many harmonic partials of pitch2 to
;;; include in the modulation.
;;; - (key :min-freq default 20): the minimum frequency (hertz) that we'll
;;; return. 
;;; - (key :max-freq default 20000): the maximum frequency (hertz) that we'll
;;; return. 
;;; - (key :round default t): if returning frequencies, round them to the
;;; nearest hertz? 
;;; - (key :remove-duplicates default t): don't allow any returned
;;; frequencies/notes to be repeated.
;;; - (key :print default nil): print data as we generate it
;;; - (key :scale default cm::*scale* i.e. the default common music scale,
;;; usually *chromatic-scale*): which scale to use when converting frequencies
;;; to notes
;;; - (key :remove-octaves default nil): if t, octaves will be removed from the
;;; result; can also be a number or list of numbers: these will be the octaves
;;; we allow, the rest being allowed (e.g. :remove-octaves '(1 2) will remove
;;; all octaves except for single and double so '(c1 c2 c3 c4 c5) would return
;;; '(c1 c2 c3) (c4 and c5 are removed as being octaves of c1)
;;; 
;;; RETURN VALUE
;;; a list of note symbols or frequencies
;;; SYNOPSIS
(defun ring-mod (pitch1 pitch2 ;; hertz or notes
                 &key (return-notes nil) (pitch1-partials 3) (pitch2-partials 2)
                 (min-freq 20) (max-freq 20000) (round t) (remove-duplicates t)
                 (print nil) remove-octaves (scale cm::*scale*))
;;; ****
  (unless (numberp pitch1)
    (setf pitch1 (note-to-freq pitch1)))
  (unless (numberp pitch2)
    (setf pitch2 (note-to-freq pitch2)))
  (let* ((pitch1p (cons pitch1 (loop for i from 2 to pitch1-partials collect 
                                  (* pitch1 i))))
         (pitch2p (cons pitch2 (loop for i from 2 to pitch2-partials collect
                                  (* pitch2 i))))
         (rmod 
          (loop for fl in pitch1p appending
               (loop for fu in pitch2p 
                  collect (+ fl fu) 
                  collect (abs (- fu fl)))))
         (notes '()))
    (setf rmod (remove-if #'(lambda (x) (or (< x min-freq)
                                            (> x max-freq)))
                          rmod)
          rmod (sort rmod #'<)
          notes (loop for f in rmod 
                   for note = (freq-to-note f scale)
                   do
                     (when print
                       (format t "~&~a: ~a ~a" f note (freq-to-degree f t)))
                   collect note))
    (when (and return-notes remove-duplicates)
      (setf notes (remove-duplicates notes)))
    (when (and (not return-notes) round)
      (setf rmod (loop for f in rmod collect (round f))))
    ;; gotta do this here now/if we've rounded
    (when (and (not return-notes) remove-duplicates)
      (setf rmod (remove-duplicates rmod)))
    (when remove-octaves
      (if return-notes 
          (setf notes (remove-octaves
                       notes
                      ;; even if remove-octaves is t passing it as :allow won't
                      ;; cause a problem
                      :as-symbol t :allow remove-octaves))
          (setf rmod (remove-octaves rmod :as-symbol nil 
                                     :allow remove-octaves))))
    (if return-notes
        notes
        rmod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* set-palette/ring-mod-bass
;;; FUNCTION
;;; ring-mod-bass
;;; Invent sensible bass note(s) from a list of frequencies.
;;;
;;; RETURN VALUE
;;; a list of frequencies or note symbols
;;;
;;; SYNOPSIS
(defun ring-mod-bass (freqs &key (bass-octave 0) (low 'a0) (high 'g3)
                      (warn t) (return-notes nil))
;;; ****
  (unless (numberp low)
    (setf low (note-to-freq low)))
  (unless (numberp high)
    (setf high (note-to-freq high)))
  (flet ((octave-of-freqs (freq) ;; is freq in <freqs> with an 8ve shift?
           (loop for f in freqs 
              when (octave-freqs freq f t)
              do
                (return t))))
    (let* ((pairs (get-all-pairs freqs))
           (result 
            (loop for pair in pairs
               for left = (first pair)
               for right = (second pair)
               for diff = (abs (- left right))
               for bass = 
               (loop for i from 1.0 to 100 for freq = (/ diff i) do
                    (when (and (not (< freq low))
                               (not (> freq high))
                               (in-octave freq bass-octave)
                               (not (octave-of-freqs freq)))
                      (return freq)))
               when bass collect bass)))
      (if result
          (progn
            (setf result (sort result #'<))
            (when return-notes
              (setf result (loop for f in result collect (freq-to-note f))))
            (remove-duplicates result))
          (when warn
            (warn "set-palette::ring-mod-bass: can't get bass from ~a!"
                  freqs))))))


(defun ring-mod-bass-old (freqs &key (bass-octave 3) (low 'a0) (high 'g3)
                      (warn t) (return-notes nil))
;;; ****
  (unless (numberp low)
    (setf low (note-to-freq low)))
  (unless (numberp high)
    (setf high (note-to-freq high)))
  (let ((pairs (get-all-pairs freqs))
        (result '()))
    (labels ((octave-of-freqs (freq) ;; is freq in <freqs> with an 8ve shift?
               (loop for f in freqs 
                  when (octave-freqs freq f t)
                  do
                  ;; (format t "~%~a has octave in ~a" freq freqs)
                  (return t)))
             ;; our main algorithm: can try with different freq multipliers in
             ;; case we don't get a bass note the first time
             ;; multiplier acts on the second of each pair
             (try (multiplier) 
               (loop for pair in pairs
                  for left = (first pair)
                  for right = (* multiplier (second pair))
                  ;; take our notes down to the bass octave so that
                  ;; subtracting them will give frequencies in the bass register
                  for left-pitch = (transpose-to-octave (make-pitch left) 
                                                        bass-octave)
                  for right-pitch = (unless (> right 20000)
                                      (transpose-to-octave (make-pitch right) 
                                                           bass-octave))
                  for diff = 
                    (when right-pitch
                      ;; just the difference tones here, not the sums (we're
                      ;; after bass after all).  we do this in a loop so as to
                      ;; get in the audible frequency range.
                      (loop with d = (abs (- (frequency left-pitch) 
                                             (frequency right-pitch)))
                         do
                         (cond ((zerop d) (return nil))
                               ((>= d 20) (return d))
                               ;; try up an octave
                               (t (setf d (* 2 d))))))
                  for new = (when diff (freq-to-note diff))
                  for new-pitch = (when new (make-pitch new))
                  when (and new 
                            ;; don't go above or below our user-specified note
                            ;; extremes. 
                            (<= diff high)
                            (>= diff low)
                            ;; don't get notes that are just 8ve shifts of
                            ;; existing notes
                            (not (octave-of-freqs diff))
                            ;; don't just get existing notes or their octaves
                            ;; (are we duplicating above here? (works))
                            (not (pitch= new-pitch left-pitch))
                            (not (pitch= new-pitch right-pitch))
                            (not (is-octave new-pitch left-pitch))
                            (not (is-octave new-pitch right-pitch)))
                  collect new-pitch into bass
                  finally (return (remove-duplicates bass)))))
      (setf result (try 1))
      (unless result
        (setf result (try 3)))
      (unless result
        (setf result (try 5)))
      (when (and warn (not result))
        (warn "ring-mod-bass: couldn't get bass for ~a" freqs))
      (setf result 
            (loop for n in result collect
                 (if return-notes
                     (data n)
                     (frequency n))))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF set-palette.lsp
