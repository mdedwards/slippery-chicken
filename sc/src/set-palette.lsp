;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* palette/set-palette
;;; NAME 
;;; set-palette
;;; 
;;; File:             set-palette.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> set-palette
;;;
;;; Version:          1.0.5
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
;;;                   bcl-chord3 set of the example below will instantiate a
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
;;; $$ Last modified: 12:21:17 Mon Jun 22 2015 BST
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

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; SAR Tue Feb  7 12:12:24 GMT 2012: Added robodoc entry

#+cmn
;;; ****m* set-palette/cmn-display
;;; DESCRIPTION
;;; Generate printable music notation output (.EPS) of the given set-palette
;;; object, including separate notation of the SUBSETS and RELATED-SETS slots,
;;; using the Common Music Notation (CMN) interface. The method requires at
;;; least the name of the given set-palette object, but has several additional
;;; optional arguments for customizing output.
;;;
;;; NB: Some of the keyword arguments are CMN attributes and share the same
;;;     name as the CMN feature they effect.
;;; 
;;; ARGUMENTS
;;; - A set-palette object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :file. The file path, including the file name, of the file to be
;;;   generated.
;;; - :4stave. T or NIL to indicate whether the note-heads of the output should
;;;   be printed on 4 staves (or 2). T = 4. Default = NIL.
;;; - :text-x-offset. Number (positive or negative) to indicate the horizontal
;;;   offset of any text in the output. A value of 0.0 results in all text
;;;   being lined up left-flush with the note-heads below it. Units here and
;;;   below are relative to CMN staff size. Default = -0.5. 
;;; - :text-y-offset. Number (positive or negative) to indicate the vertical
;;;   offset of any text in the output.
;;; - :font-size. A number indicating the size of any text font used in the
;;;   output. This affects text only and not the music (see :size below for
;;;   changing the size of the music).
;;; - :break-line-each-set. T or NIL to indicate whether each set-palette
;;;   object should be printed on a separate staff or consecutively on the same
;;;   staff. T = one staff per set-palette object. Default = T.
;;; - :line-separation. A number to indicate the amount of white space between
;;;   lines of music (systems), measured as a factor of the staff
;;;   height. Default = 3. This is a direct CMN attribute.
;;; - :staff-separation. A number to indicate the amount of white space between
;;;   staves belong to the same system, measured as a factor of the staff
;;;   height. Default = 3. This is a direct CMN attribute.
;;; - :transposition. Nil or a number (positive or negative) to indicate the
;;;   number of semitones by which the pitches of the given set-palette object
;;;   should be transposed before generating the CMN output. Default = NIL (0).
;;; - :size. A number to indicate the size of the music-font in the CMN
;;;   output. This affects music only, not text.
;;; - :use-octave-signs. T or NIL to indicate whether to automatically insert
;;;   ottava spanners. Automatic placement depends on the overall pitch
;;;   content. This is a slippery-chicken process and may produce different
;;;   results than :automatic-octave-signs, which is a direct CMN process. 
;;;   T = insert octave signs. Default = NIL.
;;; - :automatic-octave-signs. T or NIL to indicate whether to automatically
;;;   insert ottava spanners. Automatic placement depends on the overall pitch
;;;   content. This is a direct CMN process and may produce different results
;;;   than :use-octave-signs, which is a slippery-chicken process. T = insert
;;;   octave signs. Default = NIL.
;;; - :include-missing-chromatic. T or NIL to indicate whether to also print
;;;   any chromatic pitches from the complete-set that are not present in the
;;;   given set-palette object. T = print. Default = T.
;;; - :include-missing-non-chromatic. T or NIL to indicate whether to also
;;;   print any microtonal pitches from the complete-set that are not
;;;   present in the given set-palette object. T = print.  Default = T.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
;; A typical example with some specified keyword values for file, font-size,
;; break-line-each-set, size, include-missing-chromatic and
;; include-missing-non-chromatic 
(let ((msp (make-set-palette 
            'test
            '((1 ((1
                   ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)))
                  (2
                   ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)
                    :subsets
                    ((tc1 (d2 g3 cs5))
                     (tc2 (eqs2 f3 bqf3))
                     (tc3 (b2 bqs3 gqf5)))))))
              (2 ((1 ((1 1) :transposition 5))
                  (2 ((1 2) :transposition 5))))
              (3 ((1 ((1 1) :transposition -2))
                  (2 ((1 2) :transposition -2))))))))
  (cmn-display msp
               :file "/tmp/sp-output.eps"
               :font-size 8
               :break-line-each-set nil
               :size 10
               :include-missing-chromatic nil
               :include-missing-non-chromatic nil))

|#
;;; SYNOPSIS
(defmethod cmn-display ((sp set-palette)
                        &key
                        ;; 10.3.10: display on 4 staves (treble+15 bass-15)?
                        (4stave nil)
                        (file 
                         (format nil "~a~a.eps" 
                                 (get-sc-config 'default-dir) 
                                 (string-downcase (id sp))))
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
                        (auto-open (get-sc-config 'cmn-display-auto-open+))
                        (include-missing-non-chromatic t))
;;; ****
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
     :automatic-line-breaks (not break-line-each-set))
    (when auto-open
      (system-open-file file))
    t))
   
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
     finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Feb  7 13:49:36 GMT 2012: Slightly modified MDE's comments
;;; SAR Tue Feb  7 13:47:11 GMT 2012: Added robodoc entry

;;; ****m* set-palette/find-sets-with-pitches
;;; DESCRIPTION
;;; Return a list of sets (as complete-set objects) from a given set-palette
;;; object based on whether they contain specified pitches.
;;;
;;; NB: Only sets which contain all of the specified pitches will be returned. 
;;;
;;; ARGUMENTS
;;; - A set-palette object.
;;; - A list of pitches, either as pitch objects or note-name symbols.
;;;
;;; OPTION ARGUMENTS
;;; - T or NIL to indicate whether to print the notes of each successful set as
;;;   they are being examined.
;;; 
;;; RETURN VALUE
;;; A list of complete-set objects.
;;; 
;;; EXAMPLE
#|
;; Find sets that contain a single pitch
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
  (find-sets-with-pitches msp '(c4)))

=>
(
COMPLETE-SET: complete: NIL
[...]
data: (BF3 C4 D4 F4)
[...]
COMPLETE-SET: complete: NIL
[...]
data: (C4 F4 A4 C5)
[...]
COMPLETE-SET: complete: NIL
[...]
data: (C4 D4 E4 G4)
[...]
COMPLETE-SET: complete: NIL
[...]
data: (G3 C4 E4 G4)
)

;; Search for a set of two pitches, printing the successfully matched sets
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
  (print (find-sets-with-pitches msp '(c4 f4) t)))

=>
(2 1): (C4 F4 A4 C5)
(3 2): (BF3 C4 D4 F4)
(
COMPLETE-SET: complete: NIL
[...]
data: (BF3 C4 D4 F4)
COMPLETE-SET: complete: NIL
[...]
data: (C4 F4 A4 C5)
)

|#
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

;;; SAR Tue Feb  7 14:22:54 GMT 2012: Added robodoc entry

;;; SAR Tue Feb  7 14:19:26 GMT 2012: Edited robodoc entry

;;; ****m* set-palette/gen-max-coll-file
;;; DATE
;;; 26-Dec-2009
;;; 
;;; DESCRIPTION
;;; Write a text file from a given set-palette object suitable for reading into
;;; Max/MSP's coll object. The resulting text file has one line for each set in
;;; the palette, with the coll index being the ID of the set. The rest of the
;;; line is a list of frequency/amplitude pairs (or MIDI note numbers if
;;; required).
;;; 
;;; ARGUMENTS
;;; - A set-palette object.
;;; - The name (and path) of the .txt file to write.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether MIDI note numbers or frequencies should be
;;;   generated. T = MIDI. Default = NIL (frequencies).
;;; 
;;; RETURN VALUE
;;; 
;;; EXAMPLE
#|
;; Generates frequencies by default
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
  (gen-max-coll-file msp "/tmp/msp-mcf.txt"))

;; Set the optional argument to T to generate MIDI note numbers instead
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
  (gen-max-coll-file msp "/tmp/msp-mcf.txt" t))

|#
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

;;; SAR Tue Feb  7 14:28:07 GMT 2012: Edited robodoc entry

;;; ****m* set-palette/gen-midi-chord-seq
;;; DESCRIPTION
;;; Generate a MIDI file in which each set of the given set-palette object is
;;; played at 1 second intervals.
;;; 
;;; ARGUMENTS 
;;; - A set-palette object.
;;; - The name and path for the MIDI file to be generated.
;;; 
;;; RETURN VALUE  
;;; Always returns T
;;;
;;; EXAMPLE
#|
(let ((msp (make-set-palette 
            'test
            '((1 ((1
                   ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5 dqf5 gs5
                         b5))) 
                  (2
                   ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5 gqf5)
                    :subsets
                    ((tc1 (d2 g3 cs5))
                     (tc2 (eqs2 f3 bqf3))
                     (tc3 (b2 bqs3 gqf5)))))))
              (2 ((1 ((1 1) :transposition 5))
                  (2 ((1 2) :transposition 5))))
              (3 ((1 ((1 1) :transposition -2))
                  (2 ((1 2) :transposition -2))))))))
  (gen-midi-chord-seq msp "/tmp/msp-gmchs.mid"))

|#
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

;;; SAR Tue Feb  7 14:32:56 GMT 2012: Edited robodoc info

;;; ****m* set-palette/force-micro-tone
;;; DESCRIPTION
;;; Change the value of the MICRO-TONE slot of all pitch objects in a given
;;; set-palette object to the specified <value>.
;;; 
;;; ARGUMENTS 
;;; - A set-palette object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - An item of any type that is to be the new value of the MICRO-TONE slot of
;;;   all pitch objects in the given sc-set object (generally T or
;;;   NIL). Default = NIL. 
;;; 
;;; RETURN VALUE  
;;; Always returns T.
;;;
;;; EXAMPLE
#|

;; Create a set-palette object whose individual sets contain some micro-tones
;; and print the contents of all the MICRO-TONE slots to see the values. Then
;; apply the force-micro-tone method and print the slots again to see the
;; changes. 

(let ((msp (make-set-palette 
            'test
            '((1 ((1
                   ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5 dqf5 gs5 
                         b5)))
                  (2
                   ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5 gqf5)
                    :subsets
                    ((tc1 (d2 g3 cs5))
                     (tc2 (eqs2 f3 bqf3))
                     (tc3 (b2 bqs3 gqf5)))))))
              (2 ((1 ((1 1) :transposition 5))
                  (2 ((1 2) :transposition 5))))
              (3 ((1 ((1 1) :transposition -2))
                  (2 ((1 2) :transposition -2))))))))
  (print (loop for i in (data msp) 
            collect (loop for j in (data (data i))
                       collect (loop for p in (data j)
                                  collect (micro-tone p)))))
  (force-micro-tone msp t)
  (print (loop for i in (data msp) 
            collect (loop for j in (data (data i))
                       collect (loop for p in (data j)
                                  collect (micro-tone p))))))

=>
(((NIL NIL T NIL NIL T T NIL NIL NIL NIL T T NIL NIL)
  (NIL NIL T T NIL NIL NIL NIL T T NIL NIL NIL NIL T))
 ((NIL NIL T NIL NIL T T NIL NIL NIL NIL T T NIL NIL)
  (NIL NIL T T NIL NIL NIL NIL T T NIL NIL NIL NIL T))
 ((NIL NIL T NIL NIL T T NIL NIL NIL NIL T T NIL NIL)
  (NIL NIL T T NIL NIL NIL NIL T T NIL NIL NIL NIL T)))

(((T T T T T T T T T T T T T T T) (T T T T T T T T T T T T T T T))
 ((T T T T T T T T T T T T T T T) (T T T T T T T T T T T T T T T))
 ((T T T T T T T T T T T T T T T) (T T T T T T T T T T T T T T T)))

|#
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

;;; SAR Tue Feb  7 11:04:13 GMT 2012: Shortened MDE's example
;;; SAR Tue Feb  7 10:56:35 GMT 2012: Edited robodoc entry
;;; SAR Tue Jun 19 18:32:19 BST 2012: Added info on init one set from another
;;; by referencing it and issuing a transposition.

;;; ****f* set-palette/make-set-palette
;;; DESCRIPTION
;;; Create a set-palette object.
;;;
;;; Note that the sets in this palette may refer to previously defined sets in
;;; order to avoid retyping note lists (see example below).
;;; 
;;; ARGUMENTS 
;;; - A symbol that is to be the ID of the resulting set-palette object.
;;; - A recursive list of key/data pairs, of which the deepest level of data
;;;   will be a list of note-name symbols.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :recurse-simple-data. T or NIL to indicate whether to interpret
;;;   two-element data lists as recursive palettes. Default = T.
;;; - :warn-note-found. T or NIL to indicate whether to print warnings when
;;;   specified data is not found with subsequent calls to the get-data method.
;;; 
;;; RETURN VALUE  
;;; A set-palette object.
;;; 
;;; EXAMPLE
#|
;;; Create a set-palette object
(make-set-palette 
 'test
 '((1 ((1
        ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5 dqf5 gs5 b5)
         :subsets
         ((tc1 ((ds2 e3 a4) "a-tag"))
          (tc2 (bf1 d4 cqs5))
          (tc3 (c3 cs4 gs5)))))
       (2
        ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5 gqf5)
         :subsets
         ((tc1 (d2 g3 cs5))
          (tc2 (eqs2 f3 bqf3))
          (tc3 (b2 bqs3 gqf5)))))
       (3
        ((cqs2 fs2 g2 c3 d3 fqs3 gqf3 cs4 ds4 e4 gs4 dqf5 f5 a5 bqs5)
         :subsets
         ((tc1 (cqs2 c3 f5))
          (tc2 (fs2 e4 bqs5))
          (tc3 (d3 ef4 a5)))))))
   (2 ((1 ((1 1) :transposition 5))
       (2 ((1 2) :transposition 5))
       (3 ((1 3) :transposition 5))))
   (3 ((1 ((1 1) :transposition -2))
       (2 ((1 2) :transposition -2))
       (3 ((1 3) :transposition -2))))))

=>
SET-PALETTE: 
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 9
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: TEST, tag: NIL, 
data: (
[...]

;;; NB A simple list of sets (with unique id slots) can also be passed.

;;; Create a set-palette object by referencing a set-palette-object already
;;; defined (sp1) and transposing a clone of that object.
(make-set-palette 
 'test
 '((sp1
    ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5
          dqf5 gs5 b5) 
     :subsets
     ((tc1 (ds2 e3 a4))
      (tc2 (bf1 d4 cqs5))
      (qc1 (aqf2 e3 a4 dqf5 b5))
      (qc2 (bf1 c3 gqs3 cs4 cqs5)))
     :related-sets
     ((missing (bqs0 eqs1 f5 aqs5 eqf6 fqs6 
                     bqf6 dqs7 fs7)))))
   (sp2
    (sp1 :transposition 13))))

=>
SET-PALETTE: 
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 2
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: TEST, tag: NIL, 
data: (
COMPLETE-SET: complete: NIL
              num-missing-non-chromatic: 7
              num-missing-chromatic: 2
              missing-non-chromatic: (BQS BQF AQS FQS EQS EQF DQS)
              missing-chromatic: (FS F)
[...]
    subsets: 
TC1: (DS2 E3 A4)
TC2: (BF1 D4 CQS5)
QC1: (AQF2 E3 A4 DQF5 B5)
QC2: (BF1 C3 GQS3 CS4 CQS5)
    related-sets: 
MISSING: (BQS0 EQS1 F5 AQS5 EQF6 FQS6 BQF6 DQS7 FS7)
SCLIST: sclist-length: 15, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SP1, tag: NIL, 
data: (BF1 EF2 AQF2 C3 E3 GQF3 GQS3 CS4 D4 G4 A4 CQS5 DQF5 GS5 B5)
[...]
COMPLETE-SET: complete: NIL
              num-missing-non-chromatic: 7
              num-missing-chromatic: 2
              missing-non-chromatic: (BQS BQF AQS FQS EQS EQF DQS)
              missing-chromatic: (FS F)
TL-SET: transposition: 13
        limit-upper: NIL
        limit-lower: NIL
[...]
    subsets: 
TC1: (E3 F4 BF5)
TC2: (B2 EF5 DQF6)
QC1: (AQS3 F4 BF5 DQS6 C7)
QC2: (B2 CS4 AQF4 D5 DQF6)
    related-sets: 
MISSING: (BQS0 EQS1 F5 AQS5 EQF6 FQS6 BQF6 DQS7 FS7)
SCLIST: sclist-length: 15, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SP2, tag: NIL, 
data: (B2 E3 AQS3 CS4 F4 GQS4 AQF4 D5 EF5 AF5 BF5 DQF6 DQS6 A6 C7)
**************
)

|#
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
    ;; (print (data ral))
    (loop for i in (data ral) and j from 0 do
         (if (is-ral (data i))
             (setf (data (nth j (data ral)))
                   (ral-to-set-palette (data i) god))
             ;; MDE Fri May 17 18:37:33 2013 -- if we have a set object already
             ;; it'll be in the data slot of this named object 
             ;; (unless (sc-set-p i)
             (if (and (named-object-p i) (sc-set-p (data i)))
                 (progn
                   ;; we've probably made a set without an id so copy it over
                   (setf (id (data i)) (id i)
                         (nth j (data ral)) (data i)))
                 ;; (print i)
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
                                    ;; we allow the initialization of a set
                                    ;; from a previous one just given in this
                                    ;; palette!  In order to allow this, we
                                    ;; have to now do a look-up of the set-id
                                    ;; in the assoc-list (palette) we're
                                    ;; currently processing!
                                    (append (list set-object :id id)
                                            (rest data)))
                             ;; this is a new set with a list of notes.
                             (apply #'make-complete-set 
                                    (append (list set :id id)
                                            (rest data)))))))))
    (sc-change-class ral 'set-palette)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Feb  7 14:53:12 GMT 2012: Edited robodoc entry

;;; ****f* set-palette/set-palette-p
;;; DESCRIPTION
;;; Test whether a given object is a set-palette object.
;;; 
;;; ARGUMENTS 
;;; - A lisp object
;;;
;;; EXAMPLE
#|

(let ((msp (make-set-palette 
            'test
            '((1 ((1
                   ((bf1 ef2 aqf2 c3 e3 gqf3 gqs3 cs4 d4 g4 a4 cqs5 dqf5 gs5 
                         b5)))
                  (2
                   ((bf1 d2 fqf2 fqs2 b2 c3 f3 g3 bqf3 bqs3 fs4 gs4 a4 cs5 gqf5)
                    :subsets
                    ((tc1 (d2 g3 cs5))
                     (tc2 (eqs2 f3 bqf3))
                     (tc3 (b2 bqs3 gqf5)))))))
              (2 ((1 ((1 1) :transposition 5))
                  (2 ((1 2) :transposition 5))))
              (3 ((1 ((1 1) :transposition -2))
                  (2 ((1 2) :transposition -2))))))))
  (set-palette-p msp))

=> T

|#

;;; 
;;; RETURN VALUE  
;;; t or nil
;;; SYNOPSIS
(defun set-palette-p (thing)
;;; ****
  (typep thing 'set-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Ring Modulation routines

;;; SAR Wed Feb  8 12:48:17 GMT 2012: Edited robodoc entry
;;; 
;;; ****f* set-palette/recursive-set-palette-from-ring-mod
;;; DESCRIPTION
;;; Create a set-palette object consisting of sub palette-objects whose pitch
;;; content is generated based on ring modulation routines applied to the
;;; specified pitches.
;;; 
;;; ARGUMENTS 
;;; - A list of note-name symbols, each of which will serve as the reference
;;;   pitch from which a new set-palette object is made using the
;;;   set-palette-from-ring-mod method.
;;; - A symbol that will be the ID for the top-level set-palette object. The
;;;   IDs of the new set-palette objects contained in the top-level object are
;;;   generated from the note-name symbols of the reference-pitches, with the
;;;   IDs of the pitch sets contained with them then generated by sequential
;;;   numbers.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :partials. A list of integers that are the partials which the method is
;;;   to ring modulate, with 1 being either the reference-note or the bass note
;;;   that would have the reference-note as the highest partial in the given
;;;   list. Default = '(1 3 5 7).
;;; - :warn-no-bass. T or NIL to indicate whether to issue a warning when
;;;   ring-mod-bass fails to find suitable bass notes for the generated sets. T
;;;   = warn. Default = T.
;;; - :do-bass. T or NIL to indicate whether to add notes created by the
;;;   ring-mod-bass function to the resulting set-palette object. T = create and
;;;   add bass notes. Default = T.
;;; - :remove-octaves. T or NIL to indicate whether to remove the upper
;;;   instances of any octave-equivalent pitches from the resulting set-palette
;;;   object. T = remove. Default = NIL.
;;; - :min-bass-notes. An integer that is the minimum number of bass notes to
;;;   be generated and added to the resulting set-palette object. Default = 1.
;;; - :ring-mod-bass-octave. An integer that is the MIDI octave reference
;;;   number (such as the 4 in 'C4), indicating the octave from which the bass
;;;   note(s) are to be taken.
;;; 
;;; RETURN VALUE  
;;; - A set-palette object (recursive)
;;;
;;; EXAMPLE

#|
;; Simple useage with default keyword argument values
(recursive-set-palette-from-ring-mod '(a4 b4 c4) 'rspfrm-test)

=>
SET-PALETTE: 
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 3
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: RSPFRM-TEST, tag: NIL, 
data: (
NAMED-OBJECT: id: A4, tag: NIL, 
data: 
SET-PALETTE: 
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 21
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 21, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: A4, tag: NIL, 
data: (
COMPLETE-SET: complete: NIL
[...]

|#

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

;;; SAR Wed Feb  8 11:22:07 GMT 2012: MDE's original comment for :partials = 
;;;    we ring mod partials 1,3,4,7 by default, starting from
;;;    reference-note but also from the bass note that would have reference-note
;;;    as the 7th partial (or the highest in the given list).

;;; SAR Wed Feb  8 11:14:35 GMT 2012: Edited robodoc entry

;;; ****f* set-palette/set-palette-from-ring-mod
;;; DESCRIPTION
;;; Create a new set-palette object from the pitches returned by applying ring 
;;; modulation procedures (difference and sum tones of partials).
;;; 
;;; ARGUMENTS
;;; - A note-name symbol that is the central pitch from which we perform the
;;;   ring-modulation.  See :partials below.
;;; - A symbol that is to be the ID for the new set-palette object.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :partials. A list of integers that are the partials which the method uses
;;;   to ring modulate.  We create partials ascending from the reference-note
;;;   but also ascending from a fundamental calculated so that reference-note
;;;   would be the highest partial in the partials list.  E.g. if
;;;   reference-note were 'a4 (440Hz) and :partials was '(1 2) we'd have
;;;   partial frequencies of 440 and 880, as these are the ascending partials 1
;;;   and 2 from 440, but also have 220, as that is the fundamental for which
;;;   440 would be the highest partial out of (1 2).  Default = '(1 3 5 7).
;;; - :warn-no-bass. T or NIL to indicate whether to issue a warning when
;;;   ring-mod-bass fails to find suitable bass notes for the generated sets. 
;;;   T = warn. Default = T.
;;; - :do-bass. T or NIL to indicate whether to add notes created by the
;;;   ring-mod-bass function to the resulting set-palette object. T = create and
;;;   add bass notes. Default = T.
;;; - :remove-octaves. T or NIL to indicate whether to remove the upper
;;;   instances of any octave-equivalent pitches from the resulting set-palette
;;;   object. T = remove. Default = NIL.
;;; - :min-bass-notes. An integer that is the minimum number of bass notes to
;;;   be generated and added to the resulting set-palette object. Default = 1.
;;; - :ring-mod-bass-octave. An integer that is the MIDI octave reference
;;;   number (such as the 4 in 'C4), indicating the octave from which the bass
;;;   note(s) are to be taken.
;;; 
;;; RETURN VALUE
;;; A set-palette object.
;;;
;;; EXAMPLE
#|
;; Simple usage with default keyword argument values
(set-palette-from-ring-mod 'a4 'spfrm-test)

=>
SET-PALETTE: 
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 21
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 21, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SPFRM-TEST, tag: NIL, 
data: (
[...]

;;; Use with the :partials argument
(let ((spfrm2 (set-palette-from-ring-mod 'a4 'spfrm-test
                                           :partials '(2 4 6 8))))
  (loop for cs in (data spfrm2) collect (pitch-symbols cs)))

=> ((BQS0 CS5 E5 GQF5 B5 CS6 DQS6 FQS6 GQF6 AF6 BF6 B6 C7)
    (BQF0 B0 A2 A3 E4 A4 CS5 E5 GQF5)
    (BQS0 FQS6 GQF6 AF6 BF6 B6 C7 GQS7 AF7 AQF7 AQS7 BF7 BQF7)
    (B0 A2 A3 E4 A4 CS5 E5 GQF5 A5 B5) (BQS0 DQF7 DQS7 EQF7 EQS7 FQS7 FS7)
    (BQF0 A2 A3 E4 CS5 E5 GQF5 B5 CS6 DQS6)
    (AQS0 BQF0 B0 GQS7 AF7 AQF7 AQS7 BF7 BQF7)
    (B0 A4 E5 CS6 E6 GQF6 B6 CS7 DQS7 FQS7 GQF7) (B0 A5 A6 E7 A7)
    (B0 A3 CS5 CS6 DQS6 FQS6 GQF6 B6 C7 DQF7 DQS7 FS7 AF7) (BQS0 A5 A6 E7 A7)
    (B0 A4 A5 E6 A6 CS7 E7 GQF7 A7) (BQS0 A5 A6 E7)
    (BQS0 CS6 E6 GQF6 B6 CS7 DQS7 FQS7 GQF7 AF7 BF7 B7 C8)
    (BQF0 B0 BQS0 A2 A3 E4 A4 CS5 GQF5 A5 B5 CS6 E6)
    (BQS0 B6 CS7 DQS7 FQS7 GQF7 AF7) (B0 A3 A4 E5 A5 CS6 E6 GQF6)
    (B0 BQS0 FQS7 GQF7 AF7 BF7 B7 C8) (BQS0 CS6 FQS6 C7 DQS7 FQS7 GQS7 BQF7 C8)
    (B0 A5 A6 E7 A7) (BQS0 A5 E6 CS7 E7 GQF7 B7) (BQS0 A6 A7)
    (BQS0 AF6 B6 DQF7 FS7 AF7 AQS7)
    (BQF0 B0 BQS0 A2 A3 CS5 GQF5 CS6 DQS6 FQS6 GQF6 BF6)
    (BQS0 EQF7 FQS7 GQS7 BQF7 C8) (BQS0 A6 CS7 GQF7 A7) (B0 A5 A6)
    (BQS0 CS7 E7 GQF7 B7))

;;; Use with the :do-bass and :remove-octaves arguments
(let ((spfrm3 (set-palette-from-ring-mod 'a4 'spfrm-test
                                         :do-bass nil
                                         :remove-octaves t)))
  (loop for cs in (data spfrm3) collect (pitch-symbols cs)))
=> ((BQS1 GQF3 EF4 A4 DQF5) (DQF6 DQS6 EF6 F6 FQS6 GQF6 EQF7 EQS7)
    (BQS2 GQF3 A4 DQF5 F5 GQS5) (BQS6 C7 CQS7 DQF7 D7 DQS7)
    (BQS3 EF4 GQF4 DQF5 F5 GQS5 BF5 CQS6) (FQS7 FS7 GQF7 G7 GQS7)
    (GQF5 BF5 DQF6 AQF6 C7 DQS7 F7 GQS7) (BQS1 A4 F5 GQS5 DQS6)
    (GQS6 AQS6 BQS6 DQS7 EQF7 F7 BQF7) (BQS1 EF4 F5 GQS5 CQS6 FQS6)
    (EF7 EQS7 FQS7 GQS7 AQF7 AQS7) (F5 BQS5 GQS6 B6 D7 FS7 AF7 AQS7) (A4 GQF7)
    (A4 CS7 GQF7) (A4 CS7) (EF6 G6 BF6 F7 AQF7 BQS7)
    (BQS1 GQF3 DQF5 CQS6 DQS6 F6 AQS6) (CQS7 DQS7 F7 AQF7 BF7 BQS7)
    (E6 A6 GQF7 B7) (A4 E6 B7) (A6 CS7 E7 B7))


|#
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
                     rm :bass-octave ring-mod-bass-octave
                     :warn warn-no-bass)))
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
             ;; here there's numbers instead of symbols so we can still get
             ;; duplicates when making the set :/ 
             set (remove-duplicates (append rm rm-bass))
             ;; MDE Thu May 3 10:57:21 2012 -- as we removed octaves and
             ;; duplicates above when looking at freq, when these are resolved
             ;; to the nearest note, we still might have octaves/duplicates so
             ;; do this again at the set level, below
             set (make-complete-set set :id i :subsets `((rm-bass ,rm-bass))
                                    ;; MDE Mon May 20 12:56:47 2013 -- don't
                                    ;; warn about duplicate pitches but do
                                    ;; remove them
                                    :warn-dups nil :rm-dups t
                                    :tag (combine-into-symbol 
                                          (freq-to-note left) '-ringmod- 
                                          (freq-to-note right))))
       ;; MDE Thu May  3 10:59:13 2012 
       (rm-duplicates set t)       ; comparing symbols, not pitch= (freqs etc.)
       (when remove-octaves
         (rm-octaves set))
       (add set sp))
    sp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Feb  7 15:48:24 GMT 2012: Edited robodoc entry

;;; ****f* set-palette/ring-mod
;;; DESCRIPTION
;;; Ring modulate two frequencies and return the resulting pitch and harmonic
;;; partials thereof. 
;;; 
;;; ARGUMENTS
;;; - A first pitch, either as a numeric hertz frequencey or a note-name
;;;   symbol.  
;;; - A second pitch, either as a numeric hertz frequencey or a note-name
;;;   symbol. The second value needn't be higher than first.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :return-notes. T or NIL to indicate whether to return the results as
;;;   note-name symbols or frequency numbers. T = note-name symbols. 
;;;   Default = NIL.
;;; - :pitch1-partials. An integer that indicates how many harmonic partials of
;;;   the first pitch are to be included in the modulation. Default = 3.
;;; - :pitch2-partials. An integer that indicates how many harmonic partials of
;;;   the second pitch are to be included in the modulation. Default = 2.
;;; - :min-freq. A number that is the the minimum frequency (hertz) that may be
;;;   returned. Default = 20.
;;; - :max-freq. A number that is the the maximum frequency (hertz) that may be
;;;   returned. Default = 20000.
;;; - :round. T or NIL to indicate whether frequency values returned are first
;;;   rounded to the nearest hertz. T = round. Default = T
;;; - :remove-duplicates. T or NIL to indicate whether any duplicate
;;;   frequencies are to be removed from the resulting list before returning
;;;   it. T = remove. Default = T.
;;; - :print. T or NIL to indicate whether resulting data is to be printed as
;;;   it is being generated. T = print. Default = NIL.
;;; - :remove-octaves. T or NIL to indicate whether octave repetitions of
;;;   pitches will be removed from the resulting list before returning it,
;;;   keeping only the lowest instance of each pitch. This argument can also be
;;;   set as a number or a list of numbers that indicates which octave
;;;   repetitions will be allowed, the rest being removed. For example,
;;;   :remove-octaves '(1 2) will remove all octave repetitions of a given
;;;   pitch except for those that are 1 octave and 2 octaves above the given
;;;   pitch; thus '(c1 c2 c3 c4 c5) would return '(c1 c2 c3), removing c4 and
;;;   c5. Default = NIL.
;;; - :scale. A variable that indicates which scale to use when converting
;;;   frequencies to note-names. Default = cm::*scale* i.e. the value to which
;;;   the Common Music scale is set, which in slippery chicken is
;;;   *quarter-tone* by default.
;;; 
;;; RETURN VALUE
;;; A list of note-name symbols or frequencies.
;;;
;;; EXAMPLE
#|
;; Apply ring modulation to 'C4 and 'D4, using 5 partials of the first pitch
;; and 3 partials of the second, removing octave repetitions, and returning the
;; results as rounded hertz-frequencies
(ring-mod 'c4 'd4
          :pitch1-partials 5
          :pitch2-partials 3
          :min-freq 60
          :max-freq 2000
          :remove-octaves t)

=> (64.0 96.0 166.0 198.0 230.0 358.0 427.0 459.0 491.0 555.0 619.0 817.0
    1079.0 1143.0 1340.0 1372.0 1404.0 1666.0 1895.0 1927.0)

;; Applying ring modulation to two frequencies, returning the results as
;; note-name symbols within the chromatic scale.
(ring-mod '261.63 '293.66 
          :return-notes t
          :remove-duplicates nil
          :scale cm::*chromatic-scale*)

=> (C1 C2 G3 BF3 E4 B4 CS5 AF5 AF5 CS6 CS6 F6)

|#
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
  ;; MDE Mon Mar 19 20:05:29 2012 -- got to make sure our float maths works in
  ;; different lisps
  (setf pitch1 (decimal-places pitch1 2)
        pitch2 (decimal-places pitch2 2))
  ;; (print pitch1) (print pitch2)
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
                       (format t "~&~a: ~a ~a"
                               f note (freq-to-degree f 'chromatic-scale)))
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
    ;; (print rmod)
    (if return-notes
        notes
        rmod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Feb 11 14:15:13 GMT 2012: Edited and extended robodoc entry

;;; ****f* set-palette/ring-mod-bass
;;; DESCRIPTION
;;; Using ring-modulation techniques, invent (sensible) bass note(s) from a
;;; list of frequencies.  
;;;
;;; ARGUMENTS
;;; - A list of numbers that are hertz frequencies from which the bass note(s)
;;;   are to be generated.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :bass-octave. An integer that is an octave indicator (e.g. the 4 in
;;;   'C4). The method will only return any frequencies/note-names generated
;;;   that fall in this octave. Default = 0.
;;; - :low. A note-name symbol that is the lowest possible pitch of those
;;;   returned. This argument further restricts the :bass-octave argument. Thus
;;;   a :bass-octave value of 1 could be further limited to no pitches below
;;;   :low 'DS1. Default = 'A0.
;;; - :high. A note-name symbol that is the highest possible pitch of those
;;;   returned. This argument further restricts the :bass-octave argument. Thus
;;;   a :bass-octave value of 1 could be further limted to no pitches above
;;;   :high 'FS1. Default = 'G3.
;;; - :round. T or NIL to indicate whether the frequencies returned are rounded
;;;   to integer values. T = round. Default = T.
;;; - :warn. T or NIL to print a warning when no bass can be created from the
;;;   specified frequencies/note-names. T = print warning. Default = T.
;;; - :return-notes. T or NIL to indicate whether the resulting pitches should
;;;   be returned as note-names instead of frequencies. T = return as
;;;   note-names. Default = NIL.
;;; - :scale. A variable pointing to the scale to which any translation of
;;;   frequencies into note-names symbols should take place. By default this
;;;   value is set to cm::*scale*, which is automatically set by slippery
;;;   chicken to 'quarter-tone at initialisation. To return e.g. pitches rounded
;;;   to chromatic note-names set this argument to cm::*chromatic-scale*.
;;;
;;; RETURN VALUE
;;; Returns a list of frequencies by default.
;;;
;;; Setting the :return-notes keyword argument to T will cause the method to
;;; return note-name symbols instead.
;;;
;;; EXAMPLE
#|
;; Simple usage with default keyword argument values
(ring-mod-bass '(261.63 293.66 329.63 349.23))

=> (28 29 32)

;; Return as note-names instead, in quarter-tone scale by default
(ring-mod-bass '(261.63 293.66 329.63 349.23)
               :return-notes t)

=> (A0 BF0 BQS0) 

;; Set the :scale argument to cm::*chromatic-scale* to return equal-tempered
;; note-name symbols instead
(ring-mod-bass '(261.63 293.66 329.63 349.23)
               :return-notes t
               :scale cm::*chromatic-scale*)

=> (A0 BF0 C1)

;; Return pitches from bass octave 1 rather than default 0
(ring-mod-bass '(261.63 293.66 329.63 349.23 392.00)
               :return-notes t
               :scale cm::*chromatic-scale*
               :bass-octave 1)

=> (CS1 D1 F1 G1 A1 B1)

;; Further limit the notes returned by setting :low and :high values
(ring-mod-bass '(261.63 293.66 329.63 349.23 392.00)
               :return-notes t
               :scale cm::*chromatic-scale*
               :bass-octave 1
               :low 'e1
               :high 'a1)

=> (F1 G1)

;; Set the :round argument to NIL to return decimal-point frequencies
(ring-mod-bass '(261.63 293.66 329.63 349.23 392.00)
               :bass-octave 1
               :low 'e1
               :high 'a1
               :round NIL)

=> (42.76999999999998 43.45666666666667 43.80000000000001 49.16999999999999)

;; The method prints a warning by default if no bass note can be made
(ring-mod-bass '(261.63))

=>
NIL
WARNING: set-palette::ring-mod-bass: can't get bass from (261.63)!

;; This warning can be suppressed by setting the :warn argument to NIL
(ring-mod-bass '(261.63) :warn nil)

=> NIL

|#
;;;
;;; SYNOPSIS
(defun ring-mod-bass (freqs &key (bass-octave 0) (low 'a0) (high 'g3) (round t)
                      (warn t) (return-notes nil) (scale cm::*scale*))
;;; ****
  ;; MDE Mon Mar 19 19:42:17 2012 -- when we get down low, float errors can
  ;; cause different notes to be created among different lisps e.g. 
  ;; (ring-mod-bass '(62.85714177508388 314.2857088754194))
  ;; today returns 31 in CCL and 28 in SBCL.  So round all freqs to 3 decimal
  ;; places before processing
  (setf freqs (loop for f in freqs collect (decimal-places f 3)))
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
               (loop for i from 1.0 to 100 
                  ;; MDE Mon Mar 19 20:27:34 2012 -- round to 2 places here
                  ;; too! 
                  for freq = (decimal-places (/ diff i) 2)
                  do
                    (when (and (not (< freq low))
                               (not (> freq high))
                               (in-octave freq bass-octave)
                               (not (octave-of-freqs freq)))
                      (return freq)))
               when bass collect bass)))
      (if result
          (progn
            (setf result (sort result #'<))
            (if return-notes
                (setf result (loop for f in result collect
                                  (freq-to-note f scale)))
                ;; MDE Wed Feb  8 17:42:34 2012 -- added round
                (when round
                  (setf result (loop for f in result collect (round f)))))
            (remove-duplicates result))
          (when warn
            (warn "set-palette::ring-mod-bass: can't get bass from ~a!"
                  freqs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
