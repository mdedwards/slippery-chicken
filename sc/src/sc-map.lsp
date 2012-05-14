;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* recursive-assoc-list/sc-map
;;; NAME 
;;; sc-map
;;;
;;; File:             sc-map.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   sc-map
;;;
;;; Version:          1.0.0-beta
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the sc-map class for mapping rhythmic
;;;                   sequences, chords etc. to specific parts of a piece. 
;;;                   The extension to the recursive-assoc-list class is in the
;;;                   data returned when get-data-from-palette is called: being
;;;                   a map, the data returned by the superclass get-data
;;;                   function is actually a reference into a palette.  Instead
;;;                   of just returning this reference, with
;;;                   get-data-from-palette we then use this as a lookup into
;;;                   the palette slot.  If the reference happens to be a list,
;;;                   then each element of the list is used as a reference into
;;;                   the palette and the resulting objects are returned in a
;;;                   list.   
;;;
;;;                   When in a list of references, perhaps the rthm-seq
;;;                   references for a section, a single reference is also a
;;;                   list this can be one of two things: the reference is to a
;;;                   recursive palette, whereupon the data will simply be
;;;                   returned for that reference; or, the reference is a list
;;;                   of references that together build up an object consisting
;;;                   of the referenced smaller objects.  This is the case
;;;                   when, for example, 4-bar sequences in one or more
;;;                   instruments are accompanied by groups of 4 single bar
;;;                   sequences in others:
;;;
;;;                   (2
;;;                    ((bsn ((r1-1 r1-2 r1-3 r1-5) 20 1 ...))
;;;                     (trb (2 23 3 ...))))
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 21st 2001
;;;
;;; $$ Last modified: 20:24:20 Thu Mar  1 2012 GMT
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

(defclass sc-map (recursive-assoc-list)
  ((palette :accessor palette :initarg :palette :initform nil)
   ;; often sc-map data is generated algorithmically but individual elements of
   ;; the lists need to be changed; this can be done here with a list of lists
   ;; of the type (((1 2 vla) 3 20b) ((2 3 vln) 4 16a)); each list is a change,
   ;; the first element of the list is the reference into the sc-map (the viola
   ;; voice of section 1 subsection 2 in the first e.g.), the second element is
   ;; the nth of the data list for this key to change, and the third is the new
   ;; data.
   (replacements :accessor replacements :type list :initarg :replacements
                 :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See above

(defmethod initialize-instance :after ((scm sc-map) &rest initargs)
  (declare (ignore initargs))
  (do-replacements scm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod do-replacements ((scm sc-map))
  (loop for r in (replacements scm) do
       (unless (= 3 (length r))
         (error "sc-map::initialize-instance: In sc-map with id ~a: ~
                 replacement data should be a list of 3-element ~
                 lists (key nth replacement): ~a"
                (id scm) r))
       ;; (print "****") (print r)
       (set-nth-of-data (first r) (1- (second r)) (third r) scm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf replacements) :after (rplmts (scm sc-map))
  (declare (ignore rplmts))
  (do-replacements scm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((scm sc-map) stream)
  (format stream "~%SC-MAP: palette id: ~a"
          (when (palette scm)
            (id (palette scm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((scm sc-map))
  (clone-with-new-class scm 'sc-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((scm sc-map) new-class)
  (declare (ignore new-class))
  (let ((ral (call-next-method)))
    (setf (slot-value ral 'palette) (palette scm))
    ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bind-palette ((scm sc-map) (p palette) &optional (nil-ok t))
  (check-sc-map-refs scm p nil-ok)
  (setf (palette scm) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Mar  3 18:37:04 GMT 2012: Added robodoc entry

;;; ****m* sc-map/get-all-data-from-palette
;;; FUNCTION
;;; Given an sc-map object that has been bound to a palette object of any type,
;;; return all of the palette data contained in the given sc-map object as it
;;; has been allocated to the map, in the order in which it appears in the map.
;;;
;;; The given sc-map object must be bound to a palette object for this method
;;; to work. If no palette object has been bound to the given sc-map object,
;;; the method returns NIL and prints a warning.
;;; 
;;; ARGUMENTS
;;; - An sc-map object.
;;; 
;;; RETURN VALUE
;;; - A list of objects, the type depending on the given palette.
;;; 
;;; EXAMPLE
#|

;; Create a set-palette object and an sc-map object, bind them using the
;; <palette> argument of the make-sc-map function, and print the results of
;; applying the get-all-data-from-palette method by printing the data of each
;; of the objects in the list it returns as note-name symbols.
(let* ((sp (make-set-palette 'set-pal '((set1 ((c2 b2 a3 g4 f5 e6)))
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
				  (vc (set3 set2 set3)))))
			:palette sp)))
  (loop for cs in (get-all-data-from-palette scm)
     collect (pitch-list-to-symbols (data cs))))

=>
((C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6)
 (D2 C3 B3 A4 G5 F6) (E2 D3 C4 B4 A5 G6) (C2 B2 A3 G4 F5 E6)
 (E2 D3 C4 B4 A5 G6) (C2 B2 A3 G4 F5 E6) (D2 C3 B3 A4 G5 F6)
 (C2 B2 A3 G4 F5 E6) (D2 C3 B3 A4 G5 F6) (C2 B2 A3 G4 F5 E6)
 (D2 C3 B3 A4 G5 F6) (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6)
 (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (E2 D3 C4 B4 A5 G6)
 (C2 B2 A3 G4 F5 E6) (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6)
 (C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6)
 (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6) (E2 D3 C4 B4 A5 G6))

;; Applying the method to an sc-map object that is not bound to a palette
;; object returns NIL
(let ((scm (make-sc-map 'sc-m '((sec1
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
  (get-all-data-from-palette scm))

=>
NIL
WARNING:
   sc-map::get-all-data-from-palette: 
   palette slot is nil so can't return data from it. 

|#
;;; SYNOPSIS
(defmethod get-all-data-from-palette ((scm sc-map))
;;; ****
  (if (palette scm)
      (let ((all-refs (get-all-refs scm)))
        (loop 
           for ref in all-refs 
           append (get-data-from-palette ref scm)))
      (warn "sc-map::get-all-data-from-palette: palette slot is nil so can't ~
           return data from it.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See comment at head of class.

;;; SAR Sun Mar  4 10:31:07 GMT 2012: Edited robodoc entry
;;; SAR Wed Feb  8 16:08:21 GMT 2012: Added robodoc entry

;;; ****m* sc-map/get-data-from-palette
;;; FUNCTION
;;; Given an sc-map object that has been bound to a palette object of any type,
;;; return the palette data contained allocated to the location within the
;;; given sc-map object as specified by the <IDs> argument.
;;; 
;;; Deeper levels of the map can be accessed by specifying a path of IDs into
;;; the given sc-map object.
;;;
;;; If no palette object has been bound to the given sc-map object, the method
;;; returns the contents of the sc-map object at the specified location
;;; instead. 
;;;
;;; ARGUMENTS
;;; - A symbol or list of symbols that is/are the ID or path of nested IDs
;;;   within the given sc-map object for which the data is sought.
;;; - The sc-map object in which the data is sought.
;;; 
;;; OPTIONAL ARGUMENTS 
;;; - T or NIL to indicate whether to print a warning if the specified ID is
;;;   not found in the given sc-map object. T = print warning. Default = T.
;;; 
;;; RETURN VALUE
;;; The named object or list of named objects associated with the specified ID
;;; or path of IDs.
;;;
;;; If the specified ID is not found within the given sc-map object, the method
;;; returns NIL. If the optional <warn> argument is set to T, a warning is also
;;; printed in this case.
;;; 
;;; EXAMPLE
#|
;;; Create a palette object and an sc-map object and bind them using the
;;; <palette> keyword argument of the make-sc-map function. Then apply the
;;; get-data-from-palette object to a nested ID in the sc-map object. Loop
;;; through the data of the named objects in the list returned and return them
;;; as note-name symbols. 
(let* ((sp (make-set-palette 'set-pal '((set1 ((c2 b2 a3 g4 f5 e6)))
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
				  (vc (set3 set2 set3)))))
			:palette sp)))
  (loop for cs in (get-data-from-palette '(sec1 vn) scm)
     collect (pitch-list-to-symbols (data cs))))

=> ((C2 B2 A3 G4 F5 E6) (E2 D3 C4 B4 A5 G6) (D2 C3 B3 A4 G5 F6))

;; If applied to an sc-map object that is not bound to a palette, the contents
;; of the sc-map object at the specified location are returned and a warning is
;; printed by default 
(let ((scm (make-sc-map 'sc-m '((sec1
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
   (get-data-from-palette '(sec1 vn) scm))

=> 
NAMED-OBJECT: id: VN, tag: NIL, 
data: (SET1 SET3 SET2)
**************
, NO-PALETTE

|#
;;; SYNOPSIS
(defmethod get-data-from-palette (ids (scm sc-map) &optional (warn t))
;;; ****
  (let* ((palette-ref (get-data ids scm warn))
         (palette-ref-data (when palette-ref (data palette-ref))))
    (cond ((not (palette scm))
           ;; MDE Thu Feb 23 10:52:44 2012 -- just return the get-data call if
           ;; there's no palette, but indicate something in the second returned
           ;; value to this effect 
           (values palette-ref 'no-palette))
          ;; often a sc-map has multiple references to a palette stored for any
          ;; given key(s) so palette-ref-data is a list of references; if so,
          ;; give them all back
          ((list-of-refs-p palette-ref-data)
           (get-data-from-palette-aux palette-ref-data scm))
          ((assoc-list-id-p palette-ref-data)
           (get-data palette-ref-data (palette scm)))
          (t palette-ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 13.2.11.  this allows us, once we've got a map, to e.g. have several
;;; players playing in rhythmic unison.  start-seq and end-seq are 1-based and
;;; inclusive. 

;;; ****m* sc-map/double
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod double ((scm sc-map) section-ref start-seq end-seq master-player
                   doubling-players)
;;; ****
  (unless (listp doubling-players)
    (setf doubling-players (list doubling-players)))
  (loop for seq-num from (1- start-seq) below end-seq ; still inclusive!
     for master-seq = (get-nth-from-map (list section-ref master-player)
                                        seq-num scm)
     do
     (loop for dp in doubling-players do
          (set-nth-of-data (list section-ref dp) seq-num master-seq scm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For a particular section there may be a list of references into the palette
;;; (for instance when you get the set references for a section, there is often
;;; a list of them).  Given the section reference, get the nth (0-based) from
;;; the palette.

;;; ****m* sc-map/get-nth-from-palette
;;; FUNCTION
;;; Given an sc-map object that is bound to a palette object of any type,
;;; return the data of the palette object located at the nth position of the
;;; list found at the specified ID or path of nested IDs.
;;;
;;; If the given sc-map object is not bound to a palette object, NIL is
;;; returned instead. 
;;; 
;;; ARGUMENTS 
;;; - An ID or list of IDs that are the path to the list within the given
;;;   sc-map object from which the specified nth position is to be returned. 
;;; - A zero-based integer that is the position within the list found at the
;;;   path specified from which the given element is to be returned.
;;; - An sc-map object.
;;; 
;;; RETURN VALUE
;;; - An element/object of the type contained within the given palette object
;;;   of the given sc-map object.
;;; 
;;; EXAMPLE
#|
;;; Create a set-palette object and an sc-map object, bind them using the
;;; <palette> object of the make-sc-map function, and apply the
;;; get-nth-from-palette method
(let* ((sp (make-set-palette 'set-pal '((set1 ((c2 b2 a3 g4 f5 e6)))
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
				  (vc (set3 set2 set3)))))
			:palette sp)))
  (get-nth-from-palette '(sec1 vn) 0 scm))

=> 
COMPLETE-SET: complete: NIL
              num-missing-non-chromatic: 12
              num-missing-chromatic: 6
              missing-non-chromatic: (BQS BQF AQS AQF GQS GQF FQS EQS EQF DQS
                                      DQF CQS)
              missing-chromatic: (BF AF FS EF D CS)
TL-SET: transposition: 0
        limit-upper: NIL
        limit-lower: NIL
SC-SET: auto-sort: T, used-notes: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 0
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 0, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: USED-NOTES, tag: NIL, 
data: NIL
**************


**** N.B. All pitches printed as symbols only, internally they are all 
pitch-objects.


    subsets: 
    related-sets: 
SCLIST: sclist-length: 6, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SET1, tag: NIL, 
data: (C2 B2 A3 G4 F5 E6)
**************

;;; Applying the method to an sc-map object that is not bound to a palette
;;; object returns NIL
(let ((scm (make-sc-map 'sc-m '((sec1
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
  (get-nth-from-palette '(sec1 vn) 0 scm))

=> NIL

|#
;;; SYNOPSIS
(defmethod get-nth-from-palette (sc-map-ref nth (scm sc-map))
;;; ****
  (let ((p (palette scm))
        (refs (data (get-data sc-map-ref scm nil))))
    (when p
      (get-data (nth nth refs) p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 17:07:42 GMT 2012: Added robodoc entry

;;; ****m* sc-map/get-nth-from-map
;;; FUNCTION
;;; Get the element located at the nth position within a given sc-map
;;; object. Both the map-ref (the path of IDs into the list to be searched) and
;;; the nth must be specified.
;;; 
;;; ARGUMENTS
;;; - A list that is the map-ref; i.e., the path of IDs into the list to be
;;;   searched. 
;;; - An integer that is the zero-based index of the element to be returned
;;;   from the specified list.
;;; - An sc-map object.
;;; 
;;; RETURN VALUE
;;; Returns the element located at the given index.
;;;
;;; Returns NIL if the index does not exist.
;;; 
;;; EXAMPLE
#|

;; Specify the path of IDs into the desired list ("map-ref") as a list, then
;; the position to be read from within the list located there.
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
  (get-nth-from-map '(1 vn) 1 mscm))

=> 2

;; Returns NIL if the specified index does not exist
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
  (get-nth-from-map '(3 vn) 1 mscm))

=> NIL

|#

;;; SYNOPSIS
(defmethod get-nth-from-map (map-ref nth (scm sc-map))
;;; ****
  (nth nth (data (get-data map-ref scm nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 17:18:26 GMT 2012: Added robodoc entry

;;; ****m* sc-map/delete-nth-in-map
;;; DATE
;;; 05 Feb 2011 
;;;
;;; FUNCTION
;;; Replace the element at the specified location within the specified list of
;;; a given sc-map object with NIL.
;;; 
;;; ARGUMENTS
;;; - A list that is the map-ref; i.e., the path of IDs into the list to be
;;;   searched. 
;;; - An integer that is the zero-based index of the element to be returned
;;;   from the specified list.
;;; - An sc-map object.
;;; 
;;; RETURN VALUE
;;; Always returns NIL
;;; 
;;; EXAMPLE
#|
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
  (delete-nth-in-map '(1 vn) 1 mscm)
  (get-data-from-palette '(1 vn) mscm))

=> 
NAMED-OBJECT: id: VN, tag: NIL, 
data: (1 NIL 3 4 5)


|#
;;; SYNOPSIS
(defmethod delete-nth-in-map (map-ref nth (scm sc-map))
;;; ****
  (set-nth-of-data map-ref nth nil scm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 6.12.11, from and to are 0-based

(defmethod delete-from-to-in-map (map-ref from to (scm sc-map))
;;; ****
  (loop for n from from to to do
       (delete-nth-in-map map-ref n scm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get all the objects from the palette with references in palette-refs list.

(defmethod get-data-from-palette-aux (palette-refs (scm sc-map))
  (unless (listp palette-refs)
    (setf palette-refs (list palette-refs)))
  (let ((p (palette scm)))
    (unless p
      (error "sc-map::get-data-from-palette-aux:  ~
              No palette in sc-map with id ~a" 
             (id scm)))
    (loop for ref in palette-refs collect (get-data ref p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1.3.11
(defmethod incf-ids ((scm sc-map) inc &key start end) ; inclusive!
  (unless start
    (setf start (id (first (data scm)))))
  (unless end 
    (setf end (id (first (last (data scm))))))
  (loop for no in (data scm) 
     for id = (id no)
     do
       (unless (numberp id)
         (error "scm-map::decf-ids: id is not a number, can't decrement: ~a"
                id))
       (when (and (>= id start) (<= id end))
         (incf (id no) inc)))
  scm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb  8 14:35:30 GMT 2012: Added robodoc entry

;;; ****f* sc-map/make-sc-map
;;; FUNCTION
;;; Create an sc-map object, which will be used for mapping rhythmic sequences,
;;; chords etc. to specific parts of a piece. 
;;; 
;;; ARGUMENTS
;;; - An element of any data type that will be the ID of the resulting sc-map
;;;   object. 
;;; - A list of of data, most likely recursive.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:

;;; - :warn-not-found. T or NIL to indicate whether a warning is printed when
;;;   an index which doesn't exist is used for lookup.  T = warn. Default = T.

;;; - :recurse-simple-data. T or NIL to indicate whether to recursively
;;;   instantiate a recursive-assoc-list in place of data that appears to be a
;;;   simple assoc-list (i.e. a 2-element list). If NIL, the data of 2-element
;;;   lists whose second element is a number or a symbol will be ignored,
;;;   therefore remaining as a list. For example, this data would normally
;;;   result in a recursive call: (y ((2 23) (7 28) (18 2))).  
;;;   T = recurse. Default = T.

;;; - :replacements. A list of lists in the format 
;;;   '(((1 2 vla) 3 20b) ((2 3 vln) 4 16a)) that indicate changes to
;;;   individual elements of lists within the given sc-map object. (Often
;;;   sc-map data is generated algorithmically but individual elements of the
;;;   lists need to be changed.) Each such list indicates a change, the first
;;;   element of the list being the reference into the sc-map (the viola voice
;;;   of section 1 subsection 2 in the first here e.g.), the second element is
;;;   the nth of the data list for this key to change, and the third is the new
;;;   data.

;;; - :palette. A palette object or NIL. If a palette object is specified or
;;;   defined here, it will be automatically bound to the given sc-map
;;;   object. Default = NIL
;;; 
;;; RETURN VALUE
;;; An sc-map object.
;;; 
;;; EXAMPLE
#|
;; Create an sc-map object with contents that could be used as a rthm-seq-map 
(make-sc-map 'scm-test
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
                 (vc (9))))))

=>
SC-MAP: palette id: NIL
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 9
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SCM-TEST, tag: NIL, 
data: (
NAMED-OBJECT: id: 1, tag: NIL, 
data: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 3
                      linked: NIL
                      full-ref: (1)
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "sub-ral-of-SCM-TEST", tag: NIL, 
data: (
NAMED-OBJECT: id: VN, tag: NIL, 
data: (1 2 3 4 5)
[...]

;;; Create an sc-map object and automatically bind it to a set-palette object
;;; using the <palette> keyword argument. Then read the PALETTE slot of the
;;; sc-map created to see its contents.
(let ((scm 
       (make-sc-map 
	'scm-test
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
	:palette (make-set-palette 'set-pal 
				   '((set1 ((c2 b2 a3 g4 f5 e6)))
				     (set2 ((d2 c3 b3 a4 g5 f6)))
				     (set3 ((e2 d3 c4 b4 a5 g6))))))))
  (palette scm))

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
NAMED-OBJECT: id: SET-PAL, tag: NIL, 
data: (
COMPLETE-SET: complete: NIL
[...]
data: (C2 B2 A3 G4 F5 E6)
[...]
COMPLETE-SET: complete: NIL
[...]
data: (D2 C3 B3 A4 G5 F6)
[...]
COMPLETE-SET: complete: NIL
[...]
data: (E2 D3 C4 B4 A5 G6)
)

|#
;;; SYNOPSIS
(defun make-sc-map (id scm &key (palette nil) (warn-not-found t)
                    (recurse-simple-data t) (replacements nil))
;;; ****
  (make-instance 'sc-map :data scm :id id :warn-not-found warn-not-found
                 :palette palette
                 :replacements replacements
                 :recurse-simple-data recurse-simple-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-of-refs-p (candidate)
  (when (listp candidate)
    (loop for i in candidate with ok do
          (setf ok
            (if (listp i)
                (list-of-refs-p i)
              (assoc-list-id-p i)))
          (unless ok
            (return nil))
        finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check that every reference in sc-map refers to an object in the palette

(defun check-sc-map-refs (sc-map palette &optional (nil-ok t))
  (loop 
     for i below (sclist-length sc-map) 
     for thing = (get-nth i sc-map) 
     with combo
     do (cond ((not (typep thing 'named-object))
               (error "sc-map::check-sc-map-refs: ~
                        In sc-map with id ~a expected a named-object but ~
                        got ~a" 
                      (id sc-map) thing))
              ((is-ral (data thing))
               (check-sc-map-refs (data thing) palette))
              ((not (listp (data thing)))
               (error "sc-map::check-sc-map-refs: ~
                        In sc-map with id ~a expected ~
                        a named-object with a list as data but got ~a" 
                      (id sc-map) thing))
              (t (loop for ref in (data thing) 
                    and j from 0
                    do
                    ;; nil is legal!
                    ;; 29/3/10: only when nil-ok: nil is fine for
                    ;; rthm-seq-maps as it indicates a rest, but it's not ok
                    ;; for set-map 
                    (when (and (not nil-ok)
                               (not ref))
                      (error "~a~%sc-map::check-sc-map-refs::found ~
                                nil reference in map" sc-map))
                    (when ref
                      ;; don't warn when not found
                      (unless (get-data ref palette nil)
                        ;; Here, when it's a list but not a legal ref
                        ;; into the palette, then try to make a compound
                        ;; item out of the references, i.e. each ref in the
                        ;; list is a ref into the palette, they are all
                        ;; stuck together to make a super-ref combining all
                        ;; given refs in the list.
                        (if (setf combo (do-combination ref palette))
                            ;; when successful, do-combination combines the
                            ;; objects referenced into a new object and
                            ;; returns it; we should now store it in the
                            ;; palette and replace the list of referencesin
                            ;; the sc-map with the new reference of the
                            ;; created object 
                            (progn
                              (setf (nth j (data (nth i (data sc-map))))
                                    (id combo))
                              (add combo palette))
                            (error "sc-map::check-sc-map-refs: ~
                                      In section ~a of ~
                                      sc-map ~a:~%Found illegal reference into ~
                                      palette ~a: ~a"
                                   (id thing) (id sc-map) (id palette) 
                                   ref))))))))
  t)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-combination (ref palette)
  (when (listp ref)
    (let ((objects (loop 
                       for r in ref
                                ;; don't warn when not found
                       for object = (get-data r palette nil)
                       if object collect object
                       else do (return nil))))
      (when objects
        (combine-all objects)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-map-p (thing)
  (typep thing 'sc-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF sc-map.lsp
