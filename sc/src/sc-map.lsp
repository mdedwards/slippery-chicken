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
;;; Version:          1.0
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

;;; ****m* sc-map/get-all-data-from-palette
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

;;; SAR Wed Feb  8 16:08:21 GMT 2012: Added robodoc entry

;;; ****m* sc-map/get-data-from-palette
;;; FUNCTION
;;; Get the data associated with a specifed ID (key) within a given sc-map
;;; object. If getting data from a nested key/data pair, the <id> argument must
;;; be the path of ids into that key/data pair (see example).
;;; 
;;; ARGUMENTS
;;; - The ID (key) or path of nested IDs of the key/data pair for which the
;;;   data is sought.
;;; - The sc-map object in which the data is sought.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print a warning if the specified key is
;;;   not found in the given sc-map object. T = print warning. Default = T.
;;; 
;;; RETURN VALUE
;;; The named object associated with the specified key.
;;;
;;; If the specified key is not found within the given sc-map object, the
;;; method returns NIL, and if the optional <warn> argument is set to T, a
;;; warning is printed in this case.
;;; 
;;; EXAMPLE
#|
;; Get the value of a top-level ID
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
  (get-data-from-palette '(2) mscm))

=> 
NAMED-OBJECT: id: 2, tag: NIL, 
data: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 3
                      linked: NIL
                      full-ref: (2)
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "sub-ral-of-SCM-TEST", tag: NIL, 
data: (
NAMED-OBJECT: id: VN, tag: NIL, 
data: (6 7 8)
**************

       
NAMED-OBJECT: id: VA, tag: NIL, 
data: (7 8 6)
**************

       
NAMED-OBJECT: id: VC, tag: NIL, 
data: (8 6 7)
**************
)

;; Enter the path of keys to a nested key when getting data stored more deeply
;; in the given sc-map object
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
  (get-data-from-palette '(3 va) mscm))

=> 
NAMED-OBJECT: id: VA, tag: NIL, 
data: (9)

;; The method prints a warning by default if the specified key is not found in
;; the given sc-map object
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
  (get-data-from-palette '(1 tb) mscm))

=> NIL

WARNING:
   assoc-list::get-data: Could not find data with key TB 
   in assoc-list with id sub-ral-of-SCM-TEST

;; This warning can be suppressed by setting the optional argument to NIL
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
  (get-data-from-palette '(1 tb) mscm nil))

=> NIL

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

;;; - keyword argument :palette. T or NIL. Default = NIL

;;; - keyword argument :warn-not-found. T or NIL to indicate whether a warning
;;;   is printed when an index which doesn't exist is used for lookup.
;;;   T = warn. Default = T.
;;; - keyword argument :recurse-simple-data. T or NIL to indicate whether to
;;;   recursively instantiate a recursive-assoc-list in place of data that
;;;   appears to be a simple assoc-list (i.e. a 2-element list). If NIL, the
;;;   data of 2-element lists whose second element is a number or a symbol will
;;;   be ignored, therefore remaining as a list. For example, this data would
;;;   normally result in a recursive call: (y ((2 23) (7 28) (18 2))). 
;;;   T = recurse. Default = T.
;;; - keyword argument :replacements. A list of lists of the type 
;;;   (((1 2 vla) 3 20b) ((2 3 vln) 4 16a)) that indicate changes to individual
;;;   elements of lists within the given sc-map object. (Often sc-map data is
;;;   generated algorithmically but individual elements of the lists need to be
;;;   changed.) Each such list indicates a change, the first element of the
;;;   list being the reference into the sc-map (the viola voice of section 1
;;;   subsection 2 in the first here e.g.), the second element is the nth of
;;;   the data list for this key to change, and the third is the new data. 
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
