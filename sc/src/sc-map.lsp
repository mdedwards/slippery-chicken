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
;;; $$ Last modified: 17:28:49 Wed Feb  8 2012 GMT
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
   ;; ata.
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

(defmethod link ((scm sc-map) (p palette) &optional (nil-ok t))
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
  (when (palette scm)
    (let ((all-refs (get-all-refs scm)))
      (loop 
         for ref in all-refs 
         append (get-data-from-palette ref scm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See comment at head of class.

;;; ****m* sc-map/get-data-from-palette
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
(defmethod get-data-from-palette (ids (scm sc-map) &optional (warn t))
;;; ****
  (let* ((palette-ref (get-data ids scm warn))
         (palette-ref-data (when palette-ref (data palette-ref))))
    (cond ((not (palette scm))
           palette-ref)
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

;;; ****m* sc-map/get-nth-from-map
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
(defmethod get-nth-from-map (map-ref nth (scm sc-map))
;;; ****
  (nth nth (data (get-data map-ref scm nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5.2.11

;;; ****m* sc-map/delete-nth-in-map
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

;;; ****f* sc-map/make-sc-map
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
