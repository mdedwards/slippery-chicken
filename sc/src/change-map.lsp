;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc-map/change-map
;;; NAME 
;;; change-map
;;;
;;; File:             change-map.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   sc-map -> change-map
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the change-map class where, unlike
;;;                   normal sc-maps (data is given for each sequence) gives
;;;                   data sporadically when the parameter changes, for
;;;                   instance tempo.
;;;
;;;                   It is assumed that maps will be typed in in the order in
;;;                   which sections occur so that previous-data slots can be
;;;                   kept up-to-date; also, unless all data will be given,
;;;                   that the sections (but not instrument ids)
;;;                   will be in integer sequential order so that nearest
;;;                   sections can be returned when given a non-existent
;;;                   section reference.
;;;                    
;;;                   For example, the following change-map indicates tempo 
;;;                   (though tempo-maps have their own class now).
;;;                   It has sections within sections so that the tempo for
;;;                   section '(1 2 3) can be defined, that is, section 1 has
;;;                   subsections 1, 2, and 3 and subsection 2 has further
;;;                   subsections 1, 2, and 3.  This can be nested to any
;;;                   depth.  The tempo information itself is given in sublists
;;;                   where (3 27) means that in the third sequence of the
;;;                   section, the tempo is 27.  (3 2 27) means the 2nd bar of
;;;                   the third sequence has tempo 27: when only two numbers
;;;                   are in the list, bar 1 is assumed.  The trick is, that
;;;                   this tempo then remains, as would be expected, until the
;;;                   next tempo change is indicated, which means that
;;;                   requesting the tempo of section (2 2 3) with any sequence
;;;                   and bar in the map below would return 25, because that is
;;;                   the last tempo given in section 1 and no tempo is defined
;;;                   for section 2.
;;;
;;;                   (setf x 
;;;                     (make-change-map
;;;                      'test nil
;;;                      '((0 ((3 27) (9 3 45)))
;;;                        (1 
;;;                         ((1 ((1 21) (5 28) (8 35) (3 2 40) (3 1 54)))
;;;                          (2 
;;;                           ((1 ((1 23) (6 28) (18 35)))
;;;                            (2 ((2 2 24) (7 28) (18 22)))
;;;                            (3 ((3 34) (7 28) (18 42)))))
;;;                          (3 ((1 22) (5 34) (10 5 25)))))
;;;                        (4
;;;                         ((1 ((1 21) (5 28) (8 36) (3 2 40) (3 1 55)))
;;;                          (2 ((1 22) (5 34) (10 5 103)))))
;;;                        (5 ((2 28) (6 3 45)))
;;;                        (10
;;;                         ((1 ((1 21) (5 28) (8 37) (3 2 40) (3 1 56)))
;;;                          (2 ((1 22) (5 34) (10 5 27))))))))
;;;
;;;                   You have to be careful with change-maps however as the
;;;                   nesting is flexible and therefore ambigous.  For
;;;                   instance, in the following the bcl, tape1 etc. ids are
;;;                   not subsections of section 1, rather these are the hint
;;;                   pitches assigned to the instruments in section 1 (which
;;;                   has no subsections).  This is where the last-ref-required
;;;                   class slot comes in:  If this slot is t (this is the
;;;                   second argument to make-change-map) then the last
;;;                   reference in a call to cm-get-data is always respected,
;;;                   i.e. not the last data given will be returned when the
;;;                   section doesn't exist, rather the last data for this
;;;                   reference.  E.g. In the following map, if
;;;                   last-ref-required  were nil, then the call to
;;;                   (cm-get-data x '(2 tape2) 1) would fail (because we can't
;;;                   find nearest data when references aren't numbers), but
;;;                   because it's t, we get the last data given for tape2 and
;;;                   return cs5. 
;;;
;;;                   (setf x                                         
;;;                     (make-change-map                              
;;;                         'hint-pitches t                           
;;;                         '((1 ((bcl ((1 a4) (2 b4) (3 c5) (4 d6))) 
;;;                               (tape1 ((1 a3) (2 ds2) (3 e4)))     
;;;                               (tape2 ((1 a3) (2 ds2) (3 cs5)))    
;;;                               (tape3 ((1 a3) (2 ds2) (3 eqf4))))) 
;;;                           (2 ((bcl ((1 a4) (2 b4) (3 c5) (4 d6))) 
;;;                               (tape1 ((1 a3) (2 ds2) (5 fs4)))))  
;;;                           (3 ((bcl ((1 a4) (2 b4) (3 c5) (4 d6))) 
;;;                               (tape1 ((1 a3) (2 ds2) (5 f4))))))))
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    2nd April 2001
;;;
;;; $$ Last modified: 21:14:11 Thu Dec  8 2011 ICT
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

(defclass change-map (sc-map)
  ;; See the description above for this slot. 
  ((last-ref-required :accessor last-ref-required :type boolean 
                      :initarg :last-ref-required :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((cm change-map) &rest initargs)
  (declare (ignore initargs))
  (unless (typep cm 'simple-change-map)
    (setf (recurse-simple-data cm) nil)
    (link-named-objects cm)
    (update-change-data-previous cm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((cm change-map) stream)
  (format stream "~%CHANGE-MAP: last-ref-required: ~a" 
          (last-ref-required cm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((cm change-map))
  (clone-with-new-class cm 'change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((cm change-map) new-class)
  (declare (ignore new-class))
  (let ((map (call-next-method)))
    (setf (slot-value map 'last-ref-required) (last-ref-required cm))
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((cm change-map))
  ;; don't do this for derived class...
  (unless (typep cm 'simple-change-map)
    (ral-to-change-map cm (last-ref-required cm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Section may be a simple key reference into the map or a list of references.
;;; sequence is 1-based.

;;; ****m* change-map/cm-get-data
;;; FUNCTION
;;; cm-get-data:
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod cm-get-data ((cm change-map) section 
                        &optional (sequence 1) (bar 1))
;;; ****
  (when (data cm)
    (let ((cd (get-data section cm nil)))
      (if cd
          (if (typep cd 'change-data)
              ;; data were given for this section
              (get-change-data cd sequence bar)
              (warn "change-map::cm-get-data: ~
                     Your reference (~a) into the change-map ~a was not ~
                     specific enough.  The data returned contains subsections." 
                    section (id cm)))
          (last-data (find-nearest section cm))))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns a change-data instance.

;;; ****m* change-map/find-nearest
;;; FUNCTION
;;; find-nearest:
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod find-nearest (section (cm change-map))
;;; ****
  (unless (listp section)
    (setf section (list section)))
  (let* ((last-ref (when (last-ref-required cm) (first (last section))))
         (refs (copy-list (if last-ref (butlast section) section)))
         (failed-ref nil)
         (nearest cm))
    ;; loop through each ref, from the outer(left)most in, and see if we can
    ;; get a result for this reference.   For change-maps, all section ids have
    ;; to be numbers except for the last one which could be an instrument id or
    ;; some such.  That's why we don't use the latter in the following loop if
    ;; it's there.
    (loop for ref = (pop refs) while ref do
          (let ((temp (get-data ref nearest nil)))
            (cond ((and temp (is-ral (data temp)))
                   (setf nearest (data temp)))
                  ;; is temp a change-data? if so save it, if not, then leave
                  ;; at what it was last time through.
                  (temp (when temp (setf nearest temp))
                        (return))
                  ;; didn't get data for this ref, remember it
                  (t (setf failed-ref ref)
                     (return)))))
    ;; Sometimes, when we give a subsection that doesn't exist, for instance,
    ;; we haven't got a failed-ref as such, but there's still a ref or refs
    ;; left to be gotten so set failed-ref to be this.
    (when (and (not failed-ref) refs)
      (setf failed-ref (first refs)))
    ;; When we get to the end of the above loop, have we used all the refs?  If
    ;; no, nearest has to be a change-map (check and signal error if not): loop
    ;; through the data and get the closest id based on number comparison.  
    (when failed-ref
      (unless (typep nearest 'change-map)
        (error "change-map::find-nearest: Expected a change-map: ~
                There are no subsections here, please add them to the ~
                change-map.  Change-map id = ~a, requested section = ~a ~
                remaining references = ~a ~%~a"
               (id cm) section refs nearest))
      (setf nearest (get-nearest-by-number nearest failed-ref)))
    (unless nearest 
      (error "change-map::find-nearest: Couldn't find nearest for reference ~
              ~a in change-map with id ~a" section (id cm)))
    ;; Now we've either got a change-data or a change-map: if cd: if last-ref
    ;; get-previous until we get to last-ref, else return cd; if cm: if
    ;; last-ref return it if it's in the map, else get the first cd and
    ;; get-previous until we get to last-ref; if no last-ref, return the last
    ;; in this subsection  
    (cond ((typep nearest 'change-data)
           (if last-ref
               (get-nearest-by-last-ref cm nearest last-ref)
             nearest))
          ((typep nearest 'change-map)
           (cond (last-ref
                  (let ((temp (get-data last-ref nearest nil)))
                    (if temp
                        temp
                      (get-nearest-by-last-ref cm (get-first nearest)
                                               last-ref))))
                 ;; we got all the given refs for the section but there's still
                 ;; subsections.  
                 ((null refs) (get-last nearest))
                 (t (get-nearest-by-number nearest failed-ref))))
          (t (error "change-map::find-nearest: Expected change-data or ~
                     change-map object: ~a" nearest)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-nearest-by-last-ref ((complete-cm change-map) 
                                    nearest
                                    last-ref
                                    &optional (error-not-found t))
  (let ((this (if (typep nearest 'change-map)
                  (get-last nearest)
                nearest)))
    (when this
      (loop 
        (when (id-eq last-ref this)
          (return this))
        (setf this (get-data (previous this) complete-cm nil))
        (unless this
          (when error-not-found
            (error "change-map::get-nearest-by-last-ref: ~
                    Couldn't find ~a in change-map with id ~a"
                   last-ref (id complete-cm)))
          (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-nearest-by-number ((cm change-map) number)
  (let ((first (first (data cm)))
        (result nil))
    (flet ((check-number (num)
             (unless (numberp num)
               (error "change-map::get-nearest-by-number: Argument should ~
                       be a number: ~a (change-map id = ~a)"
                      num (id cm)))
             num)) 
      ;; do we want the data from the previous ral?
      (if (< (check-number number) (check-number (id first)))
          (if (typep cm 'simple-change-map)
              (error "change-map::get-nearest-by-number: No data defined for ~
                      reference ~a in ~a" 
                     number (id cm))
            (get-previous first cm))
        (loop for lno in (data cm) do
              (if (> number (check-number (id lno)))
                  (setf result lno)
                (return))
              ;; if the above if isn't triggered then we asked for a number
              ;; greater than the one highest one given so return the last one 
            finally (setf result lno)))
      ;; if we've referenced a section with subsections we'll have a
      ;; named-object with data being a change-map, otherwise we'll have a
      ;; change-data
      (if (typep (data result) 'change-map)
          (data result)
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-change-data-previous ((cm change-map) 
                                        &optional 
                                        (top-level-cm nil)
                                        (previous nil))
  (unless top-level-cm
    (setf top-level-cm cm))
  (let ((last-ref-required (last-ref-required cm)))
    (loop for thing in (data cm) do
          (let* ((thing-is-cd (typep thing 'change-data))
                 (thing-data (data thing))
                 (thing-data-is-cm (typep thing-data 'change-map)))
            (unless (or thing-is-cd thing-data-is-cm)
              (error "change-map::update-change-data-previous: thing ~
                      should be a change-map or a change-data: ~a"
                     thing))
            (if thing-is-cd
                (setf (previous-data thing)
                  (let ((temp (if last-ref-required
                                  (get-nearest-by-last-ref top-level-cm
                                                           previous 
                                                           (id thing)
                                                           nil)
                                previous)))
                    (when temp
                      (last-data temp))))
              ;; thing-data-is-cm
              (update-change-data-previous thing-data top-level-cm previous))
            (setf previous (if thing-is-cd
                               thing 
                             ;; thing-data-is-cm
                             (get-last thing-data)))))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ral-to-change-map (ral last-ref-required)
  (when (data ral)
    (loop for i in (data ral) and j from 0 do
          (let ((data (data i)))
            (typecase data
              (recursive-assoc-list 
               (setf (data (nth j (data ral)))
                 (ral-to-change-map data last-ref-required)
                 (last-ref-required (data (nth j (data ral))))
                 last-ref-required))
              (list (unless (typep (first data) 'change-data)
                      (setf (nth j (data ral))
                        (make-change-data (id i) data))))
              (t (error "change-map::ral-to-change-map: ~
                         Can't make change-data from ~a" 
                        data)))))
    (sc-change-class ral 'change-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-change-map (id last-ref-required cm &key (warn-not-found nil))
  (make-instance 'change-map :data cm :id id 
                 :last-ref-required last-ref-required
                 :warn-not-found warn-not-found
                 :recurse-simple-data nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun change-map-p (thing)
  (typep thing 'change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF change-map.lsp
