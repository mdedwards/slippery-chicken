;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* change-map/instrument-change-map
;;; NAME 
;;; instrument-change-map
;;;
;;; File:             instrument-change-map
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   sc-map -> change-map -> instrument-change-map
;;;
;;; Version:          0.91
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Extends change-map to check that instruments defined in
;;;                   the map have data for the first bar of the first section.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    12th April 2002
;;;
;;; $$ Last modified: 19:00:50 Fri Dec  9 2011 ICT
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

(defclass instrument-change-map (change-map)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((icm instrument-change-map) stream)
  (format stream "~%INSTRUMENT-CHANGE-MAP: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((icm instrument-change-map))
  (clone-with-new-class icm 'instrument-change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instrument-for-first-bar ((icm instrument-change-map)
                                     player first-section-ref)
  (unless (cm-get-data icm (econs first-section-ref player))
    (error "instrument-change-map::instrument-for-first-bar: ~
            ~a doubles but the instrument for the first bar has not been set!"
           player)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return the first data in the whole map for the given instrument

(defmethod get-first-for-instrument ((icm instrument-change-map)
                                     instrument)
  (when (data icm)
    (get-change-data (get-data instrument (data (first (data icm))))
                     1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instrument-change-map (id icm &key (warn-not-found nil))
  (make-instance 'instrument-change-map :data icm :id id 
                 :last-ref-required t
                 :warn-not-found warn-not-found
                 :recurse-simple-data nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instrument-change-map-p (thing)
  (typep thing 'instrument-change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF instrument-change-map.lsp
