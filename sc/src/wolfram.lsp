;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* assoc-list/wolfram
;;; NAME 
;;; rhythm
;;;
;;; File:             wolfram.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist ->
;;;                   circular-sclist -> assoc-list -> wolfram
;;;
;;; Version:          1.0.7
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the wolfram class for 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    7th June 2017, Edinburgh
;;;
;;; $$ Last modified:  09:42:24 Wed Jun  7 2017 BST
;;;
;;; SVN ID: $Id: rhythm.lsp 6210 2017-04-07 11:42:29Z medward2 $
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
(defclass wolfram (assoc-list)
  ;; the data slot holds the indexed generations
  ;; the rules for the next state, given the current and left and right
  ;; neighbours. our default rules our Wolfram's Rule 30 (remember the rule
  ;; number is the binary number indicated by the new cells, in this case 
  ;; 00011110. the triplet first element of each sublist is the state of
  ;; left|current|right and the integer second element is the result.
  ((rules :accessor rules :type list :initarg :rules :initform
          '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 1)
            ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
   (width :accessor width :type integer :initarg :width :initform 100)
   (initial-state :accessor initial-state :type integer :initarg :initial-state
                  :initform 0)))

(defmethod print-object :before ((w wolfram) stream)
  (format stream "~&WOLFRAM: rules: ~a~
                  ~%         width: ~a, initial-state: ~a"
          (rules w) (width w) (initial-state w)))

;;; start is the middle cell of a row; all cells to the left and right of it
;;; will be initial-state
(defmethod generate ((w wolfram) generations &optional (start 1))
  ;; this will always be the current row (i.e. change through each
  ;; generation). it will be copied into the data slot with an index.
  (let ((row (ml (initial-state w) (width w)))
        (1-w (1- (width w))))
    (flet ((next (n)                    ; 0-based
             (let* ((l (if (>= n 1) (nth (1- n) row) (initial-state w)))
                    (c (nth n row))
                    (r (if (>= n 1-w) (initial-state w) (nth (1+ n) row)))
                    (triplet (list l c r)))
               (loop for r in (rules w) do
                    (when (equal (first r) triplet)
                      (return (second r)))
                  finally (error "wolfram::generate: ~a not in rules (~a)"
                                 triplet (rules w))))))
      (setf (nth (floor (width w) 2) row) start)
      (loop for i below generations
         for rowi = (loop for c below (width w) collect (next c))
         do
           (add (list (1+ i) (copy-list row)) w)
           (setq row rowi)))))

;;; on and off could be any object
(defmethod print-matrix ((w wolfram) &key (stream t) (on #\.) (off #\ )
                                       (row-number t))
  (loop for row in (data w) do
       (if row-number
           (format stream "~&~3,'0d: " (id row))
           (terpri stream))
       (loop for cell in (data row) do (princ (if (zerop cell) off on)
                                              stream))))

(defmacro defwolfram (fun rules &optional (w 100) (is 0))
  `(defun ,fun (&optional (width ,w) (initial-state ,is))
     (make-instance 'wolfram :rules ,rules
                    :width width
                    :initial-state initial-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
      .     
     ...    
    ..  .   
   .. ....  
  ..  .   . 
 .. .... ...
..  .    .  
. ....  ... 
. .   ...  .
. .. ..  ...
. .  . ...  
. .... .  . 
. .    .....
. ..  ..    
. . ... .   
. . .   ..  
. . .. .. . 
. . .  .  ..
. . ....... 
. . .      .
. . ..    ..
. . . .  .. 
. . . .... .
. . . .    .
. . . ..  ..
. . . . ... 
. . . . .  .
. . . . ....
. . . . .   
. . . . ..  
. . . . . . 
. . . . . ..   
then last two rows repeat
|#
(defwolfram make-wolfram-r30 ; the default: #b00011110
    '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))


#|
      .     
.....  .....
.    . .    
 ...    ....
 .  ... .   
  . .    ...
.    ... .  
 ... .    ..
 .    ... . 
  ... .    .
. .    ...  
   ... .  ..
.. .    . . 
.   ...    .
 .. .  ...  
 .   . .  ..
  ..    . . 
. . ...    .
    .  ...  
...  . .  ..
.  .    . . 
 .  ...    .
  . .  ...  
.    . .  ..
 ...    . . 
 .  ...    .
  . .  ...  
.    . .  .. 
then last four rows repeat
|#

(defwolfram make-wolfram-r25 ; #b00011001 = 25. 
    '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1)
      ((0 1 1) 1) ((0 1 0) 0) ((0 0 1) 0) ((0 0 0) 1)))

#|
      .     
     ...    
    ... .   
   ... ...  
  ... ... . 
 ... ... ...
... ... ... 
.. ... ... .
. ... ... ..
.... ... .. 
... ... .. .
.. ... .. ..
. ... .. .. 
.... .. .. .
... .. .. ..
.. .. .. .. 
. .. .. .. .    
then last three rows repeat
|#
(defwolfram make-wolfram-r190 ; #b10111110 = 190
    '(((1 1 1) 1) ((1 1 0) 0) ((1 0 1) 1) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))

#|
using 1, the following repeats but not forever
............
            
.          .
..        ..
 ..      .. 
....    ....
   ..  ..   
. ........ .
...      ...
  ..    ..  
.....  .....
    ....    
.  ..  ..  .
or using 0:
      .     
     ...    
    .. ..   
   .......  
  ..     .. 
 ....   ....
..  .. ..  .
............
.          .
..        ..
...      ...
. ..    .. .
.....  .....
.   ....   .
.. ..  .. ..
............

|#
(defwolfram make-wolfram-r126 ; #b01111110
    '(((1 1 1) 0) ((1 1 0) 1) ((1 0 1) 1) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))

#|
non-repeating but lovely patterns
      .     
     . .    
    .   .   
   . . . .  
  .       . 
 . .     . .
.   .   .   
 . . . . .  
.         . 
 .       . .
. .     .   
   .   . .  
  . . .   . 
 .     . . .
. .   .     
   . . .    
  .     .   
 . .   . .  
.   . .   . 
 . .   . . .
.   . .     
 . .   .    
.   . . .   
 . .     .  
.   .   . . 
 . . . .   .
.       . . 
 .     .   .
. .   . . . 
   . .     .
  .   .   . 
 . . . . . .
.           
 .          
. .         
#|
(defwolfram make-wolfram-r82 ; #b01010010
    '(((1 1 1) 0) ((1 1 0) 1) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 0)
      ((0 1 0) 0) ((0 0 1) 1) ((0 0 0) 0)))

(defwolfram make-wolfram-rx ; #b01010010
    '(((1 1 1) 0) ((1 1 0) 1) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 0)
      ((0 1 0) 0) ((0 0 1) 1) ((0 0 0) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF wolfram.lsp
