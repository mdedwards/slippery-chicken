;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* assoc-list/wolfram
;;; NAME 
;;; rhythm
;;;
;;; File:             wolfram.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist ->
;;;                   circular-sclist -> assoc-list -> wolfram
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the wolfram class for one-dimensional
;;;                   binary cellular automata introduced by Stephen Wolfram in
;;;                   1983. The rule number is the decimal representation of
;;;                   the binary number indicated by the eight transition rules
;;;                   (i.e. the results given the triplet test cases; see the
;;;                   rules slot below).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    7th June 2017, Edinburgh
;;;
;;; $$ Last modified:  10:56:40 Sat Aug 24 2024 CEST
;;;
;;; SVN ID: $Id: wolfram.lsp 6210 2017-04-07 11:42:29Z medward2 $
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

(declaim (ftype function make-wolfram-empty-bars splice-in-rest-seqs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the data slot holds the indexed generations
(defclass wolfram (assoc-list)
  ;; the rules for the next state, given the current and left and right
  ;; neighbours. Our default rules are Wolfram's Rule 30 (remember the rule
  ;; number is the binary number indicated by the new cells, in this case 
  ;; 00011110). the triplet first element of each sublist is the state of
  ;; left|current|right and the integer second element is the result.
  ((rules :accessor rules :type list :initarg :rules :initform
          '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 1)
            ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
   ;; MDE Sat Jun 10 13:20:10 2017 -- when we call defwolfram, keep track of
   ;; the new function so we can try all at once with the same parameters, in
   ;; order to compare
   (functions :accessor functions :type list :initform nil :allocation :class)
   (width :accessor width :type integer :initarg :width :initform 100)
   (initial-state :accessor initial-state :type integer :initarg :initial-state
                  :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((w wolfram) stream)
  (format stream "~&WOLFRAM: rules: ~a~
                  ~%         width: ~a, initial-state: ~a~
                  ~%         functions: ~a"
          (rules w) (width w) (initial-state w) (functions w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod verify-and-store :after ((w wolfram))
  (unless (integer>0 (width w))
    (error "wolfram::verify-and-store: the width slot should be an integer ~
            greater than ~%zero, not ~a" (width w)))
  (state-check (initial-state w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((w wolfram))
;;; ****
  (clone-with-new-class w 'wolfram))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((w wolfram) new-class)
  (declare (ignore new-class))
  (let ((al (call-next-method)))
    (setf (slot-value al 'rules) (my-copy-list (rules w))
          (slot-value al 'width) (width w)
          (slot-value al 'initial-state) (initial-state w))
    al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* wolfram/generate
;;; DATE
;;; June 6th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Generate the cellular automata rows using the given rules. N.B. Each time
;;; this method is called, the results (data slot) of the previous calls are
;;; deleted.
;;;
;;; At the beginning, we hava a row. This is a list of length <width>, with all
;;; elements being <initial-state>. However, we set the middle element to the
;;; <start> argument given to this method. We then loop through the elements of
;;; the list and make a triplet out of the element to the left of the current
;;; element (or <initial-state> when we begin the loop), the current element,
;;; and the element to the right of current (or, again, <initial-state> if we're
;;; at the last element of the row). The we loop through the <rules> and when
;;; the current triplet matches the rule we return the 0 or 1 associated with
;;; the triplet in the rule (i.e. the second element of the matching rule).
;;; 
;;; so e.g. we start with:
;;; (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;; 
;;; set the middle element to default of 1 (first generation)
;;; (0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
;;;
;;; then proceed with the loop. Using e.g. the r30 rules:
;;; (((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 1)
;;;              ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0))
;;; 
;;; all those triplet 0s return 0 (because of the last rule) but when we get to
;;; the 1 we first of all have (0 0 1) which returns 1, then (0 1 0) which also
;;; returns 1, and finally--before we only have 0 triplets again--(1 0 0), also
;;; returning 1. So we end up with this as the second generation:
;;; 
;; (0 0 0 0 0 0 1 1 1 0 0 0 0 0 0)
;;;
;;;then this is the third generation, etc.:
;; (0 0 0 0 0 1 1 0 0 1 0 0 0 0 0)
;;;
;;;
;;; ARGUMENTS
;;; - the wolfram object
;;; - the number of generations (rows) to generate
;;; 
;;; OPTIONAL ARGUMENTS
;;; <start> is the value of the middle cell of the first row (or just to the
;;; left of middle, if width is an even number). All cells to the left and
;;; right of it will be in <initial-state>. This should be 0 or 1 only.
;;; 
;;; RETURN VALUE
;;; The data slot, which will be the rows in the form of a list of
;;; named-objects containing the ID (starting from 1) and the list of cell
;;; values.
;;; 
;;; SYNOPSIS
(defmethod generate ((w wolfram) generations &optional (start 1))
;;; ****
  (state-check start)
  (remove-data w)
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
           (setq row rowi))))
  (data w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* wolfram/print-matrix
;;; DATE
;;; June 6th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Print the state of the cells to the terminal or another open stream . On
;;; and off could theoretically be any object but of course single characters
;;; are clearest. See the defwolfram definitions below for examples.
;;; 
;;; ARGUMENTS
;;; - the wolfram object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :stream. The stream to print to. Default t which means it prints to the
;;;   terminal. 
;;; - :on. what to print when a cell has the value 1. Default: the . character
;;; - :off. what to print when a cell has the value 0. Default: space
;;; - :row-number: whether to print row numbers. Default t.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod print-matrix ((w wolfram) &key (stream t) (on #\.) (off #\ )
                                          (row-number nil))
;;; ****
  (loop for row in (data w) do
           (if row-number
               (format stream "~&~3,'0d: " (id row))
               (terpri stream))
           (loop for cell in (data row) do (princ (if (zerop cell) off on)
                                                  stream)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun state-check (arg)
  (unless (and (integerp arg)
               (or (zerop arg)
                   (= 1 arg)))
    (error "wolfram::state-check: this value should be either 0 or 1, ~
            as an integer, not ~a" arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* wolfram/defwolfram
;;; DATE
;;; June 6th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Define your own Wolfram function to create an object with the flavour in
;;; the rules passed. See below for several examples.
;;;
;;; In addition, the name of the newly created function will be added to an
;;; internal list of all functions defined by this macro, so that (try-all) can
;;; be called (see below).
;;; 
;;; ARGUMENTS
;;; - the name of the function you're defining (symbol)
;;; - the list of rules
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the default width (integer>0) for the Wolfram object that will be created
;;; - the default initial-state: 1 or 0.
;;; 
;;; RETURN VALUE
;;; the name of the function (symbol), just as with defun.
;;; 
;;; SYNOPSIS
(defmacro defwolfram (fun rules &optional (w 100) (is 0))
;;; ****
  ;; just to keep track of the new function so we can call in (try-all)
  (let ((tmp (make-instance 'wolfram)))
    (pushnew fun (functions tmp)))
  `(defun ,fun (&optional (width ,w) (initial-state ,is))
     (make-instance 'wolfram :rules ,rules
                    :width width
                    :initial-state initial-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/try-all
;;; DATE
;;; June 10th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Try all of the Wolfram rules defined via defwolfram. An instance of each
;;; rule set defined will be created, the rows will be generated, and
;;; print-matrix will be called. This allows you to see and compare the results
;;; of each rule set using the same parameters.
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :width. The width slot (number of columns) for the Wolfram objects. Any
;;; positive ingeger. Default = 100.
;;; :initial-state. The initial-state (i.e. default) slot for the Wolfram
;;; objects. 0 or 1. Default = 0.
;;; :start. The value of the middle cell of the first row. See generate method
;;; for more details. 0 or 1. Default = 1.
;;; :generations. The number of rows to generate. Any positive integer. Default
;;; = 30.
;;; :stream, :on, :off, :row-number: see print-matrix method.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defun try-all (&key (width 100) (initial-state 0) (start 1) (generations 30)
                  (stream t) (on #\.) (off #\ ) (row-number nil))
;;; ****
  (let ((w (make-instance 'wolfram)))
    (loop for fun in (functions w) do
         (format t "~&~a:" fun)
         (setq w (funcall (symbol-function fun) width initial-state))
         (generate w generations start)
         (print-matrix w :row-number row-number :off off :on on
                       :stream stream)))
  t)
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r30
;;; DATE
;;; June 6th 2017, Edinburgh.
;;; 
;;; DESCRIPTION
;;; This is the default Wolfgram object: #b00011110 = 30
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
;; so width: 12, initial-state: 0
(let ((w (make-wolfram-r30 12 0)))
  (generate w 100)
  (print-matrix w :row-number nil :off "." :on 'X)
  (print-matrix w :row-number nil))
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
;;; SYNOPSIS
(defwolfram make-wolfram-r30 
    '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r25
;;; DATE
;;; June 6th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b00011001 = 25
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
width: 12, initial-state: 0
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

also very nice starting with 0 instead of 1 (no obvious repeats):
(generate w 100 0)
............
.           
 ...........
 .          
  ..........
. .         
   .........
.. .        
.   ........
 .. .       
 .   .......
  .. .      
. .   ......
   .. .     
.. .   .....
.   .. .    
 .. .   ....
 .   .. .   
  .. .   ...
. .   .. .  
   .. .   ..
.. .   .. . 
.   .. .   .
 .. .   ..  
 .   .. . ..
  .. .    . 
. .   ...  .
   .. .  .  
.. .   .  ..
.   ..  . . 
 .. . .    .
 .     ...  
  .... .  ..
. .     . . 
   ....    .
.. .   ...  

|#
;;; SYNOPSIS
(defwolfram make-wolfram-r25
    '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1)
      ((0 1 1) 1) ((0 1 0) 0) ((0 0 1) 0) ((0 0 0) 1)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r190
;;; DATE
;;; June 6th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b10111110 = 190
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
width: 12, initial-state: 0
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
;;; SYNOPSIS
(defwolfram make-wolfram-r190 
    '(((1 1 1) 1) ((1 1 0) 0) ((1 0 1) 1) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r126
;;; DATE
;;; June 6th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b01111110 = 126
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
width: 12, initial-state: 0
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
|#
;;; SYNOPSIS
(defwolfram make-wolfram-r126 
    '(((1 1 1) 0) ((1 1 0) 1) ((1 0 1) 1) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r82
;;; DATE
;;; June 6th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b01010010 = 82
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
width: 12, initial-state: 0 
non-repeating  but lovely patterns
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
|#
;;; SYNOPSIS
(defwolfram make-wolfram-r82 
    '(((1 1 1) 0) ((1 1 0) 1) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 0)
      ((0 1 0) 0) ((0 0 1) 1) ((0 0 0) 0)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r150
;;; DATE
;;; June 7th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b10010110 = 150
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
width: 12, initial-state: 0 
non-repeating  but lovely patterns
      .     
     ...    
    . . .   
   .. . ..  
  .   .   . 
 ... ... ...
. .   .   . 
. .. ... ...
.     .   . 
..   ... ...
  . . .   . 
 .. . .. ...
.   .     . 
.. ...   ...
    . . . . 
   .. . . ..
  .   . .   
 ... .. ..  
. .       . 
. ..     ...
.   .   . . 
.. ... .. ..
    .       
   ...      
  . . .     
 .. . ..    
.   .   .   
.. ... ...  
    .   . . 
   ... .. ..
  . .       
 .. ..      
.     .     
..   ...    
  . . . .   
 .. . . ..  
.   . .   . 
.. .. .. ...
          . 
         ...
        . . 
       .. ..
      .     
     ...    
    . . .   
   .. . ..  
  .   .   . 
 ... ... ...
|#
;;; SYNOPSIS
(defwolfram make-wolfram-r150 
    '(((1 1 1) 1) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 0)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r151
;;; DATE
;;; June 7th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b10010111 = 151
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
      .     
............
 .......... 
. ........ .
.  ......  .
... .... ...
 .   ..   . 
.....  .....
 ... .. ... 
. .      . .
. ........ .
.  ......  .
then the last 7 lines repeat
|#
(defwolfram make-wolfram-r151
    '(((1 1 1) 1) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 0)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 1)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r159
;;; DATE
;;; June 7th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b10011111 = 159
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
      .     
............
........... 
.......... .
.........  .
........ ...
.......  .. 
...... ... .
.....  ..  .
.... ... ...
...  ..  .. 
.. ... ... .
.  ..  ..  .
then the last 4 lines repeat
|#
(defwolfram make-wolfram-r159
    '(((1 1 1) 1) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 1)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r62
;;; DATE
;;; June 7th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b00111110 = 62
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
      .     
     ...    
    ..  .   
   .. ....  
  .. ..   . 
 .. .. . ...
.. .. ....  
. .. ..   . 
... .. . ...
.  .. ....  
.... ..   . 
.   .. . ...
.. .. ....  
. .. ..   . 
... .. . ...
.  .. .... 
then the last 6 lines repeat
|#
(defwolfram make-wolfram-r62
    '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 1) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r93
;;; DATE
;;; June 7th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b01011101 = 93
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
      .     
..... ......
.   ...    .
... . .... .
. .....  ...
...   .. . .
. ... ......
... ...    .
. ... .... .
... ...  ...
. ... .. . .
... ........
. ...      .
... ...... .
. ...    ...
... .... . .
. ...  .....
... .. .   .
. ........ .
...      ...
. ...... . .
...    .....
. .... .   .
...  ..... .
. .. .   ...
........ . .
.      .....
...... .   .
.    ..... .
.... .   ...
.  ..... . .
.. .   .....
...... .   .
.    ..... .
then the last 5 lines repeat
|#
(defwolfram make-wolfram-r93
    '(((1 1 1) 0) ((1 1 0) 0) ((1 0 1) 0) ((1 0 0) 1) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 0) ((0 0 0) 1)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r110
;;; DATE
;;; June 7th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b01101110 = 110
;;; This is the one Wolfram suggests might be Turing complete. This was proved
;;; in 2000. "Rule 110, like the Game of Life, exhibits what Wolfram calls
;;; "Class 4 behavior", which is neither completely stable nor completely
;;; chaotic. Localized structures appear and interact in various
;;; complicated-looking ways" (https://en.wikipedia.org/wiki/Rule_110)
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
so this one only goes up to column 7 when called with width of 12:
      .     
     ..     
    ...     
   .. .     
  .....     
 ..   .     
...  ..     
. . ...     
..... .     
.   ...     
.  .. .     
. .....     
...   .     
. .  ..     
... ...     
. ... .      
then the last 2 lines repeat

but called with width of 60:
                              .                             
                             ..                             
                            ...                             
                           .. .                             
                          .....                             
                         ..   .                             
                        ...  ..                             
                       .. . ...                             
                      ....... .                             
                     ..     ...                             
                    ...    .. .                             
                   .. .   .....                             
                  .....  ..   .                             
                 ..   . ...  ..                             
                ...  .... . ...                             
               .. . ..  ..... .                             
              ........ ..   ...                             
             ..      ....  .. .                             
            ...     ..  . .....                             
           .. .    ... ....   .                             
          .....   .. ...  .  ..                             
         ..   .  ..... . .. ...                             
        ...  .. ..   ........ .                             
       .. . ......  ..      ...                             
      .......    . ...     .. .                             
     ..     .   .... .    .....                             
    ...    ..  ..  ...   ..   .                             
   .. .   ... ... .. .  ...  ..                             
  .....  .. ... ...... .. . ...                             
 ..   . ..... ...    ........ .                             
...  ....   ... .   ..      ...                             
. . ..  .  .. ...  ...     .. .                             
...... .. ..... . .. .    .....                             
.    ......   ........   ..   .                             
.   ..    .  ..      .  ...  ..                             
.  ...   .. ...     .. .. . ...                             
. .. .  ..... .    .......... .                             
...... ..   ...   ..        ...                             
.    ....  .. .  ...       .. .                             
.   ..  . ..... .. .      .....                             
.  ... ....   ......     ..   .                             
. .. ...  .  ..    .    ...  ..                             
...... . .. ...   ..   .. . ...                             
.    ........ .  ...  ....... .                             
.   ..      ... .. . ..     ...                             
.  ...     .. .........    .. .                             
. .. .    .....       .   .....                             
......   ..   .      ..  ..   .                             
.    .  ...  ..     ... ...  ..                             
.   .. .. . ...    .. ... . ...                             
.  .......... .   ..... ..... .                             
. ..        ...  ..   ...   ...                             
....       .. . ...  .. .  .. .                             
.  .      ....... . ..... .....                             
. ..     ..     .....   ...   .                             
|#
(defwolfram make-wolfram-r110
    '(((1 1 1) 0) ((1 1 0) 1) ((1 0 1) 1) ((1 0 0) 0) ((0 1 1) 1)
      ((0 1 0) 1) ((0 0 1) 1) ((0 0 0) 0)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* wolfram/make-wolfram-r73
;;; DATE
;;; June 8th 2017, Edinburgh. 
;;; 
;;; DESCRIPTION
;;; #b01001001 = 73
;;; Suggested by Orestis Papadopoulos
;;; 
;;; ARGUMENTS
;;; none required
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the width of a row. Default = 100.
;;; - the initial state of the object. Default = 0.
;;; 
;;; RETURN VALUE
;;; the function name as a symbol but when called a wolfram object with the
;;; given rules. 
;;; 
;;; EXAMPLE
#|
      .     
.....   ....
.   . . .  .
  .         
.   ........
  . .      .
.     ....  
  ... .  . .
. . .       
      ......
..... .    .
.   .   ..  
  .   . .. .
then the last 2 lines repeat
|#
(defwolfram make-wolfram-r73
    '(((1 1 1) 0) ((1 1 0) 1) ((1 0 1) 0) ((1 0 0) 0) ((0 1 1) 1)
      ((0 1 0) 0) ((0 0 1) 0) ((0 0 0) 1)))
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF wolfram.lsp
