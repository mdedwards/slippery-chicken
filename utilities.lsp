;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/utilities
;;; NAME 
;;; utilities
;;;
;;; File:             utilities.lsp
;;;
;;; Class Hierarchy:  none: no classes defined
;;;
;;; Version:          1.0.0-beta2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Various helper functions of a general nature.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    June 24th 2002
;;;
;;; $$ Last modified: 17:47:02 Thu May 17 2012 BST
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; separator is what separates mins and secs; default is e.g. 12:06
;;; same-width t would make sure all those minutes under 10 are written in the
;;; format e.g. 00:12.2 

;;; ****f* utilities/secs-to-mins-secs
;;; DESCRIPTION
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
(defun secs-to-mins-secs (seconds &key (separator ":") (same-width nil))
;;; ****
  (unless (and (numberp seconds) (>= seconds 0))
    (error "utilities::secs-to-mins-secs: ~a should be a number > 0." seconds))
  (multiple-value-bind
        (minutes seconds)
      (floor seconds 60)
    ;; the formatting of the seconds results in rounding up 59.99999 to 60.0
    ;; which is not what we want...  
    (when (> seconds 59.999)
      (setf seconds 0.0)
      (incf minutes))
    (if same-width
        (format nil "~2,'0d~a~3,2$" minutes separator seconds)
        (if (> minutes 0)
            (format nil "~d~a~3,2$" minutes separator seconds)
            (format nil "~,3f" seconds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 12:08:59 BST 2012: Added robodc entry

;;; ****f* utilities/mins-secs-to-secs
;;; DESCRIPTION
;;; Derive the number of seconds from a minutes-seconds value that is indicated
;;; as a two-item list in the form '(minutes seconds).
;;; 
;;; ARGUMENTS
;;; - A two-item list of integers in the form '(minutes seconds).
;;; 
;;; RETURN VALUE
;;; A decimal number that is a number in seconds.
;;; 
;;; EXAMPLE
#|
(mins-secs-to-secs '(2 1))

=> 121.0

|#
;;; SYNOPSIS
(defun mins-secs-to-secs (list)
;;; ****
  (flet ((secs-msecs (secs msecs)
           (when (or (> secs 60)
                     (> msecs 1000))
             (error "utilities::mins-secs-to-secs: secs = ~a ~
                               millisecs = ~a???" secs msecs))))
    (cond ((not list) nil)
          ((numberp list) list)
          ((= 2 (length list))
           (let ((mins (first list))
                 (secs (second list)))
             (secs-msecs secs 0)
             (+ secs (* 60.0 mins))))
          ((= 3 (length list))
           (let ((mins (first list))
                 (secs (second list))
                 (msecs (third list)))
             (secs-msecs secs msecs)
             (+ secs (/ msecs 1000.0) (* 60.0 mins))))
          (t (error "utilities::mins-secs-to-secs: arg must be a 2- or ~
                         3-element list (mins secs [millisecs]): ~a"
                    list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun float-int-p (x &optional (tolerance 0.000001d0))
  (equal-within-tolerance 0.0 (nth-value 1 (round x)) tolerance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integer>0 (x)
  (and (integerp x) (> x 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integer>=0 (x)
  (and (integerp x) (>= x 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integer<0 (x)
  (and (integerp x) (< x 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integer<=0 (x)
  (and (integerp x) (<= x 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integer-between (x lower upper)
  (and (integerp x)
       (>= x lower)
       (<= x upper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 12:15:03 BST 2012: Added robodoc entry

;;; ****f* utilities/string-replace
;;; DESCRIPTION
;;; Replace specified segments of a string with a new specified string.
;;; 
;;; ARGUMENTS
;;; - A string that is the string segment to be replaced.
;;; - A string that is the string with which the specified string segment is to
;;;   be replaced.
;;; - The string in which the specified segment is to be sought and replaced.
;;; 
;;; RETURN VALUE
;;; A string.
;;; 
;;; EXAMPLE
#|
(string-replace "flat" "\\flat" "bflat clarinet")

=> "b\\flat clarinet"

|#
;;; SYNOPSIS
(defun string-replace (what with string)
;;; **** ; all strings
  (let ((pos (search what string))
        (wlen (length what)))
    (when pos 
      (concatenate 'string
                   (subseq string 0 pos)
                   with
                   (subseq string (+ pos wlen))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strcpy (string)
  (if (stringp string)
      (coerce (make-array (length string) :initial-contents string)
               'string)
    (error "utilities::strcpy: Argument must be a string: ~a" string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 12:21:30 BST 2012: Added robodoc entry

;;; ****f* utilities/econs
;;; DESCRIPTION
;;; Add a specified element to the end of an existing list.
;;; 
;;; ARGUMENTS
;;; - A list.
;;; - An element to add to the end of the list.
;;; 
;;; RETURN VALUE
;;; A new list.
;;; 
;;; EXAMPLE
#|
(econs '(1 2 3 4) 5)

=>  '(1 2 3 4 5)

|#
;;; SYNOPSIS
(defun econs (list new-back)
;;; ****
  (unless (listp list)
    (error "utilities::econs: first argument must be a list: ~a" list))
  (append list (list new-back)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm-package (key &optional (package :sc))
  (if (symbolp key)
      (values (intern (string key) package))
    key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun object-is-nil? (object function parameter)
  (unless object
    (error "utilities::object-is-nil?: In function ~a, parameter ~a is NIL!"
           function parameter)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e.g. (GET-SUBLIST-INDICES '((1 2) (3 4) (5 6 7) (8 9) (10))) ->
;;; (0 2 4 7 9)

;;; ****f* utilities/get-sublist-indices
;;; DESCRIPTION
;;; Get the starting position of sublists within a list as though the complete
;;; set of items were a flat list.
;;; 
;;; ARGUMENTS
;;; - A list of lists.
;;; 
;;; RETURN VALUE
;;; A list of integers that are the indices of the sublists.
;;; 
;;; EXAMPLE
#|
(get-sublist-indices '((1 2) (3 4 5 6) (7 8 9) (10 11 12 13 14) (15)))

=> (0 2 6 9 14)

|#
;;; SYNOPSIS
(defun get-sublist-indices (list)
;;; ****
  (loop 
     with index = 0
     for sublist in list collect index
     do (incf index (length sublist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 12:32:31 BST 2012: Added robodoc entry

;;; In order to flatten then recreate a list of lists, we need to know
;;; the index in the flattened list of the first element of each sublist.
;;; e.g. (GET-SUBLIST-INDICES '((1 2) (3 4) (5 6 7) (8 9) (10))) ->
;;; (0 2 4 7 9)

;;; ****f* utilities/get-sublist-lengths
;;; DESCRIPTION
;;; Get the lengths of all sublists in a given list.
;;; 
;;; ARGUMENTS
;;; - A list of lists.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to first remove zeros caused by empty
;;;   sublists from the result.
;;; 
;;; RETURN VALUE
;;; A list of integers.
;;; 
;;; EXAMPLE
#|
;; Straightforward usage allows zeros in the result
(get-sublist-lengths '((1 2) (3 4 5 6) (7 8 9) (10 11 12 13 14) ()))

=> (2 4 3 5 0)

;; Setting the optional argument to T removes zeros from the result

(get-sublist-lengths '((1 2) (3 4 5 6) (7 8 9) (10 11 12 13 14) ()) t)

=> (2 4 3 5)

|#
;;; SYNOPSIS
(defun get-sublist-lengths (list &optional (remove-zeros nil))
;;; ****
  (let ((result (loop for sublist in list collect (length sublist))))
    (if remove-zeros
        (remove 0 result)
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 12:40:53 BST 2012: Conformed robodoc entry

;;; ****f* utilities/all-members
;;; DESCRIPTION

;;; Find out whether the members of the list given as the second argument are
;;; all present in the list given as the first argument.
;;; 
;;; ARGUMENTS     
;;; - A list in which the members of the second argument will be sought. 
;;; - A list whose members will be sought in the first argument.
;;; 
;;; OPTIONAL ARGUMENT
;;; - A comparison function.
;;;
;;; RETURN VALUE  
;;; T or NIL.
;;; 
;;; EXAMPLE
#|
(all-members '(1 2 3 4 5 6 7) '(1 2 3 7))

=> T

|#
;;;
;;; SYNOPSIS
(defun all-members (list test-list &optional (test #'equal))
;;; ****
  (loop 
     with len = (length test-list)
     with count = 0
     for el in test-list
     do
       (when (member el list :test test)
         (incf count))
     finally
       (when (= count len)
         (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 12:45:48 BST 2012: Added robodoc entry

;;; e.g (split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 2 3 2 1)) ->
;;; ((1 2) (3 4) (5 6 7) (8 9) (10))

;;; ****f* utilities/split-into-sub-groups
;;; DESCRIPTION
;;; Create a new list consisting of sublists made from the elements of the
;;; original flat list, whose lengths are determined by the second argument to
;;; the function.
;;;
;;; NB: The lengths given in the second argument are not required to add up to
;;;     the length of the original list. If their sum is less than the original
;;;     list, the resulting list of sublists will only contain a segment of the
;;;     original elements. If their sum is greater than the length of the
;;;     original list, the last sublist in the new list will be shorter than
;;;     the corresponding group value.
;;; 
;;; ARGUMENTS
;;; - A flat list.
;;; - A list of integers that are the lengths of the consecutive subgroups
;;;   into which the original list is to be divided. 
;;; 
;;; RETURN VALUE
;;; A list of lists.
;;; 
;;; EXAMPLE
#|
;; Used with a list of subgroup lengths whose sum is equal to the length of the
;; original list
(split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 2 3 2 1))

=> ((1 2) (3 4) (5 6 7) (8 9) (10))

;; Used with a list of subgroup lengths whose sum is less than the length of the
;; original list 
(split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 1))

=> ((1 2) (3))

;; Used with a list of subgroup lengths whose sum is greater than the length of
;; the original list
(split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 3 17))

=> ((1 2) (3 4 5) (6 7 8 9 10))

|#
;;; SYNOPSIS
(defun split-into-sub-groups (list groups)
;;; ****
  (when (member 0 groups)
    (error "utilities::split-into-subgroups: 0? ~a" groups))
  (loop 
     for num in groups 
     with scl = (make-sclist list)
     with count = 0
     ;; it could be that one instrument in a group is not being printed so the
     ;; list length will be less than the subgroup being asked for  
     collect (sc-subseq scl count (min (sclist-length scl) (+ count num)))
     do (incf count num)
     while (< count (sclist-length scl))))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 13:55:49 BST 2012: Added robodoc entry

;;; Like above but just one length is given and this will be used for sublists
;;; e.g. (split-into-sub-groups2 '(1 2 3 4 5 6 7 8 9) 4) ->
;;; ((1 2 3 4) (5 6 7 8) (9))

;;; ****f* utilities/split-into-sub-groups2
;;; DESCRIPTION
;;; Create a new list of lists by splitting the original flat list into
;;; sublists of the specified length.
;;;
;;; NB: The length given as the second argument is not required to be fit
;;;     evenly into the length of the original flat list. If the original list
;;;     is not evenly divisible by the specified length, the resulting list of
;;;     sublists will contain a final sublist of a different length.
;;; 
;;; ARGUMENTS
;;; - A flat list.
;;; - An integer that is the length of each of the sublists to be created.
;;; 
;;; RETURN VALUE
;;; A list of lists.
;;; 
;;; EXAMPLE
#|

;; The second argument fits evenly into the length of the original list. 
(split-into-sub-groups2 '(1 2 3 4 5 6 7 8 9 10 11 12) 3)

=> ((1 2 3) (4 5 6) (7 8 9) (10 11 12))

;; The second argument does not fit evenly into the length of the original
;; list. 

(split-into-sub-groups2 '(1 2 3 4 5 6 7 8 9 10 11 12) 5)

=> ((1 2 3 4 5) (6 7 8 9 10) (11 12))

|#
;;; SYNOPSIS
(defun split-into-sub-groups2 (list length)
;;; ****
  (loop for i from 0 by length while list collect
        (loop repeat length while list collect (pop list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 14:04:19 BST 2012: Added robodoc entry

;;; Same as above but put any left over elements in the penultimate sub-list
;;; e.g. (split-into-sub-groups3 '(1 2 3 4 5 6 7 8 9) 4) ->
;;; ((1 2 3 4) (5 6 7 8 9))

;;; ****f* utilities/split-into-sub-groups3
;;; DESCRIPTION
;;; Split a given flat list into sublists of the specified length, putting any
;;; remaining elements, if there are any, into the last sublist.
;;; 
;;; ARGUMENTS
;;; - A flat list.
;;; - An integer that is the length of the new sublists.
;;; 
;;; RETURN VALUE
;;; A list of lists.
;;; 
;;; EXAMPLE
#|
(split-into-sub-groups3 '(1 2 3 4 5 6 7 8 9 10 11 12) 3)

=> ((1 2 3) (4 5 6) (7 8 9) (10 11 12))

(split-into-sub-groups3 '(1 2 3 4 5 6 7 8 9 10 11 12) 5)

=> ((1 2 3 4 5) (6 7 8 9 10 11 12))

|#
;;; SYNOPSIS
(defun split-into-sub-groups3 (list length)
;;; ****
  ;; we work in groups of 7 but stick any remainder onto
  ;; the end of the last loop 
  (if (<= (length list) length)
      (list list)
    (let* ((sub-groups (split-into-sub-groups2 list length))
           (len-sgs (length sub-groups))
           (last (first (last sub-groups))))
      (if (= (length last) length)
          sub-groups
        (progn
          (setf (nth (- len-sgs 2) sub-groups)
            (append (nth (- len-sgs 2) sub-groups)
                    last))
          (butlast sub-groups))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whole-num-p (float &optional (allow-tolerance nil))
  (let ((test (mod float 1.0)))
    (if allow-tolerance
        (or (equal-within-tolerance 0.0 test) ;; 0.00001)
            (equal-within-tolerance 1.0 test))
        (values (zerop test)
                float))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 14:09:30 BST 2012: Added robodoc entry

;;; ****f* utilities/power-of-2
;;; DESCRIPTION
;;; Test whether the specified number is a power of two and return the
;;; logarithm of the specified number to base 2.
;;;
;;; This method returns two values: T or NIL for the test and a decimal that is
;;; the logarithm of the specified number to base 2.
;;; 
;;; ARGUMENTS
;;; - A number.
;;; 
;;; RETURN VALUE
;;; Two values: T or NIL for the test and a decimal number that is the
;;; logarithm of the specified number to base 2.
;;; 
;;; EXAMPLE
#|
(power-of-2 16)

=> T, 4.0

(power-of-2 17.3)

=> NIL, 4.1127

|#
;;; SYNOPSIS
(defun power-of-2 (float)
;;; ****
  (whole-num-p (log float 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 14:16:28 BST 2012: Added robodoc entry

;; returns the power of 2 <= num

;;; ****f* utilities/nearest-power-of-2
;;; DESCRIPTION
;;; Return the closest number to the specified value that is a power of two but
;;; not greater than the specified value.
;;; 
;;; ARGUMENTS
;;; - A number.
;;; 
;;; RETURN VALUE
;;; An integer that is a power of two.
;;; 
;;; EXAMPLE
#|
(nearest-power-of-2 31)

=> 16

(nearest-power-of-2 32)

=> 32

(nearest-power-of-2 33)

=> 32

|#
;;; SYNOPSIS
(defun nearest-power-of-2 (num)
;;; ****
  (if (power-of-2 num)
      num
    (loop with p = 1 do
          (if (> p num)
              (return (/ p 2))
            (setf p (* p 2))))))                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 14:28:28 BST 2012: Added robodoc entry

;;; ****f* utilities/flatten
;;; DESCRIPTION
;;; Return a list of nested lists of any depth as a flat list.
;;; 
;;; ARGUMENTS
;;; - A list of nested lists.
;;; 
;;; RETURN VALUE
;;; A flat list.
;;; 
;;; EXAMPLE
#|
(flatten '((1 (2 3 4) (5 (6 7) (8 9 10 (11) 12)) 13) 14 15 (16 17)))

=> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)

|#
;;; SYNOPSIS
(defun flatten (nested-list)
;;; ****
  (cond ((null nested-list) nil)
        ((atom nested-list) (list nested-list))
        (t (append (flatten (first nested-list))
                   (flatten (rest nested-list))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 14:31:47 BST 2012: Added robodoc entry

;;; Sometimes a series of calculations ends up with floating-point errors
;;; making your variable a little smaller or larger than you expect.  So use
;;; this test instead of =
;;; MDE Mon Jan  2 11:30:32 2012 -- the following returns NIL!
;;; (equal-within-tolerance
;;;   (coerce 261.63 'short-float) 
;;;   (coerce 261.63 'double-float))
;;; might be best at some point to calculate with the max relative error (%)
;;; but for now we'll just have to fiddle tolerance (epsilon) e.g.
;;; (equal-within-tolerance
;;;   (coerce 261.63 'short-float) 
;;;   (coerce 261.63 'double-float) 0.00001) => T

;;; ****f* utilities/equal-within-tolerance
;;; DESCRIPTION

;;; Test whether the difference between two decimal numbers falls within a
;;; specified tolerance.
;;;
;;; This test is designed to compensate for calculation discrepancies caused by
;;; floating-point errors (such as 2.0 vs. 1.9999997), in which the equations
;;; should yield equal numbers. It is intended to be used in place of = in such
;;; circumstances.
;;; 
;;; ARGUMENTS
;;; - A first number.
;;; - A second number.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A decimal value that is the maximum difference allowed between the two
;;;   numbers that will still return T. Default = 0.000001d0.
;;; 
;;; RETURN VALUE
;;; T if the two tested numbers are equal within the specified tolerance,
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; An example of floating-point error
(loop for i from 0.0 below 1.1 by 0.1 collect i)

=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.70000005 0.8000001 0.9000001 1.0000001) 

;; Using =
(loop for i from 0.0 below 1.1 by 0.1 
   for j in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
   collect (= i j))

=> (T T T T T T T NIL NIL NIL NIL)

;; Using equal-within-tolerance
(loop for i from 0.0 below 1.1 by 0.1 
   for j in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
   collect (equal-within-tolerance i j))

=> (T T T T T T T T T T T)

|#
;;; SYNOPSIS
(defun equal-within-tolerance (a b &optional (tolerance 0.000001d0))
;;; ****
  ;; BTW coercing a and b to 'double-float doesn't help if they are of
  ;; different float types to begin with.
  (<= (abs (- a b)) tolerance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Mar 19 19:39:55 2012 

;;; ****f* utilities/decimal-places
;;; DATE
;;; 19-Mar-2012
;;;
;;; DESCRIPTION
;;; Round the given number to the specified number of decimal places.
;;; 
;;; ARGUMENTS
;;; - A number.
;;; - An integer that is the number of decimal places to which to round the
;;;   given number.
;;; 
;;; RETURN VALUE
;;; A decimal number.
;;; 
;;; EXAMPLE
#|
(decimal-places 1.1478349092347 2)

=> 1.15

|#
;;; SYNOPSIS
(defun decimal-places (num places)
;;; ****
  (let ((pow (float (expt 10 places))))
    (/ (round num (/ pow)) pow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 15:22:55 BST 2012: Added robodoc entry

;;; ****f* utilities/almost-zero
;;; DESCRIPTION
;;; Return T if a given decimal is within 0.000001 of 0.0.
;;; 
;;; ARGUMENTS
;;; - A number.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is a user-specified difference for the comparison test. 
;;; 
;;; RETURN VALUE
;;; T if the number is within the tolerance difference to zero, otherwise NIL. 
;;; 
;;; EXAMPLE
#|
(almost-zero 0.0000007)

=> T

|#
;;; SYNOPSIS
(defun almost-zero (num &optional (tolerance 0.000001))
;;; ****
  (equal-within-tolerance num 0.0 tolerance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 15:28:17 BST 2012: Added robodoc entry

;;; (round-if-close 23.99999) -> 24
;;; (round-if-close 23.00000001) -> 23
;;; (round-if-close 23.0001) -> 23.0001
;;; (round-if-close 22) -> 22

;;; ****f* utilities/round-if-close
;;; DESCRIPTION
;;; Round a decimal number if it is within a given tolerance to the next whole
;;; number. 
;;; 
;;; ARGUMENTS
;;; - A decimal number.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - If the given number is this amount or less than the nearest whole number,
;;;   round the given number to the nearest whole number.
;;; 
;;; RETURN VALUE
;;; If the given number is within the tolerance, return the number, otherwise
;;; return the nearest whole number.
;;; 
;;; EXAMPLE
#|
(round-if-close 1.999998)

=> 1.999998

(round-if-close 1.999999)

=> 2

|#
;;; SYNOPSIS
(defun round-if-close (num &optional (tolerance 0.000001))
;;; ****
  (multiple-value-bind
        (int rem)
      (round num)
    (if (< (abs rem) tolerance)
        int
        num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minus-last-slash (string)
  (delete #\/ string :count 1 :from-end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trailing-slash (path)
  (when (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
      (format nil "~a/" path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 15:36:38 BST 2012: Added robodoc entry

;;; ****f* utilities/sort-symbol-list
;;; DESCRIPTION
;;; Sort a list of symbols alphabetically ascending, case-insensitive. 
;;; 
;;; ARGUMENTS
;;; A list of symbols.
;;; 
;;; RETURN VALUE
;;; The same list of symbols sorted alphabetically ascending, case-insensitive.  
;;; 
;;; EXAMPLE
#|
(sort-symbol-list '(Lorem ipsum dolor sit amet consectetur adipiscing))

=> (ADIPISCING AMET CONSECTETUR DOLOR IPSUM LOREM SIT)

|#
;;; SYNOPSIS
(defun sort-symbol-list (list)
;;; ****
  (sort list
        #'(lambda (x y) 
            (string-lessp (string x) (string y)))))
                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 15:41:59 BST 2012: Added robodoc entry

;;; (1 2 3 4 ".") -> 1.2.3.4

;;;(format nil "~{~a~a~^~}" separator list))

;;; ****f* utilities/list-to-string
;;; DESCRIPTION
;;; Convert a list to a string.
;;; 
;;; ARGUMENTS
;;; - A list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A string that will serve as a separator between the elements. 
;;;   Default = " ".
;;; - T or NIL to indicate whether a list value of NIL is to be returned as
;;;   "NIL" or NIL. T = "NIL" as a string. Default = T.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;;; Using defaults
(list-to-string '(1 2 3 4 5))

=> "1 2 3 4 5"

;;; Specifying a different separator
(list-to-string '(1 2 3 4 5) "-")

=> "1-2-3-4-5"

;;; A NIL list returns "NIL" as a string by default
(list-to-string NIL)

=> "nil"

;;; Setting the second optional argument to NIL returns a NIL list as NIL
;;; rather than as "NIL" as a string
(list-to-string NIL "" nil)

=> NIL

|#
;;; SYNOPSIS
(defun list-to-string (list &optional (separator " ") (nil-as-string t))
;;; ****
  (if list
      (if (listp list)
          (loop for i in list with result = "" do
               (setf result (format nil "~a~a~a" result separator i))
             finally (return (subseq result (length separator))))
          list)
      (if nil-as-string
          "nil"
          nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns whether all the elements in the list are atoms (i.e. no sublists)

(defun simple-listp (list)
  (when (listp list)
    (loop for i in list unless (atom i) do (return nil) finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-of-numbers-p (list)
  (when (listp list)
    (loop for i in list
       unless (numberp i) 
       do (return nil)
       finally (return t))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. we have to define a combine method for any classes where we need a
;;; combine-all call 

(defun combine-all (objects)
  (let ((result (clone (first objects))))
    (loop for rs in (cdr objects) do
         (setf result (combine result rs)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 15:52:17 BST 2012: Added robodoc entry

;;; ****f* utilities/semitones
;;; DESCRIPTION
;;; Return the sample-rate conversion factor required for transposing an audio
;;; file by a specific number of semitones. The number of semitones can be
;;; given as a decimal number, and may be positive or negative.
;;; 
;;; ARGUMENTS
;;; - A number of semitones.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is the factor required to transpose by an octave. 
;;;   Default = 2.0.
;;; - A number that is the number of semitones per octave. Default = 12. 
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE
#|
;;; Usage with default values
(semitones 3)

=> 1.1892071

;;; Specifying a different number of semitones per octave
(semitones 3 2.0 13)

=> 1.1734605

;;; Specifying a different factor for transposing by an octave 
(semitones 3 4.0)

=> 1.4142135

;;; Fractional semitones are allowed
(semitones 3.72)

=> 1.2397077

;;; Negative semitones are also allowed
(semitones -3.72)

=> 0.80664176

|#
;;; SYNOPSIS
(defun semitones (st &optional (octave-size 2.0) (divisions-per-octave 12))
;;; ****
  "semitones just returns the right src from the given 
   semitone transposition. e.g. (semitones 12) => 2.0"
  (expt octave-size (/ st divisions-per-octave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 16:03:22 BST 2012: Added robodoc entry

;;; ****f* utilities/srt
;;; DESCRIPTION
;;; Return the semitone transposition for a given sampling rate conversion
;;; factor.
;;; 
;;; ARGUMENTS
;;; - A number that is a sample-rate conversion factor.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is the factor required for transposing one octave. 
;;; - A number that is the number of scale degrees in an octave.
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE
#|
;;; Using the defaults
(srt 1.73)

=> 9.4893

;;; Using a sample-rate conversion factor of 4.0 for the octave and specifying
;;; 13 divisions of the octave
(srt 1.73 4.0 13)

=> 5.14

|#
;;; SYNOPSIS
(let ((last8vesize 0)
      (log8ve 0.0)) ;; so we don't have to recalculate each time
  (defun srt (srt &optional (octave-size 2.0) (divisions-per-octave 12)
              ;; MDE Tue Feb  7 16:59:45 2012 -- round so we don't get tiny
              ;; fractions of semitones due to float inaccuracies?
              (round-to 0.0001))
;;; ****
    (unless (= octave-size last8vesize)
      (setf last8vesize octave-size
            log8ve (log octave-size 10.0)))

    (let ((result (/ (* divisions-per-octave (log srt 10.0))
                     log8ve)))
      (if round-to
          (/ (round result round-to) (/ round-to))
          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 16:09:37 BST 2012: Added robodoc entry

;;; ****f* utilities/replace-elements
;;; DESCRIPTION
;;; Replace the elements in list between start and end (inclusive) with the new
;;; list.
;;; 
;;; ARGUMENTS
;;; - A list.
;;; - An integer that is first position of the segment of the original list to
;;;   be replaced.
;;; - An integer that is the last position of the segment of the original list
;;;   to be replaced.
;;; - A list that is to replace the specified segment of the original
;;;   list. This list can be of a different length than that of the segment
;;;   of the original specified by the start and end positions.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|

(replace-elements '(1 2 3 4 5 6 7 8 9) 3 7 '(dog cat goldfish))

=> (1 2 3 DOG CAT GOLDFISH 9)

|#
;;; SYNOPSIS
(defun replace-elements (list start end new)
;;; ****
  (unless (listp new)
    (setf new (list new)))
  (when (or (>= start end)
            (>= end (length list)))
    (error "~a~%utilities::replace-elements: bad indices: ~a ~a" 
           list start end))
  (let ((before (subseq list 0 start))
        (after (subseq list (1+ end))))
    (append before new after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 16:15:26 BST 2012: Added robodoc entry

;;; ****f* utilities/splice
;;; DESCRIPTION
;;; Insert the elements of a first list into a second list beginning at a
;;; specified index (0-based).
;;; 
;;; ARGUMENTS
;;; - A list that contains the elements to be inserted into the second list.
;;; - A list into which the elements of the first argument are to be inserted. 
;;; - An integer that is the index within the second list where the elements
;;;   are to be inserted.
;;; 
;;; RETURN VALUE
;;; - A list.
;;; 
;;; EXAMPLE
#|
(splice '(dog cat goldfish) '(1 2 3 4 5 6 7 8 9) 3)

=> (1 2 3 DOG CAT GOLDFISH 4 5 6 7 8 9)

|#
;;;
;;; SYNOPSIS
(defun splice (elements into-list where)
;;; ****
  (unless (and (listp elements)
               (listp into-list))
    (error "splice: arguments 1 and 2 must be lists!: ~a ~a"
           elements into-list))
  (unless (and (integerp where)
               (<= where (length into-list)))
    (error "~%~a~%splice: argument 3 must be an integer <= the length of ~
            argument 2: where = ~a (length = ~a)" 
           into-list where (length into-list)))
  (let ((before (subseq into-list 0 where))
        (after (nthcdr where into-list)))
    (append before elements after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 16:21:11 BST 2012: Added robodoc entry

;;; ****f* utilities/remove-elements
;;; DESCRIPTION
;;; Remove a specified number of elements from a given list starting at a
;;; specified position (0-based) within the list.
;;; 
;;; ARGUMENTS
;;; - A list.
;;; - An integer that is the 0-based position within that list that will be the
;;;   first element to be removed.
;;; - An integer that is the number of elements to remove.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
(remove-elements '(1 2 3 4 5 6 7) 2 4)

=> (1 2 7)

|#
;;;
;;; SYNOPSIS
(defun remove-elements (list start how-many)
;;; ****
  (unless (listp list)
    (error "remove-elements: argument 1 must be a list!: ~a"
           list))
  (unless (and (integer>=0 start)
               (integer>=0 how-many)
               (<= (+ start how-many) (length list)))
    (error "remove-elements: arguments 2 and 3 must be integers < the ~
            length of argument 1: ~a ~a ~a"
           start how-many (length list)))
  (append (subseq list 0 start)
          (nthcdr (+ start how-many) list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 16:26:57 BST 2012: Added robodoc entry

;;; ****f* utilities/setf-last 
;;; DESCRIPTION
;;; Change the last element in a given list to a specified new element.
;;; 
;;; ARGUMENTS
;;; - A list.
;;; - The new last element of that list.
;;; 
;;; RETURN VALUE
;;; Returns the new last element.
;;; 
;;; EXAMPLE
#|
(let ((l '(1 2 3 4 5)))
  (setf-last l 'dog)
  l)

=> (1 2 3 4 DOG)

|#
;;; SYNOPSIS
(defmacro setf-last (list new-last)
;;; ****
  `(progn
     (unless ,list
       (error "utilities::setf-last: list is empty!"))
     ;; MDE Sun May  6 16:23:04 2012 -- 
     (unless (listp ,list)
       (error "utilities::setf-last: first argument must be a list: ~a" ,list))
     (setf (nth (1- (length ,list)) ,list) ,new-last)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
;;; ****
(defmacro mk (mark &rest data)
  `(cmn::get-cmn-marks ',mark ,@data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ml (element repeat)
  (make-list repeat :initial-element element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 09:46:13 BST 2012

;;; ****f* utilities/nconc-sublists
;;; DESCRIPTION

;;; Concatenate corresponding sublists of a given list. Each sublist in the
;;; argument should have the same length and number of sublists etc.
;;; 
;;; ARGUMENTS
;;; A list of lists.
;;; 
;;; RETURN VALUE
;;; A list of lists.
;;; 
;;; EXAMPLE
#|
(nconc-sublists '(((1 2) (a b) (cat dog)) 
                  ((3 4) (c d) (bird fish)) 
                  ((5 6) (e f) (pig cow))))

=> ((1 2 3 4 5 6) (A B C D E F) (CAT DOG BIRD FISH PIG COW))

|#
;;; SYNOPSIS
(defun nconc-sublists (lists)
;;; ****
  (let ((len (length lists)))
    (loop for d in (first lists) and i from 0 collect
          (if (simple-listp d)
              (loop for j below len nconc (nth i (nth j lists)))
            (nconc-sublists (loop for j below len 
                                  collect (nth i (nth j lists))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 09:51:05 BST 2012: Added robodoc entry

;;; ****f* utilities/logarithmic-steps
;;; DESCRIPTION

;;; Create a list of progressing from the first specified argument to the
;;; second specified argument over the specified number of steps using an
;;; exponential curve rather than linear interpolation.
;;; 
;;; ARGUMENTS
;;; - A number that is the starting value in the resulting list.
;;; - A number that is the ending value in the resulting list.
;;; - An integer that will be the length of the resulting list - 1.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that will be used as the exponent when determining the
;;;   exponential interpolation between values. Default = 2.
;;; 
;;; RETURN VALUE
;;; A list of numbers.
;;; 
;;; EXAMPLE
#|
(logarithmic-steps 1 100 19)

=> (1.0 1.3055556 2.2222223 3.75 5.888889 8.638889 12.0 15.972222 20.555555
    25.75 31.555555 37.97222 45.0 52.63889 60.88889 69.75 79.22222 89.30556
    100.0)

|#
;;; SYNOPSIS
(defun logarithmic-steps (low high num-steps &optional (exponent 2))
;;; ****
  (loop for i below num-steps 
     with curve = (list 0 low (1- num-steps) high)
     collect
       (interpolate i curve :exp exponent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 09:58:22 BST 2012: Added robodoc entry

;;; ****f* utilities/interpolate
;;; DESCRIPTION

;;; Get the interpolated value at a specified point within an envelope. The
;;; envelope must be specified in the form of a list of break-point pairs.
;;; 
;;; ARGUMENTS
;;; - A number that is the point within the specified envelope for which to
;;;   return the interpolated value.
;;; - A list of break-point pairs.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :scaler. A number that is the factor by which to scale the values of
;;;   the break-point pairs in the given envelope before retrieving the
;;;   interpolated value. Default = 1.
;;; - :exp. A number that is the exponent to which the result should be
;;;   raised. Default = 1.
;;; - :warn. T or NIL to indicate whether the method should print a warning if
;;;   the specified point is outside of the bounds of the x-axis specified in
;;;   the list of break-point pairs. T = warn. Default = T.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;;; Using the defaults
(interpolate 50 '(0 0 100 1))

=> 0.5

;;; Specifying a different scaler
(interpolate 50 '(0 0 100 1) :scaler 2)

=> 1.0

;;; Specifying a different exponent by which the result is to be raised
(interpolate 50 '(0 0 100 1) :exp 2)

=> 0.25

|#
;;; SYNOPSIS
(defun interpolate (point env &key (scaler 1) (exp 1) (warn t))
;;; ****
  "e.g. (interpolate 50 '(0 0 100 1) :scaler .5 :exp 2)
   => 0.0625
   The :EXP arg is the exponent that the interpolation result should
   be raised to." 
  (let ((lastx (lastx env))
        (lasty (first (last env))))
    (cond ((> point lastx)
           (when warn
             (warn "interpolate: ~a is off the x axis of ~a, returning ~a"
                   point env lasty))
           lasty)
          ((< point (car env))
           (error "interpolate: Can't interpolate ~a in ~a" point env))
          (t (interp-aux point env scaler exp)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun interp-aux (point env scaler exp)
  (let ((here (loop for i in env by #'cddr and j from 1 do
                   (if (<= point i) (return (+ j j -2))))))
    ;; rounding in making the new-env with new-lastx can cause the
    ;; very last event to be just a little bigger than the last x
    ;; value in the new-env.  If this is the case, <here> will be nil
    ;; so we better deal with this: 
    (unless here 
      (setq here (- (length env) 2)))
    (when (zerop here)
      (setq here 2))
    ;; MDE Mon Apr  9 13:08:16 2012 -- catch divide by zero error
    (let ((x1 (nth (- here 2) env))
          (x2 (nth here env)))
      ;; MDE Mon May 14 12:26:16 2012 
      (unless (and (numberp x1) (numberp x2))
        (error "utilities::interp-aux: y values in envelope must be numbers: ~a"
               env))
      (when (= x1 x2)
        (error "utilities::interp-aux: can't interpolate ~a in ~a." point env))
      (get-interpd-y point 
                     x1 
                     (* scaler (nth (- here 1) env))
                     x2
                     (* scaler (nth (+ here 1) env))
                     exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-interpd-y (point x1 y1 x2 y2 exp)
  "The arguments are the point we want interpolated,
   the x,y coordinates of the line the point lies within, and an exponent.  
   The calculation is how far away point is from x1 divided by how far x2 is 
   from x1 raised to the exponent exp, multiplied by the difference of y2 and 
   y1 all added to y1."
  (float (+ y1 (* (expt (/ (- point x1) 
                           (- x2 x1)) exp)
                  (- y2 y1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lastx (env)
  "lastx returns the last x value in the given envelope.
   e.g. (lastx '(0 0 20 4 30 5 100 0)) => 100"
  (let ((len (length env)))
    (when (oddp len) 
        (error "utilities::lastx: Wrong number of elements in ~a." env))
    (nth (- len 2) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun new-lastx (env x)
  "new-lastx will take an envelope and return it
   with scaled x values, the maximum of which is the value of new-lastx's 
   second argument.
   e.g. (new-lastx '(0 0 30 2 100 0) 20) => (0.0 0 6.0 2 20.0 0)"
  (let* ((scaler (float (/ x (lastx env))))
         (result (loop for x in env by #'cddr and y in (cdr env) by #'cddr
                     collect (* x scaler) collect y)))
    ;; rounding errors can cause lastx to be slightly off so correct
    (setf (nth (- (length result) 2) result) x)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 10:11:28 BST 2012: Added robodoc entry

;;; ****f* utilities/scale-env
;;; DESCRIPTION
;;; Scale either the x-axis values, the data values, or both of a list of
;;; break-point pairs by specified factors.
;;; 
;;; ARGUMENTS
;;; - An envelope in the form of a list of break-point pairs.
;;; - A number that is the factor by which the y values (data segment of the
;;;   break-point pairs) are to be scaled.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:

;;; - :y-min. A number that is the minimum value for all y values after
;;;   scaling.

;;; - :y-max. A number that is the maximum value for all y values after
;;;   scaling.

;;; - :x-scaler. A number that is the factor by which to scale the x-axis
;;;   values of the break-point pairs.

;;; - :x-min. A number that is the minimum value for all x values after
;;;   scaling. NB: This optional argument can only be used if a value has been
;;;   specified for the :x-scaler. 

;;; - :x-max. A number that is the maximum value for all x values after
;;;   scaling. NB: This optional argument can only be used if a value has been
;;;   specified for the :x-scaler.

;;; 
;;; RETURN VALUE
;;; An envelope in the form of a list of break-point pairs.
;;; 
;;; EXAMPLE
#|

;;; Scaling only the y values.
(scale-env '(0 53 25 189 50 7 75 200 100 3) 0.5)

=> (0 26.5 25 94.5 50 3.5 75 100.0 100 1.5)

;;; Scaling the y values and setting a min and max for those values
(scale-env '(0 53 25 189 50 7 75 200 100 3) 0.5 :y-min 20 :y-max 100)

=> (0 26.5 25 94.5 50 20 75 100 100 20)

;;; Scaling only the x-axis values
(scale-env '(0 53 25 189 50 7 75 200 100 3) 1.0 :x-scaler 2)

=> (0 53.0 50 189.0 100 7.0 150 200.0 200 3.0)

;;; Scaling the x values and setting a min and max for those values
(scale-env '(0 53 25 189 50 7 75 200 100 3) 1.0 :x-scaler 2 :x-min 9 :x-max 90)

=> (9 53.0 50 189.0 90 7.0 90 200.0 90 3.0)

|#
;;; SYNOPSIS
(defun scale-env (env y-scaler &key x-scaler 
                                    (x-min most-negative-double-float)
                                    (y-min most-negative-double-float)
                                    (x-max most-positive-double-float)
                                    (y-max most-positive-double-float))
;;; ****
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
     collect (if x-scaler (min x-max (max x-min (* x x-scaler)))
                 x) 
     collect (min y-max (max y-min (* y y-scaler)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 10:32:25 BST 2012: Added robodoc entry

;;; ****f* utilities/reverse-env
;;; DESCRIPTION
;;; Reverse the order of y values in a list of break-point pairs.
;;; 
;;; ARGUMENTS
;;; - An envelope in the form of a list of break-point pairs.
;;; 
;;; RETURN VALUE
;;; An envelope in the form of a list of break-point pairs.
;;; 
;;; EXAMPLE
#|
(reverse-env '(0 0 25 11 50 13 75 19 100 23))

=> (0 23 25 19 50 13 75 11 100 0)

|#
;;; SYNOPSIS
(defun reverse-env (env)
;;; ****
  "reverse-env returns the reverse of the envelope 
   supplied to it.  
   e.g. (reverse-env '(0 0 60 .3 100 1)) => (0 1 40 0.3 100 0)."
  (let ((x-max (lastx env))
        (result nil))
    (loop for x in env by #'cddr and y in (cdr env) by #' cddr do
         (push y result)
         (push (- x-max x) result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 10:36:29 BST 2012: Added robodoc entry

;;; ****f* utilities/repeat-env
;;; DESCRIPTION

;;; Create a new list by repeating the y values of a list of break-point pairs
;;; a specified number of times over the same total x-axis span of the original
;;; envelope. A quick ramp is inserted between repeats to ensure that all
;;; x-axis values are unique and incremental.
;;;
;;; If the optional argument is set to T, the method will reverse the order of
;;; every second repeat.
;;; 
;;; ARGUMENTS
;;; - An envelope in the form of a list of break-point pairs.
;;; - An integer that is the number of times the elements of the given envelope
;;;   should be repeated in the new list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether every second repetition of the original
;;;   envelope should be returned in reverse order. 
;;;   T = reverse. Default = NIL.
;;; 
;;; RETURN VALUE
;;; - A new envelope in the form of a list of break-point pairs.
;;; 
;;; EXAMPLE
#|
(repeat-env '(0 1 50 2 100 3) 3)

=> (0.0 1 16.666666 2 33.333332 3 34.333332 1 50.0 2 66.666664 3 67.666664 1
    83.33333 2 100.0 3)

(repeat-env '(0 1 50 2 100 3) 3 t)

=> (0.0 1 16.666666 2 33.333332 3 50.0 2 66.666664 1 83.33333 2 100.0 3)

|#
;;; SYNOPSIS
(defun repeat-env (env num-times &optional reflected)
;;; ****
  "repeat-env will repeat an envelope the number 
   of times specified by its second argument.  
   e.g. (repeat-env '(0 0 100 1) 2) => (0 0 50 1 51 0 100 1).  
   Because the final y value was different to the first y value, 
   a quick ramp was inserted between repeats.  
   Every other repeat can be a reflection of the given envelope 
   by setting the optional variable to t.
   e.g. (repeat-env '(0 0 100 1) 2 t) => (0 0 50 1 100 0)."
  (let* ((result nil)
         (x-inc 0.0)
         (x-max (lastx env))
         (base (/ x-max num-times))
         (starting (if (and reflected (evenp num-times)) 
                       (reverse-env env) env))
         (copy (reverse starting))
         (first-y-is-last-y (when (numberp (cadr env))
                              (= (cadr env) (car (last env)))))
         (offset (/ x-max 100)))
    (dotimes (count num-times)
             (setq x-inc (* base (- num-times count 1.0)))
             (loop for y in copy by #'cddr and x in (cdr copy) by #'cddr do
                   (push y result)
                   (push (+ x-inc (/ x num-times)) result))
             (setf copy (cond ((and (not reflected) (not first-y-is-last-y)) 
                               copy)
                              ((and reflected (evenp count))
                               (cddr (reverse (reverse-env starting))))
                              ((or (and reflected (oddp count)) 
                                   first-y-is-last-y)
                               (cddr (reverse starting))))
                   (car result)
                   (if (or (= count (- num-times 1)) reflected 
                           first-y-is-last-y)
                       (car result) (+ (car result) offset))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 11:01:20 BST 2012: Added robodoc entry

;;; ****f* utilities/env-plus
;;; DESCRIPTION
;;; Increase all y values of a given list of break-point pairs by a specified
;;; amount.
;;; 
;;; ARGUMENTS
;;; - An envelope in the form of a list of break-point pairs.
;;; - A number that is the amount by which all y values of the given envelope
;;;   are to be increased.
;;; 
;;; RETURN VALUE
;;; A list of break-point pairs.
;;; 
;;; EXAMPLE
#|
(env-plus '(0 0 25 11 50 13 75 19 100 23) 7.1)

=> (0 7.1 25 18.1 50 20.1 75 26.1 100 30.1)

|#
;;; SYNOPSIS
(defun env-plus (env add)
;;; ****
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr
     collect x collect (+ y add)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 11:17:05 BST 2012: Added robodoc entry

;;; ****f* utilities/env-symmetrical
;;; DESCRIPTION
;;; Create a new list of break-point pairs that is symmetrical to the original
;;; around a specified center. If no center is specified, the center value
;;; defaults to 0.5
;;; 
;;; ARGUMENTS
;;; - An envelope in the form of a list of break-point pairs.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is the center value around which the values of the
;;;   new list are to be symmetrical.
;;; - A number that is to be the minimum value for the y values returned.
;;; - A number that is to be the maximum value for the y values returned.
;;; 
;;; RETURN VALUE
;;; An envelope in the form of a list of break-point pairs.
;;; 
;;; EXAMPLE
#|
;;; Default center is 0.5
(env-symmetrical '(0 0 25 11 50 13 75 19 100 23))

=> (0 1.0 25 -10.0 50 -12.0 75 -18.0 100 -22.0)

;; Specifying a center of 0
(env-symmetrical '(0 0 25 11 50 13 75 19 100 23) 0)

=> (0 0.0 25 -11.0 50 -13.0 75 -19.0 100 -23.0)

;;; Specifying minimum and maximum y values for the envelope returned
(env-symmetrical '(0 0 25 11 50 13 75 19 100 23) 0 -20 -7)

=> (0 -7 25 -11.0 50 -13.0 75 -19.0 100 -20)

|#
;;; SYNOPSIS
(defun env-symmetrical (env &optional (centre .5) 
                        (min most-negative-double-float)
                        (max most-positive-double-float))
;;; ****
  "Returns an envelope that is symmetrical around the key variable 'centre'.
   e.g. (symmetrical '(0 0 30 .2 70 .95 100 .5)) => 
   (0 1.0 30 0.8 70 0.05 100 0.5)"
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
     for new-y = (- (* 2.0 centre) y)
     collect x collect (max min (min new-y max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 11:28:13 BST 2012: Added robodoc entry

;;; ****f* utilities/force-length
;;; DATE
;;; 03-FEB-2011
;;;
;;; DESCRIPTION

;;; Create a new a list of a specified new length by adding or removing items
;;; at regular intervals from the original list. If adding items and the list
;;; contains numbers, linear interpolation will be used, but only between two
;;; adjacent items; i.e. not with a partial increment.
;;;
;;; NB: The function can only create new lists that have a length between 1 and
;;;     1 less than double the length of the original list.
;;; 
;;; ARGUMENTS
;;; - A flat list.
;;; - A number that is the new length of the new list to be derived from the
;;;   original list. This number must be a value between 1 and 1 less than
;;;   double the length of the original list.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;;; Shortening a list
(force-length (loop for i from 1 to 100 collect i) 17)

=> (1 7 13 20 26 32 39 45 51 57 63 70 76 82 89 95 100)

;;; Lengthening a list
(force-length (loop for i from 1 to 100 collect i) 199)

=> (1 1.5 2 2.5 3 3.5 4 4.5 5 5.5 6 6.5 7 7.5 8 8.5 9 9.5 10 10.5 11 11.5 12
    12.5 13 13.5 14 14.5 15 15.5 16 16.5 17 17.5 18 18.5 19 19.5 20 20.5 21
    21.5 22 22.5 23 23.5 24 24.5 25 25.5 26 26.5 27 27.5 28 28.5 29 29.5 30
    30.5 31 31.5 32 32.5 33 33.5 34 34.5 35 35.5 36 36.5 37 37.5 38 38.5 39
    39.5 40 40.5 41 41.5 42 42.5 43 43.5 44 44.5 45 45.5 46 46.5 47 47.5 48
    48.5 49 49.5 50 50.5 51 51.5 52 52.5 53 53.5 54 54.5 55 55.5 56 56.5 57
    57.5 58 58.5 59 59.5 60 60.5 61 61.5 62 62.5 63 63.5 64 64.5 65 65.5 66
    66.5 67 67.5 68 68.5 69 69.5 70 70.5 71 71.5 72 72.5 73 73.5 74 74.5 75
    75.5 76 76.5 77 77.5 78 78.5 79 79.5 80 80.5 81 81.5 82 82.5 83 83.5 84
    84.5 85 85.5 86 86.5 87 87.5 88 88.5 89 89.5 90 90.5 91 91.5 92 92.5 93
    93.5 94 94.5 95 95.5 96 96.5 97 97.5 98 98.5 99 99.5 100)

|#
;;; SYNOPSIS
(defun force-length (list new-len)
;;; ****
  (let* ((len (length list))
         (diff (- new-len len)))
    ;; (format t "~%hey ~a ~a ~a" len new-len diff)
    (when (or (zerop new-len) (>= diff len))
      (error "force-length:: new length must be between 1 and 1 less than ~%~
              double the original length: ~a ~%(length: ~a, new length: ~a)" 
             list len new-len))
    (if (= len new-len)
        list
        (if (< new-len len)
            (let* ((skip (/ len (1- new-len)))
                   (last-el (first (last list)))
                   (result
                    (loop for count below new-len with i = 0 collect
                         (nth (round i) list)
                       do
                         (incf i skip))))
              (unless (equalp last-el (first (last result)))
                (setf-last result last-el))
              result)
            (let* ((adiff (abs diff))
                   (result '())
                   (cycle (unless (zerop diff) (max 1 (1- (floor len adiff)))))
                   (points (loop for i from cycle by cycle repeat adiff
                              collect i)))
              (loop with next = (pop points) with last-el
                 for el in list and i from 0 do
                   (if (and next (= i next))
                       (progn 
                         (when (> diff 0)
                           (if (and (numberp el) (numberp last-el)) ; add items
                               ;; interpolate to get the new element
                               (push (+ last-el (/ (- el last-el) 2.0)) 
                                     result)
                               ;; if not numbers just push in the last element
                               (push last-el result))
                           (push el result)) ; get this element too of course
                         (setf next (pop points)))
                       ;; not on point so get it
                       (push el result))
                   (setf last-el el))
              (setf result (nreverse result))
              (unless (= (length result) new-len)
                (error "force-length:: somehow got the wrong length: ~a"
                       (length result)))
              result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These functions read a wavelab marker file and prints the list of arguments
;;; necessary to create a sndfile object for a sndfile-palette.  The marker
;;; file defines sections with start/stop marker pairs, the first one of which
;;; should be named (the description).
;;; 
;;; marker-file is the path to the file
;;; sndfile is the name of the file, not the whole path, rather the name given
;;; at the start of a new sndfile in a palette
;;;
;;; Used by parse-wavelab-marker-files-for-sections and friends in
;;; sndfile-palette.lsp  

(defstruct wavelab-section
  (sndfile) (description) (start) (end))


;;; This handles as many marker files as you want, if they're in a list.  Only
;;; caveat is that each file must refer to the same sndfile...but that could be
;;; easily altered. 

(defun parse-wavelab-marker-files-for-sections 
    (marker-files sndfile
     &key (sampling-rate 44100) (print nil))
  (if (listp marker-files)
      (loop for file in marker-files appending
            (parse-wavelab-marker-file-for-sections
             file sndfile :sampling-rate sampling-rate :print print))
    (parse-wavelab-marker-file-for-sections
     marker-files sndfile :sampling-rate sampling-rate :print print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This handles one marker file.
(defun parse-wavelab-marker-file-for-sections
    (marker-file sndfile
     &key (sampling-rate 44100) (print nil))
  (with-open-file 
      (mrk marker-file :direction :input :if-does-not-exist :error)
    (loop with name 
       with time 
       with last-time = -1.0
       with start 
       with end 
       with region-name
       with count = 0 
       with result = '()
       do
       (multiple-value-bind
             (line eof)
           (read-line mrk nil)
         (multiple-value-bind
               (param value)
             (get-parameter line)
           (when (or (string= param "name")
                     ;; MDE Tue May 15 14:23:08 2012 -- wavelab 7 uses Name8
                     (string= param "name8"))
             (setf name value)
             (incf count)
             ;; only the start markers are given names, we assume
             (when (oddp count)
               (setf region-name name)))
           (when (string= param "pos")
             (setf time (float (/ (read-from-string value)
                                  sampling-rate)))
             (when (= time last-time)
               (error "utilities::parse-wavelab-marker-file-for-sections: ~
                          Two markers at same point: ~a"
                      time))
             (setf last-time time)
             (if (oddp count)
                 (setf start time)
                 (progn 
                   (unless (string= name "*")
                     (warn "parse-wavelab-marker-file-for-sections: ~
                            Got marker with name \"~a\" ~%at ~a (pos ~a); ~
                            expected no name."
                           name (secs-to-mins-secs time) value))
                   (setf end time)
                   (push (make-wavelab-section :sndfile sndfile 
                                               :description region-name
                                               :start start
                                               :end end)
                         result)
                   (when print
                     (format t "~%(~a ~%~t:description \"~a\" ~
                                 ~%~t:start ~,5f :end ~,5f)"
                             sndfile region-name start end)))))
           (when eof 
             (format t "~&~a markers read from ~a~%" count marker-file)
             (return (nreverse result))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wavelab-markers-to-sampler
    (sndfile outdir &optional (sampling-rate 44100))
  (with-open-file 
      (mrk (format nil "~a:~a~a.mrk" 
                   (pathname-device sndfile)
                   (directory-namestring sndfile)
                   (pathname-name sndfile))
           :direction :input :if-does-not-exist :error)
    (with-open-file
        (txt (format nil "~azzz__~a.txt" 
                     outdir
                     (file-namestring sndfile))
             :direction :output :if-exists :error)
      (loop 
         with count = 0
         with time
         do
         (multiple-value-bind
               (line eof)
             (read-line mrk nil)
           (multiple-value-bind
                 (param value)
               (get-parameter line)
             (when (string= param "pos")
               (setf time (float (/ (read-from-string value)
                                    sampling-rate)))
               (format txt "~&~a, ~,6f;" count time))
             (when eof 
               (terpri txt)
               (format t "~%~%~a markers read~%" count)
               (return t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(wavelab-to-audacity-marker-file 
 (concatenate 'string
              cl-user::+slippery-chicken-home-dir+ 
              "test-suite/24-7.mrk")
 44100)
|#

;;; MDE Tue May 15 16:53:24 2012 
;;; writes a .txt file suitable for import to audacity with the same name and
;;; in the same directory as the file argument.
;;; ****f* utilities/wavelab-to-audacity-marker-file
;;; DESCRIPTION
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
(defun wavelab-to-audacity-marker-file (file &optional (sampling-rate 44100))
;;; ****
  (with-open-file 
      (mrk file :direction :input :if-does-not-exist :error)
    (with-open-file
        (txt (format nil "~a~a.txt" 
                     (directory-namestring file)
                     (pathname-name file))
             :direction :output :if-exists :error)
      (loop 
         with name 
         with time 
         with count = 0 
         do
         (multiple-value-bind
               (line eof)
             (read-line mrk nil)
           (multiple-value-bind
                 (param value)
               (get-parameter line)
             (when (or (string= param "name")
                       ;; MDE Tue May 15 14:23:08 2012 -- wavelab 7 uses Name8  
                       (string= param "name8"))
               (setf name (if (string= value "*")
                              nil
                              value))
               (incf count))
             (when (string= param "pos")
               (setf time (float (/ (read-from-string value)
                                    sampling-rate)))
               (if name
                   (format txt "~&~,6f        ~a" time name)
                   (format txt "~&~,6f" time)))
             (when eof 
               (terpri txt)
               (format t "~%~%~a markers read~%" count)
               (return t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This function reads a wavelab marker file and prints loop points.  The
;;; marker file must contain markers with the name "loop": a marker with that
;;; name will start a new set of loop points, and nameless markers will belong
;;; to the group until the next "loop" marker.  marker-file is the path to the
;;; file.  Max-length is the max distance (secs) between two points: anything
;;; greater than this and a warning will be issued.

;;; ****f* utilities/parse-wavelab-marker-file-for-loops
;;; DESCRIPTION
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
(defun parse-wavelab-marker-file-for-loops
    (marker-file &key (sampling-rate 44100) (max-length 1.0))
;;; ****
  (let ((loop-points '())
        (in-loop nil)
        (result '()))
    (flet ((write-loop-points ()
             (when loop-points
               (setf loop-points (nreverse loop-points))
               (when (< (length loop-points) 5)
                 (warn "loop starting at ~a has ~a loop points: this ~
                        probably won't yield enough permutations"
                       (first loop-points)
                       (length loop-points)))
               ;; the ~^ adds a space after the element only
               ;; when this isn't the last one.        
               ;; (format t "~%(~{~a~^ ~})" loop-points)
               (push loop-points result)
               (setf loop-points nil))))
      (with-open-file 
          (mrk marker-file :direction :input :if-does-not-exist :error)
        (loop 
            with time = 0.0
            with last-time = 0.0
            with length
            with count = 0 
            with num-loops = 0 
            with this-loop-count = 0
            do
              (multiple-value-bind
                  (line eof)
                  (read-line mrk nil)
                (multiple-value-bind
                    (param value)
                    (get-parameter line)
                  ;; (print value)
                  (when (or (string= param "name")
                            ;; MDE Tue May 15 14:23:08 2012 -- wavelab 7 uses
                            ;; Name8   
                            (string= param "name8"))
                    (incf count)
                    (if (string= value "loop")
                        (progn
                          (incf num-loops)
                          (setf in-loop t
                                this-loop-count 0)
                          (write-loop-points))
                      ;; marker must have another name so it's nothing to
                      ;; do with a loop 
                      (unless (string= value "*")
                        (setf in-loop nil))))
                  (when (string= param "pos")
                    (setf last-time time
                          time (float (/ (read-from-string value)
                                         sampling-rate))
                          length (- time last-time))
                    (when in-loop
                      (incf this-loop-count)
                      ;; should only be looking for too-long segments after the
                      ;; first "*" marker after "loop"
                      (when (and (> this-loop-count 1)
                                 (> length max-length))
                        (warn "utilities::parse-wavelab-marker-file-for-loops ~
                               loop points ~a to ~a are too long (~f)"
                              (secs-to-mins-secs last-time) 
                              (secs-to-mins-secs time)
                              length))
                      (push time loop-points)))
                  (when eof 
                    (write-loop-points)
                    (format t "~%~%~a markers, ~a loops read~%"
                            count num-loops)
                    (return (nreverse result))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sort-labels (file)
  (with-open-file 
      (in file :direction :input :if-does-not-exist :error)
    (with-open-file
        (out (format nil "~a~a-sorted.~a" 
                     (directory-namestring file)
                     (pathname-name file)
                     (pathname-type file))
         :direction :output :if-exists :error)
      (loop 
          with result = '()
          do
            (multiple-value-bind
                (line eof)
                (read-line in nil)
              (when line
                (push line result))
              (when eof
                (setf result
                  (sort 
                   (nreverse result)
                   #'(lambda (x y)
                       (< (read-from-string x)
                          (read-from-string y)))))
                (loop 
                    for line in result 
                    with time = -1.0
                    with temp
                    do
                      (setf temp (read-from-string line))
                      ;; labels somehow get duplicated...
                      (unless (= temp time)
                        (setf time temp)
                        (format out "~&~a" line)))
                (return t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If this fails it's probably because there's a tab between time and
;;; label instead of spaces: save in emacs to detab.

;;; NB Beware that marker files created on different operating systems from the
;;; one one which this function is called might trigger errors due to newline
;;; character mismatches.

;;; (parse-audacity-label-file-for-loops "loops.txt")

;;; ****f* utilities/parse-audacity-label-file-for-loops
;;; DESCRIPTION
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
(defun parse-audacity-label-file-for-loops (label-file)
;;; ****
  (let ((loop-points '())
        (result '()))
    (flet ((write-loop-points ()
             (when loop-points
               (setf loop-points (nreverse loop-points))
               (when (< (length loop-points) 5)
                 (error "too few loop points at ~a" (first loop-points)))
               ;; the ~^ adds a space after the element only
               ;; when this isn't the last one.
               ;; (format t "~%(~{~a~^ ~})" loop-points)
               (push (copy-list loop-points) result)
               (setf loop-points nil))))
      (with-open-file 
          (mrk label-file :direction :input :if-does-not-exist :error)
        (loop
           with count = 0 
           with num-loops = 0 
           do
             (multiple-value-bind
                   (line eof)
                 (read-line mrk nil)
               ;; (print line)
               (multiple-value-bind
                     (label time)
                   (read-audacity-line line)
                 ;; (format t "~&~a ~a" label time)
                 (incf count)
                 (when (string= label "loop")
                   (incf num-loops)
                   (write-loop-points))
                 (setf time (read-from-string time))
                 (when time
                   (push time loop-points)))
               (when eof 
                 (write-loop-points)
                 (format t "~%~%~a markers, ~a loops read~%"
                         count num-loops)
                 (return))))))
    (nreverse result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trim-leading-trailing-whitespace (string)
   (string-trim '(#\Space #\Tab #\Newline) string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; aux function for parse-wavelab-marker-file-for...

(defun get-parameter (string &optional (separator #\=))
  (flet ((trim-whitespace (string)
                          (string-downcase
                           (string-trim '(#\Space #\Tab #\Newline) string))))
        (let ((sep-pos (position separator string)))
          (when sep-pos
            (let ((param (trim-whitespace (subseq string 0 sep-pos)))
                  (value (trim-whitespace (subseq string (1+ sep-pos)))))
              (values param value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; aux function for parse-wavelab-marker-file-for...

;;; (read-audacity-line "3.777420        loop")
;;; (read-audacity-line "3.777420")

(defun read-audacity-line (string &optional (separator #\Space))
  (flet ((trim-whitespace (string)
           (string-downcase
            (string-trim '(#\Space #\Tab #\Newline) string))))
    (let* ((sep-pos (position separator string))
           (time (trim-whitespace (subseq string 0 sep-pos)))
           (label nil))
      (when sep-pos
        (setf label (trim-whitespace (subseq string (1+ sep-pos)))))
      (values label time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun audacity-mid-phrase-loops (file)
  (with-open-file 
      (in file :direction :input :if-does-not-exist :error)
    (with-open-file
        (out (format nil "~a~a-mid-phrase.~a" 
                     (directory-namestring file)
                     (pathname-name file)
                     (pathname-type file))
         :direction :output :if-exists :error)
      (flet ((split-phrase (time-list)
               (when time-list
                 (let ((subs (split-into-sub-groups3 time-list 7)))
                   (loop for sl in subs do
                         (format out "~&~,6f        loop" (first sl))
                         (loop for time in (cdr sl) do
                               (format out "~&~,6f" time)))))))
        (loop 
            with phrase = '()
            do
              (multiple-value-bind
                  (line eof)
                  (read-line in nil)
                (when line
                  (multiple-value-bind
                      (label time)
                      (read-audacity-line line)
                    (if (string= label "loop")  
                        (progn
                          (split-phrase (nreverse phrase))
                          (setf phrase '()))
                      (push (read-from-string time) phrase))))
                (when eof
                  (split-phrase (nreverse phrase))
                  (return t))))))))
                    
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun audacity-min-length (file &optional (min 0.05))
  (with-open-file 
      (in file :direction :input :if-does-not-exist :error)
    (with-open-file
        (out (format nil "~a~a-min.~a" 
                     (directory-namestring file)
                     (pathname-name file)
                     (pathname-type file))
         :direction :output :if-exists :error)
      (loop 
          with last = -999999.0
          do
            (multiple-value-bind
                (line eof)
                (read-line in nil)
              (when line
                (multiple-value-bind
                    (label time)
                    (read-audacity-line line)
                  ;; don't check length if this is a loop begin
                  (if (string= label "loop")
                      (format out "~&~a" line)
                    (progn
                      (setf time (read-from-string line))
                      (when (> (- time last) min)
                        (setf last time)
                        (format out "~&~a" line))))))
              (when eof
                (return t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun May 20 15:17:41 EDT 2012: Added robodoc entry

;;; MDE original comment:
;;; reflect-list will order a list from least to greatest (so has to be
;;; numbers) then transpose the list so that if an element is the second
;;; lowest, it will be replace by the second highest etc.

;;; (reflect-list '(1 4 3 5 9 6 2 7 8 8 9))

;;; ****f* utilities/reflect-list
;;; DESCRIPTION
;;; Order a list of numbers from least to greatest, then transpose the list so
;;; that if an element is the second lowest, it will be replaced by the second
;;; highest etc.
;;; 
;;; ARGUMENTS
;;; - A list or numbers.
;;; 
;;; RETURN VALUE
;;; A list of numbers.
;;; 
;;; EXAMPLE
#|
(reflect-list '(1 4 3 5 9 6 2 7 8 8 9))

=> (9 6 7 5 1 4 8 3 2 2 1)

|#
;;; SYNOPSIS
(defun reflect-list (list)
;;; ****
  ;; MDE Mon May  7 15:11:49 2012 -- got to remove duplicates
  (let* ((sorted (sort (remove-duplicates (copy-list list)) #'<))
         (len (length sorted))
         (pos 0))
    (loop for i in list 
       do
       ;; MDE Mon May  7 15:12:04 2012 -- use sorted, not list
       (setf pos (position i sorted))
       collect
       ;; MDE Mon May  7 15:12:04 2012 -- use sorted, not list
       (nth (- len pos 1) sorted))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 15:05:44 BST 2012: Added robodoc entry

;;; ****f* utilities/middle
;;; DESCRIPTION
;;; Get the number value that is middle of two number values.
;;; 
;;; ARGUMENTS
;;; - A first number.
;;; - A second number.
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE
#|
(middle 7 92)

=> 49.5

|#
;;; SYNOPSIS
(defun middle (lower upper)
;;; ****
  (+ lower (/ (- upper lower) 2.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 15:08:31 BST 2012: Added robodoc entry

;;; ****f* utilities/hz2ms
;;; DESCRIPTION
;;; Convert a frequency in Hertz to the equivalent number of milliseconds. 
;;; 
;;; ARGUMENTS
;;; - A number that is a Hertz frequency.
;;; 
;;; RETURN VALUE
;;; A number that is the millisecond equivalent of the specified Hertz
;;; frequency. 
;;; 
;;; EXAMPLE
#|
(hz2ms 261.63)

=> 3.8221915

|#
;;; SYNOPSIS
(defun hz2ms (hertz)
;;; ****
  (/ 1000.0 hertz))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 15:13:28 BST 2012: Added robodoc entry

;;; (split-groups 31 10) -> (10 10 10 1)

;;; ****f* utilities/split-groups
;;; DESCRIPTION
;;; Create a list consisting of as many repetitions of a specified number as
;;; will fit into a given greater number, with the last item in the new list
;;; being the value of any remainder.
;;; 
;;; ARGUMENTS
;;; - A number that is to be split into repetitions of a specified smaller
;;;   number (the second argument).
;;; - The number that is to be the repeating item in the new list. This number
;;;   must be smaller than the first number.
;;; 
;;; RETURN VALUE
;;; A list consisting of repetitions of the specified number, with the last
;;; element being any possible remainder.
;;; 
;;; EXAMPLE
#|
(split-groups 101 17)

=> (17 17 17 17 17 16)

|#
;;; SYNOPSIS
(defun split-groups (num divider)
;;; ****
  (unless (and (> num 0) (> divider 0))
    (error "utilities::split-groups: num (~a) and dividor (~a) should both ~
            be > 0" num divider))
  (multiple-value-bind
      (div rem)
      (floor num divider)
    (loop repeat div collect divider into result
        finally (return (if (zerop rem)
                            result
                          (econs result rem))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 15:26:49 BST 2012: Added robodoc entry

;;; ****f* utilities/remove-more
;;; DESCRIPTION
;;; Remove all instances of a list of specified elements from an original
;;; list. The predicate used to test the presence of the specified elements in
;;; the original list must be specified by the user (such as #'eq, #'equalp,
;;; #'= etc.)
;;; 
;;; ARGUMENTS
;;; - A list.
;;; - A predicate with which to test the presence of the specified elements.
;;; - A sequence of elements to be removed from the given list.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
(remove-more '(1 2 3 4 5 5 5 6 7 7 8) #'= 5 7 2)

=> (1 3 4 6 8)

|#
;;; SYNOPSIS
(defun remove-more (list test &rest remove)
;;; ****
  (loop 
     with result = list
     for r in remove
     do
       (setf result (remove r result :test test))
     finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 19:12:20 BST 2012: Added robodoc entry

;;; ****f* utilities/pts2cm
;;; DESCRIPTION
;;; Convert a specified number of points to a length in centimeters at a
;;; resolution of 72ppi
;;; 
;;; ARGUMENTS
;;; - A number.
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE

#|
(pts2cm 150)

=> 5.2916665

|#
;;; SYNOPSIS
(defun pts2cm (points)
;;; ****
  (* 2.54 (/ points 72.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 19:20:07 BST 2012: Added robodoc entry

;; low and high are inclusive except if float given: then we can't quite reach
;; high.  

;;; ****f* utilities/between
;;; DESCRIPTION

;;; Return a random number between two specified numbers. If the two numbers
;;; are integers, the random selection is inclusive. If they are decimal
;;; numbers, the result cannot be absolutely inclusive. 
;;; 
;;; ARGUMENTS
;;; - A first, lower, number.
;;; - A second, higher, number. 
;;;
;;; NB: The first number must always be lower than the second.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the random seed should be fixed.
;;; - If fixed-random is set to T, a function must be given for <restart> to
;;;   reset the seed (see below)
;;; 
;;; RETURN VALUE
;;; An integer if both numbers are integers, or a float if one or both are
;;; decimal numbers.
;;; 
;;; EXAMPLE
#|
;;; Using the defaults. This will produce a different result each time.
(loop repeat 10 collect (between 1 100))

=> (43 63 26 47 28 2 99 93 66 23)

;;; Setting fixed-random to T and using zerop to reset the random when i is 0 
(loop repeat 5 
   collect (loop for i from 0 to 9 collect (between 1 100 t (zerop i))))

=> ((93 2 38 81 43 19 70 18 44 26) (93 2 38 81 43 19 70 18 44 26)
    (93 2 38 81 43 19 70 18 44 26) (93 2 38 81 43 19 70 18 44 26)
    (93 2 38 81 43 19 70 18 44 26))

|#
;;; SYNOPSIS
(defun between (low high &optional fixed-random restart)
;;; ****
  (unless (> high low)
    (error "utilities::between: high (~a) should be > low (~a)" high low))
  (when restart
    (random-rep 10 t))
  (if (and (integerp low) (integerp high))
      (+ low (funcall (if fixed-random #'random-rep #'random)
                      (1+ (- high low))))
      (+ low (funcall (if fixed-random #'random-rep #'random)
                      (- high low)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 19:37:44 BST 2012: Added robodoc entry

;;; ****f* utilities/randomise
;;; DESCRIPTION
;;; Return a random decimal number close to the number specified (within a
;;; certain percentage of that number's value).
;;; 
;;; ARGUMENTS
;;; - A number.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is a percentage value, such that any random number returned
;;;   will be within that percentage of the original number's value. 
;;;   Default = 5.
;;; 
;;; RETURN VALUE
;;; A decimal number.
;;; 
;;; EXAMPLE
#|
(loop repeat 10 collect (randomise 100))

=> (99.413795 99.15346 98.682014 100.76199 97.74929 99.05693 100.59494 97.96452
    100.42091 100.01329)

|#
;;; SYNOPSIS
(defun randomise (number &optional (percent 5))
;;; ****
  (let ((room (* number (/ percent 200.0))))
    (between (- number room) (+ number room))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 19:48:41 BST 2012: Added robodoc entry

;;; this returns a random portion of the number, +/- percent

;;; ****f* utilities/random-amount
;;; DESCRIPTION

;;; Return a random number from within a total range of <percent> of the given number, 
;;; centering around zero. Thus, if the <number> is 100, and the <percent> is 5, the 
;;; results will be a random number between -2.5 and +2.5.
;;; 
;;; ARGUMENTS
;;; A number.
;;; 
;;; OPTIONAL ARGUMENTS
;;; A number that will be a percent of the given number.
;;; 
;;; RETURN VALUE
;;; A random positive or negative number.
;;; 
;;; EXAMPLE
#|
;;; Using the default will return numbers within a 5% span of the given number, 
;;; centering around zero. With 100 that means between -2.5 and +2.5.
(loop repeat 10 collect (random-amount 100))

=> (0.7424975 -1.4954442 -1.7126495 1.5918689 -0.43478793 -1.7916341 -1.9115914
    0.8541988 0.057197176 2.0713913)

;;; Specifying 10% of 80 will return random numbers between -4.0 and +4.0
(loop repeat 10 collect (random-amount 80 10))

=> (-0.66686153 3.0387697 3.4737322 -2.3753185 -0.8495751 -0.47580242
    -0.25743783 -1.1395472 1.3560238 -0.5958566)

|#
;;; SYNOPSIS
(defun random-amount (number &optional (percent 5))
;;; ****
  (let ((pc (float (/ percent 100.0))))
    (* number (+ (- (/ pc 2.0))
                 (random pc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 20:00:12 BST 2012: Added robodoc entry

;;; ****f* utilities/random-from-list
;;; DESCRIPTION
;;; Return a random element from a specified list of elements. 
;;; 
;;; ARGUMENTS
;;; - A list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer can be passed stating the length of the list, for more
;;;   efficient processing. NB: There is no check to ensure this number is
;;;   indeed the length of the list. If the number is less than the length of
;;;   the list, only elements from the first part of the list will be
;;;   returned. If it is greater than the length of the list, the method may
;;;   return NIL.
;;; 
;;; RETURN VALUE
;;; An element from the specified list.
;;; 
;;; EXAMPLE
#|
(random-from-list '(3 5 7 11 13 17 19 23 29))

=> 13

|#
;;; SYNOPSIS
(defun random-from-list (list &optional list-length) ; for efficiency
;;; **** 
  (nth (random (if list-length 
                   list-length 
                 (length list)))
       list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May 21 10:13:35 EDT 2012: Added robodoc entry

;;; ****f* utilities/read-from-file
;;; DESCRIPTION
;;; Read a Lisp expression from a file. This is determined by the Lisp
;;; parenthetical syntax.
;;; 
;;; ARGUMENTS
;;; - A string that is a file name including directory path and extension. 
;;; 
;;; RETURN VALUE
;;; The Lisp expression contained in the file.
;;; 
;;; EXAMPLE
#|
(read-from-file "/path/to/lisp-lorem-ipsum.txt")

=> (LOREM IPSUM DOLOR SIT AMET CONSECTETUR ADIPISCING ELIT CRAS CONSEQUAT
    CONVALLIS JUSTO VITAE CONSECTETUR MAURIS IN NIBH VEL EST TEMPUS LOBORTIS
    SUSPENDISSE POTENTI SED MAURIS MASSA ADIPISCING VITAE DIGNISSIM CONDIMENTUM
    VOLUTPAT VEL FELIS FUSCE AUGUE DUI PULVINAR ULTRICIES IMPERDIET SED
    PHARETRA EU QUAM INTEGER IN VULPUTATE VELIT ALIQUAM ERAT VOLUTPAT VIVAMUS
    SIT AMET ORCI EGET EROS CONSEQUAT TINCIDUNT NUNC ELEMENTUM ADIPISCING
    LOBORTIS MORBI AT LOREM EST EGET MATTIS ERAT DONEC AC RISUS A DUI MALESUADA
    LOBORTIS AC AT EST INTEGER AT INTERDUM TORTOR VIVAMUS HENDRERIT CONSEQUAT
    AUGUE QUISQUE ALIQUAM TELLUS NEC VESTIBULUM LOBORTIS RISUS TURPIS LUCTUS
    LIGULA IN BIBENDUM FELIS SEM PULVINAR DOLOR VIVAMUS RHONCUS NISI GRAVIDA
    PORTA VULPUTATE IPSUM LACUS PORTA RISUS A VULPUTATE MAGNA JUSTO A EST)

|#
;;; SYNOPSIS
(defun read-from-file (file)
;;; ****
  (with-open-file
      (stream file :direction :input :if-does-not-exist :error)
    (read stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 22:47:10 BST 2012: Added robodoc entry

;;; ****f* utilities/wrap-list
;;; DESCRIPTION
;;; Shift the elements of a list to start at a specified position and wrap to
;;; the beginning of the list to the list's tail.
;;; 
;;; ARGUMENTS
;;; - A list.
;;; - An integer which is the 0-based position in the original list where the
;;;   new list is to begin.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
(wrap-list '(1 2 3 4 5 6 7 8 9) 4)

=> (5 6 7 8 9 1 2 3 4)

|#
;;; SYNOPSIS
(defun wrap-list (list start)
;;; ****
  (append (nthcdr start list) (subseq list 0 start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 22:53:23 BST 2012: Added robodoc entry

;;; ****f* utilities/combine-into-symbol
;;; DESCRIPTION
;;; Combine a sequence of elements of any combination of type string, number,
;;; or symbol into a symbol.
;;; 
;;; ARGUMENTS
;;; - A sequence of elements.
;;; 
;;; RETURN VALUE
;;; A symbol as the primary value, with the length of that symbol as a
;;; secondary value.
;;; 
;;; EXAMPLE
#|
(combine-into-symbol "test" 1 'a)

=> TEST1A, 6

|#
;;; SYNOPSIS
(defun combine-into-symbol (&rest params)
;;; ****
  (read-from-string (format nil "~{~a~^~}" params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert a character object to a symbol
(defun char2sym (char)
  (intern (coerce (list char) 'string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 22:58:12 BST 2012: Added robodoc entry
;;; SAR Mon May 21 10:05:22 EDT 2012: Completed robodoc entry

;;; ****f* utilities/swap-elements
;;; DESCRIPTION
;;; Swap the order of each consecutive pair of elements in a list.
;;; 
;;; ARGUMENTS
;;; - A list.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
(swap-elements '(1 2 3 4 5 6 7 8 9 10))

=> (2 1 4 3 6 5 8 7 10 9)

(swap-elements '(1 2 3 4 5 6 7 8 9))

=> (2 1 4 3 6 5 8 7 9)

|#
;;; SYNOPSIS
(defun swap-elements (list)
;;; ****
  (let ((result (loop for a in list by #'cddr and b in (cdr list) by #'cddr 
                   collect b collect a)))
    (if (oddp (length list))
        (append result (last list))
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 23:06:43 BST 2012: Added robodoc entry

;;; ****f* utilities/factor
;;; DESCRIPTION
;;; Boolean test to check if a specified number is a multiple of a second
;;; specified number.
;;; 
;;; ARGUMENTS
;;; - A number that will be tested to see if it is a multiple of the second
;;;   number. 
;;; - A second number that is the base number for the factor test.
;;; 
;;; RETURN VALUE
;;; T if the first number is a multiple of the second number, otherwise NIL.
;;; 
;;; EXAMPLE
#|
(factor 14 7)

=> T

|#
;;; SYNOPSIS
(defun factor (num fac)
;;; ****
  (when (zerop fac)
    (error "utilities::factor: 2nd argument cannot be 0: (factor ~a ~a)"
           num fac))
  (zerop (mod num fac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May 21 09:55:17 EDT 2012: Added robodoc entry

;;; ****f* utilities/octave-freqs
;;; DESCRIPTION
;;; A boolean test to determine whether two specified frequencies are octave
;;; transpositions of the same pitch class. 
;;; 
;;; ARGUMENTS
;;; - A first number that is a frequency in Hertz.
;;; - A second number that is a frequency in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether identical frequencies ("unison") are also
;;;   to be considered octave transpositions of the same pitch class. 
;;;   T = unisons are also octaves. Default = T.
;;; 
;;; RETURN VALUE
;;; T or NIL.
;;; 
;;; EXAMPLE
#|
(octave-freqs 261.63 2093.04)

=> T

(octave-freqs 261.63 3000.00)

=> NIL

(octave-freqs 261.63 261.63)

=> T

(octave-freqs 261.63 261.63 nil)

=> NIL

|#
;;; SYNOPSIS
(defun octave-freqs (freq1 freq2 &optional (unison-also t))
;;; ****
  (values (or (and unison-also (= freq1 freq2))
              (and (/= freq1 freq2)
                   (or (power-of-2 (/ freq1 freq2))
                       (power-of-2 (/ freq2 freq1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 23:25:30 BST 2012: Added robodoc entry
;;; SAR Mon May 21 09:19:54 EDT 2012: Added to robodoc entry

;;; ****f* utilities/partial-freqs
;;; DATE
;;; 13-Dec-2011
;;;
;;; DESCRIPTION
;;; A Boolean test to determine whether either of two specified frequencies
;;; can be considered a harmonic partial of the other.
;;; 
;;; ARGUMENTS
;;; - A first frequency in Hertz.
;;; - A second frequency in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether identical frequencies ("unison") are also to
;;;   be considered partials of each other. T = unison are partials. 
;;;   Default = T.
;;; 
;;; RETURN VALUE
;;; T if one of the frequencies has the ratio of a harmonic partial to the
;;; other, otherwise NIL.
;;; 
;;; EXAMPLE
#|
(partial-freqs 300 900)

=> T

(partial-freqs 300 700)

=> NIL

(partial-freqs 300 300)

=> T

(partial-freqs 300 300 nil)

=> NIL

|#
;;; SYNOPSIS
(defun partial-freqs (freq1 freq2 &optional (unison-also t))
;;; ****
  (values (or (and unison-also (= freq1 freq2))
              (and (/= freq1 freq2)
                   (or (factor freq1 freq2)
                       (factor freq2 freq1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun path-minus-extension (path)
  (values
   (format nil "~a~a~a" 
           (if (pathname-device path)
               (format nil "~a:" (pathname-device path))
               "")
           (directory-namestring path)
           (pathname-name path))
   (pathname-type path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mods (counter int-list)
  (unless (listp int-list)
    (setf int-list (list int-list)))
  (let ((return nil))
    (unless (zerop counter) ;; ignore the first
      (loop for int in int-list do
           (unless (integerp int)
             (error "mods: elements of 2nd argument list should be integers"))
           (when (zerop (mod counter int))
             (setf return t)
             (return))))
    return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 23:32:57 BST 2012: Added robodoc entry

;;; ****f* utilities/get-harmonics
;;; DESCRIPTION
;;; Return a list of the harmonic partial frequencies in Hertz above a
;;; specified fundamental frequency.
;;; 
;;; ARGUMENTS
;;; - A number that is the fundamental frequency in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :start-at. An integer that is the number of the first harmonic partial to
;;;   return. Default = 1.
;;; - :min-freq. A number that is the lowest frequency in Hertz to
;;;   return. Default = 20.
;;; - :max-freq. A number that is the highest frequency in Hertz to
;;;   return. Default = 20000.
;;; 
;;; RETURN VALUE
;;; A list of numbers that are the frequencies in Hertz of harmonic partials
;;; above the same fundamental frequency.
;;; 
;;; EXAMPLE
#|
;;; Get the first 15 harmonic partials above a fundamental pitch of 64 Hertz,
;;; starting with partial 2, and specifying an upper cut-off of 1010 Hz.

(get-harmonics 63 :start-at 2 :max-freq 1010)

=> (126 189 252 315 378 441 504 567 630 693 756 819 882 945 1008)

|#
;;; SYNOPSIS
(defun get-harmonics (fundamental &key (start-at 1) (min-freq 20)
                      (max-freq 20000))
;;; ****
  (loop for h from start-at
     for freq = (* fundamental h)
     while (<= freq max-freq)
     if (>= freq min-freq)
     collect freq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 23:42:18 BST 2012: Added robodoc entry

;;; ****m* utilities/db2amp 
;;; DESCRIPTION
;;; Convert a decibel value to a standard digital amplitude value (>0.0 to 1.0),
;;; whereby 0dB = 1.0.
;;; 
;;; ARGUMENTS
;;; - A number that is a value in decibel.
;;; 
;;; RETURN VALUE
;;; A decimal number between >0.0 and 1.0.
;;; 
;;; EXAMPLE
#|
(db2amp -3)

=> 0.70794576

|#
;;; SYNOPSIS
(defmacro db2amp (db)
;;; ****
  `(expt 10.0 (/ ,db 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 23:49:18 BST 2012: Added robodoc entry

;;; ****m* utilities/amp2db 
;;; DESCRIPTION
;;; Convert a standard digital amplitude value (>0.0 to 1.0) to a corresponding
;;; decibel value.
;;; 
;;; ARGUMENTS
;;; - A decimal number between >0.0 and 1.0.
;;; 
;;; RETURN VALUE
;;; A decimal number that is a value in decibel.
;;; 
;;; EXAMPLE
#|
(amp2db 0.3)

=> -10.457575

|#
;;; SYNOPSIS
(defmacro amp2db (amp)
;;; ****
  `(* 20.0 (log ,amp 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 23:53:34 BST 2012: Added robodoc entry

;;; ****f* utilities/remove-all
;;; DESCRIPTION
;;; Remove all of the specified elements from a list, returning a list
;;; containing only those elements that are not in the first argument list.
;;; 
;;; ARGUMENTS
;;; - A first list that is the list of items to remove.
;;; - A second list that is the original list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A predicate for testing equality between the elements of the two lists. 
;;;   Default = #'eq.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
(remove-all '(3 5 8 13) '(1 2 3 4 5 6 7 8 9 10 11 12 13))

=> (1 2 4 6 7 9 10 11 12)

|#
;;; SYNOPSIS
(defun remove-all (rm-list list &optional (test #' eq))
;;; ****
  (loop for rm in rm-list do
       (setf list (remove rm list :test test)))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 23:59:45 BST 2012: Added robodoc entry

;;; ****f* utilities/amplitude-to-dynamic
;;; DESCRIPTION
;;; Convert a specified digital amplitude between 0.0 and 1.0 to a
;;; corresponding dynamic between niente and ffff.
;;; 
;;; ARGUMENTS
;;; - A decimal number between 0.0 and 1.0.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print a warning if the specified
;;;   amplitude is <0.0 or >1.0. T = warn. Default = T.
;;; 
;;; RETURN VALUE
;;; A symbol that is a dynamic level.
;;; 
;;; EXAMPLE
#|
(amplitude-to-dynamic 0.3)

=> PP

|#
;;; SYNOPSIS
(defun amplitude-to-dynamic (amp &optional (warn t))
;;; ****
  (let ((dynamics '(niente pppp ppp pp p mp mf f ff fff ffff)))
    (flet ((warn-range (direction)      ; t=too high, nil= too low
             (when warn
               (if direction
                   (warn "utilities::amplitude-to-dynamic: ~a is > 1.0. ~
                          setting to maximum dynamic." amp)
                   (warn "utilities::amplitude-to-dynamic: ~a is < 0.0. ~
                          setting to minimum dynamic." amp)))))
      (cond ((> amp 1.0) (warn-range t) (first (last dynamics)))
            ((< amp 0.0) (warn-range nil) (first dynamics))
            (t (nth (round (* 10 amp)) dynamics))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue May  8 00:11:23 BST 2012: Added to robodoc entry

;;; ****f* utilities/dynamic-to-amplitude
;;; DESCRIPTION

;;; Convert a symbol that is a dynamic level between niente and ffff to a
;;; corresponding digital amplitude value between 0.0 and 1.0.
;;; 
;;; ARGUMENTS
;;; - A symbol that is a dynamic level between niente and fff.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print a warning when the symbol specified
;;;   is not recognized as a dynamic. T = warn. Default = T.
;;; 
;;; RETURN VALUE
;;; A decimal number between 0.0 and 1.0.
;;; 
;;; EXAMPLE
#|
(dynamic-to-amplitude 'fff)

=> 0.9

|#
;;; SYNOPSIS
(defun dynamic-to-amplitude (dynamic &optional (warn t))
;;; ****
  (let ((pos (position dynamic '(niente pppp ppp pp p mp mf f ff fff ffff))))
    (if pos
        (/ pos 10.0)
        (when warn
          (warn "utilities::dynamic-to-amplitude: unrecognised dynamics: ~a"
                dynamic)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue May  8 00:16:57 BST 2012: Added robodoc entry

;;; ****f* utilities/move-elements
;;; DATE
;;; 02-Mar-2011
;;;
;;; DESCRIPTION
;;; Move the specified elements from one list (if they are present in that
;;; list) to another, deleting them from the first.
;;; 
;;; ARGUMENTS
;;; - A list of elements that are the elements to be moved.
;;; - A list from which the specified elements are to be moved and deleted.
;;; - A list to which the specified elements are to be moved.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A predicate by which to test that the specified elements are equal to
;;;   elements of the source list. Default = #'eq.
;;; 
;;; RETURN VALUE
;;; Two values: A first list that is the source list after the items have been
;;; moved; a second list that is the target list after the items have been
;;; moved. 
;;; 
;;; EXAMPLE
#|
(move-elements '(3 5 8) '(1 2 3 4 5 6 7 8 9) '(a b c d e))

=> (1 2 4 6 7 9), (8 5 3 A B C D E)

|#
;;; SYNOPSIS
(defun move-elements (what from to &optional (test #'eq))
;;; ****
  (unless (listp what)
    (setf what (list what)))
  (loop for el in what do
       (when (member el from :test test)
         (setf from (remove el from))
         (push el to)))
  (values from to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue May  8 00:26:00 BST 2012: Added robodoc entry

;;; ****f* utilities/move-to-end
;;; DATE
;;; 22-May-2011
;;; 
;;; DESCRIPTION
;;; Move a specified element of a given list to the end of the list, returning
;;; the new list. 
;;;
;;; NB: If the element exists more than once in the given list, all but on of
;;;     the occurrences will be removed and only one of them will be placed at
;;;     the end.
;;; 
;;; ARGUMENTS
;;; - An item that is an element of the list that is the second argument.
;;; - A list.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
;;; All unique items
(move-to-end 2 '(1 2 3 4 5))

=> (1 3 4 5 2)

;;; Duplicate items
(move-to-end 2 '(1 2 3 2 4 2 5))

=> (1 3 4 5 2)

|#
;;; SYNOPSIS
(defun move-to-end (what list &optional (test #'eql))
;;; ****
  (if (member what list :test test)
      (econs (remove what list :test test) what)
      list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun force-list (thing)
  (if (listp thing)
      thing
      (list thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue May  8 00:32:05 BST 2012: Added robodoc entry

;;; ****f* utilities/hailstone
;;; DESCRIPTION
;;; Implementation of the Collatz conjecture (see
;;; http://en.wikipedia.org/wiki/Collatz_conjecture )
;;;
;;; The Collatz conjecture suggests that by starting with a given number, and
;;; if it is even dividing it by two or if it is odd multiplying it by three
;;; and adding one, then repeating with the new result, the process will
;;; eventually always result in one.
;;; 
;;; ARGUMENTS
;;; - A number to start with.
;;; 
;;; RETURN VALUE
;;; A list of the results collected from each iteration starting with the
;;; specified number and ending with one.
;;; 
;;; EXAMPLE
#|
(hailstone 11)

=> (11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)

|#
;;; SYNOPSIS
(defun hailstone (n)
;;; ****
  (loop collect n while (> n 1) 
     do (setf n (if (oddp n)
                    (1+ (* 3 n))
                    (/ n 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Dec 15 15:57:32 2011

;;; SAR Thu Dec 15 11:45:12 GMT 2011: added carriage returns to formatted text

(defun load-from-examples-dir (file)
  (format t "~%~%*******  Loading ~a~%" file)
  (load 
     (format nil "~a/doc/examples/~a"
             cl-user::+slippery-chicken-home-dir+ file)))

(defun load-from-test-suite-dir (file)
  (format t "~%~%*******  Loading ~a~%" file)
  (load 
     (format nil "~atest-suite/~a" cl-user::+slippery-chicken-home-dir+ file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri May  4 11:01:14 2012 
(defun safe-subseq (seq start &optional end)
  (let* ((len (length seq)))
    (when (and (integerp end) (> end len))
      (setf end len))
    (when (and (integerp start) (< start len))
      (subseq seq start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lsp
