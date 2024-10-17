;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/utilities
;;; NAME 
;;; utilities
;;;
;;; File:             utilities.lsp
;;;
;;; Class Hierarchy:  none: no classes defined
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Various helper functions of a general nature.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    June 24th 2002
;;;
;;; $$ Last modified:  11:21:47 Thu Oct 17 2024 CEST
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
;;; separator is what separates mins and secs; default is e.g. 12:06
;;; same-width t would make sure all those minutes under 10 are written in the
;;; format e.g. 00:12.2 

;;; ****f* utilities/secs-to-mins-secs
;;; DESCRIPTION
;;; Convert a number of seconds into a string of the form "24:41.723" where
;;; seconds are always rounded to three decimal places (i.e. milliseconds).
;;; 
;;; ARGUMENTS
;;; - the number of seconds
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :post-mins. The string used to separate minutes and seconds. Default ":"
;;; - :post-secs. The string used to separate seconds and milliseconds.
;;;    Default "." 
;;; - :post-msecs. The string used to follow milliseconds. Default "" 
;;; - :same-width. Ensure minutes values are always two characters wide, like
;;;   seconds, i.e with a leading 0.
;;; - :round. Round to the nearest second and don't print milliseconds. Default
;;;   NIL. 
;;; 
;;; RETURN VALUE
;;; A string
;;; 
;;; EXAMPLE
#|
(secs-to-mins-secs 77.1232145)
"1:17.123"
(secs-to-mins-secs 67.1)
"1:07.100"
(secs-to-mins-secs 67.1 :same-width t)
"01:07.100"
(secs-to-mins-secs 67.1 :same-width t :post-secs "s")
"01:07s100"
(secs-to-mins-secs 67.1 :post-secs "secs" :post-mins "min" :post-msecs "msecs")
"1min07secs100msecs"
(secs-to-mins-secs 67.7 :same-width t :round t)
"01:08"
|#
;;; SYNOPSIS
(defun secs-to-mins-secs (seconds &key
                                    round
                                    (post-mins ":")
                                    (post-secs ".")
                                    (post-msecs "")
                                    (same-width nil))
;;; ****
  (unless (and (numberp seconds) (>= seconds 0))
    (error "utilities::secs-to-mins-secs: ~a should be a number > 0." seconds))
  (when round
    (setf seconds (round seconds)))
  (multiple-value-bind
        (minutes seconds)
      (floor seconds 60)
    ;; the formatting of the seconds results in rounding up 59.99999 to 60.0
    ;; which is not what we want...  
    (when (> seconds 59.999)
      (setf seconds 0.0)
      (incf minutes))
    ;; MDE Tue Jul  3 18:57:44 2012 -- updating to avoid decimal point as that
    ;; confuses CCL in file names 
    (let* ((secs (floor seconds))
           (ms (floor (* 1000 (decimal-places (- seconds secs) 3))))
           (result
            (if same-width
                (format nil "~2,'0d~a~2,'0d~a~3,'0d~a"
                        minutes post-mins secs post-secs ms post-msecs)
                (if (> minutes 0)
                    (format nil "~d~a~2,'0d~a~3,'0d~a" minutes post-mins secs
                            post-secs ms post-msecs)
                    (format nil "~d~a~3,'0d~a" secs post-secs ms post-msecs)))))
      (if round 
          (subseq result 0 (- (length result) 3 (length post-secs)
                              (length post-msecs)))
          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/mins-secs-to-secs
;;; DESCRIPTION
;;; Derive the number of seconds from a minutes-seconds value that is indicated
;;; as a string of the form "0:00.000" or a two-item list in the form '(minutes
;;; seconds) or three-item list in the form '(minutes seconds milliseconds)
;;; 
;;; ARGUMENTS
;;; - A time in minutes and seconds, as described above.
;;;
;;; OPTIONAL ARGUMENTS
;;; - if a string is to be passed, then a character that denotes the separator
;;; between minutes and seconds. Default = #\:
;;; 
;;; RETURN VALUE
;;; A decimal number that is a number in seconds.
;;; 
;;; EXAMPLE
#|
(mins-secs-to-secs '(2 1))
=> 121.0
(mins-secs-to-secs '(16 59 534)))
=> 1019.534 
(mins-secs-to-secs "3:06.829"))
=> 186.829
;; using a different separator character between minutes and seconds
(mins-secs-to-secs "3-36.29" #\-) 0.0001)
=> 216.29
|#
;;; SYNOPSIS
(defun mins-secs-to-secs (time &optional (post-mins #\:))
;;; ****
  (flet ((secs-msecs (secs msecs)
           (when (or (> secs 60)
                     (> msecs 1000))
             (error "utilities::mins-secs-to-secs: secs = ~a ~
                               millisecs = ~a???" secs msecs))))
    (cond ((not time) nil)
          ((numberp time) time)
          ;; MDE Thu Aug 29 13:30:34 2019 -- allow strings like "12:36.23"
          ((stringp time) (mins-secs-to-secs-aux time post-mins))
          ((= 2 (length time))
           (let ((mins (first time))
                 (secs (second time)))
             (secs-msecs secs 0)
             (+ secs (* 60.0 mins))))
          ((= 3 (length time))
           (let ((mins (first time))
                 (secs (second time))
                 (msecs (third time)))
             (secs-msecs secs msecs)
             (+ secs (/ msecs 1000.0) (* 60.0 mins))))
          (t (error "utilities::mins-secs-to-secs: arg must be a 2- or ~
                         3-element list (mins secs [millisecs]): ~a"
                    time)))))

(defun mins-secs-to-secs-aux (string &optional (post-mins ":"))
  (let* ((pos (position post-mins string))
         (mins (read-from-string (subseq string 0 pos)))
         (secs (read-from-string (subseq string (1+ pos)))))
    (+ (* 60.0 mins) secs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/agnostic-directory-pathname
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; March 17th 2024.
;;;
;;; DESCRIPTION
;;; A replacement for directory-namestring, as that function does not return
;;; device names when used on windows.
;;;
;;; ARGUMENTS
;;; a string representing a pathname.
;;;
;;; RETURN VALUE
;;; a pathname - to a directory without filename and type
;;;
;;; SYNOPSIS
(defun agnostic-directory-pathname (namestring)
;;; ****
  (make-pathname :directory  (pathname-directory namestring)
                 :device (pathname-device namestring)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NB check whether a floating point number is very close to an integer. NB
;;; returns T also with an integer.
(defun float-int-p (x &optional (tolerance 0.000001d0))
  (and (numberp x)
       (equal-within-tolerance 0.0 (nth-value 1 (round x)) tolerance)))

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
(defun number-between (x lower upper)
  (and (numberp x)
       (>= x lower)
       (<= x upper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/nearest
;;; DATE
;;; 15th May 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Return the nearest number in a list to the first argument
;;; 
;;; ARGUMENTS
;;; - the number we're looking to get the closest to
;;; - the list of numbers we'll search
;;; 
;;; RETURN VALUE
;;; the element of the list that's closest to the first argument, the list
;;; sorted by nearest to the number, the distances to the number for the sorted
;;; list. 
;;;
;;; OPTIONAL ARGUMENTS
;;; none
;;; 
;;; EXAMPLE
#|
(nearest 1.21 '(4 2 5 3 5 4 1.2 1.3 1.1999))
--> 1.2
|#
;;; SYNOPSIS
(defun nearest (num list)
;;; ****
  (unless (every #'numberp (cons num list))
    (error "utilities::nearest: first argument and list must be numbers"))
  (let* ((lds (loop for n in list collect (list n (abs (- num n)))))
         (sorted (sort lds #'(lambda (x y)
                               (< (second x) (second y)))))
         (ls (mapcar #'first sorted))
         (deltas (mapcar #'second sorted)))
    (values (first ls) ls deltas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (if pos 
      (concatenate 'string
                   (subseq string 0 pos)
                   with
                   (subseq string (+ pos wlen)))
      ;; MDE Sat Dec 15 16:37:20 2018 -- return unmodified if what not found
      string)))

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
(defun split-into-sub-groups2 (list length &optional shuffle)
;;; ****
  (let ((result (loop for i from 0 by length while list collect
                         (loop repeat length while list collect (pop list)))))
    (if shuffle
        (multi-shuffle result 3) ; fixed-seed randomness by default
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; max-perms is the maximum length of any single list-permutation we perform
(defun sub-groups (groups &optional (max-perms 100))
  (when (simple-listp groups) ; could be just one list we want sub-groups form
    (setq groups (list groups)))
  (loop for group in groups
     for glen = (length group)
     do
       (when (> glen 8)
         (warn "utilities::sub-groups: long lists; this might take a while"))
     appending
       (loop for len downfrom (1- glen) to 1
          for lperms = (list-permutations group len)
          appending (if (> (length lperms) max-perms)
                      (subseq lperms 0 max-perms)
                      lperms))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/split-into-sub-groups4
;;; DATE
;;; August 28th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Split into sub-groups with lengths defined by the integers in the second
;;; argument list. The difference here to related functions is that the given
;;; lengths will repeat circularly until no more elements of the first argument
;;; remain. 
;;; 
;;; ARGUMENTS
;;; - the list to split into sub-groups
;;; - a list of lengths, to be repeated
;;; 
;;; RETURN VALUE
;;; a list of sublists
;;; 
;;; EXAMPLE
#|

(split-into-sub-groups4 '(1 2 3 4 5 6 7 8 9 10 11 12) '(3 4))
==> ((1 2 3) (4 5 6 7) (8 9 10) (11 12))

(split-into-sub-groups4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14) '(3 4))
==> ((1 2 3) (4 5 6 7) (8 9 10) (11 12 13 14))

(split-into-sub-groups4 '(1 2 3 4 5 6 7 8 9 10 11 12) '(1 2 3 4))
==> '((1) (2 3) (4 5 6) (7 8 9 10) (11) (12))

|#
;;; SYNOPSIS
(defun split-into-sub-groups4 (list lengths)
;;; ****
  (let ((cscl (make-cscl lengths)))
    (loop while list 
       for n = (get-next cscl)
       collect
         (safe-subseq list 0 n)
       do
         (setq list (nthcdr n list)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a special case for dealing with large lists that need to be varied (e.g. the
;;; results of a (slow/long) procession.
(defun split-into-8-subgroups (list &optional cscls mirror)
  (let* ((sgs (split-into-sub-groups2 list (ceiling (length list) 8)))
         (result (interleave 
                  (loop for sg in sgs by #'cddr collect sg)
                  (loop for sg in (rest sgs) by #'cddr collect sg))))
    (when mirror (setq result (loop for sg in result
                                    collect (append sg (rest (reverse sg))))))
    (if cscls
      (mapcar #'make-cscl result)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whole-num-p (float &optional (allow-tolerance nil))
  (let ((test (mod float 1.0)))
    (if allow-tolerance
        (or (equal-within-tolerance 0.0 test) ;; 0.00001)
            (equal-within-tolerance 1.0 test))
        (values (zerop test)
                float))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/prime
;;; DATE
;;; August 5th 2019, Heidhausen
;;; 
;;; DESCRIPTION
;;; Taken from dlocsig.lisp by Fernando Lopez Lezcano (in the CLM package):
;;; Return T or NIL to indicated whether the argument is a prime number or not.
;;; 
;;; ARGUMENTS
;;; an integer (all other types, including floats, will trigger an error)
;;;
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; SYNOPSIS
(defun prime (val)
  ;;; ****
  (or (= val 2)
      (and (oddp val)
           (do ((i 3 (+ i 2))
                (lim (sqrt val)))
               ((or (= 0 (mod val i)) (> i lim))
                (> i lim))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/get-primes
;;; DATE
;;; July 6th 2022, Werden
;;; 
;;; DESCRIPTION
;;; Get all the prime numbers between minimum and maximum values
;;; 
;;; ARGUMENTS
;;; - the minimum (start point). Doesn't have to be a prime number.
;;; - the maximum (end point). Doesn't have to be a prime number.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - an integer to specify the maximum number of primes to return. If NIL
;;;   (default) then we'll stop at max
;;; 
;;; RETURN VALUE
;;; a list of prime numbers
;;; 
;;; EXAMPLE
#|
(get-primes 5 19) -> (5 7 11 13 17 19)
(get-primes 5 19 3) -> (5 7 11)
|#
;;; SYNOPSIS
(defun get-primes (min max &optional num)
;;; ****  
  (let ((result '()))
    (loop with count = 0 for i from min to max
          do
             (when (prime i)
               (push i result)
               (incf count))
             (when (and num (= num count))
               (return)))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (unless (zerop float) ; would cause division-by-zero error
    (whole-num-p (log float 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/nearest-power-of-2
;;; DESCRIPTION
;;; Return the closest number to the specified value that is a power of two but
;;; not greater than the specified value (unless the optional argument is T:
;;; see below).
;;; 
;;; ARGUMENTS
;;; - A number.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether we can return a power of 2 greater than the
;;;   argument, if that is nearer to the argument than the lower power or 2.
;;; 
;;; RETURN VALUE
;;; An integer that is a power of two.
;;; 
;;; EXAMPLE
#|
(nearest-power-of-2 31)

=> 16

(nearest-power-of-2 31 t)

=> 32

(nearest-power-of-2 32)

=> 32

(nearest-power-of-2 33)

=> 32

|#
;;; SYNOPSIS

(defun nearest-power-of-2 (num &optional allow>)
;;; ****
  (if (power-of-2 num)
      num
      (round (nearest-power-of-x num 2 allow>))))

(defun nearest-power-of-x (num x &optional allow>)
  (let* ((p2 (loop with p = 1 do
                  (if (> p num)
                      ;; MDE Mon Nov 26 18:15:59 2012 -- don't return 1/2...
                      (return (max 1 (/ p x)))
                      (setf p (* p x)))))
         (p2next (* x p2)))
    (if (and allow> (< (- p2next num) (- num p2)))
        p2next
        p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/almost-flatten
;;; DATE
;;; September 4th 2013
;;; 
;;; DESCRIPTION
;;; Similar to flatten but allows one level of nesting
;;; 
;;; ARGUMENTS
;;; A list with an arbitrary level of nesting.
;;; 
;;; 
;;; RETURN VALUE
;;; A list with a maximum of one level of nesting
;;; 
;;; EXAMPLE
#|

(almost-flatten '((1 (2 3 4) (5 (6 7) (8 9 10 (11) 12)) 13) 14 15 (16 17)))
=> (1 (2 3 4) 5 (6 7) 8 9 10 (11) (12) (13) 14 15 (16 17))

|#
;;; SYNOPSIS
(defun almost-flatten (nested-list)
;;; ****
;;; ****
  (cond ((null nested-list) nil)
        ((or (simple-listp nested-list) (atom nested-list)) (list nested-list))
        (t (append (almost-flatten (first nested-list))
                   (almost-flatten (rest nested-list))))))


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
  (when (and (numberp a) (numberp b))
    ;; BTW coercing a and b to 'double-float doesn't help if they are of
    ;; different float types to begin with.
    (<= (abs (- a b)) tolerance)))

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
;;; MDE Thu Dec 23 15:23:38 2021, Heidhausen -- removes a character from the end
;;; of a string if that character is the last in the string 
(defun minus-last-char (string char)
  (let ((len-1 (1- (length string))))
    (if (char= char (elt string len-1))
        (subseq string 0 len-1)
        string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minus-last-slash (string &optional only-at-end)
  ;; MDE Sat Sep  5 15:56:41 2015 -- 
  (when only-at-end (setf string (trailing-slash string)))
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
;;; Returns whether all the elements in the list are atoms (i.e. no
;;; sublists). NB passing NIL will result in T 
(defun simple-listp (list)
  (when (listp list)
    (loop for i in list unless (atom i) do (return nil) finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed May 25 19:34:59 2016 -- updated so that passing NIL won't return T
(defun list-of-numbers-p (list)
  (when (and list (listp list))
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
            length ~%of argument 1: ~a ~a ~a ~%~a"
           start how-many (length list) list))
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
;;; ****f* utilities/logarithmic-steps
;;; DESCRIPTION
;;; Create a list of numbers progressing from the first specified argument to
;;; the second specified argument over the specified number of steps using an
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
  ;; MDE Thu Jul 14 21:29:59 2016 -- could happen...
  (if (not env)
    point
    (let ((lastx (lastx env))
          (lasty (first (last env))))
      (cond ((> point lastx)
             (when warn
               (warn "interpolate: ~a is off the x axis of ~%~a~
                      ~%returning last y value: ~a"
                     point env lasty))
             lasty)
            ((< point (car env))
             (let (y1)
               ;; (error "interpolate: Can't interpolate ~a in ~a" point env))
               ;; MDE Thu Apr 23 09:48:57 2020, Heidhausen -- if our x values
               ;; start > the point we're looking for, return the first y value
               (warn "utilities::interp-aux: envelope starts later than point!~
                      ~%Returning first y value: ~a" (setq y1 (second env)))
               y1))
            (t (interp-aux point env scaler exp))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun interp-aux (point env scaler exp)
  (let ((here (loop for i in env by #'cddr and j from 1 do
    (if (<= point i) (return (+ j j -2))))))
    ;; rounding in making the new-env with new-lastx can cause the very last
    ;; event to be just a little bigger than the last x value in the new-env.
    ;; If this is the case, <here> will be nil so we better deal with this:
    (unless here 
      (setq here (- (length env) 2)))
    (when (zerop here)
      (setq here 2))
    ;; MDE Mon Apr  9 13:08:16 2012 -- catch divide by zero error
    (let ((x1 (nth (- here 2) env))
          (x2 (nth here env)))
      ;; MDE Mon May 14 12:26:16 2012 
      (unless (and (numberp x1) (numberp x2))
        (error "utilities::interp-aux: y values in envelope must be ~
                numbers: ~a"
               env))
      (if (= x1 x2)
        (progn
          (warn "utilities::interp-aux: weird values for interp-aux: ~a in ~a."
                point env)
          (nth (1- here) env))
        (get-interpd-y point 
                       x1 
                       (* scaler (nth (- here 1) env))
                       x2
                       (* scaler (nth (+ here 1) env))
                       exp)))))

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
  (unless env
    (warn "utilities::lastx: env should not be nil."))
  (let ((len (length env)))
    (when (oddp len) 
        (error "utilities::lastx: Wrong number of elements in ~a." env))
    (when env (nth (- len 2) env))))

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
;;;   scaling.  NB The -min/-max arguments are hard-limits only; they do not
;;;   factor into the arithmetic.
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
;;; - :first-x. If a number, scale the x-axis so that this is the first x-value.
;;;   This then ignores x-scaler.
;;; - :last-x. If a number, scale the x-axis so that this is the last x-value.
;;;   This then ignores x-scaler.
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

;;; 'Stretching' the envelope by providing first-x and last-x values
(scale-env '(1 0 5 1 20 0) 1 :first-x 0 :last-x 100)

=> (0.0 0 21.052631 1 100.0 0)

|#
;;; SYNOPSIS
(defun scale-env (env y-scaler &key x-scaler first-x last-x
                                 (x-min most-negative-double-float)
                                 (y-min most-negative-double-float)
                                 (x-max most-positive-double-float)
                                 (y-max most-positive-double-float))
;;; ****
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
     collect (cond ((or first-x last-x)
                    (let* ((old-first (first env))
                           (old-last (lastx env)))
                      (min x-max
                           (max x-min
                                (rescale x old-first old-last
                                         (or first-x old-first)
                                         (or last-x old-last))))))
                   (x-scaler (min x-max (max x-min (* x x-scaler))))
                   (t x))
     collect (min y-max (max y-min (* y y-scaler)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun env-y-min (env)
  (loop for y in (cdr env) by #'cddr minimize y))

(defun env-y-max (env)
  (loop for y in (cdr env) by #'cddr maximize y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jun 15 15:05:02 2017
;;; ****f* utilities/invert-env
;;; DATE
;;; June 15th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Invert an envelope so that its maximum value becomes its minimum,
;;; vice-versa, and everything inbetween.
;;; 
;;; ARGUMENTS
;;; A list of X-Y breakpoint pairs
;;; 
;;; RETURN VALUE
;;; A list of X-Y breakpoint pairs exhibiting the inversion.
;;; 
;;; EXAMPLE
#|
(invert-env '(0 0 100 1)) -> (0 1.0 100 0.0)
(invert-env '(0 .3 40 .4 100 .9)) -> (0 0.9 40 0.79999995 100 0.3)
(invert-env '(0 -.9 40 .4 100 .9)) -> (0 0.9 40 -0.39999998 100 -0.9)
|#
;;; SYNOPSIS
(defun invert-env (env)
;;; ****
  (let* ((min (env-y-min env))
         (max (env-y-max env))
         (range (- max min)))
    (loop for x in env by #'cddr and y in (cdr env) by #'cddr
       for d = (/ (- y min) range)
       collect x collect (between-extremes min max (- 1.0 d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/auto-scale-env
;;; DATE
;;; August 29th 2013
;;;
;;; DESCRIPTION
;;; Automatically scale both the x and y values of an envelope to fit within
;;; the given ranges. Normally we'll assume that the minimum and maximum Y
;;; values are present in the original envelope and so the automatically scaled
;;; envelope will represent these with the new minimum and maximum
;;; values. However sometimes an envelope doesn't range over the possible
;;; extremes, for example (0 .3 100 .6) where the y range is from 0 to 1. If
;;; this is the case and you need a scaled envelope to take this into account,
;;; then how is the original envelopes minimum and maximum values to the
;;; keyword argument :orig-y-range.
;;; 
;;; ARGUMENTS
;;; - The envelope: a list of x y pairs
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :x-min: The new minimum (starting) x value
;;; - :x-max: The new maximum (last) x value
;;; - :y-min: The new minimum (not necessarily starting!) y value
;;; - :y-max: The new maximum (not necessarily starting!) y value
;;; - :orig-y-range: a two-element list specifying the original envelope's
;;;   minimum and maximum values (see above).
;;; 
;;; RETURN VALUE
;;; The new envelope (list).
;;; 
;;; EXAMPLE
#|

(auto-scale-env '(0 0 10 1))
=>
(0.0 0.0 100.0 10.0)

(auto-scale-env '(-1 0 .3 -3 1 1) :y-min 5 :y-max 6 :x-min 2)
=>
(2.0 5.75 65.7 5.0 100.0 6.0))

(auto-scale-env '(0 1 5 1.5 7 0 10 1) :y-min -15 :y-max -4)
=>
(0.0 -7.6666665 50.0 -4.0 70.0 -15.0 100.0 -7.6666665))

(auto-scale-env '(0 .5 100 .5) :y-min 1 :y-max 2)
=> (0.0 1.0 100.0 1.0)

(auto-scale-env '(0 .5 100 .5) :y-min 1 :y-max 2 :orig-y-range '(0 1))
=> (0.0 1.5 100.0 1.5)

|#
;;; SYNOPSIS
(defun auto-scale-env (env &key
                             (x-min 0.0) (x-max 100.0)
                             (y-min 0.0) (y-max 10.0)
                             orig-y-range)
;;; ****
  (unless (and (> x-max x-min) (>= y-max y-min))
    (error "utilities::auto-scale-env: x-max (~a) must be > x-min (~a) and ~
            ~%y-max (~a) >= y-min (~a): ~%~a" x-max x-min y-max y-min env))
  (let* ((env-x-min (first env))
         (env-x-max (lastx env))
         (env-x-range (- env-x-max env-x-min))
         (env-y-min (if orig-y-range (first orig-y-range) (env-y-min env)))
         (env-y-max (if orig-y-range (second orig-y-range) (env-y-max env)))
         (env-y-range (- env-y-max env-y-min))
         (new-env-x-range (abs (- x-max x-min)))
         (new-env-y-range (abs (- y-max y-min)))
         (x-scaler (/ new-env-x-range env-x-range))
         ;; MDE Wed Jul 29 21:00:23 2015 -- we could have an envelope like (0
         ;; .5 100 .5) whereupon  there's no y-range at all and we'd get a
         ;; division-by-zero error
         (y-scaler (if (zerop env-y-range)
                       1.0
                       (/ new-env-y-range env-y-range))))
    (loop for x in env by #'cddr and y in (cdr env) by #'cddr
       collect (float (+ x-min (* (- x env-x-min) x-scaler)))
       collect (float (+ y-min (* (- y env-y-min) y-scaler))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/exaggerate-env
;;; DESCRIPTION
;;; Makes the y values in an envelope more radically pushed towards its
;;; extremes. Y values below the mid-point will be pushed downwards; those
;;; above will be pushed upwards. The opposite can be accomplished by making
;;; the exponent argument > 1 (see below).
;;; 
;;; ARGUMENTS
;;; - the envelope: a list of numbers representing an envelope: x y pairs
;;; - the exponent: this determines the amount of
;;;   exaggeration. Counterintuitively perhaps, the lower values are than 1 the
;;;   more exaggeration takes place. Values > 1 will mean the opposite:
;;;   understated y values, if you will.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - easy-expt: because of the counterintuitive nature of the exponent, you
;;;   can pass values between -10 and +10 if this third argument is T. This
;;;   will be scaled to useful though not over-extreme exponents of 1.9 (-10)
;;;   to .1 (+10) with 0 equating to an exponent of 1, i.e. no change.
;;; 
;;; RETURN VALUE
;;; The new exaggerated envelope (a list).
;;; 
;;; EXAMPLE
#|
(exaggerate-env '(0 0 50 .8 100 1) 1.9)
--> (0 0.0 50 0.6894338 100 1.0)
(exaggerate-env '(0 0 50 .8 100 1) .1)
--> (0 0.0 50 0.9751001 100 1.0)
(exaggerate-env '(0 0 50 .8 100 1) -10)
--> (0 0.0 50 83.19083 100 1.0)
(exaggerate-env '(0 0 50 .8 100 1) -10 t)
--> (0 0.0 50 0.6894338 100 1.0)
(exaggerate-env '(0 0 50 .8 100 1) 0 t)
--> (0 0.0 50 0.8 100 1.0)
(exaggerate-env '(0 0 50 .8 100 1) 10 t)
--> (0 0.0 50 0.9751001 100 1.0)
|#
;;; SYNOPSIS
(defun exaggerate-env (env expt &optional easy-expt)
;;; ****
  (let* ((env-y-min (env-y-min env))
         (env-y-max (env-y-max env))
         (env-y-range (- env-y-max env-y-min))
         (env-y-range2 (/ env-y-range 2.0))
         (env-y-mid (+ env-y-min (* .5 env-y-range))))
    (loop for x in env by #'cddr and y in (cdr env) by #'cddr
       ;; current y distance from the overall y mid-point
       for ydist = (- y env-y-mid)
       ;; normalised 0. to 1.
       for ydistf = (/ ydist env-y-range2)
       for ydistfe = (expt (abs ydistf) 
                           (if easy-expt
                               (progn
                                 (unless (and (>= expt -10)
                                              (<= expt 10))
                                   (error "utilities::exaggerate-env: ~
                                           when <easy-expt> is T, the exponent
                                           should be between -10 and 10: ~a"
                                          expt))
                                 (fscale expt -10 10 1.9 .1))
                               expt))
       for newy = (progn
                    (when (< ydistf 0)
                      (setf ydistfe (- ydistfe)))
                    (+ env-y-mid (* ydistfe env-y-range2)))
       collect x
       collect newy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Sep  5 19:02:18 2013
;;; ****f* utilities/decimate-env
;;; DESCRIPTION
;;; Reduce the number of x,y pairs in an envelope.  In every case the envelope
;;; is first stretched along the x-axis to fit the new number of points
;;; required.  Then we proceed by one of three methods:
;;; 
;;; 1) average: for every new output x value, interpolate 100 times from -0.5
;;; to +0.5 around the point, then average the y value.  This will catch
;;; clustering but round out spikes caused by them
;;; 
;;; 2) points: also an averaging method but only using the existing points in
;;; the original envelope (unless none is present for a new x value, whereupon
;;; interpolation is used): Take an average of the (several) points nearest the
;;; new output point. This might not recreate the extremes of the original
;;; envelope but clustering is captured, albeit averaged.
;;; 
;;; 3) interpolate: for each new output point, interpolate the new y value from
;;; the original envelope.  This will leave out details in the case of
;;; clustering, but accurately catch peaks if there are enough output points.
;;; 
;;; In each case we create an even spread of x values, rather than clustering
;;; where clusters exist in the original.
;;; 
;;; ARGUMENTS
;;; - the original envelope (list of x,y values on any scales).
;;; - the number of points required in the output list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the method to be applied (symbol): 'points, 'average, 'interpolate.
;;;   Default = 'points.
;;; 
;;; RETURN VALUE
;;; A list representing the x,y values of the new envelope
;;; 
;;; EXAMPLE
#|

(decimate-env '(0 0 4 4 5 5 5.1 5.1 5.3 1 5.6 5.6 6 6 10 10) 6)
=>
(0.0 0.0 1 2.0 2 4.5 3 4.425 4 8.0 5.0 10.0)

|#
;;; SYNOPSIS
(defun decimate-env (env num-points &optional (method 'points))
;;; ****
  (let* ((e (auto-scale-env env :x-max (1- num-points) :y-min (env-y-min env)
                            :y-max (env-y-max env)))
         (length (length env))
         (first (subseq e 0 2))
         (last (subseq e (- length 2) length))
         (sums (ml 0.0 num-points))
         (nums (ml 0 num-points)))
    (case method
      (average
       (loop for x from 1 to (- num-points 2) do
            (loop for xf from (- x 0.5) by 0.01 repeat 100 do
                 (incf (nth x sums) (interpolate xf e))))
       (append first
               (loop for s in (butlast (cdr sums))
                  for x from 1 collect x collect (/ s 100.0))
               last))
      (points
       (loop for x in (cddr e) by #'cddr 
          for xi = (round x)
          for y in (cdddr e) by #'cddr do
          (incf (nth xi sums) y)
          (incf (nth xi nums)))
       (append first
               (loop for s in (cdr (butlast sums))
                  for n in (cdr nums)
                  for i from 1
                  collect i collect
                  (if (zerop n)
                      (interpolate i e)
                      (/ s n)))
               last))
      (interpolate
       (loop for x from 0 below num-points collect x  
          collect (interpolate x e)))
      (t (error "utilities::decimate-env: unknown method: ~a" method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/douglas-peucker
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-10-16
;;; 
;;; DESCRIPTION
;;; Implementation of the (Ramer-)Douglas–Peucker algorithm. This algorithm
;;; reduces the number of points in an envelope
;;; (cf. https://en.wikipedia.org/wiki/Ramer–Douglas–Peucker_algorithm). 
;;;
;;; ARGUMENTS
;;; - The envelope. Must be a list with xy-pairs.
;;; - The epsilon value. This value determines the degree of decimation by
;;;   defining the maximum distance between the original points and the
;;;   reduced/simplified envelope. The higher the value, the more the envelope
;;;   will be simplified.  Must be a float >= 0.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :sort. A function (e.g. #'<) indicating whether to sort the list by its
;;;   x-values before applying the algorithm. Default = NIL.
;;; 
;;; 
;;; RETURN VALUE
;;; Three values:
;;; - The simplified envelope.
;;; - The number of reduced/removed points.
;;; - The reduction ratio in percent (0.0-1.0).
;;;
;;; EXAMPLE
#|
(let ((env '(0. 0.481 0.626 1.394 3.052 1.458 3.13 3.397
             3.443 2.484 8.294 4.712 8.529 2.869 13.615
             3.189 17.293 5.673 19.092 4.856 23.552 5.144
             26.526 5.497 27.778 4.487 30.203 5.369 31.612
             4.054 34.585 5.577 34.664 3.59 36.62 5.337 39.515
             5.369 40.767 6.186 44.053 4.087 44.757 5.08 48.983
             4.103 49.609 2.997 55.634 5.272 56.495 3.958 56.495
             2.901 60.172 3.125 61.033 4.135 61.659 2.901 62.128
             3.958 64.241 7.276 65.649 2.58 65.962 3.253 65.962
             7.212 65.962 7.276 67.997 7.292 68.936 5.897 71.596
             7.372 72.926 3.462 73.865 7.548 74.413 5.577 77.7 4.663
             80.438 4.856 83.49 5.304 86.307 4.087 86.62 4.888 91.862
             2.837 94.053 4.167 95.931 5.321 97.418 4.952 100. 4.167))
      (epsilon 1.8))
  (douglas-peucker env epsilon))
;; =>
;; (0 0.481 3.13 3.397 13.615 3.189 17.293 5.673 34.585 5.577 34.664 3.59 40.767
;;  6.186 49.609 2.997 55.634 5.272 56.495 2.901 61.659 2.901 64.241 7.276
;;  65.649 2.58 65.962 7.276 71.596 7.372 72.926 3.462 73.865 7.548 77.7 4.663
;;  100 4.167)
|#
;;; SYNOPSIS
(defun douglas-peucker (env epsilon &key sort)
;;; ****
  (unless (<= 0.0 epsilon)
    (error "utilities::douglas-peucker: epsilon must be >= 0"))
  (unless (evenp (length env))
    (error "utilities::douglas-peucker: the envelope is malformed"))
  (let* ((points (env-to-xy-list env :sort sort))
         (max-distance 0)
         (idx 0)
         (num-pts (length points))
         (result '()))
    (loop for i from 1 to (- num-pts 2)
          for distance = (perpendicular-distance
                          (list (car points) (car (last points)))
                          (nth i points))
          do (when (> distance max-distance)
               (setf idx i)
               (setf max-distance distance)))
    (if (> max-distance epsilon)
        (let ((res1 (douglas-peucker (xy-list-to-env (butlast points
                                                            (- num-pts
                                                               (1+ idx))))
                                     epsilon))
              (res2 (douglas-peucker (xy-list-to-env (nthcdr idx points))
                                     epsilon)))
          (setf result (append (butlast res1 2) res2)))
        (setf result (xy-list-to-env (cons (car points) (last points)))))
    (let ((reduced-points (- num-pts (/ (length result) 2))))
      (values result
              reduced-points
              (/ reduced-points num-pts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; ****f* utilities/env-to-xy-list
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-10-16
;;; 
;;; DESCRIPTION
;;; This function converts an envelope-list of the form '(x1 y1 x2 y2 ...) to
;;; a nested list of x,y-pairs of the form '((x1 y1) (x2 y2) ...). 
;;;
;;; ARGUMENTS
;;; The envelope to be transformed. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :sort. Whether to sort the envelope by its x-values. Must be either NIL or
;;;   a function (e.g. #'<). Default = NIL.
;;; 
;;; RETURN VALUE
;;; The x,y-list. 
;;;
;;; EXAMPLE
#|
(env-to-xy-list '(0 2.4 4 1.3 40 3.3 100 0.1))
;; => ((0 2.4) (4 1.3) (40 3.3) (100 0.1))
|#
;;; SYNOPSIS
(defun env-to-xy-list (env &key sort)
;;; ****
  (unless (evenp (length env))
    (error "utilities::env-to-xy-list: The envelope is malformed."))
  (unless (or (functionp sort) (null sort))
    (error "utilities::env-to-xy-list: sort must be of type function."))
  (let ((res (loop for x in env by #'cddr and y in (rest env) by #'cddr
                   collect (list x y))))
    (if sort
        (sort res sort :key #'car)
        res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RP  Thu Oct 17 11:07:36 2024
(defun xy-list-p (thing)
  (every #'(lambda (x)
             (and (listp x)
                  (eq 2 (length x))))
         thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/xy-list-to-env
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-10-16
;;; 
;;; DESCRIPTION
;;; This function converts an xy-list of the form '((x1 y1) (x2 y2) ...) to
;;; an envelope list of the form '(x1 y1 x2 y2 ...). Cf. env-to-xy-list. 
;;;
;;; ARGUMENTS
;;; The xy-list to be converted. 
;;; 
;;; RETURN VALUE
;;; The envelope. 
;;;
;;; EXAMPLE
#|
(xy-list-to-env '((0 3.4) (30 5.6) (80 4.4) (100 0.1)))
;; => (0 3.4 30 5.6 80 4.4 100 0.1)
|#
;;; SYNOPSIS
(defun xy-list-to-env (xy-list)
;;; ****
  (unless (xy-list-p xy-list)
    (error "utilities::xy-list-to-env: The xy-list is malformed."))
  (loop for val in xy-list
        append (list (first val) (second val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/perpendicular-distance
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-10-16
;;; 
;;; DESCRIPTION
;;; Calculate the perpendicular distance between a line and a point.
;;;
;;; ARGUMENTS
;;; - The line. Must be a two-item list, each item containing the xy-coordinates
;;;   of the starting and end point of the line. E.g.: '((0 0) (2 3))
;;; - The point. Must be a two-item list with the coordinates of the point.
;;;   E.g.: '(2 2)
;;; 
;;; RETURN VALUE
;;; The perpendicular distance. 
;;;
;;; EXAMPLE
#|
(perpendicular-distance '((0 0) (10 0)) '(2 2))
;; => 2.0
|#
;;; SYNOPSIS
(defun perpendicular-distance (line point)
;;; ****
  (unless (and (listp line)
               (= 2 (length line))
               (every #'listp line)
               (every #'(lambda (x)
                          (= 2 (length x)))
                      line))
    (error "utilities::perpendicular-distance: line must be a two-item list ~
            with each item being a two-item list."))
  (unless (and (listp point)
               (= 2 (length point)))
    (error "utilities::perpendicular-distance: point must be a two-item list."))
  (let* ((p1 (first line))
         (p2 (second line))
         (x1 (first p1))
         (y1 (second p1))
         (x2 (first p2))
         (y2 (second p2))
         (x0 (first point))
         (y0 (second point)))
    (/ (abs
        (+ (- (* (- y2 y1) x0)
              (* (- x2 x1) y0))
           (- (* x2 y1) (* y2 x1))))
       (sqrt
        (+ (expt (- y2 y1) 2)
           (expt (- x2 x1) 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; These functions read a wavelab marker file and print the list of arguments
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
                       Two markers at same point: ~a" time))
             (setf last-time time)
             (if (oddp count)
                 (setf start time)
                 (progn 
                   ;; MDE Tue Sep 25 16:54:24 2012 -- wavelab 7 marker names
                   ;; default to "marker" 
                   (unless (or (string= name "*" :end1 1) 
                               (string= name "marker" :end1 6))
                     (warn "parse-wavelab-marker-file-for-sections: ~
                            Got marker with name \"~a\" ~%at ~a (pos ~a) ; ~
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
                   (agnostic-directory-pathname sndfile)
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
               (format txt "~&~a, ~,6f  ;" count time))
  (when eof 
    (terpri txt)
    (format t "~%~%~a markers read~%" count)
    (return t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jun 15 13:11:04 BST 2012: Added robodoc entry

;;; ****f* utilities/wavelab-to-audacity-marker-file
;;; DESCRIPTION
;;; Write a .txt file suitable for import to audacity with the same name and in
;;; the same directory as the file argument.
;;; 
;;; ARGUMENTS
;;; - A string that is the name of a wavelab marker file, including directory
;;;   path and extension.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer that is the sampling rate of the sound file to which the
;;;   wavelab marker file refers. This value will affect the times of the
;;;   output. 
;;; 
;;; RETURN VALUE
;;; Returns T and prints the number of markers read to the listener. 
;;; 
;;; EXAMPLE
#|
(wavelab-to-audacity-marker-file "/path/to/24-7.mrk"  44100)

=> 51 markers read

|#
;;; SYNOPSIS
(defun wavelab-to-audacity-marker-file (file &optional (sampling-rate 44100))
;;; ****
  (with-open-file 
      (mrk file :direction :input :if-does-not-exist :error)
    (with-open-file
        (txt (format nil "~a~a.txt" 
                     (agnostic-directory-pathname file)
                     (pathname-name file))
             :direction :output :if-exists :error)
      (loop 
         with name 
         with time 
         with count = 0
         with tab = #\tab
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
                   (format txt "~&~,6f~c~,6f~c~a" time tab time tab name)
                   (format txt "~&~,6f~c~,6f~c" time tab time tab)))
             (when eof 
               (terpri txt)
               (format t "~%~%~a markers read~%" count)
               (return t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/parse-wavelab-marker-file-for-loops
;;; DESCRIPTION
;;; Read a wavelab marker file and return its loop points as groups.
;;;
;;; The marker file must contain markers with the word "loop". A marker with
;;; that name will start a new set of loop points, and nameless markers will
;;; belong to the group until the next "loop" marker.
;;; 
;;; ARGUMENTS
;;; - A string that is the name of the marker file to be parsed, including
;;;   directory path and extension.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :sampling-rate. An integer that is the sampling rate of the sound file to
;;;   which the marker file refers. This value will affect the resulting time
;;;   points. Default = 44100.
;;; - :max-length. The maximum duration in seconds between two points: anything
;;;   greater than this will result in a warning being printed.
;;; 
;;; RETURN VALUE
;;; Returns a list of lists which are the grouped time points. 
;;;
;;; Also prints separate feedback to the listener.
;;; 
;;; EXAMPLE
#|
(parse-wavelab-marker-file-for-loops "/path/to/24-7loops1.mrk")

=>
WARNING:
   utilities::parse-wavelab-marker-file-for-loops 
   loop points 10:13.213 to 10:14.475 are too long (1.2620239)
WARNING:
   utilities::parse-wavelab-marker-file-for-loops 
   loop points 10:33.223 to 10:34.486 are too long (1.2630615)
WARNING:
   utilities::parse-wavelab-marker-file-for-loops 
   loop points 10:36.456 to 10:37.522 are too long (1.06604)


312 markers, 50 loops read

((25.674559 25.829296 26.116327 26.649048 27.038843)
 (32.211884 32.33669 32.481815 32.618233 32.716915 32.902676 33.227757
  33.61959)
 (36.893604 37.059048 37.160633 37.27383 37.439274 37.4683 37.627937)
 (39.52907 39.81932 39.999275 40.2634 40.338867 40.605896)
 (45.612698 45.818775 46.050976 46.145306 46.275192)
 (46.4566 46.644535 46.76934 46.886894 46.971066 47.16553)
 (84.15927 84.260864 84.292786 84.355194 84.47274 84.52789 84.556915
  84.65415)
 ...
 (655.91077 656.4554 656.80304 657.4519 658.04285 658.8192)
 (676.1075 676.79114 677.1503 677.57904 678.12366)
 (799.29205 799.8019 800.58984 800.96063 801.13446 801.45886)
 (804.98145 805.2016 805.5724 805.83887 806.31396))

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
                     (agnostic-directory-pathname file)
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

;;; SAR Fri Jun 15 13:02:50 BST 2012: Added robodoc entry

;;; ****f* utilities/parse-audacity-label-file-for-loops
;;; DESCRIPTION
;;; Read an audacity label file and return its loop points as groups.
;;;
;;; NB: If this fails it's probably because there's a tab between time and
;;;     label instead of spaces: save in emacs to detab.
;;;
;;; NB: Beware that marker files created on different operating systems from
;;;     the one on which this function is called might trigger errors due to
;;;     newline character mismatches.
;;; 
;;; ARGUMENTS
;;; - A string that is the name of the label file to be parsed, including
;;;   directory path and extension.
;;; 
;;; RETURN VALUE
;;; Returns a list of lists which are the grouped time points. 
;;;
;;; Also prints separate feedback to the listener.
;;; 
;;; EXAMPLE
#|
(parse-audacity-label-file-for-loops  "/path/to/24-7loops1.txt")

=>
313 markers, 50 loops read

((25.674559 25.829296 26.116327 26.649048 27.038843)
 (32.211884 32.33669 32.481815 32.618233 32.716915 32.902676 33.227757
  33.61959)
 (36.893604 37.059048 37.160633 37.27383 37.439274 37.4683 37.627937)
 (39.52907 39.81932 39.999275 40.2634 40.338867 40.605896)
 (45.612698 45.818775 46.050976 46.145306 46.275192)
 (46.4566 46.644535 46.76934 46.886894 46.971066 47.16553)
 (84.15927 84.260864 84.292786 84.355194 84.47274 84.52789 84.556915
  84.65415)
 ...
 (676.1075 676.79114 677.1503 677.57904 678.12366)
 (799.29205 799.8019 800.58984 800.96063 801.13446 801.45886)
 (804.98145 805.2016 805.5724 805.83887 806.31396))

|#
;;; SYNOPSIS
(defun parse-audacity-label-file-for-loops (label-file)
;;; ****
  (let ((loop-points '())
        (result '()))
    (flet ((write-loop-points ()
             (when loop-points
               (setq loop-points (nreverse loop-points))
               (when (< (length loop-points) 5)
                 (error "too few loop points at ~a" (first loop-points)))
               ;; the ~^ adds a space after the element only
               ;; when this isn't the last one.
               ;; (format t "~%(~{~a~^ ~})" loop-points)
               (push (copy-list loop-points) result)
               (setq loop-points nil))))
      (with-open-file 
          (mrk label-file :direction :input :if-does-not-exist :error)
        (loop with label with time
           with count = 0 
           with num-loops = 0 
           do
             (multiple-value-bind
                   (line eof)
                 (read-line mrk nil)
               ;; (print line)
               ;; (multiple-value-bind
               ;;   (label time)
               (setq label (read-audacity-line line))
               ;; (format t "~&~a ~a" label time)
               (incf count)
               (when (string= (third label) "loop")
                 ;; (print 'here)
                 (incf num-loops)
                 (write-loop-points))
               (setq time (first label))
               (when time
                 (push time loop-points))
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
;;; MDE Fri Jan 29 13:03:32 2021, Heidhausen -- added downcase optional arg
(defun get-parameter (string &optional (separator #\=) (downcase t))
  (flet ((trim-whitespace (string)
           (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) string)))
             (if downcase
                 (string-downcase trimmed)
                 trimmed))))
    (let ((sep-pos (position separator string)))
      (when sep-pos
        (let ((param (trim-whitespace (subseq string 0 sep-pos)))
              (value (trim-whitespace (subseq string (1+ sep-pos)))))
          (values param value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/filter-parameters
;;; DATE 
;;; July 1st 2021, Heidhausen
;;; 
;;; DESCRIPTION
;;; Though generally applicable, a reaper marker specifically could look like
;;; this:  ("MARKER" (67 709.726 "b364 F1 molto pesante" 0 0 1 B)) Allow
;;; therefore searching of the results from (get-parameters ...) for specific
;;; strings, returning whole parameter lists or specific elements thereof. NB
;;; The strings are case-insensitive.
;;; 
;;; ARGUMENTS
;;; - a list of parameters, where each element is a 2-element list: the
;;; parameter name and a list of associated data. I.e. this implies e.g. calling
;;; get-parameters with the optional to-line-end set to T
;;; - the string to search for in the data lists
;;; 
;;; OPTIONAL ARGUMENTS
;;; - a number to specify the nth element that will be returned from the data
;;; lists 
;;; 
;;; RETURN VALUE
;;; a list of matching data lists
;;; 
;;; EXAMPLE
#|
(filter-parameters
  (get-parameters "markers.RPP" '("MARKER") #\  t)
  "harmonic" 1)
-->
85 parameters read
(322.7273 527.37866 542.9216 686.73846 944.6802 952.6701)

;;; or without the nth option:

(filter-parameters
 (get-parameters "markers.RPP" '("MARKER") #\  t)
 "harmonic")
-->
((29 322.7273 "c harmonic resonance" 0 0 1 B
  {8F7A97DD-F0B6-4E4D-9F67-27635F109E05})
 (55 527.37866 "nice resonant harmonic pizz" 0 0 1 B
  {E380FEF5-FE44-3B47-9C43-2BA80A3D0FAC})
 (57 542.9216 "nice harmonic S trem" 0 0 1 B
  {04BB89FE-8FB8-5B4A-AB5F-EDE1F9C921CC})
 (65 686.73846 "b347 E7 nice single harmonic repeat" 0 0 1 B
  {586CDD49-BE23-F149-AABC-A98EDD6C6EEE})
 (79 944.6802 "b483: nice spe and harmonics" 0 0 1 B
  {11E361AD-22C8-0D42-AB91-ECCBCAAEA075})
 (80 952.6701 "b491 muted harmonics" 0 0 1 B
  {097D7A4D-E9F0-FE4B-8BE6-E7BC85E2969B}))

|#
;;; SYNOPSIS
(defun filter-parameters (parameters string &optional get-nth)
;;; ****
  (setq string (string-upcase string))
  (loop for parameter in parameters
     for para = (second parameter) 
     when (and (listp para)
               (some #'numberp
                     (mapcar #'(lambda (x)
                                 (search string
                                         (typecase x
                                           (string (string-upcase x))
                                           ;; will be uppercase because symbol
                                           (symbol (string x))
                                           (t "")))) ; i.e. skip
                             para)))
     collect (if get-nth
                 (nth get-nth para)
                 para)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/parse-reaper-file-for-loops
;;; DATE
;;; July 1st 2021
;;; 
;;; DESCRIPTION
;;; Read a reaper file and return its loop points as groups.
;;;
;;; The reaper file must contain markers with the word 'clm-loop-point-start'
;;; and 'clm-loop-point-stop' with any number of 'clm-loop-point' markers
;;; inbetween. This defines a new set of loop points, of which there can be any
;;; number. Any number of markers without these names can be between the -start
;;; and -stop markers; they will be ignored. It is assumed that the sound file
;;; you'll process starts at time zero in the reaper file and within itself.
;;; 
;;; ARGUMENTS
;;; - A string that is the name of the marker file to be parsed, including
;;;   directory path and extension.
;;;
;;; OPTIONAL ARGUMENTS:
;;; T or NIL to issue a warning if a marker is found beginning with
;;; 'clm-loop-point' but continuing with something other than -start or
;;; -stop. Default = T
;;;
;;; RETURN VALUE
;;; Returns a list of lists which are the grouped time points. 
;;; 
;;; SYNOPSIS
(defun parse-reaper-file-for-loops (reaper-file &optional (warn t))
;;; ****
  (let ((markers (filter-parameters
                  (get-parameters reaper-file '("MARKER") #\  t)
                  ;; this will get the whole data list for any clm-loop-point
                  ;; marker. the time is the 2nd element and the marker complete
                  ;; name is the third
                  'clm-loop-point))
        (result '())
        (lp '()))
    (flet ((saveit (list) (push (second list) lp))
           (gotit (lp sym)
             (let ((el (third lp)))
               (when (symbolp el)
                 (eq el sym)))))
      (loop for loop-point in markers do
         ;;(print loop-point)
           (cond ((gotit loop-point 'clm-loop-point-stop)
                  (when lp
                    (saveit loop-point)
                    (push (reverse lp) result)
                    (setq lp '())))
                 ((gotit loop-point 'clm-loop-point-start) 
                  (saveit loop-point))
                 ((gotit loop-point 'clm-loop-point)
                  (saveit loop-point))
                 (t (when warn
                      (format t "parse-reaper-file-for-loops: ignoring ~a"
                              (third loop-point)))))))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/parse-reaper-file-for-segment
;;; DATE
;;; July 5th 2021
;;; 
;;; DESCRIPTION
;;; Parse a reaper file and look for markers that correspond to the second
;;; argument. What we're after is sound file segments i.e. with a start and an
;;; end. So markers can be added with a certain symbol e.g. gran marking the
;;; start and that symbol with -end tacked on e.g. gran-end to indicate where a
;;; segment ends.
;;; 
;;; ARGUMENTS
;;; - the path to the reaper file
;;; - the marker name we'll look for
;;; 
;;; RETURN VALUE
;;; a list of start-end pairs (in seconds)
;;; 
;;; EXAMPLE
#|

(parse-reaper-file-for-segment "barbara-markers2.RPP" 'gran)
->  '((542.9326 548.098) (598.7433 600.6894) (944.8951 945.41925)
      (947.9406 948.8009) (952.61755 959.5987) (971.784 975.9188)))))

|#
;;; SYNOPSIS
(defun parse-reaper-file-for-segment (reaper-file marker)
;;; ****
  (let ((markers (filter-parameters
                  (get-parameters reaper-file '("MARKER") #\  t)
                  marker))
        (marker-end (read-from-string (format nil "~a-end" marker))))
    (loop for start in markers by #'cddr
       for end in (rest markers) by #'cddr do
         (unless (and (eq marker (third start))
                      (eq marker-end (third end)))
           (error "Markers ~a and ~a should be in ascending time order, ~
                   with no similarly named markers inbetween."
                  marker marker-end))
       collect (list (second start) (second end)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/get-parameters
;;; DATE
;;; January 29th 2021
;;; 
;;; DESCRIPTION
;;; A general routine for searching text files for parameters and their
;;; values. Here we search a file line by line, matching parameters and
;;; returning them in a list of parameter-value pairs. This is limited, however,
;;; to one parameter per line, and values of one word (i.e. numbers, strings,
;;; etc. not containing space), unless optional argument to-line-end is T (see
;;; below) 
;;; 
;;; ARGUMENTS
;;; - the text file to search
;;; - either a single string or list thereof to search for (case-sensitive)
;;; - the separator character which divides the parameter name from its value
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the parameter-value separator (character)
;;; - T or NIL to indicate whether all tokens after a recognised parameter
;;;   should be returned in a list. Default = NIL = get just one parameter.
;;; 
;;; RETURN VALUE
;;; A list of parameter-value pairs
;;; 
;;; EXAMPLE
#|
;;; search a reaper file, where parameters are followed simply by space rather
;;; than = or : E.g. a current reaper file has lines like:
      SNAPOFFS 0
      LENGTH 0.36292517006803
      LOOP 1
      ALLTAKES 0
      FADEIN 2 0 0 2 0 1 1
      FADEOUT 2 0 0 2 0 -1 -1
      MUTE 0 0
      MIXFLAG 1
      BEAT 2
      SEL 1
      IGUID {3B79D8DF-AC08-EC4F-B93C-CAFE24FA1CBB}
      IID 3
      NAME sunni-mosque.wav
      VOLPAN 1 0 1 -1
      SOFFS 0.67933106575964
      PLAYRATE 1 1 0 -1 0 0.0025
;;; hence:
(get-parameters "~/projects/sndfilenet/reaper/sunni-mosque-split.RPP"
                '("SOFFS" "LENGTH") #\ )
-->
362 parameters read
(("LENGTH" 0.36292517) ("SOFFS" 0.67933106) ("LENGTH" 0.38848072)
 ("SOFFS" 1.0422562) ("LENGTH" 1.4923356) ("SOFFS" 1.430737)
 ("LENGTH" 1.9968253) ("SOFFS" 2.9230726) ("LENGTH" 0.5023356)
 ("SOFFS" 4.919898) ("LENGTH" 0.4907483) ("SOFFS" 5.4222336)
 ("LENGTH" 0.17068027) ("SOFFS" 5.912982) ("LENGTH" 3.6765532)
...
|#
;;; SYNOPSIS
(defun get-parameters (file parameters &optional (separator #\=) to-line-end)
;;; ****
  (let ((count 0)
        (results '()))
    (setq parameters (force-list parameters))
    (with-open-file 
        (input file :direction :input :if-does-not-exist :error)
      (loop 
         (multiple-value-bind
               (line eof)
             (read-line input nil)
           (setq line (trim-leading-trailing-whitespace line))
           ;;(print line)
           (multiple-value-bind
                 (param value)
               (get-parameter line separator nil) ; don't downcase
             ;; (print param)
             (loop for p in parameters do
                  (when (string= param p)
                    (push (list p (read-from-string
                                   (if to-line-end
                                       ;; force a list
                                       (format nil "(~a)" value)
                                       value)))
                          results)
                    (incf count)))
             (when eof (return))))))
    (format t "~%~%~a parameters read~%" count)
    (nreverse results)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aux function for parse-wavelab-marker-file-for...

;;; (read-audacity-line "3.777420        loop")
;;; (read-audacity-line "3.777420")

(defun read-audacity-line-old (string &optional (separator #\Space))
  (flet ((trim-whitespace (string)
           (string-downcase
            (string-trim '(#\Space #\Tab #\Newline) string))))
    (let* ((sep-pos (position separator string))
           (time (trim-whitespace (subseq string 0 sep-pos)))
           (label nil))
      (when sep-pos
        (setf label (trim-whitespace (subseq string (1+ sep-pos)))))
      (values label time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aux function for parse-wavelab-marker-file-for...updated 2019 for new format
;;; (start \tab end \tab label) 

(defun read-audacity-line (string &optional (separator #\Tab))
  (flet ((trim-whitespace (str)
           (string-downcase
            (string-trim '(#\Space #\Tab #\Newline) str))))
    (let* ((sep-pos1 (position separator string))
           (sep-pos2 (when sep-pos1
                       (position separator (subseq string (1+ sep-pos1)))))
           start end label)
      (when sep-pos1
        (setq start (read-from-string
                     (trim-whitespace (subseq string 0 sep-pos1)))
              end (read-from-string
                   (trim-whitespace
                    (subseq string (1+ sep-pos1) (+ sep-pos1 sep-pos2)))
                   nil)
              label (trim-whitespace
                     (subseq string (+ 1 sep-pos1 sep-pos2)))))
      (list start end label))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/parse-audacity-label-file
;;; DATE
;;; August 12th 2019
;;; 
;;; DESCRIPTION
;;; Parse a labels file exported from Audacity and return a list of (start end
;;; label) triplets. Times are in seconds.
;;; 
;;; ARGUMENTS
;;; - the path to the label file
;;; 
;;; RETURN VALUE
;;; a list
;;; 
;;; SYNOPSIS
(defun parse-audacity-label-file (label-file)
;;; ****
  (with-open-file 
          (mrk label-file :direction :input :if-does-not-exist :error)
        (loop
           with count = 0 with labels with marker
           do
             (multiple-value-bind
                   (line eof)
                 (read-line mrk nil)
               (setq marker (read-audacity-line line))
               (unless (not (first marker))
                 (push marker labels)
                 (incf count))
               (when eof 
                 (format t "~%~%~a markers read~%" count)
                 (return (nreverse labels)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun audacity-mid-phrase-loops (file)
  (with-open-file 
      (in file :direction :input :if-does-not-exist :error)
    (with-open-file
        (out (format nil "~a~a-mid-phrase.~a" 
                     (agnostic-directory-pathname file)
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
           with phrase = '() with label do
             (multiple-value-bind
                   (line eof)
                 (read-line in nil)
               (when line
                 ;;(multiple-value-bind
                 ;;  (label time)
                 (setq label (read-audacity-line line))
                 (if (string= (third label) "loop")  
                     (progn
                       (split-phrase (nreverse phrase))
                       (setf phrase '()))
                     (push (first label) phrase)))
               (when eof
                 (split-phrase (nreverse phrase))
                 (return t))))))))
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun audacity-min-length (file &optional (min 0.05))
  (with-open-file 
      (in file :direction :input :if-does-not-exist :error)
    (with-open-file
        (out (format nil "~a~a-min.~a" 
                     (agnostic-directory-pathname file)
                     (pathname-name file)
                     (pathname-type file))
             :direction :output :if-exists :error)
      (loop with last = -999999.0 with label with time do
           (multiple-value-bind
                 (line eof)
               (read-line in nil)
             (when line
               ;; (multiple-value-bind
               ;;(label time)
               (setq label (read-audacity-line line))
               ;; don't check length if this is a loop begin
               (if (string= (third label) "loop")
                   (format out "~&~a" line)
                   (progn
                     (setq time (first label))
                     (when (> (- time last) min)
                       (setq last time)
                       (format out "~&~a" line)))))
             (when eof
               (return t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri May  8 15:33:52 2020, Heidhausen 
;;; at the moment this will fail on non-ascii chars
(defun file-to-string (file)
  (let (result)
    (with-open-file 
        (in file :direction :input :if-does-not-exist :error)
      (loop 
         (multiple-value-bind
               (line eof)
             (read-line in nil)
           (when line
             (push line result))
           ;; (print line)
           (when eof
             (return))))
      (list-to-string (nreverse result)))))

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
;;; MDE Fri Oct 20 20:55:52 2017 -- the remove function copies its sequences
;;; whereas with a list of objects we often don't want things copying so remove
;;; 'by hand'
(defun remove-with-id (list id)
  (if (every #'named-object-p list)
      (loop for no in list unless (id-eq id no) collect no)
      list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 19:12:20 BST 2012: Added robodoc entry

;;; ****f* utilities/pts2cm
;;; DESCRIPTION
;;; Convert a specified number of points to a length in centimeters at a
;;; resolution of 72ppi.
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
;;; Return a random number between two specified numbers. If the two numbers are
;;; integers, the random selection is inclusive. If either are floating-point
;;; (decimal) numbers, the result will be a float between the first (inclusive)
;;; and just less than the second (i.e. exclusive).
;;; 
;;; ARGUMENTS
;;; - A first, lower, number.
;;; - A second, higher, number. 
;;;
;;; NB: The first number must always be lower than the second.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the random seed should be fixed.
;;; - T or NIL to indicate whether, when fixed-random is set to T, we should
;;;   reset the random number generator (to guarantee the same random
;;;   sequences). This would generally only be called once, perhaps at the
;;;   start of a generation procedure.
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
;;; ****f* utilities/randomise
;;; DESCRIPTION
;;; Return a random decimal number close to (+ or -) the number specified
;;; (within a certain percentage of that number's value). Note that if you want
;;; the result to go from 0 to 2x the argument, then <percent> needs to be 200,
;;; not 100.
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
  (if (zerop percent)
      number
      (let ((room (* number (/ percent 200.0))))
        (between (- number room) (+ number room)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May  7 19:48:41 BST 2012: Added robodoc entry

;;; this returns a random portion of the number, +/- percent

;;; ****f* utilities/random-amount
;;; DESCRIPTION
;;; Return a random number from within a total range of <percent> of the given
;;; number, centering around zero. Thus, if the <number> is 100, and the
;;; <percent> is 5, the results will be a random number between -2.5 and +2.5.
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
  (if (zerop percent)
      0.0
      (let ((pc (float (/ percent 100.0))))
        (* number (+ (- (/ pc 2.0))
                     (random pc))))))

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

(defun write-to-file (file data)
;;; ****
  (with-open-file
      (stream file :direction :output :if-exists :supersede
              :if-does-not-exist :create)
    (print data stream)))

(defun read-from-default-dir-file (file)
  (read-from-file (concatenate 'string (get-sc-config 'default-dir) file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/read-file-as-string
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; April 30th 2023.
;;;
;;; DESCRIPTION
;;; Read an entire file (not just an s-expression) a into string and return it
;;; 
;;; ARGUMENTS
;;; - A string that is the path to a file (directory and filename)
;;; 
;;; RETURN VALUE
;;; A string with the contents of the file
;;; 
;;; EXAMPLE
#|
(read-from-file "/path/to/lisp-lorem-ipsum.txt")
|#
;;; SYNOPSIS
(defun read-file-as-string (infile)
;;; ****
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/edit-file
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; April 30th 2023.
;;;
;;; DESCRIPTION
;;; a simple wrapper to open a file as a string and save it into a lexical
;;; variable. This variable is then set to the return values of the expressions
;;; within the body of edit-file. Eventually the new value of the variable is
;;; written into the original file. This was originally designed to be used with
;;; the editing functions in reaper.lsp (set-track-channels etc.).
;;; 
;;; ARGUMENTS
;;; - A string that is the path to a file (directory and filename)
;;; - A name for the lexical variable - this can be used within body
;;; (without quote)
;;; 
;;; RETURN VALUE
;;; whatever was written into the file
;;; 
;;; EXAMPLE
#|
;;; set all faders of project.rpp to 0.5 and use "anything" as lexical variable
(edit-file "/E/project.rpp" anything
      (set-all-faders anything .5))
|#
#|
;;; more complex: insert a plugin on 3 tracks in two ways
(edit-file "/E/project.rpp" project
  (insert-plugin project *iem-stereo-encoder* 1)
  (insert-plugin project *iem-stereo-encoder* 2)
  (insert-plugin project *iem-stereo-encoder* 3))
;;; alternatively:
(edit-file "/E/project.rpp" project
  (loop for i from 1 to 3 with temp-var = project
     do (setf temp-var (insert-plugin temp-var *iem-stereo-encoder* i))
       finally (return temp-var)))
|#
;;; SYNOPSIS
(defmacro edit-file (file var &body body)
;;; ****
  `(let* ((,var (read-file-as-string ,file)))
     (setf ,@(loop for i in `,body collect `,var collect i))
     (with-open-file 
              (out ,file :direction :output :if-exists :rename-and-delete)
            (princ ,var out))
     (format t "~&succesfully edited ~a" ,file)
     ,var))

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
           (agnostic-directory-pathname path)
           (pathname-name path))
   (pathname-type path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Jul  1 18:16:53 2014 
(defun parent-dir (path)
  (subseq path 0 (position #\/ path :from-end t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skip is a list of directories to not include--it's the last directory
;;; name only we're interested in. pattern is a string that the file name must
;;; have, or a list of such strings
(defun get-all-files (dir &optional skip pattern force-quotes)
  (setq skip (force-list skip))
  (let ((files (loop for file in (directory (starify dir t))
                  for files = (namestring file)
                  for isdir = (is-directory files)
                  if (and isdir
                          (not (member (first (last (pathname-directory files)))
                                       skip :test #'string=)))
                  append (get-all-files files)
                  else if (not isdir) collect files)))
    ;; MDE Wed Dec 22 13:07:13 2021, Heidhausen
    (loop for pattn in (force-list pattern) do
      (setq files (remove-if-not #'(lambda (x) (search pattn x)) files)))
    ;; MDE Tue Sep 27 12:21:36 2022, Heidhausen -- for files with spaces etc. we
    ;; might want to put "" marks around the string
    (when force-quotes
      (setq files (mapcar #'(lambda (f) (format nil "\"~a\"" f)) files)))
    ;; MDE Mon Feb 28 15:12:25 2022, Heidhausen -- there's no way we could want
    ;; .DS_Store ... famous last words?
    (remove-if #'(lambda (f) (search ".DS_Store" f)) files)))

(defun is-directory (path)
  (append (directory (starify path)) (directory (starify path t))))

(defun starify (path &optional (extension))
  (concatenate 'string (trailing-slash path) (if extension "*.*" "*")))

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
;;; MDE Tue May 28 20:32:36 2013 -- added start-freq-is-partial keyword arg

;;; ****f* utilities/get-harmonics
;;; DESCRIPTION
;;; Return a list of the harmonic partial frequencies in Hertz from a
;;; specified (usually fundamental) frequency.
;;; 
;;; ARGUMENTS
;;; - A number that is the fundamental or starting frequency in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :start-partial. An integer that is the number of the first harmonic
;;;    partial to return. Default = 1.
;;; - :min-freq. A number that is the lowest frequency in Hertz to
;;;   return. Default = 20.
;;; - :max-freq. A number that is the highest frequency in Hertz to
;;;   return. Default = 20000.
;;; - :start-freq-is-partial.  Rather than treating the first argument as the
;;;   fundamental, treat it as the partial number indicated by this argument.
;;;   Default = 1.
;;; - :max-results.  The maximum number of harmonics to return.  Default =
;;;    most-positive-fixnum  
;;; - :skip. The increment for the harmonics.  If 1, then we ascend the
;;;    harmonics series one partial at a time; 2 would mean skipping every other
;;;    Default = 1.
;;; - :pitches. Return a list of pitch objects instead of frequencies. Default =
;;;   NIL. 
;;; - :notes. Return a list of 2-element sublists: note symbols in the
;;;   chromatics scale, with cent deviations 
;;; 
;;; RETURN VALUE
;;; A list of numbers that are the frequencies in Hertz of harmonic partials
;;; above the same fundamental frequency, or with the respective keyword, as
;;; pitch objects or note symbols 
;;; 
;;; EXAMPLE
#|
;;; Get the first 15 harmonic partials above a fundamental pitch of 64 Hertz,
;;; starting with partial 2, and specifying an upper cut-off of 1010 Hz.

(get-harmonics 63 :start-partial 2 :max-freq 1010)

=> (126 189 252 315 378 441 504 567 630 693 756 819 882 945 1008)

|#
;;; SYNOPSIS
(defun get-harmonics (start-freq &key (start-partial 1) (min-freq 20)
                                   (start-freq-is-partial 1) (max-freq 20000)
                                   (skip 1)
                                   pitches notes
                                   (max-results most-positive-fixnum))
;;; ****
  (unless (and (integer>0 start-partial)
               (integer>0 start-freq-is-partial))
    (error "utilities::get-harmonics: :start-partial (~a) and/or ~
            :start-freq-is-partial (~a) ~%need to be integers >= 1"
           start-partial start-freq-is-partial))
  (when (and pitches notes)
    (error "utilities::get-harmonics: either :pitches or :notes but not both."))
  (let ((result
         (loop with fundamental = (float (/ start-freq start-freq-is-partial))
            for h from start-partial by skip
            for freq = (* fundamental h)
            while (<= freq max-freq)
            repeat max-results
            if (>= freq min-freq)
            collect freq))
        (scale cm::*scale*))
    (cond (pitches
           (mapcar #'(lambda (f) (make-pitch f)) result))
          (notes
           (prog2
               ;; only get pitches and their cent devitions in the
               ;; chromatic scale
               (in-scale :chromatic)
               (mapcar #'(lambda (f)
                           (let ((p (make-pitch f)))
                             (list (data p)
                                   (floor (pitch-bend p) 0.01))))
                       result)
             (in-scale scale)))
          (t result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keywords same as get-harmonics (above)
(defun get-pitch-list-harmonics (pitch-list &rest keywords)
  (setf pitch-list (init-pitch-list pitch-list)) ; in case they're symbols
  (let ((harms (loop for pitch in pitch-list appending
                    (apply #'get-harmonics (cons (frequency pitch) keywords)))))
    (sort
     (remove-duplicates (init-pitch-list harms) :test #'pitch=)
     #'pitch<)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/db2amp 
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
       (setq list (remove rm list :test test)))
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
  (let ((pos (position (rm-package dynamic)
                       '(niente pppp ppp pp p mp mf f ff fff ffff))))
    (if pos
        (/ pos 10.0)
        (when warn
          (warn "utilities::dynamic-to-amplitude: unrecognised dynamics: ~a"
                dynamic)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/degree-to-radian
;;;
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;; 
;;; DESCRIPTION
;;; Convert an angle in degrees to its equivalent in radians
;;; 
;;; ARGUMENTS
;;; - The number in degrees
;;; 
;;; RETURN VALUE
;;; The number in radians
;;; 
;;; EXAMPLE
#|
(degree-to-radian 180)

=> 3.141592653589793d0

|#
;;; SYNOPSIS
(defun degree-to-radian (degree)
;;; ****
  (rationalize (* pi (/ degree 180))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/radian-to-degree
;;;
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;; 
;;; DESCRIPTION
;;; Convert an angle in radians to its equivalent in degrees
;;; 
;;; ARGUMENTS
;;; - The number in radians
;;; 
;;; RETURN VALUE
;;; The number in degrees
;;; 
;;; EXAMPLE
#|
(radian-to-degree pi)

=> 180

|#
;;; SYNOPSIS
(defun radian-to-degree (radian)
;;; ****
  (rationalize (* 180 (/ radian pi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/polar-to-cartesian
;;;
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;; 
;;; DESCRIPTION
;;; Convert a point in a 3D coordinate space from the polar system to cartesian
;;; coordinates. This differs from the normal definition of this conversion, in
;;; that the elevation is the angle from true horizontal, not vertical...
;;; 
;;; ARGUMENTS
;;; - The horizonal angle from the Y axis (alpha, azimuth angle) in degree.
;;; - The vertical angle from the X axis (polar, elevation) between +-180°
;;; - The distance from the origin (the radius), 0 <= distance <= 1
;;; 
;;; RETURN VALUE
;;; A list that holds the x y and z coordinates for the point.
;;; 
;;; EXAMPLE
#|
(polar-to-cartesian 0 45 1)

=> (0.0 0.70710677 0.70710677)

|#
;;; SYNOPSIS
(defun polar-to-cartesian (angle elevation distance)
;;; ****
  (unless (<= -180 elevation 180) (error "elevation ~a out of bounds" elevation))
  (unless (>= distance 0) (error "distance ~a out of bounds" distance))
  (let* ((sina (sin (degree-to-radian angle)))
         (sine (sin (degree-to-radian elevation)))
         (cosa (cos (degree-to-radian angle)))
         (cose (cos (degree-to-radian elevation)))
         (x (* distance sina cose))
         (y (* distance cosa cose))
         (z (* distance sine)))
    `(,x ,y ,z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/cartesian-to-polar
;;;
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;; 
;;; DESCRIPTION
;;; Convert a point in a 3D coordinate space from the cartesian system to polar
;;; coordinates. This differs from the normal definition of this conversion, in
;;; that the elevation is the angle from true horizontal, not vertical...
;;; 
;;; ARGUMENTS
;;; - The x-coordinate
;;; - The y-coordinate
;;; - The z-coordinate
;;; 
;;; RETURN VALUE
;;; A list that holds the angle (azimuth), elevation and distance of the point.
;;; 
;;; EXAMPLE
#|
(cartesian-to-polar 0 0 1)

=> (0 90 1)

|#
;;; SYNOPSIS
(defun cartesian-to-polar (x y z)
;;; ****
  (unless (<= -1 x 1) (error "x ~a out of bounds" x))
  (unless (<= -1 y 1) (error "y ~a out of bounds" y))
  (unless (<= -1 z 1) (error "z ~a out of bounds" z))
  (let* ((distance
          (/ (round (* (sqrt (+ (expt x 2) (expt y 2) (expt z 2))) 1000)) 1000))
         (elevation
          (/ (round (* (radian-to-degree (asin (/ z distance)))  1000)) 1000))
         (angle (/ (round (* (radian-to-degree (atan y x)) 1000)) 1000)))
    `(,angle ,elevation ,distance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/convert-polar-envelopes
;;;
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;; 
;;; DESCRIPTION
;;; Convert a set of an angle-env and elevation-env into an envelope for the
;;; x, y and z coordinates. Distance is assumed to be 1, if no additional
;;; distance-env is given. The x axis represents left (-1) and right (+1).
;;; The y axis is front (+1) to back (-1), z goes up (+1) to head-level (0).
;;; 
;;; ARGUMENTS
;;; - An angle-env
;;; - An elevation-env
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :distance-env. A distance-env...
;;; :minimum-samples. A number - minimal amount of points between first and
;;; last point of the envelopes at which to convert. If nil, only the original
;;; points of the envelopes are used, this however doesn't always fully
;;; represent the envelopes... Going from 0° to 180° is something else than
;;; going from y = 1 to y = -1.
;;; 
;;; RETURN VALUE
;;; A list that holds the three envelopes for x, y and z
;;; 
;;; EXAMPLE
#|
(convert-polar-envelopes '(0 0  1 180) '(0 30  .5 0  1 45) :minimum-samples 5)

=> (0.0 0.0 25 0.68301266 50.0 1.0 75 0.65328145 100.0 8.6595606e-17)
=> (0.0 0.8660254 25 0.68301266 50.0 6.123234e-17 75 -0.65328145 100.0 -0.70710677)
=> (0.0 0.5 25 0.25881904 50.0 0.0 75 0.38268343 100.0 0.70710677)

|#
;;; SYNOPSIS
(defun convert-polar-envelopes (angle-env elevation-env
                                &key (distance-env '(0 1 1 1))
                                  minimum-samples)
;;; ****
  ;; stretch the envelopes so they align:
  (setf angle-env (scale-env angle-env 1 :first-x 0 :last-x 100)
        elevation-env (scale-env elevation-env 1 :first-x 0 :last-x 100)
        distance-env (scale-env distance-env 1 :first-x 0 :last-x 100))
  (let* ((angle-all-x (loop for x in angle-env by #'cddr collect x))
         (elevation-all-x (loop for x in elevation-env by #'cddr collect x))
         (distance-all-x (loop for x in distance-env by #'cddr collect x))
         (all-x angle-all-x))
    (loop for i in elevation-all-x
       unless (member i all-x :test #'=) do (push i all-x))
    (loop for i in distance-all-x
       unless (member i all-x :test #'=) do (push i all-x))
    (when minimum-samples
      (loop for i from 0 to 100 by (/ 100 (1- minimum-samples))
         unless (member i all-x :test #'=) do (push i all-x)))
    (setf all-x (sort all-x #'<))
    (loop for i in all-x
       for new = (polar-to-cartesian
                  (interpolate i angle-env)
                  (interpolate i elevation-env)
                  (interpolate i distance-env))
       collect i into x-env collect (first new) into x-env
       collect i into y-env collect (second new) into y-env
       collect i into z-env collect (third new) into z-env
       finally (return (values x-env y-env z-env)))))
          
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
  (loop for el in what 
     for m = (first (member el from :test test))
       do
       ;; (print el)
     (when m
       (setf from (remove m from :test #'equalp))
       (push m to)))
  ;; (print from)
  ;; (print to)
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
;;; NB: If the element exists more than once in the given list, all but one of
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
;;; MDE Mon Jan 18 11:46:00 2016 
(defun move-all-to-end (what list &optional (test #'eql))
  (loop for el in what do (setf list (move-to-end el list test)))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun force-list (thing)
  ;; MDE Thu Oct 1 16:47:36 2015 -- added (when thing ...) so that we don't
  ;; force NIL into a list
  (when thing
    (if (listp thing)
        thing
        (list thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/hailstone
;;; DESCRIPTION
;;; Implementation of the Collatz conjecture (see
;;; http://en.wikipedia.org/wiki/Collatz_conjecture)
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
  (let ((result (loop collect n while (> n 1) 
                   do (setf n (if (oddp n)
                                  (1+ (* 3 n))
                                  (/ n 2))))))
    (values result (apply #'+ result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Dec 15 15:57:32 2011

;;; SAR Thu Dec 15 11:45:12 GMT 2011: added carriage returns to formatted text

(defun load-from-examples-dir (file)
  (declare (special cl-user::+slippery-chicken-home-dir+))
  (format t "~%~%*******  Loading ~a~%" file)
  (load 
     (format nil "~a/doc/examples/~a"
             cl-user::+slippery-chicken-home-dir+ file)))

(defun load-from-test-suite-dir (file)
  (declare (special cl-user::+slippery-chicken-home-dir+))
  (format t "~%~%*******  Loading ~a~%" file)
  (load 
     (format nil "~atests/~a" cl-user::+slippery-chicken-home-dir+ file)))

;;; e.g. (FILE-FROM-SC-DIR "test-suite/blah.lsp") 
;;; -> "/Users/medward2/lisp/sc/test-suite/blah.lsp"
(defun file-from-sc-dir (file)
  (declare (special cl-user::+slippery-chicken-home-dir+))
  (concatenate 'string cl-user::+slippery-chicken-home-dir+ file))

(defun run-tests (&optional full)
  (load-from-test-suite-dir (if full "sc-test-full.lsp" "sc-test-suite.lsp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri May  4 11:01:14 2012 
(defun safe-subseq (seq start &optional end)
  (let* ((len (length seq)))
    (when (and (integerp end) (> end len))
      (setf end len))
    (when (and (integerp start) (< start len))
      (subseq seq start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl
(defun shell-to-string (command &rest arguments)
  (let ((stream (make-string-output-stream)))
    (cl-user::run-program command arguments :output stream
                                            :wait t :input nil)
     (get-output-stream-string stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Wed May 29 14:40:43 2013 -- 
(defun shell (command &rest arguments)
  ;; (print command) (print arguments)
  #+sbcl
  (cl-user::process-exit-code
   (cl-user::run-program command arguments :output *standard-output*
                         :wait t :input nil))
  #+ccl
  (nth-value 1
             (cl-user::external-process-status
              (cl-user::run-program command arguments :output t)))
  #-(or sbcl ccl)
  (warning "utilities::shell: Can't execute ~a on your system. Sorry."
           command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed May 29 14:54:14 2013 
(defun system-open-file (file)
  ;; MDE Thu Nov  3 15:37:40 2022, Heidhausen
  (when (get-sc-config 'system-open-file-escape-spaces)
    (setq file (escape-spaces file)))
  #-(or darwin linux)
  (warning "utilities::system-open-file: Can't open ~a on your system. Sorry."
           file)
  #+(or darwin linux)
  ;; MDE Wed Jun 30 16:08:47 2021, Heidhausen -- Following conversion code
  ;; courtesy of Leon Focker 
  ;; automatically converts a .eps file to a .pdf file using the epstopdf
  ;; command
  (when (and (equal (pathname-type file) "eps")
             (get-sc-config 'autoconvert-eps-to-pdf))
    (format t "~&Converting to pdf....")
    #+linux
    (shell "/usr/bin/epstopdf" file)
    #+darwin
    (let* ((ps2pdf "/usr/local/bin/ps2pdf")
           (epstopdf "/Library/TeX/texbin/epstopdf")
           (pdf (concatenate 'string (path-minus-extension file) ".pdf")))
      (cond ((probe-file ps2pdf) (shell ps2pdf file pdf))
            ((probe-file epstopdf) (shell epstopdf file "-o" pdf))
            (t (warn "~&utilities::system-open-file: Can't convert to pdf,
                      neither ~a nor ~a found" ps2pdf epstopdf)))
      (setq file pdf)))
  #+darwin (shell "/usr/bin/open" file)
  #+linux
  (let ((xdg "/usr/bin/xdg-open"))
    (if (probe-file xdg)
        (shell xdg file)
        (warn "utilities::system-open-file: Can't open without ~a" xdg))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jun  1 11:21:10 2013 -- get a file name from a piece title by
;;; replacing spaces with hyphens etc. 
(defun filename-from-title (title)
  (string-downcase
   (remove #\, (remove #\: (remove #\' (substitute #\- #\  title))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-name-from-title (title)
  (read-from-string (format nil "+~a+" (filename-from-title title))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/update-app-src
;;; DATE
;;; June 1st 2013
;;;
;;; DESCRIPTION
;;; TEMPORARILY DISABLED DUE TO SVN SERVER ACCESS RESTRICTIONS.
;;; NB This function currently works in SBCL and CCL on UNIX systems only.
;;; 
;;; For users of the slippery chicken app, this function will update the source
;;; code of the app to the latest in the online subversion (svn) repository.
;;; An internet connection is therefore necessary.  
;;;
;;; The first time it is run it will delete the current source code and
;;; download all the new source code, so make sure to back up if you've
;;; modified the source code yourself (not recommended).  When it is run from
;;; then on, it will only update the source code that is out of date.
;;;
;;; Once the source code is updated, you'll need to restart the app or just
;;; Lisp for the changes to be recompiled. 
;;; 
;;; **NB** The first time you call this function, you might get a "certificate
;;; error".  In order to accept the certificate, start the terminal application
;;; and type the following:
;;; 
;;; cd /tmp/
;;; svn co https://svn.ecdf.ed.ac.uk/repo/user/medward2/sc-tags/sc-latest/src
;;; 
;;; That should give you a prompt in the terminal from which you can accept the
;;; certificate.  Then the next time you try it from Lisp the certificate
;;; should not cause a problem.
;;; 
;;; Users without the app can always download the latest source code in a
;;; terminal by issuing the following command.
;;; svn co https://svn.ecdf.ed.ac.uk/repo/user/medward2/sc-tags/sc-latest/src
;;; 
;;; ARGUMENTS
;;; The full path to the slippery-chicken application, minus the last slash.
;;; Remember that this can't include any spaces in file/folder names 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :rm.  The path to the shell 'rm' command.  Default = "/bin/rm"
;;; - :svn.  The path to the shell 'svn' command.  Default = "/usr/bin/svn"
;;; 
;;; RETURN VALUE
;;; The shell return value of the call to SVN, usually 0 on success.
;;; 
;;; EXAMPLE
#|
Running for the first time:
(update-app-src "/tmp/sc-app/slippery-chicken.app")
A    /tmp/sc-app/slippery-chicken.app/Contents/Resources/sc/src/sndfile.lsp
A    /tmp/sc-app/slippery-chicken.app/Contents/Resources/sc/src/osc.lsp
A    /tmp/sc-app/slippery-chicken.app/Contents/Resources/sc/src/osc-sc.lsp
[...]
Checked out revision 3608.
0

or after successfully updating a previously updated version:
...
At revision 3608.
0
|#
;;; SYNOPSIS
(defun update-app-src (path-to-app &key (rm "/bin/rm") (svn "/usr/bin/svn"))
;;; ****
  #+(and (or ccl sbcl) unix)
  (let* ((sc (concatenate 'string path-to-app "/Contents/Resources/sc/"))
         (src (concatenate 'string sc "src/"))
         (svn-command
          (if (probe-file (concatenate 'string src ".svn/entries"))
              ;; we've already used svn
              (list "update" src)
              ;; need to check out for the first time
              (progn
                (shell rm "-r" "-f" src)
                (list 
                 "co" 
                 (concatenate 'string
                              "https://svn.ecdf.ed.ac.uk/repo/user/medward2/"
                              "sc-tags/sc-latest/src")
                 src)))))
    (prog1
        (apply #'shell (cons svn svn-command))
      (format t "Please restart slippery chicken for changes to take effect.")))
  #-(and (or ccl sbcl) unix)
  (warn "utilities::update-app-src: Sorry but this currently only runs ~
         with SBCL or CCL on Mac OSX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (get-date-string) --> "2017-04-01, 13:35"
;;; (get-date-string nil) --> "2017-04-01"
(defun get-date-string (&optional (time t))
  (multiple-value-bind
        (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec))
    (if time
        ;; (format nil "~d-~2,'0d-~2,'0d, ~2,'0d:~2,'0d" year month day hour
        ;; min)
        (format nil "~d-~2,'0d-~2,'0d-~2,'0d.~2,'0d" year month day hour min)
        (format nil "~d-~2,'0d-~2,'0d" year month day))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(and mac-osx X86-64)
(defun kontakt-to-coll (nki &key write-file
                              (converter
                               (concatenate 'string
                                            cl-user::+slippery-chicken-home-dir+
                                            "bin/nki")))
  (flet ((get-value (line &optional (read t))
           (let* ((pos (search "value=" line))
                  (val (subseq line (+ pos 7))))
             (unless pos
               (error "utilities::kontakt-to-coll: can't find 'value' in ~%~a"
                      line))
             (if read
                 (read-from-string val)
                 val))))
    (let ((xml (if (search ".xml" nki)
                   nki
                   (progn
                     (shell converter nki)
                     (concatenate 'string nki ".xml"))))
          (result '())
          (txt (concatenate 'string nki ".txt")))
      (with-open-file 
          (input xml :direction :input :if-does-not-exist :error)
        (loop
           with key with sample
           do
             (multiple-value-bind
                   (line eof)
                 (read-line input nil)
               (cond 
                 ((search "rootKey" line)
                  (setf key (get-value line)))
                 ((search "file_ex2" line)
                  ;; files are the paths with some strange directory delimiter
                  ;; but they all seem to start the actual file name with a
                  ;; 12-char string like F-00010 or F000--this will of course
                  ;; break if the sample file names begin with either of these
                  ;; strings
                  (let* ((val (get-value line nil))
                         (pos1 (or (search "F-00010" val)
                                   (search "F000" val)))
                         (pos2 (search "\"/>" val)))
                    (unless (and pos1 pos2)
                      (error "utilities::kontakt-to-coll: can't find file ~
                              in ~%~a" line))
                    (setf sample (subseq val (+ pos1 12) pos2))
                    (push (list key sample) result))))
               (when eof 
                 (return)))))
      (setf result (sort (nreverse result)
                         #'(lambda (x y) (< (first x) (first y)))))
      (when write-file
        (with-open-file 
            (output txt :direction :output :if-exists :overwrite
                    :if-does-not-exist :create)
          ;; if the key skips a few write the previous file in the gaps, with
          ;; semitone offsets
          (flet ((write-coll-line (index sample num-times)
                   ;; remember a line looks something like
                   ;; 20, Baritone_Marimba62 .wav 0;
                   ;; with the extension separately
                   (loop for i below num-times
                      with name = (pathname-name sample)
                      with type = (concatenate 'string "."
                                               (pathname-type sample))
                      do
                        (format output "~&~a, ~a ~a ~a;"
                                (+ i index) name type i))))
            (loop with offset = (first (first result))
               with last-key with last-sample
               for pair in result
               for key = (first pair)
               for sample = (second pair)
               do
                 (when last-key
                   (write-coll-line (- last-key offset) last-sample
                                    (- key last-key)))
                 (setf last-key key
                       last-sample sample)
               finally 
                 (write-coll-line (- key offset) sample 1)))))
      (values result (length result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/pdivide
;;; DESCRIPTION
;;; Creates a list of proportionally related times, dividing a starting
;;; duration into a number of smaller durations a specified number of times.
;;; We start with a proportion as a ratio (e.g. 3/2) and divide the given
;;; duration into two parts according to that ratio.  Then those two parts will
;;; be divided into the same ratios.  This will iterate the number of times
;;; indicated by the second argument.
;;;
;;; The following are some classical proportions:
;;;         Latin        (Greek)
;;; (3 : 2) Sesquialtera (Diapente)
;;; (4 : 3) Sesquitertia (Diatessaron)
;;; (5 : 4) Sesquiquarta (Diatonus Semitonus)
;;; (8 : 3) Duplasuperbipartiens (Diapson Diatesseron)
;;; (9 : 8) Sesquioctava (Tonus)
;;; 
;;; ARGUMENTS
;;; - an integer or ratio (in Lisp terms, a rational) e.g. 3/2
;;; - an integer >=1 specifying the number of times to iterate the process of
;;;   dividing the duration into proportions.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :duration. The overall duration to apply the proportional divisions to.
;;;   Units are arbitrary of course as this is just a number. Default 1.0.
;;; - :print. If T, print each level of division as we proceed. Default NIL.
;;; - :reverse. If T reverse the proportion (so 3/2 becomes 2/3). Default NIL.
;;; - :alternate. If T, reverse the proportion every other division (not
;;;   iteration) so that if we have a proportion of 3/2 on the second iteration
;;;   we divide into 3/2 then 2/3.  Default NIL.
;;; - :increment. If T, then each time we divide we increment both sides of the
;;;   proportion,  so 3:2 becomes 4:3 which becomes 5:4 etc.  Default NIL.
;;; - :halves. This will only make a difference if :increment is T: As results
;;;   tend overall towards increasing (when numerator < denominator e.g. 2/3) or
;;;   decreasing (numerator > denominator e.g. 3/2) numbers, we can mix things
;;;   up by dividing the resultant list into two halves and splicing their
;;;   elements one after the other.  Default NIL.
;;; - :shuffle. Mix things up by shuffling the resultant list.  As this uses
;;;   the shuffle algorithm we have fixed-seed randomness so results will be
;;;   the same upon each call within the same Lisp implementation/version.
;;;   Default NIL. 
;;; 
;;; RETURN VALUE
;;; Three values: the list of ascending timings from the last generation of the
;;; calculated proportions; the durations of each part for the last generation;
;;; the list of ascending timings for _each_ generation of the calculated
;;; proportions (a list of lists).
;;; 
;;; EXAMPLE
#|

Notice here that each generation prints the proportions along with the
durations these correspond to and the start time of each (cumulative durations).

(pdivide 3/2 4 :duration 35 :print t)

PRINTS:
Generation 1: 3 (21.00=21.00), 2 (14.00=35.00), 

Generation 2: 3 (12.60=12.60), 2 (8.40=21.00), 3 (8.40=29.40), 2 (5.60=35.00), 

Generation 3: 3 (7.56=7.56), 2 (5.04=12.60), 3 (5.04=17.64), 2 (3.36=21.00), 
3 (5.04=26.04), 2 (3.36=29.40), 3 (3.36=32.76), 2 (2.24=35.00), 

Generation 4: 3 (4.54=4.54), 2 (3.02=7.56), 3 (3.02=10.58), 2 (2.02=12.60), 
3 (3.02=15.62), 2 (2.02=17.64), 3 (2.02=19.66), 2 (1.34=21.00), 3 (3.02=24.02),
2 (2.02=26.04), 3 (2.02=28.06), 2 (1.34=29.40), 3 (2.02=31.42), 2 (1.34=32.76),
3 (1.34=34.10), 2 (0.90=35.00), 

RETURNS: 
(0.0 4.5360003 7.5600004 10.584001 12.6 15.624001 17.640001 19.656002 21.000002
 24.024002 26.040003 28.056004 29.400003 31.416004 32.760006 34.104008
 35.000008)
(4.5360003 3.0240002 3.0240004 2.0160003 3.0240004 2.0160003 2.0160003
 1.3440002 3.0240004 2.0160003 2.0160003 1.3440002 2.0160003 1.3440001
 1.3440001 0.896)
((0.0 4.5360003 7.5600004 10.584001 12.6 15.624001 17.640001 19.656002
  21.000002 24.024002 26.040003 28.056004 29.400003 31.416004 32.760006
  34.104008 35.000008)
 (0.0 7.5600004 12.6 17.640001 21.000002 26.040003 29.400003 32.760002
  35.000004)
 (0.0 12.6 21.0 29.400002 35.0) (0.0 21.0 35.0))


(pdivide 3/2 4 :duration 35 :print t :increment t :halves t)

PRINTS:
Generation 1: 3 (21.00=21.00), 2 (14.00=35.00), 

Generation 2: 4 (12.00=12.00), 3 (9.00=21.00), 5 (7.78=28.78), 4 (6.22=35.00), 

Generation 3: 6 (6.55=6.55), 5 (5.45=12.00), 7 (4.85=16.85), 6 (4.15=21.00), 
8 (4.15=25.15), 7 (3.63=28.78), 9 (3.29=32.07), 8 (2.93=35.00), 

Generation 4: 10 (3.44=3.44), 9 (3.10=6.55), 11 (2.86=9.40), 10 (2.60=12.00), 
12 (2.53=14.53), 11 (2.32=16.85), 13 (2.16=19.01), 12 (1.99=21.00), 
14 (2.15=23.15), 13 (2.00=25.15), 15 (1.88=27.03), 14 (1.75=28.78), 
16 (1.70=30.48), 15 (1.59=32.07), 17 (1.51=33.58), 16 (1.42=35.00), 

RETURNS:
(0.0 3.4449766 5.595868 8.696347 10.6936035 13.550747 15.428142 18.025545
 19.77778 22.30621 24.0064 26.324125 27.918053 30.078053 31.58647 33.580315
 35.0)
(3.4449766 2.1508918 3.100479 1.9972568 2.8571434 1.8773947 2.5974028 1.752235
 2.5284283 1.7001898 2.317726 1.593928 2.16 1.5084175 1.9938462 1.4196872)
((0.0 3.4449766 6.5454555 9.402599 12.000002 14.52843 16.846155 19.006155
  21.000002 23.150894 25.148151 27.025547 28.777782 30.477972 32.0719 33.58032
  35.000004)
 (0.0 6.5454555 12.000002 16.846157 21.000004 25.148151 28.77778 32.0719
  35.000004)
 (0.0 12.000001 21.0 28.777779 35.0) (0.0 21.0 35.0))

|#
;;; SYNOPSIS
(defun pdivide (start levels &key (duration 1.0) print reverse alternate
                    halves shuffle increment)
;;; ****
  (setf duration (float duration))
  (let ((result '())
        (resultd '())
        (resultd-first '())
        (num (numerator start))
        (den (denominator start))
        this thisd)
    (unless (rationalp start)
      (error "utilities::pdivide: start (~a) should be a rational number ~
             (integer or ratio e.g. 3/2)" start))
    (unless (and (integerp levels) (>= levels 1))
      (error "utilities::pdivide: levels (~a) should be an integer >= 1."
             levels))
    (loop with n = num with d = den for i from 0 repeat levels do
         (setf this (loop with l for i below (expt 2 i)
                       do
                       (setf l (list n d))
                       (when reverse
                         (setf l (reverse l)))
                       (when (and alternate (oddp i))
                         (setf l (reverse l)))
                       collect l
                       do (when increment (incf d) (incf n)))
               thisd (loop for ps in this
                        for dur in (if resultd (first resultd) (list duration))
                        for sum = (+ (first ps) (second ps))
                        collect (* dur (/ (first ps) sum))
                        collect (* dur (/ (second ps) sum))))
         (push this result)
         (push thisd resultd)
         (when print 
           (terpri) 
           (terpri)
           (format t "Generation ~a: " (1+ i))
           (loop with time = 0.0 for p in (flatten this) for d in thisd do
                (format t "~d (~,2f=~,2f), " p d (incf time d)))))
    (setf resultd-first (first resultd))
    (when halves
      (let ((half (expt 2 (1- levels))))
        (setf resultd-first (loop for i in resultd-first
                               for j in (nthcdr half resultd-first)
                               collect i collect j))))
    (when shuffle
      (setf resultd-first (shuffle resultd-first)))
    (flet ((cumulative (list)
             (cons 0.0 (loop with time = 0.0 for d in list
                          collect (incf time d)))))
      (values 
       (cumulative resultd-first)
       resultd-first
       (loop for l in resultd collect (cumulative l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/pexpand
;;; DESCRIPTION
;;; Instead of dividing an overall duration (pdivide) we start with a
;;; proportion and expand outwards from there, keeping each newly created part
;;; in the same proportion.  This is repeated the number of times specified in
;;; the first argument.  Useful for generating maps (section structure).
;;; 
;;; ARGUMENTS
;;; The number of times to expand proportionally.
;;; 
;;; OPTIONAL ARGUMENTS
;;; As many integer proportions as required.  If the last argument here is t,
;;; then instead of using letters to denote sections we use numbers instead. 
;;; 
;;; RETURN VALUE
;;; 3 values: 
;;; 1) a list showing the cumulative count (e.g. bar numbers) of where major
;;; and minor sections occur.  Topmost sections will have the labels A, B, C,
;;; etc. with subsections such as A.A, A.B, ... C.C.C.C.  Of course, wherever a
;;; major section starts, an arbitrary number of subsections also begin, but
;;; only the most major section is present in the list.
;;; 2) the structure of the sections and subsections in the form of a list of
;;; sublists for each, and containing the section labels paired with their
;;; length.  The bottommost subsection will have a length of the sum of the
;;; proportions, with higher subsection groupings showing multiples of this.
;;; 3) the overall length of the structure produced (also the first element of
;;; the second returned value).
;;; 
;;; EXAMPLE
#|
;;; 2 generations:
(pexpand 2 3 2) =>
(1 (A) 6 (A A A B) 11 (A A A C) 16 (A A B) 21 (A A B B) 26 (A B) 31 (A B A B)
 36 (A B A C) 41 (A B B) 46 (A B B B) 51 (A C) 56 (A C A B) 61 (A C A C) 66
 (A C B) 71 (A C B B) 76 (B) 81 (B A A B) 86 (B A A C) 91 (B A B) 96 (B A B B)
 101 (B B) 106 (B B A B) 111 (B B A C) 116 (B B B) 121 (B B B B))
(125
 (((A) 75)
  (((A A) 25) (((A A A) 15) ((A A A A) 5) ((A A A B) 5) ((A A A C) 5))
   (((A A B) 10) ((A A B A) 5) ((A A B B) 5)))
  (((A B) 25) (((A B A) 15) ((A B A A) 5) ((A B A B) 5) ((A B A C) 5))
   (((A B B) 10) ((A B B A) 5) ((A B B B) 5)))
  (((A C) 25) (((A C A) 15) ((A C A A) 5) ((A C A B) 5) ((A C A C) 5))
   (((A C B) 10) ((A C B A) 5) ((A C B B) 5))))
 (((B) 50)
  (((B A) 25) (((B A A) 15) ((B A A A) 5) ((B A A B) 5) ((B A A C) 5))
   (((B A B) 10) ((B A B A) 5) ((B A B B) 5)))
  (((B B) 25) (((B B A) 15) ((B B A A) 5) ((B B A B) 5) ((B B A C) 5))
   (((B B B) 10) ((B B B A) 5) ((B B B B) 5)))))
125

;;; 3 generations:
(pexpand 3 3 2) =>
(1 (A) 6 (A A A A A B) 11 (A A A A A C) 16 (A A A A B) 21 (A A A A B B) 26
 (A A A B) 31 (A A A B A B) 36 (A A A B A C) 41 (A A A B B) 46 (A A A B B B) 51
 (A A A C) 56 (A A A C A B) 61 (A A A C A C) 66 (A A A C B) 71 (A A A C B B) 76
...
 581 (B B B A A B) 586 (B B B A A C) 591 (B B B A B) 596 (B B B A B B) 601
 (B B B B) 606 (B B B B A B) 611 (B B B B A C) 616 (B B B B B) 621
 (B B B B B B))
(625
 (((A) 375)
  (((A A) 125)
   (((A A A) 75)
    (((A A A A) 25)
     (((A A A A A) 15) ((A A A A A A) 5) ((A A A A A B) 5) ((A A A A A C) 5))
     (((A A A A B) 10) ((A A A A B A) 5) ((A A A A B B) 5)))
...
   (((B B B) 50)
    (((B B B A) 25)
     (((B B B A A) 15) ((B B B A A A) 5) ((B B B A A B) 5) ((B B B A A C) 5))
     (((B B B A B) 10) ((B B B A B A) 5) ((B B B A B B) 5)))
    (((B B B B) 25)
     (((B B B B A) 15) ((B B B B A A) 5) ((B B B B A B) 5) ((B B B B A C) 5))
     (((B B B B B) 10) ((B B B B B A) 5) ((B B B B B B) 5)))))))
625

;;; 2 generations of 3 proportional values, returning numbers for labels
(pexpand 2 3 2 4 t) =>
(1 (1) 10 (1 1 1 2) 19 (1 1 1 3) 28 (1 1 2) 37 (1 1 2 2) 46 (1 1 3) 55
 (1 1 3 2) 64 (1 1 3 3) 73 (1 1 3 4) 82 (1 2) 91 (1 2 1 2) 100 (1 2 1 3) 109
 (1 2 2) 118 (1 2 2 2) 127 (1 2 3) 136 (1 2 3 2) 145 (1 2 3 3) 154 (1 2 3 4)
... (3 4 2 2) 694 (3 4 3) 703 (3 4 3 2) 712 (3 4 3 3) 721 (3 4 3 4))

(729
 (((1) 243)
  (((1 1) 81) (((1 1 1) 27) ((1 1 1 1) 9) ((1 1 1 2) 9) ((1 1 1 3) 9))
   (((1 1 2) 18) ((1 1 2 1) 9) ((1 1 2 2) 9))
   (((1 1 3) 36) ((1 1 3 1) 9) ((1 1 3 2) 9) ((1 1 3 3) 9) ((1 1 3 4) 9)))
...
   (((3 2 2) 18) ((3 2 2 1) 9) ((3 2 2 2) 9))
   (((3 2 3) 36) ((3 2 3 1) 9) ((3 2 3 2) 9) ((3 2 3 3) 9) ((3 2 3 4) 9)))
  (((3 3) 81) (((3 3 1) 27) ((3 3 1 1) 9) ((3 3 1 2) 9) ((3 3 1 3) 9))
   (((3 3 2) 18) ((3 3 2 1) 9) ((3 3 2 2) 9))
   (((3 3 3) 36) ((3 3 3 1) 9) ((3 3 3 2) 9) ((3 3 3 3) 9) ((3 3 3 4) 9)))
  (((3 4) 81) (((3 4 1) 27) ((3 4 1 1) 9) ((3 4 1 2) 9) ((3 4 1 3) 9))
   (((3 4 2) 18) ((3 4 2 1) 9) ((3 4 2 2) 9))
   (((3 4 3) 36) ((3 4 3 1) 9) ((3 4 3 2) 9) ((3 4 3 3) 9) ((3 4 3 4) 9)))))
729
|#
;;; SYNOPSIS
(defun pexpand (generations &rest proportions)
;;; ****
  ;; MDE Fri Oct 11 18:53:42 2013 
  (let ((numbers (eq t (first (last proportions)))))
    (when numbers
      (setf proportions (butlast proportions)))
    (unless (every #'integerp proportions)
      (error "utilities::pexpand: proportions must be integers."))
    (let* ((result proportions)
           (rehearsal-letters nil))
      (loop repeat generations do
           (setf result (loop for p in proportions collect
                             (ml result p))))
      (setf result (pexpand-aux result nil))
      (values
       (progn
         (setf rehearsal-letters
               (pexpand-count result (loop for n in proportions sum n)
                              (* generations 2)))
         (if numbers
             (pexpand-letters-to-numbers rehearsal-letters)
             rehearsal-letters))
       (progn
         (setf (first result) (second (first result)))
         (if numbers
             (pexpand-letters-to-numbers result)
             result))
       ;; the number we produce will be the sum of the proportions ^ (1+ 
       ;; generations)                    
       (first result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/pexpand-section-length
;;; DESCRIPTION
;;; Return the length (integer) of any arbitrary section in the data returned
;;; by pexpand. 
;;; 
;;; ARGUMENTS
;;; - The (rest of) the kind of list returned as the second value of a call to
;;;   pexpand.
;;; - The section ID we want the length of, either as a list or single symbol.
;;; 
;;; RETURN VALUE
;;; An integer or NIL if the section can't be found.
;;; 
;;; EXAMPLE
#|

(pexpand-section-length (rest (nth-value 1 (pexpand 2 3 6 4 5))) '(c a b))
=> 108

(pexpand-section-length (rest (nth-value 1 (pexpand 2 3 6 4 5))) 'c)
=> 1296

|#
;;; SYNOPSIS
(defun pexpand-section-length (pexpand-list section)
;;; ****
  (when pexpand-list
    (flet ((do-rest () (pexpand-section-length (rest pexpand-list) section)))
      (unless (listp section)
        (setf section (list section)))
      (let ((fpl (first pexpand-list))
            (fplr nil))
        (if (and (listp fpl)
                 (= 2 (length fpl))
                 (numberp (second fpl)))
            (if (equalp (first fpl) section)
                (second fpl)
                (do-rest))
            (if (setf fplr (pexpand-section-length fpl section))
                fplr
                (do-rest)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pexpand-letters-to-numbers (list)
  (loop for l in '(a b c d e f g h i j k l m) and i from 1 do
       (setf list (subst i l list)))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/pexpand-find
;;; DESCRIPTION
;;; Find the cumulative number of where a label occurs in a list returned by
;;; pexpand.
;;; 
;;; ARGUMENTS
;;; - the label we're looking for
;;; - a list of the type returned by pexpand (first returned value).
;;;
;;; OPTIONAL ARGUMENTS
;;; - a function to be called when the label cannot be found.  Default =
;;; #'error but could also be #'warn or NIL.
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; SYNOPSIS
(defun pexpand-find (label list &optional (on-error #'error))
;;; ****
  (when (symbolp label)
    (setf label (list label)))
  (let ((pos (position label list :test #'equal)))
    (if pos
        (nth (1- pos) list)
        (when on-error
          (funcall on-error 
                   "utilities::pexpand-find: ~a: no such label in list." 
                   label)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pexpand-aux (list id)
  (let* ((outer (flatten list))
         (sum (loop for el in outer sum el))
         (letters '(a b c d e f g h i j k l m)))
         ;; (letters (loop for i from 1 to 13 collect i)))
    (if (atom (first list))
        ;; these are the lowest-level sections
        (list id sum)
        (cons (list id sum)
              (loop for l in list and letter in letters collect
                   (pexpand-aux l (econs id letter)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pexpand-print (pexp)
  (loop for num in pexp by #'cddr
     for section in (rest pexp) by #'cddr
     for indent = (* 2 (1- (length section)))
     do
       (terpri)
       (loop repeat indent do (write-char #\ )) ; the space after \ is vital
       (format t "~a: ~a" num (list-to-string section "-"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pexpand-count (list unit-size inner-label-length &optional (start 1))
  (loop with result = '() with count = start with label with last
     for el in (almost-flatten list) do
     (cond ((and (not label) (first el) (symbolp (first el)))
            ;; so we cache the first we saw, not the last
            (setf label el))
           ;; we're looking for twice the generations
           ((and (numberp (first el)) (= (first el) unit-size)
                 (= inner-label-length (length last)))
            (push count result)
            (push label result)
            (incf count unit-size)
            (setf label nil)))
     (setf last el)
     finally
     (return (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/a-weighting
;;; DESCRIPTION
;;; Implementation of A-weighting loudness compensation.  Formula taken from
;;; http://en.wikipedia.org/wiki/A-weighting.  This doesn't take 1000Hz
;;; loudness into account, rather it implements the 40-phon Fletcher-Munson
;;; curve only.
;;; 
;;; ARGUMENTS
;;; The frequency in Hertz for which to find the loudness weighting.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword aguments:
;;; - :expt. A power (exponent) to raise the result to in order to
;;;    tame/exaggerate the curve (make the db weightings less/more
;;;    extreme). This only really makes sense if :linear t though will work
;;;    with db values also of course.  Values < 1 result in linear values
;;;    closer to 1 (less extreme).  Values > 1 are further from 1. Default = NIL
;;;    i.e. no exponential function.
;;; - :linear.  If T return amplitude values as linear scalers rather than
;;;    logarithmic decibel values.  NB If this is NIL then returned values are
;;;    likely to be negative (db) values.  Default = T.  
;;; - :invert.  As the weighting routine tries to tell us what relative
;;;    loudness we'll perceive given constant amplitudes, low and high
;;;    frequencies will return negative values as we perceive them Xdb less
;;;    than our most sensitive frequency area.  If :invert t, just flip this
;;;    negatives to positives so that if :linear T you get a scaler to make
;;;    lower/higher frequences equally loud as the most sensitive frequencies.
;;;
;;; RETURN VALUE
;;; The linear or db weighting value for the given frequency.
;;; 
;;; EXAMPLE
#|

;;; Decibels:
(a-weighting 50 :invert nil :linear nil) => -30.274979
(a-weighting 50 :invert t :linear nil) => 30.274979
;;; Linear amplitude scalers:
(a-weighting 50) => 32.639904
(a-weighting 50 :invert nil) => 0.030637344
;;; Exaggeration:
(a-weighting 50 :expt 1.1) => 46.251286
;;; Smoothing:
(a-weighting 50 :expt .5) => 5.7131343

;;; Looping through the MIDI note range by tritones returning decibel values:
(loop for midi from 0 to 127 by 6
     for freq = (midi-to-freq midi)
     collect (list (midi-to-note midi)
                   (a-weighting freq :linear nil :invert nil)))
=>
((C-1 -76.85258) (FS-1 -65.94491) (C0 -55.819363) (FS0 -46.71565)
 (C1 -38.714867) (FS1 -31.724197) (C2 -25.598646) (FS2 -20.247103)
 (C3 -15.622625) (FS3 -11.657975) (C4 -8.258142) (FS4 -5.358156)
 (C5 -2.9644737) (FS5 -1.1277018) (C6 0.13445985) (FS6 0.8842882) (C7 1.226917)
 (FS7 1.2351798) (C8 0.89729404) (FS8 0.09495151) (C9 -1.3861179)
 (FS9 -3.7814288))

;;; Similar but returning linear amplitude scalers:
(loop for midi from 0 to 127 by 6
     for freq = (midi-to-freq midi)
     collect (list (midi-to-note midi) (a-weighting freq)))
=>
((C-1 6960.316) (FS-1 1982.6475) (C0 617.9711) (FS0 216.6619) (C1 86.246864)
 (FS1 38.56647) (C2 19.051636) (FS2 10.288571) (C3 6.041312) (FS3 3.827355)
 (C4 2.5876594) (FS4 1.8531382) (C5 1.4067719) (FS5 1.1386365) (C6 0.9846389)
 (FS6 0.9032034) (C7 0.8682687) (FS7 0.86744314) (C8 0.9018521) (FS8 0.9891278)
 (C9 1.1730213) (FS9 1.5455086))

|#
;;; SYNOPSIS
(defun a-weighting (f &key expt (linear t) (invert t))
;;; ****
  (let* ((result (+ 2.0 (* 20.0 (log (a-weighting-aux f) 10)))))
    (when invert
      (setf result (- result)))
    (when linear
      (setf result (db2amp result)))
    (when expt
      (setf result (if (< result 0.0)
                   (expt (- result) expt)
                   (expt result expt))))
    result))

(defun a-weighting-aux (f)
  (let ((f2 (* f f))
        (c1 (* 12200.0 12200.0)))
    (/ (* c1 f2 f2)
       (* (+ (* 20.6 20.6) f2) (sqrt (* (+ f2 (* 107.7 107.7))
                                        (+ f2 (* 737.9 737.9))))
          (+ f2 c1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/env2gnuplot
;;; DATE
;;; 24th December 2013
;;;
;;; DESCRIPTION
;;; Write a data file of x,y envelope values for use with gnuplit.  Once called
;;; start gnuplot and issue commands such as:
;;; gnuplot> set terminal postscript default
;;; gnuplot> set output '/tmp/env.ps'
;;; gnuplot> plot '/tmp/env.txt' with lines.
;;; 
;;; ARGUMENTS
;;; - The envelope as the usual list of x y pairs
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The pathname of the data file to write.  Default = "/tmp/env.txt".
;;; 
;;; RETURN VALUE
;;; Always T
;;; 
;;; SYNOPSIS
(defun env2gnuplot (env &optional (file "/tmp/env.txt"))
;;; ****
  (with-open-file
      (stream file :direction :output :if-exists :supersede
              :if-does-not-exist :create)
    (loop for x in env by #'cddr and y in (rest env) by #'cddr do
         (format stream "~&~a ~a" x y)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/interleave
;;; DESCRIPTION
;;; Interleave the elements of an aribitrary number of lists. Should the lists
;;; not be of the same length, this function will only use up as many elements
;;; as in the shortest list.
;;; 
;;; ARGUMENTS
;;; As many lists as need to be interleaved.
;;; 
;;; RETURN VALUE
;;; A new list of interleaved elements.
;;; 
;;; EXAMPLE
#|
(INTERLEAVE '(1 2 3 4 5) '(a b c d) '(x y z))
--> (1 A X 2 B Y 3 C Z)

(INTERLEAVE '(1 2 3 4 5) '(a b c d e) '(v w x y z))
--> (1 A V 2 B W 3 C X 4 D Y 5 E Z)
|#
;;; SYNOPSIS
(defun interleave (&rest lists)
;;; ****
  (loop with result = '()
       with len = (length lists)
     ;; with rl = (reverse lists)
     repeat (loop for l in lists minimize (length l))
     do
     (loop for i below len do
          (push (pop (nth i lists)) result))
     finally (return (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/envelope-boundaries
;;; DESCRIPTION
;;; Find sharp changes in envelope values. These are defined as when a y value
;;; rises or falls over 30% (by default) of it's overall range within 5%
;;; (again, by default) of its overall x axis range.
;;; 
;;; ARGUMENTS
;;; The envelope (a list of x y pairs).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - jump-threshold: the minimum percentage change in y value that is deemed a
;;;   sharp change. 
;;; - steepness-min: the maximum percentage of the overall x axis that
;;;   constitutes a 'quick' change.
;;; 
;;; RETURN VALUE
;;; A list of x values at which boundaries are deemed to lie.
;;; 
;;; EXAMPLE
#|
(ENVELOPE-BOUNDARIES '(0 10 20 10 21 3 25 4 26 9 50 7 51 1 55 2 56 7 70 10
                     100 10))
--> (21 26 51 56)
|#
;;; SYNOPSIS
(defun envelope-boundaries (envelope &optional (jump-threshold 30)
                            (steepness-min 5))
;;; ****
  (loop 
     with last-x = (first envelope)     ; i.e. first x val first time around
     with last-y = (second envelope)    ; i.e. first y val first time around
     with ymin = (env-y-min envelope)
     with ymax = (env-y-max envelope)
     with xmin = last-x
     with xmax = (first (last (butlast envelope)))
     with ythresh = (* .01 jump-threshold (- ymax ymin))
     with xthresh = (* .01 steepness-min (- xmax xmin))
     for x in envelope by #'cddr and y in (rest envelope) by #'cddr 
     for ydiff = (abs (- last-y y))
     for xdiff = (- x last-x)
     when (and (<= xdiff xthresh) (>= ydiff ythresh)) collect x
     do (setf last-y y
              last-x x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/get-clusters
;;; DESCRIPTION
;;; Takes a list with (ascending) numbers and creates sublists of those numbers
;;; within <threshold> of each other. 
;;; 
;;; ARGUMENTS
;;; A list of (ascending) numbers. NB Though the numbers don't have to be in
;;; ascending order, the design application of the function makes most sense if
;;; they are.
;;; 
;;; OPTIONAL ARGUMENTS
;;; The maximum distance between two numbers in order for them to be considered
;;; as part of the same cluster.
;;; 
;;; RETURN VALUE
;;; A list with clusters in sublists.
;;; 
;;; EXAMPLE
#|
(get-clusters '(24 55 58 59 60 81 97 102 106 116 118 119 145 149 151 200 210
                211 214 217 226 233 235 236 237 238 239 383 411 415 419))
--> (24 (55 58 59 60) 81 (97 102 106) (116 118 119) (145 149 151) 200
        (210 211 214 217) 226 (233 235 236 237 238 239) 383 (411 415 419))

(get-clusters '(0 .1 .3 .7 1.5 1.55 2 4.3 6.3 6.4) 1)
--> ((0 0.1 0.3 0.7 1.5 1.55 2) 4.3 (6.3 6.4))

(get-clusters '(0 .1 .3 .7 1.5 1.55 2 4.3 6.3 6.4) 0.5)
--> ((0 0.1 0.3 0.7) (1.5 1.55 2) 4.3 (6.3 6.4))
|#
;;; SYNOPSIS
(defun get-clusters (list &optional (threshold 5))
;;; ****
  (unless (every #'numberp list)
    (error "utilities::mean: can't find the mean of non-numbers: ~a" list))
  (loop with last = (first list)
     with tmp = (list last)
     with result = '()
     for el in (rest list)
     do
     (if (<= (- el last) threshold) 
         (push el tmp)
         (progn (push (if (> (length tmp) 1) (reverse tmp) (first tmp))
                      result)
                (setf tmp (list el))))
     (setf last el)
     finally
     (when tmp (push (reverse tmp) result))
     (return (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-time ()
  "Returns a string similar to:
   21:51:24 on Thursday the 19th of January 1995"
  (flet ((day-name (day) 
           (nth day '("Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday"  
                      "Saturday" "Sunday")))
         (month-name (month)
           (nth (- month 1) '("January" "February" "March"
                              "April" "May" "June" "July"
                              "August" "September" "October"
                              "November" "December")))
         (suffix (date) (case date
                          (1 "st")
                          (21 "st")
                          (31 "st")
                          (2 "nd")
                          (22 "nd")
                          (3 "rd")
                          (23 "rd")
                          (t "th"))))
    (multiple-value-bind 
          (seconds minutes hours date month year day)
        (get-decoded-time)
      (format nil "~2,'0D:~2,'0D:~2,'0D on ~a the ~a~a of ~a ~a" hours 
              minutes seconds (day-name day) date (suffix date) 
              (month-name month) year))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Mar  7 14:54:30 2015 
(defun limit-high-low (value high low)
  (unless (and (numberp high) (numberp low) (numberp value))
    (error "utilities::limit-high-low: all three arguments must be numbers: ~
            ~a ~a ~a" value high low))
  (cond ((> value high) high)
        ((< value low) low)
        (t value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rationalize-if-simple (number &optional
                              (decimal-places 6) (max-fraction 24))
  (let* ((n (if decimal-places (decimal-places number decimal-places) number))
         (r (rationalize n)))
    (if (and (<= (denominator r) max-fraction) (<= (numerator r) max-fraction))
        r 
        ;; try a different method
        (loop with result = n
           for num from 1 to max-fraction do
             (loop for denom from 3 to max-fraction 
                for fraction = (/ num denom)
                do
                (when (equal-within-tolerance number fraction
                                              (expt 10 (- decimal-places)))
                  (setf result fraction)
                  (return)))
           finally (return result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Mar 25 16:41:13 2015 
(defun float-list= (list1 list2 &optional (tolerance 0.000001d0))
  ;; MDE Tue Jun 23 15:30:08 2020, Heidhausen -- every backs out when the
  ;; shortest list is exhausted, meaning one could have more elements than the
  ;; other but we'd still return T
  (and (= (length list1) (length list2))
       (every #'(lambda (x y) (equal-within-tolerance x y tolerance))
              list1 list2)))

;;; (fscale 0 -10 10 1.9 .1) means we can map exponents of 1.9 to .1 from -10
;;; to 10 with a zero point returning an exponent of 1
(defun fscale (val min max new-min new-max)
  (float (+ new-min (* (/ (- val min) (- max min))
                       (- new-max new-min)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May 25 10:53:49 2015 -- remove the occurrence of a pair of elements
;;; in a list. Symbols and numbers should work as is but other list element
;;; types might need an optional test.

(defun remove-pair (list pair &optional (test #'eq))
  (let ((positions '())
        (result '())
        (p1 (first pair))
        (p2 (second pair)))
    (loop for e1 in list and e2 in (rest list) and i from 0 do
         (when (and (funcall test e1 p1) (funcall test e2 p2))
           (push i positions)
           (push (1+ i) positions)))
    (if positions
        (progn
          (setf positions (nreverse positions))
          (loop with pos = (pop positions) for el in list and i from 0 do
               (if (and pos (= i pos))
                   (setf pos (pop positions))
                   (push el result)))
          (nreverse result))
        list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jul 27 16:24:23 2015 -- normalise a set of numbers to between 0 and
;;; 1 (as floats). NB this is not the same as normalising a set of amplitudes as
;;; they only conform to a max of 1.0 and shouldn't descend to 0.0
(defun normalise (numbers)
  (if (= (length numbers) 1)
      '(1.0)
      (let* ((min (loop for n in numbers minimize n))
             (max (loop for n in numbers maximize n))
             (diff (- max min))
             (scaler (if (zerop diff) 1.0 (float (/ diff))))
             (offset (* min scaler)))
        (loop for n in numbers collect (- (* scaler n) offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Oct 1 16:23:43 2015 -- return T or NIL depending on whether
;;; sequence contains _all_ of the sub sequences in patterns
;;; e.g. (seq-has-all '("rests" "sdf" "kjr") "sdflkjrestsdf" ) --> T
(defun seq-has-all (patterns sequence)
  (loop with count = 0
     for p in patterns do
       (when (search p sequence)
         (incf count))
     finally (return (= count (length patterns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun seq-has-some (patterns sequence)
  (loop for p in patterns do
    (when (search p sequence)
      (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Oct  1 16:29:34 2015 -- T or NIL depending on whether sequence has
;;; _any_ of the patterns.
;;; (seq-has-none '("rerts" "sadf" "kjrm") "sdflkjrestsdf" ) --> T
(defun seq-has-none (patterns sequence)
  (loop for p in patterns do
       (when (search p sequence)
         (return nil))
     finally (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jan 30 17:27:50 2016
(defun factorial (int)
  (if (= 1 int)
      1
      (* int (factorial (1- int)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Apr 23 16:07:43 2016 -- 
;;; ****f* utilities/between-extremes
;;; DATE
;;; April 23rd 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Given a <progress> value of between 0 and 1, we'll return whatever that
;;; proportion is of the difference between <min> and <max> added to min. NB No
;;; randomness here.
;;; 
;;; ARGUMENTS
;;; - the minimum value (returned when <progress> is 0.0)
;;; - the maximum value (returned when <progress> is 1.0)
;;; - a value between 
;;; 
;;; RETURN VALUE
;;; a number between min and max
;;; 
;;; EXAMPLE
#|
(BETWEEN-EXTREMES 0.5 1 0.5)
==> 0.75
(BETWEEN-EXTREMES 0.5 1 0.9)
==> 0.95
|#
;;; SYNOPSIS
(defun between-extremes (min max progress)
;;; ****  
  (+ min (* progress (- max min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue May  3 20:31:23 2016
(defun flip (list)
  (let ((min (apply #'min list))
        (max (apply #'max list)))
    (loop for el in list collect (+ min (- max el)))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sun May  8 14:26:44 2016 
(defstruct morph i1 i2 proportion)

(defun morph-list (list &optional first-down (test #'eq))
  (let ((llen (length list)))
    (if (<= llen 2)
        list
        (let* ((rmd (remove-duplicates list))
               (el1 (first rmd))
               (el2 (second rmd))
               (positions (loop for el in list and i from 0
                             when (funcall test el el2) collect i))
               (push-el2 nil)
               (result '()))
          (unless (= 2 (length rmd))
            (error "utilities::morph-list: list should only have two distinct ~
                  elements: ~a" list))
          (flet ((do-em (&rest amounts)
                   (if push-el2
                       (push el2 result)
                       (setq push-el2 t))
                   ;; the float (am) is the proportion of the el2 we want
                   ;; so (3 1 0.1) would mean 10% 1 and 90% 3
                   (loop for am in amounts
                      do (push (make-morph :i1 el1 :i2 el2 :proportion am)
                               result))))
            ;; we need el1 at the beginning not el2
            (push (if first-down
                      (make-morph :i1 el1 :i2 el2 :proportion .9)
                      el1)
                  result)
            (if (= 1 (first positions))
                (push (if first-down
                          (make-morph :i1 el1 :i2 el2 :proportion .6)
                          el1)
                      result)
                (apply #'do-em (down-up (1- (first positions))
                                        :down first-down)))
            (loop for p1 in positions for p2 in (rest positions)
               for num = (- p2 p1) do
                 (case num
                   (1 (push el2 result))
                   (2 (do-em .75))
                   (3 (do-em .7 .85))
                   (4 (do-em .75 .5 .75))
                   (5 (do-em .8 .6 .4 .7))
                   (6 (do-em .8 .6 .3 .6 .8))
                   (7 (do-em .8 .6 .4 .2 .5 .8))
                   (t (apply #'do-em (down-up (1- num))))))
            (push el2 result)
            (setq result (reverse result))
            ;; MDE Tue May 24 11:43:26 2016 -- with lists of length 3, 4, and 5
            ;; we've not had enough wiggle room to do the morph properly and
            ;; we'll have one too many el2's at the end of the list so lop them
            ;; off
            (unless (= llen (length result))
              (setq result (subseq result 0 llen)))
            ;; (format t "~%morph-list: ~a --> ~a" list result)
            result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doesn't include the target at the end by default
;;; ****f* utilities/down-up
;;; DATE
;;; May 8th 2016
;;; 
;;; DESCRIPTION
;;; This is a routine used in morphing maps but may be useful elsewhere. It
;;; interpolates between two numbers over a given number of steps before
;;; returning back to the first number. 
;;; 
;;; ARGUMENTS
;;; - the number of steps over which the procedure should interpolate
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :down. T or NIL: whether to first descend before ascending. Default = T
;;; - :up. T or NIL: whether to ascend after descending. Default = T
;;; - :start. The number to start at. Default = 1.0
;;; - :target. The number to interpolate towards. Default = 0.0.
;;; - :cons. Whether to return :start as the first number in the result list.
;;; - :butlast. Whether to omit the :start when ascending. Default = T.
;;; 
;;; RETURN VALUE
;;; A list of numbers.
;;; 
;;; EXAMPLE
#|
(mapcar #'(lambda (x) (decimal-places x 2)) (down-up 20))
--> (0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.0 0.09 0.18 0.27 0.36 0.45 0.55 0.64
     0.73 0.82 0.91)
(mapcar #'(lambda (x) (decimal-places x 2)) (down-up 20 :cons t))
--> (1.0 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
     0.9)
(mapcar #'(lambda (x) (decimal-places x 2)) (down-up 20 :butlast nil))
--> (0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
     1.0)
|#
;;; SYNOPSIS
(defun down-up (steps &key (down t) (up t) (start 1.0d0) (target 0.0d0)
                        (cons nil) (butlast t))
;;; ****
  (unless (> steps 0)
    (error "utilities::down-up: <steps> should be > 1: ~a" steps))
  (when butlast (incf steps))
  (when cons (decf steps))
  (let* ((fn2 (floor steps 2))
         (rem (- steps fn2))
         (both (and down up))
         (range (abs (- start target)))
         (inc1 (/ range (if both fn2 steps)))
         (inc2 (/ range (if both rem steps)))
         (seq1 (progn
                 (when (< start target)
                   (setq inc1 (- inc1)
                         inc2 (- inc2)))
                 ;; (format t "~&inc1 ~a inc2 ~a" inc1 inc2)
                 (when down (loop with am = (- start inc1)
                               repeat (if both fn2 steps)
                               collect am ; (+ target am)
                               ;; have to do it this way as loop's by clause
                               ;; has to be a positive number
                               do (decf am inc1)))))
         (seq2 (when up (loop with am = inc2
                           repeat (if both rem steps)
                           collect (+ target am)
                           do (incf am inc2))))
         (result (cond (both (append seq1 seq2))
                       (down seq1)
                       (up seq2))))
    (when cons (push (if down start target) result))
    (if butlast (butlast result) result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/rescale
;;; DATE
;;; June 8th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Given a value within an original range, return its value withing a new range
;;; 
;;; ARGUMENTS
;;; - the value we want to rescale
;;; - the original minimum
;;; - the original maximum
;;; - the new minimum
;;; - the new maximum
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :out-of-range. The function to call when the first argument is not within
;;; the range of arguments two and three. This would normally be #'error,
;;; #'warn or NIL. If #'warn or NIL, argument 1 will be hard-limited to the
;;; original range. Default = #'error
;;; :type-of-result. Usually this function uses float precision, but by setting
;;; type-of-result to #'double-float or #'rationalize, it is more precise.
;;; 
;;; RETURN VALUE
;;; The value within the new range (a number)
;;; 
;;; EXAMPLE
#|
(rescale .5 0 1 0 100)
==> 50.0
|#
;;; SYNOPSIS
(defun rescale (val min max new-min new-max &optional (out-of-range #'error)
                                              (type-of-result #'float))
;;; ****
  (flet ((oor () ; in case we need to call it on more than one occasion...
           (when (functionp out-of-range)
             (funcall out-of-range
                      "utilities::rescale: first argument (~a) should be ~
                       within the ~%original range (~a to ~a)" val min max))))
    (when (or (>= min max)
              (>= new-min new-max))
      (error "utilities::rescale: argument 2 (~a) must be < argument 3 (~a) ~
              ~%and sim. for argument 4 (~a) and 5 (~a)"
             min max new-min new-max))
    (unless (and (>= val min)
                 (<= val max))
      (oor)
      (setf val (if (> val max) max min)))
    (let* ((range1 (funcall type-of-result (- max min)))
           (range2 (funcall type-of-result (- new-max new-min)))
           (prop (funcall type-of-result (/ (- val min) range1))))
      (+ new-min (* prop range2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/centre-list
;;; DATE
;;; March 9th 2021, Heidhausen
;;; 
;;; DESCRIPTION
;;; Take a list of numbers and scale them so that they are symmetrical(ish)
;;; around either the mid-point (exactly) or the middle element (in terms of
;;; value). 
;;; 
;;; ARGUMENTS
;;; - a list of numbers
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether the middle element should the zero point from
;;; which the other elements are offset or (if NIL) to use the calculated middle
;;; point. So if T, we won't centre around 0 but if NIL we will (i.e. if NIL
;;; we'll go equally as far in the negative direction as positive). In each case
;;; however, the returned list will by necessity range from negative to positive
;;; values.
;;; 
;;; RETURN VALUE
;;; A list of numbers.
;;; 
;;; EXAMPLE
#|
(centre-list '(1 2 3 4 5 6) t)
-> (-3 -2 -1 0 1 2) 
(centre-list '(1 2 3 4 5 6) NIL)
-> (-2.5 -1.5 -0.5 0.5 1.5 2.5)
(centre-list '(12.2 -11 7 13 14 15 16 -2) nil)
-> '(9.7 -13.5 4.5 10.5 11.5 12.5 13.5 -4.5)
|#
;;; SYNOPSIS
(defun centre-list (list &optional zero)
;;; ****
  (unless (every #'numberp list)
    (error "utilities::centre-list: argument 1 must be a list of numbers: ~a"
           list))
  (let* ((sorted (sort (copy-list list) #'<))
         (high (first sorted))
         (low (first (last sorted)))
         (range (- high low))
         (len (length list))
         ;; given a list of even length, this means we centre higher rather than
         ;; lower 
         (mid-el (nth (floor len 2) sorted))
         (mid-point (+ low (/ range 2.0))))
    (if zero
        (loop for i in list collect (- i mid-el))
        (loop for i in list collect (- i mid-point)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro popnew (list avoid &key (test #'eq))
  (let ((index (gensym))
        (result (gensym)))
    `(let* ((,index (loop for i in ,list and j from 0
                       unless (funcall ,test i ,avoid) return j))
            (,result (when ,index (nth ,index ,list))))
       (if ,result
           (progn (setf ,list (append (subseq ,list 0 ,index)
                                      (subseq ,list (1+ ,index))))
                  ,result)
           ;; <avoid> wasn't in <list>
           (pop ,list)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; good for closing tuplets: if list1 has negative numbers whose absolute
;;; value is in list2, change neg for pos, e.g.
;;; (pos4neg '(-1 -3) '(-1 3)) -> (-1 3) 
;;; (pos4neg '(-1 (3 3)) '(-1 3)) -> (-1 (3 3))
(defun pos4neg (list1 list2)
  (loop for i in list1 for ai = (when (numberp i) (abs i)) collect
       (if (and ai (member ai list2))
           ai
           i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Jun 13 18:36:30 2017 -- divide a list of integers by their greatest
;;; common divisor e.g. (/gcd '(21 12 6)) -> (7 4 2) or (/gcd '(20 12 6)) ->
;;; (10 6 3)
(defun /gcd (list)
  (let ((gcd (apply #'gcd list)))
    (mapcar #'(lambda (x) (/ x gcd)) list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun id-as-list (thing)
  (and (listp thing) (= 1 (length thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Nov  5 11:57:45 2018 -- constrain any given integer to within a
;;; range (inclusive). Works with negative numbers too. 
(defun constrain-int (int min max)
  (unless (and (integerp int) (integerp min) (integerp max))
    (error "utilities::constrain-int: all three arguments should be integers."))
  (unless (> max min)
    (error "utilities::constrain-int: 3rd (~a) arg must be > 2nd (~a)" max min))
  (let* ((range (1+ (- max min)))
         (im (mod (- int min) range)))
    (+ min im)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue May 21 07:55:42 2019 -- interleave elements from a list but starting
;;; in the middle and working forwards and backwards 
(defun middle-out (list)
  (let* ((len (length list))
         (len2 (floor len 2))
         (mup (subseq list len2))
         (mdown (reverse (subseq list 0 len2)))
         (il (interleave mup mdown)))
    ;; interleave ignores last element of one list if they're not of the same
    ;; length (see above)
    (if (evenp len)
        il
        (append il (last list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jul 15 13:50:12 2019
;;; LF 2024-02-23 14:13:19 more flexible version (should also work on windoze):
(defun auto-set-default-dir (&optional subdir)
  (let ((load-name (or *load-truename* *compile-file-truename*
                       *default-pathname-defaults*)))
    (set-sc-config
     'default-dir
     (trailing-slash
      (format nil "~a~a"
              (namestring
               (make-pathname :directory (pathname-directory load-name)
                              :device (pathname-device load-name)))
              (if subdir subdir ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan 21 14:08:55 2021, Heidhausen
;;; LF 2024-02-23 14:12:54 more flexible version (should also work on windoze):
(defun path-from-same-dir (&optional file)
  (let ((load-name (or *load-truename* *compile-file-truename*
                       *default-pathname-defaults*)))
    (format nil "~a~a"
            (namestring
             (make-pathname :directory (pathname-directory load-name)
                            :device (pathname-device load-name)))
            (if file file ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jul 15 13:50:21 2019
(defun load-from-same-dir (file)  
  (load (path-from-same-dir file)))

;;; MDE Thu Nov 25 10:26:36 2021, Heidhausen
(defun compile-from-same-dir (file)  
  (load (compile-file (path-from-same-dir file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Thu 6 Feb 2020 17:41:19 GMT
;;; Taken from Stackoverflow, many thanks Barmar
;;; stackoverflow.com/questions/42867749/reading-lisp-objects-from-a-string 

(defun string-to-list (string)
  (with-input-from-string (stream string)
    (loop with eof-marker = '#:eof
       for object = (read stream nil eof-marker)
       until (eq object eof-marker)
       collect object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/write-list-to-coll
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Tue 18 Feb 2020 15:38:40 GMT
;;; 
;;; DESCRIPTION
;;; Turn a list of lists into a text file, formatted to be read by the MaxMSP
;;; [coll] object. This is a bit like gen-max-coll-file (see set-palette.lsp)
;;; but instead works with any data in a list.
;;; 
;;; ARGUMENTS
;;; - A list of lists in the form '((a b c) (d e f))
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; :file - the output file. Default = "/tmp/sc-max-coll.txt"
;;; :base - the minimum number for coll indexing. In the resulting output file,
;;; each list in the list of lists will be preceeded by an (increasing) integer
;;; and a comma. This argument sets the base value of that integer. Default = 0.
;;; :capitalize - Should any outputted text be capitalized or not?
;;; Default = NIL.
;;; :if-exists - what to do if the file already exists. This argument is passed
;;; to with-open-file. More info here: http://clhs.lisp.se/Body/m_w_open.htm
;;; Default = :supercede
;;; :prefix - add a string prefix to the item number. Default = NIL.
;;; :alt-label - if you do not want the items labels to be consecutive numbers,
;;; then you can here either provide a list of lists or a function. If a list of
;;; lists, this must be the same length as the first argument. If a function, it
;;; must be called with the item number (which increases incrementally from
;;; base). 
;;;
;;; RETURN VALUE
;;; The output file location
;;; 
;;; EXAMPLE
#|
(let ((l '((hello!)(how are you?)(very well thank you.)(1 2 3 4))))
      (write-list-to-coll l :base 6))

=> "/tmp/sc-max-coll.txt" 

The resulting text file will looks like this when opened:

6, hello!;
7, how are you?;
8, very well thank you.;
9, 1 2 3 4;

;; DJR Tue 3 Mar 2020 13:52:34 GMT
(let ((l '((hello!)(how are you?)(very well thank you.)(1 2 3 4))))
      (write-list-to-coll l :base 15 
                          :alt-label #'(lambda (count)
                                         (let ((l '(foo bar)))
                                           (nth (mod count 2) l)))
                          :prefix "yes_"))

=> "/tmp/sc-max-coll.txt" 

The resulting text file will looks like this when opened:

yes_bar, hello!;
yes_foo, how are you?;
yes_bar, very well thank you.;
yes_foo, 1 2 3 4;

|#
;;; SYNOPSIS
(defun write-list-to-coll (data-list &key (base 0)
                                       (file "/tmp/sc-max-coll.txt")
                                       (capitalize nil)
                                       (if-exists :supersede)
                                       ;; DJR Tue 3 Mar 2020 13:52:34 GMT
                                       (prefix "")
                                       alt-label)
;;; ****
  (with-open-file
      (stream file
              :direction :output :if-exists if-exists
              :if-does-not-exist :create)
    (loop for i in data-list
       for count from base
       with item
       do
         (cond ((and alt-label (listp alt-label))
                (if (= (length data-list) (length alt-label))
                    (setf item (nth count alt-label))
                    (error "~%utilities::write-list-to-coll: If you provide ~
                           alt-label as a list, ~%it must be the same length ~
                           as the data-list.")))
               ((functionp alt-label)
                (setf item (funcall alt-label count)))
               (t (setf item count)))
         (when (null item)
           (error "~%utilities::write-list-to-coll: Items set to 'nil'. ~
                    Check the value of 'base' or ~%the output of 'alt-list'. ~
                    ~%base: ~a~%count: ~a~%list-item: ~a" base count i))
         (if capitalize
             (format stream "~&~a~a, ~a;" prefix item (list-to-string i))
             (format stream "~&~(~a~)~(~a~), ~(~a~);" prefix item
                     (list-to-string i))))
    file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/now-string
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Sat 28 Mar 2020 13:21:08 GMT - London
;;; 
;;; DESCRIPTION
;;; Return a string representing the current time in the format:
;;; YEAR MONTH DAY - HOURS MINUTES SECONDS
;;; e.g. "20200328-132227"
;;;
;;; It is thought that this function might be useful when outputing multiple
;;; files during the test phase of a piece. E.g.
;;; (cmn-display +mini+
;;;              :file (concatenate 'string "my-piece" (now-string) ".eps"))
;;; 
;;; ARGUMENTS
;;; None
;;; 
;;; OPTIONAL ARGUMENTS
;;; None
;;; 
;;; RETURN VALUE
;;; A string
;;; 
;;; EXAMPLE
#|
(now-string)

=> "20200328-132227"

(concatenate 'string "my-piece_" (now-string) ".eps")

=> "my-piece_20200328-133357.eps"
|#
;;; SYNOPSIS
(defun now-string ()
;;; ****
  (let ((d (multiple-value-list (get-decoded-time))))
    (format nil "~a~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
            (sixth d)
            (fifth d)
            (fourth d)
            (third d)
            (second d)
            (first d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; for convenience, precision is fraction of a tone not a semitone
(defun semitones-precision (semitones &optional (precision 1/4))
  (setq precision (/ precision 2))
  (float (/ (round semitones precision) (/ precision))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/one-to-many
;;; DATE
;;; June 23rd 2020
;;; 
;;; DESCRIPTION
;;; Find a one-to-many relationship between the first argument and a number of
;;; equally-spaced points given in the second argument. The first argument is a
;;; number between 0.0 and 1.0 (inclusive). We calculate the proximities from
;;; this point to the number of points given and return them as a list,
;;; optionally raised to a given exponent. The list returned is scaled so that
;;; all values sum to 1.0, so this is particularly useful for, say, calculating
;;; a number of amplitude scalers for a multi-voice synthesis process.
;;; 
;;; ARGUMENTS
;;; - the point: a number between 0.0 and 1.0 inclusive
;;; - the number of points to use in the calculation. This will also be the
;;;   number of results returned. Alternatively this can be a list of numbers
;;;   between 0.0 and 1.0. This way you can pass your own points for e.g. an
;;;   unequally-spaced set. In this case though the proximity is still
;;;   determined from a maximum of 1.0, not the highest number in the given
;;;   list. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the exponent to raise proximities to. 1.0 will return a linear
;;;   relationship. > 1.0 will exaggerate the relationships so that those points
;;;   further away from the first argument will be pushed further away than a
;;;   linear relationship. < 1.0 will lessen the distances.
;;; 
;;; RETURN VALUE
;;; a list of numbers the length of which is the same as the 2nd argument and
;;; the sum of which is 1.0
;;; 
;;; EXAMPLE
#|
(one-to-many .8 7) -->
(0.045112778 0.082706764 0.12030074 0.15789473 0.19548872 0.21804512 0.18045112)
(one-to-many .8 7 .7) -->
(0.06509622 0.09950039 0.1293403 0.15646003 0.18169008 0.19612299 0.17178996)
(one-to-many .8 7 1.3)  -->
(0.030845987 0.06782856 0.11039718 0.15721251 0.20752355 0.23917702 0.1870151)
;;; passing 5 points: these don't have to have min/max of 0 and 1 ...
(one-to-many .8 '(0 .1 .35 .7 .92)) -->
(0.07067137 0.10600708 0.19434628 0.3180212 0.31095406)
;;; ... and they don't have to be in ascending order either
(one-to-many .8 '(0 .1 .35 .7 .2)) -->
(0.08510638 0.12765959 0.23404254 0.38297874 0.17021276)
|#
;;; SYNOPSIS
(defun one-to-many (one how-many &optional (expt 1.0))
;;; ****
  (unless (number-between one 0.0 1.0)
    (error "utilities::one-to-many: first argument (~a) should be between ~
            0.0 and 1.0" one))
  (unless (or (and (integerp how-many) (> how-many 1))
              (and (listp how-many)
                   (every #'(lambda (x)
                              (and (numberp x)
                                   (>= x 0.0)
                                   (<= x 1.0)))
                          how-many)))
    (error "utilities::one-to-many: second argument (~a) ~%should either be ~
            an integer > 1 or a list of numbers between 0.0 and 1.0"
           how-many))
  (let* ((points (if (integerp how-many)
                     (loop for i to 1 by (/ (1- how-many)) collect i)
                     how-many))
         (proximities (loop for p in points collect
                           (expt (- 1.0 (abs (- one p))) expt)))
         (sum (apply #'+ proximities)))
    (loop for p in proximities collect (/ p sum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun minlen4 (list &optional (warn t))
  (let* ((e1 (first list))
         (e2 (second list))
         (e3 (third list))
         (len (length list))
         (result (case len
                   (1 (list e1 e1 e1 e1))
                   (2 (list e1 e2 e1 e2))
                   (3 (list e1 e2 e3 e2))
                   (t list))))
    (when (and warn (< len 4))
      (warn "utilities::minlen4: the caller needs at least  ~
                        4 list elements.~%Using ~a instead of ~a"
            result list))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Sep 24 15:22:11 2020, Heidhausen
(defun positions (item sequence &key (test #'eq))
  (loop with start = 0
     for pos = (position item sequence :test test :start start)
     while pos
     collect pos
     do (setq start (1+ pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Sep 24 15:22:17 2020, Heidhausen
;;; positions starts with the first end point, not with 0
;;; if skip, the positions refer to markers in the list that shouldn't be
;;; returned  
(defun subseqs (list positions &optional skip) 
  (loop for i1 in (cons (if skip -1 0) positions)
     for i2 in (econs positions nil) ; so we go to the end
       ;; skip the elements at the positions
     for subseq = (subseq list (if skip (1+ i1) i1) i2)
     when subseq collect subseq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if the list length is odd, the first half will be one shorter than the 2nd.
(defun halves (list)
  (let ((middle (floor (length list) 2)))
    (list (subseq list 0 middle) (subseq list middle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wiggle-extend (list &key (start 0) max (freq 3))
  (let* ((beg (subseq list 0 start))
         (al (make-al))
         (count 0)
         result last)
    (loop for i in (nthcdr start list) do
         (push i result)
         (when (and last (active al freq))
           (if (or (not max) (< (1+ count) max))
               (progn
                 (incf count 2)
                 (push last result)
                 (push i result))
               ;; if max is an odd number, we won't be able to add the last and
               ;; the current before continuing but rather than generated too
               ;; many results, just insert the last, skip the current, then
               ;; proceed with the next. this will mean results like 236 237 236
               ;; 238 if we have consecutive numbers
               (when (< count max)
                 (incf count)
                 (push last result))))
         (setq last i))
    (append beg (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/average
;;; DESCRIPTION
;;; Get the average value of a list of numbers.
;;; 
;;; ARGUMENTS
;;; - the list of numbers
;;; 
;;; RETURN VALUE
;;; the average of the list, as a float
;;; 
;;; SYNOPSIS
(defun average (num-list)
;;; ****
  (unless (every #'numberp num-list)
    (error "utilities::average: argument must be a list of numbers:~%~a"
           num-list))
  ;; this exhausts the stack with long lists
  ;; (float (/ (apply #'+ num-list) (length num-list))))
  (let ((sum 0.0d0))  ; double-precision
    (mapcar #'(lambda (x) (incf sum x)) num-list)
    (/ sum (length num-list))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/force-symmetrical-and-normalise
;;; DATE
;;; February 8th 2022
;;; 
;;; DESCRIPTION
;;; Take a list of numbers (usually samples but any of course) and, using the
;;; average sample value, offset them to be symmetrical around 0.0
;;; (usually). This is not the same as but is related to removing DC offset:
;;; here we're offsetting numbers individually rather than using the 
;;; high-pass filter approach in the DSP time domain.
;;;
;;; Note that to make the samples symmetrical we offset by the average of the
;;; existing samples so any occasional outliers will still spike in one
;;; direction or another, thus making a waveform view seem to be still offset.
;;;
;;; Optionally also normalise to within -1.0 and 1.0 (or other values: see
;;; below). If either :min or :max are nil then normalisation won't be applied.
;;; 
;;; ARGUMENTS
;;; a list of floats (double precision)
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :min. The new minimum value to map to. Default = -1.0
;;; :max. The new minimum value to map to. Default = 1.0
;;; :verbose. Print some stats. Default = NIL.
;;; 
;;; RETURN VALUE
;;; the list of new samples
;;; 
;;; SYNOPSIS
(defun force-symmetrical-and-normalise (samples &key (min -1.0)
                                                  (max 1.0) verbose)
;;; ****
  (let* ((av (average samples))
         (sampmax 0.0d0)                ; double-precision please
         (newsamples (loop for sample in samples
                           for symsamp = (- sample av)
                           for symsampv = (abs symsamp)
                           do
                              (when (> symsampv sampmax)
                                (setq sampmax symsampv))
                           collect symsamp))
         (scaler (/ sampmax)))
    (when verbose
      (format t "~&force-symmetrical-and-normalise: ~%  sampmax ~
                 (abs, after correcting offset): ~,6f, ~%  ~
                 average (before correction): ~,6f, scaler: ~,6f~%"
              sampmax av scaler))
    ;; make samples range from -1.0 to 1.0
    (when (and min max)                 ; normalise
      (setq newsamples (mapcar #'(lambda (s) (* s scaler)) newsamples)))
    (if (and min max
             (not (equal-within-tolerance min -1.0))
             (not (equal-within-tolerance max 1.0)))
        (mapcar #'(lambda (s) (rescale s -1.0 1.0 min max)) newsamples)
        newsamples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun accumulate (numbers)
  (unless (every #'numberp numbers)
    (error "utilities::accumulate: argument must be a list of numbers:~%~a"
           numbers))
  (loop for n in numbers with a = 0.0
        do (incf a n)
        collect a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May 30 20:03:00 2022, Heidhausen -- precede spaces with a \ e.g. in
;;; pathnames 
(defun escape-spaces (string)
  (string-replace " " "\\ " string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sndfile-size (duration channels bits-per-sample
                     &optional (sampling-rate 48000))
  (let ((bytes (float (* channels (mins-secs-to-secs duration)
                         (/ bits-per-sample 8) sampling-rate )))
        (kbytes (ash 1 10)) ; or (expt 2 10)
        (mbytes (ash 1 20)) ; or (expt 2 20)
        (gbytes (ash 1 30)))
    (cond ((> bytes gbytes) (values (/ bytes gbytes) 'gigabytes))
          ((> bytes mbytes) (values (/ bytes mbytes) 'megabytes))
          ((> bytes kbytes) (values (/ bytes gbytes) 'kilobytes))
          (t (values bytes 'bytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/decider
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; February 23rd 2023
;;;
;;; DESCRIPTION
;;; Return an index, which can be used to select an element from a sequence,
;;; when provided with a list of weights (see the example). It does that by
;;; scaling the selector argument relative to the sum of all weights, using
;;; rescale. Then it goes through all the weights and as soon as the selector
;;; is smaller than the sum of the weights so far, the index of the current
;;; weight is returned. So when given a list of weights '(2 1), the following
;;; selectors will return:
;;; 0     => 0
;;; 1/3   => 0
;;; 19/30 => 0
;;; 2/3   => 1
;;; 1     => 1
;;; This process is thus deterministic. By providing a random number as a
;;; selector, you can make random choices etc.
;;; 
;;; ARGUMENTS
;;; - A number between 0 and 1
;;; - A list of numbers, representing weights 
;;; 
;;; RETURN VALUE
;;; index of chosen element
;;;
;;; EXAMPLE
#|
;;; simple example, choosing from a list:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (nth (decider 0.1 weights) ls))
=> d4

;;; making a simple melody, following a sine wave:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (loop for i from 0 to pi by 0.25 collect
       (nth (decider (abs (sin i)) weights) ls)))
=> (C4 E4 F4 G4 B4 B4 B4 B4 B4 A4 G4 F4 D4)

;;; make a random melody with 10 pitches:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (loop repeat 10 collect
       (nth (decider (random 1.0) weights) ls)))
|#
;;; SYNOPSIS
(defun decider (selector weights)
;;; ****  
  (labels ((helper (selector ls1 index sum)
             (cond ((null ls1) (1- (length weights)))
                   ((< selector sum) index)
                   (t (helper selector
                              (cdr ls1)
                              (+ index 1)
                              (+ sum (rationalize (car ls1))))))))
    (helper (rescale (rationalize selector) 0 1 0 (loop for i in weights sum
                                                       (rationalize i)))
            (cdr weights) 0 (rationalize (car weights)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/visualize
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; February 23rd 2023
;;; 
;;; DESCRIPTION
;;; Print a Visualization of an array or a list into the repl with 64 values
;;; -> *ascii art* <-
;;; 
;;; ARGUMENTS
;;; - An array or a list
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :y-range. Maximum value that the y-axis display. When nil, the graph will be
;;; normalized.
;;; :start. Where to start reading the sequence from. Default = 0 
;;; :abs. When t, the absolute value of all numbers is visualized
;;; :scale. If the sequence is shorter than 64 and scale is t, the graph will
;;; be scaled to 64 values
;;; 
;;; RETURN VALUE
;;; ":)"
;;;
;;; EXAMPLE
#|
(visualize (loop repeat 64 for i from 0 by 0.1 collect (sin i)))
(visualize (loop repeat 128 for i from 0 by 0.1 collect (sin i)) :start 64)
(visualize (loop repeat 128 for i from 0 by 0.1 collect (* (sin i) 2))
   :scale nil :start 96)
(visualize (loop repeat 55 for i from 0 by 0.1 collect (* (sin i) 2))
   :scale nil :abs t :y-range 1)
|#
;;; SYNOPSIS
(defun visualize (ls &key y-range (start 0) abs (scale t))
;;; ****
  (when (arrayp ls)
    (setf ls (loop for i across ls collect i)))
  (when abs (setf ls (loop for i in ls collect (abs i))))
  (let* ((matrix (make-array '(64 17) :initial-element 0.0))
         (maxi (apply #'max (mapcar #'abs ls)))
         (y-range (if y-range y-range
                      (if (= maxi 0) 1 maxi)))
         (len (length ls))
         (size (if (or scale (>= (- len start) 64)) 64 (- len start))))
    (loop for i from start below (+ size start) do
         (loop for j below 17 do
              (if (= (round (+ (* (/ (nth (mod (floor
                                                (+ start
                                                   (if scale
                                                       (* (/ i size)
                                                          (- len start))
                                                       i)))
                                               len)
                                          ls)
                                     y-range)
                                  8 (if abs 2 1))
                               (* 8 (if abs 0 1))))
                     j)
                  (setf (aref matrix (- i start) j) 1)
                  (setf (aref matrix (- i start) j) 0))))
    (loop for j downfrom 16 to 0 do
         (print (apply 'concatenate 'string
                       (loop for i below 64 collect
                            (if (= (aref matrix i j)  1)
                                "_"
                                " ")))))
    "=)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/os-format-path
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; March 30th 2023
;;; 
;;; DESCRIPTION
;;; Converts device-names ("/E/", "E:/") according to type
;;; Windows:  "E:"
;;; Unix:     "/E/"
;;; If type is nil it will be set to the system this function was called on.
;;; 
;;; ARGUMENTS
;;; - a string representing a path
;;;
;;; OPTIONAL ARGUMENTS
;;; - a string or symbol which could be unix, linux or windows. If nil the type
;;; of the current system will automatically be chosen.
;;; 
;;; RETURN VALUE
;;; - a string representing a path
;;;
;;; EXAMPLE
#|
(os-format-path "/E/samples/kicks/kick.wav")
=> "/E/samples/kicks/kick.wav"
(os-format-path "E:/samples/kicks/kick.wav")
=> "/E/samples/kicks/kick.wav"
(os-format-path "/E/samples/kicks/kick.wav" 'windows)
=> "E:/samples/kicks/kick.wav"
(os-format-path "E:/samples/kicks/kick.wav" 'windows)
=> "E:/samples/kicks/kick.wav"
|#
;;; SYNOPSIS
(defun os-format-path (path &optional type)
;;; ****  
  (let* ((new-path (substitute #\/ #\: path))
         (device (if (char= #\/ (elt path 0))
                     (second (pathname-directory path))
                     (format nil "~{~a~}"
                             (loop with break until break for i from 0 collect
                                  (let ((this (elt path i))
                                        (next (elt path (1+ i))))
                                    (when (or (char= #\: next)
                                              (char= #\/ next))
                                      (setf break t))
                                    this)))))
         (helper (subseq new-path (1+ (position #\/ new-path :start 1))))
         (rest (if (and (> (length helper) 0) (char= #\/ (elt helper 0)))
                   helper
                   (format nil "/~a" helper))))
    (unless type
      (setf type
            #+(or win32 win64) 'windows
            #-(or win32 win64) 'unix))
    ;; intering the symbol is nicer when calling this from other packages
    (case (intern (string type) :sc)
      ((unix linux) (format nil "/~a~a" device rest))
      ((or windows) (format nil "~a:~a" device rest))
      ;; if type is unknown, no error but unix type path:
      (t (format nil "/~a~a" device rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper/main worker for smooth-procession
(defun single-steps (list)
  (if (every #'integerp list)
    (let ((result (list (first list))))
      (loop for i1 in list for i2 in (rest list)
            for diff = (- i2 i1)
            for dabs = (abs diff)
            for down = (< diff 0)
            do
               (when (> dabs 1)
                 (loop with ni = i1 repeat (1- dabs) do
                   (if down (decf ni) (incf ni))
                   (push ni result)))
               (push i2 result))
      (nreverse result))
    (error "utilities::single-steps: all elements of the given list ~
            must be integers: ~a" list)))

(defun steps-by-one-max (list)
  (loop for i1 in list for i2 in (rest list)
        for diff = (abs (- i2 i1)) do
          (unless (or (zerop diff) (= 1 diff))
            (return nil))
        finally (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/smooth-procession
;;; DATE
;;; 23rd August 2024
;;; 
;;; DESCRIPTION
;;; like the procession function but ensuring that we only ever ascend or
;;; descend values by a step of 1 
;;; 
;;; ARGUMENTS
;;; the same as the procession function
;;; 
;;; OPTIONAL ARGUMENTS
;;; ditto for the keyword arguments
;;; 
;;; RETURN VALUE
;;; the procession list
;;; 
;;; SYNOPSIS
(defun smooth-procession (num-results items
                          &rest keyargs &key &allow-other-keys)
  ;; if :orders isn't explicitly given, then use a different default that should
  ;; result in less jumps
  (unless (member :orders keyargs)
    (push '((1 2 1 2 3 2) (1 2 1 2 3) (2 1 2 3 2 1)) keyargs)
    (push :orders keyargs))
  (let ((last (first (last items)))
        (nr num-results)
        (max-len (floor num-results .9))
        (indices (loop for i below (length items) collect i))
        sproc)
    (flet ((do-it ()
             (setq sproc (single-steps
                          (apply #'procession
                                 (cons nr (cons indices keyargs))))))
           (wiggle (x) (setq nr (floor nr x))))
      ;; reduce num-results by 10% until the length of smooth-procession is
      ;; within 10% of the target. This won't guarantee we hit the last element
      ;; but it'll give us a decent chance.
      (loop (do-it)
            (if (<= (length sproc) max-len)
              (return)
              (wiggle 1.1)))
      ;; we couldn't get a result that was longer than num-results but not too
      ;; long so go back up again
      (loop while (< (length sproc) num-results) do
        (let ((tmp nr))
          (wiggle .9)
          ;; when we get down below 10 wiggle might not result in enough
          (when (= tmp nr)
            (incf nr))
          (do-it)))
      (setq sproc (subseq sproc 0 num-results)
            indices (copy-list sproc)
            sproc (mapcar #'(lambda (i) (nth i items)) sproc))
      (unless (member last sproc)
        (warn "utilities: smooth-procession: last element (~a) not present in ~
             result." last))
      (values sproc indices))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lsp
