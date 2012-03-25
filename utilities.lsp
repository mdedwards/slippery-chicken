;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/utilities
;;; NAME 
;;; utilities
;;;
;;; File:             utilities.lsp
;;;
;;; Class Hierarchy:  none: no classes defined
;;;
;;; Version:          0.9
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Various helper functions of a general nature.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    June 24th 2002
;;;
;;; $$ Last modified: 19:45:45 Mon Mar 19 2012 GMT
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

;;; separator is what separates mins and secs; default is e.g. 12:06
;;; same-width t would make sure all those minutes under 10 are written in the
;;; format e.g. 00:12.2 

(defun secs-to-mins-secs (seconds &key (separator ":") (same-width nil))
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

(defun mins-secs-to-secs (list)
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

(defun float-int-p (x)
  (equal-within-tolerance 0.0 (nth-value 1 (round x))))

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

;;; replace 'words' in a string
;;; e.g. (string-replace "flat" "\\flat" "bflat clarinet") -> "b\\flat clarinet"
(defun string-replace (what with string) ; all strings
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

;;; End cons: puts an element at the end of a list.

(defun econs (list new-back)
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

(defun get-sublist-indices (list)
  (loop 
      with index = 0
      for sublist in list collect index
      do (incf index (length sublist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In order to flatten then recreate a list of lists, we need to know
;;; the index in the flattened list of the first element of each sublist.
;;; e.g. (GET-SUBLIST-INDICES '((1 2) (3 4) (5 6 7) (8 9) (10))) ->
;;; (0 2 4 7 9)

(defun get-sublist-lengths (list &optional (remove-zeros nil))
  (let ((result (loop for sublist in list collect (length sublist))))
    (if remove-zeros
        (remove 0 result)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* utilities/all-members
;;; FUNCTION
;;; all-members:
;;; find out whether the members of the list given as the second argument are
;;; all present in the list that forms the first argument.
;;; 
;;; ARGUMENTS     
;;; - the list we need to find members of the second argument in
;;; - the list whose members we need to find in the first list
;;; - (optional) the test 
;;;
;;; RETURN VALUE  
;;; t or nil
;;; 
;;; EXAMPLE
;;; (all-members '(1 2 3 4 5 6 7) '(1 2 3 7)) -> T
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

;;; e.g (split-into-sub-groups '(1 2 3 4 5 6 7 8 9 10) '(2 2 3 2 1)) ->
;;; ((1 2) (3 4) (5 6 7) (8 9) (10))

(defun split-into-sub-groups (list groups)
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

;;; Like above but just one length is given and this will be used for sublists
;;; e.g. (split-into-sub-groups2 '(1 2 3 4 5 6 7 8 9) 4) ->
;;; ((1 2 3 4) (5 6 7 8) (9))

(defun split-into-sub-groups2 (list length)
  (loop for i from 0 by length while list collect
        (loop repeat length while list collect (pop list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Same as above but put any left over elements in the penultimate sub-list
;;; e.g. (split-into-sub-groups3 '(1 2 3 4 5 6 7 8 9) 4) ->
;;; ((1 2 3 4) (5 6 7 8 9))

(defun split-into-sub-groups3 (list length)
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

(defun power-of-2 (float)
  (whole-num-p (log float 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the power of 2 <= num

(defun nearest-power-of-2 (num)
  (if (power-of-2 num)
      num
    (loop with p = 1 do
          (if (> p num)
              (return (/ p 2))
            (setf p (* p 2))))))                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flatten (nested-list)
  (cond ((null nested-list) nil)
        ((atom nested-list) (list nested-list))
        (t (append (flatten (first nested-list))
                   (flatten (rest nested-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun equal-within-tolerance (a b &optional (tolerance 0.000001d0))
  ;; BTW coercing a and b to 'double-float doesn't help if they are of
  ;; different float types to begin with.
  (<= (abs (- a b)) tolerance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Mar 19 19:39:55 2012 

(defun decimal-places (num places)
  (let ((pow (float (expt 10 places))))
    (/ (round num (/ pow)) pow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun almost-zero (num &optional (tolerance 0.000001))
  (equal-within-tolerance num 0.0 tolerance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (round-if-close 23.99999) -> 24
;;; (round-if-close 23.00000001) -> 23
;;; (round-if-close 23.0001) -> 23.0001
;;; (round-if-close 22) -> 22

(defun round-if-close (num &optional (tolerance 0.000001))
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

;;; Sort a list of symbols alphabetically but case-insensitive.

(defun sort-symbol-list (list)
  (sort list
        #'(lambda (x y) 
            (string-lessp (string x) (string y)))))
                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (1 2 3 4 ".") -> 1.2.3.4

;;;(format nil "~{~a~a~^~}" separator list))

(defun list-to-string (list &optional (separator " ") (nil-as-string t))
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

(defun semitones (st &optional (octave-size 2.0) (divisions-per-octave 12))
  "semitones just returns the right src from the given 
   semitone transposition. e.g. (semitones 12) => 2.0"
  (expt octave-size (/ st divisions-per-octave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return the semitone transposition for a given sampling rate
;;; conversion factor.

(let ((last8vesize 0)
      (log8ve 0.0)) ;; do we don't have to recalculate each time
  (defun srt (srt &optional (octave-size 2.0) (divisions-per-octave 12)
              ;; MDE Tue Feb  7 16:59:45 2012 -- round so we don't get tiny
              ;; fractions of semitones due to float inaccuracies?
              (round-to 0.0001))
    (unless (= octave-size last8vesize)
      (setf last8vesize octave-size
            log8ve (log octave-size 10.0)))
    (let ((result (/ (* divisions-per-octave (log srt 10.0))
                     log8ve)))
      (if round-to
        (/ (round result round-to) (/ round-to))
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Replace the elements in list between start and end (inclusive) with the new
;;; list  
;;; (replace-elements '(0 1 2 3 4 5 6 7 8 9) 3 7 '(dog cat))
;;; -> (0 1 2 DOG CAT 8 9)

(defun replace-elements (list start end new)
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

;;; (splice '(3 4 5) '(1 2 6 7 8) 2) -> (1 2 3 4 5 6 7 8)

(defun splice (elements into-list where)
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

;;; start is 0-based.
;;; (remove-elements '(0 1 2 3 4 5 6) 0 2) -> (2 3 4 5 6)

(defun remove-elements (list start how-many)
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

(defun setf-last (list new-last)
  (unless list
    (error "utilities::setf-last: list is empty!"))
  (setf (nth (1- (length list)) list) new-last))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmacro mk (mark &rest data)
  `(cmn::get-cmn-marks ',mark ,@data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ml (element repeat)
  (make-list repeat :initial-element element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Each list in the argument should all be of the same structure, ie have the
;;; same length and number of sublists etc.
;;;
;;; e.g. (nconc-sublists '(((1 2) (a b)) ((3 4) (c d)) ((5 6) (e f))))
;;; -> ((1 2 3 4 5 6) (A B C D E F))
;;; (nconc-sublists '(((1 2 3) (a b c) (blah blah blah)) 
;;;                   ((3 4 5) (c d e) (ding dong ding)) 
;;;                   ((5 6 7) (e f g) (sing song sing))))
;;;-> ((1 2 3 3 4 5 5 6 7) (A B C C D E E F G)
;;;    (BLAH BLAH BLAH DING DONG DING SING SONG SING))


(defun nconc-sublists (lists)
  (let ((len (length lists)))
    (loop for d in (first lists) and i from 0 collect
          (if (simple-listp d)
              (loop for j below len nconc (nth i (nth j lists)))
            (nconc-sublists (loop for j below len 
                                  collect (nth i (nth j lists))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; go from low to high in num-steps steps but using an exponential curve 
;;; instead of straight interpolation 

(defun logarithmic-steps (low high num-steps &optional (exponent 2))
  (loop for i below num-steps 
      with curve = (list 0 low (1- num-steps) high)
      collect
        (interpolate i curve :exp exponent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpolate (point env &key (scaler 1) (exp 1) (warn t))
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
  
  
(defun interp-aux (point env scaler exp)
  (let ((here (loop for i in env by #'cddr and j from 1 do
                    (if (<= point i) (return (+ j j -2))))))
    ;; rounding in making the new-env with new-lastx can cause the
    ;; very last event to be just a little bigger than the last x
    ;; value in the new-env.  If this is the case, <here> will be nil
    ;; so we better deal with this: 
    (unless here 
        (setq here (- (length env) 2)))
    (if (= here 0) (setq here 2))
    (get-interpd-y point 
                   (nth (- here 2) env)
                   (* scaler (nth (- here 1) env))
                   (nth here env)
                   (* scaler (nth (+ here 1) env))
                   exp)))

(defun get-interpd-y (point x1 y1 x2 y2 exp)
  "The arguments are the point we want interpolated,
   the x,y coordinates of the line the point lies within, and an exponent.  
   The calculation is how far away point is from x1 divided by how far x2 is 
   from x1 raised to the exponent exp, multiplied by the difference of y2 and 
   y1 all added to y1."
  (float (+ y1 (* (expt (/ (- point x1) 
                           (- x2 x1)) exp)
                  (- y2 y1)))))


(defun lastx (env)
  "lastx returns the last x value in the given envelope.
   e.g. (lastx '(0 0 20 4 30 5 100 0)) => 100"
  (let ((len (length env)))
    (when (oddp len) 
        (error "utilities::lastx: Wrong number of elements in ~a." env))
    (nth (- len 2) env)))

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

(defun scale-env (env y-scaler &key x-scaler 
                                    (x-min most-negative-double-float)
                                    (y-min most-negative-double-float)
                                    (x-max most-positive-double-float)
                                    (y-max most-positive-double-float))
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
      collect (if x-scaler (min x-max (max x-min (* x x-scaler)))
                x) 
      collect (min y-max (max y-min (* y y-scaler)))))

(defun reverse-env (env)
  "reverse-env returns the reverse of the envelope 
   supplied to it.  
   e.g. (reverse-env '(0 0 60 .3 100 1)) => (0 1 40 0.3 100 0)."
        (let ((x-max (lastx env))
              (result nil))
          (loop for x in env by #'cddr and y in (cdr env) by #' cddr do
                (push y result)
                (push (- x-max x) result))
          result))


(defun repeat-env (env num-times &optional reflected)
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

(defun env-plus (env add)
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr
      collect x collect (+ y add)))

(defun env-symmetrical (env &optional (centre .5) 
                        (min most-negative-double-float)
                        (max most-positive-double-float))
  "Returns an envelope that is symmetrical around the key variable 'centre'.
   e.g. (symmetrical '(0 0 30 .2 70 .95 100 .5)) => 
   (0 1.0 30 0.8 70 0.05 100 0.5)"
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
     for new-y = (- (* 2.0 centre) y)
     collect x collect (max min (min new-y max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 3.2.11: returns a list of length new-len by adding or removing items at
;;; regular intervals.  If adding items and the list contains numbers, linear
;;; interpolation will be used.
(defun force-length (list new-len)
  (let* ((len (length list))
         (diff (- new-len len)))
    (when (or (zerop new-len) (>= diff len))
      (error "force-length:: new length must be between 1 and 1 less than ~%~
              the original length: ~a ~%(length: ~a, new-len: ~a)" 
             list len new-len))
    (if (= len new-len)
        list
        (let* ((adiff (abs diff))
               (result '())
               (cycle (unless (zerop diff) (max 1 (1- (floor len adiff)))))
               (points (loop for i from cycle by cycle repeat adiff collect i)))
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
                     (push el result))  ; get this element too of course
                   (setf next (pop points)))
                 ;; not on point so get it
                 (push el result))
             (setf last-el el))
          (setf result (nreverse result))
          (unless (= (length result) new-len)
            (error "force-length:: somehow got the wrong length: ~a"
                   (length result)))
          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This function reads a wavelab marker file and prints the list of arguments
;;; necessary to create a sndfile object for a sndfile-palette.  The marker
;;; file defines sections with start/stop marker pairs, the first one of which
;;; should be named (the description).
;;; 
;;; marker-file is the path to the file
;;; sndfile is the name of the file, not the whole path, rather the name given
;;; at the start of a new sndfile in a palette

(defstruct wavelab-section
  (sndfile) (description) (start) (end))


;;; This handles as many marker files as you want, if they're in a list.  Only
;;; caveat is that each file must refer to the same sndfile...but that could be
;;; easily altered. 
(defun parse-wavelab-marker-files-for-sections 
    (marker-files sndfile
     &optional (sampling-rate 44100) (print nil))
  (if (listp marker-files)
      (loop for file in marker-files appending
            (parse-wavelab-marker-file-for-sections
             file sndfile sampling-rate print))
    (parse-wavelab-marker-file-for-sections
     marker-files sndfile sampling-rate print)))

;;; This handles one marker file.
(defun parse-wavelab-marker-file-for-sections
    (marker-file sndfile
     &optional (sampling-rate 44100) (print nil))
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
              (when (string= param "name")
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
                             Got marker with name \"~a\" at ~a; ~
                             expected no name"
                            name (secs-to-mins-secs time)))
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
                (format t "~&~a markers read from ~a" count marker-file)
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
             #|
             (when (string= param "name")      
             (setf name (if (string= value "*")
             nil
             value))
             (incf count))
             |#
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
"/music/limine/nuendo/zkm-compressed-reverb-44-24.MRK" 88200)
             |#

(defun wavelab-to-audacity-marker-file (file &optional (sampling-rate 44100))
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
                (when (string= param "name")      
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

(defun parse-wavelab-marker-file-for-loops
    (marker-file &optional (sampling-rate 44100) (max-length 1.0))
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
                  (when (string= param "name")
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

;;; (parse-audacity-label-file-for-loops "loops.txt")

(defun parse-audacity-label-file-for-loops (label-file)
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

;;; reflect-list will order a list from least to greatest (so has to be
;;; numbers) then transpose the list so that if an element is the second
;;; lowest, it will be replace by the second highest etc.

;;; (reflect-list '(1 4 3 5 9 6 2 7 8 8 9))

(defun reflect-list (list)
  (let ((sorted (sort (copy-list list) #'<))
        (len (length list))
        (pos 0))
    (loop for i in list 
        do
          (setf pos (position i sorted))
        collect
          (nth (- len pos 1) list))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun middle (lower upper)
  (+ lower (/ (- upper lower) 2.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hz2ms (hertz)
  (/ 1000.0 hertz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (split-groups 31 10) -> (10 10 10 1)

(defun split-groups (num divider)
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

;;; (remove-more '(1 2 3 4 5 5 5 6 7 7 8) 5 7 2) -> (1 3 4 6 8)

(defun remove-more (list test &rest remove)
  (loop 
      with result = list
      for r in remove
      do
        (setf result (remove r result :test test))
      finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; convert points (72 per inch) to centimeters.

(defun pts2cm (points)
  (* 2.54 (/ points 72.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; low and high are inclusive except if float given: then we can't quite reach
;; high.  
(defun between (low high &optional fixed-random)
  (unless (> high low)
    (error "utilities::between: high (~a) should be > low (~a)" high low))
  (if (and (integerp low) (integerp high))
      (+ low (funcall (if fixed-random #'random-rep #'random)
                      (1+ (- high low))))
      (+ low (funcall (if fixed-random #'random-rep #'random)
              (- high low)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this randomises a number
;;; always returns float
(defun randomise (number &optional (percent 5))
  (let ((room (* number (/ percent 200.0))))
    (between (- number room) (+ number room))))

;;; this returns a random portion of the number, +/- percent
(defun random-amount (number &optional (percent 5))
  (let ((pc (/ percent 100.0)))
    (* number (+ (- (/ pc 2.0))
                 (random (float pc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-from-list (list &optional list-length) ;; for efficiency
  (nth (random (if list-length 
                   list-length 
                 (length list)))
       list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-from-file (file)
  (with-open-file
   (stream file :direction :input :if-does-not-exist :error)
   (read stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-list (list start)
  (append (nthcdr start list) (subseq list 0 start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-into-symbol (&rest params)
  (read-from-string (format nil "~{~a~^~}" params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert a character object to a symbol
(defun char2sym (char)
  (intern (coerce (list char) 'string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Swap elements of a list e.g. '(1 2 3 4 5 6 7 8) -> '(2 1 4 3 6 5 8 7)
(defun swap-elements (list)
  (loop for a in list by #'cddr and b in (cdr list) by #'cddr 
     collect b collect a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun factor (num fac)
  (when (zerop fac)
    (error "utilities::factor: 2nd argument cannot be 0: (factor ~a ~a)"
           num fac))
  (zerop (mod num fac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun octave-freqs (freq1 freq2 &optional (unison-also t))
  (or (and unison-also (= freq1 freq2))
      (power-of-2 (/ freq1 freq2))
      (power-of-2 (/ freq2 freq1))))

;;; MDE Tue Dec 13 12:08:03 2011 -- whether either freq is a partial of the
;;; other  
(defun partial-freqs (freq1 freq2 &optional (unison-also t))
  (or (and unison-also (= freq1 freq2))
      (factor freq1 freq2)
      (factor freq2 freq1)))

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

(defun get-harmonics (fundamental &key (start-at 1) (min-freq 20)
                      (max-freq 20000))
  (loop for h from start-at
     for freq = (* fundamental h)
     while (<= freq max-freq)
     if (>= freq min-freq)
       collect freq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro db2amp (db)
  `(expt 10.0 (/ ,db 20)))

(defmacro amp2db (amp)
  `(* 20.0 (log ,amp 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; return a list containing onlt those elements that are not in the first
;;; argument list

(defun remove-all (rm-list list &optional (test #' eq))
  (loop for rm in rm-list do
       (setf list (remove rm list :test test)))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun amplitude-to-dynamic (amp &optional (warn t))
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

(defun dynamic-to-amplitude (dynamic &optional (warn t))
  (let ((pos (position dynamic '(niente pppp ppp pp p mp mf f ff fff ffff))))
    (if pos
        (/ pos 10.0)
        (when warn
          (warn "utilities::dynamic-to-amplitude: unrecognised dynamics: ~a"
                dynamic)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2.3.11: if any of the items in <what> are in <from>, they're moved to <to>
(defun move-elements (what from to &optional (test #'eq))
  (unless (listp what)
    (setf what (list what)))
  (loop for el in what do
       (when (member el from :test test)
         (setf from (remove el from))
         (push el to)))
  (values from to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.5.11: move an element of a list to the end of the list, returning the
;;; new list.  NB if the element exists more than once all occurrences will be
;;; removed and only one of them placed at the end. e.g. 
;;; (move-to-end 2 '(1 2 2 3 4 5)) -> (1 3 4 5 2)
(defun move-to-end (what list &optional (test #'eql))
  (if (member what list :test test)
      (econs (remove what list :test test) what)
      list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun force-list (thing)
  (if (listp thing)
      thing
      (list thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implementation of the Collatz conjecture (see
;;; http://en.wikipedia.org/wiki/Collatz_conjecture)
;;; (loop for i from 5 to 30 do
;;;        (print i) (print (loop for n in (hailstone i) sum n)))
(defun hailstone (n)
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
     (format nil "~aexamples/~a" cl-user::+slippery-chicken-home-dir+ file)))

(defun load-from-test-suite-dir (file)
  (format t "~%~%*******  Loading ~a~%" file)
  (load 
     (format nil "~atest-suite/~a" cl-user::+slippery-chicken-home-dir+ file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lsp
