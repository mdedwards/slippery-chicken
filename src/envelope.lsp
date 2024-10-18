;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/envelope
;;; NAME 
;;; envelope
;;;
;;; File:             envelope.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   envelope
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the envelope class for holding
;;;                   information about x-y pairs, which can be used for
;;;                   any kind of automation.
;;;
;;; Author:           Leon Focker: leon@leonfocker.de
;;;
;;; Creation date:    October 17th 2024
;;;
;;; $$ Last modified:  23:08:45 Fri Oct 18 2024 CEST
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

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass envelope (sclist)
  ;; the actual x-y pair list is stored in the data slot
  ;; We won't assume that the minimum and maximum Y values are present in the
  ;; original envelope. Sometimes an envelope doesn't range over the possible
  ;; extremes, for example (0 .3 100 .6) where the y range is from 0 to 1. For
  ;; this case adjust the following slots:
  ;; (note that these ranges might affect the behavior of some of the methods
  ;; below)
  ((x-min :accessor x-min :initarg :x-min :initform 0.0 :type number)
   (x-max :accessor x-max :initarg :x-max :initform 100.0 :type number)
   (y-min :accessor y-min :initarg :y-min :initform 0.0 :type number)
   (y-max :accessor y-max :initarg :y-max :initform 1.0 :type number)
   ;; a scaled version of the env-list, that matches the 'standard'-values
   ;; for the x and y range (see the initforms above)
   (normalised-env :accessor normalised-env :initarg :normalised-env
                   :initform nil)
   ;; a minimum distance between points on the x-axis. Relevant for example when
   ;; converting the coordinate-space of an envelope, which could potentially
   ;; distort the original shape.
   (min-point-distance :accessor min-point-distance :initarg :min-point-distance
                       :initform 1.0 :type number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((o envelope) stream)
  (format stream "~&ENVELOPE: x-min: ~a, x-max: ~a, y-min: ~a, y-max: ~a"
          (x-min o) (x-max o) (y-min o) (y-max o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((env envelope))
  (clone-with-new-class env 'envelope))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((env envelope) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    ;; the data list is copied by the named-object class clone method
    (setf (slot-value named-object 'x-min) (x-min env)
          (slot-value named-object 'x-max) (x-max env)
          (slot-value named-object 'y-min) (y-min env)
          (slot-value named-object 'y-max) (y-max env)
          (slot-value named-object 'normalised-env) (normalised-env env)
          (slot-value named-object 'min-point-distance)
          (min-point-distance env))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf data) :after (value (i envelope))
  (declare (ignore value))
  (verify-and-store i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((env envelope) &rest initargs)
  (declare (ignore initargs))
  (unless (<= (x-min env) (env-x-min env))
    (warn "envelope: the data seems to exceed the x-min"))
  (unless (>= (x-max env) (env-x-max env))
    (warn "envelope: the data seems to exceed the x-max"))
  (unless (<= (y-min env) (env-y-min env))
    (warn "envelope: the data seems to exceed the y-min"))
  (unless (>= (y-max env) (env-y-max env))
    (warn "envelope: the data seems to exceed the y-max")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-sanity ((env envelope))
  (unless (data env)
    (warn "envelope::check-sanity: env-data should not be nil."))
  (when (oddp (length (data env)))
    (error "envelope::check-sanity: Wrong number of elements in ~a." env))
  (unless (loop for first in (data env) by #'cddr
                and next in (cddr (data env)) by #'cddr
                always (<= first next))
    (warn "envelope::check-sanity: x-values don't seem to be sorted in ~a."
          env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((env envelope))
  (check-sanity env)
  (setf (normalised-env env)
        (auto-scale-env (data env)
                        :orig-y-range (list (y-min env) (y-max env))
                        :y-max 1.0)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/interpolate
;;; DESCRIPTION
;;; Get the interpolated value at a specified point within an envelope.
;;; 
;;; ARGUMENTS
;;; - A number that is the point within the specified envelope for which to
;;;   return the interpolated value.
;;; - An envelope object
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
;;; - the interpolated value (number)
;;; 
;;; EXAMPLE
#|
;;; Using the defaults                  
(interpolate 50 (make-envelope '(0 0 100 1))) 
                                        
=> 0.5                                  
                                        
;;; Specifying a different scaler       
(interpolate 50 (make-envelope '(0 0 100 1)) :scaler 2) 
                                        
=> 1.0                                  
                                        
;;; Specifying a different exponent by which the result is to be raised 
(interpolate 50 (make-envelope '(0 0 100 1)) :exp 2) 
                                        
=> 0.25                                 
                                        
|#
;;; SYNOPSIS
(defmethod interpolate (point (env envelope) &key (scaler 1) (exp 1) (warn t))
;;; ****
  (check-sanity env)
  (interpolate point (data env) :scaler scaler :exp exp :warn warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod lastx ((env envelope))
  "lastx returns the last x value in the given envelope.
   e.g. (lastx '(0 0 20 4 30 5 100 0)) => 100"
  (check-sanity env)
  (when env (nth (- (length (data env)) 2) (data env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod new-lastx ((env envelope) x)
  "new-lastx will take an envelope and return it
   with scaled x values, the maximum of which is the value of new-lastx's 
   second argument.
   e.g. (new-lastx '(0 0 30 2 100 0) 20) => (0.0 0 6.0 2 20.0 0)"
  ;; let's avoid another (sanity-check), becaus it will get called in lastx
  (let* ((scaler (float (/ x (lastx env))))
         (result (loop for x in (data env) by #'cddr
                       and y in (cdr (data env)) by #'cddr
                       collect (* x scaler) collect y)))
    ;; rounding errors can cause lastx to be slightly off so correct
    (setf (nth (- (length result) 2) result) x)
    (setf (data env) result)
    env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/scale-env
;;; DESCRIPTION
;;; Scale either the x-axis values, the data values, or both of the envelope
;;; 
;;; ARGUMENTS
;;; - An envelope object
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
;;; An envelope object.
;;; 
;;; EXAMPLE
#|

;;; Scaling only the y values.
(data (scale-env (make-envelope '(0 53 25 189 50 7 75 200 100 3)) 0.5))

=> (0 26.5 25 94.5 50 3.5 75 100.0 100 1.5)

;;; Scaling the y values and setting a min and max for those values
(data (scale-env (make-envelope '(0 53 25 189 50 7 75 200 100 3))
                 0.5 :y-min 20 :y-max 100))

=> (0 26.5 25 94.5 50 20 75 100 100 20)

;;; Scaling only the x-axis values
(data (scale-env (make-envelope '(0 53 25 189 50 7 75 200 100 3)) 1.0 :x-scaler 2))

=> (0 53.0 50 189.0 100 7.0 150 200.0 200 3.0)

;;; Scaling the x values and setting a min and max for those values
(data (scale-env (make-envelope '(0 53 25 189 50 7 75 200 100 3))
                 1.0 :x-scaler 2 :x-min 9 :x-max 90))

=> (9 53.0 50 189.0 90 7.0 90 200.0 90 3.0)

;;; 'Stretching' the envelope by providing first-x and last-x values
(data (scale-env (make-envelope '(1 0 5 1 20 0)) 1 :first-x 0 :last-x 100))

=> (0.0 0 21.052631 1 100.0 0)

|#
;;; SYNOPSIS
(defmethod scale-env ((env envelope) y-scaler &key x-scaler first-x last-x
                                        (x-min most-negative-double-float)
                                        (y-min most-negative-double-float)
                                        (x-max most-positive-double-float)
                                        (y-max most-positive-double-float))
;;; ****
  (check-sanity env)
  (setf (data env)
        (scale-env (data env) y-scaler :x-scaler x-scaler :first-x first-x
                                       :last-x last-x :x-min x-min :x-max x-max
                                       :y-min y-min :y-max y-max))
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LF 2024-10-17 15:54:41
;;; Get min/max values of the actual x-y-pairs (the data), not the value in the
;;; corresponding slot!

(defmethod env-y-min ((env envelope))
  (loop for y in (cdr (data env)) by #'cddr minimize y))

(defmethod env-y-max ((env envelope))
  (loop for y in (cdr (data env)) by #'cddr maximize y))

(defmethod env-x-min ((env envelope))
  (loop for x in (data env) by #'cddr minimize x))

(defmethod env-x-max ((env envelope))
  (loop for x in (data env) by #'cddr maximize x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/invert-env
;;; 
;;; DESCRIPTION
;;; Invert an envelope so that its maximum value becomes its minimum,
;;; vice-versa, and everything inbetween. 
;;; 
;;; ARGUMENTS
;;; An envelope object
;;; 
;;; RETURN VALUE
;;; The inverted envelope object
;;; 
;;; EXAMPLE
#|
(data (invert-env (make-envelope '(0 0 100 1))))
=> '(0 1.0 100 0.0)
(data (invert-env (make-envelope '(0 0 100 1) :y-max 2)))
=> '(0 2.0 100 1.0)
(data (invert-env (make-envelope '(0 .3 40 .4 100 .9))))
=> '(0 0.7 40 0.6 100 0.100000024)
(data (invert-env (make-envelope '(0 -.9 40 .4 100 .9) :auto-set-min-max t)))
=> (0 0.9 40 -0.39999998 100 -0.9)
|#
;;; SYNOPSIS
(defmethod invert-env ((env envelope))
;;; ****
  (check-sanity env)
  (let* ((range (- (y-max env) (y-min env))))
    (setf (data env)
          (loop for x in (data env) by #'cddr
                and y in (cdr (data env)) by #'cddr
                for d = (/ (- y (y-min env)) range)
                collect x
                collect (between-extremes (y-min env) (y-max env) (- 1.0 d))))
    (verify-and-store env)
    env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/auto-scale-env
;;; DESCRIPTION
;;; Automatically scale both the x and y values of an envelope to fit within
;;; the given ranges.
;;; 
;;; ARGUMENTS
;;; - An envelope object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :x-min: The new minimum (starting) x value
;;; - :x-max: The new maximum (last) x value
;;; - :y-min: The new minimum (not necessarily starting!) y value
;;; - :y-max: The new maximum (not necessarily starting!) y value
;;; 
;;; RETURN VALUE
;;; The scaled envelope object.
;;; 
;;; EXAMPLE
#|

(data (auto-scale-env (make-envelope '(0 0 10 1))))
=>
(0.0 0.0 100.0 10.0)

(data (auto-scale-env (make-envelope '(-1 0 .3 -3 1 1) :auto-set-min-max t)
                      :y-min 5 :y-max 6 :x-min 2))
=>
(2.0 5.75 65.7 5.0 100.0 6.0))

(data (auto-scale-env (make-envelope '(0 1 5 1.5 7 0 10 1) :auto-set-min-max t)
                      :y-min -15 :y-max -4))
=>
(0.0 -7.6666665 50.0 -4.0 70.0 -15.0 100.0 -7.6666665)

(data (auto-scale-env (make-envelope '(0 .5 100 .5) :auto-set-min-max t)
                      :y-min 1 :y-max 2))

=> (0.0 1.0 100.0 1.0)

(data (auto-scale-env (make-envelope '(0 .5 100 .5) :y-min 0 :y-max 1)
                      :y-min 1 :y-max 2))
=> (0.0 1.5 100.0 1.5)

|#
;;; SYNOPSIS
(defmethod auto-scale-env ((env envelope) &key
                                            (x-min 0.0) (x-max 100.0)
                                            (y-min 0.0) (y-max 10.0)
                                            (orig-y-range (list (y-min env)
                                                                (y-max env))))
;;; ****
  (check-sanity env)
  (setf (data env)
        (auto-scale-env (data env) :x-min x-min :x-max x-max
                                   :y-min y-min :y-max y-max
                                   :orig-y-range orig-y-range))
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/exaggerate-env
;;; DESCRIPTION
;;; Makes the y values in an envelope more radically pushed towards its
;;; extremes. Y values below the mid-point will be pushed downwards; those
;;; above will be pushed upwards. The opposite can be accomplished by making
;;; the exponent argument > 1 (see below).
;;; 
;;; ARGUMENTS
;;; - An envelope object
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
;;; The envelope object with new exaggerated data.
;;; 
;;; EXAMPLE
#|
(data (exaggerate-env (make-envelope '(0 0 50 .8 100 1)) 1.9))
--> (0 0.0 50 0.6894338 100 1.0)
(data (exaggerate-env (make-envelope '(0 0 50 .8 100 1)) .1))
--> (0 0.0 50 0.9751001 100 1.0)
(data (exaggerate-env (make-envelope '(0 0 50 .8 100 1)) -10))
--> (0 0.0 50 83.19083 100 1.0)
(data (exaggerate-env (make-envelope '(0 0 50 .8 100 1)) -10 t))
--> (0 0.0 50 0.6894338 100 1.0)
(data (exaggerate-env (make-envelope '(0 0 50 .8 100 1)) 0 t))
--> (0 0.0 50 0.8 100 1.0)
(data (exaggerate-env (make-envelope '(0 0 50 .8 100 1)) 10 t))
--> (0 0.0 50 0.9751001 100 1.0)
|#
;;; SYNOPSIS
(defmethod exaggerate-env ((env envelope) expt &optional easy-expt)
;;; ****
  (check-sanity env)
  (setf (data env) (exaggerate-env (data env) expt easy-expt))
  (verify-and-store env)
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/decimate-env
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
;;; - the original envelope object.
;;; - the number of points required in the output list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the method to be applied (symbol): 'points, 'average, 'interpolate.
;;;   Default = 'points.
;;; 
;;; RETURN VALUE
;;; The envelope object with the new data.
;;; 
;;; EXAMPLE
#|

(data (decimate-env (make-envelope '(0 0 4 4 5 5 5.1 5.1 5.3 1
                                     5.6 5.6 6 6 10 10))
                    6))
;;=> (0.0 0.0 1 2.0 2 4.5 3 4.425 4 8.0 5.0 10.0)

|#
;;; SYNOPSIS
(defmethod decimate-env ((env envelope) num-points &optional (method 'points))
;;; ****
  (check-sanity env)
  (setf (data env) (decimate-env (data env) num-points method))
  (verify-and-store env)
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/douglas-peucker
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-10-18
;;; 
;;; DESCRIPTION
;;; Implementation of the (Ramer-)Douglas–Peucker algorithm. This algorithm
;;; reduces the number of points in an envelope
;;; (cf. https://en.wikipedia.org/wiki/Ramer–Douglas–Peucker_algorithm). 
;;;
;;; ARGUMENTS
;;; - The envelope-object. 
;;; - The epsilon value. This value determines the degree of decimation by
;;;   defining the maximum distance between the original points and the
;;;   reduced/simplified envelope. The higher the value, the more the envelope
;;;   will be simplified.  Must be a float >= 0.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :sort. A function (e.g. #'<) indicating whether to sort the list by its
;;;   x-values before applying the algorithm. Default = NIL.
;;; - :destructive. When T, the original envelope object will be changed by
;;;   this method. Default = NIL.
;;; 
;;; 
;;; RETURN VALUE
;;; Three values:
;;; - The simplified envelope-object.
;;; - The number of reduced/removed points.
;;; - The reduction ratio in percent (0.0-1.0).
;;;
;;; EXAMPLE
#|
(let ((env (make-envelope
            '(0. 0.481 0.626 1.394 3.052 1.458 3.13 3.397
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
              2.837 94.053 4.167 95.931 5.321 97.418 4.952 100. 4.167)))
      (epsilon 2.8))
  (douglas-peucker env epsilon :destructive t))
;; =>
ENVELOPE: x-min: 0, x-max: 100, y-min: 0, y-max: 1
SCLIST: sclist-length: 18, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, 
                     this: NIL, 
                     next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0 0.481 17.293 5.673 61.659 2.901 64.241 7.276 65.649 2.58 65.962 7.276
       72.926 3.462 73.865 7.548 100 4.167)
**************
|#
;;; SYNOPSIS
(defmethod douglas-peucker ((env envelope) epsilon
                            &key
                              sort
                              destructive)
;;; ****
  (check-sanity env)
  (unless destructive
    (setf env (clone env)))
  (multiple-value-bind (result reduced-pts reduction-ratio)
      (douglas-peucker (data env) epsilon :sort sort)
    (setf (data env) (douglas-peucker (data env) epsilon :sort sort))
    (verify-and-store env)
    (values env reduced-pts reduction-ratio)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/env-symmetrical
;;; DESCRIPTION
;;; Create a new list of break-point pairs that is symmetrical to the original
;;; around a specified center. If no center is specified, the center value
;;; defaults to 0.5
;;; 
;;; ARGUMENTS
;;; - An envelope object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is the center value around which the values of the
;;;   new list are to be symmetrical.
;;; - A number that is to be the minimum value for the y values returned.
;;; - A number that is to be the maximum value for the y values returned.
;;; 
;;; RETURN VALUE
;;; The envelope object.
;;; 
;;; EXAMPLE
#|
;;; Default center is 0.5
(data (env-symmetrical (make-envelope '(0 0 25 11 50 13 75 19 100 23))))
                         
=> (0 1.0 25 -10.0 50 -12.0 75 -18.0 100 -22.0)
                                        
;; Specifying a center of 0             
(data (env-symmetrical (make-envelope '(0 0 25 11 50 13 75 19 100 23) 0)))
                                        
=> (0 0.0 25 -11.0 50 -13.0 75 -19.0 100 -23.0)
                                        
;;; Specifying minimum and maximum y values for the envelope returned 
(data (env-symmetrical (make-envelope '(0 0 25 11 50 13 75 19 100 23))
                       0 -20 -7))
                                        
=> (0 -7 25 -11.0 50 -13.0 75 -19.0 100 -20) 
                                     
|#
;;; SYNOPSIS
(defmethod env-symmetrical ((env envelope) &optional (centre .5) 
                                     (min most-negative-double-float)
                                     (max most-positive-double-float))
;;; ****
  (check-sanity env)
  (setf (data env)
        (env-symmetrical (data env) centre min max))
  (verify-and-store env)
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/env-plus
;;; DESCRIPTION
;;; Increase all y values of an envelope by a specified amount.
;;; 
;;; ARGUMENTS
;;; - An envelope object.
;;; - A number that is the amount by which all y values of the given envelope
;;;   are to be increased.
;;; 
;;; RETURN VALUE
;;; The envelope object.
;;; 
;;; EXAMPLE
#|
(data (env-plus (make-envelope '(0 0 25 11 50 13 75 19 100 23)) 7.1))

=> (0 7.1 25 18.1 50 20.1 75 26.1 100 30.1)

|#
;;; SYNOPSIS
(defmethod env-plus ((env envelope) add)
;;; ****
  (check-sanity env)
  (setf (data env)
        (loop for x in (data env) by #'cddr and y in (cdr (data env)) by #'cddr
              collect x collect (+ y add)))
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/convert-polar-envelopes
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
;;; - An angle-env (envelope-object)
;;; - An elevation-env (envelope-object)
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :distance-env. A distance-env (envelope-object)
;;; :minimum-samples. A number - minimal amount of points between first and
;;; last point of the envelopes at which to convert. If nil, only the original
;;; points of the envelopes are used, this however doesn't always fully
;;; represent the envelopes... Going from 0° to 180° is something else than
;;; going from y = 1 to y = -1.
;;; 
;;; RETURN VALUE
;;; - The new envelope object for the x coordinates
;;; - The new envelope object for the y coordinates
;;; - The new envelope object for the z coordinates
;;;
;;; EXAMPLE
#|

(convert-polar-envelopes (make-envelope '(0 0  1 180))
                         (make-envelope '(0 30  .5 0  1 45))
                         :minimum-samples 5)

=> ENVELOPE: x-min: 0, x-min: 100, x-min: 0, x-min: 1
SCLIST: sclist-length: 10, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0.0 0.0 25 0.68301266 50.0 1.0 75 0.65328145 100.0 8.6595606e-17)
=> ENVELOPE: x-min: 0, x-min: 100, x-min: 0, x-min: 1
SCLIST: sclist-length: 10, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0.0 0.8660254 25 0.68301266 50.0 6.123234e-17 75 -0.65328145 100.0
       -0.70710677)
=> ENVELOPE: x-min: 0, x-min: 100, x-min: 0, x-min: 1
SCLIST: sclist-length: 10, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL,  this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0.0 0.5 25 0.25881904 50.0 0.0 75 0.38268343 100.0 0.70710677)

|#
;;; SYNOPSIS
(defmethod convert-polar-envelopes ((angle-env envelope) (elevation-env envelope)
                                    &key (distance-env (make-envelope '(0 1 1 1)))
                                      minimum-samples)
;;; ****
  (check-sanity angle-env)
  (check-sanity elevation-env)
  (check-sanity distance-env)
  (multiple-value-bind (x y z)
      (convert-polar-envelopes (data angle-env) (data elevation-env)
                               :distance-env (data distance-env)
                               :minimum-samples minimum-samples)
    (values (make-envelope x)
            (make-envelope y)
            (make-envelope z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/env2gnuplot
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
;;; - The envelope object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The pathname of the data file to write.  Default = "/tmp/env.txt".
;;; 
;;; RETURN VALUE
;;; Always T
;;; 
;;; SYNOPSIS
(defmethod env2gnuplot ((env envelope) &optional (file "/tmp/env.txt"))
;;; ****
  (env2gnuplot (data env) file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* envelope/envelope-boundaries
;;; DESCRIPTION
;;; Find sharp changes in envelope values. These are defined as when a y value
;;; rises or falls over 30% (by default) of it's overall range within 5%
;;; (again, by default) of its overall x axis range.
;;; 
;;; ARGUMENTS
;;; The envelope object).
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
(ENVELOPE-BOUNDARIES
 (make-envelope '(0 10 20 10 21 3 25 4 26 9 50 7 51 1 55 2 56 7 70 10 100 10)))

=> (21 26 51 56)
|#
;;; SYNOPSIS
(defmethod envelope-boundaries ((env envelope) &optional (jump-threshold 30)
                                                  (steepness-min 5))
;;; ****
  (envelope-boundaries (data env) jump-threshold steepness-min))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelope/make-envelope
;;; DESCRIPTION
;;; Create an envelope object with the specified list.
;;; 
;;; ARGUMENTS
;;; - A list of x-y pairs, ie. an envelope
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :id. A symbol that will be the ID of the given sclist object. 
;;;   Default = NIL.
;;; - :bounds-alert. T or NIL to indicate whether a warning should be issued
;;;   when a request is given to set or get an out-of-bounds element (i.e. not
;;;   enough elements in list). T = print warning. Default = NIL.
;;; - :copy. T or NIL to indicate whether the data in the list should be copied
;;;   (any slippery-chicken class instances will be cloned), with subsequent
;;;   modifications being applied to the copy. T = copy. Default = T.
;;; - :x-min. The minimum x-value of the envelope. Default = 0.
;;; - :x-max. The maximum x-value of the envelope. Default = 100.
;;; - :y-min. The minimum y-value of the envelope. Default = 0.
;;; - :y-max. The minimum y-value of the envelope. Default = 1.
;;; - :auto-set-min-max. If t, set the min and max values above to the actual
;;;   minimum and maximum values of the provided envelope-list. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns an envelope object. 
;;; 
;;; EXAMPLE
#|
;; Create a simple object with just a two-point envelope:
(make-envelope '(0 0  100 1))

=> 
ENVELOPE: x-min: 0, x-min: 100, x-min: 0, x-min: 1
SCLIST: sclist-length: 4, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0 0 100 2)

;; Create the same object, assign an ID and set the min and max:
(make-envelope '(0 -1  1 2) :id 'simple-env :auto-set-min-max t)

=> 
ENVELOPE: x-min: 0, x-min: 1, x-min: -1, x-min: 2
SCLIST: sclist-length: 4, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SIMPLE-ENV, tag: NIL, 
data: (0 -1 1 2)

|#
;;; SYNOPSIS
(defun make-envelope (list &key (id nil) (bounds-alert t) (copy t)
                             (x-min 0) (x-max 100) (y-min 0) (y-max 1)
                             auto-set-min-max)
;;; ****
  (when auto-set-min-max
    (setf x-min (env-x-min list)
          x-max (env-x-max list)
          y-min (env-y-min list)
          y-max (env-y-max  list)))
  (make-instance 'envelope :id id :data list :bounds-alert bounds-alert
                           :copy copy :x-min x-min :x-max x-max
                           :y-min y-min :y-max y-max))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
