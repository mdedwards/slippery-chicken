;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* circular-sclist/popcorn
;;; NAME 
;;; assoc-list
;;;
;;; File:             popcorn.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> popcorn
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Inspired by popping popcorn, generate a series of values
;;;                   ranging between > 0.0 and <= 1.0 by (optionally fixed)
;;;                   random selection.  Given 1 or more starting values (not
;;;                   zero) we generate tendentially increasing new values
;;;                   until we reach 1.0.  This is not a linear process,
;;;                   rather, we get spike values that increase the average
;;;                   value and thus increase the chance of further spikes.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    3rd February 2011 (Ko Lanta, Thailand)
;;;
;;; $$ Last modified: 16:27:55 Sat Apr 28 2012 BST
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

;;; We only subclass the circular version for convenience; it's a straight list
;;; we're generally after.  The starting data is stored in the data slot.

(defclass popcorn (circular-sclist)
  ;; our results
  ((kernels :accessor kernels :initform nil)
   ;; the running total
   (total :accessor total :initform -1.0)
   ;; the number of kernels we've generated so far
   (numk :accessor numk :initform -1)
   ;; the current average
   (mean :accessor mean :initform -1.0)
   ;; whether to use fixed-randomness (for repeatable results) or not
   (fixed-random :accessor fixed-random :initarg :fixed-random
                 :initform t)
   ;; the min/max multipliers we'll use to scale the average when creating
   ;; spikes
   (min-spike :accessor min-spike :initarg :min-spike :initform 2.0)
   (max-spike :accessor max-spike :initarg :max-spike :initform 4.0)
   ;; the min/max value of the kernels so far
   (mink :accessor mink :initform -1.0)
   (maxk :accessor maxk :initform -1.0)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((pc popcorn) &rest initargs)
  (declare (ignore initargs))
  ;; reset the random-rep function
  (when (fixed-random pc)
    (random-rep 1.0 t))
  (heat pc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((pc popcorn))
  (loop for val in (data pc) do
       (when (or (<= val 0.0) (>= val 1.0))
         (error "popcorn::verify-and-store: starting values should be ~
                 > 0.0 and < 1.0; found ~a in ~a" val (data pc))))
  (setf (total pc) (loop for k in (data pc) sum k)
        (numk pc) (length (data pc)))
  ;; all named-objects have to be able to be initialised with data=nil so that
  ;; clone works
  (when (data pc)
    (unless (> (numk pc) 1)
      (error "popcorn:verify-and-store: popcorn must be initialised with ~
              at least two values: ~a" (data pc)))
    ;; we'll be pushing new kernels in so need to reverse
    (setf (kernels pc) (reverse (data pc))
          (mink pc) (loop for k in (data pc) minimize k)
          (maxk pc) (loop for k in (data pc) maximize k))
    (set-mean pc nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((pc popcorn))
  (clone-with-new-class pc 'popcorn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((pc popcorn) new-class)
  (declare (ignore new-class))
  (let ((cscl (call-next-method)))
    (setf (slot-value cscl 'kernels) (copy-list (kernels pc))
          (slot-value cscl 'total) (total pc)
          (slot-value cscl 'numk) (numk pc)
          (slot-value cscl 'mink) (mink pc)
          (slot-value cscl 'maxk) (maxk pc)
          (slot-value cscl 'fixed-random) (fixed-random pc)
          (slot-value cscl 'min-spike) (min-spike pc)
          (slot-value cscl 'mean) (mean pc)
          (slot-value cscl 'max-spike) (max-spike pc))
    cscl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((pc popcorn) stream)
  (format stream "~%POPCORN: kernels: ~a ~%total: ~a, numk: ~a, mink: ~a, ~
                   maxk: ~a~%min-spike: ~a, max-spike: ~a, ~
                   fixed-random: ~a, mean: ~a"
          (kernels pc) (total pc) (numk pc) (mink pc) (maxk pc)
          (min-spike pc) (max-spike pc) (fixed-random pc) (mean pc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this uses the internal slots to calculate the average and stores a new
;;; value as well.
;;; ****m* popcorn/set-mean
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
(defmethod set-mean ((pc popcorn) new-kernel)
;;; ****
  (when new-kernel
    (incf (total pc) new-kernel)
    (incf (numk pc))
    (push new-kernel (kernels pc)))
  (setf (mean pc) (/ (total pc) (numk pc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Apr 28 14:13:25 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; this is the main function we'll call

;;; ****m* popcorn/heat
;;; FUNCTION
;;; Generate a series of values for a specified popcorn object, ranging between
;;; >0.0 and <= 1.0, by (optionally fixed) random selection.
;;;
;;; Taking the one or more starting values of the popcorn object, the method
;;; generates tendentially increasing new values until it reaches 1.0. This is
;;; not a linear process; rather, the method produces spike values based on the
;;; min-spike and max-spike values of the given popcorn object that increase
;;; the average value and thus increase the chance of further spikes.
;;; 
;;; ARGUMENTS
;;; - A popcorn object.
;;; 
;;; RETURN VALUE
;;; Returns a popcorn object with a newly generated list of 'kernel' values. 
;;; 
;;; EXAMPLE
#|
(let ((ppcn (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)))
  (heat ppcn))

=> 
POPCORN: kernels: (0.01 0.02 0.015828498 0.015408514 0.015781755 0.01670348
                   0.019892192 0.017849509 0.016623463 0.019682804 0.017869182
                   0.019521425 0.017451862 0.017689057 0.01758664 0.01863435
                   0.018176062 0.01843462 0.018410202 0.018346658 0.017676951
                   0.01797271 0.017877633 0.01845327 0.017664826 0.019336458
                   0.018315567 0.01932739 0.06895776 0.020922642 0.060532082
                   0.065836325 0.023154635 0.03457066 0.049225926 0.036467202
                   0.034944084 0.06656975 0.03086034 0.059058335 0.047811303
                   0.054176323 0.037284575 0.042504393 0.033154454 0.060916394
                   0.065254904 0.03896744 0.06031616 0.05883082 0.053335194
                   0.06291643 0.06688037 0.060092658 0.04888454 0.04399702
                   0.052252926 0.057551242 0.05167044 0.058247015 0.044084225
                   0.059822164 0.050194997 0.049477555 0.038833477 0.12390294
                   0.102146074 0.05612515 0.06555225 0.10998846 0.046505846
                   0.09265815 0.11737937 0.09315801 0.08859882 0.12227169
                   0.09748195 0.12235684 0.07775879 0.06688609 0.04773577
                   0.053784113 0.048935942 0.058266696 0.06296183 0.068995684
                   0.11519497 0.109607905 0.10322696 0.09138289 0.09752339
                   0.07788754 0.10596406 0.062283877 0.06499343 0.11718479
                   0.122381 0.07470882 0.08504762 0.12193823 0.07747522
                   0.077790916 0.22953248 0.1118135 0.09011707 0.08901599
                   0.18462129 0.060578153 0.21434677 0.1899727 0.10457036
                   0.1569566 0.15677738 0.18686697 0.18597648 0.19952527
                   0.1779859 0.0916294 0.15786766 0.07046401 0.1054758
                   0.09319253 0.12790032 0.13795748 0.16031852 0.080126666
                   0.17231674 0.32247475 0.3597934 0.21751955 0.15212698
                   0.29898494 0.14838159 0.10816099 0.12190436 0.29802915
                   0.2750276 0.3183031 0.2381512 0.34616244 0.22499663
                   0.12633777 0.2022125 0.44220465 0.28806102 0.09475795
                   0.22921178 0.10413365 0.42066357 0.42282805 0.13085815
                   0.19452573 0.24054746 0.42521718 0.43162823 0.36088073
                   0.17512095 0.32570451 0.3879723 0.33864918 0.3092579
                   0.186878 0.31767192 0.43376347 0.32372838 0.26407608
                   0.22793972 0.27224308 0.36888647 0.3487581 0.20514718
                   0.29874432 0.38396028 0.19942182 0.27353722 0.33861154
                   0.19429457 0.29090053 0.36862695 0.27244034 0.22764236
                   0.23576522 0.15455621 0.30663335 0.15333486 0.24629539
                   0.42236584 0.30913097 0.28433847 0.15702207 0.3474056
                   0.35555315 0.37104702 0.3161528 0.5221748 0.15071268
                   0.44310752 0.4066335 0.2379877 0.257548 0.30994186
                   0.28978062 0.23058617 0.51340276 0.3163629 0.4516536
                   0.29794478 0.580572 0.66034466 0.32460666 0.5384419
                   0.18716219 0.34882295 0.46895513 0.57769305 0.17264767
                   0.37031317 0.33509848 0.484978 0.21844691 0.4013908
                   0.18625522 0.2474415 0.55304855 0.20333095 0.22846258
                   0.65167195 0.6276612 0.37307173 0.21918331 0.6191604
                   0.27020022 0.7847625 0.70708793 0.25344852 0.85900164
                   0.29388186 0.5300259 0.6454721 0.47205538 0.31486645
                   0.22869632 0.8383382 0.3004762 0.67320794 0.7120085
                   0.51940644 0.955306 0.7064787 0.6791451 0.80159026 0.8806825
                   0.6786009 0.27193812 0.69382477) 
total: 55.72378, numk: 255, mink: 0.01, maxk: 0.955306
min-spike: 3.0, max-spike: 5.0, fixed-random: T, mean: 0.21852463
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0.01 0.02)

|#
;;; SYNOPSIS
(defmethod heat ((pc popcorn))
;;; ****
  (loop for k = (get-kernel pc) while k)
  (setf (kernels pc) (nreverse (kernels pc)))
  pc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; our values range between >0 <= 1; put them in a new range
;;; NB this doesn't change our internal state except for the kernels slot
;;; ****m* popcorn/scale
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
(defmethod scale ((pc popcorn) max &optional (min 0.0) ignore1 ignore2)
;;; ****
  (declare (ignore ignore1)
           (ignore ignore2))
  (let ((scaler (/ (- max min) (- (maxk pc) (mink pc)))))
    (setf (kernels pc)
          (loop for k in (kernels pc) collect
               (+ min (* scaler (- k (mink pc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* popcorn/fit-to-length
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
(defmethod fit-to-length ((pc popcorn) length)
;;; ****
  (setf (kernels pc) (force-length (kernels pc) length)
        (numk pc) length))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; get the next kernel and change internal state.  returns nil when kernel >
;;; 1.0 
;;; ****m* popcorn/get-kernel
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
(defmethod get-kernel ((pc popcorn))
;;; ****
  (let ((k (get-kernel-aux pc)))
    (when (<= k 1.0)
      (set-mean pc k)
      (when (> k (maxk pc))
        (setf (maxk pc) k))
      (when (< k (mink pc))
        (setf (mink pc) k))
      k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; just return the next kernel (spike or no spike) without changing internal
;;; state 
(defmethod get-kernel-aux ((pc popcorn))
  (let ((spike (<= (between 0.0 1.0 (fixed-random pc))
                   ;; so the lower the mean, the less chance i.e. the more we
                   ;; do this, the more chance of a spike
                   (mean pc))))
    (if spike
        ;; random value between min/max spikes * the current mean
        (* (between (min-spike pc) (max-spike pc) (fixed-random pc))
           (mean pc))
        ;; if this isn't a spike, just get a value between mean and max
        (between (mean pc) (maxk pc) (fixed-random pc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; create text and data files suitable for plotting with gnuplot. file should
;;; be without extension as we'll create a .txt and a .data file, for the
;;; command and data files repectively.  call gnuplot in a terminal with
;;; something like "gnuplot popcorn.txt; open popcorn.ps"; draw data points
;;; connected by lines by default

;;; ****m* popcorn/plot
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
(defmethod plot ((pc popcorn) file &optional (lines t))
;;; ****
  (with-open-file 
      (command (concatenate 'string file ".txt")
               :direction :output :if-does-not-exist :create
               :if-exists :rename-and-delete)
    (format command "~&set terminal postscript default ~%set output \"~a.ps\"~
                  ~%plot \"~a.data\" notitle ~a~%~%" file file 
                  (if lines "with linespoints" "")))
  (with-open-file 
      (data (concatenate 'string file ".data")
               :direction :output :if-does-not-exist :create
               :if-exists :rename-and-delete)
    (loop for k in (kernels pc) and x from 0 do
         (format data "~%~a ~a" x k)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Apr 28 11:35:43 BST 2012: Added robodoc entry

;;; ****f* popcorn/make-popcorn
;;; FUNCTION
;;; Make a popcorn object in its raw state. This method sets the initial state
;;; and processing attributes of the object. The final data (list of 'kernel'
;;; values) is generated by calling the heat method on this object.
;;;
;;; ARGUMENTS
;;; - A list of at least two decimal numbers from which the 'kernel' values
;;;   will be generated. These values must be >0.0 and <1.0.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :id. An optional ID for the popcorn object to be created. Default = NIL. 
;;; - :fixed-random. T or NIL to indicate whether the 'kernel' values generated
;;;   by the subsequent heat method are to be based on a fixed random seed. 
;;;   T = fixed random. Default = T.
;;; - :max-spike. A decimal number that is the highest possible 'spike' value
;;;   that the heat method may produce when generating the 'kernel'
;;;   values. This is a sudden high value that will itself not be present in
;;;   the final data, but will go towards skewing the mean, thus increasing the
;;;   kernel values more rapidly and increasing the chance of more spikes
;;;   occurring. Default = 4.0.
;;; - :min-spike. A decimal number that is the lowest possible 'spike' value
;;;   that the heat method may produce when generating the 'kernel'
;;;   values. This is a sudden high value that will itself not be present in
;;;   the final data, but will go towards skewing the mean, thus increasing the
;;;   kernel values more rapidly and increasing the chance of more spikes
;;;   occurring. Default = 2.0.
;;; 
;;; RETURN VALUE
;;; - A popcorn object.
;;; 
;;; EXAMPLE
#|
(make-popcorn '(0.1 0.2) :max-spike 4.2 :min-spike 3.7)

=> 
POPCORN: kernels: (0.2 0.1) 
total: 0.3, numk: 2, mink: 0.1, maxk: 0.2
min-spike: 3.7, max-spike: 4.2, fixed-random: T, mean: 0.15
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0.1 0.2)

|#
;;; SYNOPSIS
(defun make-popcorn (starting-values &key (id nil) (fixed-random t)
                     (max-spike 4.0) (min-spike 2.0))
;;; ****
  (make-instance 'popcorn :data starting-values :id id 
                 :fixed-random fixed-random :max-spike max-spike
                 :min-spike min-spike))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF popcorn.lsp

