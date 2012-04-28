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
;;; Generate a series of values for an popcorn object for which the kernels
;;; have not yet been generated, ranging between >0.0 and <= 1.0, by
;;; (optionally fixed) random selection.
;;;
;;; Taking the one or more starting values of the popcorn object, the method
;;; generates tendentially increasing new values until it reaches 1.0. This is
;;; not a linear process; rather, the method produces spike values based on the
;;; min-spike and max-spike values of the given popcorn object that increase
;;; the average value and thus increase the chance of further spikes.
;;;
;;; NB: This method is called within the initialize-instance for the popcorn
;;;     object, and as such is not necessarily needed to be accessed directly
;;;     by the user.
;;; 
;;; ARGUMENTS
;;; - An popcorn object.
;;; 
;;; RETURN VALUE
;;; Returns a popcorn object with a newly generated list of 'kernel' values. 
;;; 
;;; EXAMPLE
#|
(let ((ppcn (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)))
  (heat ppcn))

=> 
POPCORN: kernels: (0.69382477 0.27193812 0.6786009 0.8806825 0.80159026
                   0.6791451 0.7064787 0.955306 0.51940644 0.7120085 0.67320794
                   0.3004762 0.8383382 0.22869632 0.31486645 0.47205538
                   0.6454721 0.5300259 0.29388186 0.85900164 0.25344852
                   0.70708793 0.7847625 0.27020022 0.6191604 0.21918331
                   0.37307173 0.6276612 0.65167195 0.22846258 0.20333095
                   0.55304855 0.2474415 0.18625522 0.4013908 0.21844691
                   0.484978 0.33509848 0.37031317 0.17264767 0.57769305
                   0.46895513 0.34882295 0.18716219 0.5384419 0.32460666
                   0.66034466 0.580572 0.29794478 0.4516536 0.3163629
                   0.51340276 0.23058617 0.28978062 0.30994186 0.257548
                   0.2379877 0.4066335 0.44310752 0.15071268 0.5221748
                   0.3161528 0.37104702 0.35555315 0.3474056 0.15702207
                   0.28433847 0.30913097 0.42236584 0.24629539 0.15333486
                   0.30663335 0.15455621 0.23576522 0.22764236 0.27244034
                   0.36862695 0.29090053 0.19429457 0.33861154 0.27353722
                   0.19942182 0.38396028 0.29874432 0.20514718 0.3487581
                   0.36888647 0.27224308 0.22793972 0.26407608 0.32372838
                   0.43376347 0.31767192 0.186878 0.3092579 0.33864918
                   0.3879723 0.32570451 0.17512095 0.36088073 0.43162823
                   0.42521718 0.24054746 0.19452573 0.13085815 0.42282805
                   0.42066357 0.10413365 0.22921178 0.09475795 0.28806102
                   0.44220465 0.2022125 0.12633777 0.22499663 0.34616244
                   0.2381512 0.3183031 0.2750276 0.29802915 0.12190436
                   0.10816099 0.14838159 0.29898494 0.15212698 0.21751955
                   0.3597934 0.32247475 0.17231674 0.080126666 0.16031852
                   0.13795748 0.12790032 0.09319253 0.1054758 0.07046401
                   0.15786766 0.0916294 0.1779859 0.19952527 0.18597648
                   0.18686697 0.15677738 0.1569566 0.10457036 0.1899727
                   0.21434677 0.060578153 0.18462129 0.08901599 0.09011707
                   0.1118135 0.22953248 0.077790916 0.07747522 0.12193823
                   0.08504762 0.07470882 0.122381 0.11718479 0.06499343
                   0.062283877 0.10596406 0.07788754 0.09752339 0.09138289
                   0.10322696 0.109607905 0.11519497 0.068995684 0.06296183
                   0.058266696 0.048935942 0.053784113 0.04773577 0.06688609
                   0.07775879 0.12235684 0.09748195 0.12227169 0.08859882
                   0.09315801 0.11737937 0.09265815 0.046505846 0.10998846
                   0.06555225 0.05612515 0.102146074 0.12390294 0.038833477
                   0.049477555 0.050194997 0.059822164 0.044084225 0.058247015
                   0.05167044 0.057551242 0.052252926 0.04399702 0.04888454
                   0.060092658 0.06688037 0.06291643 0.053335194 0.05883082
                   0.06031616 0.03896744 0.065254904 0.060916394 0.033154454
                   0.042504393 0.037284575 0.054176323 0.047811303 0.059058335
                   0.03086034 0.06656975 0.034944084 0.036467202 0.049225926
                   0.03457066 0.023154635 0.065836325 0.060532082 0.020922642
                   0.06895776 0.01932739 0.018315567 0.019336458 0.017664826
                   0.01845327 0.017877633 0.01797271 0.017676951 0.018346658
                   0.018410202 0.01843462 0.018176062 0.01863435 0.01758664
                   0.017689057 0.017451862 0.019521425 0.017869182 0.019682804
                   0.016623463 0.017849509 0.019892192 0.01670348 0.015781755
                   0.015408514 0.015828498 0.02 0.01 0.6955594 0.5994141
                   0.723659 0.8830113 0.83183795 0.2782702 0.7276858 0.5202771
                   0.83829635 0.47318345 0.48766276 0.6831889 0.37791103
                   0.46015772 0.8579426 0.25558403 0.25740623 0.45415193
                   0.423969) 
total: 66.55295, numk: 274, mink: 0.01, maxk: 0.955306
min-spike: 3.0, max-spike: 5.0, fixed-random: T, mean: 0.24289396
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

;;; SAR Sat Apr 28 18:56:46 BST 2012: Added robodoc entry

;;; ****m* popcorn/fit-to-length
;;; FUNCTION
;;; Change the length of the list of kernels contained in a given popcorn
;;; object by adding or removing items at regular intervals. If adding items,
;;; linear interpolation will be used.
;;; 
;;; ARGUMENTS
;;; - A popcorn object.
;;; - An integer that is the new length of the list of the KERNELS slot of the
;;;   given popcorn object.
;;; 
;;; RETURN VALUE
;;; Returns the integer that is the new length of the KERNELS slot.
;;; 
;;; EXAMPLE
#|
(let ((ppcn (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)))
  (fit-to-length ppcn 100))

=> 100

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
;;; Make a popcorn object. This method uses the heat method internally to
;;; generate a series of decimal values ('kernels'), ranging between >0.0 and
;;; <= 1.0, by (optionally fixed) random selection.
;;;
;;; Taking the one or more starting values, the method generates tendentially
;;; increasing new values until it reaches 1.0. This is not a linear process;
;;; rather, the method produces spike values based on the min-spike and
;;; max-spike values specified, which increase the average value of the kernels
;;; generated so far and thus increase the chance of further spikes.
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

(make-popcorn '(0.02 0.03) :max-spike 4.2 :min-spike 3.7)

=> 
POPCORN: kernels: (0.02 0.03 0.025828497 0.02540851 0.02578175 0.026703479
                   0.029892191 0.027849507 0.026623461 0.029682804 0.02786918
                   0.029521424 0.02745186 0.027689056 0.02758664 0.028634349
                   0.028176062 0.028434621 0.028410202 0.02834666 0.027676953
                   0.027972711 0.027877634 0.028453272 0.027664827 0.029336458
                   0.028315568 0.029327389 0.10877271 0.032779325 0.095442966
                   0.10383448 0.03631042 0.054371007 0.0775562 0.057371408
                   0.05496178 0.10499479 0.048501145 0.09311144 0.07531821
                   0.08538791 0.05866453 0.06692247 0.052130517 0.09605096
                   0.102914646 0.061326876 0.09510137 0.0927515 0.08405721
                   0.09921508 0.1054862 0.09474778 0.07701611 0.069283865
                   0.082345024 0.090727165 0.081423506 0.0918279 0.06942183
                   0.09431985 0.0790893 0.07795428 0.061114937 0.21615848
                   0.17666964 0.09314137 0.11025161 0.1909036 0.23906681
                   0.17467138 0.22562174 0.1757016 0.16630511 0.23570478
                   0.18461326 0.2358803 0.14396386 0.121555254 0.082086496
                   0.094552115 0.08456006 0.10379071 0.113467366 0.12590313
                   0.2211197 0.2096048 0.19645368 0.17204309 0.18469864
                   0.14422922 0.20209482 0.11207011 0.1176545 0.22522071
                   0.23593009 0.13767788 0.1589861 0.23501754 0.14337942
                   0.14403008 0.3852736 0.19077776 0.15493082 0.15311162
                   0.31107113 0.10612649 0.36018372 0.31991273 0.17881061
                   0.2653634 0.26506728 0.31478146 0.31331018 0.33569553
                   0.3001081 0.1574295 0.4698523 0.12513468 0.2010088
                   0.17438973 0.24960503 0.27139995 0.31985858 0.14607468
                   0.34586 0.52092844 0.5461051 0.33965456 0.24476483
                   0.45786726 0.23932996 0.18096672 0.5287333 0.45701692
                   0.58791053 0.5219719 0.39459002 0.56624746 0.37368405
                   0.21688993 0.3374743 0.6648663 0.44353223 0.16596928
                   0.3590309 0.17943183 0.673855 0.6455428 0.21892962
                   0.31195784 0.37920266 0.73120433 0.713979 0.5987564
                   0.29621923 0.5414667 0.64287895 0.56254905 0.514681
                   0.3153673 0.52838445 0.71745664 0.8074915 0.47637874
                   0.409207 0.49155992 0.777411 0.6339724 0.3673042 0.5411029
                   0.6993387 0.3566729 0.49429625 0.89963627 0.36773333
                   0.575006 0.74177176 0.53539884 0.4392826 0.45671058
                   0.2824728 0.60876155 0.2798523 0.47930354) 
total: 44.67911, numk: 186, mink: 0.02, maxk: 0.89963627
min-spike: 3.7, max-spike: 4.2, fixed-random: T, mean: 0.24021028
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0.02 0.03)



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

