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
;;; Generate a series of values for the KERNELS slot of a popcorn object,
;;; ranging between >0.0 and <= 1.0, by (optionally fixed) random selection.
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

|#
;;; SYNOPSIS
(defmethod heat ((pc popcorn))
;;; ****
  (loop for k = (get-kernel pc) while k)
  (setf (kernels pc) (nreverse (kernels pc)))
  pc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Apr 28 19:18:56 BST 2012: Added robodoc entry

;;; ****m* popcorn/scale
;;; FUNCTION
;;; Scale the list of number values in the KERNEL slot of a given popcorn
;;; object to a new range using specified maximum value and optional minimum
;;; value.
;;;
;;; NB: This method does not change the the internal state of the given popcorn
;;;     object except for the KERNELS slot.
;;; 
;;; ARGUMENTS
;;; - A popcorn object.
;;; - A number that is the new maximum value for the scaled list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is the new minimum value for the scaled list.
;;; 
;;; RETURN VALUE
;;; The new contents of the given popcorn object's KERNELS slot after scaling. 
;;; 
;;; EXAMPLE
#|
;; Specifying a new maximum value only
(let ((ppcn (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)))
  (scale ppcn 10.0))

=> (0.0 0.10578585 0.061657257 0.057214428 0.061162785 0.070913345 0.10464539
    0.08303669 0.07006687 0.102430366 0.08324481 0.1007232 0.07883015
    0.08133934 0.0802559 0.09133919 0.08649117 0.08922634 0.08896804 0.08829583
    0.081211284 0.084339984 0.08333421 0.089423634 0.08108301 0.09876652
    0.08796693 0.09867058 0.6236897 0.1155461 0.5345579 0.59066933 0.13915743
    0.2599228 0.4149548 0.27998555 0.2638731 0.59842795 0.22067288 0.51896775
    0.39999005 0.467323 0.28863218 0.3438505 0.24494135 0.5386234 0.58451873
    0.30643454 0.53227377 0.516561 0.458425 0.55978096 0.60171384 0.52990943
    0.41134343 0.35964036 0.44697618 0.5030249 0.4408143 0.51038516 0.36056283
    0.527048 0.42520615 0.41761667 0.30501738 1.2049319 0.9747751 0.48793882
    0.5876642 1.0577364 0.3861802 0.8744062 1.1359217 0.87969404 0.8314643
    1.1876756 0.92543525 1.1885763 0.7167921 0.60177433 0.39919102 0.46317393
    0.4118872 0.5105933 0.56026125 0.62409085 1.112814 1.0537107 0.98620933
    0.86091584 0.92587364 0.71815413 1.0151639 0.5530894 0.5817527 1.1338633
    1.1888319 0.68452775 0.79389757 1.1841481 0.7137923 0.717132 2.3223429
    1.0770427 0.8475252 0.83587736 1.847246 0.53504527 2.1616995 1.9038564
    1.0004206 1.5545927 1.552697 1.8710022 1.861582 2.004909 1.7770531
    0.86352354 1.5642304 0.63962364 1.0099988 0.8800593 1.2472185 1.353609
    1.5901572 0.7418409 1.7170814 3.3055406 3.700319 2.1952631 1.5035022
    3.057052 1.4638814 1.0384043 1.1837897 3.0469408 2.803617 3.2614107
    2.4135168 3.556123 2.27436 1.2306889 2.0333362 4.5721135 2.941492 0.8966192
    2.3189502 0.9958008 4.344239 4.3671365 1.2785082 1.952021 2.438866
    4.3924103 4.46023 3.7118216 1.746746 3.3397071 3.9984121 3.4766433 3.165725
    1.8711188 3.2547336 4.482818 3.3188024 2.6877654 2.3054938 2.7741609
    3.796511 3.5835814 2.064381 3.0545063 3.9559705 2.0038147 2.7878509
    3.4762452 1.9495757 2.9715302 3.7937658 2.7762475 2.3023481 2.3882763
    1.5292001 3.1379611 1.5162798 2.4996707 4.362247 3.1643825 2.902113
    1.5552855 3.569274 3.6554635 3.8193665 3.2386634 5.418084 1.488541
    4.5816646 4.1958213 2.411787 2.6187074 3.1729605 2.959683 2.3334894
    5.325289 3.2408857 4.67207 3.0460484 6.0358443 6.879726 3.3280933 5.5901675
    1.8741251 3.5842674 4.855096 6.005389 1.7205821 3.8116035 3.439082 5.024595
    2.205073 4.140361 1.8645307 2.511795 5.744685 2.0451677 2.311025 6.787981
    6.533982 3.840785 2.2128632 6.444055 2.7525501 8.19589 7.3742037 2.5753407
    8.9812355 3.0030684 5.501138 6.7223954 4.8878922 3.2250557 2.3134975
    8.762646 3.072827 7.0158014 7.426256 5.388799 10.0 7.367759 7.078608
    8.373905 9.210589 7.072851 2.7709346 7.233898)


;; Using both a new maximum and new minimum value
(let ((ppcn (make-popcorn '(0.01 0.02) :min-spike 3.0 :max-spike 5.0)))
  (scale ppcn 8.0 5.0))

=> (5.0 5.031736 5.018497 5.017164 5.0183487 5.021274 5.0313935 5.024911
    5.02102 5.0307293 5.0249734 5.030217 5.023649 5.0244017 5.024077 5.027402
    5.0259476 5.0267677 5.0266905 5.026489 5.0243635 5.025302 5.025 5.026827
    5.024325 5.02963 5.02639 5.029601 5.187107 5.0346637 5.1603675 5.177201
    5.041747 5.0779767 5.1244864 5.083996 5.079162 5.179528 5.0662017 5.15569
    5.119997 5.140197 5.08659 5.103155 5.0734825 5.161587 5.1753554 5.0919304
    5.1596823 5.1549683 5.1375275 5.1679344 5.1805143 5.1589727 5.123403
    5.107892 5.134093 5.1509075 5.132244 5.1531157 5.108169 5.1581144 5.127562
    5.125285 5.091505 5.3614798 5.292433 5.146382 5.176299 5.317321 5.1158543
    5.262322 5.3407764 5.2639084 5.2494392 5.3563027 5.277631 5.356573 5.215038
    5.1805325 5.119757 5.1389523 5.123566 5.153178 5.1680784 5.1872272 5.333844
    5.3161135 5.2958627 5.2582746 5.277762 5.2154465 5.304549 5.165927
    5.1745257 5.340159 5.3566494 5.2053585 5.238169 5.3552446 5.2141376
    5.2151394 5.696703 5.323113 5.2542577 5.2507634 5.554174 5.1605134 5.64851
    5.571157 5.300126 5.4663777 5.465809 5.5613008 5.5584745 5.601473 5.533116
    5.259057 5.4692693 5.191887 5.3029995 5.2640176 5.3741655 5.4060826
    5.477047 5.2225523 5.5151243 5.991662 6.110096 5.658579 5.4510508 5.9171157
    5.4391646 5.3115215 5.355137 5.9140825 5.8410854 5.978423 5.7240553
    6.066837 5.682308 5.369207 5.610001 6.3716345 5.8824477 5.2689857 5.6956854
    5.2987404 6.303272 6.310141 5.3835526 5.5856066 5.73166 6.3177233 6.338069
    6.1135464 5.524024 6.001912 6.199524 6.042993 5.9497175 5.5613356 5.9764204
    6.3448453 5.9956408 5.8063297 5.691648 5.832248 6.138953 6.075074 5.619314
    5.916352 6.1867914 5.6011443 5.836355 6.0428734 5.5848727 5.891459
    6.1381297 5.8328743 5.6907043 5.716483 5.4587603 5.9413886 5.454884
    5.7499013 6.3086743 5.9493146 5.870634 5.4665856 6.070782 6.096639 6.14581
    5.971599 6.6254253 5.4465623 6.3744993 6.258746 5.723536 5.785612 5.951888
    5.887905 5.700047 6.5975866 5.9722657 6.401621 5.9138145 6.8107533 7.063918
    5.998428 6.6770506 5.5622377 6.07528 6.4565287 6.8016167 5.516175 6.1434813
    6.0317245 6.5073786 5.661522 6.2421083 5.559359 5.7535386 6.723406 5.61355
    5.6933074 7.0363946 6.9601946 6.1522355 5.663859 6.9332166 5.825765
    7.4587674 7.212261 5.772602 7.694371 5.9009204 6.6503415 7.016719 6.4663677
    5.967517 5.6940494 7.6287937 5.9218483 7.1047406 7.227877 6.61664 8.0
    7.210328 7.123583 7.5121717 7.763177 7.1218557 5.8312807 7.17017)

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

;;; SAR Sat Apr 28 20:09:29 BST 2012: Added robodoc entry

;;; ****m* popcorn/get-kernel
;;; FUNCTION
;;; Generate the next value for the KERNELS slot of a given popcorn object and
;;; change the internal state, with the help of the get-kernel-aux method.
;;;
;;; This method is called automatically from within the heat method.
;;; 
;;; ARGUMENTS
;;; - A popcorn object.
;;; 
;;; RETURN VALUE 
;;; The next value for the given popcorn object's KERNEL slot. 
;;; Returns NIL when the kernel value is > 1.0.
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

