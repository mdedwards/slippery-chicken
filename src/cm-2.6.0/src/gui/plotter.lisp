;;; **********************************************************************
;;; Copyright (C) 2004 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.18 $
;;; $Date: 2005/01/17 18:24:27 $

(in-package :cm)

(defclass plotter (container)
  ((window :accessor plotter-window :initarg :window)
   (menubar :initarg :menubar :accessor plotter-menubar)
   (x-scroller :initarg :x-scroller :accessor plotter-x-scroller)
   (y-scroller :initarg :y-scroller :accessor plotter-y-scroller)
   (drawing-area :initarg :drawing-area 
                 :accessor plotter-drawing-area)
   (bitmap :initarg :bitmap :accessor plotter-bitmap)
   (gc :accessor plotter-gc )
   (x-axis :initarg :x-axis :accessor plotter-x-axis)
   (y-axis :initarg :y-axis :accessor plotter-y-axis)
   (point-class :initarg :point-class :accessor plotter-point-class)
   ;; plots is (({plot}* ) . {focus}))
   (plots :initarg :plots :accessor %plotter-layers
          :initform nil)
   ;; global style merges with individual layer styles
   (displays :initarg :displays :initform nil
             :accessor plotter-displays)
   (selection :initform nil :accessor plotter-selection)
   (mouseinfo :initform #(:select-points nil nil nil nil nil nil 10)
              :accessor plotter-mouseinfo)
   (style :initarg :style :accessor plotter-style )
   (draw :initarg :draw :accessor plotter-draw 
         :initform nil)
   (tools :accessor plotter-tools :initform (make-list 4))
   (inspectors :initform nil :accessor plotter-inspectors)
   (colors :accessor plotter-colors) ;; set in plotting.lisp
   (flags :accessor plotter-flags 
          :initform 0 :initarg :flags)
   ;; :new :replace :overwrite :mix
   (event-layering :accessor plotter-event-layering 
                   :initarg :event-layering :initform :add)
   (cache :accessor plotter-cache)))

;;;
;;; Accessors for the tool windows
;;;

(defun plotter-editing-tool (plotter)
  (first (plotter-tools plotter)))

(defun (setf plotter-editing-tool) (value plotter)
  (setf (first (plotter-tools plotter)) value))

(defun plotter-zooming-tool (plotter)
  (second (plotter-tools plotter)))

(defun (setf plotter-zooming-tool) (value plotter)
  (setf (second (plotter-tools plotter)) value))

(defun plotter-styling-tool (plotter)
  (third (plotter-tools plotter)))

(defun (setf plotter-styling-tool) (value plotter)
  (setf (third (plotter-tools plotter)) value))

;;;
;;; "accessors" for the mouseinfo list
;;;

(defmacro mouseinfo-mode (x)  `(elt ,x 0))
(defmacro mouseinfo-xdown (x) `(elt ,x 1))
(defmacro mouseinfo-ydown (x) `(elt ,x 2))
(defmacro mouseinfo-xmove (x) `(elt ,x 3))
(defmacro mouseinfo-ymove (x) `(elt ,x 4))
(defmacro mouseinfo-track (x) `(elt ,x 5))
(defmacro mouseinfo-class (x) `(elt ,x 6))
(defmacro mouseinfo-apart (x) `(elt ,x 7))
(defun mouseinfo-init (info)
  (dotimes ( i (length info))
    (setf (elt info i) nil))
  (setf (mouseinfo-mode info) ':select-points)
  (setf (mouseinfo-apart info) '10)
  info)

;;;
;;; the %style struct is used by plotters, plots and axis to hold
;;; display/drawing information.  view is one of :envelope, :scatter,
;;; :line, :bar, :box, :bubble, :notation. color is a hex string or
;;; name from rgb.txt. the other elements are self-explanatory.
;;;

(defstruct (%style (:type list))
  view color line-width point-width point-height font)

;;;
;;; the axis object is the main workhorse.
;;;

(defparameter badptr nil)

(defclass axis ()
  ;; type holds a defined 'axis type' (keyword) or nil.
  ((type :initarg :type :accessor axis-type
         :initform nil)
   ;; a title string for the axis, currently not used.
   (title :initarg :title :accessor axis-title
          :initform nil)
   ;; the minimum value displayed along the axis.
   (minimum :initarg :minimum :accessor axis-minimum
            :initform nil)
   ;; the maximum value displayed along the axis.
   (maximum :initarg :maximum :accessor axis-maximum
            :initform nil)
   ;; increment is value at each large tick along the axis
   ;; should probably divide evenly into axis range...
   (increment :initarg :increment :accessor axis-increment
              :initform nil)
   ;; the number of ticks per increment. first tick is
   ;; large and may have an associated value label, all other
   ;; ticks within that increment are small.
   (ticks-per-increment :initarg :ticks-per-increment
                        :accessor axis-ticks-per-increment
                        :initform nil)
   ;; a scaling factor used to determine drawing size.
   (zoom :initarg :zoom :accessor axis-zoom
         :initform nil)
   ;; orientation is either :horizonal or :vertical and is
   ;; set by insure-slots.
   (orientation :initform nil :accessor axis-orientation)
   (slots :initarg :slot :initarg :slots 
          :accessor axis-slots :initform nil)
   ;; labeler is a function or format string for generating axis value
   ;; labels at each increment.
   (labeler :initarg :labeler :accessor axis-labeler
            :initform nil)
   (labeling-threshold :initarg :labeling-threshold
                       :accessor axis-labeling-threshold
                       :initform nil)
   ;; the width or height of the axis areas to the left or below
   ;; the plotting offset
   (label-area :initarg :pixels-per-label
                     :accessor axis-label-area
                     :initform nil)
   ;; pixel offset of the axis origin, ie the position of the :minumum
   ;; axis value on the screen.
   (offset :initarg :offset :accessor axis-offset
           :initform nil)
   ;; width (in pixels) of :increment at :zoom 1. defaults to 60 since
   ;; that makes it evenly divisible by many so tick divisions: 4 5 6
   ;; 10 12 etc.
   (pixels-per-increment :initarg :pixels-per-increment
                         :accessor axis-pixels-per-increment
                         :initform nil)))
 
;;;
;;; various "accessors" of axis information, mostly used for
;;; converting between pixel and axis coodinates.  pixel routines
;;; return floats since gtk zooming value is a double.
;;;

(defgeneric axis-range (axis))
(defmethod axis-range ((axis axis))
  ;; range is difference betweeen max and min
  (- (axis-maximum axis) (axis-minimum axis)))

(defgeneric axis-increments (axis))
(defmethod axis-increments ((axis axis))
  ;; number of incremnts is range/inc
  (/ (axis-range axis)
     (axis-increment axis)))

(defgeneric axis-increment-size (axis))
(defmethod axis-increment-size ((axis axis))
  ;; return floating point size of increment in
  ;; pixels at current zoom
  (* (axis-pixels-per-increment axis)
     (axis-zoom axis)))

(defun axis-tick-size (axis)
  (/ (axis-increment-size axis)
     (axis-ticks-per-increment axis)))

(defun axis-draw-labels? (axis)
  (let ((thresh (or (axis-labeling-threshold axis) 0))
        (size 0))
    (if (consp thresh)
      (ecase (axis-orientation axis) 
        (( :horizontal ) (setq size (or (first thresh) 0)))
        (( :vertical ) (setq size (or (second thresh) 0))))
      (setq size (or thresh 0)))
    (< size (axis-increment-size axis))))

;;;
;;; axis-size returns total number of pixels along axis at current
;;; zoom
;;;

(defgeneric axis-size (axis))
(defmethod axis-size ((axis axis))
  ;; screen size of axis in pixels at current zoom
  ;; increments may not be integer...
  (* (axis-increment-size axis)
     (axis-increments axis)))

;;; pixel->axis conversion

(defgeneric axis-value (pixel axis &optional vertical?))
(defmethod axis-value (pixel (axis axis) &optional vertical?)
  ;; convert screen position to axis value
  (let ((val (if vertical?
               (- (axis-offset axis) pixel)
               (- pixel (axis-offset axis)))))
    (+ (axis-minimum axis)
       (* (axis-range axis)
          (/ val (axis-size axis))))))

;;; 
;;; axis-pixel converts a value from axis to pixel coordinates.
;;; return value can be float!
;;;

(defgeneric axis-pixel (val axis &optional vertical? nooff?))
(defmethod axis-pixel (val (axis axis) &optional vertical? nooff?)
  ;; convert axis value to screen pixel. if noshift is true then
  ;; offsetting is ignored (for width and height values)
  (let ((pos (* (axis-size axis)
                (/ (- val (if nooff? 0 (axis-minimum axis)))
                   (axis-range axis)))))
    (if nooff?
      pos
      (if vertical?
        (- (axis-offset axis) pos)
        (+ (axis-offset axis) pos)))))

;;;
;;; log-axis. :increment is a base
;;;

(defclass log-axis (axis)
  ((increment :initarg :base :accessor axis-base
              :initform nil)))

(defgeneric axis-value-at-increment (axis i j))
(defmethod axis-value-at-increment ((axis axis) i j)
  ;; return axis value at i'th increment and j'th tick
  (let* ((inc (axis-increment axis))
         (val (+ (axis-minimum axis) (* inc i))))
    (if (= j 0)
      val
      (+ val (* inc (/ j (axis-ticks-per-increment axis)))))))

(defmethod axis-value-at-increment ((axis log-axis) i j)
  ;; return axis value at i'th increment and j'th tick
  (let* ((base (axis-base axis))
         (val (* (axis-minimum axis)
                 (expt (axis-increment axis) i))))
    (if (= j 0)
      val
      (* val (expt base (/ j (axis-ticks-per-increment axis)))))))
 
; (setf x (axis :normalized ))
; (loop for i below 10 collect (axis-value-at-increment x i))
; (setq x (make-instance 'log-axis :minimum 55 :maximum 56320 :increment 2 :ticks-per-increment 12))
; (loop for i below 10 collect (axis-increment-value x i))
; (setq x (make-instance 'log-axis :minimum 55 :maximum 56320 :increment 10 :ticks-per-increment 10))
; (loop for i below 10 collect (axis-increment-value x i))
; (loop for i below 10 collect (* 55 (expt 10 i)))

(defmethod axis-range ((axis log-axis))
  ;; range is exponents
  (log (/ (axis-maximum axis)
          (axis-minimum axis))
       (axis-base axis)))

(defmethod axis-increments ((axis log-axis))
  ;; num increments are same as range.
  (axis-range axis))

(defmethod axis-pixel (val (axis log-axis) &optional vertical? nooff?)
  ;; convert val or ratio of exponent to exponent ragen, then offset
  ;; and scale for pixel.
  (let* ((min (axis-minimum axis))
         (rng (axis-range axis))
         (exp (log (/ val min)
                   (axis-base axis)))
         (pos (* (axis-size axis)
                 (/ exp rng))))
    (if nooff?
      pos
      (if vertical?
        (- (axis-offset axis) pos)
        (+ (axis-offset axis) pos)))))

;;;
;;; axis-value converts pixels to axis values.
;;;

(defmethod axis-value (pixel (axis log-axis) &optional vertical?)
  ;; convert axis value to screen position
  vertical?
  (* (axis-minimum axis)
     (expt (axis-base axis)
           (* (axis-range axis)
              (/ (if vertical? 
                   (- (axis-offset axis) pixel)
                   (- pixel (axis-offset axis)))
                 (axis-size axis))))))

;;;
;;; pixel values
;;;

(defmacro pixel (x)
  `(FLOOR ,x))

;;; xpixel and ypixel are wrappers that convert slot values to pixel
;;; positions and sizes. but calling axis-pixel on each point involves
;;; lots of axis slot references -- these wrappers should be optimized
;;; at some point.

(defmacro xpixel (instance slot axis &optional rel?)
  `(FLOOR (axis-pixel (slot-value ,instance ,slot) ,axis nil ,rel?)))

(defmacro ypixel (instance slot axis &optional rel?)
  `(FLOOR (axis-pixel (slot-value ,instance ,slot) ,axis t ,rel?)))

;;;
;;; plotting slots
;;;

(defun axis-plotting-slots-for-layer (axis layer)
  layer
  (axis-slots axis))

;;(defun axis-plotting-slots-for-layer (axis layer)
;;  ;; returns the slot(s) to access for plotting the specified layer.
;;  (loop for e in (axis-plotting-slots axis)
;;        if (or (eq (car e) t)
;;               (eq (car e) layer))
;;        do (return (cdr e))))

(defmacro with-display-slots ((axis layer &optional a b)
                              &body body)
  (let ((slots (gensym))
        (binds (list)))
    (push (list slots `(axis-plotting-slots-for-layer
                        ,axis ,layer))
          binds)
    (if a (push (list a `(car ,slots)) binds))
    (if b (push (list b `(cadr ,slots)) binds))
    `(let* ,(nreverse binds)
      ,@body)))

;(defmacro with-layer-slots ((plotter layer &optional x y w h)
;                            &body body)
;  (let (xaxis yaxis xslots yslots binds)
;    (when (or x w)
;      (setf xaxis (gensym))
;      (setf xslots (gensym))
;      (push (list xaxis `(plotter-x-axis ,plotter)) binds)
;      (push (list xslots `(axis-plotting-slots-for-layer
;                           ,xaxis ,layer))
;            binds)
;      (if x (push (list x `(car ,xslots)) binds))
;      (if w (push (list y `(cadr ,xslots)) binds)))
;    (when (or y h)
;      (setf yaxis (gensym))
;      (setf yslots (gensym))
;      (push (list yaxis `(plotter-y-axis ,plotter)) binds)
;      (push (list yslots `(axis-plotting-slots-for-layer
;                           ,yaxis ,layer))
;            binds)
;      (if y (push (list y `(car ,yslots)) binds))
;      (if h (push (list h `(cadr ,yslots)) binds)))
;    `(let* ,(nreverse binds)
;      ,@body)))

;;;
;;; axis prototypes and defaxis
;;;
;;; an axis prototype is an allocated axis instance that serves as a
;;; template for creating common axis configurations. Prototypes also
;;; maintain some "meta-information" abount their axis "type", such a
;;; list of common slots to which the axis might be attached. im using
;;; prototypes rather than subclasses because subclasssing doesnt
;;; really help (since most axis values cannot be determined at at
;;; make-instance time) and there is no need to perform run-time
;;; method dispatching based on prototype values.
;;;

(defparameter *axis-prototypes* (make-hash-table :size 32))

(defun set-axis-prototype (axis &optional info)
  (setf (gethash (axis-type axis) *axis-prototypes*)
        (cons axis info)))

(defun find-prototype (type &optional (error t))
  ;; return axis prototype for axis type 
  (or (first (gethash type *axis-prototypes*))
      (if error (error "No prototype axis for ~S." type)
          nil)))

(defun prototype-info (proto key)
  ;; return axis "metainfo" for prototype and info key.  for
  ;; convenience proto can be a type symbol.
  (let ((entry (gethash (if (symbolp proto) proto (axis-type proto))
                        *axis-prototypes*)))
    (cdr (assoc key (cdr entry)))))


(defun pfloor (val &optional force (base 10))
  ;; returns closest power of base <= than value. if the value and the
  ;; lesser power are identical then force T returns one power less.
  (if (= val 0)
    (values 0 0)
    (let* ((exp (ceiling (log val base)))
           (top (expt base exp))
           (low (expt base (1- exp))))
      (if (and (not force)
               (= val top))
        (values top exp)
        (values low (1- exp))))))

(defun pceiling (val &optional force (power 10))
  ;; return closest power value >= than value. if = then force t
  ;; returns greater value.
  (if (= val 0)
    (values 1 0)
    (let* ((exp (floor (log val power)))
           (low (expt power exp))
           (top (expt power (1+ exp))))
      (if (and (not force)
               (= val low))
        (values low exp)
        (values top (1+ exp)))))) ;(* low (ceiling val low))

;;;
;;; insure-axis insures that all non-display axis properties are fully
;;; established before attempting to display any data.
;;;

(defun insure-axis (spec orientation pc slot layers)
  ;; the workhorse. spec is the users axis description, either nil, a
  ;; type keyword or an axis object, orientation is either :horizontal
  ;; or :vertical, pc is the user specified point class (or <point> if
  ;; none was specified), layers are all the layers passed into the
  ;; plotter, and slot is a slot the user specified or nil.
  (let (type axis proto insurer datamin datamax)
    (cond ((symbolp spec) ; also catches NIL type
           (setq type spec)
           (setq axis (make-instance 'axis :type type)))
          ;((consp type)
          ; (setq proto (find-prototype (car type)))
          ; (setq axis (apply #'make-instance (class-of proto)
          ;                   :type (axis-type proto) (cdr type))))
          ((typep spec 'axis)
           (setq axis spec)
           (setq type (axis-type axis)))
          (t
           (error "~S is not an axis or axis type." spec)))
    (setq proto (or (find-prototype type nil)
                    (error "No prototype for axis type ~S."
                           type)))
    ;; axis orientation is set here, its not an init
    (setf (axis-orientation axis) orientation)
    ;; first insure axis plotting slots since these may be needed to
    ;; access layer values to insure axis minumum and maximum values.
    (when layers
      (insure-axis-plotting-slots axis proto pc slot layers))
    ;; if the prototype has it own insurer, call it  without dataargs
    (setq insurer (prototype-info type ':insurer))
    (when insurer
      (funcall (car insurer) axis proto nil nil))
    ;; ...then insure all afterwards. 
    ;; this is gross and i wonder if i still need it.
    (insure-slots axis proto
                  '(title minimum maximum
                    increment ticks-per-increment
                    pixels-per-increment labeler labeling-threshold))
    ;; values that were not explicitly supplied by user or
    ;; prototype are determined now. if axis min or max was
    ;; not specified then grovel through data and find them.
    (unless (and (axis-maximum axis)
                 (axis-minimum axis))
      (dolist (p layers)
        (multiple-value-setq 
         (datamin datamax)
         (plotting-bounds (layer-data p )
                          (car (axis-plotting-slots-for-layer axis p))
                          (eq orientation ':horizontal)
                          datamin datamax)))
      ;; 0-1 if no layer data 
      (if (not datamin) (setq datamin 0))
      (if (not datamax) (setq datamax 1))
      (if insurer
        (funcall (car insurer) axis proto datamin datamax)        
        (progn
          (unless (axis-minimum axis)
            (setf (axis-minimum axis)
                  (if (< datamin 0) (- (pceiling (abs datamin)))
                      0)))
          (unless (axis-maximum axis)
            (setf (axis-maximum axis)
                  (pceiling datamax))))))
    (if (not (axis-increment axis))
      (setf (axis-increment axis)
            (pfloor (axis-maximum axis) t)))
    ;; a few simple error checks...
    (unless (> (axis-increment axis) 0)
      (error "Axis :increment ~S not greater than zero."
             (axis-increment axis))      )
    (unless (>= (axis-maximum axis) (axis-increment axis))
      (error "Axis :maximum ~S less than :increment ~S."
             (axis-maximum axis) (axis-increment axis)))
    (unless (> (axis-maximum axis) (axis-minimum axis))
      (error "Axis :maximum ~S less than :minumum ~S."
             (axis-maximum axis) (axis-minimum axis)))
    ;(unless (= 0 (rem (axis-range axis) (axis-increment axis)))
    ;  (error "Axis range (~S) not multiple of :increment ~S."
    ;         (axis-range axis) (axis-increment axis)))
    axis))

(defun insure-slots (to from slots)
  (dolist (s slots)
    (unless (and (slot-boundp to s)
                 (slot-value to s))
      (setf (slot-value to s)
            (slot-value from s)))))

(defun insure-keynum-axis (axis proto datamin datamax)
  (unless (axis-minimum axis)
    (setf (axis-minimum axis)
          (if datamin
            (* 12 (floor datamin 12))
            (axis-minimum proto))))
  (unless (axis-maximum axis)
    (setf (axis-maximum axis)
          (if datamax
            (* 12 (ceiling datamax 12))
            (axis-maximum proto)))))

(defun insure-seconds-axis (axis proto datamin datamax)
  proto datamin
  (unless (axis-minimum axis)
    (setf (axis-minimum axis) 0 ))
  (when datamax
    (unless (axis-maximum axis)
      (setf (axis-maximum axis)
            (if (> datamax 60)
              (* 60 (ceiling (/ datamax 60)))
              (if (> datamax 10)
                (* 10 (ceiling (/ datamax 10)))
                (ceiling datamax)))))))
        
(defun axis-appropriate-slot (axis layer pc)
  ;; try to find a slot to display for a given layer because the user
  ;; didnt provide an axis :slot value.  check the first element in
  ;; the data, if its an object use it to determine the slots, else if
  ;; its a number use the plotter's point-class.
  (let* ((one (car (layer-data layer))))
    (if one
      (let* ((class (if (numberp one) pc
                        (class-of one)))
             (type (axis-type axis))
             ;; fetch possible :vertical or :horizontal slots cached
             ;; in the prototype's :appropriate-slots info.
             (try (getf (prototype-info type ':appropriate-slots) 
                        (axis-orientation axis)))
             (slots (class-slots class)))
        (or 
         (loop for s in slots
               for n = (slot-definition-name s)
               when (find n try)
               return n)
         (error "Missing a :slot in the ~@{~A~} axis."
                (axis-orientation axis))))
      nil)))

(defun insure-axis-plotting-slots (axis proto pc slot layers)
  ;; see axis defclass documentation on 'plotting-slots' slot.
  (let ((slots (or (axis-slots axis)
                   slot
                   (axis-slots proto))))
    (cond ((consp slots)
           ;; its ({slot1} {slot1}*)
           )
          ((null slots)
           ;; user did not specify any layering slots. if there is
           ;; layer data then either pick a reasonable slot or signal
           ;; an error. if there is no data then dont signal and error
           ;; but will be disabled until :slots is set.
           ;; iterate all layers and get an appropriate slot
           ;; to layer according to the axis type, axis orientation
           ;; and plotting data.
           (loop for p in layers
              for s = (axis-appropriate-slot axis p pc)
              when s
              do (progn (setf (axis-slots axis)
                              (list s))
                        (return))))
          (t
           (setf (axis-slots axis)
                 (list slots))))))

;;;
;;; layers associate data with plotting color and style.
;;;

(defclass layer ()
  ((data :initarg :data :initform nil :accessor %layer-data)
   (name :initarg :name :accessor layer-name
         :initform nil)
   (style :initarg :style :accessor layer-style :initform nil)))

(defmethod print-object ((obj layer) stream)
  (let ((name (layer-name obj)))
    (if name (format stream "#<layer ~S>" name)
        (call-next-method))))

(defgeneric layer-data (layer))
(defgeneric (setf layer-data) (value layer))

(defmethod layer-data ((data layer))
  (let ((x (slot-value data 'data)))
    (if (listp x)
      x
      (subobjects x))))

(defmethod (setf layer-data) (value (layer layer))
  (let ((x (slot-value layer 'data)))
    (if (listp x)
      (setf (%layer-data layer) value)
      (setf (subobjects x) value))))

(defun insure-layer (plotter layer x-axis y-axis pc)
  (let ((data (layer-data layer)))
    (unless (layer-name layer)
      (let ((n (1+ (length (plotter-layers plotter)))))
        (setf (layer-name layer)
              (format nil "Layer ~D" n))))
    (unless (layer-style layer)
      (setf (layer-style layer) (make-%style)))
    ;; if we have three slots and no view then set default view of
    ;; layer to :box
    (unless (%style-view (layer-style layer))
      (when (or (cdr (axis-plotting-slots-for-layer x-axis layer))
                (cdr (axis-plotting-slots-for-layer y-axis layer)))
        (setf (%style-view (layer-style layer)) ':box)))
    (insure-style plotter (layer-style layer))
    ;; convert numeric values to point objects of pc class
    (when (and data (numberp (car data)))
      (let ((xs (CAR (axis-plotting-slots-for-layer x-axis layer)))
            (ys (CAR (axis-plotting-slots-for-layer y-axis layer))))
        (setf (%layer-data layer) 
              (loop for (x y) on data by #'cddr
                 for i = (make-instance pc)
                 do (setf (slot-value i xs) x)
                 (setf (slot-value i ys) y)
                 collect i))))))

(defun plotting-bounds (data slot ordered? lo hi)
  ;; grovel through layer data and return min and max values for one
  ;; axis. its really inefficient if both axes have do this since each
  ;; layer will be traversed twice (once for each axis).  but if the
  ;; user doesnt use a prototype and/or doesnt specify axis bounds
  ;; perhaps they deserve to wait...
  (let (it)
    (if data
      ;; if its the ordered (x) axis dont grovel so much
      (if ordered?
        (cond ((numberp (car data))
               ;; user passed in numbers not points, return first and
               ;; last X values from the list.
               (setq it (car data))
               (if (or (not lo) (< it lo)) (setq lo it))
               (setq it (loop for tail on data by #'cddr
                              when (null (cddr tail))
                              do (return (car tail))))
               (if (or (not hi) (> it hi)) (setq hi it)))
              (t
               (setq it (slot-value (first data) slot))
               (if (or (not lo) (< it lo)) (setq lo it))
               (setq it (slot-value (car (last data)) slot))
               (if (or (not hi) (> it hi)) (setq hi it))))
        (cond ((numberp (car data))
               ;; search Y values in xy list
               (loop for y in (cdr data) by #'cddr do
                     (if (or (not lo) (< y lo)) (setq lo y))
                     (if (or (not hi) (> y hi)) (setq hi y))))
              (t
               (loop for d in data
                     for v = (slot-value d slot) do
                     (if (or (not lo) (< v lo)) (setq lo v))
                     (if (or (not hi) (> v hi)) (setq hi v)))))))
    (values lo hi)))

(defun ordered-layer? (layer x-slot)
  ;; return t if layer is in increasing order
  (let ((l most-negative-fixnum))
    (loop for p in (layer-data layer)
          for x = (slot-value p x-slot)
          never (< x l) do (setq l x))))

(defun sort-layer (layer x-slot)
  ;; sort data if its out of otder
  (setf (layer-data layer)
        (sort (layer-data layer)
              (lambda (a b)
                (< (slot-value a x-slot)
                   (slot-value b x-slot)))))
  layer)

;;;
;;; selections maintain state related to points/region editing
;;;

(defclass selection ()
  ((type :initform nil :initarg :type :accessor selection-type)
   (points :initform nil :initarg :points :accessor selection-points)
   (layer :initform nil :initarg :layer :accessor selection-layer)
   (region :initform nil :initarg :region :accessor selection-region)
   (all :initform nil :initarg :all :accessor selection-all?)))

(defun selection-type? (sel typ)
  (eql (selection-type sel) typ))

;;;
;;; basic point class for drawing
;;;

(defclass point ()
  ((x :initarg :x :accessor point-x
      :initform nil)
   (y :initarg :y :accessor point-y
      :initform nil)
   (z :initarg :z :accessor point-z
      :initform nil)))

(defmethod print-object ((obj point) stream)
  (format stream "#<~(~A~) ~S ~S~@[ ~S~]>"
          (class-name (class-of obj))
          (point-x obj)
          (point-y obj)
          (point-z obj)))

;;;
;;; Plotter API...
;;; 

(defconstant +show-grid+        (expt 2 0))
(defconstant +show-back-layers+ (expt 2 1))
(defconstant +show-guide+       (expt 2 2))
(defconstant +zoom-points+      (expt 2 3))
(defconstant +zoom-lines+       (expt 2 4))
(defparameter *plotter-flags*   (logior +show-grid+ 
                                        +show-back-layers+
                                        +show-guide+
                                        +zoom-points+))

(defun plotter (&rest args &aux (inits (list))
                (layers (list))
                (keys '(:title :name :x-axis :y-axis
                        :point-class :zoom
                        :x-slot :y-slot
                        :view :color :event-layering
                        :point-width :point-height :point-size
                        :line-width :font :flags
                        :no-window :layers))
                x-axis y-axis style plotter pc zoom flags name
                (xy (find-class 'point)))
  ;; HACK: let an existing plotter get redisplayed.
  (when (and (eql (length args) 1)
             (typep (car args) 'plotter))
    (warn "Use PLOTTER-OPEN to display an existing plotter because PLOTTER won't support this soon.")
    (plotter-open (car args) )
    (return-from plotter (car args)))
  ;; parse user's args into plotter inits and plotting data
  (do ((tail args))
      ((null tail)
       (setf inits (nreverse inits))
       (setf layers (nreverse layers)))
    (cond ((keywordp (car tail))
           (unless (find (car tail) keys)
             (error "'~S' is not a  plotter init. Legal inits are ~S."
                    (car tail) keys))
           (push (pop tail) inits)
           (if (null tail)
             (error "Missing value for plotter init ~s."
                    (car keys))
             (push (pop tail) inits)))
          (t (push (pop tail) layers))))
  ;; ensure that we have a unique name
  (when (setq name (or (getf inits ':name) (getf inits :title)))
    (if (find-object name nil)
      (setq name
            (loop for i from 2
               for s = (format nil "~a ~d" name 2)
               unless (find-object s nil) return s))))
  ;; point data may be specified "inline" or passed as the value of
  ;; the :layers arg.
  (setf layers (or layers (copy-list (getf inits ':layer))))
  ;; point data can be layers, lists or seqs.  insure all are layers and
  ;; fill in any global/local style info the user passed to plotter.
  (let ((view (getf inits ':view))
        (color (getf inits ':color))
        (linew (getf inits ':line-width))
        (pointw (or (getf inits ':point-width)
                    (getf inits ':point-size)))
        (pointh (or (getf inits ':point-height)
                    (getf inits ':point-size)))
        (font (getf inits ':font)))
    ;; atomic style elements (including nil) are global. lists are
    ;; spread across layers. nil global elements will receive default
    ;; values later by insure-style
    (setf style (make-%style :view (and (atom view) view)
                             :color (and (atom color) color)
                             :line-width (and (atom linew) linew)
                             :point-width (and (atom pointw) pointw)
                             :point-height (and (atom pointh) pointh)
                             :font (and (atom font) font)))
    (do ((tail layers (cdr tail)))
        ((null tail) nil)
      (unless (typep (car tail) 'layer)
        (setf (car tail) 
              (make-instance 'layer :data (car tail))))
      (unless (layer-style (car tail))
        (setf (layer-style (car tail))
              (make-%style :view (and (consp view) (pop view) )
                           :color (and (consp color) (pop color))
                           :line-width (and (consp linew) (pop linew))
                           :point-width (and (consp pointw)
                                             (pop pointw))
                           :point-height
                           (and (consp pointh) (pop pointh)) 
                           :font (and (consp font) (pop font)))))))
  (setq pc (getf inits ':point-class))
  ;; axis specs can be axis objects, axis types, lists or nil.
  (setq x-axis (insure-axis (getf inits ':x-axis)
                            ':horizontal
                            (or pc xy)
                            (getf inits ':x-slot)
                            layers))
  (setq y-axis (insure-axis (getf inits ':y-axis) 
                            ':vertical
                            (or pc xy)
                            (getf inits ':y-slot)
                            layers))
  ;; allow a single :zoom value to set both axes
  (setq zoom (getf inits ':zoom))
  (when zoom
    (setf (axis-zoom x-axis) zoom)
    (setf (axis-zoom y-axis) zoom))
  (setq flags (getf inits :flags *plotter-flags*))
  (setq plotter
        (make-instance 'plotter
                       :name name
                       :point-class pc
                       :event-layering
                       (getf inits ':event-layering ':add)
                       :style style
                       :x-axis x-axis
                       :y-axis y-axis
                       :flags flags))
  ;; insure values for all the global style elements.
  (insure-style plotter (plotter-style plotter))
  ;; add each layer in order, first layer is front plot. :redraw is
  ;; nil because we dont have a window yet.
  (when layers
    (dolist (p layers)
      (plotter-add-layer plotter p :redraw nil))
    (plotter-front-layer plotter :layer (car layers)
                         :redraw nil))
  ;; create the window 
  (if (getf inits ':no-window)
    plotter
    (if (gtk-running?)
      (progn ;;(format t "GTK already running, open window from inside GTK.")
        #+OPENMCL (gtk-call #'(lambda () (plotter-open plotter) plotter))
        #-OPENMCL (plotter-open plotter)
        plotter)
      (plotter-open plotter))))

;;;
;;; Plotter layer accessing.
;;;

(defun plotter-open (plotter)
  #+darwin (if (darwin-x11-running? ) (display-plotter plotter))
  #-darwin (display-plotter plotter)
  plotter)

(defun plotter-layers (plotter)
  (car (%plotter-layers plotter)))

(defun plotter-front-layer (plotter &key layer (redraw t))
  (when layer 
    (let ((new (if (typep layer 'layer) layer
                   (plotter-find-layer plotter layer))))
      (unless new
        (error "Layer ~S not in plotter ~S." layer plotter))
      (setf (cdr (%plotter-layers plotter)) new)
      (when redraw (plotter-redraw plotter redraw))))
  (cdr (%plotter-layers plotter)))

(defun plotter-find-layer (plotter id)
  (cond ((eql id ':front)
         (cdr (%plotter-layers plotter)))
        ((numberp id)
         (elt (car (%plotter-layers plotter)) id))
        (t
         (find id (car (%plotter-layers plotter))
               :test #'equal :key #'layer-name))))

(defun plotter-add-layer (plotter layer &key (redraw t))
  (unless (typep layer 'layer)
    (setf layer (make-instance 'layer :data layer)))
  (unless (plotter-layers plotter)
    (insure-axis (plotter-x-axis plotter)
                 ':horizontal (plotter-point-class plotter)
                 nil (list layer))
    (insure-axis (plotter-y-axis plotter)
                 ':vertical (plotter-point-class plotter)
                 nil (list layer)))
  ;; insure layer before adding to plotter
  (insure-layer plotter layer (plotter-x-axis plotter)
               (plotter-y-axis plotter)
               (or (plotter-point-class plotter)
                   (find-class 'point)))
  (let ((layers (%plotter-layers plotter)))
    (cond ((or (null layers)
               (null (car layers)))
           (setf (%plotter-layers plotter)
                 (cons (list layer) layer)))
          ;; "readding" a previously empty layer
          ((find layer (car layers))
           (setf (cdr layers) layer))
          (t
           (setf (%plotter-layers plotter)
                 (cons (append (car layers) (list layer))
                       layer)))))
  (when redraw
    (plotter-redraw plotter))
  layer)

(defun plotter-set-axis-values (plotter &key x-axis y-axis (redraw t))
  ;; set axis to inits in data, ignoring bogus names in data.
  (flet ((setoneaxis (axis data)
           (unless (evenp (length data))
             (let ((proto (find-prototype (car data))))
               (when proto
                 (unless (eql (class-of proto) (class-of axis))
                   (change-class axis (class-of proto)))
                 (setf (axis-type axis) (car data)))
               (pop data)))
           (unless (null data)
             (multiple-value-bind (init args)
                 (expand-inits (class-of axis) data nil t)
               args
               (dopairs (s v init) (setf (slot-value axis s) v))))))
    (when x-axis
      (setoneaxis (plotter-x-axis plotter) x-axis))
    (when y-axis
      (setoneaxis (plotter-y-axis plotter) y-axis))
    (when (and redraw (plotter-window? plotter))
      (insure-drawing-sizes (plotter-drawing-area plotter)
                            (plotter-x-axis plotter)
                            (plotter-y-axis plotter)))
    (when redraw (plotter-redraw plotter))
    plotter))

(defun plotter-window? (plotter)
  (and (slot-boundp plotter 'window)
       (not (null (plotter-window plotter)))))

;;;
;;; styling functions. these functions all merge with the "global"
;;; style defined in the plotter object.
;;;

(defparameter *plotting-views*
  '(:line-and-point :line :point :bar :bar-and-point
    :box :bubble :notation))
(defparameter *default-view* ':line-and-point)
(defparameter *default-colors*
  '("dark red" "dark green" "dark blue" "dark turquoise"
    "dark goldenrod" "dark orange" "purple" "brown"))
(defparameter *default-line-width* 1)
(defparameter *default-point-width* 8)
(defparameter *default-point-height* 8)
(defparameter *default-font* "Sans 10")

(defun styling-view (style &optional global)
  (or (%style-view style) (%style-view global)))

(defun styling-color (style &optional global)
  (or (%style-color style) (%style-color global)))

(defun styling-line-width (style &optional global)
  (or (%style-line-width style) (%style-line-width global)))

(defun styling-point-width (style &optional global)
  (or (%style-point-width style) (%style-point-width global)))

(defun styling-point-height (style &optional global)
  (or (%style-point-height style) (%style-point-height global)))

(defun styling-font (style &optional global)
  (or (%style-font style) (%style-font global)))

(defun insure-style (plotter style 
                             &aux (global (plotter-style plotter)))
  ;; insure that styling properties can be retrieved from either the
  ;; style or the global style. if both are nil then use default value.
  ;; this function is also called to insure the gobal style itself.
  (unless (styling-view style global)
    (setf (%style-view style) *default-view*))
  (unless (styling-color style global)
    (unless (eq style global)
                                        ;(%style-color style)
      ;; claim first unused default color
      (flet ((some-layer-uses (color layers)
               (loop for p in layers
                     thereis
                     (equal color (%style-color (layer-style p))))))
        (setf (%style-color style)
              (loop with layers = (plotter-layers plotter)
                    for c in *default-colors*
                    unless (some-layer-uses c layers)
                    do (return c)
                    finally (return (first *default-colors*)))))))
  (unless (styling-line-width style global)
    (setf (%style-line-width style)
          *default-line-width*))
  (unless (styling-point-width style global)
    (setf (%style-point-width style)
          *default-point-width*))
  (unless (styling-point-height style global)
    (setf (%style-point-height style)
          *default-point-height*))
  (unless (styling-font style global)
    (setf (%style-font style) *default-font*))
  style)

;;;
;;;
;;;

(defun plotting-point-width (plotter layer)
  (if (plotter-property plotter :zoom-points)
    (* (styling-point-width (layer-style layer)
                            (plotter-style plotter))
       (axis-zoom (plotter-x-axis plotter)))
    (styling-point-width (layer-style layer)
                         (plotter-style plotter))))

(defun plotting-point-height (plotter layer)
  (if (plotter-property plotter :zoom-points)
    (* (styling-point-height (layer-style layer)
                             (plotter-style plotter))
       (axis-zoom (plotter-y-axis plotter)))
    (styling-point-height (layer-style layer)
                          (plotter-style plotter))))

;;;
;;; plotter-front-styling gets/sets style elements of
;;; the front layer.
;;;

(defun plotter-front-styling (plotter &key (view nil vp)
                                      (color nil cp)
                                      (point-width nil wp)
                                      (point-height nil hp)
                                      (line-width nil lp)
                                      (font nil fp) (redraw t)
                                      (error t))
  (declare (ignore  font))
  (let ((layer (plotter-front-layer plotter)))
    (if layer
      (let ((style (layer-style layer)))
        ;; only attempt to restyle if any values passed in
        (if (or vp cp wp hp lp fp)
          (let ((reset nil))
            ;; maybe change view
            (when (and vp (not (eql view (%style-view style))))
              (unless (or (not error) (null view)
                          (member view *plotting-views*))
                (error "~S is not a plotting view. Legal views are ~S."
                       view *plotting-views*))
              (setf (%style-view style) view)
              (setf reset t))
            ;; maybe change color, which cannot be nil
            (when (and cp (not (eql color (%style-color style))))
              (unless (or (not error) (stringp color))
                (error "Color not a string name or hex value: ~S."
                       color))
              (setf (%style-color style) color)
              (setf reset t))
            ;; maybe change point-width
            (when (and wp 
                       (not (eql point-width (%style-point-width style))))
              (unless (or (not error) (null point-width)
                          (>= point-width 0))
                (error "Point width not a positive number: ~S."
                       point-width))
              (setf (%style-point-width style) point-width)
              (setf reset t))
            ;; maybe change point-width
            (when (and hp (not (eql point-height
                                    (%style-point-height style))))
              (unless (or (not error) (null point-height)
                          (>= point-height 0))
                (error "Point height not a positive number: ~S."
                       point-height))
              (setf (%style-point-height style) point-height)
              (setf reset t))
            ;; maybe change line-width
            (when (and lp
                       (not (eql line-width (%style-line-width style))))
              (unless (or (not error) (null line-width) (>= line-width 0))
                (error "Line size not a positive integer: ~S."
                       line-width))
              (setf (%style-line-width style) line-width)
              (setf reset t))
            ;; only redraw if value changed or :force is specified.
            (when (or (and reset redraw)
                      (eq redraw ':force))
              (plotter-redraw plotter redraw))
            (values style reset))
          (values style nil)))
      (values nil nil))))

;;;
;;; Plotter preferences accessing
;;;

(defun plotter-property (plotter pref &rest args)
  (let* ((vp (not (null args)))
         (val (if vp (car args)))
         flag tog?)
    (ecase pref
      (:show-grid
       (multiple-value-setq (flag tog?)
         (plotter-flag plotter +show-grid+ vp val)))
      (:show-back-layers
       (multiple-value-setq (flag tog?)
         (plotter-flag plotter +show-back-layers+ vp val)))
      (:show-guide
       (multiple-value-setq (flag tog?)
         (plotter-flag plotter +show-guide+ vp val)))
      (:zoom-points
       (multiple-value-setq (flag tog?)
         (plotter-flag plotter +zoom-points+ vp val)))
      (:zoom-lines
       (multiple-value-setq (flag tog?)
         (plotter-flag plotter +zoom-lines+ vp val)))
      )
    (if (and tog? (getf (cdr args) ':redraw t))
      (plotter-redraw plotter t))
    flag))

(defun plotter-flag (plotter flag set? bool)
  ;; return two values: the state of flag and whether or not its
  ;; current state actually changed to the new value bool.
  (unless (or (eql bool t)(eql bool nil))
    (error "Value not boolean true or false: ~S" 
           bool))
  (let* ((bits (plotter-flags plotter))
         (test (logtest bits flag)))
    (if (not set?)
      (values test nil)
      (if (eql bool test)
        (values test nil)
        (progn
          (setf (plotter-flags plotter)
                (if bool                  
                  (logior bits flag)
                  (logandc2 bits flag)))
          (values bool t))))))

(defun plotter-data (plotter &key layer (xy nil xyp)
                     (x-decimals t) (y-decimals t))
  (let ((out ())
        (one nil)
        (res nil))
    (cond ((or (member layer '(:front :front-selection)))
           (setq out (list (plotter-front-layer plotter))))
          ((null layer)
           (setq out (plotter-layers plotter)))
          (t (setq out (list (plotter-find-layer plotter layer)))))
    (flet ((floorthem (data)
             (do ((tail data (cddr tail)))
                 ((null tail) nil)
               (setf (car tail) (floor (car tail)))))
           (quantthem (data n)
             (let* ((num1 (expt 10 n))
                    (num2 (coerce num1 'single-float)))
               (do ((tail data (cddr tail)))
                   ((null tail) nil)
                 (setf (car tail)
                       (/ (floor (* (car taiL) num1)) num2)))))
           (convert-xy (layer data x-axis y-axis)
             (let ((xslot (car
                           (axis-plotting-slots-for-layer
                            x-axis layer)))
                   (yslot (car
                           (axis-plotting-slots-for-layer
                            y-axis layer))))
               (loop for p in data
                  collect (slot-value p xslot)
                  collect (slot-value p yslot)))))

      (dolist (l out)
        (setf one (if (eql layer :front-selection)
                    (let ((sel (plotter-selection plotter)))
                      (if (and sel (selection-points sel))
                        (make-instance 'layer :data
                                       (selection-points sel))
                        l))
                    l))
        (when (or xy (and (not xyp)
                          (consp (%layer-data one))
                          (typep (car (%layer-data one))
                                 'point)))
          (setf one (convert-xy one (layer-data one)
                                (plotter-x-axis plotter)
                                (plotter-y-axis plotter)))
          (cond ((not x-decimals)
                 (floorthem one))
                ((integerp x-decimals)
                 (quantthem one x-decimals)))
          (cond ((not y-decimals)
                 (floorthem (cdr one)))
                ((integerp y-decimals)
                 (quantthem (cdr one) y-decimals))))
        ;; if we have converted to a list then use it
        ;; otherwise use user's format
        (push (if (consp one) one (%layer-data one)) res))
      ;; if there is only one layer then dont returm list of layers
      (if (or layer (null (cdr res))) 
        (car res)
        (nreverse res)))))
;;;
;;;
;;;

(defmacro defaxis (type class &rest spec)
  ;; define a custom axis. type is the user's typename,
  ;; class is the axis class, and spec are slot inits
  ;; and axis "metainfo"
  (let* ((save spec)
         (inits 
          (list* :type type
                 (loop while (keywordp (car spec))
                   collect (pop spec)
                   unless spec do (error "Malformed inits: ~S." 
                                         save)
                   collect (pop spec)))))

    (if (null class) (setf class 'axis)
        (setf class (car class)))
    `(set-axis-prototype 
      (make-instance (quote ,class) ,@ inits)
      (quote ,spec))))

(defun axis (type &rest inits)
  (let* ((proto (find-prototype type))
         (class (class-of proto))
         (slots (class-slots class))
         (axis  (apply #'make-instance (class-of proto) inits)))
    (loop for d in slots
          for s = (slot-definition-name d)
          do
          (unless (and (slot-boundp axis s)
                       (slot-value axis s))
            (when (slot-boundp proto s)
              (setf (slot-value axis s)
                    (slot-value proto s)))))
    axis))

(defun intstr (n) (prin1-to-string (floor n)))

;;;
;;; event io for plotters
;;;

(defmethod init-io ((io plotter) &rest inits)
  (unless (null inits)
    (multiple-value-bind (init args)
        (expand-inits (class-of io) inits nil t)
      args
      (dopairs (s v init) (setf (slot-value io s) v))))
  io)

(defmethod open-io ((object plotter) DIR &rest args)
  (declare (ignore dir args))
  ;; initalize plotter event cache
  ;; (<startime> {<events>}+)
  (setf (plotter-cache object) (list))
  object)

(defmethod write-event ((object event) (io plotter) scoretime)
  (setf (object-time object) scoretime)
  ;; push event onto the cache (reverse time order)
  (push object (plotter-cache io))
  object)
    
(defmethod close-io ((obj plotter) &rest mode)
  (let ((cache (plotter-cache obj)))
    (if (or (eql mode ':error)
            (null cache))       ; no events
      nil
      (let ((mode (or (plotter-event-layering obj) ':add))
            (xaxis (plotter-x-axis obj))
            (yaxis (plotter-y-axis obj))
            (layer (plotter-front-layer obj))
            (maxtime (object-time (first cache))) ; maxtime
            mintime)
        (unless (find mode '(:add :mix :replace :overwrite))
          (setq mode ':add))
        ;; add or replace if no layer or no data in layer.
        (if layer
          (if (null (layer-data layer))
            (setq mode ':add))
          (setq mode ':add))
        ;; reset x time axis maximum if generated events exceed it.
        ;; include length of last duration in the new maximum if
        ;; duration slot is set.
        (when (or (not (axis-maximum xaxis))
                  (< (axis-maximum xaxis) maxtime))
          ;; if there is a duration slot, add the time of the last
          ;; duration to the
          (let ((dslot 
                 (and layer
                      (cadr (axis-plotting-slots-for-layer
                             xaxis layer)))))
            (setf (axis-maximum xaxis)
                  (+ maxtime (if dslot (slot-value (car cache) dslot)
                                 0)))))
        ;; ensure that y axis has min and max values.
        (let ((min (axis-minimum yaxis))
              (max (axis-maximum yaxis)))
          (when (or (not min) (not max))
            (multiple-value-bind (lo hi)
                (plotting-bounds cache (car (axis-slots yaxis))
                                 nil min max)
              (unless min (setf (axis-minimum yaxis) lo))
              (unless max (setf (axis-maximum yaxis) hi)))))
        ;; put event in correct order
        (setq cache (nreverse cache))
        (setq mintime (object-time (car cache)))
        (case mode
          ((:add )
           ;; "re-add" a currently empty layer so that its axis
           ;; objects are initalized for the data always add as a seq
           ;; so that events can process data correctly with respect
           ;; to start times.
           (if (and layer (null (layer-data layer)))
             (setf (layer-data layer) cache)
             (setf layer (make-instance 'seq :subobjects cache)))
           (plotter-add-layer obj layer :redraw nil))
          ((:replace )
           (setf (layer-data layer) cache))
          ((:mix )
           (setf (layer-data layer)
                 (merge 'list (layer-data layer)
                        cache
                        (lambda (a b)
                          (< (object-time a) (object-time b))))))
          ((:overwrite )
           (let* ((tail (layer-data layer))
                  (head tail)
                  (last nil))
             ;; last is nil or strictly less than mintime
             (loop while (and tail (< (object-time (car tail)) mintime))
                do (setq last tail tail (cdr tail)))
             ;; tail is nil or strictly greater than maxtime
             (loop while (and tail (<= (object-time (car tail)) maxtime))
                do (setq tail (cdr tail)))
             (if last
               (progn (setf (cdr last) (nconc cache tail))
                      (setf (layer-data layer) head))
               (setf (layer-data layer)
                     (nconc cache tail))))))
        (if (plotter-window? obj)
          (let ((area (plotter-drawing-area obj)))
            (insure-drawing-sizes area xaxis yaxis)
            (plotter-redraw obj)
            )
          (plotter-open obj)
          )
        ))
    obj))

;; (defun repl (old cache)
;;   (let ((MINTIME (CAR CACHE))
;;         (MAXTIME (END CACHE))
;;         (head old)
;;         (tail old)
;;         (last nil))
;;     (loop while (and tail (< (car tail) mintime))
;;        do (setq last tail tail (cdr tail)))
;;     ;; last is nil strictly less than mintime
;;     (loop while (and tail (<= (car tail) maxtime))
;;        do (setq tail (cdr tail)))
;;     ;; tail is nil or strictly greater than max value
;;     (if last
;;       (progn (setf (cdr last) (nconc cache tail))
;;              ;(setf (layer-data layer) head)
;;              head
;;              )
;;       ;(setf (layer-data layer)(nconc cache tail))
;;       (nconc cache tail)
;;       )))
;; (repl (list 0 1 2 3 4 6) (list 2.0 2.1 2.2))
;; (repl (list 0 1 2 3 4 6) (list 0.0 .1 .2 1.0))
;; (repl (list 0 1 2 3 4 6) (list  1.1 1.5))
;; (repl (list 0 1 2 3 4 6) (list  6.1 6.5))
;; (repl (list 1 2 3 4 6) (list  0 .1 .2))
;; (repl (list 1 2 3 4 6) (list .01  5))

(eval-when (:load-toplevel :execute)

(defaxis :normalized ()
  :minimum 0 :maximum 1 :increment 1/4
  :ticks-per-increment 5 :labeling-threshold '(32 16)
  :labeler "~,2F"
  (:appropriate-slots :vertical (amplitude amp y)
                      :horizontal (x z)))

(defaxis :percentage ()
  :minimum 0 :maximum 100 :increment 25
  :ticks-per-increment 5 :labeling-threshold '(32 16)
  :labeler #'intstr
 (:appropriate-slots :vertical (y)
                      :horizontal (x z)))

(defaxis :keynum ()
  :minimum 0 :maximum 132 :increment 12
  :ticks-per-increment 12
  :labeler #'intstr
  (:appropriate-slots :vertical (keynum note y))
  (:insurer insure-keynum-axis))

(defaxis :seconds ()
  :minimum 0 :maximum nil  :increment 1
  :ticks-per-increment 4 :labeling-threshold 20
  :labeler "~,2F"
  (:appropriate-slots :horizontal (time start start-time
                                        begin duration dur x z))
  (:insurer insure-seconds-axis))

(defaxis :milliseconds () 
  :minimum 0 :maximum nil :increment 1000
  :ticks-per-increment 4  :labeling-threshold 20
  :labeler #'intstr
  (:appropriate-slots :horizontal (time start start-time
                                        begin duration dur x z)))

(defaxis :frequency ()
  :minimum 0 :maximum 22050
  :ticks-per-increment 1  :labeling-threshold 12
  (:appropriate-slots :vertical (freq frq frequency y)))

(defaxis :hertz (log-axis)
  :minimum 8.175798 :maximum 16744.035 :increment 2
  :ticks-per-increment 12  :labeling-threshold 12
  :labeler #'intstr
  (:appropriate-slots :horizontal (freq frequency x z)
                      :vertical (freq  frequency y)))

;; the catchall prototype

(defaxis nil ()
  :minimum 0 
  :ticks-per-increment 1
  :labeler "~,1F"
  (:appropriate-slots :horizontal (time x z)
                      :vertical (y keynum frequency amplitude)))
)