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
;;; $Revision: 1.4 $
;;; $Date: 2005/01/02 16:31:14 $

(in-package :cm)

;;;
;;; key handler
;;;

(gtk:define-signal-handler key-press-event :int (widget event window)
  widget event window
  ;; SHIFT: GDK_Shift_L 0xFFE1, GDK_Shift_R 0xFFE2
  ;; CONTROL: GDK_Control_L 0xFFE3, GDK_Control_R 0xFFE4
  ;; APPLE COMMAND KEY: GDK_Meta_L 0xFFE7, GDK_Meta_R 0xFFE8, 
  ;; MY META KEY: GDK_Alt_L 0xFFE9, GDK_Alt_R 0xFFEA
  (let* ((states (gdk:EventKey.state event))
         (keyval (gdk:EventKey.keyval event))
         (plotter (widget->object window))
         (select (plotter-selection plotter)))
    (when select
      (cond ((= states 0)
             (case keyval
               (( #xFF08 #xFFFF ) 
                ;; Backspace, Delete keys
                (delete-selection plotter))
               (( #xFF51 #xFF52 #xFF53 #xFF54 ) ; arrow keys
                ;; Left, Up,    Right, Down 
                (let ((dx nil)
                      (dy nil))
                  (cond ((= keyval #xFF51)
                         (setq dx (- (axis-tick-size
                                      (plotter-x-axis plotter)))))
                        ((= keyval #xFF53)
                         (setq dx (axis-tick-size 
                                   (plotter-x-axis plotter))))
                        ((= keyval #xFF52)
                         (setq dy (- (axis-tick-size
                                      (plotter-y-axis plotter)))))
                        ((= keyval #xFF54)
                         (setq dy (axis-tick-size
                                   (plotter-y-axis plotter)))))
                  (move-selection-relative plotter select dx dy)))))
          (t nil)))
    gtk:+false+))

;;;
;;; mouse down handler
;;;

(gtk:define-signal-handler button-press-event :int (widget event data)
  ;; widget is drawing-area, data is main plotter window
  widget  
  (let* ((plotter (widget->object data))
         (x-axis (plotter-x-axis plotter))
         (y-axis (plotter-y-axis plotter))
         (layer (plotter-front-layer plotter))
         (x-slots (and layer 
                       (axis-plotting-slots-for-layer x-axis layer)))
         (y-slots (and layer (axis-plotting-slots-for-layer
                              y-axis layer)))
         (info (plotter-mouseinfo plotter)))
    (when (and (= (gdk:EventButton.button event) 1) ;bitmap
               layer x-slots y-slots)
      (if (= (gdk:EventButton.type event)
             gdk:2button-press)
        (let ((sel (plotter-selection plotter)))
          (if (and sel (selection-type? sel ':point))
            (edit-selection plotter sel)))
        (let* ((xpix (gdk:EventButton.x event))
               (ypix (gdk:EventButton.y event))
               (xval (axis-value xpix x-axis))
               (yval (axis-value ypix y-axis t))
               (mods (gdk:EventButton.state event))
               (mode (mouseinfo-mode info))
               point selection)
          (setf (mouseinfo-xdown info) xpix)
          (setf (mouseinfo-ydown info) ypix)
          (case mode
            (( :select-points )
             (setq selection (plotter-selection plotter))
             (setq point
                   (find-point-in-layer plotter
                                        (plotter-front-layer plotter)
                                        xpix ypix xval yval
                                        x-axis y-axis x-slots y-slots))
             ;; do nothing if remoused a selected point, else
             ;; make new selection or clear current selection.
             (if point
               (if (and selection 
                        (find point (selection-points selection)))
                 nil
                 (set-selection plotter :points (list point)
                                :type ':point
                                :update (logtest mods gdk:control-mask)))
               (collapse-selection plotter)))
            (( :select-regions )
             (collapse-selection plotter))
            (( :add-points )
             (setq point 
                   (add-new-point plotter
                                  (plotter-front-layer plotter)
                                  xval yval x-axis y-axis
                                  x-slots y-slots))
             (when point
               ;; set but dont redraw selection because entire display
               ;; gets recalculated.
               (set-selection plotter :points (list point)
                              :type ':point
                              :update (logtest mods gdk:control-mask)
                              :redraw NIL)
               (plotter-redraw plotter t)))
            (( :delete-points )
             ;; if a selected point is re-moused then apply the editing
             ;; operation to the entire selection else make the new
             ;; point the selection and perform the operation
             (setq selection (plotter-selection plotter))
             (setq point
                   (find-point-in-layer plotter
                                        (plotter-front-layer plotter)
                                        xpix ypix xval yval
                                        x-axis y-axis x-slots y-slots))
             (when point
               (unless (and selection
                            (find point (selection-points selection)))
                 ;; dont redraw
                 (set-selection plotter :points (list point)
                                :type ':point :redraw nil))
               (delete-selection plotter)))))))
    gtk:+true+))

;;;
;;; mouse move handler. called when moving selected points or sweeping
;;; out a region selection.
;;;

(gtk:define-signal-handler motion-notify-event :int (widget event data)
  ;; widget is drawing area, data is main window
  widget event data
  ;; only check left-button mousedown motion
  (when (and (= (gdk:EventMotion.is-hint event) gtk:+true+)
             (logtest (gdk:EventMotion.state event) gdk:button1-mask))
    (let* ((plotter (widget->object data))
           (info (plotter-mouseinfo plotter))
           (mode (mouseinfo-mode info)))
      (when (member mode '(:select-points :select-regions :add-points))
        (let ((sel (plotter-selection plotter))
              (xpix (gdk:EventMotion.x event))
              (ypix (gdk:EventMotion.y event)))
          (if sel
            ;; we are either moving a point selection, drawing new points
            (progn
              ;; maybe constrain motion to one axis
              (cond ((eq (mouseinfo-track info) ':x)
                     (setq ypix (mouseinfo-ydown info)))
                    ((eq (mouseinfo-track info) ':y)
                     (setq xpix (mouseinfo-xdown info))))
              (if (eql mode ':add-points)
                (if (>= (mouse-distance (- (mouseinfo-xdown info) xpix)
                                        (- (mouseinfo-ydown info) ypix))
                        (mouseinfo-apart info))
                  (let* ((xaxis (plotter-x-axis plotter))
                         (yaxis (plotter-y-axis plotter))
                         (layer (plotter-front-layer plotter))
                         (xslots (axis-plotting-slots-for-layer xaxis layer))
                         (yslots (axis-plotting-slots-for-layer yaxis layer))
                         (xval (axis-value xpix xaxis))
                         (yval (axis-value ypix yaxis t))
                         (point
                          (add-new-point plotter layer xval yval
                                         xaxis yaxis xslots yslots)))
                    (when point
                      (set-selection plotter :points (list point)
                                     :type ':point
                                     :update T
                                     :redraw T))
                    (setf (mouseinfo-xdown info) xpix)
                    (setf (mouseinfo-ydown info) ypix)
                    )
                  )
                ;; else we are moving an existing point selection.
                (progn
                  ;; undraw selection at old position and draw at new.  xmove
                  ;; will be NIL on the first notify after the mouse down. in
                  ;; this case dont undraw
                  (when (mouseinfo-xmove info)
                    (draw-selection plotter sel :mode ':move
                                    :dx (FLOOR (- (mouseinfo-xdown info) 
                                                  (mouseinfo-xmove info)))
                                    :dy (FLOOR (- (mouseinfo-ydown info)
                                                  (mouseinfo-ymove info)))))
                  ;; draw at new move position
                  (draw-selection plotter sel :mode ':move
                                  :dx (FLOOR (- (mouseinfo-xdown info) xpix))
                                  :dy (FLOOR (- (mouseinfo-ydown info)
                                                ypix))))))
            ;; region sweeping
            (progn
              ;; maybe constrain sweeping to one axis and select all of
              ;; other
              (if (eql (mouseinfo-track info) ':x)
                ;; select all of y dimension
                (let* ((yaxis (plotter-y-axis plotter))
                       (offset (axis-offset yaxis)))
                  (setf (mouseinfo-ydown info) offset)
                  (setf ypix (- offset (axis-size yaxis))))
                (if (eql (mouseinfo-track info) ':y)
                  (let* ((xaxis (plotter-x-axis plotter))
                         (offset (axis-offset xaxis)))
                    (setf (mouseinfo-xdown info) offset)
                    (setf xpix (+ offset (axis-size xaxis))))))
              (when (mouseinfo-xmove info)
                ;; undraw region at former position
                (draw-region plotter
                             (mouseinfo-xdown info)
                             (mouseinfo-ydown info)
                             (mouseinfo-xmove info)
                             (mouseinfo-ymove info)
                             :pen-mode gdk:xor :color "gray"
                             :redraw t))
              (draw-region plotter (mouseinfo-xdown info)
                           (mouseinfo-ydown info)
                           xpix ypix :pen-mode gdk:xor
                           :color "gray" :redraw t)))
          (setf (mouseinfo-xmove info) xpix)
          (setf (mouseinfo-ymove info) ypix)
          (gdk:window-get-pointer (gdk:EventMotion.window event)
                                  0 0 0)))))
  gtk:+true+)

;;;
;;; mouse up handler. checks to see if mouse has moved, and if
;;; so completes the xor drawing
;;;

(gtk:define-signal-handler button-release-event :int (widget event data)
  widget event data
  (let* ((plotter (widget->object data))
         (info (plotter-mouseinfo plotter)))
    ;; if xmove is set then we are releasing the button
    ;; after a move, point draw or region sweep.
    (if (mouseinfo-xmove info)
      ;; if we have a selection then we are moving points else we are
      ;; sweeping region
      (let ((sel (plotter-selection plotter)))
        (if sel
          (if (eql (mouseinfo-mode info) ':add-points)
            (plotter-redraw plotter t)
            (let ((dx (- (mouseinfo-xdown info)
                         (mouseinfo-xmove info)))
                  (dy (- (mouseinfo-ydown info)
                         (mouseinfo-ymove info))))
              ;; undraw last outline.
              (draw-selection plotter sel :mode ':move
                              :dx (FLOOR dx)
                              :dy (FLOOR dy))
              (unless (and (= dx 0) (= dy 0))
                (move-selection plotter sel 
                                (mouseinfo-xdown info)
                                (mouseinfo-ydown info)
                                (mouseinfo-xmove info)
                                (mouseinfo-ymove info)))))
          (let* ((lay (plotter-front-layer plotter))
                 (pts (find-points-in-region plotter lay
                                             (mouseinfo-xdown info)
                                             (mouseinfo-ydown info)
                                             (mouseinfo-xmove info)
                                             (mouseinfo-ymove info))))
            ;; undraw sweep's last outline
            (draw-region plotter (mouseinfo-xdown info)
                         (mouseinfo-ydown info)
                         (mouseinfo-xmove info)
                         (mouseinfo-ymove info)
                         :pen-mode gdk:xor :color "gray" 
                         :redraw NIL    ;(if pts nil t)
                         )
            (if (eql (mouseinfo-mode info) ':select-points)
              (if pts
                (set-selection plotter :points pts
                               :type ':point :layer lay)
                (plotter-redraw plotter nil))
              (set-selection plotter :type ':region
                             :region (list (mouseinfo-xdown info)
                                           (mouseinfo-ydown info)
                                           (mouseinfo-xmove info)
                                           (mouseinfo-ymove info))
                             ))))))
    (setf (mouseinfo-xdown info) nil)
    (setf (mouseinfo-ydown info) nil)
    (setf (mouseinfo-xmove info) nil)
    (setf (mouseinfo-ymove info) nil)
    gtk:+true+))

;;;
;;; support routines
;;;

(defun mouse-distance (x y)
  (sqrt (+ (expt (abs x) 2) 
           (expt (abs y) 2))))

(defun get-select-all (plotter type)
  (ecase type
    (:region
     (let ((xaxis (plotter-x-axis plotter))
           (yaxis (plotter-y-axis plotter)))
       (list (axis-pixel (axis-minimum xaxis) xaxis)
             (axis-pixel (axis-maximum yaxis) yaxis t)
             (axis-pixel (axis-maximum xaxis) xaxis)
             (axis-pixel (axis-minimum yaxis) yaxis t))))
    (:point
     (layer-data (plotter-front-layer plotter)))))

(defun select-all (plotter type)
  (let ((data  (get-select-all plotter type)))
    (if (null data)
      (collapse-selection plotter)
      (set-selection plotter :type type
                     :region (if (eq type ':region) data nil)
                     :points (if (eq type ':points) data nil)
                     :layer (plotter-front-layer plotter)))))

(defun flip-selection (plotter selection &key (redraw t))
  redraw
  (flet ((region-to-axes (x-axis y-axis region)
           ;; return: LEFT BOTTOM RIGHT TOP as axis values
           (values (axis-value (first region) x-axis)
                   (axis-value (FOURTH region) y-axis t)
                   (axis-value (third region) x-axis)
                   (axis-value (SECOND region) y-axis t)))
         (axes-to-region (x-axis y-axis left bottom right top)
           ;; return left TOP right BOTTOM as pixel values
           (list (- (axis-pixel left x-axis) 1)
                 (- (axis-pixel top y-axis t) 1)
                 (+ (axis-pixel right x-axis) 1)
                 (+ (axis-pixel bottom y-axis t) 1))))
    (let ((type (selection-type selection))
          (xaxis (plotter-x-axis plotter))
          (yaxis (plotter-y-axis plotter))
          (layer (plotter-front-layer plotter)))
      (ecase type
        (:region 
         (let (other)
           (multiple-value-bind (left bottom right top)
               (region-to-axes xaxis yaxis (selection-region selection))
             (with-display-slots (xaxis layer xs)
               (with-display-slots (yaxis layer ys)
                 (setq other
                       (loop for p in (layer-data layer)
                          for x = (slot-value p xs)
                          until (> x right)
                          when (and (<= left x)
                                    (<= bottom (slot-value p ys) top))
                          collect p))))
             (if (null other)
               (collapse-selection plotter)
               (progn (setf (selection-type selection) ':point)
                      (setf (selection-points selection) other)
                      (plotter-redraw plotter T))))))
        (:point
         (if (cdr (selection-points selection))
           (let ((left most-positive-fixnum)
                 (right most-negative-fixnum)
                 (bottom most-positive-fixnum)
                 (top most-negative-fixnum))
             (with-display-slots (xaxis layer xs)
               (with-display-slots (yaxis layer ys)
                 (loop for p in (selection-points selection)
                    for x = (slot-value p xs)
                    for y = (slot-value p ys)
                    do (setq left (min left x)
                             right (max right x)
                             top (max y top)
                             bottom (min y bottom)))))
             (setf (selection-type selection) ':region)
             (setf (selection-region selection)
                   (axes-to-region xaxis yaxis left bottom right top))
             (plotter-redraw plotter T))
           (collapse-selection plotter)))))))

(defun move-selection (plotter selection x1 y1 x2 y2)
  (let* ((xaxis (plotter-x-axis plotter))
         (yaxis (plotter-y-axis plotter))
         (layer (plotter-front-layer plotter))
         (dx (if  (= 0 (- x1 x2)) nil
                  (- (axis-value x1 xaxis) (axis-value x2 xaxis))))
         (dy (if  (= 0 (- y1 y2)) nil
                  (- (axis-value y1 yaxis t) (axis-value y2 yaxis t)))))
    (if dx
      (if dy
        (with-display-slots (xaxis layer xs)
          (with-display-slots (yaxis layer ys)
            (dolist (p (selection-points selection))
              (decf (slot-value p xs) dx)
              (decf (slot-value p ys) dy)))
          (unless (ordered-layer? layer xs)
            (sort-layer layer xs)))
        (with-display-slots (xaxis layer xs)
          (dolist (p (selection-points selection))
            (decf (slot-value p xs) dx))
          (unless (ordered-layer? layer xs)
            (sort-layer layer xs))))
      (if dy
        (with-display-slots (yaxis layer ys)
          (dolist (p (selection-points selection))
            (decf (slot-value p ys) dy)))))
    (plotter-redraw plotter t)))

(defun move-selection-relative (plotter selection dx dy)
  (let* ((xaxis (plotter-x-axis plotter))
         (yaxis (plotter-y-axis plotter))
         (layer (plotter-front-layer plotter)))
    (if dx
      (if dy
        (with-display-slots (xaxis layer xs)
          (with-display-slots (yaxis layer ys)
            (dolist (p (selection-points selection))
              (setf (slot-value p xs)
                    (axis-value (+ dx (axis-pixel (slot-value p xs)
                                                  xaxis))
                                xaxis))
              (setf (slot-value p ys)
                    (axis-value (+ dy (axis-pixel (slot-value p ys) 
                                                  yaxis t))
                                yaxis t))))
          (unless (ordered-layer? layer xs)
            (sort-layer layer xs)))
        (with-display-slots (xaxis layer xs)
          (dolist (p (selection-points selection))
            (setf (slot-value p xs)
                  (axis-value (+ dx (axis-pixel (slot-value p xs)
                                                xaxis))
                              xaxis)))
          (unless (ordered-layer? layer xs)
            (sort-layer layer xs))))      
      (if dy
        (with-display-slots (yaxis layer ys)
          (dolist (p (selection-points selection))
            (setf (slot-value p ys)
                  (axis-value (+ dy (axis-pixel (slot-value p ys) 
                                                yaxis t))
                              yaxis t))))))
    (plotter-redraw plotter t)))

(defun find-points-in-region (plotter layer px1 py1 px2 py2)
  ;; find all points with pixel region
  (if (< px2 px1) (rotatef px1 px2))
  (if (< py1 py2) (rotatef py1 py2))
  (let* ((x-axis (plotter-x-axis plotter))
         (y-axis (plotter-y-axis plotter))
         (vx1 (axis-value px1 x-axis))
         (vy1 (axis-value py1 y-axis t))
         (vx2 (axis-value px2 x-axis))
         (vy2 (axis-value py2 y-axis t)))
    (with-display-slots (x-axis layer xs)
      ;;(print (list :x-> xs vx1 vx2))
      (with-display-slots (y-axis layer ys)
        ;;(print (list :y-> ys vy1 vy2))
        (loop for p in (layer-data layer)
              for x =  (slot-value p xs)
              while (<= x vx2)
              when (and (>= x vx1)
                        (<= vy1 (slot-value p ys) vy2))
              collect p)))))

(defun draw-region (plotter x1 y1 x2 y2
                            &key filled pen-mode color (redraw t))
  (let ((gc (plotter-gc plotter))
        (co (drawing-color color (plotter-colors plotter))))
    (gdk:gc-set-foreground gc co)
    (gdk:gc-set-function gc pen-mode)
    (if (< x2 x1) (rotatef x1 x2))
    (if (< y2 y1) (rotatef y1 y2))
    (gdk:draw-rectangle (plotter-bitmap plotter) gc filled
                        (FLOOR x1)
                        (FLOOR y1)
                        (FLOOR (- x2 x1))
                        (FLOOR (- y2 y1)))
    (unless (eql pen-mode gdk:copy)
      (gdk:gc-set-function gc gdk:copy))
    (when redraw
      (plotter-redraw plotter nil))
    (values)))

(defun draw-selection (plotter selection &key (mode :select)
                       points (redraw t) (dx 0) (dy 0))
  (let ((fill? t)
        (flag nil)
        (type (selection-type selection))
        (pmode gdk:copy)
        (color nil))
    (ecase mode
      (( :select )
       (if (eql type ':point)
         (setf color "black")
         (setq color "gray" pmode gdk:and)))
      (( :unselect )
       ;; revert to layer's original color
       (if (eql type ':point)
         (setf color NIL)
         (setf flag t)
         ))
      ((:move  )
       (setf color "gray")
       (setf pmode gdk:xor)
       (setf fill? nil)))
    (if (eql type ':point)
      (draw-layer plotter (selection-layer selection)
                  :points (or points (selection-points selection))
                  :color (if color 
                           (drawing-color color (plotter-colors plotter))
                           nil)
                  ;; if moving we draw outlines xor
                  :pen-mode pmode :points-filled fill? :points-only t
                  :dx dx :dy dy)
      (if (not (eql mode ':unselect))   ; HACK only draw regions on :select
        (let ((reg (selection-region selection)))
          (draw-region plotter (first reg) (second reg)
                       (third reg) (fourth reg)
                       :pen-mode pmode :color color :filled t
                       :redraw nil))))
    (when redraw
      ;; since this function updates the bitmap plotter-redraw is always
      ;; called with NIL so that it only triggers the screen update.
      ;; HACK: unselecting region redraws entire display
      (plotter-redraw plotter flag))
    selection))

(defun collapse-selection (plotter &key (redraw t))
  ;; unhighlight the current selection and flush selection
  (let ((sel (plotter-selection plotter)))
    (when sel
      (setf (plotter-selection plotter) NIL)
      (when redraw (draw-selection plotter sel :mode ':unselect)))
    (values)))

(defun set-selection (plotter &key selection points region
                              all type layer update (redraw t))
  ;; update is only checked if there is an existing selection
  (unless (member type '(:point :region))
    (error "selection :type ~S is not :POINT or :REGION" type))
  (let ((old (plotter-selection plotter)))
    (if (and old update)
      (ecase (selection-type old)
        (:region )
        (:point
         ;; control-click unselects or
         (cond ((find (car points) (selection-points old))
                (when redraw
                  (draw-selection plotter old :mode ':unselect
                                  :points points))
                (setf (selection-points old)
                      (delete-if #'(lambda (x) (member x points))
                                 (selection-points old)))
                old)
               (t
                (setf (selection-points old)
                      (nconc (selection-points old) points))
                (when redraw (draw-selection plotter old))
                old))))
      (progn
        ;; flush and undraw current selection if it exists
        (collapse-selection plotter :redraw redraw)
        (when region
          (if (< (third region) (first region))
            (rotatef (third region) (first region)))
          (if (< (fourth region) (second region))
            (rotatef (fourth region) (second region))))
        (unless selection
          (setq selection
                (make-instance 'selection
                               :type type
                               :layer (or layer
                                          (plotter-front-layer
                                           plotter))
                               :points points
                               :region region
                               :all all)))
        (setf (plotter-selection plotter) selection)
        (if redraw (draw-selection plotter selection))
        selection))))

;;;
;;; delete selection
;;;

(defun delete-selection (plotter &key (redraw t))
  (let (( selection (plotter-selection plotter)))
    (if selection
      (let ((points (selection-points selection))
            (layer (selection-layer selection)))
        (collapse-selection plotter :redraw nil)
        (if (selection-all? selection)
          (setf (layer-data layer) nil)
          (delete-points-in-layer layer points))
        (if redraw (plotter-redraw plotter))
        selection))))

(defun delete-points-in-layer (layer points)
  (setf (layer-data layer)
         (delete-if #'(lambda (x) (member x points :test #'eq))
                   (layer-data layer))))

;;;
;;; Edit selection
;;;

(defun edit-selection (plotter selection)
  (let* ((layer (plotter-front-layer plotter))
         (xslots (axis-plotting-slots-for-layer
                  (plotter-x-axis plotter)
                  layer))
         (yslots (axis-plotting-slots-for-layer
                  (plotter-y-axis plotter)
                  layer))
         (dslots (list* (car xslots)
                        (car yslots)
                        (append (cdr xslots) (cdr yslots)))))
    (edit-object selection :plotter plotter
                 :slot-order dslots :display-slots dslots)))

(defun add-new-point (plotter layer x y x-axis y-axis x-slots y-slots)
  x-axis y-axis
  (multiple-value-bind (class error)
      (plotter-new-point-class plotter layer x-axis y-axis
                               x-slots y-slots)
    (if (not class)
      (progn (warn error) nil)
      (let ((new (make-instance class)))
        (setf (slot-value new (car x-slots)) x)
        (setf (slot-value new (car y-slots)) y)
        ;; FIX: will not work for log plots
        (when (cdr x-slots)
          (setf (slot-value new (cadr x-slots))
                (/ (axis-increment x-axis)
                   (axis-ticks-per-increment x-axis))))
        ;; FIX: will not work for log plots
        (when (cdr y-slots)
          (setf (slot-value new (cadr y-slots))
                (/ (axis-increment y-axis)
                   (axis-ticks-per-increment y-axis))))
        (add-point-to-layer new layer (car x-slots))
        new))))

(defun plotter-new-point-class (plotter layer x-axis y-axis
                                        x-slots y-slots)
  ;; return a class for new points if it can be dispalyed in layer.
  x-axis y-axis
  (flet ((get-point-class (plotter)
           (let ((wind (plotter-editing-tool plotter)))
             ;; check if tool is open, if so use its point class.
             (if wind
               (let* ((tool wind)
                      ;; combo entry text is class name
                      (text (gtk:entry-get-text
                             (widget-property tool :point-class)
                             ;;(GtkCombo.entry
                             ;; (widget-property tool :point-menu))
                             )))
                 (if (string= text "")
                   (values nil "Set point class in Editing tool.")
                   (values
                    (find-class (find-symbol (string-upcase text))
                                nil)
                    (format nil "~A is not a class." text))))
               (or 
                (let ((p (first (layer-data 
                                 (plotter-front-layer plotter)))))
                  (if p
                    (class-of p) 
                    nil))
                (plotter-point-class plotter)))))
         (errstr (axis class slots)
           (format 
            nil
            "Point class ~A does not contain ~A slot~:[s~;~]~{ ~A~}."
            (class-name class)
            axis
            (not (cdr slots)) slots)))
    (multiple-value-bind (class error)
                        (get-point-class plotter)
      (cond ((not class)
             (values nil
                     (or error
                         "No point class established, use Editing tool."
                         )))
            ((typep (first (layer-data layer)) class)
             class)
            (t
             (let ((c-slots (class-slots class)))
               (if (and x-slots y-slots)
                 (if (loop for s in x-slots 
                           always 
                           (find s c-slots
                                 :key #'slot-definition-name))
                   (if (loop for s in y-slots 
                             always 
                             (find s c-slots
                                   :key #'slot-definition-name))
                     (values class nil)
                     (values nil
                             (errstr "vertical axis" class y-slots)))
                   (values nil
                           (errstr "horizontal axis" class x-slots)))
                 (if x-slots
                   (values nil
                           "No data slots are set in vertical axis.")
                   (if y-slots
                     (values nil
                             "No data slots are set in horizontal axis.")
                     (values nil
                             "No daa slots are set in either axis.")))
                 )))))))

(defun add-point-to-layer (point layer slot)
  (let ((data (layer-data layer))
        (val (slot-value point slot)))
    (if (not data)
      (setf (%layer-data layer) (list point))
      (let ((tail data) 
            (last nil))
        ;; increment postion in tail while it is <= to coord
        (loop while (and tail
                         (not (< val (slot-value (car tail) slot))))
          do (setf last tail tail (cdr tail)))
        ;; first tail is nil or strictly less than coord
        (setf tail (cons point tail))
        (if last
          (rplacd last tail)
          (setf (%layer-data layer) tail))))
    point))

(defun find-point-in-layer (plotter layer mxp myp ; mouse xy pixel
                                    xval yval
                                    x-axis y-axis x-slots y-slots)
  ;; todo: dont allow :box unless axis has 2 slots
  yval
  (let* ((x-slot (car x-slots)) ; ignore width or height slots
         (y-slot (car y-slots))
         (phalfw (/ (plotting-point-width plotter layer) 2))
         (phalfh (/ (plotting-point-height plotter layer) 2))
         ;; mouse x extent in axis coordinates
         (mxv1 (axis-value (- mxp phalfw) x-axis)) ; left
         (mxv2 (axis-value (+ mxp phalfw) x-axis)) ; right
         ;; mouse y extent in axis coordinates
         (myv1 (axis-value (+ myp phalfh) y-axis t)) ; bottom
         (myv2 (axis-value (- myp phalfh) y-axis t)) ; top
         (view (styling-view (layer-style layer)
                             (plotter-style plotter))))
    (cond ((member view '(:line-and-point :point :line))
           (loop for z = nil then c
                 for c on (layer-data layer)
                 for d = (car c)
                 for x = (slot-value d x-slot)
                 ;; search until slot values > than mousex
                 until (> x mxv2)
                 when (and (<= mxv1 x mxv2)
                           (<= myv1 (slot-value d y-slot) myv2))
                 do (return (values d z))))
          ((eql view ':box)
           (let ((w-slot (cadr x-slots))
                 (x xval))
             (loop for z = nil then c
                   for c on (layer-data layer)
                   for d = (car c)
                   for x1 = (slot-value d x-slot)
                   for x2 = (+ x1 (slot-value d w-slot))
                   until (> x1 x) ; stop if start is later
                   when (and (<= x1 x x2)
                             (<= myv1 (slot-value d y-slot) myv2))
                   do (return (values d z))))))))

(defun foo ()
  (plotter :y-axis (axis ':normalized :slots 'y)
         :x-axis (axis ':normalized :slots '(x z))
         (loop with i = 0 while (< i .9)
               collect (make-instance 'point :x i
                                      :y (random 1.0)
                                      :z (random .2))
               do (incf i (random .2)))))

; (progn (load "pload") (in-package :gtk) )
