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
;;; $Revision: 1.8 $
;;; $Date: 2005/01/15 15:50:20 $

;;;
;;; Main window drawing routines
;;;

(in-package :cm)

;;;
;;; provide reasonable defaults for some internal drawing sizes.  the
;;; default for *pixels-per-increment* is 60 because that makes it
;;; evenly divisible by many common tick values: 4, 5, 6, 10, 12 etc.
;;; label-area is the space claimed for the axis lines and
;;; labels. padding is the blank area surrounding the plotting grid on
;;; the top and right.

(defparameter *default-pixels-per-increment* 60)
(defparameter *label-area-width* 60)
(defparameter *label-area-height* 60)
(defparameter *axis-padding* 6)
(defparameter *axis-inset* 12)

(defun free-plotter (plotter)
  ;; destroy all Tool windows and inspectors....
  ;;(setq *gtk-open-toplevels*
  ;;      (remove plotter *gtk-open-toplevels*))
  (gtk-remove-toplevel plotter)
  (dolist (w (plotter-tools plotter))
    (if w (gtk:widget-destroy w)))
  (dolist (w (plotter-inspectors plotter))
    (gtk:widget-destroy w))
  ;; free up menubar
  (slot-makunbound plotter 'menubar)
  ;; free up drawing area TODO: free its cached LAYOUT
  (when (slot-boundp plotter 'drawing-area)
    (let ((darea (plotter-drawing-area plotter)))
      ;; free up our hand-allocated rect.
      (gtk:struct-free (g:object-get-data darea "user_data"))
      (slot-makunbound plotter 'drawing-area)))
  (slot-makunbound plotter 'x-scroller)
  (slot-makunbound plotter 'y-scroller)
  ;; free up bitmap
  (when (slot-boundp plotter 'bitmap)
    (g:object-unref (plotter-bitmap plotter))
    (slot-makunbound plotter 'bitmap))
  ;; TODO: Free up COLORS in colormap with gtk:struct-free
  (let ((map (plotting-colormap plotter)))
    (g:object-unref map))
  (slot-makunbound plotter 'colors)
  ;; dont need to free window since its being destroyed.
  (when (slot-boundp plotter 'window)
    ;; remove widget from hashtable
    (remove-widget (plotter-window plotter))
    (slot-makunbound plotter 'window))
  (values))

;;; this is the destroy callback. it is triggered by closing the main
;;; window or selecting Layer->Quit from the menubar.

(gtk:define-signal-handler destroy-plotter-window :void (window )
  window
  (let ((plotter (widget->object window)))
    (free-plotter plotter)
    ;;(if (null *gtk-open-toplevels*)
    ;;  (progn (setq *gtk-main* nil)
    ;;         (gtk:main-quit)))
    (unless (gtk-open-toplevels?)
      (gtk-main-stop))
    (values)))

;;;
;;; focus-in-event: move selected plotter to front of open plotter
;;; stack.

(defun focus-plotter () (car *gtk-open-toplevels*))

(gtk:define-signal-handler focus-in-event :int (window event data)
  window event data
  (let ((plotter (widget->object window)))
    (if (and (cdr *gtk-open-toplevels*)
             (not (eql plotter (car *gtk-open-toplevels*))))

      (rotatef (elt *gtk-open-toplevels* 0)
               (elt *gtk-open-toplevels*
                    (position plotter *gtk-open-toplevels*)))))
  gtk:+false+)


(defun erase-bitmap (bitmap gc x y w h)
  (gdk:draw-rectangle bitmap gc t x y w h))

(gtk:define-signal-handler configure-event :int (widget event data)
  ;; widget is drawing area, data is main plotter window.
  ;; create a new bitmap in response to a resizing of
  ;; the drawing area. this could be made alot smarter!!!
  widget event data
  (let* ((plotter (widget->object data))
         (bitmap (plotter-bitmap plotter))
         (width (gtk:Widget.allocation.width widget))
         (height (gtk:Widget.allocation.height widget)))
    (when bitmap (g:object-unref bitmap))
    (setf bitmap (gdk:pixmap-new (gtk:Widget.window widget)
                                 width height -1))
    (setf (plotter-bitmap plotter) bitmap)
    (erase-bitmap bitmap (Gtk:Style.white-gc (gtk:Widget.style widget))
                  0 0 (gtk:Widget.allocation.width widget)
                  (gtk:Widget.allocation.height widget))
    (setf (plotter-draw plotter) t)
    gtk:+true+))

;;;
;;; expose-event is the main drawing function. its called whenever the
;;; drawing area is reconfigured or invalidated due to scrolling,
;;; zooming, calling top-level plotter functions, or whatever. If the
;;; plotter's draw flag is set then expose-event FIRST redraws all
;;; plots onto the offscreen bitmap before updating the visible
;;; display. Once the bitmap contains a valid display expose-event
;;; then copies it onto the drawing area at the current scroll
;;; position with axis lines and labels always maintaing their fixed
;;; positions to the left and bottom of the plotting grid.
;;;

(gtk:define-signal-handler expose-event :int (widget event data)
  ;; widget is drawing area, data is main window
  widget event data
  (let* ((plotter (widget->object data))
         (selection (plotter-selection plotter))
         (bitmap (plotter-bitmap plotter))
         (gc (Gtk:Style.fg-gc (gtk:Widget.style widget)
                             (gtk:Widget.state widget)))
         (wgc (Gtk:Style.white-gc (gtk:Widget.style widget)))
         (win (gtk:Widget.window widget))
         (xscroll (plotter-x-scroller plotter))
         (yscroll (plotter-y-scroller plotter))
         (xaxis (plotter-x-axis plotter))
         (yaxis (plotter-y-axis plotter))
         ;; xlab is the height below the grid for drawing the x axis
         ;; display and ylab is the width to the left of the grid for
         ;; displaying the y axis and labels.
         (xlab (axis-label-area xaxis))
         (ylab (axis-label-area yaxis))
         ;; xpad and ypad are blank space surrounding the grid
         ;; this value reduces the extent of the label area.
         (xpad *axis-padding*)
         (ypad *axis-padding*)
         ;; screen left, top, width and height are current
         ;; scoll coordinates.
         (sleft (FLOOR (Gtk:Adjustment.value xscroll)))
         (stop (FLOOR (Gtk:Adjustment.value yscroll)))
         (swidth (FLOOR (Gtk:Adjustment.page-size xscroll)))
         (sheight (FLOOR (Gtk:Adjustment.page-size yscroll)))
         ;; left edge of plotting grid in scroll coords
         ;;(sgridleft (FLOOR (+ (gtk:adjustment.value xscroll)
         ;;                    ylab)))
         ;; bottom of plotting grid in scroll coords. takes the min of
         ;; the y axis-offset and the page-size since the user may
         ;; have resized the window to be much larger than the plot
         ;; itself.
         (sgridbot (FLOOR (min
                           (axis-offset yaxis)
                           (+ (gtk:adjustment.value yscroll)
                              (gtk:adjustment.page-size yscroll)
                              (- xlab)))))
         ;; get our cached rect
         (rect (g:object-get-data widget "user_data")))

    ;; if draw flag is set then draw the plots on the bitmap before
    ;; copying to drawing area
    (let ((draw? (plotter-draw plotter))
          (flags (plotter-flags plotter))
          (layers (plotter-layers plotter)))
      ;; as of now draw? is just T but at some point its value will 
      ;; convey info for controlling the layer drawing.
      (when draw?
        ;; make sure we have a plotting gc available. this should
        ;; really be allocated in main plotter function but
        ;; (apparently) it cant be created until the drawing area
        ;; actually has a window :(
        ;;*moved to plotter-open
        ;;(unless (plotter-gc plotter) 
        ;;          (setf (plotter-gc plotter)
        ;;                (gdk:gc-new (gtk:Widget.window widget))))
        ;; erase current drawing. 
        (erase-bitmap bitmap wgc 0 0 
                      (gtk:Widget.allocation.width widget)
                      (gtk:Widget.allocation.height widget))
        ;; draw plotting grid underneath the layers if use wants it.
        (when (logtest flags +show-grid+)
          (draw-graduals plotter :grid))
        (when layers
          ;; draw background layers first if user want them.
          (let ((focus (plotter-front-layer plotter)))
            (when (logtest flags +show-back-layers+)
              (dolist (p layers)
                (unless (eq p focus)
                  (draw-layer plotter p))))
            (draw-layer plotter focus)))
        (when selection
          (draw-selection plotter selection :redraw NIL))
        ;; draw axis display
        (draw-graduals plotter :axis))
      ;; clear redraw flag
      (setf (plotter-draw plotter) NIL))

    ;; now get the visible screen area and bitblit the bitmap to the
    ;; drawing area. the call to begin-paint updates the screen
    ;; display to offscreen to avoid fickering while scrolling.
    (Gdk:Rectangle.x rect sleft)
    (Gdk:Rectangle.y rect stop)
    (Gdk:Rectangle.width rect swidth)
    (Gdk:Rectangle.height rect sheight)
    (gdk:window-begin-paint-rect win rect)

    ;; Updating the visible screen area is done in three steps:
    ;; 1. fill the whole screen area with the exact same area in
    ;; the plotting bitmap.
    ;; 2. copy the bitmap's axis displays into the left and bottom
    ;; portions of the screen area.
    ;; 3. erase the lower left corner to remove any shifted axis
    ;; display due to scrolling
    (gdk:draw-drawable win gc bitmap sleft stop sleft stop
                       swidth sheight)
    ;; copy the Y axis area in the bitmap to the left edge of the
    ;; screen area
    (gdk:draw-drawable win gc bitmap 
                       0 
                       stop
                       sleft
                       stop
                       (-  ylab ypad) ;;***
                       sheight)
    ;; copy the X axis area that lies just below the gridline in the
    ;; bitmap to the corresponding position on the screen. add 1 to
    ;; avoid including the very bottom grid line in the axis area
    (gdk:draw-drawable win gc bitmap 
                       sleft
                       (+ (+ (axis-offset yaxis) 1) xpad) ;;***
                       sleft
                       (+ (+ sgridbot 1) xpad) ;;***
                       swidth
                       (-  xlab xpad) ;;***
                       )
    ;; erase the lower left corner of the displayed area to remove any
    ;; X/Y axes extending left/below the grid.
    (gdk:draw-rectangle win wgc t
                        sleft
                        (+ (+ sgridbot 1) xpad) ;;***
                        (- ylab ypad ) ;***
                        (- (- xlab 1) xpad) ;***
                        )
    
    (gdk:window-end-paint win)
    gtk:+false+))

;;; draw-graduals draws both the background grid underneath the layers
;;; as well as the axis displays on the left and underneath the
;;; layers. unfortunately, these operations cannot be accomplished in a
;;; single call because gridlines must be drawn 'behind' the layers but
;;; the axis drawing must occur AFTER the layers so that they can clip
;;; overhanging points from appearing as part of the axis displays.
;;; for example, a point with a center at 0,0 will extend into the
;;; axis area by half its diameter but these overhanging pixels should
;;; not appear in the axis display when it is scrolled.

(defun draw-graduals (plotter where)
  ;; this routine draws both the grid and the axis displays.  it
  ;; calculates everthing in "axis" coordinates and then converts to
  ;; pixel position only for drawning calls.
  (let* ((bitmap (plotter-bitmap plotter))
         (gc (plotter-gc plotter))
         (ctable (plotter-colors plotter))
         (black (drawing-color "black" ctable))
         (white (drawing-color "white" ctable))
         (gray1 (drawing-color "dark gray" ctable))
         (gray2 (drawing-color "light gray" ctable))
         (x-axis (plotter-x-axis plotter))
         (y-axis (plotter-y-axis plotter))
         (ybottom (axis-offset y-axis))
         (ytop (- ybottom (axis-size y-axis)))
         (xleft (axis-offset x-axis))
         (xright (+ xleft (axis-size x-axis)))
         majc minc            ;; major/minor line colors
         majl majr majt majb  ;; major line coords
         minl minr mint minb  ;; minor line coords
         layout xlabel ylabel)
    ;; if 'where' is :axis then we are drawing the label display
    (if (eq where ':axis)
      (let* ((darea  (plotter-drawing-area plotter))
             ;;(rect (g:object-get-data darea "user_data"))
             ;;(focus (plotter-front-layer plotter))
             (inset *axis-inset*)
             (lsiz 2))
        (setq layout (g:object-get-data darea "layout"))
        (setf xlabel (axis-labeler x-axis))
        (setf ylabel (axis-labeler y-axis))
        ;; for axis drawing the major and minor line position are
        ;; different and line colors are the same
        ;; try to use the focus layer's color, else black.
        (setf majc BLACK minc majc)
        (setf majr (- xleft inset) majl (- majr 6)
              minr majr minl (- majr 3))
        (setf majt (+ ybottom inset) majb (+ majt 6)
              mint majt minb (+ majt 3))
        ;; erase leftward and downward from grid lines to 
        ;; clip overhaning point display. this should be fixed...
        (gdk:gc-set-foreground gc white)
        (erase-bitmap bitmap gc 0 0 (pixel (- xleft *axis-padding*))
                      (gtk:Widget.allocation.height darea))
        (erase-bitmap bitmap gc 0 (pixel (+ ybottom *axis-padding*))
                      (gtk:Widget.allocation.width darea)
                      (gtk:Widget.allocation.height darea))
        (gdk:gc-set-foreground gc black)
        (gdk:gc-set-line-attributes gc lsiz 0 0 0)
        ;; draw x axis below
        (gdk:draw-line bitmap gc (pixel xleft) (pixel (+ ybottom inset))
                       (pixel xright) (pixel (+ ybottom inset)))
        ;; y axis to left
        (gdk:draw-line bitmap gc (pixel (- xleft inset)) (pixel ybottom)
                       (pixel (- xleft inset)) (pixel ytop))
        (gdk:gc-set-line-attributes gc 1 0 0 0))
      (progn
        ;; ...else we are drawing the grid itself in which case the
        ;; major and minor line positions are the same but their
        ;; colors are different
        (setf majc gray1 minc gray2)
        (setf majl xleft majr xright majt ytop majb ybottom
              minl xleft minr xright mint ytop minb ybottom)))

    (gdk:gc-set-foreground gc majc)
    ;; dray y axis grid or labeldisplay
    (draw-dimension bitmap gc y-axis ylabel majc minc layout
                    majl majb majr majt
                    minl minb minr mint)
    ;; draw x axis grid or label display
    (draw-dimension bitmap gc x-axis xlabel majc minc layout
                    majl majb majr majt
                    minl minb minr mint)
    (gdk:gc-set-foreground gc black)
    (values)))

(defun draw-dimension (bitmap gc axis labeler majc minc layout 
                              majl majb majr majt
                              minl minb minr mint)
  ;; called to draw the grid or the axis display for each axis the
  ;; majl...mint valus are in pixels and represent 'constant' values
  ;; from the other axis dimension as this axis is being drawn.
  (let* ((vert (eq (axis-orientation axis) ':vertical))
         (amin (axis-minimum axis))
         (amax (axis-maximum axis))
         (arng (- amax amin))
         (atpi (axis-ticks-per-increment axis))
         (aval amin)
         (i 0)
         apix)
    (unless (axis-draw-labels? axis)
      (setq labeler nil))
    ;; if labeling, right justify vertical and center horizontal
    (when labeler
      (pango:layout-set-alignment layout 
                                  (if vert pango:align-right
                                      pango:align-center)))
    (loop do (setq aval (axis-value-at-increment axis i 0))
      while (<= aval amax)
      do
      (setq apix (pixel (axis-pixel aval axis vert NIL)))
      (when labeler
        (draw-label bitmap gc 
                    (if vert MAJL apix)
                    (if vert apix MAJB)
                    layout
                    (/ (- aval amin) arng)
                    labeler aval vert))
      (gdk:draw-line bitmap gc
                     (if vert (pixel MAJL) apix)
                     (if vert apix (pixel MAJB))
                     (if vert (pixel MAJR) apix)
                     (if vert apix (pixel MAJT)))
      ;; draw light lines or small ticks if more than 1 tick
      (when (> atpi 1)
        (gdk:gc-set-foreground gc MINC)
        (loop for j from 1 below atpi
              do (setq aval (axis-value-at-increment axis i j))
              while (< aval amax) ; stop if on max line
              do
              (setq apix (pixel (axis-pixel aval axis vert NIL)))
              (gdk:draw-line bitmap gc
                             (if vert (pixel MINL) apix)
                             (if vert apix (pixel MINB))
                             (if vert (pixel MINR) apix)
                             (if vert apix (pixel MINT))))
        (gdk:gc-set-foreground gc majc))
      (incf i)
      )))

(defun draw-label (bitmap gc x y layout pct labeler value vert)
  ;; if vert is true then label is being drawn on y axis. pct moves
  ;; from zero to 1 over the course of the axis drawing and is used to
  ;; adjust label positions so they remain within the non-clipped
  ;; regions of the axis displays.
  (let ((text (if (stringp labeler)
                (format nil labeler value)
                (funcall labeler value)))
        width height void)
    void
    (pango:layout-set-text layout text -1)
    (multiple-value-setq (void width height)
      (pango:layout-get-pixel-size layout 0 0))
    (gdk:draw-layout bitmap gc
                     (if vert (pixel (- x width 4) )
                         (pixel (- x (* width pct))))
                     (if vert (pixel (- y (* height (- 1 pct))))
                         (pixel (+ y 4)))                           
                     layout)
    (values)))

;;;
;;;
;;;

(defun draw-layer (plotter layer &key (points-filled t) (pen-mode gdk:copy)
                           points color points-only (dx 0) (dy 0) gc)
  (let* ((data (or points (layer-data layer)))
         (x-axis (plotter-x-axis plotter))
         (y-axis (plotter-y-axis plotter))
         (x-slots (axis-plotting-slots-for-layer x-axis layer))
         (y-slots (axis-plotting-slots-for-layer y-axis layer)))
    (if (and data x-slots y-slots)
      (let* ((bitmap (plotter-bitmap plotter))
             (area (plotter-drawing-area plotter))
             (gstyle (plotter-style plotter))
             (pstyle (layer-style layer))
             (zoomp? (plotter-property plotter :zoom-points))
             (zooml? (plotter-property plotter :zoom-lines))
             (point-w (floor (* (styling-point-width pstyle gstyle)
                                (if zoomp? (axis-zoom x-axis)
                                    1))))
             (point-h (floor (* (styling-point-height pstyle gstyle)
                                (if zoomp? (axis-zoom y-axis)
                                    1))))
             (linew (floor (* (styling-line-width pstyle gstyle)
                              (if zooml? (axis-zoom x-axis) 1))))
             (view (styling-view pstyle gstyle)))
        (declare (ignore area))
        (unless gc 
          (setf gc (plotter-gc plotter)))
        (unless color
          (setq color (drawing-color (styling-color pstyle gstyle)
                                     (plotter-colors plotter))))
        ;; set up layers color and line-size
        (gdk:gc-set-foreground gc color)
        (gdk:gc-set-line-attributes gc linew 0 0 0)
        (gdk:gc-set-function gc pen-mode)
;        (IF (EQ PEN-MODE GDK:XOR)
;          (SETQ BITMAP (GTKWIDGET.WINDOW (PLOTTER-DRAWING-AREA PLOTTER))))
        (case view
          ((:line-and-point :line :point :bar :bar-and-point)
           (when points-only (setf view ':point))
           (draw-line-and-point bitmap gc x-axis y-axis x-slots y-slots
                                data point-w point-h linew
                                points-filled view dx dy))
          ((:box :bubble)
           (draw-box-and-bubble bitmap gc x-axis y-axis x-slots y-slots
                                data point-w point-h linew
                                points-filled view dx dy))
          (t nil))
        (gdk:gc-set-function gc gdk:copy)
        ))
    (values)))

;;;
;;; %360 is definition of full circle in gtk, where degrees are
;;; measured in increments of 1/64 degree.
;;;

(defconstant %360 (* 360 64))

(defun draw-line-and-point (bitmap gc x-axis y-axis x-slots y-slots
                                     points point-w point-h line-w
                                     filled? view dx dy)
  dx dy
  (let ((x-slot (car x-slots)) ; ignore width or height slots
        (y-slot (car y-slots))
        (half-w (floor point-w 2)) ; offset for centering point
        (half-h (floor point-h 2))
        x1 y1 x2 y2)
    (ecase view 
      (:line-and-point
       (dolist (p points)
         (setq x1 (xpixel p x-slot x-axis))
         (setq y1 (ypixel p y-slot y-axis))
         (gdk:draw-arc bitmap gc filled? (- x1 half-w dx) (- y1 half-h dy)
                       point-w point-h 0 %360)
         (if x2 (gdk:draw-line bitmap gc x2 y2 x1 y1))
         (setq x2 x1 y2 y1)))
      (:point
       (dolist (p points)
         (setq x1 (xpixel p x-slot x-axis))
         (setq y1 (ypixel p y-slot y-axis))
         (gdk:draw-arc bitmap gc filled? (- x1 half-w dx) (- y1 half-h dy)
                       point-w point-h 0 %360)))
      (:line
       (setq x2 (xpixel (car points) x-slot x-axis))
       (setq y2 (ypixel (car points) y-slot y-axis))
       (dolist (p (cdr points))
         (setq x1 (xpixel p x-slot x-axis))
         (setq y1 (ypixel p y-slot y-axis))
         (gdk:draw-line bitmap gc x2 y2 x1 y1)
         (setq x2 x1 y2 y1)))
      ((:bar :bar-and-point)
       (let* ((min (axis-minimum y-axis))
              (max (axis-maximum y-axis))
              ;; use zero as origin if at all possible.
              (mid (FLOOR (if (or (= min 0) (= max 0)
                           (< min 0 max)) 0
                           (/ (- min max) 2))))
              ;; center bar line too.
              (half-l (floor line-w 2)))
         (setq y2 (ROUND (axis-pixel mid y-axis t)))
         (dolist (p points)
           (setq x1 (xpixel p x-slot x-axis))
           (setq y1 (ypixel p y-slot y-axis))
           (if (eq view ':bar-and-point)
           (gdk:draw-arc bitmap gc filled? (- x1 half-w dx) 
                         (- y1 half-h dy)
                         point-w point-h 0 %360))
           (gdk:draw-line bitmap gc (- x1 half-l) y1 (- x1 half-l) y2)))))
    (values)))
      
(defun draw-box-and-bubble (bitmap gc x-axis y-axis x-slots y-slots
                            points point-w point-h line-w
                            filled? view dx dy)
  (declare (ignore line-w ))
  (let ((x-slot (car x-slots))
        (y-slot (car y-slots))
        (w-slot (cadr x-slots))
        (h-slot (cadr y-slots))
        (half-w (floor point-w 2))
        (half-h (floor point-h 2))
        (w point-w)
        (h point-h)
        x y)
    (if w-slot
      (if h-slot
        ;; x,y,w,h
        (if (eql view ':bubble)
          (dolist (p points)  
            (setq w (xpixel p w-slot x-axis t))
            (setq h (ypixel p h-slot y-axis t))
            ;; center x and y
            (setq x (- (xpixel p x-slot x-axis)
                       (floor w 2)
                       dx))
            (setq y (- (ypixel p y-slot y-axis)
                       (floor h 2)
                       dy))
            (gdk:draw-arc bitmap gc filled? x y w h 0 %360))
          (dolist (p points)  
            (setq x (xpixel p x-slot x-axis))
            (setq h (ypixel p h-slot y-axis t))
            (setq y (+ (ypixel p y-slot y-axis)
                       h))
            (setq w (xpixel p w-slot x-axis t))
            (gdk:draw-rectangle bitmap gc filled? x y w h)))
        ;; x,y,w
        (if (eql view ':bubble)
          (dolist (p points)
            (setq w (xpixel p w-slot x-axis t))
            (setq h w) ;; hmm, axes better be same!
            (setq half-w (floor w 2))
            ;; center x and y
            (setq x (- (xpixel p x-slot x-axis)
                       dx half-w))
            (setq y (- (ypixel p y-slot y-axis)
                       dy half-h))
            (gdk:draw-arc bitmap gc filled? x y w h 0 %360))
          (dolist (p points)
            (setq x (xpixel p x-slot x-axis))
            (setq y (ypixel p y-slot y-axis))
            (setq w (xpixel p w-slot x-axis t))
            (gdk:draw-rectangle bitmap gc filled? (- x dx)
                                (- y half-h dy)
                                w h))))
      (if h-slot                       
        ;; x,y,h
        (if (eql view ':bubble)
          (dolist (p points)
            (setq h (ypixel p h-slot y-axis t))
            (setq w h) ;; hmm, axes better be same!
            (setq half-h (floor h 2))
            ;; center x and y
            (setq x (- (xpixel p x-slot x-axis)
                       half-h
                       dx))
            (setq y (- (ypixel p y-slot y-axis)
                       half-h
                       dy))
            (gdk:draw-arc bitmap gc filled? x y w h 0 %360))
          (dolist (p points)
            (setq x (xpixel p x-slot x-axis))
            (setq h (ypixel p h-slot y-axis t)) ;***
            (setq y (+ (ypixel p y-slot y-axis)
                       h))
            (gdk:draw-rectangle bitmap gc filled? (- x dx)
                                (- y dy)
                                (- w half-w) h)))
        nil))))
;;;
;;; main plotter window...
;;;

(defun drawing-color (name colortable)
  ;; returns a color from the color table given a valid colorname
  ;; (rgb.text) If the color does not yet exist then allocate it in
  ;; the colormap.
  (or (gethash name colortable)
      (let ((colormap (gethash ':colormap colortable)))
        (if (stringp name)
          (let ((cptr (gtk:struct-alloc :<G>dk<C>olor)))
            (if (eql gtk:+false+ (gdk:color-parse name cptr))
              (error "Can't parse color ~S." name))
            (gdk:colormap-alloc-color colormap cptr t t)
            (setf (gethash name colortable) cptr)
            cptr)
          (error "~S is not a defined color in rgb.text"
                 name)))))

; (Gtk:Style.font-desc (gtk:Widget.style widget))
; (pango:font-description-to-string fd)
      
(defun plotting-colormap (plotter)
  (gethash ':colormap (plotter-colors plotter)))

(defun allocate-plotting-data (plotter)
  ;; insure global styling properties, including the color hashtable for
  ;; fast lookup and the private colormam from which gdk colors are 
  ;; "allocated". insure all layer styles as well.
  (let* ((gstyle (plotter-style plotter))
         (colormap (gdk:colormap-new (gdk:visual-get-system) t))
         (ctable (make-hash-table :test #'equal)))
    (setf (plotter-colors plotter) ctable)
    ;; store gdk colormap in the table under a special hashkey
    (setf (gethash ':colormap ctable) colormap)
    ;; now allocate colors that plotter itself uses.
    (dolist (c '("white" "black" "light gray" 
                 "gray" "dark gray"))
      (drawing-color c ctable))
    ;; allocate the global color if any
    (when (%style-color (plotter-style plotter))
      (drawing-color (%style-color (plotter-style plotter))
                     ctable))
    ;; allocate individual layer colors too.
    (dolist (p (plotter-layers plotter))
      (when (%style-color (layer-style p))
        (drawing-color (%style-color (layer-style p))
                       ctable)))
    ;; set the font of the cached label layout to global font.
    (let ((layout (g:object-get-data
                   (plotter-drawing-area plotter)
                   "layout")))
      (pango:layout-set-font-description 
       layout
       (pango:font-description-from-string
        (%style-font gstyle))))
    (values)))
                                            
(defun insure-drawing-sizes (area x-axis y-axis)
  (unless (axis-zoom x-axis) (setf (axis-zoom x-axis) 1))
  (unless (axis-zoom y-axis) (setf (axis-zoom y-axis) 1))
  (unless (axis-pixels-per-increment x-axis)
    (setf (axis-pixels-per-increment x-axis)
          *default-pixels-per-increment*))
  (unless (axis-pixels-per-increment y-axis)
    (setf (axis-pixels-per-increment y-axis)
          *default-pixels-per-increment*))
  (unless (axis-label-area x-axis)
    (setf (axis-label-area x-axis) *label-area-height*))
  (unless (axis-label-area y-axis)
    (setf (axis-label-area y-axis) *label-area-width*))
  (let* ((xoffset (axis-label-area x-axis))
         (yoffset (axis-label-area y-axis))
         (xpad *axis-padding*)
         (ypad *axis-padding*)
         ;; do i still assume this round??
         (xextent (ROUND (axis-size x-axis)))
         (yextent (ROUND (axis-size y-axis)))
         ;; padding is added to the left or below the grid and
         ;; stolen from the label area.
         (totalx (+ xoffset xextent xpad))
         (totaly (+ yoffset yextent ypad)))
    (setf (axis-offset x-axis) xoffset)
    (setf (axis-offset y-axis) (- totaly yoffset))
    (gtk:widget-set-size-request area totalx totaly)
    (values)))

(defun zoom-for-page-size (axis scroller)
  (let* ((offset (axis-label-area axis))
         (pagesz (Gtk:Adjustment.page-size scroller))
         (extent (- pagesz (+ *axis-padding* offset))))
    ;; extent=zoom*ppi*increments
    ;; zoom=extent/(ppi*increments)
    (/ extent (* (axis-increments axis)
                 (axis-pixels-per-increment axis)))))

;;;
;;; top-level plotter functions
;;;

(defun display-plotter (plotter)
  (gtk:init-ensure) ; make sure gtk is inited before doing anything.
  ;; allocate gtk structures 
  (let ((x-axis (plotter-x-axis plotter))
        (y-axis (plotter-y-axis plotter))
        window hints mask vbox menubar scrolled drawing-area )
    (setq window (gtk:window-new gtk:window-toplevel))
;    (gtk:widget-set-size-request window 400 400)
    ;; link our plotter object to the new main window. we will pass
    ;; the window as user data to all the plotting callbacks so the
    ;; various drawing routines can quickly access any info they might
    ;; need.
    (setf (widget->object window) plotter)
    (setf (plotter-window plotter) window)
    (gtk:window-set-title window (or (object-name plotter) ""))
    ;; now set some geometry in an attempt to stop a random GDK error
    ;; message about a null window from some geometry functin. this
    ;; error only seems to happen when menus are added.
    (setq hints (gtk:struct-alloc :<G>dk<G>eometry
                           :min_width 400 :min_height 400
                           :max_width -1 :max_height -1))
    (setq mask (logior gdk:hint-min-size)) ; just min for now
    (gtk:window-set-geometry-hints window (g:nullptr) hints mask)
    (gtk:container-set-border-width window 5)
    (g:signal-connect window "destroy"
                      (g:callback destroy-plotter-window)
                      window)
    ;; create vbox to partition window into menubar, drawing area,
    ;; and typein buffer (not yet implmented)
    (setq vbox (gtk:vbox-new nil 5))
    (gtk:container-add window vbox)
    ;; try telling the vbox its size before menues are added.
    (gtk:window-set-geometry-hints window vbox hints mask)
    (gtk:widget-show vbox)
    ;; create menubar and add to top of vbox
    (setq menubar (create-menubar *menubar* window vbox))

    (setf (plotter-menubar plotter) menubar)
    ;; create drawing area. drawing signal handlers are passed main
    ;; window as user data
    (setq drawing-area (gtk:drawing-area-new))
    (setf (plotter-drawing-area plotter) drawing-area)
    (setf (plotter-bitmap plotter) nil) 

    ;; set values in pixel slots of axes and resize the drawing area
    ;; to fit the plotting geometry
    (insure-drawing-sizes drawing-area x-axis y-axis)

    (gtk:widget-show drawing-area)
    (g:signal-connect drawing-area "expose_event" 
                      (g:callback expose-event)
                      window)
    (g:signal-connect drawing-area "configure_event" 
                      (g:callback configure-event)
                      window)
    (g:signal-connect drawing-area "button_press_event" 
                      (g:callback button-press-event) window)
    (g:signal-connect drawing-area "button_release_event" 
                      (g:callback button-release-event) window)
    (g:signal-connect drawing-area "motion_notify_event" 
                      (g:callback motion-notify-event) window)
    (g:signal-connect window "key_press_event"
                      (g:callback key-press-event) window)
    ;; cache a rect for expose_event to use.
    (g:object-set-data drawing-area "user_data"
                       (gtk:struct-alloc :<G>dk<R>ectangle))
    ;; create a pango layout for drawing axis labels and cache it in
    ;; the drawing-area.
    (g:object-set-data drawing-area "layout"
                       (gtk:widget-create-pango-layout drawing-area ""))
    (gtk:widget-set-events drawing-area
                           (logior gdk:exposure-mask
                                   gdk:leave-notify-mask
                                   gdk:button-press-mask
                                   gdk:button-release-mask
                                   gdk:pointer-motion-mask
                                   gdk:pointer-motion-hint-mask
                                   ))
    (gtk:widget-set-events window gdk:focus-change-mask)
    (g:signal-connect window "focus_in_event"
                      (g:callback focus-in-event) window)

    ;; create scrolled window and add drawing area to it
    (setq scrolled (gtk:scrolled-window-new 
                    (g:nullptr) (g:nullptr)))
    (gtk:widget-show scrolled)
    (gtk:scrolled-window-add-with-viewport scrolled drawing-area)
    (gtk:scrolled-window-set-policy scrolled
                                    gtk:policy-always
                                    gtk:policy-always)
    ;; cache the scrollers and set their step increment
    ;; to the tick size.
    (let ((hscroll (gtk:scrolled-window-get-hadjustment scrolled))
          (vscroll (gtk:scrolled-window-get-vadjustment scrolled)))
      (setf (plotter-x-scroller plotter) hscroll
            (plotter-y-scroller plotter) vscroll)
      ;; n.b. fix the accessors!
      (Gtk:Adjustment.step-increment
       hscroll (coerce (axis-tick-size x-axis)'double-float))
      (gtk:adjustment-changed hscroll)
      (Gtk:Adjustment.step-increment
       vscroll (coerce (axis-tick-size y-axis) 'double-float))
      (gtk:adjustment-changed vscroll))
    ;; add scroller to vbox
    (gtk:box-pack-start vbox scrolled t t 0)
    ;; allocate private colormap, gc, plotting colors etc.
    (allocate-plotting-data plotter)
    (setf (plotter-draw plotter) t)
    ;; ensure mode starts as :select-points
    (setf (mouseinfo-mode (plotter-mouseinfo plotter)) :select-points)
    (gtk:widget-show window)
    ;; do this here so darea has window?
    (setf (plotter-gc plotter)
          (gdk:gc-new (gtk:Widget.window drawing-area)))
    ;; scroll to origin on y axis
    (plotter-scroll plotter :y 1)
    ;; start main loop if necessary
    (push plotter *gtk-open-toplevels*)
    ;;(unless *gtk-main*
    ;;(setq *gtk-main* t)
    ;;(gtk:main))
    (gtk-main-start) ; support.lisp
    (values)))

(defun plotter-zoom (plotter &key x y )
  ;; get/set the curren zoom values of plotter
  ;; if x or y are T then calculate zoom to fit
  ;; current scrolling page size.
  (let ((x-axis (plotter-x-axis plotter))
        (y-axis (plotter-y-axis plotter)))
    (when x
      (when (eq x t)
        (setf x (zoom-for-page-size x-axis
                                    (plotter-x-scroller plotter))))
      (if (and (numberp x) (> x 0))
        (setf (axis-zoom x-axis) x)
        (error "Zoom scaler not a positive number: ~S."
               x)))
    (when y
      (when (eq y t)
        (setf y (zoom-for-page-size y-axis
                                    (plotter-y-scroller plotter))))
      (if (and (numberp y) (> y 0))
        (setf (axis-zoom y-axis) y)
        (error "Zoom scaler not a positive number: ~S."
               y)))
    (when (or x y)
      (insure-drawing-sizes (plotter-drawing-area plotter)
                            x-axis y-axis))
    (values (axis-zoom x-axis)
            (axis-zoom y-axis))))

(defun plotter-redraw (plotter &optional (redraw t))
  ;; signal plotter's drawing area to redraw plots.
  (when redraw
    (setf (plotter-draw plotter) redraw))
  (let ((area (plotter-drawing-area plotter)))
    (gtk:widget-queue-draw area))
  (values))

(defun plotter-close (plotter)
  (gtk:widget-destroy (plotter-window plotter)))

(defun plotter-resize (plotter &key width height)
  plotter width height
  ())

(defun plotter-title (plotter &optional title)
  (let ((win (plotter-window plotter)))
    (if title
      (progn (gtk:window-set-title win title)
             title)
      (gtk:window-get-title win))))

(defun plotter-scroll (pw &key x y)
  (let (h v r)
    (when x
      (setq h (plotter-x-scroller pw))
      (setq x (max (min x 1) 0))
      (setq r (- (Gtk:Adjustment.upper h)
                 (Gtk:Adjustment.page-size h)))
      (gtk:adjustment-set-value h (* r x))
      (gtk:adjustment-value-changed h))
    (when y
      (setq v (plotter-y-scroller pw))
      (setq y (max (min y 1) 0))
      (setq r (- (Gtk:Adjustment.upper v)
                 (Gtk:Adjustment.page-size v)))
      (gtk:adjustment-set-value v (* r y))
      (gtk:adjustment-value-changed v))
    (values)))



