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
;;; $Revision: 1.7 $
;;; $Date: 2005/01/01 17:51:54 $

;;; Auxilliary widgets: menubar, tool windows, etc.

(in-package :cm)

(defun safe-read? (str)
  ;; fix this hack!
  (if (and (not (equal str ""))
           (= (count #\( str) (count #\) str))
           (evenp (count #\" str)))
    (multiple-value-bind (a b)
        (read-from-string str)
      (if (= b (length str)) a :err))
    :err))

;;;
;;; Zoom Tool 
;;;

(gtk:define-signal-handler zoom-destroy :void (tool)
  ;; this callback is called when you close the zoom window.
  ;; this will have to remove window from plotter's window list.
  (let ((plotter (widget-property tool ':plotter)))
    (when plotter
      (setf (plotter-zooming-tool plotter) nil)))
  (remove-widget tool)
  (values))

;;;
;;; zoom radio button callbacks

(gtk:define-signal-handler zoom-both-clicked :void (button tool)
  ;; this callback is attached to the Both radio button.
  button                       
  (setf (widget-property tool ':active-axis) :both)
  (radio-reset-zoom tool ':both))

(gtk:define-signal-handler zoom-horizontal-clicked :void (button tool)
  ;; this callback is attached to the Horizontal radio button.
  button
  (setf (widget-property tool ':active-axis) :horizontal)
  (radio-reset-zoom tool ':horizontal))

(gtk:define-signal-handler zoom-vertical-clicked :void (button tool)
  ;; this callback is attached to the Vertical radio button.
  button
  (setf (widget-property tool ':active-axis) :vertical)
  (radio-reset-zoom tool ':vertical))

(defun radio-reset-zoom (tool radio)
  ;; called by the radio buttons to update the axis slider and
  ;; spin values and desensitize the Fit button when Both is
  ;; selected...
  (let* ((plotter (widget-property tool ':plotter))
         (fit (widget-property tool ':fit))
         zoom)
    ;;(print (list :radio-> radio))
    (ecase radio
      (:both 
       (gtk:widget-set-sensitive fit nil)
       (setf zoom (axis-zoom (plotter-x-axis plotter)))
       (let ((other (axis-zoom (plotter-y-axis plotter))))
         (unless (= zoom other)
           (setf zoom nil)
           (print :shouldnt-happen))))
      (:horizontal
       (gtk:widget-set-sensitive fit t)
       (setf zoom (axis-zoom (plotter-x-axis plotter))))
      (:vertical 
       (gtk:widget-set-sensitive fit t)
       (setf zoom (axis-zoom (plotter-y-axis plotter)))))
    (if zoom
      (set-zoom-slider (widget-property tool ':slider) zoom))))

(defun set-zoom-slider (slider value)
  (gtk:adjustment-set-value (gtk:range-get-adjustment slider)
                            (log value 2)))

;;;
;;; Zoom value callbacks

(gtk:define-signal-handler zoom-spin-changed :void (adjust tool)
  ;; the spin button is the main zooming control and this callback is
  ;; attached to its Gtk:Adjustment. both the slider and 100% button
  ;; chain their values forward to trigger this callback.  BUG: This
  ;; callback should also update the slider but I can't figure out how
  ;; to do this in gtk without deadlocking since the signals then call
  ;; each other.
  adjust
  (do-zoom tool))

(gtk:define-signal-handler zoom-slider-changed :void (adjust spin)
  ;; this callback is attached to the slider's Gtk:Adjustment, whose
  ;; value is an exponent ranging from -2 to 2 (25%-400% zoom). we
  ;; convert the exponent value into a zoom percentage and send it
  ;; forward to the spin button.
  ;(print :zoom-slider-changed)
  adjust spin
  (let ((val (gtk:adjustment-get-value adjust)))
    (gtk:spin-button-set-value spin (* 100 (expt 2.0 val)))))

(gtk:define-signal-handler zoom-100-clicked :void (button slider)
  ;; this callback is attached to the 100% button.  it sends a
  ;; zero exponent value forward to the slider.
  ;; BUG: IF spin is not 100 but slider is then this will have
  ;; no effect.
  button
  (set-zoom-slider slider 1.0)
  (values))

(gtk:define-signal-handler zoom-fit-clicked :void (button tool)
  button
  (let ((plotter (widget-property tool ':plotter))
         zoom)
    (case (widget-property tool ':active-axis)
      (:horizontal
       (setq zoom (zoom-for-page-size (plotter-x-axis plotter)
                                      (plotter-x-scroller plotter))))
      (:vertical
       (setq zoom (zoom-for-page-size (plotter-y-axis plotter)
                                      (plotter-y-scroller plotter)))))
    (if zoom
      (set-zoom-slider (widget-property tool ':slider) zoom))
    (values)))
      
(gtk:define-signal-handler zoom-policy-points :void (check tool)
  (let ((plotter (widget-property tool ':plotter)))
    (plotter-property plotter ':zoom-points
                      (gtk:toggle-button-get-active check)
                      :redraw nil)
    (values)))

(gtk:define-signal-handler zoom-policy-lines :void (check tool)
  (let ((plotter (widget-property tool ':plotter)))
    (plotter-property plotter ':zoom-lines
                      (gtk:toggle-button-get-active check)
                      :redraw nil)
    (values)))

(defun zooming-tool (plotter)
  (let* ((window (gtk:window-new gtk:window-toplevel))
         (tool window)
         vbox frame hbox button spin reset fit slider cbut)

    ;; ...and add the plotter to the tool's prop list
    (setf (widget->object tool) nil)
    (setf (widget-property tool :plotter) plotter)

    (gtk:window-set-default-size window 300 -1)
    (gtk:window-set-title window "Zooming")
    (g:signal-connect window "destroy" (g:callback zoom-destroy)
                      (g:nullptr))
    (gtk:container-set-border-width window 10)
    ;; window layout vbox: nonhomo w. 5pix separation
    (setq vbox (gtk:vbox-new nil 5))
    (gtk:container-add window vbox)
    (gtk:widget-show vbox)

    ;; Axis selection radio buttons
    (setq frame (gtk:frame-new "Zoom Axis"))
    (gtk:box-pack-start vbox frame t t 0)    
    (gtk:widget-show frame)
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:container-add frame hbox)
    (gtk:widget-show hbox)
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "Both"))
    (setf (widget-property tool ':both) button)
    (gtk:box-pack-start hbox button t t 0)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Horizontal"))
    (setf (widget-property tool ':horizontal) button)
    (gtk:box-pack-start hbox button t t 0)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Vertical"))
    (setf (widget-property tool ':Vertical) button)
    (gtk:box-pack-start hbox button t t 0)
    (gtk:widget-show button)

    ;; Zoom value frame
    (setq frame (gtk:frame-new "Zoom Value"))
    (gtk:box-pack-start vbox frame t t 0)    
    (gtk:widget-show frame)
    (setq hbox (gtk:hbox-new t 0))
    (gtk:container-add frame hbox)
    (gtk:widget-show hbox)
    ;; create and cache spin button for percent zoom selection
    (setq spin (gtk:spin-button-new-with-range 25.0 400.0 1.0))
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin t t 5)
    (gtk:widget-show spin)
    (setf (widget-property tool :spin) spin)    
    (setf (widget-property tool :spin-adjustment) 
          (gtk:spin-button-get-adjustment spin))
    ;; create and cache exponential slider
    (setq slider (gtk:hscale-new-with-range -2.0 2.0 .01))
    (gtk:range-set-value slider -2)
    (gtk:scale-set-draw-value slider nil)
    (gtk:box-pack-start hbox slider t t 5)
    (gtk:widget-show slider)
    (setf (widget-property tool :slider) slider)
    ;; reset 100% button
    (setq reset (gtk:button-new-with-label "100%"))
    (gtk:box-pack-start hbox reset t t 5)
    (gtk:widget-show reset)
    (setf (widget-property tool :reset) reset)    
    ;; fit  button
    (setq fit (gtk:button-new-with-label "Fit"))
    (gtk:box-pack-start hbox fit t t 5)
    (gtk:widget-show fit)
    (setf (widget-property tool :fit) fit)    
    ;; Zoom Policy frame
    (setq frame (gtk:frame-new "Zoom Policy"))
    (gtk:box-pack-start vbox frame t t 0)    
    (gtk:widget-show frame)
    (setq hbox (gtk:hbox-new t 0))
    (gtk:container-add frame hbox)
    (gtk:widget-show hbox)
    (setq cbut (gtk:check-button-new-with-label "Zoom Points"))
    (gtk:box-pack-start hbox cbut t t 0)    
    (gtk:widget-show cbut)
    (gtk:toggle-button-set-active 
     cbut (plotter-property plotter :zoom-points))
    (g:signal-connect cbut "clicked" (g:callback zoom-policy-points)
                      window)
    (setq cbut (gtk:check-button-new-with-label "Zoom Lines"))
    (gtk:box-pack-start hbox cbut t t 0)    
    (gtk:widget-show cbut)
    (gtk:toggle-button-set-active 
     cbut (plotter-property plotter :zoom-lines))
    (g:signal-connect cbut "clicked"
                      (g:callback zoom-policy-lines)
                      window)
    ;; connect zoom-spin-changed to the spin button's adjustment and
    ;; pass it the main window.
    (g:signal-connect (gtk:spin-button-get-adjustment spin)
                      "value_changed" 
                      (g:callback zoom-spin-changed)
                      window)
    ;; connect zoom-slider changed to the sliders adjustment and pass
    ;; it the spin button's adjustment.
    (g:signal-connect (gtk:range-get-adjustment slider)
                      "value_changed" 
                      (g:callback zoom-slider-changed)
                      spin)
    ;; pass the slider to the 100% button. clicking the
    ;; button chains the reset 100%->slider->spin.
    (g:signal-connect reset "clicked" 
                      (g:callback zoom-100-clicked)
                      slider)
    ;; pass the window to the fit button. clicking the
    ;; button chains fit->slider->spin.
    (g:signal-connect fit "clicked" 
                      (g:callback zoom-fit-clicked)
                      window)
    ;; connect the click callbacks to the radio buttons
    (g:signal-connect (widget-property tool ':both)
                      "clicked"
                      (g:callback zoom-both-clicked)
                      window)
    (g:signal-connect (widget-property tool ':horizontal)
                      "clicked"
                      (g:callback zoom-horizontal-clicked)
                      window)
    (g:signal-connect (widget-property tool ':vertical)
                      "clicked"
                      (g:callback zoom-vertical-clicked)
                      window)
    ;; connect zoom policy

    (gtk:widget-show window)
    ;; try to select both, else
    (multiple-value-bind (zx zy) (plotter-zoom plotter)
      (let ((radio (if (= zx zy) :both :horizontal)))
        (gtk:toggle-button-set-active (widget-property tool radio)
                                      t)
        (setf (widget-property tool ':active-axis) radio)
        (radio-reset-zoom tool radio)))
    (values window tool)))

(defun update-zoom (tool)
  ;; update the radio button display after
  (let* ((plotter (widget-property tool ':plotter))
         (xz (axis-zoom (plotter-x-axis plotter)))
         (yz (axis-zoom (plotter-y-axis plotter)))
         (both (widget-property tool ':both)))
    ;; if xy zoom quantities are the same set Both sensitive else
    ;; desensitize and unset it.
    (if (= xz yz)
      (gtk:widget-set-sensitive both t)
      (progn
        (if (gtk:toggle-button-get-active both)
          (gtk:toggle-button-set-active both nil))
        (gtk:widget-set-sensitive both nil)))
    ;; make the Fit button unavailable if Both is selected.
    (values)))
                                  
(defun do-zoom (tool)
  (let* ((amount (/ (gtk:adjustment-get-value
                     (widget-property tool ':spin-adjustment))
                    100.0D0))
         (plotter (widget-property tool ':plotter))
         (active (widget-property tool ':active-axis))
         zoom-x zoom-y)
    (case active
      (:both
       (setq zoom-x (axis-zoom (plotter-x-axis plotter)))
       (setq zoom-y (axis-zoom (plotter-y-axis plotter))))
      (:horizontal
       (setq zoom-x (axis-zoom (plotter-x-axis plotter))))
      (:vertical 
       (setq zoom-y (axis-zoom (plotter-y-axis plotter)))))
    (if zoom-x
      (if (= amount zoom-x) (setq zoom-x nil)
          (setq zoom-x amount)))
    (if zoom-y
      (if (= amount zoom-y) (setq zoom-y nil)
          (setq zoom-y amount)))
    (plotter-zoom plotter :x zoom-x :y zoom-y)
    (update-zoom tool)))

;;;
;;; Styling Tool
;;;

(defun GdkColor->rgb (c)
  (list;; (gdk:Color.pixel c)
   (gdk:Color.red c)
   (gdk:Color.green c)
   (gdk:Color.blue c)))

(gtk:define-signal-handler styling-tool-destroy :void (tool)
  (let ((plotter (widget-property tool ':plotter)))
    (when plotter
      (setf (plotter-styling-tool plotter) nil))))

(gtk:define-signal-handler styling-tool-color-changed :void (widget tool)
  widget
  (let* ((plotter (widget-property tool ':plotter))
         (colorsel (widget-property tool ':colorsel))
         (colormap (widget-property tool ':colormap))
         (color (widget-property tool ':color)))
    (gtk:color-selection-get-current-color colorsel color)
    (gdk:colormap-alloc-color colormap color t t)
    (plotter-redraw plotter)))

;;;
;;; line and point size callbacks
;;;

(gtk:define-signal-handler styling-tool-point-width-changed :void (adj win)
  (styling-do-spin-change adj win ':point-width))
                                 
(gtk:define-signal-handler styling-tool-point-height-changed :void (adj win)
  (styling-do-spin-change adj win ':point-height))

(gtk:define-signal-handler styling-tool-line-width-changed :void (adj win)
  (styling-do-spin-change adj win ':line-width))

(defun styling-do-spin-change (adjust tool which)
  (let* ((plotter (widget-property tool ':plotter))
         (value (floor (gtk:adjustment-get-value adjust))))
    (ecase which
      (:point-width
       (plotter-front-styling plotter :point-width value :error nil))
      (:point-height
       (plotter-front-styling plotter :point-height value :error nil))
      (:line-width
       (plotter-front-styling plotter :line-width value :error nil)))
    (values)))

;;;
;;  styling tool window
;;;

(defun styling-tool (plotter)
  (let* ((window (gtk:window-new gtk:window-toplevel))
         (tool window)
         (colorsel (gtk:color-selection-new))
         (vbox (gtk:vbox-new nil 5))
         hbox label frame spin1 spin2 spin3)
    ;; ...and add the plotter to the tool's property list
    (setf (widget->object tool) NIL)
    (setf (widget-property tool ':plotter) plotter)
    (setf (widget-property tool ':colorsel) colorsel)
    ;; cache the colormap for fast allocation
    (setf (widget-property tool ':colormap)
          (plotting-colormap plotter))
    (gtk:window-set-title window "Styling")
    (gtk:container-set-border-width window 10)

    ;; Zoom value frame
    (setq frame (gtk:frame-new "Sizing"))
    (gtk:box-pack-start vbox frame nil nil 5)    
    (gtk:widget-show frame)
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:container-add frame hbox)
    (gtk:widget-show hbox)

    ;; create and cache spin buttons for point and line sizing
    (setq label (gtk:label-new "Point width:"))
    (GTK:BOX-PACK-START hbox label nil nil 5)
    (gtk:widget-show label)
    (setq spin1 (gtk:spin-button-new-with-range 0 24 1))
    (gtk:spin-button-set-digits spin1 0)
    (GTK:BOX-PACK-START hbox spin1 nil nil 0)
    (gtk:widget-show spin1)
    (setf (widget-property tool ':spin1)
          (gtk:spin-button-get-adjustment spin1))

    (setq label (gtk:label-new "Point height:"))
    (GTK:BOX-PACK-START hbox label nil nil 5)
    (gtk:widget-show label)
    (setq spin2 (gtk:spin-button-new-with-range 0 24 1))
    (gtk:spin-button-set-digits spin2 0)
    (GTK:BOX-PACK-START hbox spin2 nil nil 0)
    (gtk:widget-show spin2)
    (setf (widget-property tool ':spin2)
          (gtk:spin-button-get-adjustment spin2))

    (setq label (gtk:label-new "Line width:"))
    (GTK:BOX-PACK-START hbox label nil nil 5)
    (gtk:widget-show label)
    (setq spin3 (gtk:spin-button-new-with-range 0 24 1))
    (gtk:spin-button-set-digits spin3 0)
    (GTK:BOX-PACK-START hbox spin3 nil nil 0)
    (gtk:widget-show spin3)
    (setf (widget-property tool ':spin3) 
          (gtk:spin-button-get-adjustment spin3))

    (gtk:container-add window vbox)
    (gtk:widget-show vbox)
    (setq frame (gtk:frame-new "Coloring"))
    (gtk:box-pack-start vbox frame t t 0)    
    (gtk:widget-show frame)
    (gtk:color-selection-set-has-palette colorsel t)
    (gtk:container-add frame colorsel)
    (gtk:widget-show colorsel)

    ;; update the tool before the signals are connected to
    (update-styling-tool tool)
    ;; connect the signal callbacks after the update.
    (g:signal-connect colorsel "color_changed"
                      (g:callback styling-tool-color-changed)
                      window)
    ;; connect spin buttons' adjustments to signales.
    (g:signal-connect (gtk:spin-button-get-adjustment spin1)
                      "value_changed" 
                      (g:callback styling-tool-point-width-changed)
                      window)
    (g:signal-connect (gtk:spin-button-get-adjustment spin2)
                      "value_changed" 
                      (g:callback styling-tool-point-height-changed)
                      window)
    (g:signal-connect (gtk:spin-button-get-adjustment spin3)
                      "value_changed" 
                      (g:callback styling-tool-line-width-changed)
                      window)
    (g:signal-connect window
                      "destroy" 
                      (g:callback styling-tool-destroy)
                      (g:nullptr))
    ;;(gtk:grab-add win)
    (gtk:widget-show window)
    (values window tool)))

(defun update-styling-tool (tool)
  (let* ((plotter (widget-property tool ':plotter))
         (colorsel (widget-property tool ':colorsel))
         (gstyle (plotter-style plotter))
         (focus (plotter-front-layer plotter)))
    (cond ((not focus) nil)
          ((not (eql focus (widget-property tool ':layer)))
           ;; focus layer has changed since tools last update.
           (let* ((pstyle (layer-style focus))
                  (color (styling-color pstyle gstyle))
                  (lwidth (styling-line-width pstyle gstyle))
                  (pwidth (styling-point-width pstyle gstyle))
                  (pheight (styling-point-height pstyle gstyle))
                  (font (styling-font pstyle gstyle)))
             (setf color (drawing-color color (plotter-colors plotter)))
             (setf (widget-property tool ':color) color)
             (setf (widget-property tool ':line-width) lwidth)
             (setf (widget-property tool ':point-width) pwidth)
             (setf (widget-property tool ':point-height) pheight)
             (setf (widget-property tool ':font) font)
             (gtk:adjustment-set-value (widget-property tool ':spin1)
                                        pwidth)
             (gtk:adjustment-set-value (widget-property tool ':spin2)
                                        pheight)
             (gtk:adjustment-set-value (widget-property tool ':spin3)
                                        lwidth)
             (gtk:color-selection-set-current-color colorsel color)
             (gtk:color-selection-set-previous-color colorsel color)
             (setf (widget-property tool ':layer) focus)
             )))
    (values)))

; (defparameter pw (plotter :no-window t))
; (styling-tool pw)

; (setf foo (styling-color "dark red" (plotter-style pw)))

;;;
;;; Editing Tool
;;;

(gtk:define-signal-handler editing-tool-destroy :void (tool)
  ;; remove tool from plotter.
  (let ((plotter (widget-property tool ':plotter)))
    (when plotter
    ;;  (gdk:window-set-cursor (plotter-drawing-area plotter)
    ;;                         (g:nullptr))
    ;;  (mouseinfo-init (plotter-mouseinfo plotter))
      (setf (plotter-editing-tool plotter) nil))
    (remove-widget tool)))

(defun get-editing-cursor (cursors mode id)
  (let ((cursor (getf cursors mode)))
    (unless cursor
      (setf cursor (gdk:cursor-new id))
      (gdk:cursor-ref cursor)
      (setf (getf cursors mode) cursor))
    cursor))

;(defun get-editing-cursor (tool mode id)
;  (or (widget-property tool mode)
;      (let ((curs (gdk:cursor-new id)))
;        (setf (widget-property tool mode) curs)
;        (gdk:cursor-ref curs)
;        curs)))

(gtk:define-signal-handler rb-select-points :void (widget tool)
  (rb-cursor-mode widget tool ':select-points))
(gtk:define-signal-handler rb-select-regions :void (widget tool)
  (rb-cursor-mode widget tool ':select-regions))
(gtk:define-signal-handler rb-add-points :void (widget tool)
  (rb-cursor-mode widget tool ':add-points))
(gtk:define-signal-handler rb-delete-points :void (widget tool)
  (rb-cursor-mode widget tool ':delete-points))

(defun rb-cursor-mode (widget tool mode)
  (if (gtk:toggle-button-get-active widget)
    (let* ((plotter (widget-property tool :plotter))
           (cursors (widget-property tool :cursors))
           (drawing (gtk:Widget.window
                     (plotter-drawing-area plotter)))
           (selection (plotter-selection plotter))
           (cursor (g:nullptr)))
      (ecase mode
        ((:select-points ) 
         (if (and selection (selection-type? selection ':region))
           (flip-selection plotter selection))
         (setq cursor (g:nullptr)))
        ((:select-regions )
         (if (and selection (selection-type? selection ':point))
           (flip-selection plotter selection))
         (setq cursor (get-editing-cursor cursors mode 130)))
        ((:add-points )
         (setq cursor (get-editing-cursor cursors mode 90)))
        ((:delete-points  ) 
         (setq cursor (get-editing-cursor cursors mode 88))))
      ;; update mousemode and cursor in plotter window
      (gdk:window-set-cursor drawing cursor)
      (setf (mouseinfo-mode (plotter-mouseinfo plotter)) mode)
      ;; point menu only available on :add-points
      (dolist (w (widget-property tool :addsubs))
        (gtk:widget-set-sensitive w (eq mode ':add-points)))
      (values))))

(gtk:define-signal-handler spin-separation :void (widget data)
  ;; widget is adjustment, data is main window
  (let ((plotter (widget-property data ':plotter)))
    (setf (mouseinfo-apart (plotter-mouseinfo plotter))
          (gtk:adjustment-get-value widget))
    (values)))

;;;
;;; "Mouse Control" page callbacks

(gtk:define-signal-handler mouse-tracking :void (widget data)
  ;; widget is toggle button data is window
  (let* ((plotter (widget-property data ':plotter))
         (menu (widget-property data ':tracking-menu))
         (info (plotter-mouseinfo plotter)))
    (if (gtk:toggle-button-get-active widget)
      (let ((which (gtk:option-menu-get-history menu)))
        (gtk:widget-set-sensitive menu t)
        (if (= which 0)
          (setf (mouseinfo-track info) ':x)
          (setf (mouseinfo-track info) ':y)))
      (progn
        (setf (mouseinfo-track info) NIL)
        (gtk:widget-set-sensitive menu nil)))))

(gtk:define-signal-handler mouse-tracking-axis :void (widget data)
  widget data
  ;; widget is option menu, data is main window
  (let* ((plotter (widget-property data ':plotter))
         (info (plotter-mouseinfo plotter))
         (which (gtk:option-menu-get-history widget)))
    (if (= which 0)
      (setf (mouseinfo-track info) ':x)
      (setf (mouseinfo-track info) ':y))))

(gtk:define-signal-handler mouse-guide :void (widget data)
  widget data
  (values))

;;;
;;; "Selecting" page callbacks

(gtk:define-signal-handler possel :void (widget data)
  ;; widget is entry, data is window
  widget
  (flet ((any? (x)
           (or (eql x 'end)
               (and (integerp x) (<= 0 x))))
         (pos? (x) (and (integerp x) (<= 0 x))))
    (let* ((plotter (widget-property data ':plotter))
           (buffs (widget-property data ':positional-selection))
           (beg (entry-value (first buffs) :test #'pos?)))
      (if (eql beg ':error)
        (collapse-selection plotter)
        (let ((end (entry-value (second buffs) :test #'any?)))
          (if (eql end ':error)
            (collapse-selection plotter)
            (let (( inc (entry-value (third buffs) :test #'pos?)))
              (if (eql inc ':error)
                (collapse-selection plotter)
                (let* ((layer (plotter-front-layer plotter))
                       (data (layer-data layer))
                       (len (length data))
                       (sel nil))
                  (when (> len 0)
                    (if (eql end 'end) 
                      (setq end (1- len))
                      (setq end (min end (1- len))))
                    (setq sel
                          (loop for i from beg to end by inc
                             collect (elt data i)))
                    (if sel
                      (set-selection plotter :type ':point
                                     :points sel :layer layer)
                      (collapse-selection plotter))))))))))))

;;;
;;; Editing tool window

(defun editing-tool (plotter)
  (let* ((window (gtk:window-new gtk:window-toplevel))
         (info (plotter-mouseinfo plotter))
         (cmodes '(:select-points :select-regions :add-points
                   :delete-points))
         menu check notebook vbox vbox2 frame hbox
         button label spin entry )
    ;; add the plotter to the window's propert list
    (setf (widget->object window) nil)
    (setf (widget-property window :plotter) plotter)
    ;; (re)set mouseinfo to defaults
    (setf (mouseinfo-mode info) ':select-points)
    (setf (mouseinfo-track info) NIL)
    (gdk:window-set-cursor (gtk:Widget.window
                            (plotter-drawing-area plotter))
                           (g:nullptr))
    (setf (widget-property window :cursors)
          (loop for m in cmodes collect m collect nil))
    (gtk:window-set-default-size window 300 -1)
    (gtk:window-set-title window "Editing")
    (gtk:container-set-border-width window 10)
    ;; Setup notebook
    (setq notebook (gtk:notebook-new))
    (gtk:container-add window notebook)
    (gtk:widget-show notebook)
    (gtk:notebook-set-tab-pos notebook gtk:pos-top)

    ;; Cursor mode page
    (setq vbox2 (gtk:vbox-new nil 0))
    (gtk:widget-show vbox2)
    ;;   Select Points
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "Select Points"))
    (g:signal-connect button "toggled" 
                      (g:callback rb-select-points)
                      window)
    (gtk:box-pack-start vbox2 button nil nil 0)
    (gtk:widget-show button)
    ;; Select Region
    (setq button (gtk:radio-button-new-with-label-from-widget
                  button "Select Regions"))
    (g:signal-connect button "toggled" 
                      (g:callback rb-select-regions)
                      window)
    (gtk:box-pack-start vbox2 button nil nil 0)
    ;;(gtk:widget-set-sensitive button nil)
    (gtk:widget-show button)

    ;; Add Points
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:box-pack-start vbox2 hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq button (gtk:radio-button-new-with-label-from-widget
                  button "Add Points"))
    (g:signal-connect button "toggled" 
                      (g:callback rb-add-points)
                      window)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    ;;    Point Class entry
    (setq entry (gtk:entry-new))
    (setf (widget-property window ':point-class) entry)
    (gtk:box-pack-start hbox entry nil nil 10)    
    (gtk:entry-set-width-chars entry 12)
    (gtk:widget-set-sensitive entry nil)
    (gtk:widget-show entry)
    ;; load point class with class of first object in layer.
    (let* ((layer (plotter-front-layer plotter))
           (data (and layer (layer-data layer))))
      (when data 
        (gtk:entry-set-text entry
                           (format nil "~(~A~)"
                                   (class-name 
                                    (class-of (first data)))))))
    ;;   Point Menu
    ;;    (setq cbox (gtk:combo-new))
    ;;    (setf (widget-property window ':point-menu) cbox)
    ;;    (gtk:box-pack-start hbox cbox nil nil 10)    
    ;;BUG (gtk:entry-set-width-chars (GtkCombo.entry cbox) 12)
    ;;    (gtk:widget-show cbox)
    ;;   Need to dealloc these strings!
    ;;    (gtk:combo-set-popdown-strings cbox (point-class-list plotter))
    ;;    (gtk:combo-disable-activate cbox)   ; keep from poping up menu
    ;;    (gtk:widget-set-sensitive cbox nil) ; make insensitive

    ;;   label
    (setq label (gtk:label-new "drawing separation" ))
    (gtk:box-pack-start hbox label nil nil 5)
    (gtk:widget-show label)
    (gtk:widget-set-sensitive label nil) ; make insensitive
    ;;   spin
    (setq spin (gtk:spin-button-new-with-range 5 50 1))
    (gtk:spin-button-set-value spin 10) ; default value...
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil 5)
    (gtk:widget-show spin)
    (gtk:widget-set-sensitive spin nil) ; make insensitive
    (g:signal-connect (gtk:spin-button-get-adjustment spin)
                      "value_changed" 
                      (g:callback spin-separation)
                      window)

    (setf (widget-property window ':addsubs) 
          (list entry label spin))
    ;; Delete Points
    (setq button (gtk:radio-button-new-with-label-from-widget
                  button "Delete Points"))
    (g:signal-connect button "toggled" 
                      (g:callback rb-delete-points)
                      window)
    (gtk:box-pack-start vbox2 button nil nil 0)
    (gtk:widget-show button)

    (setq label (gtk:label-new "Cursor Mode"))
    (gtk:widget-show label)
    (gtk:notebook-append-page notebook vbox2 label)

    ;; Selection Page
    (setq vbox (gtk:vbox-new nil 5))
    (gtk:widget-show vbox)
    ;;    Positional frame
    (setq frame (gtk:frame-new "Positional selection"))
    (gtk:container-set-border-width frame 5)
    (gtk:box-pack-start vbox frame nil nil 0)    
    (gtk:widget-show frame)
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:container-add frame hbox)
    (gtk:widget-show hbox)
    (let ((buffs '()))
      (dolist (l '(("Select from" "0") ("to" "end") ("by" "1")))
        (setq label (gtk:label-new (first l)))
        (gtk:box-pack-start hbox label nil nil 5)
        (gtk:widget-show label)
        (setq entry (gtk:entry-new))
        (gtk:entry-set-width-chars entry 5)
        (gtk:entry-set-text entry (second l))
        (gtk:entry-set-activates-default entry nil)
        (g:signal-connect entry "activate" (g:callback possel)
                          window)
        (gtk:box-pack-start hbox entry nil nil 5)
        (gtk:widget-show entry)
        (push entry buffs))
      (setf (widget-property window :positional-selection)
            (nreverse buffs)))
    ;;    Conditional frame    
    (setq frame (gtk:frame-new "Conditional selection"))
    (gtk:container-set-border-width frame 5)
    (gtk:box-pack-start vbox frame nil nil 0)    
    (gtk:widget-show frame)
    (setq vbox2 (gtk:vbox-new nil 5))
    ;; cache conditional selection as (<vbox> . rows)
    ;; each row is (<hbox> <slotmenu> <value> <rembutton>)
    (setf (widget-property window :conditional-selection)
          (list vbox2))
    (gtk:container-add frame vbox2)
    (gtk:widget-show vbox2)
    (add-selection-row window)

    (setq label (gtk:label-new "Selecting"))
    (gtk:widget-show label)
    (gtk:notebook-append-page notebook vbox label)

    ;; "Mouse Control" notebook page
    (setq vbox2 (gtk:vbox-new nil 0))
    (gtk:widget-show vbox2)
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:box-pack-start vbox2 hbox nil nil 0)
    (gtk:widget-show hbox)
    (setf check (gtk:check-button-new-with-label "Constrain to"))
    (g:signal-connect check "toggled" (g:callback mouse-tracking)
                      window)
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check)
    (setq menu (create-option-menu '("horizontal" "vertical")
                                    :changed (g:callback mouse-tracking-axis)
                                    :data window))
    (setf (widget-property window ':tracking-menu) menu)
    (gtk:box-pack-start hbox menu nil nil 5)
    (gtk:widget-set-sensitive menu nil)
    (gtk:widget-show menu)
    (setq label (gtk:label-new "motion."))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq check (gtk:check-button-new-with-label "Draw mouse guide."))
    (gtk:widget-set-sensitive check nil)
    (gtk:box-pack-start vbox2 check nil nil 0)
    (gtk:widget-show check)
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:box-pack-start vbox2 hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "'New Points' hook:"))
    (gtk:box-pack-start hbox label nil nil 0)    
    (gtk:widget-show label)
    (gtk:widget-set-sensitive label nil)
    (setq entry (gtk:entry-new ))
    (gtk:box-pack-start hbox entry nil nil 0)    
    (gtk:widget-show entry)
    (gtk:widget-set-sensitive entry nil)
    (setq label (gtk:label-new "Mouse controls"))
    (gtk:widget-show label)
    (gtk:notebook-append-page notebook vbox2 label)
    (gtk:notebook-set-current-page notebook 0)
    (g:signal-connect window "destroy"
                      (g:callback editing-tool-destroy)
                      (g:nullptr))
    (gtk:widget-show window)
    window))

(defun point-class-list (plotter)
  (let ((class-names '("Point")))
    (loop for layer in (plotter-layers plotter)
          for p = (first (layer-data layer))
          when p do
          (let ((n (format nil "~(~A~)" (class-name (class-of p)))))
            (unless (member n class-names :test #'string-equal)
              (push n class-names))))
    (when (cdr class-names)
      (setf class-names (nreverse class-names)))
    (let ((head (g:nullptr)))
      (loop for n in class-names
            do (setq head (g:list-append head (gtk:string->cstring n)))
            finally (return head)))))

(gtk:define-signal-handler remrow :void (widget tool)
  ;; widget is remove button, data is window
  (let* (;; car of data vbox holding rows.
         ;; car of each row is hbox containing row widgets
         (data (widget-property tool :conditional-selection))
         (rows (cdr data))
         (delr (find widget rows :test #'member)))
    (when (cdr rows);; never delete a single remaining row
      (setf (cdr data)
            (loop for r in rows unless (eq r delr) collect r))
      ;; car of row is row's hbox
      (gtk:widget-destroy (car delr)))
    (values)))

(gtk:define-signal-handler addrow :void (widget tool)
  ;; widget is button, tool is window ; vbox to addto
  widget
  (add-selection-row tool )
  (values))

(defparameter *selops*
  ;; conditional selection relations. each relation is passed two
  ;; values: the object's slot value and the user's value
  `(("equal to" ,(function equal))
    ("less than" , (lambda (s u)
                     (and (numberp s)
                          (numberp u)
                          (< s u))))
    ("more than" , (lambda (s u)
                        (and (numberp s)
                             (numberp u)
                             (> s u))))
    ("containing" , (lambda (s u)
                    (if (consp s)
                      (member u s :test #'equal)
                      (equal s u))))
    ("member of" , (lambda (s u)
                     (if (consp u)
                       (member s u :test #'equal)
                       nil)))))



(defun editable-value? (x)
  ;; only edit slots that contain simple, basic lisp values...
  (cond ((or (numberp x)
             (symbolp x)
             (stringp x))
         t)
        ((consp x)
         (every #'editable-value? x))
        (t nil)))

(gtk:define-signal-handler condsel :void (widget tool)
  widget tool
  (let* ((rows (cdr (widget-property tool :conditional-selection)))
         (test nil)   
         (plotter (widget-property tool ':plotter))
         (layer (plotter-front-layer plotter))
         (entry nil)
         (error nil))
    ;; each row is (hbox slot not omenu value rem) collect row data,
    ;; skip empty rows, catch errors etc.
    (loop for row in rows
          for slotw = (second row)
          for notw = (third row)
          for menuw = (fourth row)
          for exprw = (fifth row)
          for txt1 = (gtk:entry-get-text slotw)
          for txt2 = (gtk:entry-get-text exprw)
          ;; skip empty rows
          unless (and (equal txt1 "") (equal txt2 ""))
          do
          (let ((slot (safe-read? txt1))
                (oper (gtk:option-menu-get-history menuw))
                (expr (safe-read? txt2)))
            (if (or (eql slot ':err)
                    (eql slot nil)
                    (not (symbolp slot)))
              (progn (setq error txt1)
                     (setq entry slotw)
                     (return))
              (if (eql expr ':err)
                (progn (setq error txt2)
                       (setq entry exprw)
                       (return))
                (push (list slot
                            (gtk:toggle-button-get-active notw)
                            (second (elt *selops* oper)) 
                            expr)
                      test)))))
    ;;(print (reverse test))
    (if error
      (progn
        (if (equal error "") (gtk:entry-set-text entry "<text>"))
        (gtk:editable-select-region entry 0 -1)
        (collapse-selection plotter))
      (if (and test layer)
        (let ((sels nil))
          (setq test
                ;;  each d is (slot not? func expr)
                (loop for d in (nreverse test)
                      for call = `(funcall ,(third d)
                                   (slot-value obj ',(first d))
                                   ,(fourth d))
                      collect
                      `(and (slot-exists-p obj ', (first d))
                        (slot-boundp obj ', (first d))
                        , (if (second d) `(not ,call) call))))
          ;; 'and' together multiple clauses.
          (if (cdr test)
            (push 'and test)
            (setq test (car test)))
          (setq test (coerce `(lambda (obj) ,test) 'function))
          ;; map the puppies...
          (setq sels (loop for p in (layer-data layer)
                           when (funcall test p)
                           collect p))
          (if sels
            (set-selection plotter :points sels :type ':point
                           :layer layer)
            (collapse-selection plotter)))))))

(defun add-selection-row (tool )
  (let* ((hbox (gtk:hbox-new nil 10))
         (data (widget-property tool :conditional-selection))
         ;; vbox is the packing box that holds the "row" data
         (vbox (car data))
         label slot cbox omenu value add rem)
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "slot"))
    (gtk:box-pack-start hbox label nil nil 5)
    (gtk:widget-show label)
    (setq slot (gtk:entry-new))
    (gtk:entry-set-width-chars slot 12)
    (gtk:entry-set-activates-default slot nil)
    (g:signal-connect slot "activate" (g:callback condsel)
                      tool)
    (gtk:box-pack-start hbox slot nil nil 0)
    (gtk:widget-show slot)
    ;; NOT operator
    (setq cbox (gtk:check-button-new-with-label "not"))
    (gtk:box-pack-start hbox cbox nil nil 0)
    (gtk:widget-show cbox)
    (setq omenu (gtk:option-menu-new))
    (let ((menu (gtk:menu-new))
          item)
      (dolist (op *selops*)
        (setq item (gtk:menu-item-new-with-label (first op)))
        (gtk:menu-shell-append menu item)
        (gtk:widget-show item))
      (gtk:option-menu-set-menu omenu menu))
    (gtk:box-pack-start hbox omenu nil nil 0)
    (gtk:widget-show omenu)
    ;; value entry
    (setq value (gtk:entry-new))
    (gtk:entry-set-width-chars value 12)
    (gtk:entry-set-activates-default value nil)
    (g:signal-connect value "activate" (g:callback condsel)
                      tool)
    (gtk:box-pack-start hbox value nil nil 0)
    (gtk:widget-show value)
    (let ((box (gtk:hbox-new t 0)))
      (gtk:box-pack-start hbox box t t 0)
      (gtk:widget-show box)
      (setq add (gtk:button-new-with-label "+"))
      (g:signal-connect add "clicked" (g:callback addrow)
                        tool)
      (gtk:box-pack-start box add t t 0)
      (gtk:widget-show add)
      (setq rem (gtk:button-new-with-label "-"))
      (g:signal-connect rem "clicked" (g:callback remrow)
                        tool)
      (gtk:box-pack-start box rem t t 0)
      (gtk:widget-show rem))
    ;; add new row to cdr of data. widgets in each row are
    ;; (hbox slot not menu value minus)
    (nconc data (list (list hbox slot cbox omenu value rem)))
    hbox))

;;;
;;; Edit window (inspector)
;;;

(gtk:define-signal-handler edit-destroy :void (window)
  (let ((plotter (widget-property window :plotter)))
    (when plotter
      (setf (plotter-inspectors plotter)
            (remove window (plotter-inspectors plotter))))
    (remove-widget window)))

(gtk:define-signal-handler edit-set :void (widget data)
  (let ((text (gtk:entry-get-text widget)))
    (unless (equal text "")
      (let ((value (safe-read? text)))
        (if (eql value ':err)
          (gtk:editable-select-region widget 0 -1)
          (let* ((plist (widget->object data))
                 (edit (find widget (getf plist ':rows)
                             :key #'third))
                 (slot (first edit))
                 (dpys (getf plist :display-slots))
                 (source (getf plist ':source)))
            (if (typep source 'selection)
              (dolist (p (selection-points source))
                (when (slot-exists-p p slot)
                  (setf (slot-value p slot) value)))
              (setf (slot-value source slot) value))
            (when (find slot dpys)
              ;; first slot is ordered slot
              (if (eql slot (car dpys))
                (if (not (ordered-layer? (selection-layer source)
                                         slot))
                  (sort-layer (selection-layer source)
                              slot)))
              (plotter-redraw (getf plist :plotter)))))))
    (values)))

(defun edit-object (source &key omit-slots plotter slot-order
                           display-slots)
  (let ((*print-case* ':downcase)
        (edits '())
        obj multi)
    ;; if source is a slection, see if we are
    (if (typep source 'selection)
      (setq obj (car (selection-points source))
            multi (cdr (selection-points source)))
      (setq obj source multi nil))
    (loop for d in (class-slots (class-of obj))
          for s = (slot-definition-name d)
          when (not (find s omit-slots))
          do
          (if (and (not multi)
                   (slot-boundp obj s))
            (let ((v (slot-value obj s)))
              (if (editable-value? v)
                (push (list s v nil) edits)))
            (push (list s :unset nil) edits)))
    (if edits
      (let* ((window (gtk:window-new gtk:window-toplevel))
             (table (gtk:table-new (+ (length edits) 0) 
                                   2
                                   nil))
             label entry)
        (setf edits (nreverse edits))
        (when slot-order
          ;; insure that the slots in slot-order
          ;; are listed first in table.
          (setq slot-order
                (loop for s in slot-order
                      collect (find s edits :key #'car)))
          (loop for s in slot-order
                do (setq edits (delete s edits)))
          (setq edits (append slot-order edits)))
        (gtk:widget-set-size-request window 200 -1)
        (gtk:window-set-title window "Edit")
        (gtk:container-set-border-width window 10)
        (gtk:container-add window table)
        (gtk:table-set-col-spacing table 0 5)
        (gtk:widget-show table)
        (loop for e in edits
              for row from 0
              do
              (setq label (gtk:label-new (format nil "~(~A~)" (car e))))
              (gtk:table-attach-defaults table label 0 1 row (+ row 1))
              (gtk:widget-show label)
              (setq entry (gtk:entry-new))
              (gtk:entry-set-width-chars entry 12)
              (gtk:entry-set-activates-default entry nil)
              ;; dont enter value if unset or multi
              (gtk:entry-set-text entry 
                                  (if (eql (second e) ':unset)
                                    ""
                                    (format nil "~S" (second e))))
              (g:signal-connect entry "activate"
                                (g:callback edit-set)
                                window)
              (gtk:table-attach-defaults table entry 1 2 row (+ row 1))
              (gtk:widget-show entry)
              (setf (third e) entry))
        ;; data is (<object> . <rows>)
        ;; each row is (<slot> <valstr> <entry>)
        (setf (widget->object window)
              (list :plotter plotter
                    :source source
                    :display-slots display-slots
                    :multi multi
                    :rows edits))
        (g:signal-connect window "destroy"
                          (g:callback edit-destroy)
                          (g:nullptr))
        (when plotter (push window (plotter-inspectors plotter)))
        (gtk:widget-show window)
        window))))

;;;
;;; Plotter menubar definitions. Callbackshave to be defined before
;;; the menubar is itself declared.
;;;

(gtk:define-signal-handler file-menu-quit :void (window)
  ;; widget is main window...
  window                       
  (gtk:widget-destroy window))

(gtk:define-signal-handler file-menu-new-plotter :void (window)
  window
  (foo))

;;;
;;;
;;;

(gtk:define-signal-handler edit-menu-clear :void (window)
  (let ((plotter (widget->object window)))
    (collapse-selection plotter)
    (values)))

(gtk:define-signal-handler edit-menu-select-all :void (window)
  (let* ((plotter (widget->object window))
         (mode (mouseinfo-mode (plotter-mouseinfo plotter)))
         (layer (plotter-front-layer plotter)))
    (cond ((eql mode ':select-points)
           (let ((points (layer-data layer)))
             (when points
               (set-selection plotter :points points :type ':point
                              :all t :layer layer))))
          ((eql mode ':select-regions)
           (set-selection plotter :type ':region 
                          :region NIL
                          :all t :layer layer)
           ))
    (values)))

(gtk:define-signal-handler edit-menu-delete :void (window)
  (let ((plotter (widget->object window)))
    (delete-selection plotter)
    (values)))
         
;;;
;;; the View menu is the most complex and only partially implemented
;;; here. In particular, this menu really needs an "activate" handler
;;; that configures the menu's view viems each time a new front plot
;;; is chosen from the "Front Layer" submenu. This submenu, in turn,
;;; needs to be recreated each time a layer is added, deleted or
;;; renamed. As it stands now, the submenu created at runtime to hold
;;; all the initial layers but these items are static.
;;; menu-item signals:
;;; the "select" signal happens when the mouse is over the item
;;; the "toggled" signal happens when its clicked or unclicked
;;; the "activate" signal happens when it is chosen.

(gtk:define-signal-handler front-menu-toggled :void (item window)
  ;; item is the menu item from the Front menu and its label is the
  ;; name of the layer to make the new focus
  (if (gtk:check-menu-item-get-active item)
    (let* ((name (menu-item-text item))
           (plotter (widget->object window))
           (layer (plotter-find-layer plotter name)))
      (if layer
        (plotter-front-layer plotter :layer layer)))))
        
(defun view-menu-front-menu (shell gdata)
  ;; this functions is called to add the layer items to the View->Front
  ;; menu. shell is the Front menu itself; gdata is the plotters main
  ;; window (gtk pointer)
  (let* ((plotter (widget->object gdata))
         (layers (plotter-layers plotter))
         (focus (plotter-front-layer plotter))
         (callb (g:callback front-menu-toggled ))
         (group (g:nullptr)))
    (when layers
      (loop for p in layers
            for s = (layer-name p)
            for i = (gtk:radio-menu-item-new-with-label group s)
            do
            (gtk:menu-shell-append shell i)
            (gtk:widget-show i)
            (when (eq p focus)
              (gtk:check-menu-item-set-active i t))
            ;; Connect handler to all layer items
            (g:signal-connect i "toggled" callb
                              gdata)
            (setq group (gtk:radio-menu-item-get-group i))))))
    
(gtk:define-signal-handler view-menu-envelope :void (window)
 (let ((plotter (widget->object window)))
   (plotter-front-styling plotter :view ':line-and-point
                           :error nil)
   (values)))

(gtk:define-signal-handler view-menu-scatter :void (window)
  (let ((plotter (widget->object window)))
    (plotter-front-styling plotter :view ':point
                           :error nil)
    (values)))

(gtk:define-signal-handler view-menu-line :void (window)
  (let ((plotter (widget->object window)))
    (plotter-front-styling plotter :view ':line
                           :error nil)
    (values)))

(gtk:define-signal-handler view-menu-bar :void (window)
  (let ((plotter (widget->object window)))
    (plotter-front-styling plotter :view ':bar
                           :error nil)
    (values)))

(gtk:define-signal-handler view-menu-box :void (window)
  (let ((plotter (widget->object window)))
    (plotter-front-styling plotter :view ':box
                           :error nil)
    (values)))

(gtk:define-signal-handler view-menu-bubble :void (window)
  (let ((plotter (widget->object window)))
    (plotter-front-styling plotter :view ':bubble
                           :error nil)
    (values)))

(gtk:define-signal-handler view-menu-notation :void (window)
  (let ((plotter (widget->object window)))
    (plotter-front-styling plotter :view ':notation
                           :error nil)
    (values)))

(gtk:define-signal-handler view-menu-show-grid :void (window)
  ;; FIX: view menu updating needs to check the status and preset the
  ;; menu item to on or off. this code only works because the flag is
  ;; initially turned on in the menu and the plotter
  (let* ((plotter (widget->object window))
         (state (plotter-property plotter :show-grid)))
    (plotter-property plotter :show-grid (not state))))

(gtk:define-signal-handler view-menu-show-back-layers :void (window)
  (let* ((plotter (widget->object window))
         (state (plotter-property plotter :show-back-layers)))
    (plotter-property plotter :show-back-layers (not state))))

;;;
;;; Tools menu
;;;

(gtk:define-signal-handler tools-menu-zooming :void (window)
  (let ((plotter (widget->object window)))
    (let ((new (zooming-tool plotter)))
      (setf (plotter-zooming-tool plotter) new))
    (values)))

(gtk:define-signal-handler tools-menu-styling :void (window)
  (let ((plotter (widget->object window)))
    (let ((new (styling-tool plotter)))
      (setf (plotter-styling-tool plotter) new))

    (values)))

(gtk:define-signal-handler tools-menu-editing :void (window)
  (let ((plotter (widget->object window)))
    (let ((new (editing-tool plotter)))
      (setf (plotter-editing-tool plotter) new))
    (values)))

;;; 
;;; The menubar definition...
;;;

(defparameter *menubar*
  `((menu "File"
     (menu "New"
           (item "Plotter"  ,(g:callback file-menu-new-plotter))
           (item ("Layer..." :insensitive t)))
     (item ("Open..." :insensitive t))
     (item nil)
     (item ("Save..." :insensitive t))
     (item ("Save as..." :insensitive t))
     (item nil)
     (item ("Import..." :insensitive t))
     (item ("Export..." :insensitive t))
     (item nil)
     (item "Quit" ,(g:callback file-menu-quit)))
    (menu "Edit" 
     (item ("Undo" :insensitive t))
     (item ("Redo" :insensitive t))
     (item nil)
     (item ("Cut" :insensitive t))
     (item ("Copy" :insensitive t) )
     (item ("Paste" :insensitive t) )
     (item "Delete" ,(g:callback edit-menu-delete))
     (item )
     (item "Select All" ,(g:callback edit-menu-select-all))
     (item "Clear" ,(g:callback edit-menu-clear)))
    (menu "View" 
     (menu ("Front layer" :constructor
                         , (function view-menu-front-menu)))
                    ;;:selector ,(g:callback front-menu-select)
     (item nil)
     (item ("Envelope" :radio t) ,(g:callback view-menu-envelope))
     (item ("Scatter" :radio t) ,(g:callback view-menu-scatter))
     (item ("Line" :radio t) ,(g:callback view-menu-line))
     (item ("Bar" :radio t) ,(g:callback view-menu-bar))
     (item ("Box" :radio t) ,(g:callback view-menu-box))
     (item ("Bubble" :radio t) ,(g:callback view-menu-bubble))
     (item ("Notation" :radio t :insensitive t))
     (item nil)
     (item ("Show Grid" :check :checked)
      ,(g:callback view-menu-show-grid))
     (item ("Show Back Layers" :check :checked )
      ,(g:callback view-menu-show-back-layers))
     )
    (menu "Tools" 
     (item "Editing..." ,(g:callback tools-menu-editing))
     (item "Zooming..." ,(g:callback tools-menu-zooming))
     (item "Coloring..." ,(g:callback tools-menu-styling))
     (item "Sizing..." ,(g:callback tools-menu-styling))
     )
    (menu ("Help" :right t)
     (item ("Plotter Help" :insensitive t))
     (item ("Keyboard Shortcuts" :insensitive t)))))