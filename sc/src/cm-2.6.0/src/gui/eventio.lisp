;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.69 $
;;; $Date: 2005/02/20 21:50:33 $

(in-package :cm)

; (defun pixie (&rest rgb) (format nil "#~{~X~}" (loop for c in rgb collect (floor (* #xff c)))))
; (pixie .9843 .9843 .9137)

(defun bar (&optional (n 10)) ;testing
  (process repeat n output (new midi :time (now) :keynum (between 60.0 80.0)) 
           wait .2))

(defclass cmio ()
  ((pages :initform (make-hash-table :size 32) :accessor cmio-pages)
   (notebooks :initform (list nil nil nil nil)
              :accessor cmio-notebooks)
   ;; two label, the first holds a status image, the second is used as
   ;; text message buffer.
   (output :initform (list nil nil) :accessor cmio-output)
   (window :initform nil :accessor cmio-window)
   (flags :initform 0 :accessor cmio-flags)
   (colors :initform nil :accessor cmio-colors)))

(defconstant +cmio-command-active+ 1)

(defparameter *cmio-source-pages*
  '(:containers :eventio :help)) ; :lisp

(defparameter *cmio-target-pages*
  '(:clm :cmn :csound :midi :midishare :plotter :seq 
    ;; these two are right justified. ill
    :systems :files))

(defgeneric cmio-page-can-do? (cmio page op))
(defgeneric cmio-create-page (cmio page notebook ))
(defgeneric cmio-page-data (cmio page &optional op))
(defgeneric cmio-set-page-data (cmio page data))
(defgeneric cmio-ensure-write-stream (cmio target data))
(defgeneric cmio-write (cmio target objs starts))
(defgeneric cmio-ensure-execute-command (cmio type file stream data))

;;;
;;; default methods
;;;

;;;
;;; cmio-page-can-do? returns true if page supports an operation

(defmethod cmio-page-can-do? (cmio page op)
  (declare (ignore cmio page op))
  nil)

(defmethod cmio-page-can-do? (cmio (page (eql :clm)) op) 
  (declare (ignore cmio ))
  (if (member op '(:write :execute :import))
    t ;(target-system-loaded? page)
    nil))

(defmethod cmio-page-can-do? (cmio (page (eql :cmn)) op) 
  (declare (ignore cmio ))
  (if (member op '(:write :execute)) 
    t ;(target-system-loaded? page)
    nil))

(defmethod cmio-page-can-do? (cmio (page (eql :csound)) op) 
  (declare (ignore cmio ))
  (if (member op '(:write :execute :import)) t nil))

(defmethod cmio-page-can-do? (cmio (page (eql :midi)) op) 
  (declare (ignore cmio ))
  (if (member op '(:write :execute :import)) t nil))

(defmethod cmio-page-can-do? (cmio (page (eql :midishare)) op)
  (declare (ignore cmio ))
  (if (member op '(:write ))
    t ;(target-system-loaded? page)
    nil))

(defmethod cmio-page-can-do? (cmio (page (eql :plotter)) op)
  (declare (ignore cmio ))
  (if (member op '(:write )) t nil))

(defmethod cmio-page-can-do? (cmio (page (eql :seq)) op) 
  (declare (ignore cmio ))
  (if (member op '(:write )) t nil))

;;;
;;; cmio-ensure-write-stream return three values: (1) a fully
;;; initialized stream for writing, (2) a flag for auto-Executing file
;;; after write and (3) an error code or nil

(defmethod cmio-ensure-write-stream  (cmio target data)
  cmio target data
  (values nil nil nil))

;;;
;;; cmio-ensure-execute-command returns a valid command (a lisp
;;; expression or external process string) or an error code as a
;;; second value.

(defmethod cmio-ensure-execute-command (cmio type file stream data)
  cmio type file stream data
  (values nil nil))

;;;
;;; cmio-page-data returns a list whose car is the keyword page name
;;; and cdr is property list containing keyword names and values from
;;; each field.

(defmethod cmio-page-data (cmio (page t) &optional op)
  op
  (cmio-print cmio :warning "no data for page ~s" page)
  nil)

(defun get-data (data key &optional default)
  ;; get a value in data list, car of list is always keyword page name
  (getf (CDR data) key default))

(defun (setf get-data) (value data key)
  (setf (getf (CDR data) key) value))

;;;
;;; cmio-set-page-data sets page widget values given a plist of data.
;;; does not return values or signal errors

(defmethod cmio-set-page-data (cmio page data)
  cmio page data
  (values))

(defun set-page-fields (user &rest data)
  ;; user is a property list of page field names and values. set each
  ;; entry widget in data if its name in user is recognized, coercing
  ;; non-string values to strings if necessary.
  (do ((*print-case* ':downcase)
       name type widget value)
      ((null user) nil)
    (setq name (pop user)
          type nil 
          widget nil
          value nil)
    (loop with w for (n v) on data by #'cddr
       do (if (consp v)
            (setq type (first v) w (cdr v))
            (setq type ':entry w v))
       (when (eql n name)
         (setq widget w)
         (return)))
    (setq value (pop user))
    (when widget 
      ;(print (list :name-> name :type-> type :value-> value :widget-> widget))
      (ecase type
        (:entry
         (when value
           (unless (stringp value)
             (setq value (format nil "~S" value)))
           (gtk:entry-set-text widget value)))
        (:spin
         (multiple-value-bind (arg lo hi)
             (gtk:spin-button-get-range widget 0 0 )
           arg
           (when (and (numberp value) (<= lo value hi))
             (gtk:spin-button-set-value widget value))))
        (:check
         (gtk:toggle-button-set-active widget (if value t nil)))
        (:radio
         ;; widget is a PLIST of radio buttons (:name widget...)
         (when (setq widget (getf widget value))
           (gtk:toggle-button-set-active widget t)))
        (:menu
         ;; widget is (<menu> . items)
         (let ((p (position value (cdr widget))))
           (when p (gtk:option-menu-set-history (car widget) p))))))))

;;;
;;; Utilities
;;;

(defun cmio-color (cmio name)
  (or (getf (cmio-colors cmio) name)
      (error "No cmio color named ~S" name)))

(defun cmio-command-active? (cmio)
  ;; currently unused...
  (logtest (cmio-flags cmio) +cmio-command-active+))

(defun (setf cmio-command-active?) (bool cmio)
  ;; currently unused...
  (let ((flags (cmio-flags cmio)))
    (setf (cmio-flags cmio) 
          (if bool (logior flags +cmio-command-active+)
              (logandc2 flags +cmio-command-active+)))))

(defun cmio-current-source-page (cmio)
  ;; return keyword name of current source or target page
  (elt *cmio-source-pages*
       (gtk:notebook-get-current-page (first (cmio-notebooks cmio)))))

(defun cmio-current-target-page (cmio)
  (elt *cmio-target-pages*
       (gtk:notebook-get-current-page (second (cmio-notebooks cmio)))))

(defun ensure-directory (dir)
  (if (not (char= (elt dir (1- (length dir))) #\/))
    (setq dir (format nil "~a/" dir)))
  (if (not (probe-file dir)) nil dir))

;;; 
;;; Getting/setting page widgets. widgets are stored in an ad hoc
;;; manner according to the requirements of each page

(defun cmio-page-widgets (cmio page &optional group)
  (let* ((data (or (gethash page (cmio-pages cmio))
                   (error "No data for page: ~s" page))))
    (if group
      (or (getf data group)
          (error "Page ~S: no data for group: ~s" page group))
      data)))

(defun cmio-set-page-widgets (cmio page widgets)
  (setf (gethash page (cmio-pages cmio)) widgets))

(defun set-subgroup-sensitivity (window target group active?
                                        &optional first?)
  ;; sets sensitivity on subgroups of widgets
  (let* ((cmio (widget->object window))
         (subs (cmio-page-widgets cmio target group)))
    ;; car of each sublist is normally a checkbox that should remain
    ;; sensitive
    (dolist (w (if (not first?) (cdr subs) subs))
      (gtk:widget-set-sensitive w active?))
    active?))

(defparameter %bw 5)  ; border width
(defparameter %sw 5)  ; box spacing width
(defparameter %rs 2)  ; table row spacing
(defparameter %rc "blue") ; required color

(defun stub-notebook-page (cmio notebook page &optional atend)
  cmio
  (let ((label (gtk:label-new page))
        (vbox (gtk:vbox-new nil 5))
        hbox entry check)
    hbox entry check
    (gtk:notebook-append-page notebook vbox label)
    (gtk:widget-show label)
    (gtk:widget-show vbox)
    (setq label (gtk:label-new "Not yet implemented."))
    (gtk:widget-set-sensitive label nil)
    (gtk:box-pack-start vbox label t t 0)
    (gtk:widget-show label)
    (if atend (gtk:notebook-set-tab-label-packing notebook vbox nil nil 1))
    (values vbox)))

;;;
;;; Printing and error messaging
;;;

(defun cmio-print (cmio condition string &rest args)
  ;; mode is :append or :replace
  (let ((tyio (SECOND (cmio-output cmio)))
        (imag (FIRST (cmio-output cmio)))
        (colors (cmio-colors cmio))
        (icon nil)
        (width 90)
        (color nil))
    (ecase condition
      ((t :message)
       (setq color (getf colors ':green))
       (setq icon "gtk-yes"))
      (:error
       (setq color (getf colors ':red))
       (setq icon "gtk-dialog-error"))
      ((:warning :warn)
       (setq color (getf colors ':yellow))
       (setq icon "gtk-dialog-warning")))     ;#FF8D00
    (gtk:image-set-from-stock imag icon gtk:icon-size-small-toolbar)
    (gtk:widget-show imag)
    (when args (setq string (apply #'format nil string args)))
    (unless (< (length string) width)
      (setq string (concatenate 'string
                                (subseq string 0 (- width 4))
                                " ...")))
    (gtk:widget-modify-fg tyio gtk:state-normal color)
    (gtk:label-set-text tyio string)
    (values)))

(defun report-error (err &key (label "") widget entry window)
  (unless window
    (setq window (gtk:widget-get-toplevel (or entry widget))))
  (when entry (setq widget entry))
  (let* ((cmio (widget->object window))
         (str ""))
    (cond ((stringp err) (setq str err))
          ((eql err +se-nullstring+)
           (setq str " Missing value."))
          ((eql err +se-unreadable+)
           (setq str " Unreadable lisp value."))
          ((eql err +se-multiple+)
           (setq str " Only one value allowed."))
          ((eql err +se-not-number+)
           (setq str " Not a number."))
          ((eql err +se-not-symbol+)
           (setq str " Not a number."))
          ((eql err +se-not-cons+)
           (setq str " Not a list."))
          ((eql err +se-incorrect+)
           (setq str " Incorrect value."))
          ((eql err t)
           (setq str " Execution error.")))
    (setq label (concatenate 'string label str))
    ;; this could dispatch on clos object...
    (cmio-print cmio :error label)
    (if (and entry (not (eql err +se-nullstring+)))
      (gtk:editable-select-region entry 0 -1))
    (if widget (gtk:widget-grab-focus widget))
    err))

(defun eval-error-string (e)
  (format nil " Eval signaled '~(~A~)' error."
          (class-name (class-of e))))

(defun cmio-required-label (cmio text)
  ;;(gtk:widget-modify-text widget gtk:state-normal color)
  (let ((lab (gtk:label-new text)))
    (gtk:widget-modify-fg lab gtk:state-normal
                            (getf (cmio-colors cmio) ':blue))
    lab))

(defun cmio-show-widget-required (cmio widget)
  ;; called on labels whose fields are "required"
  (gtk:widget-modify-fg widget gtk:state-normal 
                        (cmio-color cmio :blue)))  

(defun cmio-show-widget-evalable (cmio widget)
  ;; called on entries and text views that are evalled.
  (gtk:widget-modify-base widget gtk:state-normal 
                          (cmio-color cmio :pale-yellow)))

(defun cmio-clear-message (cmio)
  (let ((output (cmio-output cmio)))
    (gtk:widget-hide (first output))
    (gtk:label-set-text (second output) "")))

(gtk:define-signal-handler cmio_noop :void (widget data)
  ;; widget is an uninplemented button, data is window.
  (cmio-print (widget->object data)
              :warning "Sorry, '~@(~A~)' not yet implemented."
              (gtk:button-get-label widget)))

(defun stub-button (button window)
  ;; stub button out with moderatly useful message.
  (g:signal-connect button "clicked" (g:callback cmio_noop) window))

;;;
;;; Page data getters
;;;

(defun add-check-data (data key widget)
  (nconc data (list key (gtk:toggle-button-get-active widget))))

(defun add-radio-data (data key widget onval &optional (offval nil op))
  (if (gtk:toggle-button-get-active widget)
    (nconc data (list key onval))
    (if op (nconc data (list key offval))
        nil)))

(defun add-spin-data (data key widget &key result)
  (let ((val (gtk:spin-button-get-value widget)))
    (nconc data (list key (if result (funcall result val) val)))))

(defun add-menu-data (data key widget items) 
  (let ((num (gtk:option-menu-get-history widget)))
    (nconc data (list key (elt items num)))))

(defun add-entry-data (data key widget &rest args)
  (multiple-value-bind (val err) (apply #'entry-expr widget args)
    (if err
      (nconc data (list key (list :error err :entry widget)))
      ;; if :read is nil AND user starts with #. then read
      (if (and (stringp val)
               (null (getf args :read t))
               (> (length val) 2)
               (char= (elt val 0) #\#)
               (char= (elt val 1) #\.))
        (let ((sav val))
          (multiple-value-setq (val err)
            (string->expr val))
          (if err
            (nconc data (list key (list :error
                                        (format nil "~S cannot be evaled."
                                                sav) 
                                        :entry widget)))
            (nconc data (list key val))))
        (nconc data (list key val))))))

;;;
;;; Error reporting

(defun keyword->label (keyw &optional (colon t))
  (let ((str (format nil "~@(~A~)~@[:~]" keyw colon)))
    (if (find #\- str) (substitute #\space #\- str)
        str)))

(defmacro with-data-errors-aborted ((data &rest others) &body body)
  ;; process the properties in data and report/abort on
  ;; any error
  (let ((check (gensym)))
    `(error-block
      (let ((,check ,(if others
                         `(append (cdr ,data)
                                  ,@ (loop for l in others collect `(cdr ,l)))
                         `(cdr ,data))))
        (loop for (key val) on ,check by #'cddr
           if (and (consp val) (eql (car val) ':error))
           ;; val=(:error <err> :entry <widget>)
           do (progn (apply #'report-error (second val)
                            :label (keyword->label key)
                            (cddr val))
                     (error-abort)))
        ,@body))))

(defun strip-entry (entry)
  (let ((raw (gtk:entry-get-text entry)))
    (if (string= raw "") ""
        (string-trim '(#\space #\return #\tab) raw))))

;;;
;;; Write command
;;;

(defmethod cmio-write (cmio target objs starts)
  (let ((data (cmio-page-data cmio target ':write))
        (exec (cmio-page-data cmio ':exec ':write)))
    (with-data-errors-aborted (data exec)
      ;; we only get here if both data lists are ok.
      (let* (output execute? stream error?)
        (multiple-value-setq (stream execute? error?)
          (cmio-ensure-write-stream cmio target data))
        (when error?
          (report-error error? :label "Compose:"
                        :window (cmio-window cmio))
          (error-abort))
        ;; temporary hack to stub missing methods...
        (WHEN (NOT STREAM)
          (CMIO-PRINT cmio :warning "Sorry, page ~(~A~) does not work yet."
                      target)
          (ERROR-ABORT))
        (cmio-print cmio :message "Composing...")
        (gtk:widget-queue-draw (SECOND (cmio-output cmio)))
        (multiple-value-setq (output error?)
          (safecall #'events objs stream starts))
        ;(print (list :output-> output :stream-> stream))
        (cond ((not error?)
               (cond ((typep stream 'event-stream)
                      (cmio-set-execute cmio 
                                        output
                                        stream
                                        (get-data exec ':directory))
                      (if execute?
                        (cmio-execute cmio)
                        (cmio-print cmio :message "Composing...~S ok."
                                    output)))
                     (t
                      (cmio-print cmio :message "Composing...~S ok."
                                  output))))
              (t
               (report-error (eval-error-string error?)
                             :label "Compose:"
                             :window (cmio-window cmio))
               (error-abort)))))))

;;;
;;; Importing
;;;

(defun imports->string (l &optional (e t))
  (if (not (listp l))
    (format nil "~@[#&~A~]" (object-name l))
    (loop with s = (if e "(list" "(") for o in l
       for n = (object-name o)
       if (not n) return ""
       else do (setq s (concatenate 'string s (format nil " #&~A" n)))
       finally (return (concatenate 'string s ")")))))

(defun cmio-import (cmio target)
  (let ((data (cmio-page-data cmio target))
        (exec (cmio-page-data cmio :exec))
        (wind (cmio-window cmio)))
    (with-data-errors-aborted (data)
      (let* ((name (get-data exec ':file))
             (file (namestring (merge-pathnames name
                                                (get-data exec ':directory))))
             (type (pathname-type file))
             (args (list file))
             (buff nil)
             (seqs nil)
             (err? nil))
        (cond ((not (probe-file file))
               (report-error (format nil "Import: file ~s does not exist."
                                     file)
                             :window wind)
               (error-abort))
              ((and (eql target :midi)
                    (member type '("midi" "mid") :test #'equal))
               (let ((tmpo (get-data data ':tempo))
                     (meta (get-data data ':meta-exclude))
                     (trks (get-data data ':exclude-tracks)))
                 (setq buff (FIRST (cmio-page-widgets cmio 
                                                      ':midi ':midi-file)))
                 (nconc args  (list ':time-format 
                                    (get-data data ':time-format)
                                    ':keynum-format 
                                    (get-data data ':keynum-format)))
                 (when tmpo (nconc args (list ':tempo tmpo args)))
                 (when meta (nconc args (list ':meta-exclude meta)))
                 (when meta (nconc args (list ':exclude-tracks trks)))))
              ((and (eql target :clm))
               )
              ((and (eql target :sco))
               (cmio-print cmio :warning
                           "Import: sorry, Csound not yet implemented.")
               (error-abort))
              (t
               (report-error (format nil "Import: couldn't import ~s." file))
               (error-abort)))
        (multiple-value-setq (seqs err?)
          (safeapply #'import-events args))
        (if (and (not err?) seqs)
          (let ((buf2 (FIRST (cmio-page-widgets cmio ':eventio))))
            (gtk:entry-set-text buff name)
            (gtk:entry-set-text buf2 (imports->string seqs))
            (cmio-print cmio :message "Import ~s ok." file))
          (report-error (if err? (eval-error-string err?)
                            "Import: nothing imported.")
                        :window wind))))))

;;; cmio-execute should really be a generic function with methods
;;; on the various streams...
;;; Not really sure how to execute "intermediate" targets like .clm
;;; and .sco files unless the write command was used...

(defun cmio-execute (cmio)
  (let ((data1 (cmio-page-data cmio ':exec)))
    (with-data-errors-aborted (data1)
      (let* ((file (get-data data1 ':file))
             (wdir (get-data data1 ':directory))
             (path (namestring (merge-pathnames file wdir)))
             (exestr "Executing: ~(~S~)")
             type data2 execute error? stream)
        ;; see if fully specified pathname exists.
        (when (not (probe-file path))
          (report-error (format nil " file ~S does not exist."
                                path)
                        :label "Execute:"
                        :entry (second (cmio-page-widgets cmio ':exec)))
          (error-abort))
        ;; get canonical type (keyword) of file or signal error
        (setq type (cmio-file-execution-type cmio path))
        (when (not type)
          (report-error (format nil " don't know how to execute ~S."
                                file)
                        :label "Execute:"
                        :entry (second (cmio-page-widgets cmio ':exec)))
          (error-abort))
        ;; See if we already have a stream for the file
        (let ((entry (find-execute-file cmio path))
              )
          (when entry (setq stream (second entry))))
        ;; get data from files page
        (setq data2 (cmio-page-data cmio ':files type))
        ;; get target page of "intermediate" file types like .clm and
        ;; .sco files.
        (with-data-errors-aborted (data2)
          ;; assemble command string or expr
          (multiple-value-setq (execute error?)
            (cmio-ensure-execute-command cmio type file stream data2))
          (cond ((not (null error?))
                 (report-error error?
                               :label "Execute:"
                               :window (cmio-window cmio))
                 (error-abort))
                ((null execute)         ; can this happen?
                 (cmio-print cmio :warning  
                             "Execute: No handler set (Files page).") 
                 (error-abort))
                ((consp execute) 
                 (cmio-print cmio :message exestr execute)
                 (gtk:widget-queue-draw (SECOND (cmio-output cmio)))
                 ;; stream is just a placeholder...
                 (multiple-value-setq (stream error?) (safeeval execute))
                 (when error?
                   (report-error (eval-error-string error?) 
                                 :window (cmio-window cmio))))
                (t ;; (stringp execute) 
                 (cmio-print cmio :message exestr execute)
                 (gtk:widget-queue-draw (SECOND (cmio-output cmio)))
                 (shell execute :wait (get-data data2 ':wait)
                        :output (get-data data2 ':output)))))))))


;;;
;;; :containers page
;;;

(gtk:define-signal-handler choose_container :void (widget data)
  ;; widget is menu, data in window
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':containers ))
         (which (gtk:option-menu-get-history widget)))
    (if (= which 0)
      (progn (gtk:entry-set-text (getf widgets ':name) "")
             (setf (getf widgets ':object) nil))
      (let ((seq (elt (getf widgets ':objects) (1- which))))
        (gtk:entry-set-text (getf widgets ':name)
                            (object-name seq))
        (setf (getf widgets ':object) seq)))
    (values)))

(gtk:define-signal-handler update_container_menu :void (widget data)
  ;; widget is button, data in window
  widget
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':containers )))
    (gtk:entry-set-text (getf widgets ':name) "")
    (set-container-menu data)
    (values)))

(gtk:define-signal-handler new_container :void (widget data)
  ;; widget is button, data in window
  widget 
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':containers ))
         (buff (getf widgets ':name))
         (name (strip-entry buff)))
    (cond ((string= name "")
           (report-error "New: missing container name." :entry buff))
          ((find-object name nil)
           (report-error "New: name already in use." :entry buff))
          (t
           (let ((obj (make-instance 'seq :name name)))
             (set-container-menu data obj)
             (cmio-print cmio :message "New ~(~A~) ~s ok." 
                         (class-name (class-of obj))
                         name))))
    (values)))

(gtk:define-signal-handler delete_container :void (widget data)
  ;; widget is button, data in window
  widget data
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':containers ))
         (object (getf widgets ':object)))
    (if (not object)
      (report-error "Delete: no container selected." :window data)
      (let ((name (object-name object)))
        (remhash name  *dictionary*)
        (setf (subobjects object) nil)
        (set-container-menu data)
        (gtk:entry-set-text (getf widgets ':name) "")
        (setf (getf widgets ':object) nil)
        (cmio-print cmio :message "Delete ~S ok." name)))
    (values)))

(gtk:define-signal-handler rename_container :void (widget data)
  ;; widget is button, data in window
  widget
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':containers ))
         (object (getf widgets ':object))
         (buff (getf widgets ':name))
         (name (strip-entry buff)))
    (cond ((string= name "")
           (report-error "Rename: missing container name." :entry buff))
          ((not object)
           (report-error "Rename: no container selected." :window data))
          ((string-equal name (object-name object))
           )
          ((find-object name nil)
           (report-error "Rename: name already in use." :entry buff))
          (t
           (rename-object object name)
           (set-container-menu data object)
           (cmio-print cmio :message "Rename ~s ok." name)))
    (values)))

; (progn (new seq :name 'asd)  (new seq :name 'azxc) (cmio))

(defun set-container-menu (window &optional sel)
  (let* ((cmio (widget->object window))
         (widgets (cmio-page-widgets cmio ':containers ))         
         (seqs (list-named-objects <seq>))
         (items (mapcar #'object-name seqs))
         (old  (getf widgets ':menu))
         (table (getf widgets :table))
         menu)
    (when old
      ;;(gtk:widget-destroy old) ; error! maybe the menushell?
      (gtk:container-remove table old))
    (setq menu (create-option-menu (cons "Containers" items)
                                   :changed (g:callback choose_container)
                                   :data window))
    (gtk:table-attach table menu 0 1 0 1   0 0 0 0)
    (gtk:widget-show menu)
    (setf (getf widgets :menu) menu)
    (setf (getf widgets ':objects) seqs)
    (when sel
      (let ((pos (position sel seqs)))
        ;; add 1 because "Containers" is first item
        ;; this calls the item signal!
        (gtk:option-menu-set-history menu (1+ pos))))
    (values)))

(defmethod cmio-create-page (cmio (page (eql :containers)) notebook)
  (let* ((label (gtk:label-new "Containers"))
         (window (cmio-window cmio))
         (table (gtk:table-new 2 2 nil))
         (hbox (gtk:hbox-new nil %sw))
         (widgets (list :table table :menu nil :objects nil :object nil))
         entry button)
    ;; widgets is
    ;; (:table <t> :menu <m> :objects () :name <e> :object nil)
    (gtk:container-set-border-width table %bw)
    (gtk:table-set-col-spacings table %sw)
    (gtk:table-set-row-spacings table %rs)
    (gtk:notebook-append-page notebook table label)
    (gtk:widget-show table)
    (gtk:widget-show label)
    (setq button (gtk:button-new-with-label "Update"))
    (g:signal-connect button "clicked" (g:callback update_container_menu)
                      window)
    (gtk:table-attach table button 0 1 1 2  0 0 0 0)    
    (gtk:widget-show button)
    (setq entry (gtk:entry-new ))
    (nconc widgets (list :name entry))
    (gtk:entry-set-width-chars entry 32)
    (gtk:table-attach table entry 1 2 0 1  0 0 0 0)
    (gtk:widget-show entry)
    (gtk:table-attach table hbox 1 2 1 2 0 0 0 0)
    (gtk:widget-show hbox)
    (setq button (gtk:button-new-with-label "Delete"))
    (g:signal-connect button "clicked" (g:callback delete_container)
                      window)
    (gtk:box-pack-end hbox button nil nil 0)
    (gtk:widget-show button)
    (setq button (gtk:button-new-with-label "Rename"))
    (g:signal-connect button "clicked" (g:callback rename_container)
                      window)
    (gtk:box-pack-end hbox button nil nil 0)
    (gtk:widget-show button)
    (setq button (gtk:button-new-with-label "New"))
    (g:signal-connect button "clicked" (g:callback new_container)
                      window)
    (gtk:box-pack-end hbox button nil nil 0)
    (gtk:widget-show button)
    (cmio-set-page-widgets cmio page widgets)
    (set-container-menu window)))

;;;
;;; :help page
;;;

(defmethod cmio-create-page (cmio (source (eql :help)) notebook)
  (stub-notebook-page cmio notebook "Help" t))

(gtk:define-signal-handler cmio_write :void (widget window)
  ;; widget is write button on the Event IO page.
  widget 
  (let* ((cmio (widget->object window))
         (tpage (cmio-current-target-page cmio)))
    (if (cmio-page-can-do? cmio tpage ':write)
      (if (target-system-loaded? tpage)
        (let ((edata (cmio-page-data cmio ':eventio)))
          (with-data-errors-aborted (edata)
            (cmio-clear-message cmio)
            (let ((widgets (cmio-page-widgets cmio ':eventio))
                  events error)
              events
              (multiple-value-setq (events error)
                (safeeval (get-data edata ':events)))
              (if error
                (progn (report-error (eval-error-string error)
                                     :label "Events:" :widget (first widgets))
                       (error-abort))
                (let ((starts (get-data edata :starts)))
                  (if (not starts)
                    (setq starts 0)
                    (unless (or (numberp starts)
                                (and (consp starts)
                                     (every #'numberp starts)))
                      
                      (report-error
                       (format nil
                               "Starts: not a number or list of numbers: ~S."
                               starts)
                       :widget (second widgets))
                      (error-abort))
;                    (progn (multiple-value-setq (starts error)
;                             (safeeval (get-data edata ':starts)))
;                           (if error
;                             (progn (report-error (eval-error-string error)
;                                                  :label "Starts:"
;                                                  :widget (second widgets))
;                                    (error-abort))))
                    )
                  (cmio-write cmio tpage events starts))))))
        (cmio-print cmio :error 
                    "Compose: ~@(~A~) system not loaded (see Systems page)."
                    tpage))
      (cmio-print cmio :warning "Target page '~@(~A~)' is not writable."
                  tpage))
    (values)))

(defmethod cmio-create-page (cmio (source (eql :eventio)) notebook)
  ;; Source information frame. box is window's vbox holding source and
  ;; destination fields.
  (let ((label (gtk:label-new "Event IO"))
        (window (cmio-window cmio))
        (table (gtk:table-new 3 2 nil))
        (opts (logior gtk:fill gtk:expand))
        (widgets (list))
        hbox entry button)
    (gtk:notebook-append-page notebook table label)
    (gtk:container-set-border-width table 5)
    (gtk:table-set-col-spacings table %bw)
    (gtk:table-set-row-spacings table %rs)
    (gtk:widget-show label)
    (gtk:widget-show table)
    (setq label (gtk:label-new "Events:"))
    (cmio-show-widget-required cmio label)
    (gtk:table-attach table label 0 1 0 1   0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (cmio-show-widget-evalable cmio entry)
    (setq widgets (list* entry widgets))
    (gtk:table-attach table entry 1 2 0 1  opts 0 0 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Starts:"))
    (gtk:table-attach table label 0 1 1 2  0 0 0 0)
    (gtk:widget-show label)

    (setq hbox (gtk:hbox-new nil 0))
    (gtk:table-attach table hbox 1 2 1 2  opts 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq widgets (list* entry widgets))
    (gtk:entry-set-width-chars entry 32)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (let ((hbox (gtk:hbox-new nil 2))
          (imag (gtk:image-new-from-stock "gtk-convert"
                                          gtk:icon-size-small-toolbar))
          (labl (gtk:label-new "Compose")))
      (gtk:label-set-text-with-mnemonic labl "_Compose")
      (setq button (gtk:button-new ))
      (gtk:container-add button hbox)
      (gtk:widget-show hbox)
      ;(gtk:container-set-border-width hbox 2)
      (gtk:box-pack-start hbox imag nil nil 0)
      (gtk:widget-show imag)
      (gtk:box-pack-start hbox labl nil nil 0)
      (gtk:widget-show labl))
    (setq widgets (list* button widgets))
    (gtk:box-pack-end hbox button  nil nil 0)
    (gtk:widget-show button)
    (g:signal-connect button "clicked" (g:callback cmio_write)
                      window)
    (cmio-set-page-widgets cmio :eventio (nreverse widgets))
    (values)))

(defmethod cmio-page-data (cmio (source (eql :eventio)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio ':eventio))
        (data (list source))
        check value error)
    ;; widgets is (<eventsbuf> <startsbuf> <writebutton>
    ;; since these two buffers hold random, unstructured data, users
    ;; will make so many typos and mistakes that we will both read and
    ;; (later) evaluate the data without errors, then check and report
    ;; any errors we find the usual way
    (flet ((saferead (s)
             (multiple-value-bind (a b) (string->expr s )
               (if b (list ':error b ':entry (first widgets))
                   a))))
      ;; get the events expression
      (add-entry-data data :events (first widgets) :nullok nil
                      :READ NIL) ; Don't read yet
    
      (setq check (get-data data ':events))
      (if (listp check) ; already error!
        (setq value (if (null check) 
                      (list :error +se-nullstring+ :entry (first widgets)
                            check)
                      check))
        (multiple-value-setq (value error)
          (safecall #'saferead check)))
      ;; value might be :error list already
      (if error ; lisp reading reported an error
        (setq value (list ':error (eval-error-string error)
                          :entry (first widgets))))
      (setf (get-data data ':events) value)
      ;; now get start time offset data
      (add-entry-data data :starts (second widgets)
                      :READ NIL)
      ;; optional...
      (when (setq check (get-data data ':starts))    
        (if (consp check) ; already error!
          (setq value check)
          (multiple-value-setq (value error)
            (safecall #'saferead check)))
        (if error ; lisp reading reported an error
            (setq value (list ':error (eval-error-string error)
                              :entry (second widgets))))
        (setf (get-data data ':starts) value))
      data)))

(defmethod cmio-set-page-data (cmio (page (eql :eventio)) args)
  (let ((widgets (cmio-page-widgets cmio page)))
    ;; widgets is (<eventsbuf> <startsbuf> <writebutton>
    (set-page-fields args :events (FIRST widgets)
                      :starts (SECOND widgets))))

;;;
;;; :ALSA page 
;;;

(defmethod cmio-create-page (cmio (target (eql :alsa)) notebook)
  (stub-notebook-page cmio notebook "ALSA"))

;;;
;;; :CLM page
;;;

(gtk:define-signal-handler clm_score_group :void (widget data)
  (set-subgroup-sensitivity data ':clm ':score-file
                            (gtk:toggle-button-get-active widget)))

(gtk:define-signal-handler clm_audio_group :void (widget data)
  (set-subgroup-sensitivity data ':clm ':sound-file
                            (gtk:toggle-button-get-active widget)))

(gtk:define-signal-handler clm_import :void (widget data)
  ;; widget is button data in main window
  widget
  (cmio-import (widget->object data) ':clm)
  (values))

(defmethod cmio-create-page (cmio (target (eql :clm)) notebook)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "CLM"))
        (widgets (list))
        (table (gtk:table-new 5 3 nil))
        (tops (logior gtk:fill gtk:expand))
        (hpad 0) ; 2
        hbox entry spin check data button)
    (gtk:container-set-border-width table %bw)
    (gtk:notebook-append-page notebook table label)
    (gtk:widget-show label)
    (gtk:widget-show table)
    (gtk:table-set-row-spacings table %rs)
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:table-attach table check 0 1 0 1   0 0 0 0 )
    (gtk:widget-show check) 
    (g:signal-connect check "toggled" (g:callback clm_score_group)
                      window)
    (setq label (gtk:label-new "Score file:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 0 1   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 0 1   tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 32)
    (gtk:entry-set-text entry "test.clm")
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versions"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq button (gtk:button-new-with-label "Import"))
    (setq data (list* button data))
    (g:signal-connect button "clicked" (g:callback clm_import)
                      window)
    (gtk:box-pack-end hbox button nil nil hpad)
    (gtk:widget-show button)
    ;; (x_ScoreFile label entry x_versions (import))
    (setq widgets (list* :score-file (nreverse data) widgets))
    ;; Audio group
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:toggle-button-set-active check t)
    (gtk:table-attach table check 0 1 1 2  0 0 0 0 )
    (gtk:widget-show check) 
    (g:signal-connect check "toggled" (g:callback clm_audio_group) window)
    (setq label (gtk:label-new "Sound file:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 1 2  0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 1 2  tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-text entry (if (boundp 'clm:*clm-file-name*)
                                clm:*clm-file-name* "test.aiff"))
    (gtk:entry-set-width-chars entry 32)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Play"))
    (setq data (list* check data))
    (gtk:toggle-button-set-active check t)
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq label (gtk:label-new "Srate:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil hpad)   
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-text entry (format nil "~D"
                                      (if (boundp 'clm:*clm-srate*)
                                        clm:*clm-srate* 22050)))
    (gtk:entry-set-width-chars entry 6)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Channels:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label) 
    (setq spin (gtk:spin-button-new-with-range 
                1 9 (if (boundp 'clm:*clm-channels*)
                      clm:*clm-channels* 1)))
    (setq data (list* spin data))
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil hpad)
    (gtk:widget-show spin) 
    ;; line 3
    (setq label (gtk:label-new "Scaled to:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 2 3   0 0 0 0 )
    (gtk:widget-show label) 
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 2 3   tops 0 0 0)
    (gtk:widget-show hbox) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 5)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Scaled by:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil hpad)   
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 5)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Clipped"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)   
    (gtk:widget-show check) 
    (setq check (gtk:check-button-new-with-label "Statistics"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)   
    (gtk:widget-show check) 
    (setq check (gtk:check-button-new-with-label "Verbose"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)   
    (gtk:widget-show check) 
    ;; line 4 (comment)
    (setq label (gtk:label-new "Comment:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 3 4   0 0 0 0 )
    (gtk:widget-show label) 
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:table-attach table hbox 2 3 3 4  tops 0 0 0)
    (gtk:widget-show hbox) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    ;; line 5 (reverb)
    (setq label (gtk:label-new "Reverb:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 4 5   0 0 0 0 )
    (gtk:widget-show label) 
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:table-attach table hbox 2 3 4 5  tops 0 0 0)
    (gtk:widget-show hbox) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 8)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Decay time:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil hpad)   
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 5)
    ;;(gtk:entry-set-text entry "2.5")
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Reverb data:"))
    (setq data (list* LABEL data))
    (gtk:box-pack-start hbox label nil nil hpad)   ; t t
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 16)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
;;    (setq label (gtk:label-new "Channels:"))
;;    (setq data (list* label data))
;;    (gtk:box-pack-start hbox label nil nil hpad)
;;    (gtk:widget-show label) 
;;    (setq spin (gtk:spin-button-new-with-range
;;                1 9 (if (boundp 'clm:*clm-channels*)
;;                      clm:*clm-channels* 1)))
;;    (setq data (list* spin data))
;;    (gtk:spin-button-set-digits spin 0)
;;    (gtk:box-pack-start hbox spin nil nil hpad)
;;    (gtk:widget-show spin)
    (setq widgets (list* :sound-file (nreverse data) widgets))
    (cmio-set-page-widgets cmio ':clm widgets)
    (set-subgroup-sensitivity window ':clm ':score-file nil)
    ;;(pprint (cmio-page-widgets cmio ':clm))
    (values)))

(defmethod cmio-page-data (cmio (page (eql :clm)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page))
        group)
    ;; widgets is
    ;; (:score-file (<c> <l> <e> <c> <b>)
    ;;  :sound-file (<c> <l> <e> <c> <l> <e> <l> <s> 
    ;;               <l> <e> <l> <e> <c> <c> <c>
    ;;               <l> <e>
    ;;               <l> <e> <l> <e> <l> <e> <l> ) ; <s>
    (setq group (getf widgets ':score-file))
    (when (or (null op)
              (gtk:toggle-button-get-active (first group)))
      (add-entry-data data :score-file (third group) :nullok nil :read nil)
      (add-check-data data :versions (fourth group)))
    (setq group (getf widgets ':sound-file))
    (when (or (null op)
              (gtk:toggle-button-get-active (first group)))
      (add-entry-data data :sound-file (third group) :nullok nil :read nil)
      (add-check-data data :play (fourth group))
      (add-entry-data data :srate (sixth group) :test #'numberp)
      (add-spin-data data :channels (eighth group) :result #'floor)
      (add-entry-data data :scaled-to (ELT group 9) :test #'numberp)
      (add-entry-data data :scaled-by (ELT group 11) :test #'numberp)
      (add-check-data data :clipped (ELT group 12))
      (add-check-data data :statistics (ELT group 13))
      (add-check-data data :verbose (ELT group 14))
      (add-entry-data data :comment (ELT group 16) :read nil) ; comment
      (add-entry-data data :reverb (ELT group 18) :test #'symbolp)
      (add-entry-data data :decay-time (ELT group 20) :test #'numberp)
      (add-entry-data data :reverb-data (ELT group 22) :test #'listp)
      ;;(add-spin-data data :reverb-channels (elt group 24) :result #'floor)
      )
    data))

(defmethod cmio-set-page-data (cmio (page (eql :clm)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':score-file))
        (widg2 (cmio-page-widgets cmio page ':sound-file)))
    ;; widgets is
    ;; (:score-file (<c> <l> <e> <c> <b>)
    ;;  :sound-file (<c> <l> <e> <c> <l> <e> <l> <s> 
    ;;               <l> <e> <l> <e> <c> <c> <c>
    ;;               <l> <e>
    ;;               <l> <e> <l> <e> <l> <e> <l> ) ; <s>
    (set-page-fields args :score-file (elt widg1 2)
                      :versions (cons :check (elt widg1 3))
                      :sound-file (elt widg2 2)
                      :play (cons :check (elt widg2 3))
                      :srate (elt widg2 5)
                      :channels (cons :spin (elt widg2 7))
                      :scaled-to (elt widg2 9) :scaled-by (elt widg2 11)
                      :clipped (cons :check (elt widg2 12)) 
                      :statistics (cons :check (elt widg2 13))
                      :verbose (cons :check (elt widg2 14))
                      :comment (elt widg2 16) :reverb (elt widg2 18) 
                      :decay-time (elt widg2 20)
                      :reverb-data (elt widg2 22))))

(defmethod cmio-ensure-write-stream (cmio (target (eql :clm)) data)
  cmio
  ;;(pprint data)
  (let ((snd (get-data data ':sound-file))
        (clm (get-data data ':score-file))
        playit? args stream)
    (when (and (null snd) (null clm))
      (return-from cmio-ensure-write-stream
        (values nil nil " no .clm or sound file specified.")))
    (when (and snd (not (member (pathname-type snd)
                                '("aiff" "snd" "wav")
                                :test #'equal)))
      (return-from cmio-ensure-write-stream
        (values nil nil "Only .snd .aiff and .wav allowed.")))      
    ;; playit? will be false unless audio is activated AND button
    ;; is checked.
    (setq playit? (get-data data ':play))
    ;; CAREFUL! :srate is first true audio arg in data so this
    ;; gathers just the true audio args in plist format.
    (setq args (member ':srate data))
    (cond ((null clm)
           ;; writng sound file without .clm file.
           (setq stream (or (find-object snd nil)
                            (make-instance 'clm-audio-stream :name snd
                                           :trace-output nil)))
           (set-audio-file-versions! nil)
           (set-audio-output-hook! nil)
           ;; never let with-sound call dac since we handle playback
           ;; from the Files page
           (setf (getf args ':play) NIL)
           (set-io-handler-args! stream args)
           (values stream (if playit? t nil) nil))
          (t
           ;; we are writing a score file and maybe also a soundfile.
           ;; if the latter then use clm-load to generate it.
           (setq stream (or (find-object clm nil)
                            (make-instance 'clm-stream
                                           :name clm)))
           (when snd 
             (setq args (list* :output snd :play playit?
                               args)))
           (set-clm-file-versions! (get-data data :versions))
           (set-clm-output-hook! nil)
           (set-io-handler-args! stream args)
           ;; we execute clm file if snd file was active
           (values stream (if snd t nil) nil)
           ))))

(defmethod cmio-ensure-execute-command (cmio (type (eql :clm))
                                        filename stream typedata)
  (let ((expr (get-data typedata ':clm)))
    (cond ((not stream)
           (values nil " no stream associated with file."))
          ((null expr) 
           (values nil " no .clm handler set on Files page."))
          ((stringp expr)
           (concatenate 'string expr " " filename))
          (t
           (let* ((data (cmio-page-data cmio ':clm))
                  (name (get-data data ':score-file))
                  (args (list)))
             ;; use current page data if file names are the same
             (if (string= (object-name stream) name)
               (let ((snd (get-data data ':sound-file)))
                 ;; CAREFUL :srate is beginning of audio data
                 (setq args (member ':srate data))
                 (when snd
                   (setq snd (namestring (merge-pathnames snd filename)))
                   (setq args (list* :output snd args)))
                 )
               (progn
                 (setq args (io-handler-args stream))))
             (push filename args)
             (cons (car expr) (nconc args (cdr expr))))))))

(defmethod cmio-ensure-execute-command (cmio (type (eql :audio)) filename 
                                        stream typedata)
  cmio stream
  (let ((expr (get-data typedata ':audio)))
    (cond ((null typedata) 
           (values nil " no Audio player set on Files page."))
          ((consp expr)
           (push filename (cdr expr))
           (values expr nil))
          (t
           (values (concatenate 'string expr " " filename) 
                   nil)))))

;;;
;;; :CMN page
;;;

(gtk:define-signal-handler rem_staffing_row :void (widget tool)
  ;; widget is remove button, tool is window
  (let* (;; car of data is vbox holding rows.
         ;; car of each row is hbox containing row widgets
         (cmio (widget->object tool))
         (data (cmio-page-widgets cmio ':cmn ':staffing))
         (rows (cdr data))
         ;; return row holding our widget. Check pointer EQness in CMU!
         ;;(delr (OR (find widget rows :test #'member)
         ;;          (error "Fixme! Can't find widget in row!")))
         delr )
    ;; find the row that contains our remove button
    ;; row is (<hbox> [id] [name] [meter] <clef> {+} {-}) 
    (setq delr
          (loop with me = (ptr->int widget)
                for row in rows
                for it = (SEVENTH row)
                when (equal me (ptr->int it))
                return row))
    (unless delr
      (error "Fixme! Can't find widget in row!"))
    (when (cdr rows) ;; never delete a single remaining row
      (setf (cdr data)
            (loop for r in rows unless (eq r delr) collect r))
      ;; car of row is row's hbox
      (gtk:widget-destroy (car delr)))
    (values)))

(gtk:define-signal-handler add_staffing_row :void (widget tool)
  ;; widget is button, tool is window ; vbox to addto
  widget
  (add-staffing-row tool)
  (values))

(defparameter *cmn-clefs* '(:both :treble :bass :alto :tenor))

(defun add-staffing-row (window )
  (let* ((cmio  (widget->object window))
         (rows (cmio-page-widgets cmio ':cmn ':staffing))
         (vbox (car rows))  ;; (<vbox> {row}+)
         (hbox (gtk:hbox-new nil %sw))
         (hpad 0)
         data label entry menu)
    ;; row is (<hbox> [id] [name] [meter] <clef> {+} {-})
    (setq data (list hbox))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Id:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:widget-show entry)
    (gtk:entry-set-width-chars entry 3)
    (gtk:entry-set-text entry (format nil "~D" (length (cdr rows))))
    (gtk:box-pack-start hbox entry nil nil hpad)    
    (setq label (gtk:label-new "Staff name:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 12)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Meter:"))
    (gtk:box-pack-start hbox label nil nil hpad)    
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry (if (cdr rows) 
                                (gtk:entry-get-text
                                 (FOURTH (end (cdr rows))))
                                "4/4"))
    (gtk:entry-set-width-chars entry 5)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Clef:"))
    ;(setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    ;(setq menu (create-option-menu '("Both" "Treble" "Bass" "Alto" "Tenor" )))
    (setq menu (create-option-menu (loop for c in *cmn-clefs*
                                      collect (format nil "~@(~A~)" c))))
    (setq data (list* menu data))
    (gtk:box-pack-start hbox menu nil nil hpad)
    (gtk:widget-show menu)
    ;; add + and - control buttons at end of row
    (let ((box (gtk:hbox-new t 0))
          add rem)
      (gtk:box-pack-end hbox box nil nil 0)
      (gtk:widget-show box)
      (setq add (gtk:button-new-with-label "+"))
      (setq data (list* add data))
      (g:signal-connect add "clicked" (g:callback add_staffing_row)
                        window)
      (gtk:box-pack-start box add t t 0)
      (gtk:widget-show add)
      (setq rem (gtk:button-new-with-label "-"))
      (setq data (list* rem data))
      (g:signal-connect rem "clicked" (g:callback rem_staffing_row)
                        window)
      (gtk:box-pack-start box rem t t 0)
      (gtk:widget-show rem))
    (nconc rows (list (nreverse data)))))

(defmethod cmio-create-page (cmio (target (eql :cmn)) notebook)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "CMN"))
        (widgets (list))
        (vbox (gtk:vbox-new nil 0))
        (hpad 0)                        ; 2
        (tops (logior gtk:fill gtk:expand))
        (table (gtk:table-new 3 2 nil))
        hbox entry frame check data spin )
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-append-page notebook vbox label)
    (gtk:table-set-col-spacings table %sw)
    (gtk:widget-show label)
    (gtk:widget-show vbox)
    (gtk:box-pack-start vbox table nil nil 0)
    (gtk:widget-show table)
    ;; line1
    (setq label (gtk:label-new "Output file:"))
    (cmio-show-widget-required cmio label)
    (gtk:table-attach table label 0 1 0 1   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil 4))
    (gtk:table-attach table hbox  1 2 0 1   tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "test.eps")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versions"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check)
    (setq check (gtk:check-button-new-with-label "View"))
    (gtk:toggle-button-set-active check t)
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check)
 
    (setq check (gtk:check-button-new-with-label "One paged file"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq widgets (list* :output-file (nreverse data) widgets))
    ;; line 2
    (setq data nil)
    (setq label (gtk:label-new "Score title:"))
    (gtk:table-attach table label 0 1 1 2   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox  1 2 1 2  tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "HiHo!")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Score size:"))
    (gtk:box-pack-start hbox label nil nil hpad)    
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "40")
    (gtk:entry-set-width-chars entry 3)
    (gtk:widget-show entry)
    (setq widgets (list* :score (nreverse data) widgets))
    ;; line 3
    (setq data nil)
    (setq label (gtk:label-new "Metronome:"))
    (gtk:table-attach table label 0 1 2 3   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox  1 2 2 3  tops 0 0 0)
    (gtk:widget-show hbox)
    (setq spin (gtk:spin-button-new-with-range 30 400 1))
    (gtk:spin-button-set-digits spin 0)
    (gtk:spin-button-set-value spin 60)
    (gtk:box-pack-start hbox spin nil nil hpad)
    (gtk:widget-show spin)
    (setq data (list* spin data))
    ;;(setq entry (gtk:entry-new))
    ;;(setq data (list* entry data))    
    ;;(gtk:box-pack-start hbox entry nil nil hpad)
    ;;(gtk:entry-set-text entry "60")
    ;;(gtk:entry-set-width-chars entry 4)
    ;;(gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Exact rhythms"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq widgets (list* :timeline (nreverse data) widgets))
    (setq frame (gtk:frame-new "Staffing"))
    (gtk:box-pack-start vbox frame nil nil 0)
    (gtk:widget-show frame)
    ;; add vbox to hod staffing rows
    (let ((box (gtk:vbox-new nil 0)))
      (gtk:container-add frame box)
      (gtk:container-set-border-width box %bw)
      (gtk:widget-show box)
      (setq widgets (list* :staffing (list box) widgets)))
    (cmio-set-page-widgets cmio ':cmn widgets)
    (add-staffing-row window)
    ;;(pprint widgets)
    (values)))

(defmethod cmio-page-data (cmio (page (eql :cmn)) &optional op)
  op
  (flet ((parse-time-signature (str)
           (if (and (eql (elt str 0) #\()
                    (eql (string-readable? str) 1))
             (let ((l (string-read str)))
               (if (and (= (length l) 2)
                        (numberp (car l))
                        (numberp (cadr l)))
                 l
                 (values str +se-incorrect+)))
             (let ((del (position #\/ str)))
               (if (not del)
                 (values str +se-incorrect+)
                 (let ((n (parse-integer str :end del :junk-allowed t)))
                   (if (not n)
                     (values str +se-incorrect+)
                     (let ((d (parse-integer str :start (+ del 1)
                                             :junk-allowed t)))
                       (if d (list n d)
                           (values str +se-incorrect+))))))))))
    (let ((widgets (cmio-page-widgets cmio page))
          (data (list page))
          (staffs (list))
          group)
      ;; widgets is
      ;; (:output-file (<e> <c> <c> <c>) :score (<e> <e>)
      ;;  :timeline (<s> <c>) :staffing (<vbox> ROW+ ))
      ;; each row is (<hbox> [id] [name] [meter] <clef> {+} {-})
      (setq group (getf widgets ':output-file))
      (add-entry-data data :output-file (first group) :nullok nil :read nil)
      (add-check-data data :versions (second group))
      (add-check-data data :view (third group))
      (add-check-data data :all-output-in-one-file (fourth group))
      (setq group (getf widgets ':score))
      (add-entry-data data :title (first group) :read nil)
      (add-entry-data data :size (second group) :test #'numberp)
      (setq group (getf widgets ':timeline))
      (add-spin-data data :metronome (first group) :result #'floor )
      (add-check-data data :exact (second group))
      (setq widgets (CDR (getf widgets ':staffing)))
      (error-block
       (dolist (row widgets)
         (let ((staff (list :staff)))
           (add-entry-data staff :id (second row) :nullok nil)
           (add-entry-data staff :name (third row) :read nil)
           (add-entry-data staff :meter (fourth row)
                           :read #'parse-time-signature)
           (add-menu-data staff :clef (fifth row) *cmn-clefs*)
           ;; hack sets entire staffs list to first error value so
           ;; that abort-error can discover it in top-level of the
           ;; return list. = (:staff {:prop value}+)
           (loop for x in (cddr staff) by #'cddr
              when (and (consp x) (eql (car x) ':error))
              do (progn (setq staffs x)
                        (error-abort)))
           ;; set staff to normal staffing format (<id> :name ...)
           (setq staffs (nconc staffs (list (CDDR staff))))
           )))
      (nconc data (list :staffing staffs))
      data)))

(defmethod cmio-set-page-data (cmio (page (eql :cmn)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':output-file))
        (widg2 (cmio-page-widgets cmio page ':score))
        (widg3 (cmio-page-widgets cmio page ':timeline))
        (staff (second (member ':staffing args))))
    ;; widgets is
    ;; (:output-file (<e> <c> <c> <c>) :score (<e> <e>)
    ;;  :timeline (<s> <c>) :staffing (<vbox> ROW+ ))
    ;; each row is (<hbox> [id] [name] [meter] <clef> {+} {-})
    (set-page-fields args :output-file (elt widg1 0)
                      :versions (cons :check (elt widg1 1))
                      :view  (cons :check (elt widg1 2))
                      :all-output-in-one-file (cons :check (elt widg1 3))
                      :title (elt widg2 0) :size (elt widg2 1)
                      :metronome (cons ':spin (elt widg3 0))
                      :exact (cons ':check (elt widg3 1)))
    (when staff
      (let ((wind (cmio-window cmio))
            (rows nil))
        ;; add more rows if more than 1 staff
        (dotimes (i (- (length staff) 1)) (add-staffing-row wind))
        (setq rows (CDR (cmio-page-widgets cmio page ':staffing)))
        (loop for r in rows
           for s in staff
           when (and (consp s) (oddp (length s)))
           do (let ((i (pop s))
                    (n (getf s ':name))
                    (m (getf s ':meter))
                    (c (getf s ':clef)))
                (when i (gtk:entry-set-text (second r) (format nil "~A" i)))
                (when n (gtk:entry-set-text (third r) (format nil "~A" n)))
                (when m (gtk:entry-set-text (fourth r) (format nil "~A" m)))
                (when c
                  (gtk:option-menu-set-history (fifth r)
                                               (position c *cmn-clefs*)))))))
    ))

(defmethod cmio-ensure-write-stream (cmio (target (eql :cmn)) data)
  cmio
  (let* ((file (get-data data ':output-file))
         (type (pathname-type file))
         (args (loop for arg in '(:title :all-output-in-one-file :size
                                  :metronome)
                  collect arg collect (get-data data arg))))
    (if (not (member type '("eps" "cmn")
                     :test #'equal))
      (values nil nil " output file type not .eps or .cmn" )
      (let ((stream (or (find-object file nil)
                        (make-instance 'cmn-stream :name file))))
        (set-cmn-stream-versions! (get-data data :versions))
        (set-cmn-output-hook! nil)
        ;; why isnt there an accessor?
        (setf (slot-value stream 'exact) (get-data data ':exact))
        (set-io-handler-args! stream args)
        (setf (cmn-staffing stream) (get-data data ':staffing))
        ;; if its a .cmn file write it but dont execute (load) it.
        (values stream (and (equal type "eps") (get-data data ':view))
                nil)))))

(defmethod cmio-ensure-execute-command (cmio (type (eql :cmn))
                                        filename stream typedata)
  (cmio-ensure-execute-command cmio ':lisp filename stream typedata))

(defmethod cmio-ensure-execute-command (cmio (type (eql :eps))
                                        filename stream typedata)
  cmio stream
  (let ((expr (get-data typedata ':eps)))
    (cond ((null typedata) 
           (values nil " no Audio player set on Files page."))
          ((consp expr)
           (push filename (cdr expr))
           (values expr nil))
          (t
           (values (concatenate 'string expr " " filename) 
                   nil)))))

;;;
;;; :Csound page
;;;

(gtk:define-signal-handler csound_command_group :void (widget data)
  (set-subgroup-sensitivity data ':csound ':args
                            (gtk:toggle-button-get-active widget))
  (values))

(gtk:define-signal-handler sco_import :void (widget data)
  ;; widget is button data in main window
  widget
  (cmio-import (widget->object data) ':sco)
  (values))

(defmethod cmio-create-page (cmio (target (eql :csound)) notebook)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "Csound"))
        (table (gtk:table-new 5 3 nil))
        (widgets (list))
        (tops (logior gtk:fill gtk:expand))
        (hpad 0) ;5
        hbox data entry check button)
    (gtk:container-set-border-width table %bw)
    (gtk:table-set-row-spacings table %rs)
    (gtk:notebook-append-page notebook table label)
    (gtk:widget-show label)
    (gtk:widget-show table)
    ;; line 1 (score file)
    (setq label (gtk:label-new "Score file:"))
    (cmio-show-widget-required cmio label)
    (gtk:table-attach table label 1 2 0 1   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 0 1  tops 0 0 0 )
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new ))
    (setq data (list entry))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "test.sco")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versions"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq button (gtk:button-new-with-label "Import"))
    (g:signal-connect button "clicked" (g:callback sco_import)
                      window)
    (setq data (list* button data))
    (gtk:box-pack-end hbox button nil nil hpad)
    (gtk:widget-show button)
    ;; line 2 Header -- this should be multiline..
    (setq label (gtk:label-new "Header:"))
    (gtk:table-attach table label 1 2 1 2   0 0 0 0 )
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:table-attach table entry 2 3 1 2   tops 0 0 0)
    (gtk:widget-show entry)
    (setq widgets (list* :score-file (nreverse data) widgets))

    ;; line 3 Orchestra
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:table-attach table check 0 1 2 3  0 0 0 0 )
    (g:signal-connect check "toggled" (g:callback csound_command_group)
                      window)
    (gtk:widget-show check)
    (setq label (gtk:label-new "Orchestra:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 2 3   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 2 3  tops 0 0 0 )
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    ;; line 3 Sound
    (setq label (gtk:label-new "Sound:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 3 4  0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 3 4  tops 0 0 0 )
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-text entry "devaudio")
    (setq data (list* entry data))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq widgets (list* :args (nreverse data) widgets))
    (cmio-set-page-widgets cmio ':csound widgets)
    (set-subgroup-sensitivity window ':csound ':args nil)
    ;;(pprint (cmio-page-widgets cmio :csound))
    (values)))

(defmethod cmio-page-data (cmio (source (eql :csound)) &optional op)
  (let ((widgets (cmio-page-widgets cmio ':csound))
        (data (list source))
        group1 group2)
    ;; widgets is
    ;; (:score-file (<e> <c> <b> <e>) :args (<c> <l> <e> <l> <e> )
    (setq group1 (getf widgets ':score-file))
    (setq group2 (getf widgets ':args))

    (add-entry-data data :score-file (first group1)
                    :nullok nil :read nil)      
    (add-check-data data :versions (second group1))
    (add-entry-data data :header (fourth group1) :read nil)
    ;; if op is :write then its an error to have the Orchestra
    ;; checked w/out an actual file name specified.
    (when (or (null op)
              (gtk:toggle-button-get-active (first group2)))
      (add-entry-data data :orchestra (third group2)
                      :nullok (not op) :read nil)
      (add-entry-data data :sound (fifth group2) :read nil))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :csound)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':score-file))
        (widg2 (cmio-page-widgets cmio page ':args)))

    ;; (:score-file (<e> <c> <b> <e>) :args (<c> <l> <e> <l> <e> )
    (set-page-fields args :score-file (first widg1)
                     :versions (cons ':check (second widg1))
                     :header (fourth widg1)
                     :orchestra (third widg2)
                     :sound (fifth widg2))))


(defmethod cmio-ensure-write-stream (cmio (target (eql :csound)) data)
  cmio
  (let ((name (get-data data ':score-file))
        (orc (get-data data ':orchestra))
        stream error?)
    (setq stream (or (find-object name nil)
                     (make-instance <sco-stream> :name name)))
    (set-sco-output-hook! nil)
    (set-sco-file-versions! (get-data data ':versions))
    (init-io stream :header (get-data data ':header)
             :orchestra orc :sound (get-data data ':sound))
    (values stream (if orc t nil) error?)))

(defmethod cmio-ensure-execute-command (cmio (type (eql :sco)) filename 
                                        stream typedata)
  (let ((cmd (get-data typedata ':sco)))
    (cond ((not stream)
           (values nil " no stream associated with file."))
          ((null cmd)
           (values nil " no Csound command set on Files page."))
          (t
           ;; if the name of the stream is the same as the name in the
           ;; Target page, use the current contents, else use the
           ;; cached streams contents.
           (let* ((data (cmio-page-data cmio ':csound))
                  (name (get-data data ':score-file))
                  orc snd)
             (if (string= (object-name stream) name)
               (setq orc (get-data data ':orchestra)
                     snd (get-data data ':sound))
               (let* ((args (io-handler-args stream)))
                 (setq orc (getf args ':orchestra))
                 (setq snd (getf args ':sound))))
             (cond ((null orc)
                    (values nil " no orchestra file."))
                   (t
                    (when snd
                      (setq cmd (concatenate 'string cmd 
                                             " -o " snd)))
                    (values (concatenate 'string cmd " " orc " " filename)
                            nil))))))))

;;;
;;; :MIDI page
;;;

(gtk:define-signal-handler midi_tempo_sensitivity :void (widget data)
  (let* ((active? (gtk:toggle-button-get-active widget))
         (cmio (widget->object data))
         (widgets (getf (cmio-page-widgets cmio ':midi ':importing)
                        ':time-format)))
    ;; widgets is (<r> <r> <l> <e>), set <l> and <e> insensitive
    (dolist (w (cddr widgets))
      (gtk:widget-set-sensitive w active?))
    (values)))

(gtk:define-signal-handler micro_divisions :void (widget data)
  ;; widget is radio button, data is spin button
  (let ((active? (gtk:toggle-button-get-active widget)))
    (gtk:widget-set-sensitive data active?)))

(defun add-microtuning (hbox)
  ;; Microtuning
  (let (label button spin data)
    (setq label (gtk:label-new "Microtuning:"))
    ;;(push label data) dont need this
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "None"))
    (push button data)
    (gtk:toggle-button-set-active button t)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Half-step divisions:"))
    (push button data)
    ;; have to create spin here for callback
    (setq spin (gtk:spin-button-new-with-range 2 16 1))
    (g:signal-connect button "toggled" (g:callback micro_divisions)
                      spin)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    ;; spin created earlier
    (push spin data)
    (gtk:widget-set-sensitive spin nil)
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil 0)
    (gtk:widget-show spin)
    (setq button (gtk:radio-button-new-with-label-from-widget
                  button "Note by note"))
    (push button data)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (nreverse data)))

(gtk:define-signal-handler midi_import :void (widget data)
  ;; widget is button data in main window
  widget
  (cmio-import (widget->object data) ':midi)
  (values))

(defmethod cmio-create-page (cmio (target (eql :midi)) notebook)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "MIDI"))
        (vbox (gtk:vbox-new nil 0))
        (widgets1 (list))
        (widgets (list))
        (hpad 0)
        label2 hbox hbox2 data entry check button frame)

    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-append-page notebook vbox label)
    (gtk:widget-show vbox)
    (gtk:widget-show label)

    ;; Microtuning
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq data (add-microtuning hbox))
    (setq widgets (list* :microtuning data widgets))

    ;; line 1 (score file)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "MIDI file:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)    
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list entry))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "test.mid")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versions"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq check (gtk:check-button-new-with-label "Play"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:toggle-button-set-active check t)
    (gtk:widget-show check) 
    (setq button (gtk:button-new-with-label "Import"))
    (g:signal-connect button "clicked" (g:callback midi_import)
                      window)
    (gtk:box-pack-end hbox button nil nil hpad)
    (gtk:widget-show button)
    (setq widgets1 (list* :midi-file (nreverse data) widgets))

    ;; Importing frame
    (setq widgets (list))
    (setq hpad 0)
    (setq frame (gtk:frame-new "Importing"))
    (gtk:box-pack-start vbox frame nil nil 0)
    (gtk:widget-show frame)
    (setq vbox (gtk:vbox-new nil 0))
    (gtk:container-add frame vbox)
    (gtk:container-set-border-width vbox %bw)
    (gtk:widget-show vbox)
    ;; line 1 time and keynum format
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Time format:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "Beats"))
    (setq data (list button))
    (g:signal-connect button "toggled" (g:callback midi_tempo_sensitivity)
                      window)
    (gtk:box-pack-start hbox button nil nil hpad)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Ticks"))
    (setq data (list* button data))
    (gtk:box-pack-start hbox button nil nil hpad)
    (gtk:widget-show button)
    (setq label (gtk:label-new "Override tempo:"))
    (setq data (list* label data))
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 4) 
    (gtk:widget-show entry)
    ;;LABEL AND ENTRY ADDED NEXT LINE
    (setq widgets (list* ':time-format (nreverse data) widgets))    
    (setq hbox2 (gtk:hbox-new nil %sw))
    (gtk:box-pack-end hbox hbox2 nil nil 0)
    (gtk:widget-show hbox2)
    (setq label2 (gtk:label-new "Keynum format:"))
    (gtk:box-pack-start hbox2 label2 nil nil hpad)
    (gtk:widget-show label2)
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "Keynum"))
    (setq data (list button))
    (gtk:box-pack-start hbox2 button nil nil hpad)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Note"))
    (setq data (list* button data))
    (gtk:box-pack-start hbox2 button nil nil hpad)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Hertz"))
    (setq data (list* button data))
    (gtk:box-pack-start hbox2 button nil nil hpad)
    (gtk:widget-show button)
    (setq widgets (list* ':keynum-format (nreverse data) widgets))    
    ;; line 2
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (setq hbox2 (gtk:hbox-new nil 10))    
    (gtk:box-pack-end hbox hbox2 nil nil 0)
    (gtk:widget-show hbox2)
    (setq check (gtk:check-button-new-with-label "Exclude meta messages."))
    (setq widgets (list* :meta-exclude check widgets))
    (gtk:box-pack-start hbox2 check nil nil hpad)
    (gtk:widget-show check)
    (setq label (gtk:label-new "Exclude tracks:"))
    (gtk:box-pack-start hbox2 label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq widgets (list* :exclude-tracks entry widgets))
    (gtk:box-pack-start hbox2 entry nil nil hpad)
    (gtk:entry-set-width-chars entry 12) 
    (gtk:widget-show entry)
    (cmio-set-page-widgets cmio ':midi (append widgets1
                                               (list :importing widgets)))
    ;;(pprint (cmio-page-widgets cmio ':midi))
    (values)))

(defmethod cmio-page-data (cmio (target (eql :midi)) &optional op)
  (let ((data (list target))
        (widgets (cmio-page-widgets cmio ':midi))
        group)
    ;; widgets is:
    ;; x_Microtuning o_note-by-note o_step divisions {1}
    ;; (:midi-file (<e> <c> <c>)
    ;;  :microtuning (<r> <r> <s> <r>)
    ;;  :importing (:exclude-tracks <e> :meta-exclude <c>
    ;;              :keynum-format (<r> <r> <r>)
    ;;              :time-format (<r> <r> <l> <e>)
    (setq group (getf widgets ':midi-file))
    (add-entry-data data :midi-file (first group) :nullok nil :read nil)    
    (add-check-data data :versions (second group))
    (add-check-data data :play (third group))
    ;; microtuning group
    (setq group (getf widgets ':microtuning))
    (if (gtk:toggle-button-get-active (first group))
      (nconc data (list :microtuning nil))
      (if (gtk:toggle-button-get-active (second group))
        (add-spin-data data :microtuning (third group) 
                       :result #'floor)
        (nconc data (list :microtuning t))))
    (when (or (not op) (eql op :importing))
      (setq widgets (getf widgets ':importing))
      (setq group (getf widgets ':time-format))
      (add-radio-data data :time-format (first group) ':beats)
      (add-radio-data data :time-format (second group) ':ticks)
      (if (eql (getf (CDR data) ':time-format) ':beats)
        (add-entry-data data :override-tempo (fourth group)
                        :test #'numberp))
      (setq group (getf widgets ':keynum-format))
      (add-radio-data data :keynum-format (first group) ':keynum)
      (add-radio-data data :keynum-format (second group) ':note)
      (add-radio-data data :keynum-format (third group) ':hertz)
      (add-check-data data :meta-exclude
                      (getf widgets ':meta-exclude))
      (add-entry-data data :exclude-tracks (getf widgets ':exclude-tracks)
                      :multiok t))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :midi)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':midi-file))
        (widg2 (cmio-page-widgets cmio page ':microtuning))
        (widg3 (cmio-page-widgets cmio page ':importing)))
    ;; widgets is:
    ;; (:microtuning (<r> <r> <s> <r>)
    ;;  :midi-file (<e> <c> <c>)
    ;;  :importing (:exclude-tracks <e> :meta-exclude <c>
    ;;              :keynum-format (<r> <r> <r>)
    ;;              :time-format (<r> <r> <l> <e>))
    (set-page-fields args
                      :midi-file (first widg1)
                      :microtuning (list :radio
                                         :none (first widg2)
                                         :divisions (second widg2)
                                         :note-by-note (third widg2))
                      :exclude-tracks (getf widg3 :exclude-tracks)
                      :meta-exclude (cons :check (getf widg3 :meta-exclude))
                      :keynum-format
                      (let ((fmat (getf widg3 :keynum-format)))
                        (list :radio :keynum (first fmat)
                              :note (second fmat)
                              :hertz (third fmat)))
                      :override-tempo
                      (let ((fmat (getf widg3 :time-format)))
                        (fourth fmat))
                      :time-format
                      (let ((fmat (getf widg3 :time-format)))
                        (list :radio :beats (first fmat)
                              :ticks (second fmat))))))

(defmethod cmio-ensure-write-stream (cmio (target (eql :midi)) data)
  cmio
  (let ((name (get-data data ':midi-file))
        stream error?)
    (setq stream (or (find-object name nil)
                     (make-instance <midi-file-stream> :name name)))
    (set-midi-output-hook! nil)
    (set-midi-file-versions! (get-data data ':versions))
    (init-io stream :channel-tuning (get-data data :microtuning))
    (values stream (get-data data ':play) error?)))

(defmethod cmio-ensure-execute-command (cmio (type (eql :midi)) filename 
                                        stream typedata)
  cmio stream
  (if (null typedata)
    (values nil " no Midi player set (Files page).")
    (concatenate 'string (get-data typedata ':midi) " " filename)))

;;;
;;; :MidiShare page
;;;

; (cmio)

(gtk:define-signal-handler ms_activate :void (widget data)
  (let ((act? (gtk:toggle-button-get-active widget)))
    (if (g:nullptr? (g:object-get-data widget "midiport"))
      (set-subgroup-sensitivity data ':midishare ':player act?)
      (set-subgroup-sensitivity data ':midishare ':midi-port act?))))

(gtk:define-signal-handler ms_open_midi :void (widget data)
  ;; widget is button data is window
  widget data
  (let ((cmio (widget->object data)))
    (cond ((not (target-system-loaded? ':midishare))
           (report-error "Midishare not loaded (see Systems page)."
                         :window data))
          ((midi-open?)
           (cmio-print cmio :warning "Open: \"midi.port\" already open."))
          (t (midi-open)
             (cmio-print cmio :message "Open \"midi.port\" ok.")))))

(gtk:define-signal-handler ms_close_midi :void (widget data)
  ;; widget is button data is window
  widget data
  (let ((cmio (widget->object data)))
    (cond ((not (target-system-loaded? ':midishare))
           (report-error "Midishare not loaded (see Systems page)."
                         :window data))
           ((not (midi-open?))
           (cmio-print cmio :warning "Close: \"midi.port\" already closed."))
          (t (midi-close)
             (cmio-print cmio :message "Close \"midi.port\" ok.")))))

(gtk:define-signal-handler ms_play :void (widget data)
  widget
  (do-player data ':play))

(gtk:define-signal-handler ms_stop :void (widget data)
  widget
  (do-player data ':stop))

(gtk:define-signal-handler ms_continue :void (widget data)
  widget
  (do-player data ':continue))

(gtk:define-signal-handler ms_tempo :void (widget data)
  (do-player data ':tempo (gtk:spin-button-get-value widget)))

(defun do-player (window op &optional arg)
  (let* ((cmio (widget->object window))
         (data (list op))
         (buff (THIRD (getf (cmio-page-widgets cmio ':midishare)
                            ':player))))
    (add-entry-data data :player buff :nullok nil :read nil)
    (with-data-errors-aborted (data)
      (let* ((name (get-data data ':player))
             (stream (find-object name)))
        (if (or (not stream)
                (not (typep stream 'player-stream)))
          (report-error (format nil "~@(~A~): no player named ~S."
                                op name) :entry buff)
          (ecase op
            (:play (player-play stream))
            (:stop (player-stop stream))
            (:continue (player-cont stream))
            (:tempo (player-set-tempo stream (floor arg)))))))))
            
(defmethod cmio-create-page (cmio (source (eql :midishare)) notebook)
  (let ((label (gtk:label-new "Midishare"))
        (window (cmio-window cmio))
        ;;(tops (logior gtk:fill gtk:expand))
        (widgets (list))
        (subs (list))
        (vbox (gtk:vbox-new nil 0))
        table radio hbox spin button entry check )
    (gtk:notebook-append-page notebook vbox label)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:container-set-border-width hbox %bw)
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq subs (add-microtuning hbox))
    (setq widgets (list* :microtuning subs widgets))
    ;; table setup
    (setq table (gtk:table-new 4 2 nil))
    (gtk:box-pack-start vbox table nil nil 0)
    (gtk:table-set-col-spacings table %sw)
    (gtk:table-set-row-spacings table %rs)
    (gtk:widget-show table)
    ;; midi port line
    (setq subs (list))
    (setq radio (gtk:radio-button-new-with-label
                  (g:nullptr) "Midi port:   "))
    (push radio subs)
    ;; identify radio choice by adding window as property
    ;; warning! this hack wont work if another radio is added!
    (g:object-set-data radio "midiport" window)
    (g:signal-connect radio "toggled" (g:callback ms_activate) 
                      window)
    (gtk:table-attach table radio 0 1 0 1   0 0 0 0)
    (gtk:widget-show radio)
    (gtk:toggle-button-set-active radio t)
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:table-attach table hbox 1 2 0 1  gtk:fill 0 0 0)    
    (gtk:widget-show hbox)
    (setq button (gtk:button-new-with-label "Open"))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback ms_open_midi)
                      window)
    (gtk:box-pack-start hbox button nil nil %sw)
    (gtk:widget-show button)
    (setq button (gtk:button-new-with-label "Close"))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback ms_close_midi)
                      window)
    (gtk:box-pack-start hbox button nil nil %sw)
    (gtk:widget-show button)
    (setq widgets (list* :midi-port (nreverse subs) widgets))
    ;; midi player
    (setq subs (list))
    (setq radio (gtk:radio-button-new-with-label-from-widget 
                 radio "Midi player:"))
    (push radio subs)
    (g:signal-connect radio "toggled" (g:callback ms_activate) 
                      window)
    (gtk:table-attach table radio 0 1 1 2   0 0 0 0)
    (gtk:widget-show radio)
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:table-attach table hbox 1 2 1 2   GTK:FILL 0 0 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Name: "))
    (push label subs)
    (gtk:box-pack-start hbox label nil nil %sw)
    (cmio-show-widget-required cmio label)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (push entry subs)
    (gtk:entry-set-text entry "test.mp")
    (gtk:entry-set-width-chars entry 32) 
    (gtk:box-pack-start hbox entry nil nil %sw)    
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Track:"))
    (push label subs)
    (gtk:box-pack-start hbox label nil nil %sw)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 0 7 1))
    (push spin subs)
    (gtk:widget-show spin)
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil %sw)
    (setq check (gtk:check-button-new-with-label "Play"))
    (push check subs)
    (gtk:toggle-button-set-active check t)
    (gtk:box-pack-start hbox check nil nil %sw)    
    (gtk:widget-show check)
    ;; player controls
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:table-attach table hbox 1 2 2 3   gtk:fill 0 0 0)    
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Tempo:"))
    (push label subs)
    (gtk:box-pack-start hbox label nil nil %sw)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 30 400 1))
    (push spin subs)
    (gtk:spin-button-set-value spin 120)
    (gtk:widget-show spin)
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil %sw)
    ;; connect spin after so value doesnt trigger
    (g:signal-connect spin ;; (gtk:spin-button-get-adjustment spin)
                      "value_changed" (g:callback ms_tempo)
                      window)
    (setq button (gtk:button-new-with-label "Play"))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback ms_play) window)    
    (gtk:box-pack-start hbox button nil nil %sw)
    (gtk:widget-show button)
    (setq button (gtk:button-new-with-label "Stop"))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback ms_stop) window) 
    (gtk:box-pack-start hbox button nil nil %sw)
    (gtk:widget-show button)
    (setq button (gtk:button-new-with-label "Continue"))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback ms_continue) window) 
    (gtk:box-pack-start hbox button nil nil %sw)
    (gtk:widget-show button)
    ;; add widgets
    (setq widgets (list* :player (nreverse subs) widgets))
    (cmio-set-page-widgets cmio ':midishare widgets)
    (set-subgroup-sensitivity window ':midishare ':player nil)
    ;;(pprint (cmio-page-widgets cmio ':midishare))
    ))

(defmethod cmio-page-data (cmio (page (eql :midishare)) &optional op)
  op
  (let* ((widgets (cmio-page-widgets cmio page))
         (data (list page))
         (group (getf widgets ':midi-port)))
    ;; widgets is:
    ;; (:midi-port (<r> <b> <b>)
    ;;  :player (<r> <l> <e> <l> <s> <c>
    ;;           <l> <s> <b> <b> <b>
    ;;  :microtuning (<r> <r> <s> <r>))
    (add-radio-data data :midi-port (first group) t nil)
    ;; add player group
    (setq group (getf widgets ':player))
    (add-entry-data data :player (third group)
                    :nullok (get-data data ':midi-port)
                    :read nil)
    (add-spin-data data :track (fifth group) :result #'floor)
    (add-check-data data :play (sixth group))
    (add-spin-data data :tempo (eighth group) :result #'floor)
    ;; add microtone group
    (setq group (getf widgets ':microtuning))
    (if (gtk:toggle-button-get-active (first group))
      (nconc data (list :microtuning nil))
      (if (gtk:toggle-button-get-active (second group))
        (add-spin-data data :microtuning (third group) 
                       :result #'floor)
        (nconc data (list :microtuning t))))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :midishare)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':midi-port))
        (widg2 (cmio-page-widgets cmio page ':microtuning))
        (widg3 (cmio-page-widgets cmio page ':player)))
    ;; widgets is:
    ;; (:midi-port (<r> <b> <b>)
    ;;  :player (<r> <l> <e> <l> <s> <c>
    ;;           <l> <s> <b> <b> <b>
    ;;  :microtuning (<r> <r> <s> <r>))
    (set-page-fields args
                     :microtuning (list :radio
                                        :none (first widg2)
                                        :divisions (second widg2)
                                        :note-by-note (third widg2))
                     :connection (list :radio
                                       :midi-port (first widg1)
                                       :midi-player (first widg3))
                     :name (elt widg3 2) 
                     :track (cons ':spin (elt widg3 4))
                     :play (cons ':check (elt widg3 5)))))

(defmethod cmio-ensure-write-stream (cmio (target (eql :midishare)) data)
  cmio
  (if (target-system-loaded? :midishare)
    (let ((port (get-data data ':midi-port))
          stream)
      (if port
        (setq stream "midi.port")
        (let ((name (get-data data ':player)))
          (setq stream (or (find-object name nil)
                           (make-instance <player-stream> :name name)))
          (setf (player-stream-play stream) (get-data data ':play))
          (setf (player-stream-tempo stream) (get-data data ':tempo))))
      (values (init-io stream :channel-tuning 
                       (get-data data ':microtuning))
              nil
              nil))
    (values nil nil " Midishare not loaded.")))

;;;
;;; :plotter page
;;;

(gtk:define-signal-handler plotter_prototype_menu_changed :void (widget data)
  ;; widget is menu data is window
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':plotter))
         (which (gtk:option-menu-get-history widget)))
    widgets which
  (values)))

(gtk:define-signal-handler plotter_slot2_sensitivity :void (widget data)
  ;; "vert" is true if widget belongs to vertical 
  (let* ((vert (not (g:nullptr? (g:object-get-data widget "y-axis"))))
         (cmio (widget->object data))
         (list (cmio-page-widgets cmio :plotter 
                                (if vert :y-axis :x-axis)))
         (subs (getf list ':slot2))
         (act? (gtk:toggle-button-get-active widget)))
    ;;(print (list :vert-> vert))
    (dolist (w (cdr subs)) (gtk:widget-set-sensitive w act?))
    (values)))

(gtk:define-signal-handler plotter_choose_prototype :void (widget data)
  ;; "vert?" is true if widget belongs to vertical 
  (let* ((vert? (not (g:nullptr? (g:object-get-data widget "y-axis"))))
         (cmio (widget->object data))
         (plist (cmio-page-widgets cmio ':plotter 
                                 (if vert? ':y-axis ':x-axis)))
         (types (loop for x in (cdr (getf plist ':prototypes)) ; car is menu
                   collect (first (third x))))
         (buffs (getf plist ':protoinits))
         (which (gtk:option-menu-get-history widget)))
    ;(print (list which (nth which types) (axis-type (nth which types))))
    (prototype-fill-buffers (nth which types) buffs)
    (values)))

(defun prototype-fill-buffers (proto buffs)
  (let (val)
    (setq val (axis-minimum proto))
    (gtk:entry-set-text (first buffs) (if val (format nil "~S" val) ""))
    (setq val (axis-maximum proto))
    (gtk:entry-set-text (second buffs) (if val (format nil "~S" val) ""))
    (setq val (axis-increment proto))
    (gtk:entry-set-text (third buffs) (if val (format nil "~S" val) ""))
    (setq val (axis-ticks-per-increment proto))
    (gtk:spin-button-set-value (fourth buffs) (or val 1))
    (values)))

(defun create-axis-page (window nb dimension &key omit only slot1 slot2
                          title selected)
  (let ((protos (list))
        (cmio (widget->object window))
        (vert? (eql dimension ':y-axis))
        (vbox (gtk:vbox-new nil %rs))
        (label (gtk:label-new (keyword->label dimension nil)))
        (hpad 0)
        hbox data widgets menu check entry spin)
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-append-page nb vbox label)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; get prototype data
    (maphash (lambda (k a) 
               (push (list (format nil "~@(~A~)" (or k "values")) k a) 
                     protos))
             *axis-prototypes*)
    (unless title (setf title (keyword->label dimension nil)))
    (if only
      (setq protos (loop for x in protos
                      when (member (second x) only) collect x))
      (if omit
        (setq protos (loop for x in protos 
                        unless (member (second x) omit) collect x))))
    ;; NIL selected returns default axis type (the nil prototype)
    (setq selected (or (find selected protos :key #'second)
                       (error "Not a prototype type: ~S." selected)))
    (setq protos (delete selected protos))
    (when (cdr protos)
      (setq protos (sort protos #'string-lessp :key #'first)))
    (push selected protos)
    ;; line 1
    ;; "X axis:" <menu> "From slot:" [time] [x] "and slot" [  ]
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    ;;(setq label (gtk:label-new (if vert? "Y axis:" "X axis:")))
    ;;(gtk:box-pack-start hbox label nil nil hpad)
    ;;(gtk:widget-show label)
    (setq menu (create-option-menu
                (mapcar #'first protos)
                :changed (g:callback plotter_choose_prototype)
                :data window))
    ;; set "vertical" property so handler can distinguish dimensions
    (if vert? 
      (g:object-set-data menu "y-axis" menu))
    (gtk:box-pack-start hbox menu nil nil hpad)
    (gtk:widget-show menu)
    ;; :prototypes (<menu> protos)
    (setq widgets (list* ':prototypes (list* menu protos) widgets) )
    (setq label (gtk:label-new "Slot:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list entry))
    (gtk:entry-set-width-chars entry 12)
    (when slot1 (gtk:entry-set-text entry (format nil "~(~A~)" slot1)))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry) 
    ;; :slot1 (<buffer>)
    (setq widgets (list* ':slot1 (nreverse data) widgets))
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:toggle-button-set-active check nil)
    (g:signal-connect check "toggled"
                      (g:callback plotter_slot2_sensitivity)
                      window)
    ;; set vertical property so handler can distinguish dimensions
    (if vert? 
      (g:object-set-data check "y-axis" check))
    ;; optional slot2
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check)
    (setq label (gtk:label-new "and slot:"))
    (setq data (list* label data))
    (gtk:widget-set-sensitive label nil)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:widget-set-sensitive entry nil)
    (gtk:entry-set-width-chars entry 12)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry) 
    ;; :slot2 (<check> <label> <buffer>)
    (setq widgets (list* ':slot2 (nreverse data) widgets)) 
    (when slot2
      (gtk:entry-set-text entry (format nil "~(~A~)" slot2))
      ;; this will sensitize label and entry
      (gtk:toggle-button-set-active check t))
    ;; line 2
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq data nil)
    (dolist (s '("Minimum:" "Maximum:" "Increment:"))
      (setq label (gtk:label-new s))
      (gtk:box-pack-start hbox label nil nil hpad)
      (gtk:widget-show label)
      (setq entry (gtk:entry-new))
      (setq data (list* entry data))
      (gtk:entry-set-width-chars entry 10)
      (gtk:box-pack-start hbox entry nil nil hpad)
      (gtk:widget-show entry) )
    (setq label (gtk:label-new "Ticks:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 1 24 1))
    (setq data (list* spin data))
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil hpad)
    (gtk:widget-show spin)
    ;; :protoinits (<menu> (protos))
    (setq data (nreverse data))
    (setq widgets (list* ':protoinits data widgets))
    (prototype-fill-buffers (first (third selected)) data)
    widgets))

; (cmio)

(defmethod cmio-create-page (cmio (target (eql :plotter)) notebook)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "Plotter"))
        (widgets (list))
        (hpad 0)
        (vbox (gtk:vbox-new nil 0))
        (hbox (gtk:hbox-new nil %sw))
        data entry check button menu)
    data check button 
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-append-page notebook vbox label)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    ;; line 1 (score file)
    (setq label (gtk:label-new "Title:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 32) 
    (gtk:widget-show entry)
    (setq widgets (list* ':title entry widgets))
    (setq label (gtk:label-new "Event layering:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq menu (create-option-menu '("Add as new layer"
                                     "Mix into focus layer" 
                                     "Overwrite focus events"
                                     "Replace focus layer")))
    (gtk:box-pack-start hbox menu nil nil hpad)
    (setq widgets (list* ':event-layering menu widgets))
    (gtk:widget-show menu)
    (let ((nb (gtk:notebook-new)))
      (gtk:container-add vbox nb)
      (gtk:widget-show nb)
      ;(gtk:container-set-border-width nb 5)
      (gtk:notebook-set-tab-pos notebook gtk:pos-top)
      (setq data (create-axis-page window nb ':y-axis
                                   :omit '(:seconds :milliseconds)
                                   :selected NIL))
      (setq widgets (list* :y-axis  data widgets))
      (setq data (create-axis-page window nb ':x-axis
                                   :slot1 'time
                                   :only '(:seconds ) ;:milliseconds
                                   :selected ':seconds))
      (setq widgets (list* :x-axis  data widgets)))
    ;;(setf (widget-property window ':plotter) widgets)
    (cmio-set-page-widgets cmio ':plotter widgets)
    ;;(pprint widgets)
    (values)))

(defmethod cmio-page-data (cmio (page (eql :plotter)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page))
        group axis)
    ;; widgets is:
    ;; (:title <e> :event-layering <m>
    ;;  :x-axis 
    ;;  (:prototypes (<m> . data)
    ;;   :slot1 (<e>) :slot2 (<c> <l> <e>)
    ;;   :protoinits (<e> <e> <e> <s>))
    ;;  :y-axis (...))
    (add-entry-data data :title (getf widgets ':title) 
                    :nullok nil :read nil)
    (add-menu-data data :event-layering (getf widgets ':event-layering)
                   '(:add :mix :overwrite :replace))
    ;; process the axis frame data. most of this hair is due to the
    ;; fact that axis data are returned as sublists but any errors
    ;; must be "propagated" into the top-level data list so that
    ;; report-errors can find them.
    (dolist (ax '(:x-axis :y-axis))
      (setq group (getf widgets ax))
      (error-block
        (let* ((protos (getf group ':prototypes))
               (temp (add-menu-data nil ax (first protos)
                                    (loop for x in (cdr protos)
                                       collect (second x))))
               (slot1 (getf group ':slot1))
               (slot2 (getf group ':slot2))
               slots)
          (setq axis (LIST (second temp)))
          (setq temp (add-entry-data nil :slot1 (first slot1)
                                     :nullok nil :test #'symbolp))
          (when (and (consp (second temp))
                     (eql (first (second temp)) ':error))
            ;; Set the entire axis data to the error so that
            ;; abort-errors will find it.
            (setq axis (second temp))
            (error-abort))
          (setq slots (list (second temp)))
          (when (gtk:toggle-button-get-active (first slot2))
            (setq temp (add-entry-data nil :slot2 (third slot2)
                                       :test #'symbolp))
            (when (and (consp (second temp))
                       (eql (first (second temp)) ':error))
              (setq axis (second temp))
              (error-abort))
            (nconc slots (list (second temp)))
            )
          (nconc axis (list :slots slots))
          (let ((inits (getf group ':protoinits)))
            (setq temp (list :inits))
            (add-entry-data temp :minimum (first inits) 
                            :test #'numberp)
            (add-entry-data temp :maximum (second inits) 
                            :test #'numberp)
            (add-entry-data temp  :increment (third inits) 
                            :test #'numberp)
            (add-spin-data temp :ticks-per-increment (fourth inits)
                           :result #'floor)
            (loop for x in (cddr temp) by #'cddr
                 do (if (and (consp x) (eql (car x) ':error))
                      (progn
                        (setq axis x)
                        (error-abort))))
            (nconc axis (cdr temp))
            )
          )
        ) ;; end block   
      (nconc data (list ax axis))
      )
    data))

(defmethod cmio-set-page-data (cmio (page (eql :plotter)) args)
  (let* ((widgets (cmio-page-widgets cmio page ))
         (x-axis (getf widgets ':x-axis))
         (xinits (cadr (member ':x-axis args)))
         (y-axis (getf widgets ':y-axis))
         (yinits (cadr (member ':y-axis args))))
    ;; widgets is:
    ;; (:title <e> :event-layering <c>
    ;;  :x-axis
    ;;  (:prototypes (<m> . data)
    ;;   :slot1 (<e>) :slot2 (<c> <l> <e>)
    ;;   :protoinits (<e> <e> <e> <s>)))
    ;;  :y-axis (...))
    (set-page-fields args :title (getf widgets ':title)
                     :event-layering (list :menu
                                           (getf widgets ':event-layering)
                                           :add :mix :overwrite :replace))
    (flet ((setone (plist args)
             (let* ((protos (getf plist ':prototypes))
                    (names (loop for x in (cdr protos)
                              collect (second x)))
                    (types (loop for x in (cdr protos)
                              collect (first (third x))))
                    (buffs (getf plist ':protoinits))
                    slots which)
               (if (consp args)
                 (setq which (if (oddp (length args))
                               (position (pop args) names)
                               nil))
                 (setq which (position args names) args nil))
               (when which
                 (prototype-fill-buffers (nth which types) buffs))
               (setq slots (getf args ':slot))
               (cond ((not slots) )
                     ((and (consp slots) (= (length slots) 2))
                      (gtk:entry-set-text (first (getf plist ':slot1))
                                          (format nil "~(~A~)" (car slots)))
                      (gtk:toggle-button-set-active
                       (first (getf plist ':slot2)) t)
                      (gtk:entry-set-text (third (getf plist ':slot2))
                                          (format nil "~(~A~)" (cadr slots))))
                     ((and (consp slots) (= (length slots) 1))
                      (gtk:entry-set-text (first (getf plist ':slot1))
                                          (format nil "~(~A~)" (car slots))))
                     ((symbolp slots)
                      (gtk:entry-set-text (first (getf plist ':slot1))
                                          (format nil "~(~A~)" slots)))
                     (t nil))
               (when args
                 (set-page-fields args :minimum (first buffs)
                                  :maximum (second buffs)
                                  :increment (third buffs)
                                  :ticks-per-increment 
                                  (cons ':spin (fourth buffs)))))))
      (when xinits (setone x-axis xinits))
      (when yinits (setone y-axis yinits))
      )))

(defmethod cmio-ensure-write-stream (cmio (target (eql :plotter)) data)
  (let* ((name (get-data data ':title))
         (stream (or (find-object name nil)
                     (plotter :title name :no-window t))))
    cmio
    (setf (plotter-event-layering stream)
          (get-data data ':event-layering))
    (plotter-set-axis-values stream :x-axis (get-data data ':x-axis)
                             :y-axis (get-data data ':y-axis)
                             :redraw nil)
    (values stream nil nil)))

;;;
;;; :seq page
;;;

(defmethod cmio-create-page (cmio (target (eql :seq)) notebook)
  (let ((label (gtk:label-new "Seq"))
        (vbox (gtk:vbox-new nil 5))
        (hpad 5)
        hbox data entry check)
    (gtk:notebook-append-page notebook vbox label)
    (gtk:widget-show label)
    (gtk:widget-show vbox)
    ;; line 1, Name: [  ]
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Name:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list entry))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    ;; line 2, 
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    (setq check (gtk:check-button-new-with-label "Replace existing contents."))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)    
    (gtk:widget-show check)
    (cmio-set-page-widgets cmio ':seq (nreverse data))
    (values)))

(defmethod cmio-page-data (cmio (page (eql :seq)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page)))
    ;; widgets is: (<e> <c> )
    (add-entry-data data :name (first widgets) :nullok nil :read nil)
    (add-check-data data :replace (second widgets))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :seq)) args)
  (let ((widgets (cmio-page-widgets cmio page)))
    ;; widgets is: (<e> <c> )
    (set-page-fields args :name (FIRST widgets)
                     :replace (cons ':check (SECOND widgets)))))

(defmethod cmio-ensure-write-stream (cmio (target (eql :seq)) data)
  cmio
  (let* ((name (get-data data ':name))
         (stream (or (find-object name  nil)
                     (make-instance <seq> :name name))))
    (when (get-data data ':replace)
      (remove-subobjects stream))
    (values stream nil nil)))

;;;
;;; Systems page

(defun cmio-system-set-loaded (cmio system)
  ;; set the systems icon and sensitivity according to state
  (let ((widgets (cmio-page-widgets cmio ':systems system))
        (mode (target-system-icon system))
        (size gtk:icon-size-small-toolbar))
    (gtk:image-set-from-stock (first widgets) mode size)
    (gtk:widget-set-sensitive (first (last widgets)) 
                              (equal mode "gtk-no"))))

(gtk:define-signal-handler update_system_status :void (widget data)
  ;; widget is Update button, data is window
  widget
  (let ((cmio (widget->object data)))
    (cmio-system-set-loaded cmio :clm)
    (cmio-system-set-loaded cmio :cmn)
    (cmio-system-set-loaded cmio :midishare)
    (update-file-handlers cmio)))

(gtk:define-signal-handler cmio_system_load :void  (widget data)
  ;; widget is load button, data is window
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':systems))
         (systems '( :clm :cmn :midishare)) ; alsa
         wid sys loa err)
    ;; widgets is
    ;; (:alsa (<i> <l> <b>)
    ;;  :clm (<i> <l> <l> <e> <l> <e> <b>)
    ;;  :cmn (<i> <l> <l> <e> <l> <e> <b>)
    ;;  :midishare (<i> <l> <b>))

    ;; find the sys whose widgets contain our button. button is last
    ;; widget
    (loop with id = (ptr->int widget) for s in systems
       for l = (getf widgets s)
       if (equal id (ptr->int (first (last l))) )
       do (setq sys s wid l) (return))
    (ecase sys
      ((:clm :cmn)
       (let ((d1 (gtk:entry-get-text (fourth wid)))
             (d2 (gtk:entry-get-text (sixth wid))))
         (multiple-value-setq (loa err) (ensure-clm/cmn sys d1 d2))))
      (:alsa )
      (:midishare
       (multiple-value-setq (loa err) (ensure-midishare))))
    (if err
      (report-error err :window data)
      (progn
        (cmio-print cmio :message "Loading ~A..." sys)
        (load loa :verbose nil)
        (cmio-print cmio :message "Loading ~A...ok" sys)
        (cmio-system-set-loaded cmio sys)
        (if (eql sys ':clm) (update-file-handlers cmio ':audio))
        ))
    (values)))

(defun target-system-loaded? (sys)
  (case sys
    (:midishare (= (midishare) 1))
    (:clm (find ':clm *features*))
    (:cmn (find ':cmn *features*))
    (:alsa nil)
    (t t)))

(defun target-system-icon (sys)
  (case sys
    (:midishare
     #+(and darwin openmcl)
     (if (and (probe-file "ccl:darwin-headers;midishare;")
              (probe-file "/System/Library/Frameworks/Midishare.framework"))
       (if (target-system-loaded? sys) "gtk-yes" "gtk-no")
       "gtk-dialog-error")
     #+(and linux cmu)
     (if (probe-file "/usr/lib/libMidiShare.so")
       (if (target-system-loaded? sys) "gtk-yes" "gtk-no")
       "gtk-dialog-error")
     #-(or (and darwin openmcl)
           (and linux cmu))
     "gtk-dialog-error")
    (:alsa 
     #+linux (if (target-system-loaded? sys) "gtk-yes" "gtk-no")
     #-linux "gtk-dialog-error")
    (t (if (target-system-loaded? sys) "gtk-yes" "gtk-no"))))

(defmethod cmio-create-page (cmio (source (eql :systems)) notebook)
  (let ((label (gtk:label-new "Systems"))
        (window (cmio-window cmio))
        (table (gtk:table-new 4 5 nil))
        (tops (logior gtk:fill gtk:expand))
        (widgets (list))
        (data (list))
        (icon nil)
        (bsiz 24)
        (loa? "gtk-no")
        (size gtk:icon-size-small-toolbar)
        hbox button entry image )
    tops
    (gtk:notebook-append-page notebook table label)
    (gtk:notebook-set-tab-label-packing notebook table nil nil 1)
    (gtk:container-set-border-width table 5)
    (gtk:table-set-col-spacings table %sw)
    (gtk:widget-show label)
    (gtk:widget-show table)
;;     ;; ALSA
;;     (setq data (list))
;;     (setq icon (target-system-icon ':alsa))
;;     (setq image (gtk:image-new-from-stock icon size))
;;     (push image data)
;;     (gtk:table-attach table image 0 1 0 1  0 0 0 0 )
;;     (gtk:widget-show image)
;;     (setq label (gtk:label-new "ALSA"))
;;     (push label data)
;;     (gtk:table-attach table label 1 2 0 1  0 0 0 0)
;;     (gtk:widget-show label)
;;     (setq button (gtk:button-new-with-label "Load"))
;;     (push button data)
;;     (g:signal-connect button "clicked" (g:callback cmio_system_load)
;;                       window)
;;     (gtk:table-attach table button 3 4 0 1  0 0 0 0)
;;     (gtk:widget-set-sensitive button (equal icon loa?))
;;     (gtk:widget-show button)
;;     (setq widgets (list* :alsa (nreverse data) widgets))
    ;; 2 CLM
    (setq data (list))
    (setq icon (target-system-icon ':clm))
    (setq image (gtk:image-new-from-stock icon size))
    (push image data)
    (gtk:table-attach table image 0 1 1 2  0 0 0 0 )
    (gtk:widget-show image)
    (setq label (gtk:label-new "CLM"))
    (push label data)
    (gtk:table-attach table label 1 2 1 2  0 0 0 0)
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:table-attach table hbox 2 3 1 2  0 0 0 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Directory:"))
    (cmio-show-widget-required cmio label)
    (push label data)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (push entry data)
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Binary dir:"))
    (push label data)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (push entry data)
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq button (gtk:button-new-with-label "Load"))
    (push button data)
    (g:signal-connect button "clicked" (g:callback cmio_system_load)
                      window)
    (gtk:table-attach table button 3 4 1 2  0 0 0 0)
    (gtk:widget-set-sensitive button (equal icon loa?))
    (gtk:widget-show button)
    (setq widgets (list* :clm (nreverse data) widgets))
    ;; line 3 CMN
    (setq data (list))
    (setq icon (target-system-icon ':cmn))
    (setq image (gtk:image-new-from-stock icon size))
    (push image data)
    (gtk:table-attach table image 0 1 2 3  0 0 0 0)
    (gtk:widget-show image)
    (setq label (gtk:label-new "CMN"))
    (push label data)
    (gtk:table-attach table label 1 2 2 3  0 0 0 0)
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:table-attach table hbox 2 3 2 3   0 0 0 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Directory:"))
    (cmio-show-widget-required cmio label)
    (push label data)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (push entry data)
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Binary dir:"))
    (push label data)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (push entry data)
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq button (gtk:button-new-with-label "Load"))
    (push button data)
    (g:signal-connect button "clicked" (g:callback cmio_system_load)
                      window)
    (gtk:table-attach table button 3 4 2 3  0 0 0 0)
    (gtk:widget-set-sensitive button (equal icon loa?))
    (gtk:widget-show button)
    (setq widgets (list* :cmn (nreverse data) widgets))
    ;; line 4 Midishare
    (setq data (list))
    (setq icon (target-system-icon ':midishare))
    (setq image (gtk:image-new-from-stock icon size))
    (push image data)
    (gtk:table-attach table image 0 1 3 4  0 0 0 0 )
    (gtk:widget-show image)
    (setq label (gtk:label-new "MidiShare"))
    (push label data)
    (gtk:table-attach table label 1 2 3 4  0 0 0 0)
    (gtk:widget-show label)
    (setq button (gtk:button-new-with-label "Load"))
    (push button data)
    (g:signal-connect button "clicked" (g:callback cmio_system_load)
                      window)
    (gtk:table-attach table button 3 4 3 4  0 0 0 0)
    (gtk:widget-set-sensitive button (equal icon loa?))
    (gtk:widget-show button)
    (setq widgets (list* :midishare (nreverse data) widgets))
    (setq button (gtk:button-new-with-label "Update"))
    (g:signal-connect button "clicked" (g:callback update_system_status)
                      window)
    (gtk:table-attach table button 1 4 4 5 0 0 0 0)
    (gtk:widget-show button)
    (cmio-set-page-widgets cmio ':systems widgets)
    ;;(pprint (cmio-page-widgets cmio ':systems))
    table))

(defmethod cmio-page-data (cmio (page (eql :systems)) &optional op)
  (let ((data (list page))
        (widgets (cmio-page-widgets cmio page))
        group)
    ;; (:alsa (<i> <l> <b>)
    ;;  :clm (<i> <l> <l> <e> <l> <e> <b>)
    ;;  :cmn (<i> <l> <l> <e> <l> <e> <b>)
    ;;  :midishare (<i> <l> <b>))
    (when (or (null op) (eql op ':clm))
      (setq group (getf widgets :clm))
      (add-entry-data data :clm-directory (fourth group)
                      :nullok nil :read nil)
      (add-entry-data data :clm-bin-directory (sixth group) :read nil))
    (when (or (null op) (eql op ':cmn))
      (setq group (getf widgets :cmn))
      (add-entry-data data :cmn-directory (fourth group)
                      :nullok nil :read nil)
      (add-entry-data data :cmn-bin-directory (sixth group) :read nil))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :systems)) args)
  (let ((widg1 (cmio-page-widgets cmio page :clm))
        (widg2 (cmio-page-widgets cmio page :cmn)))
    (set-page-fields args :clm-directory (fourth widg1)
                     :clm-bin-directory (sixth widg1)
                     :cmn-directory (fourth widg2)
                     :cmn-bin-directory (sixth widg2))))
           

(defun ensure-midishare ()
  (flet ((try (host dir)
           (if (equal (car (last dir)) "bin")
             (let ((path (make-pathname :host host
                                        :directory 
                                        (append (butlast dir)
                                                '("src" "midishare"))
                                        :name "Midishare-Interface"
                                        :type "lisp")))
               (if (probe-file path)
                 path
                 nil)))))
    (or (try (pathname-host cm-directory)
             (pathname-directory cm-directory))
        (try (pathname-host (cm-image-dir))
             (pathname-directory (cm-image-dir)))
        (values nil "Can't find Midishare-Interface.lisp"))))

(defun ensure-clm/cmn (sys sd bd)
  (if (equal sd "")
    (values nil "Missing required directory.")
    (if (setq sd (ensure-directory sd))
      (progn
        (if (equal bd "") 
          nil
          (setq bd (ensure-directory bd)))
        (if (or bd (equal bd ""))
          (let ((fil (if (eql sys ':clm)
                       (format nil "~aall.lisp" sd)
                       (format nil "~acmn-all.lisp" sd))))
            (when (equal bd "") (setq bd nil))
            (if (probe-file fil)
              (if (eql sys ':clm)
                (progn (defparameter clm-directory sd)
                       (if bd (defparameter clm-bin-directory bd))
                       fil)
                (progn (defparameter cmn-directory sd)
                       (if bd (defparameter cmn-bin-directory bd))
                       fil))
              (values nil (format nil "~A load file ~s does not exist."
                                  sys fil)
                      )))
          (values nil (format nil "~A: Binary directory does not exist."
                              sys))))
      (values nil (format nil "~A: source directory does not exist."
                          sys)))))

;;;
;;; :files page
;;;

(defparameter *cmio-file-types*
  '((:audio "snd" "aiff" "wav")
    (:midi "mid" "midi")
    (:sco "sco")
    (:ins "ins")
    (:clm "clm")
    (:eps "eps")
    (:html "html" "htm")
    (:lisp "lisp" "cm" "cl" "cmn" "scm")))

(defun default-file-handler (typ)
  ;; this should be merged with *cmio-file-types*
  (case typ
    (:audio (if (target-system-loaded? :clm)
              "(dac)"
              #+darwin (default-file-handler :midi)
              #+linux ""))
    (:midi #+darwin *osx-midi-file-player*
           #+linux *linux-midi-file-player*)
    (:sco "csound -d -m 0")
    (:ins "(cload)")
    (:clm "(clm-load)")
    (:eps #+darwin "open" #-darwin "gv")
    (:html #+darwin "open" #+linux "firefox")
    (:lisp "(load)")
    (t "")))
    
(defun update-file-handlers (cmio &optional typ)
  (let ((widgets (cmio-page-widgets cmio :files)))
    ;; (:audio <e> :midi <e> :sco <e> :eps <e> :html <e> :ins <e> :lisp <e>
    ;;  :wait <c> :output <c> :file-types (...))
    (loop for e in *cmio-file-types*
         for b = (getf widgets (CAR e))
         if (or (not typ) (eql typ (car e)))
         do (gtk:entry-set-text b (default-file-handler (car e))))))

(defmethod cmio-create-page (cmio (source (eql :files)) notebook)
  (let ((label (gtk:label-new "Files"))
        (table (gtk:table-new 5 4 nil))
        (tops (logior gtk:fill gtk:expand))
        (widgets (list))
        (bsiz 20)
        hbox button entry )
    tops cmio label button 
    (gtk:notebook-append-page notebook table label)
    (gtk:notebook-set-tab-label-packing notebook table nil nil 1)
    (gtk:container-set-border-width table %bw)
    (gtk:table-set-col-spacings table %sw)
    (gtk:widget-show label)
    (gtk:widget-show table)
    ;; 0:0 Audio files
    (setq label (gtk:label-new "Audio player:"))
    (gtk:table-attach table label 0 1 0 1  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 1 2 0 1  0 0 0 0)
;    (gtk:entry-set-text entry (if (target-system-loaded? ':clm) "(dac)"
;                                  #+darwin "open" #-darwin ""))
    (gtk:widget-show entry)
    (setq widgets (list* :audio entry widgets))
    ;; 1:0 Midi files
    (setq label (gtk:label-new "MIDI player:"))
    (gtk:table-attach table label 0 1 1 2  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 1 2 1 2  0 0 0 0)
;    (gtk:entry-set-text entry #+darwin "open"
;                        #-darwin "timidity -quiet=2")
    (gtk:widget-show entry)
    (setq widgets (list* :midi entry widgets))
    ;; 2:0 Ins files
    (setq label (gtk:label-new "Sco player:"))
    (gtk:table-attach table label 0 1 2 3  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 1 2 2 3  0 0 0 0)
;    (gtk:entry-set-text entry "csound -d -m 0")
    (gtk:widget-show entry)
    (setq widgets (list* :sco entry widgets))
    ;; 3:0 EPS files
    (setq label (gtk:label-new "EPS viewer:"))
    (gtk:table-attach table label 0 1 3 4  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 1 2 3 4  0 0 0 0)
;    (gtk:entry-set-text entry #+darwin "open" #-darwin "gs")
    (gtk:widget-show entry)
    (setq widgets (list* :eps entry widgets))
    ;; 4:0 HTML files
    (setq label (gtk:label-new "HTML viewer:"))
    (gtk:table-attach table label 0 1 4 5  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 1 2 4 5  0 0 0 0)
;    (gtk:entry-set-text entry #+darwin "open" #-darwin "mozilla")
    (gtk:widget-show entry)
    (setq widgets (list* :html entry widgets))
    ;; 0:1 Ins files
    (setq label (gtk:label-new "Ins loader:"))
    (gtk:table-attach table label 2 3 0 1  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 3 4 0 1  0 0 0 0)
;    (gtk:entry-set-text entry "(cload)")
    (gtk:widget-show entry)
    (setq widgets (list* :ins entry widgets))
    ;; CLM files 
    (setq label (gtk:label-new "Clm loader:"))
    (gtk:table-attach table label 2 3 1 2  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 3 4 1 2  0 0 0 0)
;    (gtk:entry-set-text entry "(clm-load)")
    (gtk:widget-show entry)
    (setq widgets (list* :clm entry widgets))
    ;; Lisp files
    (setq label (gtk:label-new "Lisp loader:"))
    (gtk:table-attach table label 2 3 2 3  0 0 0 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry bsiz)
    (gtk:table-attach table entry 3 4 2 3  0 0 0 0)
;    (gtk:entry-set-text entry "(load)")
    (gtk:widget-show entry)
    (setq widgets (list* :lisp entry widgets))

    ;; process args
    (setq label (gtk:label-new "Processes:"))
    (gtk:table-attach table label 2 3 4 5  0 0 0 0)
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 3 4 4 5  0 0 0 0)
    (gtk:widget-show hbox)
    (setq button (gtk:check-button-new-with-label "Wait"))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (setq widgets (list* :wait button widgets))
    (setq button (gtk:check-button-new-with-label "Output"))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (setq widgets (list* :output button widgets))
    (cmio-set-page-widgets cmio :files widgets)
    ;; cache file type association that user can customize.
    (nconc widgets (list ':file-types *cmio-file-types*))
    ;;(pprint (cmio-page-widgets cmio ':files))
    (update-file-handlers cmio)
    (values)))

(defmethod cmio-page-data (cmio (page (eql :files)) &optional op)
  (let ((data (list page))
        (widgets (cmio-page-widgets cmio page))
        (ops (if (null op) '(:audio :midi :sco :eps :html :ins :clm :lisp)
                 (list op))))
    ;; (:audio <e> :midi <e> :sco <e> :eps <e> :html <e> :ins <e> 
    ;;  :clm <e> :lisp <e> :wait <c> :output <c> :file-types (...))
    (dolist (o ops)
      (when (or (null op) (eql op o))
        (add-entry-data data o (getf widgets o)
                        :read #'parse-function-or-command)))
    ;; add wait and output data to exteral processes
    (when (or (null op)
              (not (member op '(:clm :ins :clm :cm :lisp))))
      (add-check-data data :wait (getf widgets ':wait))
      (add-check-data data :output (getf widgets ':output)))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :files)) args)
  (let ((widgets (cmio-page-widgets cmio page)))
    ;; (:audio <e> :midi <e> :sco <e> :eps <e> :html <e> :ins <e> 
    ;;  :clm <e> :lisp <e> :wait <c> :output <c> :file-types (...))
    ;(print widgets)
    (set-page-fields args :audio (getf widgets ':audio)
                     :midi (getf widgets ':midi)
                     :sco (getf widgets ':sco)
                     :eps (getf widgets ':eps)
                     :html (getf widgets ':html)
                     :ins (getf widgets ':ins)
                     :clm (getf widgets ':clm)
                     :lisp (getf widgets ':lisp)
                     :wait (cons ':check (getf widgets ':wait))
                     :output (cons ':check (getf widgets ':output))
                     )))
; (cmio :files '(:audio "asd" :midi "asd a" :sco "vvcv" :eps "casd" :html "zxc" :ins (lopo) :clm "fof" :lisp (fasd)))

(defmethod cmio-ensure-execute-command (cmio (type (eql :lisp)) filename 
                                        stream typedata)
  cmio stream
  (let ((expr (get-data typedata ':lisp)))
    (if (consp expr)
      (progn (push filename (cdr expr))
             (values expr nil))
      (values (concatenate 'string expr " " filename) nil))))

(defun cmio-file-execution-type (cmio file)
  ;; see if file's type is in the assoc lists of file types.
  (let ((type (pathname-type file)))
    (if type
      (let ((test (getf (cmio-page-widgets cmio ':files) ':file-types)))

        (car (find type test :test (lambda (x y) 
                                     (member x (cdr y) :test #'string=)))))
      nil)))

(defun parse-function-or-command (str)
  (if (and (eql (elt str 0) #\()
           (eql (string-readable? str) 1))
    (let ((l (string-read str)))
      (if (consp l)
        (if (and (car l) (symbolp (car l)))
          l
          (values str " Not a function call list."))
        (values str +se-not-cons+)))
    str))

;;;
;;; :Exec "page" (not part of any notebook)

(defun cmio-set-execute (cmio filename stream pwd)
  (let* ((widgets (cmio-page-widgets cmio ':exec))
         (fullname (namestring (merge-pathnames filename pwd)))
         (outputs (FOURTH widgets))
         (entry (find-execute-file cmio fullname)))
    ;; add new file to :exec's output stack
    ;; outputs is (<menu> . files)
    ;;(print (list :find-> fullname :stack-> (cdr outputs)))
    (IF entry
      NIL
      (if stream
        (push (list fullname stream) (cdr outputs))
        (push fullname (cdr outputs))))
    (gtk:entry-set-text (second widgets)
                        (namestring (enough-namestring filename
                                                       pwd))
                        )))

(defun find-execute-file (cmio file)
  ;; return an existing entry for file if it exists.
  (let ((stack (FOURTH (cmio-page-widgets cmio ':exec))))
    (loop for e in (CDR stack)
       when (string= (if (consp e) (car e) e) file)
       return e)))

(gtk:define-signal-handler cmio_execute :void (widget data)
  ;; widget is execute button, data is window
  widget
  (let ((cmio (widget->object data)))
    (unless (cmio-command-active? cmio) ; not yet implemented!
      (cmio-execute cmio))))

(gtk:define-signal-handler exec_cd :void (widget data)
  ;; widget is entry box, data is window
  widget data
  (let ((cmio (widget->object data))
        (str (gtk:entry-get-text widget)))
    (if (string= str "")
      (progn (cd) 
             (gtk:entry-set-text widget (pwd))
             (cmio-print cmio :message "Directory: cd ~S ok." (pwd))
             )
      (let ((dir (ensure-directory str)))
        (if (not dir)
          (progn
            ;(gtk:entry-set-text widget (pwd))
            (report-error (format nil "Directory: ~S does not exist."
                                  str)
                          :entry widget))
          (progn (cd dir)
                 (gtk:entry-set-text widget (pwd))
                 (cmio-print cmio :message "Directory: cd ~S ok."
                             (pwd))))))
    (values)))

(defmethod cmio-create-page (cmio (source (eql :exec)) box)
  ;; box is window's vbox
  (let ((window (cmio-window cmio))
        (esiz 30)
        (frame (gtk:frame-new "Executive"))
        hbox label entry button exec)
    ;; create typeout buffer as last line in window
    window
    (gtk:box-pack-start box frame nil nil 0)
    (gtk:widget-show frame)
    (setq hbox (gtk:hbox-new nil 0))
    (gtk:container-set-border-width hbox %bw)
    (gtk:container-add frame hbox)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Directory:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq exec (list entry))
    (gtk:entry-set-text entry (pwd))
    (gtk:entry-set-width-chars entry esiz)
    (gtk:box-pack-start hbox entry nil nil 5)
    (g:signal-connect entry "activate" (g:callback exec_cd)
                      window)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "File:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (nconc exec (list entry))
    (gtk:entry-set-width-chars entry esiz)
    (gtk:box-pack-start hbox entry nil nil 5)
    (gtk:widget-show entry)
    (setq button (gtk:button-new-from-stock "gtk-execute"))
    (nconc exec (list button))
    (g:signal-connect button "clicked" (g:callback cmio_execute)
                      window)
    (gtk:box-pack-end hbox button nil nil 0)
    (gtk:widget-show button)
    ;; mockup for menu and output file stack
    (nconc exec (list (list nil)))
    (cmio-set-page-widgets cmio ':exec exec)
    ;;(pprint (cmio-page-widgets cmio ':exec))
    (values)))

(defmethod cmio-page-data (cmio (page (eql :exec)) &optional op)
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page)))
    ;; widgets is (<e> <e> <b> (<m> . files))
    (when (or (null op) (eql op ':write))
      (add-entry-data data :directory (first widgets) :nullok nil
                      :read
                      (lambda (str)
                        (let ((dir (ensure-directory str)))
                          (cond ((not dir)
                                 (values str (format nil
                                                     " ~S does not exist."
                                                     str)))
                                ((not (string= dir (pwd)))
                                 (values str 
                                         (format nil
                                                 " cd ~S unconfirmed by Enter."
                                                 str)))
                                (t dir))))))
    (when (or (null op) (eql op ':execute))
      (add-entry-data data :file (second widgets) :read nil
                      :nullok nil))
    data))

(defun cmio-create-output-line (cmio box)
  (let ((hbox (gtk:hbox-new nil 5))
        image label)
    (gtk:box-pack-end box hbox t t 0)
    (gtk:widget-show hbox)
    (setq image (gtk:image-new-from-stock "gtk-yes"
                                          gtk:icon-size-small-toolbar))
    (setf (FIRST (cmio-output cmio)) image)
    (gtk:box-pack-start hbox image nil nil 0)
    (setq label (gtk:label-new ""))
    (gtk:misc-set-alignment label 0 .5)
    (gtk:box-pack-start hbox label t t 0)
    (gtk:widget-show label)
    (setf (SECOND (cmio-output cmio)) label)
    (values)))

;;;
;;; Targets and Sources notebooks
;;;

(defun cmio-create-sources-notebook (cmio box)
  (let ((page (position ':eventio *cmio-source-pages*))
        frame notebook)
    (setq frame (gtk:frame-new "Sources"))
    (gtk:box-pack-start box frame nil nil 0)
    (gtk:widget-show frame)
    (setq notebook (gtk:notebook-new))
    (gtk:container-set-border-width notebook %bw)
    (setf (FIRST (cmio-notebooks cmio)) notebook)
    (gtk:container-add frame notebook)
    (gtk:widget-show notebook)
    (gtk:notebook-set-tab-pos notebook gtk:pos-top)
    (dolist (s *cmio-source-pages*)
      (cmio-create-page cmio s notebook ))
    (gtk:notebook-set-current-page notebook page)
    (values)))

(defun cmio-create-targets-notebook (cmio box)
  (let (frame notebook)
    (setq frame (gtk:frame-new "Targets"))
    (gtk:box-pack-start box frame nil nil 0)
    (gtk:widget-show frame)
    (setq notebook (gtk:notebook-new))
    (gtk:container-set-border-width notebook %bw)
    (setf (SECOND (cmio-notebooks cmio)) notebook)
    (gtk:container-add frame notebook)
    (gtk:widget-show notebook)
    (gtk:notebook-set-tab-pos notebook gtk:pos-top)
    (dolist (s *cmio-target-pages*) 
      (cmio-create-page cmio s notebook ))
    ;;(g:signal-connect notebook "switch-page" (g:callback set_target_page)
    ;;                  window)
    (values)))

;;;
;;; main function

(gtk:define-signal-handler cmio_quit :void (widget data)
  data
  (let ((cmio (widget->object widget)))
    (gtk-remove-toplevel cmio)
    (unless (gtk-open-toplevels?)
      (gtk-main-stop))
    (values)))

(defparameter *cmio-title* "Common Music IO")

(defun cmio-open (cmio args)
  (gtk:init-ensure)
  (let* ((window (gtk:window-new gtk:window-toplevel))
         (vbox (gtk:vbox-new nil 5)))
    (setf (cmio-window cmio) window)
    (setf (widget->object window) cmio)
    (gtk:window-set-title window *cmio-title*)
    (gtk:container-set-border-width window 10)
    (gtk:container-add window vbox)
    (gtk:widget-show vbox)
    (cmio-allocate-colors cmio)
    (cmio-create-sources-notebook cmio vbox)
    (cmio-create-targets-notebook cmio vbox)
    ;; the exec page is in the main window
    (cmio-create-page cmio ':exec vbox)
    (cmio-create-output-line cmio vbox)
    ;; process user inits
    (dopairs (p v args)
      (when (consp v) (cmio-set-page-data cmio p v)))
    (let ((page (or (position (getf args ':target ':midi)
                              *cmio-target-pages*)
                    (position ':midi *cmio-target-pages*))))
      (gtk:notebook-set-current-page (SECOND (cmio-notebooks cmio)) page))
    ;; show main window
    (print-available-targets cmio)
    (gtk:widget-show window)
    (g:signal-connect window "destroy"
                      (g:callback cmio_quit)
                      (g:nullptr))
    (push cmio *gtk-open-toplevels*)
    ;(unless *gtk-main* (setq *gtk-main* t) (gtk:main))
    (gtk-main-start )
    cmio))

(defun print-available-targets (cmio)
  (let ((str (concatenate 'string
                          "Active targets:"
                          (if (target-system-loaded? :clm) " CLM," "")
                          (if (target-system-loaded? :cmn) " CMN,"  "")
                          " Csound, Midi,"
                          (if (target-system-loaded? :midishare)
                            " Midishare," "")
                          " Plotter, Seq.")))
    (cmio-print cmio :message str)))

(defun cmio-allocate-colors (cmio)
  ;; (GTK:WIDGET-MODIFY-TEXT)
  ;; (gtk:widget-modify-text widget gtk:state-normal color)
  (setf (cmio-colors cmio)
        (let ((map (gdk:colormap-get-system)))
          (loop for c in '((:red  "#CD2626")
                           (:green "#006400")
                           (:blue "blue")
                           (:yellow "#FF6103")
                           (:pale-yellow "#FFF8DC" ;"#EEE8AA" ;"#FFF68F" ;"#FAFAE8"
                            ))
             for s = (gtk:struct-alloc :<G>dk<C>olor)
             do (gdk:color-parse (second c) s)
             (gdk:colormap-alloc-color map s t t)
             collect (first c) collect s))))

(defun cmio (&rest args)
  #+darwin
  (unless (darwin-x11-running?)
    (return-from cmio nil))
  (let ((cmio (make-instance 'cmio)))
    (if *gtk-main*
      #+openmcl
      (gtk-call #'(lambda () (cmio-open cmio args)))
      #-openmcl
      (cmio-open cmio args)
      (cmio-open cmio args))
    cmio))


#|
(cmio :cmn '(:output-file "foo.eps" :versions t :view t :all-output-in-one-file t :title "Bif" :size 80 :metronome 120 :exact t :staffing ((0 :name a :meter (3 4) :clef :both) (1 :name b :meter (4 4) :clef :alto) (2 :name c :meter "9/8" :clef :treble))))
(cmio :clm '(:score-file "foo.clm" :versions t :sound-file "bif.snd" :play t :srate 10000 :channels 3 :scaled-to 1 :scaled-by 1 :clipped t :statistics t :verbose t :comment "hiho!" :reverb rev :decay-time 2 :reverb-data (a b c) ))
(cmio-set-page-data foo :csound '(:score-file "fig" :versions t :Header "zzz" :orchestra "xcvxcvxvvvxcxvcv" :sound "pif/ziggy"))
(cmio :eventio '(:events (Bar) :starts 1))
(cmio :files '(:audio "asd" :midi "asd a" :sco "vvcv" :eps "casd" :html "zxc" :ins (lopo) :clm "fof" :lisp (fasd)))
(cmio :midi '(:midi-file "zuz.mid" :microtuning :divisions :exclude-tracks (0 1 2) :meta-exclude t :keynum-format :hertz :override-tempo 99 :time-format :ticks))
(cmio :midishare '(:connection :midi-player
                   :name "buf.mp" :track 3 :play nil))
(cmio :plotter '(:title "asd" :event-layering :overwrite
                 :x-axis (:minimum 30 :maximum 90
                          :slot fred :ticks-per-increment 3
                          :increment 10)
                 :y-axis (:keynum :minimum 60 :maximum 200
                          :slot (buf wuzzy))
                 ))
(cmio :seq '(:name zuz :replace t))
(cmio :systems '(:clm-directory "/Lisp/clm-mk" :clm-bin-directory "/Lisp/bin/clm-mk/openmcl" :cmn-directory "/Lisp/cmn" :cmn-bin-directory "/Lisp/bin/cmn/openmcl"))

|#

