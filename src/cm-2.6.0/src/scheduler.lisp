;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; generated by scheme->cltl from scheduler.scm on 23-Mar-2005 12:21:21

(in-package :cm)

(defmacro %qe-time (qe) `(car ,qe))

(defmacro %qe-time-set! (qe time) `(rplaca ,qe ,time))

(defmacro %qe-start (qe) `(cadr ,qe))

(defmacro %qe-start-set! (qe start) `(rplaca (cdr ,qe) ,start))

(defmacro %qe-object (qe) `(caddr ,qe))

(defmacro %qe-object-set! (qe obj) `(rplaca (cddr ,qe) ,obj))

(defmacro %qe-next (qe) `(cdddr ,qe))

(defmacro %qe-next-set! (qe nxt) `(rplacd (cddr ,qe) ,nxt))

(defmacro %q-head (q) `(cycl-tail ,q))

(defmacro %q-head-set! (q e) `(cycl-tail-set! ,q ,e))

(defmacro %q-last (q) `(cycl-last ,q))

(defmacro %q-last-set! (q e) `(cycl-last-set! ,q ,e))

(defmacro %q-peek (q) `(%q-head ,q))

(defmacro %q-pop (queue)
  (let ((q (gensym)) (e (gensym)))
    `(let* ((,q ,queue) (,e (%q-head ,q)))
       (if (null ,e)
           '()
           (progn (%q-head-set! ,q (%qe-next ,e))
                  (%qe-next-set! ,e '())
                  (if (null (%q-head ,q)) (%q-last-set! ,q '()))
                  ,e)))))

(defmacro %qe-alloc (queue time start object)
  (let ((q (gensym)) (e (gensym)))
    `(let* ((,q ,queue) (,e (cycl-data ,q)))
       (if (null ,e)
           (list ,time ,start ,object)
           (progn (cycl-data-set! ,q (%qe-next (cycl-data ,q)))
                  (%qe-next-set! ,e '())
                  (%qe-time-set! ,e ,time)
                  (%qe-start-set! ,e ,start)
                  (%qe-object-set! ,e ,object)
                  ,e)))))

(defmacro %qe-dealloc (queue entry)
  (let ((q (gensym)) (e (gensym)))
    `(let ((,q ,queue) (,e ,entry))
       (%qe-time-set! ,e nil)
       (%qe-start-set! ,e nil)
       (%qe-object-set! ,e nil)
       (%qe-next-set! ,e (cycl-data ,q))
       (cycl-data-set! ,q ,e)
       (values))))

(defparameter %q (make-cycl))

(dotimes (i 50) (%qe-dealloc %q (list nil nil nil)))

(defmacro %q-insert (entry queue)
  (let ((q (gensym)) (e (gensym)) (h (gensym)) (l (gensym)))
    `(let ((,q ,queue) (,e ,entry))
       (if (null (%q-head ,q))
           (progn (%q-head-set! ,q ,e) (%q-last-set! ,q ,e))
           (if (< (%qe-time ,e) (%qe-time (%q-head ,q)))
               (progn (%qe-next-set! ,e (%q-head ,q))
                      (%q-head-set! ,q ,e))
               (if (< (%qe-time ,e) (%qe-time (%q-last ,q)))
                   (do ((,h (%q-head ,q)) (,l '()))
                       ((or (null ,h)
                            (> (%qe-time ,h) (%qe-time ,e)))
                        (%qe-next-set! ,e (%qe-next ,l))
                        (%qe-next-set! ,l ,e))
                     (setf ,l ,h)
                     (setf ,h (%qe-next ,h)))
                   (progn (%qe-next-set! (%q-last ,q) ,e)
                          (%q-last-set! ,q ,e))))))))

(defun pq (&rest args)
  (let* ((q (if (null args) %q (car args)))
         (h (%q-head q))
         (z most-negative-fixnum))
    (format t "~s entries:~%" (/ (length (%q-head q)) 3))
    (loop for i from 0
          until (null h)
          do (if (> z (car h))
                 (error "Out of order: ~s ~s ~s"
                        z
                        (car h)
                        (%q-head q)))
             (setf z (car h))
             (format t "~s. (~s ~s ~s)" i (car h) (cadr h) (caddr h))
             (terpri)
             (setf h (%qe-next h)))))

(defun %q-flush (q)
  (loop for e = (%q-pop q) until (null e) do (%qe-dealloc q e)))

(defparameter *queue* ())

(defun schedule-events (function object &rest args)
  (let* ((ahead (if (consp args) (car args) 0))
         (noerr nil)
         (entry nil)
         (qtime nil)
         (start nil)
         (thing nil))
    (setf *queue* %q)
    (if (consp object)
        (dolist (o object)
          (schedule-object o
           (if (consp ahead)
               (if (consp (cdr ahead)) (pop ahead) (car ahead))
               ahead)))
        (if (consp ahead)
            (schedule-object object (car ahead))
            (schedule-object object ahead)))
    (unwind-protect
        (progn (do ()
                   ((null (%q-head *queue*)) (setf noerr t))
                 (setf entry (%q-pop *queue*))
                 (setf qtime (%qe-time entry))
                 (setf start (%qe-start entry))
                 (setf thing (%qe-object entry))
                 (%qe-dealloc *queue* entry)
                 (process-events thing qtime start function)))
      (unless noerr (%q-flush *queue*) (unschedule-object object t))
      (setf *queue* nil))))

(defun enqueue (object time start)
  (%q-insert (%qe-alloc *queue* time start object) *queue*))

(defun early? (tim)
  (if (null (%q-head *queue*))
      nil
      (> tim (%qe-time (%q-head *queue*)))))

(defmethod schedule-object ((obj standard-object) start)
  (enqueue obj (+ start (object-time obj)) nil))

(defmethod schedule-object ((obj function) start)
  (enqueue obj start start))

(defmethod schedule-object ((obj cons) start)
  (dolist (o obj) (enqueue o start start)))

(defmethod schedule-object ((obj seq) start)
  (let ((mystart (+ start (object-time obj))))
    (enqueue (subobjects obj) mystart mystart)
    (dolist (sub (subcontainers obj)) (schedule-object sub mystart))))

(defmethod unschedule-object (obj &rest recurse) obj recurse nil)

(defmethod process-events (obj time start func)
  start
  (funcall func obj time))

(defmethod process-events ((head cons) time start func)
  time
  (let ((event nil) (next nil))
    (do ()
        ((or event (null head)) nil)
      (setf next (pop head))
      (unless (typep next <container>) (setf event next)))
    (if event
        (progn (setf next (+ start (object-time event)))
               (if (early? next)
                   (enqueue event next start)
                   (funcall func event next))
               (if (null head) nil (enqueue head next start)))
        nil)))

(defparameter *qstart* ())

(defparameter *qtime* ())

(defparameter *qnext* ())

(defparameter *process* ())

(defparameter *handler* ())

(defmethod process-events ((func function) qtime qstart handler)
  (setf *process* func)
  (setf *qtime* qtime)
  (setf *qstart* qstart)
  (setf *handler* handler)
  (setf *qnext* *qtime*)
  (if (funcall *process*) (enqueue *process* *qnext* *qstart*)))

(defun output (event &optional out)
  (if *queue*
      (let ((sav *out*))
        (if out (setf *out* out))
        (if (consp event)
            (dolist (e event)
              (let ((n (+ *qstart* (object-time e))))
                (if (early? n)
                    (enqueue e n nil)
                    (funcall *handler* e n))))
            (let ((n (+ *qstart* (object-time event))))
              (if (early? n)
                  (enqueue event n nil)
                  (funcall *handler* event n))))
        (setf *out* sav)
        (values))
      (rt-output event out)))

(defun now (&optional abs-time)
  (if *queue*
      (if (not abs-time) (- *qtime* *qstart*) *qtime*)
      (rt-now)))

(defmacro stop ()
  (if *queue*
      (process-stop nil)
      (error "Calling 'stop' outside of scheduler?")))

(defun wait (delta) (setf *qnext* (+ *qnext* (abs delta))))

(defun wait-until (time) (setf *qnext* (+ *qstart* time)))

(defmethod sprout ((obj standard-object) &optional time)
  time
  (if *queue*
      (schedule-object obj *qstart*)
      (error "Calling 'sprout' outside of scheduler?")))

(defmethod sprout ((obj function) &optional time)
  (if *queue*
      (enqueue obj (+ *qstart* time) (+ *qstart* time))
      (rt-sprout obj time)))

(defmethod sprout ((obj cons) &optional time)
  time
  (if *queue*
      (dolist (o obj) (sprout o time))
      (error "Calling 'sprout' outside of scheduler?")))
