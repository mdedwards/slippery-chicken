;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             osc-sc-bsd.lisp
;;;
;;; Class Hierarchy:  None, no classes defined.
;;;
;;; Version:          1.0.10
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of interface to OSC for networked connection
;;;                   between slippery chicken and MaxMSP etc. Requires
;;;                   osc.lsp and only works in SBCL. This is the sb-bsd-sockets
;;;                   package part that osc-sc.lsp needs. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    13th December 2012, Bangkok
;;;
;;; $$ Last modified:  16:19:49 Sat Aug 22 2020 CEST
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

(require :sb-posix)
(require :sb-bsd-sockets)
(in-package :sb-bsd-sockets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Output stream as global so we can send OSC messages from other
;;; methods/functions once we're in the osc-call loop. 
(defparameter +osc-sc-output-stream+ nil)
(defparameter +osc-sc-in-socket+ nil)
(defparameter +osc-sc-out-socket+ nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun osc-cleanup-sockets ()
  (when +osc-sc-in-socket+ (socket-close +osc-sc-in-socket+))
  (when +osc-sc-out-socket+ (socket-close +osc-sc-out-socket+))
  (setf +osc-sc-output-stream+ nil
        +osc-sc-in-socket+ nil
        +osc-sc-out-socket+ nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main function we call to process lisp code and other messages sent via
;;; OSC. Listens on a given port and sends out on another. NB ip#s need to
;;; be in the format #(127 0 0 1) for now.

(defun osc-call (listen-port send-ip send-port print) 
  ;; (let ((buffer (make-sequence '(vector (unsigned-byte 8)) 512)))
  ;; MDE Mon Apr 11 11:15:47 2016 -- increased buffer size
  ;; MDE Tue Mar 31 15:07:24 2020 -- increased again
  (let ((buffer (make-sequence '(vector (unsigned-byte 8)) 16384))) ;4096)))
    ;; in case we exited abnormally last time
    (osc-cleanup-sockets)
    (setf +osc-sc-in-socket+ (make-udp-socket)
          +osc-sc-out-socket+ (make-udp-socket))
    (socket-bind +osc-sc-in-socket+ #(0 0 0 0) listen-port)
    (socket-connect +osc-sc-out-socket+ send-ip send-port)
    (setf +osc-sc-output-stream+
          (socket-make-stream
           +osc-sc-out-socket+ :input t :output t 
           :element-type '(unsigned-byte 8) :buffering :full))
    (unwind-protect 
         (format t "~&All good. Awaiting osc messages.~%")
      (loop with happy = t while happy do 
         ;; need this otherwise messages only get printed when we quit
           (finish-output t)
           (socket-receive +osc-sc-in-socket+ buffer nil)
           (let* ((oscuff (osc:decode-bundle buffer))
                  ;; here: check if there's an opening (
                  (soscuff 
                   (progn
                     (unless (third oscuff)
                       (error "osc-sc::osc-call: Couldn't decode buffer: ~a"
                              buffer))
                     (if (char= #\( (elt (third oscuff) 0))
                         'lisp
                         (read-from-string (third oscuff))))))
             (when print                ; MDE Mon May 26 10:39:48 2014
               (format t "~&osc-->message: ~a" oscuff))
             (finish-output t)
             (case (sc::rm-package soscuff :sb-bsd-sockets)
               ;; test
               (int (handle-number +osc-sc-output-stream+ (second oscuff)))
               (quit (setf happy nil))
               ;; save opening ( so evaluate lisp code
               (lisp (osc-eval +osc-sc-output-stream+ oscuff print))
               (t (warn "osc-sc::osc-call: Don't understand ~a. Ignoring."
                        soscuff)
                  (finish-output)))))))
  (osc-cleanup-sockets)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-udp-socket()
  (make-instance 'inet-socket :type :datagram :protocol :udp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; evaluate lisp code
(defun osc-eval (stream expr &optional print)
  ;; (format t "~&osc-eval: ~a ~a" expr (stringp (first expr)))
  ;; (finish-output t)
  (unless stream
    (error "sc-sc::osc-eval: stream not open."))
  (let* ((result (read-from-string (sc::list-to-string expr)))
         (id (second result)))
    ;; MDE Sat Jun 15 12:23:57 2013 -- make sure we've got legal input and an
    ;; ID to send back 
    (unless (string= (first result) "/OSC-SC")
      (error "osc-sc::osc-eval: expected /osc-sc as first token but got ~a"
             (first result)))
    (setf result (eval (third result)))
    ;; MDE Wed Dec 19 17:38:01 2012 -- don't send T or NIL, rather 1 or 0
    ;; MDE Thu Jan 23 11:58:07 2020 -- ^ meant our functions should return 1 or
    ;; 0 but let's actually allow T or NIL byt convert to 1 or 0
    (cond ((not result) (setq result '(0)))
          ((equal result T) (setq result '(1)))
          ((not (listp result)) (setq result (list result))))
    ;; MDE Sat Aug 22 16:18:26 2020, Heidhausen -- print results
    (when print (format t "~&osc-eval: ~a" result))
    ;; stuff our id back into the list and send back
    (osc-send-list (append (list '/osc-sc id) result) stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun osc-send-list (list stream &optional (warn t))
  (if stream
      (progn 
        (write-sequence (osc:encode-message (sc::list-to-string list))
                        stream)
        (finish-output stream))
      (when warn
        (warn "sc-sc::osc-send-list: stream not open."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; no longer used
(defun osc-apply (stream fun args)
  (format t " osc-apply: ~a ~a" fun args)
  (finish-output t)
  (let ((result (apply (symbol-function fun) args)))
    (write-sequence (osc:encode-message (format nil "~a" result)) stream)
    (finish-output stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test code
(defun handle-number (stream number) 
  (write-sequence 
   (osc:encode-message "/bzzp" "got" "it:" (1+ number))
   stream)
  (finish-output stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF osc-sc-bsd.lsp

