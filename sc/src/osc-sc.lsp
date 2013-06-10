;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             osc-sc.lisp
;;;
;;; Class Hierarchy:  None, no classes defined.
;;;
;;; Version:          1.0.2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of interface to OSC for networked connection
;;;                   between slippery chicken and MaxMSP etc.  Requires
;;;                   osc.lsp and only works in SBCL.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    13th December 2012, Bangkok
;;;
;;; $$ Last modified: 21:58:33 Tue Jun  4 2013 BST
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

(require :sb-bsd-sockets)
(use-package :osc)
(use-package :sb-bsd-sockets)
(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* osc-sc/osc-call
;;; DESCRIPTION
;;; Allow OSC (over UDP) messages to be sent for processing.  The function
;;; waits for input and processes in an endless loop; send 'quit' to stop the
;;; function and return to the interpreter.  Messages the function doesn't
;;; understand will be ignored after a warning being printed.
;;;
;;; As this function only terminates when a 'quit' message is sent via OSC, the
;;; only way to quit from within Lisp is to send the Interrupt Command (usually
;;; Control-C, twice).  In that case, the open sockets will remain open, and
;;; only closed before reopening the next time this function is called.
;;; 
;;; Lisp code can be sent, e.g. in MaxMSP via a message, including
;;; opening/closing parentheses and nested calls; symbols should be quoted as
;;; per usual.  The return value of the Lisp call will be returned to the
;;; given IP address on the given port.  It is up to the receiver to then parse
;;; the result (e.g. in MaxMSP via [udpreceive][fromsymbol][route list int
;;; float symbol]).  
;;;
;;; For an example max/msp patch, see osc-test.maxpat in the examples folder of
;;; the documentation (http://michael-edwards.org/sc/examples/osc-test.maxpat)
;;; 
;;; NB: Currently only works in SBCL.
;;;     Some lists (e.g. those including strings/symbols) might not be
;;;     recognised as lists by MaxMPS's [route], so process them directly after
;;;     [fromsymbol]. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :listen-port. The UDP port to listen to for messages. Default = 8000.
;;; - :send-ip.  The IP address to send UDP messages back out on. 
;;;    Default = #(127 0 0 1))
;;; - :send-port. The UDP port to send messages back out on. Default = 8001.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defun osc-call (&key
                 (listen-port 8000)
                 (send-ip #(127 0 0 1))
                 (send-port 8001))
;;; ****
  (sb-bsd-sockets::osc-call listen-port send-ip send-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* osc-sc/osc-send-list
;;; DESCRIPTION
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
(defun osc-send-list (list &optional (warn t))
;;; ****
  (sb-bsd-sockets::osc-send-list list sb-bsd-sockets::+osc-sc-output-stream+
                                 warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sb-bsd-sockets)

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
;;; OSC .  Listens on a given port and sends out on another.  NB ip#s need to
;;; be in the format #(127 0 0 1) for now.

(defun osc-call (listen-port send-ip send-port) 
  (let (;(in (make-udp-socket))
        ;(out (make-udp-socket))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 512)))
    ;; in case we exited abnormally last time
    (osc-cleanup-sockets)
    (setf +osc-sc-in-socket+ (make-udp-socket)
          +osc-sc-out-socket+ (make-udp-socket))
    (socket-bind +osc-sc-in-socket+ #(0 0 0 0) listen-port)
    (socket-connect +osc-sc-out-socket+ send-ip send-port)
    ;; (let ((stream 
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
                   (if (char= #\( (elt (first oscuff) 0))
                       'lisp
                       (read-from-string (first oscuff)))))
             (format t "~&osc-->message: ~a" oscuff)
             (finish-output t)
             (case (sc::rm-package soscuff :sb-bsd-sockets)
               ;; test
               (int (handle-number +osc-sc-output-stream+ (second oscuff)))
               (quit (setf happy nil))
               ;; save opening ( so evaluate lisp code
               (lisp (osc-eval +osc-sc-output-stream+ oscuff))
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
(defun osc-eval (stream expr)
  ;; (format t "~&osc-eval: ~a ~a" expr (stringp (first expr)))
  ;; (finish-output t)
  (unless stream
    (error "sc-sc::osc-eval: stream not open."))
  (let ((result (sc::list-to-string expr)))
    (setf result (eval (read-from-string result)))
    ;; MDE Wed Dec 19 17:38:01 2012 -- don't send T or NIL, rather 1 or 0
    (unless (or (not result) (equal result T))
      (unless (listp result)
        (setf result (list result)))
      (osc-send-list result stream))))

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
;;; EOF osc-sc.lsp

