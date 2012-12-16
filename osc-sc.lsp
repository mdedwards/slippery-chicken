;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             osc-sc.lisp
;;;
;;; Class Hierarchy:  None, no classes defined.
;;;
;;; Version:          1.0.1
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
;;; $$ Last modified: 15:56:51 Fri Dec 14 2012 ICT
;;;
;;; SVN ID: $Id: sclist.lsp 963 2010-04-08 20:58:32Z medward2 $
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
;;; Allow OSC (over UDP) messages to be sent for processing.  Lisp code can be
;;; sent (e.g. in MaxMSP via a message, including opening/closing parentheses
;;; and nested calls; symbols should be quoted as per usual).  The return value
;;; of the Lisp call will be returned to the given IP address on the given
;;; port.  It is up to the receiver to then parse the result (e.g. in MaxMSP
;;; via [udpreceive][fromsymbol][route list int float symbol]).  The function
;;; waits for input and processes in an endless loop; send 'quit' to stop the
;;; function and return to the interpreter.  Messages the function doesn't
;;; understand will be ignored.
;;; 
;;; NB: Currently only works in SBCL.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The UDP port to listen to for messages.
;;; - The IP address to send UDP messages back out on.
;;; - The UDP port to send messages back out on.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defun osc-call (&optional 
                 (listen-port 8000)
                 (send-ip #(127 0 0 1))
                 (send-port 8001))
;;; ****
  (sb-bsd-sockets::osc-call listen-port send-ip send-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sb-bsd-sockets)

;;; The main function we call to process lisp code and other messages sent via
;;; OSC 
(defun osc-call (listen-port send-ip send-port) 
  "listens on a given port and sends out on another
   note ip#s need to be in the format #(127 0 0 1) for now.. ."
  (let ((in (make-udp-socket))
        (out (make-udp-socket))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 512)))
    (socket-bind in #(0 0 0 0) listen-port)
    (socket-connect out send-ip send-port)
    (let ((stream 
           (socket-make-stream
            out :input t :output t 
            :element-type '(unsigned-byte 8) :buffering :full)))
      (unwind-protect 
           (loop with happy = t while happy do 
                (socket-receive in buffer nil)
                (let* ((oscuff (osc:decode-bundle buffer))
                       ;; here: check if there's an opening (
                       (soscuff 
                        (if (char= #\( (elt (first oscuff) 0))
                            'lisp
                            (read-from-string (first oscuff)))))
                  (format t "~&osc-->message: ~a" oscuff)
                  ;; need this otherwise messages only get printed when we quit
                  (finish-output t)
                  (case (sc::rm-package soscuff :sb-bsd-sockets)
                    ;; test
                    (int (handle-number stream (second oscuff)))
                    (quit (setf happy nil))
                    ;; save opening ( so evaluate lisp code
                    (lisp (osc-eval stream oscuff))
                    (t (warn "osc-sc::osc-call: don't understand ~a. Ignoring."
                             soscuff)
                       (finish-output)))))
        (when in (socket-close in)) 
        (when out (socket-close out)))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-udp-socket()
  (make-instance 'inet-socket :type :datagram :protocol :udp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; evaluate lisp code
(defun osc-eval (stream expr)
  ;; (format t "~&osc-eval: ~a ~a" expr (stringp (first expr)))
  ;; (finish-output t)
  (let ((result (sc::list-to-string expr)))
    (setf result (eval (read-from-string result)))
    (unless (listp result)
      (setf result (list result)))
    (write-sequence (osc:encode-message (sc::list-to-string result))
                    stream)
    (finish-output stream)))

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

