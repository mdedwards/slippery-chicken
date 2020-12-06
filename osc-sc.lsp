;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             osc-sc.lisp
;;;
;;; Class Hierarchy:  None, no classes defined.
;;;
;;; Version:          1.0.11
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
;;; $$ Last modified:  10:20:22 Thu Nov 12 2020 CET
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
(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old note: The return value of the Lisp call will be returned to the given
;;; IP address on the given port.  It is up to the receiver to then parse the
;;; result (e.g. in MaxMSP via [udpreceive][fromsymbol][route list int float
;;; symbol] ).  

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
;;; per usual.  
;;;
;;; The first two tokens in the list must be /osc-sc and a (usually unique)
;;; identifer e.g. ((/osc-sc 1060-osc-sc-eval (print 'dog) )) Both of these
;;; tokens will be sent back over OSC in a list, along with the result of the
;;; Lisp call.  If you're using MaxMSP I would recommend using
;;; osc-sc-eval.maxpat (see below) to evaluate your Lisp code as this will
;;; package it up with the right tokens and send the evaluated result out of
;;; its outlet without causing conflicts with other instances of itself.
;;;
;;; For an example MaxMSP patch, see osc-test.maxpat in the examples folder of
;;; the documentation: http://michael-edwards.org/sc/examples/osc-test.maxpat
;;; You'll also need http://michael-edwards.org/sc/examples/osc-sc-eval.maxpat
;;;
;;; Note that as of November 2020, functions that return lists can also be
;;; processed: each element will be sent back to Max et al via UDP.
;;; 
;;; NB: Currently only works in SBCL.
;;;     Some lists (e.g. those including strings/symbols) might not be
;;;     recognised as lists by MaxMPS's [route], so process them directly after
;;;     [fromsymbol]. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :listen-port. The UDP port to listen to for messages. Default = 8090.
;;; - :send-ip.  The IP address to send UDP messages back out on. 
;;;    Default = #(127 0 0 1))
;;; - :send-port. The UDP port to send messages back out on. Default = 8091.
;;; - :print. Print messages as they arrive. Default = NIL.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defun osc-call (&key
                 (listen-port 8090)
                 (send-ip #(127 0 0 1))
                 (print nil)
                 (send-port 8091))
;;; ****
  (sb-bsd-sockets::osc-call listen-port send-ip send-port print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun osc-send-list (list &optional (warn t))
  (declare (special sb-bsd-sockets::+osc-sc-output-stream+))
  (sb-bsd-sockets::osc-send-list list sb-bsd-sockets::+osc-sc-output-stream+
                                 warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF osc-sc.lsp

