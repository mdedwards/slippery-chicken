;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             sc-test-suite-aux.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of macros/functions for testing slippery
;;;                   chicken  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    15th December 2011
;;;
;;; $$ Last modified:  09:32:41 Thu Sep 20 2018 CEST
;;;
;;; SVN ID: $Id: rthm-seq-bar.lsp 509 2011-12-14 20:35:27Z reed@seanreed.ie $
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

(in-package :sc)

;;; 08.12.2011 SAR: Added a new global variable to hold the complete list of
;;; all tests named as they're made. (This is a crude approach, I know.)
(defparameter *sc-test-name* nil)
(defparameter *sc-test-all-tests* nil)

;;; SAR Mon Jan 16 12:49:06 GMT 2012: Added a new global variable to
;;; store the results of method-and-function tests, so that the results can
;;; always be printed together with those of sc-test-full.
(defparameter *sc-test-meth-and-func-tests-state*
  "- METHOD AND FUNCTION TESTS NOT PERFORMED.")

;;; SAR Tue Jul 24 11:16:04 BST 2012: New global variable for results of method
;;; and function tests, but here for webpage tests
(defparameter *sc-test-webpage-examples-tests-state*
  "- WEBPAGE EXAMPLE TESTS NOT PERFORMED")

(defmacro sc-test-with-gensyms ((&rest names) &body body)
  "Generate code that expands into a LET that binds each named variable to a
   GENSYM'd symbol"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;; 08.12.2011 SAR: Added a line to push the name of each newly defined test
;;; into the list of all tests
(defmacro sc-deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test
  functions or use 'sc-test-check' to run individual test cases."
  (unless (member `,name *sc-test-all-tests*) (push `,name *sc-test-all-tests*))
  `(defun ,name ,parameters
     (let ((*sc-test-name* (append *sc-test-name* (list ',name))))
       ,@body)))

;;; SAR Fri Dec 30 12:06:21 EST 2011
;;; De-activated the statement of which test is being run.
;;; MDE Tue Mar 20 08:45:08 2012 -- re-activated: print each test
(defmacro sc-test-check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(sc-test-combine-results
     (not (format t "~&Testing: ~a...~%" *sc-test-name*))
     ,@(loop for f in forms collect `(sc-test-report-result ,f ',f))))

(defmacro sc-test-combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order." 
  (sc-test-with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;;; SAR Mon Dec 26 10:47:51 EST 2011: 
;;; PASS printing is currently commented out 
(defun sc-test-report-result (result form)
  "Report the results of a single test case. Called by 'sc-test-check'."
  ;; (when result (format t "~&passed: ~a ~a~%" *sc-test-name* form))
  (unless result 
    (format t "~%FAIL: ~a: ~%~a~%" *sc-test-name* form))
    ;; (error "~%FAIL: ~a: ~a~%" *sc-test-name* form))
  result)

;;; 08.12.11 SAR: Added a macro to test all tests stored in the
;;; *sc-test-all-tests* list
(defmacro sc-test-test-all ()
  "Run all tests whose names are stored within *sc-test-all-tests* (which
   should be all tests defined using sc-deftest)"  
  `(sc-test-combine-results
     ;; MDE Thu Dec 15 22:54:32 2011 -- reverse so that they run in the order
     ;; in which they were defined  
     ,@(loop for at in (reverse *sc-test-all-tests*) collect (list at))))

;;; SAR Thu Dec 15 12:25:30 GMT 2011: 
;;; Added this function to print the next test in the list of sc-test-all-tests
;;; after the given test. This is handy when the test currently being tested
;;; fails internally before the macro gets a chance to print its name. Enter
;;; the name of the last test passed to see the next test on the list. This
;;; must be called from the Lisp prompt. 
(defun sc-test-next-test (last-test)
  (nth (1+ (position last-test *sc-test-all-tests*)) *sc-test-all-tests*)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test-suite utility functions

;;; MDE Thu Jan 12 13:27:21 2012 -- rather than just probe-file

(defun file-size (file)
  (with-open-file (stream file :direction :input :element-type '(signed-byte 1))
    (file-length stream))) ; bytes

;;; MDE Sat May 12 16:30:54 2012
;;;                                   bytes
(defun file-write-ok (file &optional (min-size 1) (max-secs-ago 120)) 
  (let ((age (- (get-universal-time) (file-write-date file)))
        (size (file-size file))
        (result nil))
    (setf result (probe-file file))
    (unless result
      (warn "sc-test-suite-aux::file-write-ok: file ~a doesn't exist" file))
    (setf result (< age max-secs-ago))
    (unless result
      (warn "sc-test-suite-aux::file-write-ok: file ~a is ~a seconds old ~
             ~%(expected maximum ~a)"
            file age max-secs-ago))
    (setf result (when (numberp size) (>= size min-size)))
    (unless result
      (warn "sc-test-suite-aux::file-write-ok: file ~a has size ~a ~
             (expected minimum ~a)" file size min-size))
    result))


;;; SAR Fri Mar 16 10:07:50 GMT 2012 -- probe a file and delete if it exists
(defun probe-delete (file)
  (when (probe-file file) (delete-file file) t))

(defun equal-within-less-tolerance (x y)
  (equal-within-tolerance x y 0.0001))

(defun test-suite-file (filename)
  (concatenate 'string  cl-user::+slippery-chicken-home-dir+ "test-suite/"
               filename))

;;; SAR Tue Jul 10 13:55:11 BST 2012 -- Added new functions probe-delete-multi
;;; and file-write-ok-multi. These test multiple files in one directory only. 

(defun probe-delete-multi (directory files)
  (notany #'not
          (loop for f in files
             collect (probe-delete (concatenate 'string directory f)))))

(defun file-write-ok-multi (directory files sizes-list)
  (notany #'not
          (loop for f in files
             for s in sizes-list
             collect
               (file-write-ok (concatenate 'string directory f) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sc-test-suite-aux.lsp
