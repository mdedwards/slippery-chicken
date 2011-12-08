;;; TEST SUITE

;;; Load this file in the Lisp prompt while in the slippery-chicken package.
;;;
;;; From there you can either run individual tests for each method or function;
;;; tests for each class that run through all of the methods/functions within a
;;; given class; or all of the classes.
;;;
;;; Right now only the (test-assoc-list) is available as a class run.
;;;
;;; Results are printed for each individual test and labeled accordinglingly,
;;; with the corresponding hierarchy depending on the way it's called (not the
;;; actual class structure). Each test is marked as pass or FAIL.
;;;
;;; Each test, be it individual or combined groups, will return either T or NIL
;;; on completion to report the overall result. (T = all has passed, NIL =
;;; there's a FAIL in there somewhere).

;;; 07.12.2011 

(defvar *test-name* nil)

;;; MDE removed named-object-p: now in class

(defmacro with-gensyms ((&rest names) &body body)
  "Generate code that expands into a LET that binds each named variable to a
  GENSYM'd symbol"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test
  functions or use 'check' to run individual test cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order." 
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil))) 
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check."
  ;;; MDE: shouldn't we only print if it fails?
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;; MDE: why deftest, rather than just test straight away?  Isn't it a pain to
;;; have to define this here, then add it to test-assoc-list call below, then
;;; add that to test-all below that, then actually call it?
(deftest test-al-get-keys () 
  (check
    (equal (get-keys (make-assoc-list 'test '((cat felix)
                                              (dog fido)
                                              (cow bessie))))
           '(cat dog cow))
    (equal (get-keys (make-assoc-list 'test '((cat felix)
                                              (dog ((scottish terrier)
                                                    (german shepherd)
                                                    (irish wolfhound)))
                                              (cow bessie))))
           '(cat dog cow))))

(deftest test-al-get-first ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey)))))
    (check
      (named-object-p (get-first al))
      (eq (id (get-first al)) 'jim)
      (eq (data (get-first al)) 'beam))))

(deftest test-al-get-last ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey)))))
    (check
      (named-object-p (get-last al))
      (eq (id (get-last al)) 'wild)
      (eq (data (get-last al)) 'turkey))))

(deftest test-al-get-position ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey)))))
    (check
      (eq (get-position 'four al) 1)  
      (eq (get-position 'jack al) nil)
      (eq (get-position 'jim al 1) nil))))

;; this one is supposed to produce a warning for the third EQ boolean 
(deftest test-al-get-data-data ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey)))))
    (check
      (eq (get-data-data 'jim al) 'beam)
      ;; 8.12.11 ME: this was 'nil: removed quote
      (eq (get-data-data 'jack al) nil))))

;; this one is supposed to produce warnings for the 3rd and 4th EQ booleans
(deftest test-al-get-data ()
  (let ((al (make-assoc-list 'al-test '((jim beam) 
                                        (four roses) 
                                        (wild turkey)))))
    (check
      (named-object-p (get-data 'four al))
      (eq (id (get-data 'four al)) 'four)
      (eq (data (get-data 'four al)) 'roses)
      (eq (get-data 'jack al) nil)
      (eq (get-data 'jack al t) nil)
      (eq (get-data 'jack al nil) nil))))

(deftest test-al-add ()
  (let ((al (make-assoc-list 'test '((jim beam)
                                     (four roses)
                                     (wild turkey)))))
    (check
      (add '(makers mark) al)
      (named-object-p (get-data 'makers al))
      (eq (id (get-data 'makers al)) 'makers)
      (eq (data (get-data 'makers al)) 'mark)
      (eq (get-position 'makers al) 3)
      (add '(knob creek) al '(jack daniels)))))

;; this one is supposed to produce a warning on the 3rd EQ boolean
(deftest test-al-set-data ()
  (let ((al (make-assoc-list 'test '((cat felix)
                                     (dog fido)
                                     (cow bessie)))))
    (check
      (named-object-p (set-data 'dog '(dog spot) al))
      (eq (id (set-data 'dog '(dog spot) al)) 'dog)
      (eq (data (set-data 'dog '(dog spot) al)) 'spot)
      (eq (set-data 'pig '(pig wilber) al) nil)
      (eq (id (set-data 'dog '(pig wilbur) al)) 'pig)
      (eq (get-data-data 'pig al) 'wilbur))))

(deftest test-al-add-to-list-data ()
  (let ((al (make-assoc-list 'test '((cat felix)
                                     (dog (fido spot))
                                     (cow bessie)))))
    (check
      (named-object-p (add-to-list-data 'rover 'dog al))
      (eq (id (get-data 'dog al)) 'dog)
      (equal (get-data-data 'dog al) '(fido spot rover)))))

(deftest test-al-add-to-list-data-force ()
  (let ((al (make-assoc-list 'test '((cat felix)
                                     (dog (fido spot))
                                     (cow bessie)))))
    (check
      (named-object-p (add-to-list-data-force 'rover 'dog al))
      (eq (id (get-data 'dog al)) 'dog)
      (equal (get-data-data 'dog al) '(fido spot rover))
      (add-to-list-data-force 'wilber 'pig al)
      (equal (get-keys al) '(cat dog cow pig)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; these are the test groupings for the individual classes then:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-assoc-list ()
  (combine-results
    (test-al-get-keys)
    (test-al-get-first)
    (test-al-get-last)
    (test-al-get-position)
    (test-al-get-data-data)
    (test-al-get-data)
    (test-al-add)
    (test-al-set-data)
    (test-al-add-to-list-data)
    (test-al-add-to-list-data-force)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this then runs the entire suite.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-all ()
  (combine-results
    (test-assoc-list)))

;;; MDE: shouldn't we then call test-all in this file?
