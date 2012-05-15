;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/cmn-glyphs
;;; NAME 
;;; cmn-glyphs
;;;
;;; File:             cmn-glyphs.lsp
;;;
;;; Class Hierarchy:  none, no classes defined
;;;
;;; Version:          1.0.0-beta1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of various postscript glyphs (accidentals
;;;                   etc.) for cmn.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    10th November 2002
;;;
;;; $$ Last modified: 09:01:19 Mon Dec 12 2011 ICT
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cmn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Altered version of Bill's display-natural-harmonic to fill in the circle

(defun display-dead-harmonic (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((hl (head-line note))
         (y-off (+ .3 
                   (if (find-if #'tenuto-p (marks note)) .15 0)
                   (if (and (evenp hl) (< 1 hl 9)) .075 0))) ;was .125
         (y0 (+ (staff-y0 note) (* hl *staff-line-separation*))))
    (matrix-front score (translate-matrix 
                         score mark 
                         (+ (box-x0 note) (box-x0 mark) .05
                            (vis-dx mark) (center note))
                         (+ y0 (box-y0 mark) (vis-dy mark) 
                            (if (eq (direction-from-note mark note) :up) 
                                y-off 
                              (- y-off)))))
    (comment score "dead harmonic")
    (g-send score "0.3 setgray")
    (circle score 0 0 .10 0 360 t)
    (g-send score "0.0 setgray")
    (matrix-back score)))

(define-accent dead-harmonic #'display-dead-harmonic nil '(-.05 -.05 .05 .05))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Same as above but with cross through it like harp/percussion dampen
;;; symbol. 

(defun display-dead-harmonic-damp (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((hl (head-line note))
         (y-off (+ .3 
                   (if (find-if #'tenuto-p (marks note)) .15 0)
                   (if (and (evenp hl) (< 1 hl 9)) .075 0))) ;was .125
         (y0 (+ (staff-y0 note) (* hl *staff-line-separation*))))
    (matrix-front score (translate-matrix 
                         score mark 
                         (+ (box-x0 note) (box-x0 mark) .05
                            (vis-dx mark) (center note))
                         (+ y0 (box-y0 mark) (vis-dy mark) 
                            (if (eq (direction-from-note mark note) :up) 
                                y-off 
                              (- y-off)))))
    (comment score "dead harmonic damp")
    (g-send score "0.3 setgray")
    (circle score 0 0 .10 0 360 t)
    (with-thickness score mark .02
                    (moveto score 0 0.2)
                    (lineto score 0 -0.2)
                    (moveto score -0.2 0)
                    (lineto score 0.2 0)
                    (draw score))
    (g-send score "0.0 setgray")
    (matrix-back score)))

(define-accent dead-harmonic-damp #'display-dead-harmonic-damp nil 
               '(-.05 -.1 .05 .1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-quarter-note-in-parentheses (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ 1.5 (vis-dy mark) (staff-y0 note)))
         (x-off (+ 0.1 (box-x0 note) (vis-dx mark))))
    ;; .6 is the scaling
    (matrix-front score (translate-matrix score mark x-off y-off 0.6))
    (comment score "quarter note in parentheses")
    ;; stem
    (with-thickness score mark .03
                    (moveto score 0.27 .06)
                    (lineto score 0.27 .7)
                    (draw score))
    ;; head
    (moveto score 0.014 -0.088)
    (curveto score -0.014 -0.030 0.026 0.056 0.090 0.096)
    (curveto score 0.144 0.128 0.230 0.142 0.270 0.092)
    (curveto score 0.316 0.024 0.258 -0.060 0.190 -0.100)
    (curveto score 0.130 -0.126 0.066 -0.136 0.014 -0.088)
    (fill-in score)
    (draw score)
    (with-thickness score mark .03
                    ;; left bracket
                    (moveto score -0.25 -0.13)
                    (lineto score -0.25 .73)
                    (moveto score -0.265 -0.13)
                    (lineto score -0.1 -.13)
                    (moveto score -0.265 0.73)
                    (lineto score -0.1 .73)
                    ;; right bracket
                    (moveto score .6 -0.13)
                    (lineto score .6 .73)
                    (moveto score .615 -0.13)
                    (lineto score .45 -.13)
                    (moveto score .615 0.73)
                    (lineto score .45 .73)
                    ;;(moveto score -0.165 0.73)
                    ;;(lineto score 0.0 .73)
                    (draw score))
    ;; (g-send score (format nil "~a" (sc::ps-text -1 -.3 "[" :font-size 50)))
    (draw score)
    (matrix-back score)))

(defvar quarter-note-in-parentheses-bounds '(0.000 -0.120 0.286 0.124))

(define-accent quarter-note-in-parentheses #'draw-quarter-note-in-parentheses
  nil '(-.05 -.05 .05 .05))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Simple upside-down U

(defun display-on-bridge (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ .5 (vis-dy mark) (staff-y0 note)))
         (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "on bridge")
    (with-thickness score mark .02
                    (circle score 0.2 0 .25 0 180)
                    (draw score))
    (matrix-back score)))

(define-accent on-bridge #'display-on-bridge nil '(-.25 -.5 .25 .25))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12th tone below a sharp, an extended right vertical with a half arrow.

(defun draw-sharp-12-down (score &optional style)
  (moveto score 0.140 0.378)
  (lineto score 0.166 0.378)
  (lineto score 0.166 0.202)
  (lineto score 0.208 0.222)
  (lineto score 0.208 0.114)
  (lineto score 0.166 0.096)
  (lineto score 0.166 -0.050)
  (lineto score 0.208 -0.032)
  (lineto score 0.208 -0.136)
  (lineto score 0.166 -0.152)
  (lineto score 0.166 -0.512)
  (lineto score 0.266 -0.404)
  (lineto score 0.152 -0.694)
  (lineto score 0.140 -0.694)
  (lineto score 0.140 -0.164)
  (lineto score 0.070 -0.194)
  (lineto score 0.070 -0.378)
  (lineto score 0.042 -0.378)
  (lineto score 0.042 -0.206)
  (lineto score 0.000 -0.222)
  (lineto score 0.000 -0.114)
  (lineto score 0.042 -0.100)
  (lineto score 0.042 0.048)
  (lineto score 0.000 0.032)
  (lineto score 0.000 0.136)
  (lineto score 0.042 0.152)
  (lineto score 0.042 0.336)
  (lineto score 0.070 0.336)
  (lineto score 0.070 0.164)
  (lineto score 0.140 0.192)
  (lineto score 0.140 0.378)
  (moveto score 0.070 -0.086)
  (lineto score 0.140 -0.058)
  (lineto score 0.140 0.086)
  (lineto score 0.070 0.058)
  (lineto score 0.070 -0.086)
  (if (not style) (fill-in score) (draw score style)))

(defvar sharp-12-down-bounds '(0.000 -0.694 0.268 0.378))

(define-accidental sharp-12-down #'draw-sharp-12-down 
  sharp-12-down-bounds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-natural-12-up (score &optional style)
  (moveto score 0.000 -0.180)
  (lineto score 0.142 -0.142)
  (lineto score 0.142 -0.346)
  (lineto score 0.170 -0.346)
  (lineto score 0.170 0.174)
  (lineto score 0.028 0.136)
  (lineto score 0.016 0.582)
  (lineto score -0.100 0.296)
  (lineto score 0.000 0.402)
  (lineto score 0.000 -0.180)
  (moveto score 0.028 -0.074)
  (lineto score 0.028 0.048)
  (lineto score 0.142 0.078)
  (lineto score 0.142 -0.046)
  (lineto score 0.028 -0.074)
  (if (not style) (fill-in score) (draw score style)))

(defvar natural-12-up-bounds '(-0.100 -0.346 0.170 0.582))

(define-accidental natural-12-up #'draw-natural-12-up natural-12-up-bounds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-flat-12-down (score &optional style)
  (moveto score 0.002 0.482)
  (lineto score 0.002 -0.442)
  (lineto score 0.130 -0.208)
  (lineto score 0.030 -0.308)
  (lineto score 0.030 -0.186)
  (curveto score 0.014 -0.186 0.070 -0.130 0.092 -0.114)
  (curveto score 0.142 -0.086 0.246 -0.024 0.216 0.082)
  (curveto score 0.166 0.222 0.028 0.086 0.028 0.086)
  (lineto score 0.028 0.482)
  (lineto score 0.002 0.482)
  (moveto score 0.028 0.036)
  (lineto score 0.028 -0.136)
  (curveto score 0.034 -0.140 0.208 0.028 0.122 0.090)
  (curveto score 0.114 0.092 0.086 0.104 0.028 0.036)
  (if (not style) (fill-in score :even-odd t) (draw score style)))

(defvar flat-12-down-bounds '(-0.100 -0.442 0.220 0.482))

(define-accidental flat-12-down #'draw-flat-12-down flat-12-down-bounds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-sharp-12-up (score &optional style)
  (moveto score 0.140 0.380)
  (lineto score 0.164 0.380)
  (lineto score 0.164 0.202)
  (lineto score 0.208 0.222)
  (lineto score 0.208 0.114)
  (lineto score 0.164 0.096)
  (lineto score 0.164 -0.050)
  (lineto score 0.208 -0.032)
  (lineto score 0.208 -0.136)
  (lineto score 0.164 -0.150)
  (lineto score 0.164 -0.334)
  (lineto score 0.140 -0.334)
  (lineto score 0.140 -0.164)
  (lineto score 0.070 -0.194)
  (lineto score 0.070 -0.376)
  (lineto score 0.040 -0.376)
  (lineto score 0.040 -0.206)
  (lineto score 0.000 -0.222)
  (lineto score 0.000 -0.114)
  (lineto score 0.040 -0.100)
  (lineto score 0.040 0.048)
  (lineto score 0.000 0.032)
  (lineto score 0.000 0.136)
  (lineto score 0.040 0.150)
  (lineto score 0.040 0.514)
  (lineto score -0.060 0.406)
  (lineto score 0.056 0.694)
  (lineto score 0.070 0.164)
  (lineto score 0.140 0.192)
  (lineto score 0.140 0.378)
  (moveto score 0.070 -0.086)
  (lineto score 0.140 -0.056)
  (lineto score 0.140 0.086)
  (lineto score 0.070 0.056)
  (lineto score 0.070 -0.086)
  (if (not style) (fill-in score) (draw score style)))

(defvar sharp-12-up-bounds '(-0.056 -0.376 0.208 0.694))

(define-accidental sharp-12-up #'draw-sharp-12-up sharp-12-up-bounds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-natural-12-down (score &optional style)
  (moveto score 0.000 -0.180)
  (lineto score 0.142 -0.142)
  (lineto score 0.142 -0.402)
  (lineto score 0.156 -0.582)
  (lineto score 0.270 -0.296)
  (lineto score 0.170 -0.402)
  (lineto score 0.170 0.174)
  (lineto score 0.028 0.136)
  (lineto score 0.028 0.346)
  (lineto score 0.000 0.346)
  (lineto score 0.000 -0.180)
  (moveto score 0.028 -0.074)
  (lineto score 0.028 0.048)
  (lineto score 0.142 0.078)
  (lineto score 0.142 -0.046)
  (lineto score 0.028 -0.074)
  (if (not style) (fill-in score) (draw score style)))

(defvar natural-12-down-bounds '(0.000 -0.582 0.270 0.346))

(define-accidental natural-12-down #'draw-natural-12-down 
  natural-12-down-bounds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; changed thickness of Bill's sprechstimme.

(defun sc-sprechstimme (&rest objects)
  (let ((nm (apply #'mark nil nil objects)))
    (stem-mark #'(lambda (score x0 y0 y1)
                   (comment score "sprechstimme")
                   (with-thickness score nm .02
                                   (moveto score 
                                           (- (+ x0 (vis-dx nm)) .125 .01) 
                                           (- (+
                                               (vis-dy
                                                nm) (*
                                                .5
                                                (+
                                                 y0 y1))) .125))  
                                   (rlineto score .25 .25)
                                   (rmoveto score 0 -.25)
                                   (rlineto score -.25 .25)
                                   (draw score))))))

(defvar sc-sprechstimme (sc-sprechstimme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun draw-filled-diamond (&rest objects)
  (make-instance 
   'sundry :name :staccato 
   :mark
   #'(lambda (mark note score &optional justifying)
       (comment score "filled diamond")
       (g-send score "0.7 setgray")
       (moveto score 0 0)
       (rlineto score -.14 -.14)
       (rlineto score -.14 .14)
       (rlineto score .14 .14)
       (rlineto score .14 -.14)
       (fill-in score)
       (g-send score "0.0 setgray")
       (setf (line-width score) .03)
       (moveto score 0 0)
       (rlineto score -.14 -.14)
       (rlineto score -.14 .14)
       (rlineto score .14 .14)
       (rlineto score .14 -.14)
       (draw score)
       (setf (line-width score) 0)
       (comment score "end filled diamond"))))
|#

(defun draw-gray-filled-diamond (score &optional style)
  (declare (ignore style)) 
  (comment score "gray filled diamond")
  (g-send score "0.7 setgray")
  (moveto score 0 0)
  (rlineto score .14 .14)
  (rlineto score .14 -.14)
  (rlineto score -.14 -.14)
  (rlineto score -.14 .14)
  (fill-in score)
  (g-send score "0.0 setgray")
  (setf (line-width score) .03)
  (moveto score 0 0)
  (rlineto score .14 .14)
  (rlineto score .14 -.14)
  (rlineto score -.14 -.14)
  (rlineto score -.14 .14)
  (draw score)
  (setf (line-width score) 0)
  (comment score "end gray filled diamond"))

(defvar gray-filled-diamond-bounds '(0.0 -.14 .28 .14))

(define-accidental gray-filled-diamond
  #'(lambda (score &optional fill) 
      (declare (ignore fill)) 
      (draw-gray-filled-diamond score)) gray-filled-diamond-bounds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; It's not as simple as it would seem to add a note-head to cmn, mainly
;;; because of the offsets that need to be calculated for whether the note is
;;; stem up or stem down.  So just modify Bill's get-note-head function from
;;; cmn2.lisp to add the new case

(defun get-note-head (head-quarters &optional (head-type nil))
  (if (or (not head-type) 
          (member head-type '(:normal :small)))
      (if (= head-quarters 8)
          double-whole-note
        (if (>= head-quarters 4)
            whole-note
          (if (or (>= head-quarters 2) (= head-quarters 4/3))
              half-note
            quarter-note)))
    (if (sundry-p head-type)
        head-type
      (case head-type
        ;; ME 15/3/03
        (:gray-filled-diamond gray-filled-diamond)
        (:diamond (if (< head-quarters 2)
                      filled-diamond-note 
                    open-diamond-note))
        (:diamond-1 (if (< head-quarters 2) 
                        filled-diamond-1-note 
                      open-diamond-1-note))
        (:tremolo half-note)
        (:artificial-harmonic open-diamond-note)
        (:triangle (if (< head-quarters 2) 
                       filled-triangle-note
                     open-triangle-note))
        (:slash slash-note)
        (:square (if (< head-quarters 2) filled-square-note open-square-note))
        (:x rhythmX-note)
        (:circled-x circled-x-note)
        (:breath-in (if (< head-quarters 2)
                        filled-breath-in-note
                      (if (< head-quarters 4)
                          open-breath-in-note
                        whole-breath-in-note)))
        (:breath-out (if (< head-quarters 2)
                        filled-breath-out-note
                      (if (< head-quarters 4)
                          open-breath-out-note
                        whole-breath-out-note)))
        (:airy-head (if (< head-quarters 2)
                        filled-airy-note
                      open-airy-note))
        (:arrow-up (if (< head-quarters 2)
                       filled-arrow-up-note 
                     open-arrow-up-note))
        (:arrow-down (if (< head-quarters 2)
                         filled-arrow-down-note 
                       open-arrow-down-note))
        (:none headless-note)
        (otherwise (cmn-error "unknown note head type: ~A" head-type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cmn-glyphs.lsp
