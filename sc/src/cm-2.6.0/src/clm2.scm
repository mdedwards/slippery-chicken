;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
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
;;; $Revision: 1.5 $
;;; $Date: 2005/01/15 15:47:31 $

;;;
;;; this file contains glue code to clm2
;;;

(define-method (open-io (io <clm-audio-stream>) dir . args)
  args
  (if (eq? dir ':output)
    (let ((inits (io-handler-args io))
          (ftype (filename-type (object-name io)))
          (autype #f)
          (fmat #f))
      (cond ((string-ci=? ftype "snd")
             (set! autype mus-next)
             (set! fmat mus-bshort))
            ((string-ci=? ftype "aiff")
             (set! autype mus-aifc)
             (set! fmat mus-bshort))
            ((string-ci=? ftype "wav")
             (set! autype mus-riff)
             (set! fmat mus-lshort)))
      (unless (list-prop inits ':type)
        (push autype inits)
        (push ':type inits))
      (unless (list-prop inits ':data-format)
        (push fmat inits)
        (push ':data-format inits))
      (set! (io-open io)
            (apply #'init-with-sound
                   ':output
                   (file-output-filename io)
                   inits))
      (unless (null? (audio-file-output-trace io))
        (apply #'tell-snd
               (file-output-filename io)
               inits))
      io)
    (next-method)))

(define-method (close-io (io <clm-audio-stream>) . mode)
  (let ((wsd (io-open io))
        (old *clm-with-sound-depth*))
    (set! *clm-with-sound-depth* 1)
    (when (eq? (slot-ref io 'output-trace) #t)
      (format #t "Done!~&"))
    (when (and (pair? mode) (car mode))
      (set! (wsdat-play wsd) #f))
    (finish-with-sound wsd)
    (set! (io-open io) #f)
    (set! *clm-with-sound-depth* old)))

(define (tell-snd file . args)
  (with-args (args &key reverb decay-time reverb-data
                   (channels *clm-channels*) (srate *clm-srate*)
                   &allow-other-keys)
    (format #t "~%; File: ~s" file)
    (format #t "~%; Channels: ~s" channels)
    (format #t "~%; Srate: ~s" srate)
    (format #t "~%; Reverb: ~a~%" (or reverb "None"))
    (if decay-time
      (format #t "decay time: ~s%" decay-time))
    (if reverb-data
      (format #t "reverb data: ~s~%" reverb-data))
    (values)))

;(define definstrument-hook 
;  (lambda (name args)
;    (let* ((opts (if (pair? name) (cdr name) (list)))
;           (obj? #t ) ;(list-prop opts ':instrument-only #t)
;           ;(ins? (not (list-prop opts ':object-only)))
;           (tpar (list-prop opts ':time-parameter ))
;           )
;      (if obj?
;        (formals->defobject 
;         (list* (if opts (first name) name) args)
;         tpar)
;        (list)))))

;(define clm:definstrument
;  ;; this gets omitted in the cltl sources.
;  (if (bound? 'definstrument)
;    definstrument #f))
;				      
;(defmacro definstrument (name args . body)
;  (let* ((opts (if (pair? name) (cdr name) (list)))
;         (obj? #t ) ;(list-prop opts ':instrument-only #t)
;         (tpar (list-prop opts ':time-parameter ))
;         )
;    `(begin
;      (clm:definstrument ,name ,args ,@body)
;      ,(if obj?
;           (formals->defobject (list* (if (pair? opts)
;					(first name) 
;					name) 
;				      args)
;			       tpar)
;           (list)))))

(define (definstrument-hook name args)
  (let* ((opts (if (pair? name) (cdr name) (list)))
         (tpar (list-prop opts ':time-parameter )))
    (formals->defobject (list* (if (pair? opts)
                                 (first name) 
                                 name) 
                               args)
                        tpar)))

(define *definstrument-hook* #'definstrument-hook)

;(formals->defobject '(fm time duration frequency amplitude
;                      &key (amplitude-env '(0 0 25 1 75 1 100 0))
;                      (mratio 1) (index 1) (index-env '(0 1 100 1))
;                      (degree 0) (distance 0) (reverb 0))
;                    )
