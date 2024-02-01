;;; $Name: rel-2_6_0 $
;;; $Revision: 1.7 $
;;; $Date: 2004/06/20 17:48:47 $

;;;
;;; load script for cm in guile.
;;;

(if (< (string->number (minor-version) ) 5)
  (error "CM runs in guile-1.5.0 or higher."))

;;; user defined *cm-root* or default to current 

(if (not (module-bound? (current-module) '*cm-root*))
  (begin
   (define *cm-root* #f)

   ;; courtesy of bil, removed %load-hook
   (let ((curfile (port-filename (current-load-port)))
         (last-slash #f)
         (parent-slash #f))
     (do ((i 0 (1+ i)))
         ((= i (string-length curfile)))
       (if (char=? (string-ref curfile i) #\/)
         (begin (set! parent-slash last-slash)
                (set! last-slash i))))
     (if (not last-slash)
       (set! *cm-root* "../")
       (if (not parent-slash)
         (set! *cm-root* "./")
         (begin
          (set! *cm-root* (substring curfile 0 (1+ parent-slash)))
          (let ((new-path (substring curfile 0 last-slash)))
            (if (not (member new-path %load-path))
              (set! %load-path (cons new-path %load-path))))))))))

;;; user specified cm-bin-directory or default
;(if (not (module-bound? (current-module) 'cm-bin-directory))
;  (define cm-bin-directory
;    (string-append cm-directory "bin/")))

(do ((files '("guile"
	      "goops"
              "level1"
	      "loop"
	      "utils"
	      "mop"
	      "objects" 
	      "io"
	      "scheduler"
	      "sco"
	      "clm"
	      "clm2"
	      "midi1"
	      "midi2"
	      "midi3"
              ;; "midishare/midishare.scm"
              ;; "midishare/player.scm"
              ;; "cmn"
	      "data"
	      "scales"
	      "spectral"
	      "patterns"
              )
            (cdr files)))
    ((null? files) #f)
  (let* ((f (string-append (car files) ".scm")))
    (display (string-append "; Loading: " *cm-root*
			    "src/" f))
    (newline)
    (load (string-append (car files) ".scm"))))

(let ((ini (string-append *cm-root* "etc/cminit.lisp")))
  (if (file-exists? ini)
    (basic-load ini)))


