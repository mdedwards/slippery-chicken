;; load this file into Emacs or Xemacs to load customizations
;; for Common Music running
;;;
;;; Add CM/CLM/CMN extensions to automode.

(setq auto-mode-alist
      (append '(("\\.clm$"  . lisp-mode)
		("\\.cm$"   . lisp-mode)
		("\\.cmn$"  . lisp-mode)
		("\\.ins$"  . lisp-mode)
		("\\.scm$"  . lisp-mode))
	      auto-mode-alist))
;;;
;;; ignore midi and sound files for autocompletion.

(setq completion-ignored-extensions 
        (append (list ".midi" ".mid" ".snd" ".aiff" ".wav" )
	        completion-ignored-extensions))

;;;
;;; Indent CM/CLM/CMN forms properly

(put 'if 'common-lisp-indent-function 1)
(put 'new 'common-lisp-indent-function 1)
(put 'io 'common-lisp-indent-function 1)
(put 'defobject 'common-lisp-indent-function 'defun)
(put 'process 'common-lisp-indent-function 0)
(put 'with-sound 'common-lisp-indent-function 'defun)
(put 'definstrument 'common-lisp-indent-function 'defun)
(put 'dopairs 'common-lisp-indent-function  1)
(put 'define-signal-handler 'common-lisp-indent-function
     'lisp-indent-defmethod)
;; scheme
(put 'define 'common-lisp-indent-function 1)
(put 'define-class 'common-lisp-indent-function 2)
(put 'define-method 'common-lisp-indent-function 1)
(put 'define-list-struct 'common-lisp-indent-function 1)
(put 'letrec 'common-lisp-indent-function 1)

;(global-set-key "\C-x\l" 'lisp-listener)  ; listener.el

;;;
;;; Claim scratch buffer for Lisp mode if its empty. This

(let ((scratch (get-buffer "*scratch*")))
  (if scratch
      (if (not (buffer-modified-p scratch))
	  (with-current-buffer scratch
	    (lisp-mode)))))

