;;; **********************************************************************
;;; $Name: rel-2_6_0 $
;;; $Revision: 1.1.1.1 $
;;; $Date: 2003/06/25 10:45:05 $
;;; 
;;; This is my emacs init file that I use to work with CM on Linux and
;;; windows. To use it, edit the appropriate image variable(s) defined
;;; below to reflect your machine. Then if you are NOT using CLISP,
;;; edit the line:
;;;      (setq inferior-lisp-program clispcm)
;;; and change "clispcm"  to the one you are using. Since I use so many
;;; different images I also have functions defined to boot whatever 
;;; image I'm interested in. You use these functions by first typing
;;; ESC-: to get Emac's "Eval:" prompt and then calling the function.
;;; Once you are done, save the file as ~/.emacs on Linux
;;; and C:\_EMACS if you are on Windows.
;;; **********************************************************************

(require 'inf-lisp)
;(setq load-path
;      (append load-path
;              (list
;               "/sw/src/xemacs-21.4.9-2/xemacs-21.4.9/lisp/")))

;(load "~/.xemacs/listener.el")

(require 'font-lock)
(setq html-font-lock-keywords
      (append html-font-lock-keywords
	      '(("</?\\.[^>]*>" 0 font-lock-comment-face t))))

;;;
;;; various images for the differnet LISP/OS combinations
;;; Clisp:

(setq clisp "/sw/bin/clisp")
(defun clisp () (lisp-listener clisp))
(setq clispcm "/Lisp/bin/cm-2.4.0/clisp/cm")
(defun clispcm () (lisp-listener clispcm))

;;; Open MCL:

(setq openmcl "/Lisp/bin/openmcl")
(defun openmcl () (lisp-listener openmcl))
(setq opencm "/Lisp/bin/cm-2.4.0/openmcl/cm")
(defun opencm () (lisp-listener opencm))

;;; Guile:

(setq guile "/sw/bin/guile")
(defun guile () (lisp-listener guile))
(setq guilecm "/Lisp/bin/cm-2.4.0/guile/cm")
(defun guilecm () (lisp-listener guilecm))

;;; ACL:

(setq acl "/Lisp/acl62_trial/alisp")
(defun acl () (lisp-listener acl))
(setq aclcm "/Lisp/bin/cm-2.4.0/acl/cm")
(defun aclcm () (lisp-listener aclcm))

;;;
;;; My default is opencm

(setq inferior-lisp-program opencm)
(global-set-key "\C-x\l" 'lisp-listener)  ; cm.el

;(install-mcl-key-bindings lisp-mode-map)  ; cm.el

;;;
;;; start differnt images
;;;

(defun guile () 
  (setq listener-syntax 'scheme)
  (lisp-listener "/usr/local/bin/guile") )

(defun mkcm ()
  (interactive)
  (lisp-listener openmcl)
  (comint-send-string
   (inferior-lisp-proc)
   "(load \"/Lisp/adm/all.lisp\")"))

;; Use apple keybindings

;(global-set-key '(alt x) 'kill-primary-selection)
;(global-set-key '(alt c) 'copy-primary-selection)
;(global-set-key '(alt v) 'yank-clipboard-selection)
;(global-set-key '(alt a) 'mark-whole-buffer)
;(global-set-key '(alt z) 'undo)
;(global-set-key '(alt l) 'lisp-listener)
;(global-set-key '(alt e) 'lisp-eval-selection)
;(global-set-key '(alt h) 'lisp-eval-buffer)
;(global-set-key '(alt .) 'lisp-abort-error)

;;;
;;;
;;;

(setq auto-mode-alist
      (append '(("\\.clm$"  . lisp-mode)
		("\\.cm$"   . lisp-mode)
		("\\.cmn$"  . lisp-mode)
		("\\.ins$"  . lisp-mode)
		("\\.cl$"   . lisp-mode)
		("\\.lisp$" . lisp-mode)
		("\\.scm$"  . lisp-mode)
		("\\.c$"    . c++-mode)
		("\\.h$"    . c++-mode)
		("\\.m$"    . objc-mode)
		("\\.html$" . html-mode)
		("\\.mtxt$" . html-mode)
		) 
	      auto-mode-alist))

(setq completion-ignored-extensions 
        (append (list ".snd" ".aiff" ".wav" ".lib" ".fas" ".au" ".so" 
		      ".o" ".hqx" ".gz" ".tar" ".fusl" ".fisl") 
	        completion-ignored-extensions))

;;;
;;; Mac/Unix EOL hackery
;;;

(defun convert-buffer-eol (&optional macify) 
  "Convert buffer eol's from Mac to Unix. If macify
   is true then does the reverse."
  (let ((lf (char-to-string (int-to-char 10)))
	(cr (char-to-string (int-to-char 13))))
    (if macify
      (let ((tmp lf))
        (setq lf cr)
        (setq cr tmp)))
    (beginning-of-buffer)
    (while (search-forward cr nil t)
      (replace-match lf))
    (beginning-of-buffer)))

(global-set-key '(meta f1)
  (lambda () (interactive) (convert-buffer-eol)))

(global-set-key '(meta f2)
  (lambda () (interactive) (convert-buffer-eol t)))

(defun zap-crlf ()
  (interactive)
  ;; delete all ^M (Return) in buffer
  (let ((cr (char-to-string (int-to-char 13))))
    (beginning-of-buffer)
    (while (search-forward cr nil t)
      (delete-backward-char))
    (beginning-of-buffer)))

;;;
;;; personal preferences
;;;

(setq bell-volume 0) 
(setq inhibit-startup-message t)
(setq default-frame-plist '(width 75 height 45))
(setq initial-scratch-message nil)

;;;
;;; make display time show day/date and not use silly glyphs
;;

(setq display-time-day-and-date t)
(setq display-time-form-list '(date time))
(setq display-time-compatible t)
(display-time)

(setq delete-auto-save-files t)
(setq mouse-highlight-text 'symbol)






