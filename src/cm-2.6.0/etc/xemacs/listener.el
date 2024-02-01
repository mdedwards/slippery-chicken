;;; **********************************************************************
;;; $Name: rel-2_6_0 $
;;; $Revision: 1.7 $
;;; $Date: 2004/01/10 19:41:09 $
;;; 
;;; Provides a Lisp interaction menu in Lisp Mode and a dedicated Lisp
;;; Listener frame (window) for working in the Lisp process.  To start 
;;; the Listener use the 'lisp-listener' function documented below. The
;;; Lisp menu and Listener window are built on top inf-lisp.
;;;
;;; lisp-listener (cmdstr)                                     [Function]
;;;   Starts up a lisp process in an *inferior-lisp* buffer running
;;;   in its own dedicated frame.
;;;
;;; listener-properties                                        [Variable]
;;;   A list of frame properties passed to make-frame when the Listener
;;;   frame is created. Note that Xemacs and Emacs differ in both the
;;;   format and content of tese properties.
;;;
;;; Lisp Menu Commands:
;;;   C-x C-e    lisp-eval-selection
;;;              evals expression in region, at point or after point.
;;;   C-x C-h    lisp-eval-buffer
;;;              evals whole buffer
;;;   C-x C-m    lisp-macroexpand-selection
;;;              macroexpands region OR expr at point OR before point.
;;;   C-x C-.    lisp-abort-error
;;;   C-x C-a    lisp-arglist
;;;   Tab        indent-selection
;;;              indents region or single line containing point.
;;;   Backspace  deletes region or char before point.
;;;   M-space    delete-whitespace
;;;              deletes all whitespace from point to next expr.
;;;
;;; To install:
;;; 1. Add a line to your .emacs or custom.el file to load this file
;;;    and then set the 'inferior-lisp' variable to whatever command
;;;    string you use to start up lisp. The command string may include
;;;    options for the inferior lisp process when it starts up.
;;;      (load "/<dir>/listener.el")
;;;      (setq inferior-lisp-program "/<dir>/<lispcmd>")
;;; 2. Edit a lisp file or type "M-x lisp-mode" and then start the Lisp
;;;    Listener by selecting "Lisp Listener" from the Lisp menu or 
;;;    typing: C-x l

(defvar xemacs?
  ;; For dealing with differences between emacs/xemacs
  (string-match "XEmacs" emacs-version))

(load "cl-indent")
(setq lisp-indent-function 'common-lisp-indent-function)
(require 'inf-lisp)

(defvar listener-deleted nil)
(defvar listener-own-frame t)
(defvar listener-autoscroll t)

(defvar listener-properties
  (if xemacs?
      '(name "Listener"
        listener-face (:size "12pt" :family "Fixed") ; :background "gray90"
	modeline t
	height 24
	width 75
	unsplittable t)
      '((name . "Listener")
	(title . "Listener")
	;(background-color . "gray90")
	(modeline . t)
	(height . 24)
	(width . 75)
	(unsplittable . t)
	)))
    
(defun install-lisp-mode-commands ()
  ;; dont indent with Tab or CLISP has problems.
  (setq-default indent-tabs-mode nil)
  ;; use pending delete mode for region editing
  (pending-delete-mode 1)
  (define-key lisp-mode-map "\C-x\C-e" 'lisp-eval-selection)
  (define-key lisp-mode-map "\C-x\C-m" 'lisp-macroexpand-selection)
  (define-key lisp-mode-map "\C-x\C-h" 'lisp-eval-buffer)
  (define-key lisp-mode-map "\C-x\C-a" 'lisp-arglist)
  (define-key lisp-mode-map [(control ?x) (control ?.)]
    'lisp-abort-error)
  (define-key lisp-mode-map "\M- " 'delete-whitespace)
  ;;(setq mouse-highlight-text 'symbol)
  ;;(setq indent-line-function 'indent-selection)
  ;; reset TAB to work on Line or Region
  (define-key lisp-mode-map [?\t] 'indent-selection)
  (global-set-key "\C-x\l" 'lisp-listener)  ; cm.el
  )

(defun install-listener-commands ()
  (define-key inferior-lisp-mode-map "\C-x\C-f"
    'listener-open-file)
  (define-key inferior-lisp-mode-map "\C-xb"
    'listener-select-buffer)
  (define-key inferior-lisp-mode-map [?\b] ; [backspace]
    'listener-delete-backward-char)	    
  (define-key inferior-lisp-mode-map "\C-b"
    'listener-backward-char-command)
  (define-key inferior-lisp-mode-map "\C-a"
    'listener-beginning-of-line)
  (define-key inferior-lisp-mode-map "\C-m" 'lisp-enter-input))

(define-key inferior-lisp-mode-map "\C-x\C-e" 'lisp-eval-selection)
(define-key inferior-lisp-mode-map "\C-x\C-m" 'lisp-macroexpand-selection)
(define-key inferior-lisp-mode-map "\C-x\C-h" 'lisp-eval-buffer)
(define-key inferior-lisp-mode-map "\C-x\C-a" 'lisp-arglist)
(define-key inferior-lisp-mode-map [(control ?x) (control ?.)]
  'lisp-abort-error)

(defvar listener-frame nil)

;;;
;;; Lisp menu -- active in all Lisp-Mode buffers.
;;;

(when (not xemacs?)
  (defun region-exists-p ()
    (and mark-active ; simple.el
	 (not (null (mark))))))

(defvar listener-menu nil)
(defvar listener-menu-items
  '("Listener"
    ["Lisp Listener" lisp-listener]
    ["Quit Listener" listener-quit :active (inferior-lisp-p)]
    "---"
    ["Eval Selection" lisp-eval-selection :keys "C-x C-e"
     :active (inferior-lisp-p) ]
    ["Eval Buffer" lisp-eval-buffer :keys "C-x C-h"
     :active (inferior-lisp-p)]
    ["Macroexpand" lisp-macroexpand-selection :keys "C-x C-m"
     :active (inferior-lisp-p)]
    ["Abort Error" lisp-abort-error :keys "C-x C-."
     :active (inferior-lisp-p) ] ; (listener-cltl-p)
    "---"
    ["Indent" indent-selection]
    ["Comment Region" comment-region :active (region-exists-p)]
    ["Remove Comments" uncomment-region :active (region-exists-p)]
    "---"
    ("Tools"
     ["Funargs..." lisp-arglist :keys "C-x C-a"
      :active (inferior-lisp-p) ] ;(listener-cltl-p)
     ["Apropos..." lisp-apropos
      :active (inferior-lisp-p)]
     ["Trace..." lisp-trace
      :active (inferior-lisp-p)]
     ["Describe..." lisp-describe
      :active (inferior-lisp-p)] ;(listener-cltl-p)
     ["Untrace All" lisp-untrace
      :active (inferior-lisp-p)]
     )
    ))

(defvar listener-file-menu nil)
(defvar listener-file-menu-items
  '("File"
    ["New" listener-new-file]
    ["Open..." listener-open-file]
    "---"
    ["Load..." listener-load-file]
;    ["Compile..." listener-compile-file :active (listener-cltl-p)]
    "---"
    ["Quit Listener" listener-quit :active (inferior-lisp-p)]))


;;;
;;; This defines the Listener menu for Lisp-mode
;;;

(easy-menu-define listener-menu lisp-mode-map 
  "Listener Menu" (purecopy listener-menu-items))

(defun listener-mode-hook ()
  ; XEMACS (a noop in emacs)
  (easy-menu-add listener-menu lisp-mode-map)
  (install-lisp-mode-commands))

;;;
;;; this adds our the Listener menu to lisp-mode buffers
;;;

(add-hook 'lisp-mode-hook 'listener-mode-hook)

;;;

;;; Menubar twiddling. This has to be handled differently for Emacs
;;; and Xemacs.
;;; Listener menus:  File   Edit  Buffers
;;;

(when (not xemacs?)
  ;; Sigh. Emacs' menubar is really crappy and makes you work with 
  ;; keymaps. Remove all menus so the Listener can start from scratch.
  ;; lookup-key returns (keymap . ({menu} ...)  ...)
  (let ((all (append (cdr (lookup-key inferior-lisp-mode-map [menu-bar]))
		     (cdr (lookup-key global-map [menu-bar]))
		     )))
    (dolist (m all)
      (define-key inferior-lisp-mode-map (vector 'menu-bar (car m))
	'undefined))))

(when (not xemacs?)
  ;; In Emacs the listener's menus are installed in reverse order using
  ;; define-key because I could not get define-key-after to work!

  (define-key inferior-lisp-mode-map [menu-bar file-buffers]
    (cons "Buffers" (make-sparse-keymap "Buffers")))

  ;; add the system Edit menu after Listener
  (define-key inferior-lisp-mode-map [menu-bar listener-edit]
    (cons "Edit" (lookup-key global-map [menu-bar edit])))

  (defun buffer-menu-select-buffer ()
    ;; call C-Xb with buffer name from menu
    (interactive)
    (listener-select-buffer last-command-event))

  (defun set-buffer-menu-items ()
    ;; this mess is pilfered from menu-bar.el
    ;; /usr/share/emacs/21.3.50/lisp/menu-bar.el
    (when (and (eq (selected-frame) listener-frame))
      (let ((items nil))
        (dolist (b (buffer-list))
          ;; we only collect buffers for files
          (when (buffer-file-name b)
            (push (nconc (list (buffer-name b)
                               (buffer-name b)
                               (cons nil nil))
                         'buffer-menu-select-buffer)
                  items)))
        (define-key inferior-lisp-mode-map [menu-bar file-buffers]
          (cons "Buffers" (cons 'keymap items)))))
    nil)

  ;; add hook to set current list of buffers.
  (add-hook 'menu-bar-update-hook 'set-buffer-menu-items)
  )

;;; Use easy-menu-define to define the  Listener's File menu for both
;;; Emacs and Xemacs. It has to placed here _after_ all the other Emacs
;;; menus have been created so that its the left-most menu in the menubar.

(easy-menu-define listener-file-menu inferior-lisp-mode-map 
  "File Menu" (purecopy listener-file-menu-items))

(when xemacs?
  (defvar listener-edit-menu (car (cdr default-menubar)))
  (defvar listener-buffers-menu nil)
  (defvar listener-buffers-items
    '("Buffers" :filter buffers-menu-filter))
  (easy-menu-define listener-buffers-menu nil ""
                    (purecopy listener-buffers-items))
  (defvar listener-menubar
    (list listener-file-menu
          listener-edit-menu
          listener-buffers-menu))
  )

;;;
;;; Lisp Menu Commands
;;;

(defun listener-input-p ()
  (and comint-input-ring (> (ring-length comint-input-ring) 0)))

(defun lisp-listener (cmd)
  (interactive (list (or inferior-lisp-program
                         (read-string "Command to start Lisp: "))))
  (let ((this (current-buffer)))
    (setq listener-deleted nil) ; clear deleted flag
    ;; create lisp process if lisp-buffer not active
    (if (not (inferior-lisp-p))
	(progn
	  ;; file/buffer commands redirect to other frame
	  (install-listener-commands)
          ;; ----- dont call (inferior-lisp cmd) so we can
          ;; use parse-command-string in place of inf-lisp's
          ;; totally broken split-string parsing.
          (if (not (comint-check-proc "*inferior-lisp*"))
            (let ((cmdlist (parse-command-string cmd 0 (length cmd) '())))
              (set-buffer (apply (function make-comint)
                                 "inferior-lisp"
                                 (car cmdlist) 
                                 nil 
                                 (cdr cmdlist)))
              (inferior-lisp-mode)))
          (setq inferior-lisp-buffer "*inferior-lisp*")
          (pop-to-buffer "*inferior-lisp*")
          ;;-----end inferior lisp
	  (if (and xemacs? listener-own-frame listener-menubar)
	    (set-buffer-menubar listener-menubar))
	  ;; for some reason we dont get our frame title unless the 
	  ;; default format is changed.
	  (make-local-variable 'frame-title-format)
	  (setq frame-title-format "%S")
	  (make-local-variable 'buffers-menu-switch-to-buffer-function)
	  (make-local-variable 'buffers-menu-omit-function)))
    ;; if we are running in a console then there cannot be a
    ;; seperate Lisp Listener frame even if user says so.
    (if (not window-system)
      (setq listener-own-frame nil))

    (if (and listener-own-frame
	     (or (not listener-frame)
		 (not (frame-live-p listener-frame))))
      (progn
        ;; Claim scratch buffer for Lisp if its empty
        ;; this happens when xcm launches. 
        (let ((scratch (get-buffer "*scratch*")))
          (if scratch
            (if (not (buffer-modified-p scratch))
              (with-current-buffer scratch
                (lisp-mode)))))

	(with-current-buffer inferior-lisp-buffer
	  ;; Emacs wants an alist, Xemacs wants a plist...
	  ;(setq default-frame-alist (purecopy listener-properties))
	  (setq listener-frame (new-frame listener-properties)))

	;; mark Listener's buffer and window as dedicated.
	(setq buffers-menu-switch-to-buffer-function
	      'listener-select-buffer)
	(setq buffers-menu-omit-function
	      'listener-omit-buffers)
	(switch-to-buffer this)))
    (if listener-own-frame
      (progn
	(make-frame-visible listener-frame)
	(raise-frame listener-frame)
	(select-frame listener-frame))
      (pop-to-buffer inferior-lisp-buffer))
    t)
  (if (inferior-lisp-p)
    (setq inferior-lisp-program cmd))
  (message nil))

(defun parse-command-string (str beg end sofar)
  ;; inf-lisp's command string parsing  breaks on pathnames 
  ;; with spaces. this routine parses a lisp command string 
  ;; according to unix "-arg" syntax
  (let ((wspace '(?\  ?\t ?\r ?\n)))
    ;; skip white space at start of string
    (while (and (< beg end) 
                (member (elt str beg) wspace ))
      (setq beg (+ beg 1)))
    ;; beg is either at end or at first non-space
    (if (= beg end)
      ;; only space till end of string
      sofar
      ;; beg is now on non-space, look for next arg
      ;; it might start right away or after intervening data
      (let ((pos (if (char-equal (elt str beg) ?-)
                   beg
                   (or (string-match " -" str beg)
                       end))))
        (cond ((= pos end)
               ;; no arg found, trim space at end of data
               ;; and list of args with data at end
               (let ((pos (- end 1)))
                 (while (and (> pos beg)
                             (member (elt str pos) wspace))
                   (setq pos (- pos 1)))
                 (append sofar (list (substring str beg (+ pos 1))))))
            ((= pos beg)
             ;; if pos=beg then we are on the start of an arg
             ;; find the next white space and recurse with
             ;; our arg added at end of list
             (while (and (< pos end)
                         (not (member (elt str pos) wspace)))
               (setq pos (+ pos 1)))
             (parse-command-string
              str pos end 
              (append sofar (list (substring str beg pos)))))
            (t
             ;; otherwise if pos>beg then we have data before pos
             ;; trim back pos to end of data and recurse with arg
             ;; at start of next arg in string
             (let ((loc pos))
               (while (and (> loc beg)
                           (member (elt str loc) wspace))
                 (setq loc (- loc 1)))
               ;; now loc on first space after end of data
               (setq loc (+ loc 1))
               (parse-command-string
                str pos end 
                (append sofar (list (substring str beg loc)))))))))))

; (defun pa (s) (parse-command-string s 0 (length s) '()))
; (pa "")
; (pa "    ")
; (pa "foo/bar baz/buz")
; (pa "  foo/bar baz/buz")
; (pa "  foo/bar baz/buz -i   ")
; (pa "clisp -i")
; (pa "/program files/clisp -i")
; (pa " c:/program files/clisp -i  ")
; (pa " c:/program files/clisp -i   -M a/b c/de -K")
; (pa "c:\\Program Files\\clisp-2.31\\full\\lisp.exe -I -M \\Program Files\\Common Music\\bin\\cm.mem -i c:\\Program Files\\Common Music\\bin\\cminit.lisp")
; (pa "c:/Program Files/clisp-2.31/full/lisp.exe -I -M /Program Files/Common Music/bin/cm.mem -i c:/Program Files/Common Music/bin/cminit.lisp")

(defun lisp-eval-selection ()
  "Evaluate expr at/before point or in region."
  (interactive )
  (let ((ext (sexp-extent)))
    (if (consp ext)
	(progn
	  (lisp-eval-region (car ext) (cdr ext))
	  (goto-pmark)))))

(defun lisp-eval-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (mark-whole-buffer)
  (lisp-eval-region (point) (mark)))

(defun lisp-macroexpand-selection ()
  "Macroexpand expr at/before point or in region."
  (interactive)
  (let ((ext (sexp-extent)))
    (if (consp ext)
      (progn
	(comint-send-string
	 (inferior-lisp-proc)
	 (format "(pprint (macroexpand '%s))\n"
		 (buffer-substring (car ext) (cdr ext)))))
      (goto-pmark))))

(when (not xemacs?)
  ;; why isnt this defun in effect in Emacs?
  (defun lisp-indent-region (start end)
    "Indent every line whose first char is between START and END inclusive."
    (save-excursion
      (let ((endmark (copy-marker end)))
	(goto-char start)
	(and (bolp) (not (eolp))
	     (lisp-indent-line))
	(indent-sexp endmark)
	(set-marker endmark nil))))
  )

(defun indent-selection ()
  ;; indent single line or region<line
  (interactive)
  (if (and (region-exists-p)
	   (> (count-lines (region-beginning) (region-end)) 1))
    (lisp-indent-region (region-beginning) (region-end))
    (lisp-indent-line)))

(defun delete-whitespace ()
  (interactive)
  (save-excursion
    (let ((beg (point))
	  (end (skip-syntax-forward " >")))
      (if (> end 0)
	(delete-region beg (+ beg end))))))

(defun uncomment-region ()
  (interactive)
  (comment-region (region-beginning) (region-end) '(t)))

(defun lisp-arglist (fn)
  "Print Lisp function arguments."
  ;; taken from inf-lisp.
  (interactive (lisp-symprompt "Show arguments of"
			       (lisp-fn-called-at-pt)))
  (comint-send-string
   (inferior-lisp-proc)
   (format "(let ((sym '%s))
              (format t %c~%%~(~A: ~A~)%c sym
                #+MCL (ccl:arglist sym)
                #+EXCL (excl:arglist sym)
                #+CLISP (ext:arglist sym)
                #+CMU (pcl::function-arglist sym)
                #+SBCL (sb-kernel::%%simple-fun-arglist sym))
               (values))\n"
	   fn ?\" ?\"))
   (goto-pmark))

(defun lisp-apropos (sym)
  "Print Apropos inforation about symbol."
  (interactive (lisp-symprompt "Apropos"
			       (selected-symbol)))
  (comint-send-string (inferior-lisp-proc)
		      (if t ;(listener-implementation-p 'guile)
                        (format "(apropos %S)\n" sym)
                        ;(format "(apropos '%s)\n" sym)
			))
  (goto-pmark))

(defun lisp-trace (fn)
  "Trace function."
  (interactive (lisp-symprompt "Trace function"
			       (lisp-fn-called-at-pt)))

  (comint-send-string (inferior-lisp-proc)
		      (format "(trace %s)\n" fn))
  (goto-pmark))

(defun lisp-untrace ()
  "Untrace all functions."
  (interactive)
  (comint-send-string (inferior-lisp-proc) "(untrace)\n")
  (goto-pmark))

(defun lisp-describe (sym)
  "Trace function."
  (interactive (lisp-symprompt "Describe variable"
			       (selected-symbol)))
  (comint-send-string (inferior-lisp-proc)
		      (format "(describe %s)\n" sym))
  (goto-pmark))

(defun lisp-interrupt ()
  (interactive)
  (with-current-buffer inferior-lisp-buffer
    (comint-interrupt-subjob))
  (goto-pmark))

(defun lisp-abort-error ()
  "Abort Listener error."
  (interactive)
  (comint-send-string
   inferior-lisp-buffer
   "(progn (terpri)
     #+MCL (throw :toplevel nil)
     #+EXCL (tpl::reset-command)
     #+CLISP (system::debug-unwind)
     #+CMU (invoke-restart (first (last (compute-restarts))))
     #+SBCL (let ((top (find-restart 'sb-impl::toplevel)))
              (invoke-restart top)))\n")
  (goto-pmark))

(defun listener-quit ()
  ;; If listener is the only frame and there
  ;; are no other frames then quit xemacs.
  ;; there are two ways to kill the listener: close the frame or
  ;; kill the buffer. both tasks must be performed if either is
  ;; invoked. so we define two hooks: 1) close the frame if buffer
  ;; is killed and 2) kill the buffer if the frame is closed. since
  ;; these hooks call each other they both set and test a flag 
  ;; (the "deleted" frame property) to avoid deadlock.
  (interactive)
  (if (and (frame-live-p listener-frame)
           (not (cdr (frame-list))))
    (kill-emacs 0)
    (if (inferior-lisp-p)
      (kill-buffer inferior-lisp-buffer))))

(defun kill-listener-1 ()
  ;; hook called by kill-buffer. 
  (if (and inferior-lisp-buffer
	   (eq (current-buffer)
	       (get-buffer inferior-lisp-buffer))
	   (frame-live-p listener-frame)
	   (not listener-deleted ))
      (progn
	;(set-frame-property listener-frame 'deleted t)
        (setq listener-deleted t)
	(delete-frame listener-frame)
	(setq listener-frame nil))))

(defun kill-listener-2 (f)
  ;; hook called by delete-frame
  (if (and (eq f listener-frame)
	   (not listener-deleted)
	   (inferior-lisp-p))
      (progn
        (setq listener-deleted t)
	;(set-frame-property f 'deleted t)
	(kill-buffer inferior-lisp-buffer))))

(add-hook 'kill-buffer-hook 'kill-listener-1)
(add-hook 'delete-frame-hook 'kill-listener-2)

;;;
;;; Listener commands.
;;;

(defun listener-new-file ()
  (interactive)
  (let ((buff (create-file-buffer "temp.lisp")))
    (with-current-buffer buff
      (lisp-mode)
      (new-frame))))

(defun listener-open-file (file)
  (interactive "FOpen file: ")
  (let ((other (previous-frame listener-frame nil)))
    ;; if there already is another frame, use it
    (if other
      (progn
	(select-frame other)
	(raise-frame other)
	(find-file file))
      (find-file-other-frame file))))

(defun listener-select-buffer (buf)
  (interactive "BSelect buffer: ")
  (let ((other (previous-frame listener-frame nil)))
    (if other
	(progn
	  (select-frame other)
	  (raise-frame other)
	  (switch-to-buffer buf)
	  )
      (switch-to-buffer-other-frame buf))))

(defun unixify (file)
  (let ((p 0)
        (l (length file)))
    (while (< p l)
      (if (char-equal (aref file p) ?\\)
        (aset file p ?/))
      (setq p (+ p 1)))
    file))

(defun listener-load-file (file)
  (interactive "FLoad file: ")
  ;; cant pass a string with dos directory chars even if its escaped.
  (when (eq system-type 'windows-nt)
    (setq file (unixify file)))
  (comint-send-string inferior-lisp-buffer
                      (format "(load \"%s\")\n" 
                              file)))

(defun listener-compile-file (file)
  (interactive "FCompile file: ")
  (when (eq system-type 'windows-nt)
    (setq file (unixify file)))
  (comint-send-string inferior-lisp-buffer
                      (format "(compile-file \"%s\")\n"
                              file)))




(defun listener-omit-buffers (b)
  (or (buffers-menu-omit-invisible-buffers b)
      (eq b (get-buffer inferior-lisp-buffer))))

;;;
;;; Prompt line enhancements. By default comint doesnt protect 
;;; the prompt from cursor editing commands very well. It also
;;; defines Return to immediately send the current line as 
;;; input even if the input expression isn't balanced, which
;;; emeans that multi-line exprs can only be trivially editied.
;;;

(defun lisp-enter-input ()
  ;; comint calls comint-send-input whenever Return is entered.
  ;; but this isn't correct if the user is entering a multi-line
  ;; lisp expression.
  (interactive)
  (let ((pmark (pmark-pos))
	(point (point)))
    (if (> point pmark)
      (if (balanced-input-p pmark (point-max))
	(comint-send-input)
	(newline))
      (comint-send-input))))

(defmacro with-input-prompt-protected (&rest forms)
  `(if (backward-ok-p) 
    (progn ,@forms)
    (beep)))

(defun listener-delete-backward-char ()
  (interactive)
  (with-input-prompt-protected
      (delete-backward-char 1)))

(defun listener-backward-char-command ()
  (interactive)
  (with-input-prompt-protected
      (backward-char)))

(defun listener-beginning-of-line ()
  (interactive)
  (let ((flag (backward-ok-p)))
    (if (eq flag 'before)
      (beginning-of-line)
      (if (eq flag 'after)
	(goto-char (pmark-pos))
	(beep)))))

;;;
;;; support code
;;;

(defun scan-sexps-noerr (a b)
  (condition-case nil
      (scan-sexps a b)
    (error nil)))

(defun balanced-input-p (pos end)
  (let ((flag t))
    (save-excursion
      (goto-char pos)
      ;; skip whitespace and newlines
      (setq pos (+ pos (skip-syntax-forward " >")))
      (while (and flag (< pos end))
	;(setq pos (scan-sexps pos 1 lisp t)
	(setq pos (scan-sexps-noerr pos 1))
	(if (null pos)
	  (setq flag nil)
	  (progn
	    (goto-char pos)
	    (setq pos (+ pos (skip-syntax-forward " >")))
	    ))))
    flag))

(defun backward-ok-p ()
  ;; true if point is after process mark or before the beginning 
  ;; of that line. this doenst depend on regexp matching to find
  ;; a prompt but assumes the start of the line to the process
  ;; mark contains the lisp prompt.
  (let ((end (pmark-pos))
	(pos (point)))
    (if (> pos end)
      'after
      (save-excursion
	(goto-char end)
	(if (< pos (point-at-bol))
	  'before
	  nil)))))

(defun listener-frame ()
  (if (and listener-own-frame
	   (frame-live-p listener-frame))
    listener-frame
    (selected-frame)))
    
(defun goto-pmark ()
  ;; scroll to the process mark at the end of the inferior-lisp-buffer
  ;; so that output is visible.  for some reason the lisp frame
  ;; has to be selected for this to work...
  (if listener-autoscroll
    (let ((lisp (listener-frame))
	  (this (selected-frame)))
      (unless (eq this lisp)
	(select-frame listener-frame))
      (with-current-buffer inferior-lisp-buffer
	(goto-char (point-max)))
      (unless (eq this lisp)
	(select-frame this)))))

(defun pmark-pos ()
  (marker-position (process-mark (inferior-lisp-proc))))

(defun inferior-lisp-p ()
  (comint-check-proc inferior-lisp-buffer))

(defun listening-p ()
  ;; true iff this function is called from the inferior lisp 
  ;; buffer in the lisp listener. returns false if it is called
  ;; from an inferior lisp buffer running anywhere else. used
  ;; to decide if the command should use other frame or not.
  (and (frame-live-p listener-frame)
       (eq (frame-root-window listener-frame)
	   (get-buffer-window (current-buffer)))))

(defun sexp-extent ()
  (if (region-exists-p)
    (cons (region-beginning) (region-end))
    (point-sexp)))

(defun point-sexp ()
  "Return extent of sexp at point. point can be at start or end 
   of sexp or inside a symbol."
  (let ((wspace '(?\  ?\t ?\r ?\n))
	(left-char (char-before))
	(right-char (char-after))
	left-side right-side)
    (setq left-side
	  (if (or (not left-char)
		  (member left-char wspace)
		  (member left-char '(?\( )))
	      (point)
	      (save-excursion
		(backward-sexp)
		(point))))
    (setq right-side
	  (if (or (not right-char)
		  (member right-char wspace)
		  (member right-char '(?\) ))
		  ;; dont look ahead if different sexp leftward
		  (and (< left-side (point))
		       (char-equal left-char ?\))))
	      (point)
	      (save-excursion
		(forward-sexp)
		(point))))
    (if (equal left-side right-side)
      nil
      (cons left-side right-side))))

(defun selected-symbol ()
  (let ((ext (sexp-extent)))
    (and (consp ext)
	 (let ((s (buffer-substring (car ext) (cdr ext))))
	   (and (not (string= s ""))
		(symbolp (setq s (read s)))
		s)))))

(defun set-listener-bgcolor (color)
  (interactive (list (read-color "Color: ")))
  (unless (string= color "")
    (set-face-background 'default color listener-frame)))
                       
;(defun install-mcl-key-bindings (map)
;  (define-key map '(alt x) 'kill-primary-selection)   ; cut
;  (define-key map '(alt c) 'copy-primary-selection)   ; copy
;  (define-key map '(alt v) 'yank-clipboard-selection) ; paste
;  (define-key map '(alt a) 'mark-whole-buffer)        ; select all      
;  (define-key map '(alt z) 'undo)                     ; undo
;  (define-key map '(alt l) 'lisp-listener)            ; get lisp listener
;  (define-key map '(alt e) 'lisp-eval-selection)      ; eval 
;  (define-key map 'kp-enter 'lisp-eval-selection)     ; eval 
;  (define-key map '(alt h) 'lisp-eval-buffer)         ; eval whole buffer
;  )

(provide 'listener)


