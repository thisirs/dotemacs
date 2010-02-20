;;$Revision: 1.32 $
;;scilab.el - SCILAB INTEGRATED SHELL, EDITING AND  HELP in GNU-Emacs/Xemacs 
;;
;; Copyright (C) 2001 Alexander Vigodner

;; Created: 03.01 2001
;; Version: 2.1.7
;; Author: Alexander Vigodner <avigodner@bloomberg.com>>

;;This program is free software; you can redistribute it and/or
;;modify it under the terms of the GNU General Public License
;;as published by the Free Software Foundation; either version 2
;;of the License, or (at your option) any later version.

;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with this program; if not, write to the Free Software
;;Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;B.F.M. Financial Research, Ltd., hereby disclaims all copyright
;;interest in the program `scilab.el' written by Alexander Vigodner

;;Eric Berger, 17 July 2001
;;Managing Director

;;
;; This program was written by Alexander Vigodner <avigodner@bloomberg.com>
;;  on the base of  matlab.el file, Version: 2.2.2 (
;;  Copyright (C) 1997-1999 Eric M. Ludlam <eludlam@mathworks.com>
;;  Copyright (C) 1991-1997 Matthew R. Wette <mwette@alumni.caltech.edu> )

;;
;;
;;
;;; Commentary:
;; File contains three integrated together modes:
;; SCILAB-MODE is  the  major mode for GNU Emacs.
;; It  provides support for editing SCILAB dot-sci/sce
;; files.  It automatically indents for block structures, line continuations
;; (e.g., ...), and comments.
;;
;; Additional features include auto-fill including auto-additions of
;; ellipsis for commands, and even strings.  Block/end construct
;; highlighting as you edit.  Primitive code-verification and
;; identification.  Templates and other code editing functions.
;; Advanced symbol completion.  Code highlighting via font-lock.
;; There are many navigation commands that let you move across blocks
;; of code at different levels.
;;
;; SCILAB-SHELL mode for launching scilab with -nw. It has much more 
;; features than the standard x-mode of scilab. There are only two minor 
;; points:
;; 1) Graphic commands change a behaviour of the prompt. See my bug report.
;;;  The bug exists in all versions of scilab till 2.6 (included).
;; 2) Scicos does not run. 
;;  
;;;   For a solution one can ask me for the patch for scilab-2.6
;;;  Or take it from my  page
;;; http://www.geocities.com/avezunchik/scilab.html

;;
;; SCILAB-HELP   fully clickable help system  with the
;; the same set of the man pages as the basic scilab. Works independely on 
;; the scilab-shell mode, but you can send commands from the man page to the
;; scilab-shell

;;
;;; Installation:
;;
;;  1.  Put the this file as "scilab.el" somewhere on the load-path.
;;  2.  Find file scilab-startup.el. If it does not exists then copy the lines below
;;  into this file and place this file into the same directory as scilab.el.
;;  3.  In your init file put the command
;;     (load "scilab-startup")
;;  4.Remark: instead of loading you can just insert the file "scilab-startup.el" 
;;  5. When you open emacs/xemacs in "Tools" you will see "Scilab Setup". Run it
;;    and carefully customize  all main variables of scilab/emacs system 
;;  6. Enjoy 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;START OF FILE SCILAB-STARTUP.EL;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JUST REMOVE ;;;; 
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ;;; This file must be inserted or loaded into users' init emacs file (.emacs usually).
;;;; ;;; load-path variable must contain the directory where scilab.el and scilab-startup.el;;; files are placed 
;;;; ;;; For instance the following two command can be added to the end of the init-emacs
;;;; ;;; File
;;;; ;;;
;;;; ;;(setq load-path  (append (list "<YOUR DIRECTORY>" ) load-path))
;;;; ;;(load "scilab-startup")

;;;;;;;;;;;;  START  OF SCILAB STUFFS FOR .EMACS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setq load-path (append (list (expand-file-name "./") ) load-path)) 
;;;; (defvar running-xemacs (string-match "XEmacs\\|Lucid" (emacs-version)))
;;;; (let* (
;;;;       (sciel (locate-library "scilab.el"))
;;;;       (scilec (concat sciel "c"))
;;;;       (scilab-elc-xemacs running-xemacs)
;;;;      )

;;;;   (if (not (file-newer-than-file-p scilec sciel))
;;;;         (byte-compile-file sciel)
;;;;         (find-file scilec) 
;;;;         (goto-line 4)
;;;;         (setq scilab-elc-xemacs (looking-at ".*\\(XEmacs\\|Lucid\\)"))
;;;;         (kill-buffer "scilab.elc")
;;;;         (if (not (eq scilab-elc-xemacs running-xemacs))
;;;; 	    (byte-compile-file sciel))
;;;;       )
;;;; )
;;;;    (autoload 'scilab-mode "scilab" "Enter Scilab editing mode." t)
;;;;    (setq auto-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\)" . scilab-mode) 
;;;;        auto-mode-alist))
;;;;    (autoload 'scilab-shell "scilab" "Interactive Scilab Shell mode." t)      
;;;;    (autoload 'scilab-mode-setup "scilab" "Scilab modes Setup." t)      
;;;;    (autoload 'scilab-help "scilab" "Scilab Topic Browser." t)      
;;;;    (autoload 'scilab-help-function "scilab" "Scilab Help Function." t)
;;;;    (autoload 'scilab-apropos-function "scilab" "Scilab Apropos Function." t)

;;;; (defun my-scilab-mode-hook ()
;;;;   (if running-gnuemacs (show-paren-mode))
;;;;      (setq fill-column 76))		; where auto-fill should wrap
;;;; (defun my-scilab-shell-mode-hook () 
;;;; (if running-gnuemacs (show-paren-mode))
;;;; )
;;;; (add-hook 'scilab-mode-hook 'my-scilab-mode-hook)
;;;; (add-hook 'scilab-shell-mode-hook 'my-scilab-shell-mode-hook)

;;;; (defcustom scilab-shell-global-key "\C-cs"
;;;;   "Global key for `scilab-shell' command \"^C\" means Ctrl-c, \"^X\" 
;;;; means Ctrl-x,etc" 
;;;;   :group 'scilab-shell
;;;;   :group 'scilab-setup
;;;;   :type 'string)

;;;; (global-set-key  scilab-shell-global-key 'scilab-shell)

;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ;;;;To display start and setup menu of scilab in "Tools" menu (not necessary)
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (if running-xemacs
;;;;   (progn
;;;; 	(add-menu-button '("Tools") "---" )
;;;; 	(add-menu-button '("Tools") ["Scilab Start" scilab-shell t] )
;;;; 	(add-menu-button '("Tools") ["Scilab Setup" scilab-mode-setup t] )
;;;; 	(add-menu-button '("Help") ["Scilab Topic" scilab-help t] )
;;;; 	(add-menu-button '("Help") ["Scilab Help" scilab-help-function t] )
;;;; 	(add-menu-button '("Help") ["Scilab Apropos" scilab-apropos-function t] )
;;;;   )
;;;;      (define-key menu-bar-tools-menu [separator-scilab]
;;;;     '("--"))
;;;;      (define-key menu-bar-tools-menu [scilab-start] '("Scilab Start"  . scilab-shell))
;;;;      (define-key menu-bar-tools-menu [scilab-setup] '("Scilab Setup"  . scilab-mode-setup))

;;;;     (define-key menu-bar-help-menu [separator-scilab]
;;;;     '("--"))
;;;;     (define-key menu-bar-help-menu [scilab-apropos] '("Scilab Apropos"  . scilab-apropos-function))
;;;;      (define-key menu-bar-help-menu [scilab-help] '("Scilab Help"  . scilab-help-function))
;;;;      (define-key menu-bar-help-menu [scilab-topic] '("Scilab Topic"  . scilab-help))
;;;; )
;;;; ;;;;;;;;;;  END OF SCILAB STUFFS FOR .EMACS;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF FILE SCILAB-STARTUP.EL;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Please read the mode help for scilab-mode for additional
;; configuration options.
;;

;; This package requires easymenu, tempo, and derived.
;; This package will optionally use custom, font-lock, hilit19, shell, and gud.

;;;;;;;;;;;;;;;;;;;;;;;;;;; START of THE BODY ;;;;;;;;;;;;;;;;;;;;

;; Which emacs are we running?
(defvar running-xemacs (string-match "X[Ee]macs\\|Lucid" (emacs-version)))
(defvar running-gnuemacs (string-match "GNU" (emacs-version)))
(string-match "\\(GNU\\|X\\)\\s-*[Ee]macs\\s-+\\([0-9.]+\\)\\s-+" (emacs-version))
(defvar version-emacs (match-string 2  (emacs-version)))
;;; Code:

(require 'easymenu)
(require 'tempo)
(require 'derived)

(defconst scilab-mode-version "2.1.7"
  "Current version of Scilab mode.")
(defvar scilab-mode-all-versions nil
  "Current status of Scilab mode, Emacs/Xemacs and existence of client.")

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro custom-add-option (&rest args)
      nil)
    (defmacro defface (&rest args) nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

;; compatibility
(if running-xemacs
    (progn
      (defalias 'scilab-make-overlay 'make-extent)
      (defalias 'scilab-overlay-put 'set-extent-property)
      (defalias 'scilab-delete-overlay 'delete-extent)
      (defalias 'scilab-overlay-start 'extent-start)
      (defalias 'scilab-overlay-end 'extent-end)
      (defalias 'scilab-cancel-timer 'delete-itimer)
      (defalias 'scilab-run-with-timer 'run-with-timer)
      (require 'cus-face)
      (defalias 'scilab-face-get-attribute  'face-custom-attributes-get)
      (defun scilab-run-with-idle-timer (secs repeat function &rest args)
	(condition-case nil
	    (start-itimer "scilab" function secs (if repeat secs nil) t)
	  (error
	   ;; If the above doesn't work, then try this old version of start itimer.
	   (start-itimer "scilab" function secs (if repeat secs nil)))))
      )
  (defalias 'scilab-make-overlay 'make-overlay)
  (defalias 'scilab-overlay-put 'overlay-put)
  (defalias 'scilab-delete-overlay 'delete-overlay)
  (defalias 'scilab-overlay-start 'overlay-start)
  (defalias 'scilab-overlay-end 'overlay-end)
  (defalias 'scilab-cancel-timer 'cancel-timer)
  (defalias 'scilab-run-with-idle-timer 'run-with-idle-timer)
  (defalias 'scilab-run-with-timer 'run-with-timer)
  (defalias 'scilab-face-get-attribute  'face-attr-construct)
  )


(defvar scilab-server-status nil)
;;In standard Emacs server-start is used. However, gnuserv-start
;; has much more features and can be installed also.So: 


(defun scilab-server-start()
"Run gnuserver if it is possible. For Xemacs this is the standard
server. For GNU emacs server is the standard server. However, gnuserver
can be installed for emacs too. In this case scilab-server-start runs
gnuserver for emacs too. In contrast to usual run serevers run only
if scilab is active and then %version variable
is updated"
(interactive)
(if (scilab-shell-active-p)
 (progn
   (if (fboundp 'gnuserv-start)
      (progn 
	(let* ((TMPDIR  
		(substring 
		 (scilab-shell-collect-command-output 
		  "write(%io(2),getenv('TMPDIR'))") 0 -3))
	       (process-environment 
		(nconc (list (concat "TMPDIR=" TMPDIR) 
			     (concat "GNU_PORT=" 
				     (substring TMPDIR -6 -1))) 
		       process-environment) ))
	  (gnuserv-start))
	(setq scilab-server-status 
	      (if (eq (process-status gnuserv-process) 'run) 
		  "gnuclient" nil))
	)
     (if (fboundp 'server-start) 
         (progn  
	 (server-start)
	 (setq scilab-server-status 
	       (if (eq (process-status gnuserv-process) 'run) 
		   "emacsclient" nil))
	 )
       )
     )
   (scilab-shell-versions)
   )
 nil
 )
)


(defun scilab-server-kill()
"Run gnuserver if it is possible. For Xemacs this is the standard
server. For GNU emacs server is the standard server. However, gnuserver
can be installed for emacs too. In this case scilab-server-start runs
gnuserver for emacs too. Besides, if scila bis active then %version variable
is updated"
(interactive)
(if (fboundp 'gnuserv-start) 
    (if (gnuserv-running-p)
     (progn 
      (gnuserv-shutdown)
      (setq scilab-server-status nil)
       ))
  (if (fboundp 'server-start)
      (progn  
	(server-start 1))
    (setq scilab-server-status nil)
   )
)
(if (scilab-shell-active-p)
    (scilab-shell-versions)
)
)

 

(defun scilab-server-status()
"Gives status of scilab-server. Notice that gnuserv can rub while the status
for scilab-server will be \"not run\". To privide proper behaviour of gnuserver
it must be run only via `scilab-server-start'. This is cheked via the variable 
`scilab-server-status' and this function "
(interactive)
(message (if (and (fboundp 'gnuserv-running-p) 
	 (gnuserv-running-p)
         (string-equal scilab-server-status "gnuclient")
         (equal (process-status gnuserv-process) 'run))
     "gnuclient"
   (if (and (boundp 'server-process)
            (string-equal scilab-server-status "emacsclient")
	    (equal (process-status server-process) 'run))
     "emacsclient"
     "No client"))
)
) 
 


(if (fboundp 'global-font-lock-mode)
      (defalias 'scilab-font-lock-mode 'global-font-lock-mode)
 (if (fboundp 'font-lock-mode)    
      (defalias 'scilab-font-lock-mode 'font-lock-mode)
))

(defcustom scilab-font-lock-mode 
(if (boundp 'global-font-lock-mode) 
    global-font-lock-mode 
(if (boundp 'font-lock-mode) 
font-lock-mode nil))
" font-lock-mode for scilab modes. Overrided by global-font-lock mode."
  :group 'scilab
  :group 'scilab-setup
  :type 'boolean)

 (defcustom scilab-highlight-builtin  scilab-font-lock-mode 
 "There is no regular expressions for builtins. The names are taken from the
 custom `scilab-builtin-list'. So it can work slowly. You can disable this option" 
   :group 'scilab
   :group 'scilab-setup
   :type 'boolean)

 (defcustom scilab-highlight-macros  nil
 "There is no regular expressions for macros. The names are taken from the
 dynamically updated file `scilab-libfunc-list-path'. So it can work eben slower
 than for builtins. You can disable this option" 
   :group 'scilab
   :group 'scilab-setup
   :type 'boolean)



(if (fboundp 'point-at-bol)
    (progn
      (defalias 'scilab-point-at-bol 'point-at-bol)
      (defalias 'scilab-point-at-eol 'point-at-eol))
  (defun scilab-point-at-bol ()
    (interactive) (save-excursion (beginning-of-line) (point)))
  (defun scilab-point-at-eol ()
    (interactive) (save-excursion (end-of-line) (point))))


;;; User-changeable variables =================================================

;; Variables which the user can change
(defgroup scilab nil
  "Scilab mode."
  :prefix "scilab-"
  :group 'languages)

(defgroup scilab-edit nil
  "Scilab subgroup containing editing options."
  :group 'scilab)

(defcustom scilab-indent-level 2
  "*The basic indentation amount in `scilab-mode'."
  :group 'scilab-edit
  :type 'integer)
(defcustom scilab-dynamical-indent t
  "Ebable/disable indentation in `scilab-mode'. For big files indentation works slowly.
   Thus a user can switch this property "
  :group 'scilab-edit
  :type 'boolean)

(defcustom scilab-cont-level 4
  "*Basic indentation after continuation if no other methods are found."
  :group 'scilab-edit
  :type 'integer)

(defcustom scilab-case-level '(1 . 1)
  "*How far to indent case/else statements in a select.
This can be an integer, which is the distance to indent the CASE and
ELSE commands, and how far to indent commands appearing in CASE
and ELSE blocks.  It can also be a cons cell which is of form
  (CASEINDENT . COMMANDINDENT)
where CASEINDENT is the indentation of the CASE and OTHERWISE
statements, and COMMANDINDENT is the indentation of commands appearing
after the CASE or ELSE command."
  :group 'scilab-edit
  :type 'sexp)

(defcustom scilab-indent-function t
  "*If non-nil, indent body of function."
  :group 'scilab-edit
  :type 'boolean)

;;;(defcustom scilab-indent-past-arg1-functions
;;;  "input"
;;;  "*Regex describing functions whose first arg is special.
;;;This specialness means that all following parameters which appear on
;;;continued lines should appear indented to line up with the second
;;;argument, not the first argument."
;;;  :group 'scilab
;;;  :type 'string)

;;;(defcustom scilab-arg1-max-indent-length 15
;;;  "*The maximum length to indent when indenting past arg1.
;;;If arg1 is exceptionally long, then only this number of characters
;;;will be indented beyond the open paren starting the parameter list.")

(defcustom scilab-maximum-indents '(;; = is a convenience. Don't go too far
				    (?= . (10 . 4))
				    ;; Fns should provide hard limits
				    (?\( . 50)
				    ;; Matrix/Cell arrays
				    (?\[ . 20)
				    (?\{ . 20))
  "Alist of maximum indentations when lining up code.
Each element is of the form (CHAR . INDENT) where char is a character
the indent engine is using, and INDENT is the maximum indentation
allowed.  Indent could be of the form (MAXIMUM . INDENT), where
MAXIMUM is the maximum allowed calculated indent, and INDENT is the
amount to use if MAXIMUM is reached.")

;(defcustom scilab-handle-scicos nil
(defvar scilab-handle-scicos nil
  "*If true, add in a few scicos customizations.
This variable's state is mostly useful when set at load time when
scicos font lock keywords can be removed.  This will handle
additional cases as the need arrises. It is not implemented yet"
;;  :group 'scilab
;;  :type 'boolean
)

(defcustom scilab-auto-fill t
  "*If true, set variable `auto-fill-function' to our function at startup."
  :group 'scilab-edit
  :type 'boolean)

(defcustom scilab-fill-fudge 10
  "Number of characters around `fill-column' we can fudge filling.
Basically, there are places that are very convenient to fill at, but
might not be the closest fill spot, or occur after `fill-column'.
If they occur within this fudge factor, we will use them.
Also, if none of the above occur, and we find a symbol to break at,
but an open paren (group) starts or ends within this fudge factor,
move there to boost the amount of fill leverage we can get."
  :group 'scilab-edit
  :type 'integer)

(defcustom scilab-fill-fudge-hard-maximum 79
  "The longest line allowed when auto-filling code.
This overcomes situations where the `fill-column' plus the
`scilab-fill-fudge' is greater than some hard desired limit."
  :group 'scilab-edit
  :type 'integer)

(defcustom scilab-elipsis-string "..."
  "Text used to perform continuation on code lines.
This is used to generate and identify continuation lines.")

(defcustom scilab-fill-code t
  "*If true, `auto-fill-mode' causes code lines to be automatically continued."
  :group 'scilab-edit
  :type 'boolean)

(defcustom scilab-fill-count-ellipsis-flag t
  "*Non-nil means to count the ellipsis when auto filling.
This effectively shortens the `fill-column' by the length of
`scilab-elipsis-string'."
  :group 'scilab-edit
  :type 'boolean)


(defcustom scilab-fill-strings-flag t
  "*Non-nil means that when auto-fill is on, strings are broken across lines.
If `scilab-fill-count-ellipsis-flag' is non nil, this shortens the
`fill-column' by the length of `scilab-elipsis-string'."
  :group 'scilab-edit
  :type 'boolean)


(defcustom scilab-comment-column 40
  "*The goal comment column in `scilab-mode' buffers."
  :group 'scilab-edit
  :type 'integer)

(defvar scilab-comment-line-s "// "
  "*String to start comment on line by itself."
;;;  :group 'scilab
;;;  :type 'string)
)
(defvar scilab-comment-on-line-s "// "
  "*String to start comment on line with code."
;;;  :group 'scilab
;;;  :type 'string)
)
(defcustom scilab-comment-region-s "// "
  "*String inserted by \\[scilab-comment-region] at start of each line in \
region."
  :group 'scilab-edit
  :type 'string)

(defcustom scilab-verify-on-save-flag nil
  "*Non-nil means to verify sci whenever we save a file. It is better to put t, if a user want to write programms in 1 file-1 function matlab style"
  :group 'scilab
  :type 'boolean)

(defcustom scilab-mode-verify-fix-functions
  '(
    scilab-mode-vf-block-matches-forward
    scilab-mode-vf-block-matches-backward)
  "List of function symbols which perform a verification and fix to sci code.
Each function gets no arguments, and returns nothing.  They can move
point, but it will be restored for them."
  :group 'scilab
  :type '(repeat (choice :tag "Function: "
			 '(scilab-mode-vf-functionname
			   scilab-mode-vf-block-matches-forward
			   scilab-mode-vf-block-matches-backward
			   scilab-mode-vf-quietafy-buffer
			   ))))

(defcustom scilab-block-verify-max-buffer-size 50000
  "*Largest buffer size allowed for block verification during save."
  :group 'scilab
  :type 'integer)

(defcustom scilab-vers-on-startup t
  "*If non-nil, show the version number on startup."
  :group 'scilab
  :type 'boolean)

(defcustom scilab-highlight-block-match-flag t
  "*Non-nil means to highlight the matching if/end/whatever.
The highlighting only occurs when the cursor is on a block start or end
keyword."
  :group 'scilab-edit
  :type 'boolean)

(defcustom scilab-show-periodic-code-details-flag nil
  "*Non-nil means to show code details in the minibuffer.
This will only work if `scilab-highlight-block-match-flag' is non-nil."
  :group 'scilab-edit
  :type 'boolean)

(defcustom scilab-mode-hook nil 
  "*List of functions to call on entry to Scilab mode."
  :group 'scilab
  :group 'scilab-setup
  :type 'hook)


(defcustom scilab-completion-technique 'complete
  "*How the `scilab-complete-symbol' interfaces with the user.
Valid values are:

'increment - which means that new strings are tried with each
             successive call until all methods are exhausted.
             (Similar to `hippie-expand'.)
'complete  - Which means that if there is no single completion, then
             all possibilities are displayed in a completion buffer."
  :group 'scilab
  :type '(radio (const :tag "Incremental completion (hippie-expand)."
		       increment)
		(const :tag "Show completion buffer."
		       complete)))

(defgroup scilab-face nil
  "Scilab subgroup containing all customized faces."
  :group 'scilab)

;; Load in the region we use for highlighting stuff.
(if (and (featurep 'custom) (fboundp 'custom-declare-variable))

    (let* ((l-region-face (if (facep 'region) 'region 'zmacs-region)))
;; If we have custom, we can make our own special face like this
      (defface scilab-region-face
	(list
	 (list t 
                (scilab-face-get-attribute l-region-face nil)
                          ))
	"*Face used to highlight a scilab region."
	:group 'scilab-face
 ))

  ;; If we do not, then we can fake it by copying 'region.
  (cond ((facep 'region)
	 (copy-face 'region 'scilab-region-face))
	(t
	 (copy-face 'zmacs-region 'scilab-region-face))))

(defcustom scilab-which-func-format '(" " which-func-current " " 
);;;scilab-number-line-in-function)
  "Format for displaying the function in the mode line."
  :group 'scilab
  :type 'sexp)

(defvar scilab-number-line-in-function "?")

(defvar scilab-unterminated-string-face 'scilab-unterminated-string-face
  "Self reference for unterminated string face.")

(defvar scilab-scicos-keyword-face 'scilab-scicos-keyword-face
  "Self reference for scicos keywords.")
(defvar scilab-valid-variable-name "\\<[A-Za-z$#_%][A-Za-z0-9$#_]*\\>"
  "Regexp describing all valid variable names")


(defun scilab-simple-send (proc string)
  "Default function for sending in scilab session to PROC input STRING.
Takes in account the meaning of the symbol '!'. 
See the hook `comint-input-sender'."
  (setq string (scilab-string-garbage-filter string))
  (let  ((str "") (curbuff (current-buffer)) (tmpbuff nil))
    (if (not (string-equal string ""))
	(setq str (substring string 0 1)))

; ;     (if (string-match "^\\*\\*" string)
; ;         (progn
; ; 	  (eval (car  (read-from-string (substring string 2))))
; ; ;          (setq tmpbuff  (current-buffer))
; ;           (switch-to-buffer curbuff)
; ; ;          (comint-send-string proc (concat "//LISP//" (substring string 2) "\n"))  
; ;           (comint-send-string proc "")
; ;                 )
      (if (string-equal str "!")
	  (comint-previous-matching-input (concat "^" (substring string 1)) 1)
	(comint-send-string proc string)
	(comint-send-string proc "\n"))
; ;)

   )
)



(defun scilab-string-garbage-filter (string)
  "Filters STRING for the scilab-simple-send"
  (let ((garbage (concat "\\(" (regexp-quote "\C-g") "\\|"
 			 (regexp-quote "\033[H0") "\\|"
 			 (regexp-quote "\033[H\033[2J") "\\|"
 			 (regexp-quote "\033H\033[2J") "\\)")))
    (while (string-match garbage string)
      (if (= (aref string (match-beginning 0)) ?\C-g)
	  (beep t))
      (setq string (replace-match "" t t string))))
(if (not string) "" string))

(defun scilab-output-garbage-filter (&optional string)
  "Filters output.This function could be on `comint-output-filter-functions' or bound to a key."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer))))
        (garbage (concat "\\(" (regexp-quote "\C-g") "\\|"
                         (regexp-quote "\C-M") "\\|"
 			 (regexp-quote "\033[H0") "\\|"
 			 (regexp-quote "\033[H\033[2J") "\\|"
 			 (regexp-quote "\033H\033[2J") "\\)")))
    (save-excursion
      (condition-case nil
	  (goto-char
	   (if (interactive-p) comint-last-input-end comint-last-output-start))
	(error nil))
      (while (re-search-forward garbage pmark t)
      (replace-match "" t t)))))


(defun scilab-font-lock-adjustments ()
  "Make adjustments for font lock.
If font lock is not loaded, lay in wait."
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
	
      (progn
	(defface scilab-unterminated-string-face 
	  (list
	   (list t
                      `(,@(scilab-face-get-attribute 'font-lock-string-face nil) :underline t)
))
	  "*Face used to highlight unterminated strings."
	  :group 'scilab-face)
	(defface scilab-scicos-keyword-face
	  (list
	   (list t
                 `(,@(scilab-face-get-attribute 'font-lock-type-face nil)
                            :underline t)
))
	  "*Face used to highlight scicos specific functions." :group 'scilab-face)

        (defface scilab-string-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-string-face nil)

         ))
	  "*Face used to highlight strings." :group 'scilab-face)
	
       (defface scilab-comment-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-comment-face nil)

        ))
	  "*Face used to highlight comments." :group 'scilab-face)
	
       (defface scilab-builtin-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-builtin-face nil)

        ))
	  "*Face used to highlight builtins." :group 'scilab-face)

       (defface scilab-macros-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-variable-name-face nil)

        ))
	  "*Face used to highlight macros." :group 'scilab-face)
	

        (defface scilab-keyword-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-keyword-face nil)

          ))
	  "*Face used to highlight keywords." :group 'scilab-face)

       (let* ((constant-face (if (facep 'font-lock-constant-face) 
                 'font-lock-constant-face 'font-lock-reference-face)))
        (defface scilab-constant-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute constant-face nil)

          ))
	  "*Face used to highlight constants and labels." :group 'scilab-face))

        (defface scilab-type-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-type-face nil)

          ))
	  "*Face used to highlight types and classes." :group 'scilab-face)
        (defface scilab-warning-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-warning-face nil)

          ))
	  "*Face used to highlight warnings." :group 'scilab-face)
        (defface scilab-function-name-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-function-name-face nil)

          ))
	  "*Face used to highlight functions names in function header." :group 'scilab-face)
        (defface scilab-tlist-field-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-variable-name-face nil)

          ))
	  "*Face used to highlight fields in tlists when it is called 
          \"foo.field\"." :group 'scilab-face)

        (defface scilab-variable-name-face 	  
         (list
	   (list t
                 (scilab-face-get-attribute 'font-lock-variable-name-face nil)

          ))
	  "*Face used to highlight variable names (in function header)." :group 'scilab-face)
        )	
	
	

      
    ;; Now, lets make the unterminated string face
    (cond ((facep 'font-lock-string-face)
	   (copy-face 'font-lock-string-face
		      'scilab-unterminated-string-face))
	  (t
	   (make-face 'scilab-unterminated-string-face)))
    (set-face-underline-p 'scilab-unterminated-string-face t)
    
    ;; Now make some scicos faces
    (cond ((facep 'font-lock-type-face)
	   (copy-face 'font-lock-type-face 'scilab-scicos-keyword-face))
	  (t
	   (make-face 'scilab-scicos-keyword-face)))
    (set-face-underline-p 'scilab-scicos-keyword-face t)
    )

  (remove-hook 'font-lock-mode-hook 'scilab-font-lock-adjustments))

;; Now we create our own faces, but we keep these variables for compatibility
;; and they give users another mechanism for changing face appearance.
;; We now allow a FACENAME in `font-lock-keywords' to be any expression that
;; returns a face.  So the easiest thing is to continue using these variables,
;; rather than sometimes evaling FACENAME and sometimes not.  sm.
(defvar scilab-comment-face		'scilab-comment-face
  "Face name to use for comments.")

(defvar scilab-string-face		'scilab-string-face
  "Face name to use for strings.")

(defvar scilab-keyword-face		'scilab-keyword-face
  "Face name to use for keywords.")

(defvar scilab-builtin-face		'scilab-builtin-face
  "Face name to use for builtins.")

(defvar scilab-macros-face		'scilab-macros-face
  "Face name to use for macros.")

(defvar scilab-function-name-face	'scilab-function-name-face
  "Face name to use for function names.")

(defvar scilab-variable-name-face	'scilab-variable-name-face
  "Face name to use for variable names.")
(defvar scilab-tlist-field-face	'scilab-tlist-field-face
  "Face name to use for tlist-fields names.")

(defvar scilab-type-face		'scilab-type-face
  "Face name to use for type and class names.")

(defvar scilab-constant-face		'scilab-constant-face
  "Face name to use for constant and label names.")

(defvar scilab-warning-face		'scilab-warning-face
  "Face name to use for things that should stand out.")

(defvar scilab-reference-face	'scilab-constant-face
  "This variable is obsolete.  Use scilab-constant-face.")


;; Make the adjustments for font lock after it's loaded.
;; I found that eval-after-load was unreliable.
(if (featurep 'font-lock)
    (scilab-font-lock-adjustments)
  (add-hook 'font-lock-mode-hook 'scilab-font-lock-adjustments))

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
(remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom)
(add-hook  'comint-output-filter-functions 'scilab-output-garbage-filter)

;;; Scilab mode variables =====================================================

(defvar scilab-tempo-tags nil
  "List of templates used in Scilab mode.")

;; syntax table
(defvar scilab-mode-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?_  "_" st)
    (modify-syntax-entry ?/  ". 124b" st)
  ;  (modify-syntax-entry ?/  ". 1456" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?*  "." st)
    (modify-syntax-entry ?'  "." st)
    (modify-syntax-entry ?\"  "." st)
  ;  (modify-syntax-entry ?/  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)
;    (modify-syntax-entry ?\{ "(}  "  st)
;    (modify-syntax-entry ?\[ "(]  "  st)
;    (modify-syntax-entry ?\( "()  "  st)
;    (modify-syntax-entry ?\} "){  "  st)
;    (modify-syntax-entry ?\] ")[  "  st)
;    (modify-syntax-entry ?\) ")(  "  st)
    st)
  "The syntax table used in `scilab-mode' buffers.")

(defvar scilab-mode-special-syntax-table
  (let ((st (copy-syntax-table scilab-mode-syntax-table)))
    ;; Make _ a part of words so we can skip them better
    (modify-syntax-entry ?_  "w" st)
    st)
  "The syntax table used when navigating blocks.")

;; abbrev table
(defvar scilab-mode-abbrev-table nil
  "The abbrev table used in `scilab-mode' buffers.")

(define-abbrev-table 'scilab-mode-abbrev-table ())

;;; Keybindings ===============================================================

(defvar scilab-help-map
  (let ((km (make-sparse-keymap)))
    (define-key km "w" 'scilab-shell-whereami)
    (define-key km "f" 'scilab-shell-describe-command)
    (define-key km "a" 'scilab-shell-apropos)
    (define-key km "v" 'scilab-shell-describe-variable)
    (define-key km "t" 'scilab-shell-topic-browser)
    km)
  "The help key map for `scilab-mode' and `scilab-shell-mode'.")

(defvar scilab-insert-map
  (let ((km (make-sparse-keymap)))
    (define-key km "c" 'scilab-insert-next-case)
    (define-key km "e" 'scilab-insert-end-block)
    (define-key km "i" 'tempo-template-scilab-if)
    (define-key km "I" 'tempo-template-scilab-if-else)
    (define-key km "f" 'tempo-template-scilab-for)
    (define-key km "s" 'tempo-template-scilab-select)
    (define-key km "t" 'tempo-template-scilab-try)
    (define-key km "w" 'tempo-template-scilab-while)
    (define-key km "F" 'tempo-template-scilab-function)
    (define-key km "'" 'scilab-stringify-region)
    ;; Not really inserts, but auto coding stuff
    (define-key km "\C-s" 'scilab-ispell-strings)
    (define-key km "\C-c" 'scilab-ispell-comments)
    km)
  "Keymap used for inserting simple texts based on context.")

;; mode map
(defvar scilab-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [return] 'scilab-return)
    (define-key km "\C-c;" 'scilab-comment-region)
    (define-key km "\C-c:" 'scilab-uncomment-region)
    (define-key km [(control c) return] 'scilab-comment-return)
    (define-key km [(control c) (control c)] scilab-insert-map)
    (define-key km [(control c) (control f)] 'scilab-fill-comment-line)
    (define-key km [(control c) (control j)] 'scilab-justify-line)
    (define-key km [(control c) (control q)] 'scilab-fill-region)
    (define-key km [(control c) (control i)] 'scilab-indent-defun)
    (define-key km [(control c) (control l)] 'scilab-shell-save-and-getf-or-run)
    (define-key km "\C-cr" 'scilab-shell-run-region)
    (define-key km [(control c) (control e)] 'scilab-shell-remote-exit)
    (define-key km [(control c) ?. ] 'scilab-find-file-on-path)
    (define-key km "\C-c/" 'scilab-shell-cd)
    (define-key km "\C-cd" 'scilab-shell-cd)
    (define-key km [(control h) return] scilab-help-map)
    (define-key km [(control h) return] scilab-help-map)
    (define-key km [(control j)] 'scilab-linefeed)
    (define-key km "\M-\r" 'newline)
    (define-key km [(meta \;)] 'scilab-comment)
    (define-key km [(meta q)] 'scilab-fill-paragraph)
    (define-key km [(meta a)] 'scilab-beginning-of-command)
    (define-key km [(meta e)] 'scilab-end-of-command)
    (define-key km [(meta j)] 'scilab-comment-line-break-function)
    (define-key km "\M-\t" 'scilab-complete-symbol)
    (define-key km [(meta control f)] 'scilab-forward-sexp)
    (define-key km [(meta control b)] 'scilab-backward-sexp)
    (define-key km [(meta up)] 'scilab-beginning-of-prev-defun)
    (define-key km [(meta down)] 'scilab-beginning-of-next-defun)
    (define-key km [(control up)] 'scilab-beginning-of-defun)
    (define-key km [(control n)] 'scilab-function-goto-line)
    (define-key km [(control down)] 'scilab-end-of-defun)
    (if running-xemacs
	(define-key km [(control meta button1)] 'scilab-find-file-click)
      (define-key km [(control meta mouse-2)] 'scilab-find-file-click))
    (substitute-key-definition 'comment-region 'scilab-comment-region
			       km) ; global-map ;torkel
    km)
  "The keymap used in `scilab-mode'.")

;;; Font locking keywords =====================================================

(defvar scilab-string-start-regexp "\\(^\\|[^]})a-zA-Z0-9_.']\\)"
  "Regexp used to represent the character before the string char '.
The ' character has restrictions on what starts a string which is needed
when attempting to understand the current context.")

;; To quote a quote, put two in a row, thus we need an anchored
;; first quote.  In addition, we don't want to color strings in comments.
(defvar scilab-string-end-regexp "[^'\"\n]*\\(['\"]['\"][^'\"\n]*\\)*['\"]"
  "Regexp used to represent the character pattern for ending a string.
The ' character can be used as a transpose, and can transpose transposes.
Therefore, to end, we must check all that goop.")

(defun scilab-font-lock-string-match-normal (limit)
  "When font locking strings, call this function for normal strings.
Argument LIMIT is the maximum distance to scan."
  (scilab-font-lock-string-match-here
   (concat scilab-string-start-regexp
	   "\\(['\"]" scilab-string-end-regexp "\\)"
	   "\\([^'\"]\\|$\\)")
   limit))

(defun scilab-font-lock-string-match-unterminated (limit)
  "When font locking strings, call this function for normal strings.
Argument LIMIT is the maximum distance to scan."
  (scilab-font-lock-string-match-here
   (concat scilab-string-start-regexp "\\(['\"][^'\"\n]*\\(['\"]['\"][^'\"\n]*\\)*\\)$")
   limit))

(defun scilab-font-lock-string-match-here (regex limit)
  "When font-locking strings, call this function to determine a match.
Argument REGEX is the expression to scan for.  Match 2 must be the string.
Argument LIMIT is the maximum distance to scan."
  (let (e)
    (while (and (re-search-forward regex limit t)
		(progn
		  ;; This gets us out of a comment after the string.
		  (setq e (match-end 2))
		  (goto-char (match-beginning 2))
		  (prog1
		      (or (scilab-cursor-in-comment)
			  (if (bolp) nil
			    (save-excursion
			      (forward-char -1)
			      (scilab-cursor-in-string))))
		    (goto-char e))))
      (setq e nil))
    (if (not e)
	nil
      (goto-char e)
      t)))

(defun scilab-font-lock-comment-match (limit)
  "When font-locking comments, call this function to determine a match.
Argument LIMIT is the maximum distance to scan."
  (let (e)
   (while (and (re-search-forward "\\(//[^\n]*\\)" limit t)
		(progn
		  (setq e (match-end 1))
		  (member (get-text-property (match-beginning 0) 'face)
			  '(scilab-string-face
			    scilab-unterminated-string-face))))
      (setq e nil))
    (if (not e)
	nil
      (goto-char e)
      t)))

;;font-lock-solo-keywords

(defvar scilab-font-lock-solo-keywords 
   (list 
   '("\\<\\(break\\|case\\|e\\(lse\\(\\|if\\)\\|ndfunction\\)\
\\|for\\|global\\|clear\\|clearglobal\\|if\\|return\\|while\\|pause\\|function\\|select\\|then\\|quit\\|exit\\|stop\\|abort\\|do\\|resume\\|help\\)\\>"
     (0 scilab-keyword-face)))
"Font lock of keywords that appear on a line by themselves."
)
;; font-lock keywords
(defcustom scilab-builtin-list
;;; From the file $SCI/routines/default/fundef
  '("%i_abs" "%i_cumsum" "%i_diag" "%i_matrix" "%i_max" "%i_maxi" "%i_min" 
"%i_mini" "%i_p" "%i_sum" "%i_tril" "%i_triu" "%msp_full" "%msp_spget" 
"CreateLink" "DestroyLink" "ExecAppli" "GetMsg" "Matplot" "NumTokens" 
"SendMsg" "WaitMsg" "abs" "addf" "addinter" "addmenu" "amell" "apropos" 
"argn" "arl2_ius" "ascii" "atan" "balanc" "bdiag" "besseli" "besselj" 
"besselk" "bessely" "bezout" "bfinit" "blkfc1i" "blkslvi" "bool2s" "bvode" 
"c2dex" "c_link" "calerf" "call" "cdfbet" "cdfbin" "cdfchi" "cdfchn" "cdff"
"cdffnc" "cdfgam" "cdfnbn" "cdfnor" "cdfpoi" "cdft" "ceil" "champ" 
"champ1" "chdir" "chol" "clean" "clear" "clearfun" "clearglobal" "code2str"
"coeff" "comp" "cond" "conj" "contour" "contour2d" "contour2di" "contr"
"convstr" "corr" "cos" "cumprod" "cumsum" "curblock" "dasrt" "dassl" 
"debug" "deff" "definedfields" "degree" "delbpt" "delip" "delmenu" "det" 
"diag" "diary" "disp" "dispbpt" "dlgamma" "double" "driver" "emptystr" 
"ereduc" "errcatch" "errclear" "error" "exec" "execstr" "exists" "exp" 
"expm" "eye" "fadj2sp" "fec" "feval" "fft" "file" "find" "floor" "format" 
"fort" "fprintfMat" "freq" "fscanfMat" "fsolve" "fstair" "full" "funcprot"
"funptr" "gamma" "gammaln" "geom3d" "getblocklabel" "getcwd" "getdate"
"getenv" "getf" "getfield" "getpid" "getscicosvars" "glist" "global" 
"grand" "grayplot" "gschur" "gsort" "gspec" "gstacksize" "havewindow" 
"help" "hess" "host" "iconvert" "ieee" "imag" "impl" "inpnvi" "int" "int16" 
"int2d" "int32" "int3d" "int8" "interp" "intg" "intppty" "inttype" "inv"
"iserror" "isglobal" "isreal" "kron" "ldiv" "ldivf" "length" "lib" "lines" "link" "list" "load" "loadwave" "log" "lsslist" "lstcat" "ltitr" "lu"
 "ludel" "lufact" "luget" "lusolve" "lyap" "m6bandred" "m6bmatch" 
"m6busack" "m6cent" "m6chcm" "m6clique" "m6clique1" "m6compc" "m6compfc" 
"m6concom" "m6deumesh" "m6dfs" "m6dfs2" "m6diam" "m6dijkst" "m6dmtree" 
"m6edge2st" "m6findiso" "m6flomax" "m6floqua" "m6fordfulk" "m6frang" "m6hamil"
 "m6hullcvex" "m6inimet" "m6johns" "m6kilter" "m6knapsk" "m6loadg" 
"m6lp2tad" "m6lp2tau" "m6mesh2b" "m6meshmesh" "m6metasync" "m6ns2p" "m6p2ns"
"m6pcchna" "m6permuto" "m6prevn2p" "m6prevn2st" "m6prfmatch" "m6relax" 
"m6saveg" "m6sconcom" "m6showg" "m6showns" "m6showp" "m6ta2lpd" 
"m6ta2lpu" "m6tconex" "m6transc" "m6umtree" "m6umtree1" "m6visitor" "macr2lst" 
"matrix" "max" "maxi" "mclearerr" "mclose" "meof" "mfprintf" "mfscanf" 
"mget" "mgetstr" "min" "mini" "mlist" "mode" "mopen" "mprintf" "mput" 
"mputstr" "mscanf" "mseek" "msprintf" "msscanf" "mtell" "mtlb_mode" 
"mtlb_sparse" "mulf" "netclose" "netwindow" "netwindows" "newfun" "nnz" "norm" 
"ode" "odedc" "oldload" "oldsave" "ones" "optim" "ordmmd" "param3d" 
"param3d1" "part" "pinv" "plot2d" "plot2d1" "plot2d2" "plot2d3" "plot2d4" 
"plot3d" "plot3d1" "poly" "ppol" "pppdiv" "predef" "print" "prod" 
"pvm_addhosts" "pvm_barrier" "pvm_bcast" "pvm_config" "pvm_delhosts" 
"pvm_error" "pvm_exit" "pvm_get_timer" "pvm_getinst" "pvm_gettid" "pvm_gsize" 
"pvm_halt" "pvm_joingroup" "pvm_kill" "pvm_lvgroup" "pvm_mytid" "pvm_parent" 
"pvm_recv" "pvm_recv_var" "pvm_reduce" "pvm_send" "pvm_send_var" 
"pvm_set_timer" "pvm_spawn" "pvm_spawn_independentIN_pvm" "pvm_start" 
"pvm_tasks" "pvm_tidtohost" "qpqpqp" "qr" "rand" "rank" "rat" "rcond" "rdivf" 
"read" "read4b" "readb" "readgif" "readmps" "readxbm" "real" "remez" "residu"
 "resume" "return" "ricc" "rlist" "roots" "round" "rpem" "rref" "rtitr"
"save" "savewave" "schur" "sci_tree2" "sci_tree3" "sci_tree4" "sciargs"
"scicosim" "sctree" "semidef" "setbpt" "setfield" "setmenu" 
"setscicosvars" "sfact" "sfinit" "sign" "simp" "simp_mode" "sin" "size" "sort" 
"sparse" "spchol" "spcompack" "spec" "spget" "splin" "sqrt" "stacksize" 
"str2code" "strcat" "strindex" "string" "strsubst" "subf" "sum" "sva" "svd" 
"sylv" "symfcti" "syredi" "testmatrix" "timer" "tlist" "tr_zer" "tril" 
"triu" "type" "typename" "uint16" "uint32" "uint8" "ulink" "unix" "unsetmenu"
 "user" "var2vec" "varn" "vec2var" "what" "where" "whereis" "who" 
"winsid" "writb" "write" "write4b" "x_choose" "x_dialog" "x_mdialog" 
"x_message" "xarc" "xarcs" "xarrows" "xaxis" "xchange" "xchoicesi" "xclea" 
"xclear" "xclick" "xdel" "xend" "xfarc" "xfarcs" "xfpoly" "xfpolys" 
"xfrect" "xg2ps" "xget" "xgetech" "xgetfile" "xgetmouse" "xgraduate" "xgrid" 
"xinfo" "xinit" "xlfont" "xload" "xname" "xnumb" "xpause" "xpoly" "xpolys"
 "xrect" "xrects" "xs2fig" "xsave" "xsegs" "xselect" "xset" "xsetech" "x
string" "xstringl" "xtape" "xtitle"
;;; from the file "$SCI/routines/fraclab/fundef.fraclab
"FWT" "FWT2D" "IWT" "IWT2D" "Koutrouvelis" "McCulloch" "WTDwnHi" "WTDwnLo" 
"alphagifs" "bbch" "beep" "binom" "cfg1d" "cwt" "fcfg1d" "fch1d" "fif" 
"gifs2wave" "gifseg" "holder2d" "lepskiiap" "linearlt" "mcfg1d" "mch1d" 
"mdfl1d" "mdzq1d" "mdzq2d" "monolr" "multim1d" "multim2d" "prescalpha" 
"readgif" "reynitq" "sbinom" "sgifs" "sim_stable" "smultim1d" "smultim2d" 
"stable_cov" "stable_sm" "stable_test" "wave2gifs"
;;; from the file $SCI/routines/tksci/fundef.tksci 
"TK_DoOneEvent" "TK_EvalFile" "TK_EvalStr" "TK_GetVar" "TK_SetVar" "close" 
"essai" "figure" "findobj" "gcf" "get" "getgvar" "opentk" "set" "setgvar" 
"uicontrol" "uimenu")
  "List of Scilab Builtin functions. Not so elegant, but stable. It is taken
from 3  files: $SCI/routines/default/fundef and  $SCI/routines/default/fundef
This list admits further extensions"
  :group 'scilab-shell
  :type '(repeat (string :tag "Name: "))
)

(defcustom scilab-user-keywords-list
'("demos" "info" "help" "doc" "apropos" "what" "whos" "end" "cd" "end" "clear" "load" "save" "getf" "getd" "make" "whereis" "whereami" "where" "break" "pause" "resume " "quit" "exit" "stop" "abort" "do" "xset" "xget" "deff")
"List of words a user wants to higlight as keywords" 
  :group 'scilab-shell
  :type '(repeat (string :tag "Name: ")
))

(defvar scilab-path-type-regexp "[\'\" ]?[~/.$]/*[a-zA-Z0-9_./%$-]*"
"Regexp describing possible path string when we are working with disk. Should
;be changed for MSDOS"
)


(defun scilab-make-regexp-from-builtin ()
"Make regexp from builtin list of strings"
(concat "\\<\\(" (mapconcat 'regexp-quote scilab-builtin-list "\\|") 
	"\\)\\>")
) 

(defcustom scilab-libfunc-list-path (if (getenv "SCILIBFUNC") (getenv "SCILIBFUNC") (concat (if (getenv "SCIHOME") (getenv "SCIHOME") (getenv "HOME")) "/libfunc"))
  "*The path to the file where all library functions are listed. Need for efficient completion mechanism. You don't have to build this file, it will be done automatically"
  :group 'scilab
  :group 'scilab-setup
  :type 'string)


(defun scilab-make-regexp-from-libfunc ()
"Make regexp from libfunc file"
(if (not (file-exists-p scilab-libfunc-list-path))
   "\\<\\(genlib\\)\\>" 
   (let ((currbuf (current-buffer)) (buff nil) (lst nil))
   (find-file scilab-libfunc-list-path)
   (setq buff (current-buffer))
   (setq lst  (split-string (buffer-substring-no-properties (point-min) (point-max))))
   (kill-buffer buff)
   (switch-to-buffer currbuf)
   (concat "\\<\\(" (mapconcat 'regexp-quote lst "\\|") 
	   "\\)\\(\\s-*[()=\n,;~]"
           "\\|\\s-+" scilab-valid-variable-name
           "\\|\\s-+" scilab-path-type-regexp "\\)")
)
) 
)
(defvar scilab-font-lock-keywords
  (list
   ;; String quote chars are also used as transpose, but only if directly
   ;; after characters, numbers, underscores, or closing delimiters.
   '(scilab-font-lock-string-match-normal 2 scilab-string-face)
   ;; A string with no termination is not currently highlighted.
   ;; This will show that the string needs some attention.
   '(scilab-font-lock-string-match-unterminated
     2 scilab-unterminated-string-face)
   ;; Comments must occur after the string, that way we can check to see
   ;; if the comment start char has occurred inside our string. (EL)
   '(scilab-font-lock-comment-match 1 scilab-comment-face)
;   ;; General keywords
;   '("\\<\\(break\\|ca\\(se\\|tch\\)\\|e\\(lse\\(\\|if\\)\\|ndfunction\\)\
;\\|for\\|global\\|if\\|return\\|while\\|pause\\|function\\|select\\|then\\|quit\\|exit\\|stop\\|abort\\|do\\|resume\\)\\>"
;     (0 scilab-keyword-face))
   ;; The end keyword is only a keyword when not used as an array
   ;; dereferencing part.
   '("\\(^\\|[;,]\\)[ \t]*\\(end\\)\\b"
     2 (if (scilab-valid-end-construct-p) scilab-keyword-face nil))
   ;; The global keyword defines some variables.  Mark them.
   '("^\\s-*global\\s-+"
     ("\\(\\w+\\)\\(\\s-*=[^,; \t\n]+\\|[, \t;]+\\|$\\)"
      nil nil (1 scilab-variable-name-face)))
   '("\\<\\(ax\\(es\\|is\\)\\|figure\\|get\\|image\\|li\\(ght\\|ne\\)\\|\
patch\\|s\\(et\\(\\|color\\|font\\)\\|urface\\)\\|text\\|\
ui\\(cont\\(ext\\(\\|menu\\)\\|rol\\)\\|menu\\|\
\\(toggle\\|push\\)tool\\|toolbar\\)\\)\\>"
     (0 scilab-type-face))
;;;punctuation
   '("[],.;:[)(^~=-]"  0 scilab-type-face)
   )
  "Expressions to highlight in Scilab mode.")

(defvar scilab-gaudy-font-lock-keywords
  (append
   scilab-font-lock-keywords
   scilab-font-lock-solo-keywords
   (list
    ;; defining a function, a (possibly empty) list of assigned variables,
    ;; function name, and an optional (possibly empty) list of input variables
    (list (concat "^\\s-*\\(function\\)\\>[ \t\n.]*"
		  "\\(\\[[^]=()]*\\]\\|" scilab-valid-variable-name "\\)"
                  "[ \t\n.]*"
		  "=[ \t\n.]*\\("
                  scilab-valid-variable-name
                  "\\)[ \t\n.]*"
		  "(?\\("
                  "[^)]*"
                  "\\))?"
                  "\\s-*[,;\n//]")
	  '(1 scilab-keyword-face append)
	  '(2 scilab-variable-name-face append)
	  '(3 scilab-function-name-face append)
	  '(4 scilab-variable-name-face append)
)
    ;; defining a function, a function name without output
      (list (concat "^\\s-*\\(function\\)[ \t\n.]+\\("
                   scilab-valid-variable-name
                   "\\)[ \t\n.]*"
  		   "\\(([^)]*)\\)")
  	  '(1 scilab-keyword-face append)
  	  '(2 scilab-function-name-face append)
  	  '(3 scilab-variable-name-face append)
       )
  ;; Pathalogy: only function name
      (list (concat "^\\s-*\\(function\\)[ \t\n.]+\\("
                   scilab-valid-variable-name "\\)")
  	  '(1 scilab-keyword-face append)
  	  '(2 scilab-function-name-face append)
       )


    '("\\<\\(for\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*\
\\(\\([^\n,;//(]+\\|([^\n//)]+)\\)+\\)"
      (1 scilab-keyword-face)
      (2 scilab-variable-name-face append)
      (3 scilab-constant-face append))
    ;; Items after a select statements are cool
    '("\\<\\(case\\|select\\)\\s-+\\({[^}\n]+}\\|[^,//\n]+\\)"
      (1 scilab-keyword-face) (2 scilab-constant-face))
    ;; How about a few scilab constants such as pi, infinity, and sqrt(-1)?
    ;; The ^>> is in case I use this in an interactive mode someday
    '("\\<\\(%eps\\|%[a-z]_[a-z]\\|%[a-z]_[a-z]_[a-z]\\|%[a-z]\\|%[A-Z]\\|%pi\\|%inf\\|Inf\\|%Inf\\|NaN\\|%nan\\|Nan\\|%Nan\\|ans\\|%i\\|%n\\|%tab\\|\\^>>\\)\\>"
      (1 scilab-constant-face))
    ;; Define these as variables since this is about as close
    ;; as scilab gets to variables
;;;    (list (concat "\\<" scilab-indent-past-arg1-functions "\\s-*")
;;;	  '("(\\s-*\\(\\w+\\)\\s-*\\(,\\|)\\)" nil nil
;;;	    (1 scilab-variable-name-face)))
;;; I want to see fields of tlist  highlighted
   (list (concat scilab-valid-variable-name "\\.\\(" scilab-valid-variable-name "\\)")
 '(1 scilab-tlist-field-face append))
    ))
  "Expressions to highlight in Scilab mode.")
(if scilab-highlight-builtin
  (setq scilab-gaudy-font-lock-keywords
	(append
	 scilab-gaudy-font-lock-keywords
	 (list (list (scilab-make-regexp-from-builtin) '(1 scilab-builtin-face append))))))

(if scilab-highlight-macros
  (setq scilab-gaudy-font-lock-keywords
	(append
	 scilab-gaudy-font-lock-keywords
	 (list (list (scilab-make-regexp-from-libfunc) '(1 scilab-macros-face append))))))




(defvar scilab-really-gaudy-font-lock-keywords
  (append
   scilab-gaudy-font-lock-keywords
   (list
    ;; Since it's a math language, how bout dem symbols?
    '("\\([<>~]=?\\|\\.[*^']\\|=+\\|\\<xor\\>\\|[-!^&|*+\\/~:]\\)"
      1 scilab-type-face)
    ;; How about references in the HELP text.
    (list (concat "^" scilab-comment-line-s "\\s-*"
		  "\\(\\([A-Z]+\\s-*=\\s-+\\|\\[[^]]+]\\s-*=\\s-+\\|\\)"
		  "\\([A-Z][0-9A-Z]+\\)\\(([^)\n]+)\\| \\)\\)")
	  '(1 scilab-constant-face prepend))
    (list (concat "^" scilab-comment-line-s "\\s-*"
		  "SEE ALSO\\s-+")
	  '("\\([A-Z][A-Z0-9]+\\)\\([,.]\\| and\\|$\\) *" nil nil
	    (1 scilab-constant-face prepend)))
    (list (concat "//\\s-*"
		  "\\(\\$Revision: 1.32 $]+\\$\\)")
	  '(1 scilab-constant-face prepend))
    ;; continuation ellipsis.
    '("[^.]\\(\\.\\.\\.*\\)\\([^\n]*\\)" 1 scilab-keyword-face)
;      (2 scilab-comment-face))
    ;; How about debugging statements?
    ;;'("\\<\\(db\\sw+\\)\\>" 1 'bold)
    ;;(make-regexp '("dbstop" "dbclear" "dbcont" "dbdown" "dbmex"
    ;;		   "dbstack" "dbstatus" "dbstep" "dbtype" "dbup" "dbquit"))
;;    '("\\<\\(db\\(c\\(lear\\|ont\\)\\|down\\|mex\\|quit\\|
;;st\\(a\\(ck\\|tus\\)\\|ep\\|op\\)\\|type\\|up\\)\\)\\>" (0 'bold)))
    '("\\<\\(\\(set\\|del\\|disp\\)bpt\\)\\>" (0 'bold)))

   (if scilab-handle-scicos
       ;; Scicos functions,  a scicos user has to edit this.
       (list (list (concat "\\<\\(\\([sg]et_param\\|sim\\([gs]et\\)?\\|"
			   "\\(mld\\|ss\\)[A-Z]\\w+\\)\\|"
			   "\\(new\\|open\\|close\\|save\\|find\\)_system\\|"
			   "\\(add\\|delete\\|replace\\)_\\(block\\)\\|"
			   "scicos\\|bd\\(root\\|close\\)"
			   "\\)\\>")
		   '(1 scilab-scicos-keyword-face)))
     nil))
  "Expressions to highlight in Scilab mode.")

(defvar scilab-shell-font-lock-keywords
  (list
   ;; How about Errors?
   '("\\(!--error\\)\\s-+\\([^\n]+\n\\)\\(.+\n\\)" (1 scilab-warning-face t)(2 scilab-warning-face t) (3 scilab-warning-face t))
   ;; and line numbers
   '("^\\(at line\\)\\s-+\\([0-9]+\\)" (1 scilab-warning-face) (2 scilab-warning-face))
   ;; Warnings
   '("\\(Warning:?\\|Warnings:?\\|WARNING:?\\|WARNINGS:?\\)" 1 scilab-warning-face prepend) 
 '("\\(Error:?\\|Errors:?\\|ERROR:?\\|ERRORS:?\\)" 1 scilab-warning-face prepend) 
   ;; User beep things
   '("\\(\\?\\?\\?[^\n]+\\)" 1 scilab-warning-face)
   ;; Useful user commands, but not useful programming constructs
   (list (concat "\\<\\(" (regexp-opt scilab-user-keywords-list) "\\)\\>")
     '(1 scilab-keyword-face))
   ;; Various notices
   '("S C I L A B (R)" 0 'underline)
   '("SCILAB (R)" 0 'underline)
   '("S c i l a b" 0 scilab-function-name-face) 
   '("All Rights Reserved" 0 'italic)
   '("\\((c)\\s-+Copyright[^\n]+\\)" 1 scilab-comment-face t)
   '("\\(Copyright (C)\\)\\s-+\\([^\n]+\\)" (1 scilab-function-name-face t) (2 scilab-variable-name-face t))
   '("\\(Version\\)\\s-+\\([^\n]+\\)"
     (1 scilab-function-name-face t) (2 scilab-variable-name-face t))
   '("\\(scilab-\\)\\([1-9]\\.?[0-9]?\\.?[0-9]?\\)"
     (1 scilab-comment-face t) (2 scilab-variable-name-face t))
   )
  "Additional keywords used by Scilab when reporting errors in interactive\
mode and displays varios messages.")


;; hilit19 patterns
(defvar scilab-hilit19-patterns
  '(
    ("\\(^\\|[^//]\\)\\(//[ \t].*\\|//\\)$" 2 comment)
    ("\\(^\\|[;,]\\)[ \t]*\\(\
function\\|global\\|for\\|while\\|if\\|elseif\\|else\\|end\\(function\\)?\
\\|return\\|select\\|case\\|then\\|quit\\|exit\\|stop\\|abort\\|resume\\|break\\|do\\)\\b" 2 keyword)))

(defvar scilab-imenu-generic-expression
  '((nil "^\\s-*function\\>[ \t\n.]*\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*\
=\[ \t\n.]*\\)?\\(%?[a-zA-Z0-9#_]+\\)"
	 3))
  "Expressions which find function headings in Scilab sci files.")
(defvar  scilab-contline-regexp "\\(\\.\\.+[ \t.]*\n\\)"
  "Regexp used to perform continuation on code lines.
It may be .. ... .. ... etc \n.")

;; May be excess, but ...
(defvar  scilab-output-function-regexp "\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t]*=[ \t]*\\)"
"Regexp used to perform output of function like []= or foo= or nothing"
)
(defvar scilab-function-head-regexp (concat
 "^\\s-*function\\>[ \t\n.]*"
  scilab-output-function-regexp "?"
  "[ \t\n.]*"
  "\\(%?[a-zA-Z0-9#_]+\\)"
)
  "Regular Expression for function head  in  sci files.")	 

 
;;; Scilab mode entry point ==================================================

;;;###autoload
(defun scilab-mode ()
  "Scilab-mode is a major mode for editing SCILAB dot-sci files.
\\<scilab-mode-map>
Convenient editing commands are:
 \\[scilab-comment-region]   - Comment  out a region of code.
 \\[scilab-uncomment-region]   - Comment  out a region of code.
 \\[scilab-fill-comment-line] - Fill the current comment line.
 \\[scilab-fill-region] - Fill code and comments in region.
 \\[scilab-fill-paragraph]     - Refill the current command or comment.
 \\[scilab-complete-symbol]   - Symbol completion of scilab symbols\
 \\[scilab-indent-defun]   -	 Indent correspondently all lines in the current function body in `scilab-mode'

Convenient navigation commands are:
 \\[scilab-beginning-of-command]   - Move to the beginning of a command.
 \\[scilab-end-of-command]   - Move to the end of a command.
 \\[scilab-beginning-of-defun] - Move to the beginning of the current function.
 \\[scilab-beginning-of-next-defun] - Move to the beginning of the next function.
\\[scilab-get-number-line-in-function]-Gets number line of the current function \\[scilab-beginning-of-prev-defun] - Move to the beginning of the previous function.
 \\[scilab-function-goto-line] - Move to the N-th line  of the current\
 function.

 \\[scilab-end-of-defun] - Move do the end of thecurrent function.
 \\[scilab-forward-sexp] - Move forward over a syntactic block of code.
 \\[scilab-backward-sexp] - Move backwards over a syntactic block of code.

Convenient template insertion commands:
 \\[tempo-template-scilab-function] - Insert a function definition.
 \\[tempo-template-scilab-if] - Insert an IF END block.
 \\[tempo-template-scilab-for] - Insert a FOR END block.
 \\[tempo-template-scilab-select] - Insert a SELECT END statement.
 \\[scilab-insert-next-case] - Insert the next CASE condition in a SELECT.
 \\[scilab-insert-end-block] - Insert a matched END statement.  With \
optional ARG, reindent.
 \\[scilab-stringify-region] - Convert some plaintext into a string \
with correctly quoted chars.

Variables:
  `scilab-indent-level'		Level to indent blocks.
  `scilab-cont-level'		Level to indent continuation lines.
  `scilab-case-level'		Level to unindent case statements.
;;  `scilab-indent-past-arg1-functions'
;;                                Regexp of functions to indent past the first
;;                                  argument on continuation lines.
  `scilab-maximum-indents'      List of maximum indents during lineups.
  `scilab-comment-column'       Goal column for on-line comments.
  `fill-column'			Column used in auto-fill.
  `scilab-indent-function'	If non-nil, indents body of SCILAB functions.
  `scilab-return-function'	Customize RET handling with this function
  `scilab-auto-fill'            Non-nil, do auto-fill at startup
  `scilab-fill-code'            Non-nil, auto-fill code.
  `scilab-fill-strings'         Non-nil, auto-fill strings.
  `scilab-verify-on-save-flag'  Non-nil, enable code checks on save
  `scilab-highlight-block-match-flag'
                                Enable matching block begin/end keywords
  `scilab-vers-on-startup'	If t, show version on start-up.
  `scilab-handle-scicos'      If t, enable scicos keyword highlighting.

All Key Bindings:
\\{scilab-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map scilab-mode-map)
  (setq major-mode 'scilab-mode)
  (setq mode-name "Scilab")
  (setq local-abbrev-table scilab-mode-abbrev-table)
  (set-syntax-table scilab-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'scilab-indent-line)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//\\s-*")
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-column)
  (setq comment-column scilab-comment-column)

  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'scilab-comment-indent)
  (make-local-variable 'fill-column)
  (setq fill-column default-fill-column)
  (make-local-variable 'auto-fill-function)
  (make-local-variable 'which-func-format)
  (setq which-func-format scilab-which-func-format)
;; These stuffs still do not work
;;  (make-local-variable 'mode-line-format)
;;  (setq mode-line-format scilab-mode-line-format)
;; (make-local-variable 'which-function)
;;  (setq which-function 'scilab-whereami)
;;  (make-local-variable 'which-func-update)
;;  (setq which-func-update 'scilab-which-func-update)
  (if scilab-auto-fill (setq auto-fill-function 'scilab-auto-fill))
  ;; Emacs 20 supports this variable.  This lets users turn auto-fill
  ;; on and off and still get the right fill function.
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'scilab-auto-fill)
  (make-local-variable 'fill-prefix)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression scilab-imenu-generic-expression)
  ;; Save hook for verifying src.  This lets us change the name of
  ;; the function in `write-file' and have the change be saved.
  ;; It also lets us fix mistakes before a `save-and-getf-or-run'.
  (make-local-variable 'write-contents-hooks)
  (add-hook 'write-contents-hooks 'scilab-mode-verify-fix-file-fn)
  ;; Tempo tags
  (make-local-variable 'tempo-local-tags)
  (setq tempo-local-tags (append scilab-tempo-tags tempo-local-tags))
  ;; give each file it's own parameter history
  (make-local-variable 'scilab-shell-save-and-go-history)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((scilab-font-lock-keywords
;                              scilab-font-lock-solo-keywords
;			      scilab-gaudy-font-lock-keywords
			      scilab-really-gaudy-font-lock-keywords
			      )
			     t ; do not do string/comment highlighting
			     nil ; keywords are case sensitive.
			     ;; This puts _ as a word constituent,
			     ;; simplifying our keywords significantly
			     ((?_ . "w"))))
  (scilab-enable-block-highlighting 1)
;  (make-local-variable 'getf-exec)
;  (setq getf-exec nil)
  (if window-system (scilab-frame-init))
;  (gud-make-debug-menu)
  (run-hooks 'scilab-mode-hook)
  (if (and (boundp 'font-lock-mode) (fboundp 'font-lock-mode))
    (if scilab-font-lock-mode 
        (turn-on-font-lock)  
        (font-lock-mode -1)))
 
  (if scilab-vers-on-startup (scilab-show-version))
  (if scilab-launch-automatically
    (let ((buf (current-buffer)))
    (progn   
      (split-window-vertically) 
      (scilab-shell)
      (set-buffer buf)
                    )))
)

;;; Utilities =================================================================

(defun scilab-show-version ()
  "Show the versions number in the minibuffer."
  (interactive)
  (if (not scilab-mode-all-versions)
      (message "scilab-mode, version %s" scilab-mode-version)
      (message "%s" scilab-mode-all-versions)
)
)
(defun scilab-find-prev-line ()
  "Recurse backwards until a code line is found."
  (if (= -1 (forward-line -1)) nil
    (if (or (scilab-ltype-empty)
	    (scilab-ltype-comm-ignore))
	(scilab-find-prev-line) t)))

(defun scilab-prev-line ()
  "Go to the previous line of code.  Return nil if not found."
  (interactive)
  (let ((old-point (point)))
    (if (scilab-find-prev-line) t (goto-char old-point) nil)))

(defun scilab-uniquafy-list (lst)
  "Return a list that is a subset of LST where all elements are unique."
  (let ((nlst nil))
    (while lst
      (if (and (car lst) (not (member (car lst) nlst)))
	  (setq nlst (cons (car lst) nlst)))
      (setq lst (cdr lst)))
    (nreverse nlst)))

; Aki Vehtari <Aki.Vehtari@hut.fi> recommends this: (19.29 required)
;(require 'backquote)
;(defmacro scilab-navigation-syntax (&rest body)
;  "Evaluate BODY with the scilab-mode-special-syntax-table"
;  '(let	((oldsyntax (syntax-table)))
;    (unwind-protect
;	(progn
;	  (set-syntax-table scilab-mode-special-syntax-table)
;	   ,@body)
;      (set-syntax-table oldsyntax))))

(defmacro scilab-navigation-syntax (&rest forms)
  "Set the current environment for syntax-navigation and execute FORMS."
  (list 'let '((oldsyntax (syntax-table))
	       (case-fold-search nil))
	 (list 'unwind-protect
		(list 'progn
		       '(set-syntax-table scilab-mode-special-syntax-table)
			(cons 'progn forms))
		'(set-syntax-table oldsyntax))))

(put 'scilab-navigation-syntax 'lisp-indent-function 0)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec scilab-navigation-syntax def-body)))

(defun scilab-up-list (count &optional restrict)
  "Move forwards or backwards up a list by COUNT.
Optional argument RESTRICT is where to restrict the search."
  ;; Scilab syntax table has no disabling strings or comments.
  (let ((dir (if (> 0 count) -1 +1))
	(origin (point))
	(ms nil))
    ;; Make count positive
    (setq count (* count dir))
    (if (= dir -1)
	(while (/= count 0)
	  ;; Search till we find an unstrung paren object.
	  (setq ms (re-search-backward "\\s(\\|\\s)" restrict t))
	  (while (and (save-match-data (scilab-cursor-in-string-or-comment))
		      (setq ms (re-search-backward "\\s(\\|\\s)" restrict t))))
	  (if (not ms)
	      (progn
		(goto-char origin)
		(error "Scan Error: List missmatch")))
	  ;; View it's match.
	  (let ((s (match-string 0)))
	    (if (string-match "\\s(" s)
		(setq count (1- count))
	      (setq count (1+ count)))))
      (error "Not implemented"))
    ms))

(defun scilab-valid-end-construct-p ()
  "Return non-nil if the end after point terminates a block.
Return nil if it is being used to dereference an array."
  (let ((p (point))
	(err1 t))
    (condition-case nil
	(save-restriction
	  ;; Restrict navigation only to the current command line
	  (save-excursion
	    (scilab-beginning-of-command)
	    (narrow-to-region (point)
			      (progn ;;(scilab-end-of-command (point))
				(end-of-line)
				     (if (> p (point))
					 (progn
					   (setq err1 nil)
					   (error)))
				     (point))))
	  (save-excursion
	    ;; beginning of param list
	    (scilab-up-list -1)
	    ;; backup over the parens.  If that fails
	    (condition-case nil
		(progn
		  (forward-sexp 1)
		  ;; If we get here, the END is inside parens, which is not a
		  ;; valid location for the END keyword.  As such it is being
		  ;; used to dereference array parameters
		  nil)
	      ;; This error means that we have an unterminated paren
	      ;; block, so this end is currently invalid.
	      (error nil))))
      ;; an error means the list navigation failed, which also means we are
      ;; at the top-level
      (error err1))))

;;; Regexps for SCILAB language ===============================================

;; "-pre" means "partial regular expression"
;; "-if" and "-no-if" means "[no] Indent Function"

(defconst scilab-defun-regex "^\\s-*function\\>"
  "Regular expression defining the beginning of a Scilab function.")
(defconst scilab-endfun-regex "^\\s-*endfunction\\>"
  "Regular expression defining (not mandatory) end of a Scilab function.")

(defconst scilab-block-beg-pre-if "function\\|for\\|while\\|if\\|select"
  "Keywords which mark the beginning of an indented block.
Includes function.")

(defconst scilab-block-beg-pre-no-if "for\\|while\\|if\\|select"
  "Keywords which mark the beginning of an indented block.
Excludes function.")

(defconst scilab-not-variable-symbol "[():; ,.^'\"\n-]")

(defun scilab-block-beg-pre ()
  "Partial regular expression to recognize Scilab block-begin keywords."
  (if scilab-indent-function
      scilab-block-beg-pre-if   
    scilab-block-beg-pre-no-if))

(defconst scilab-block-mid-pre
  "elseif\\|else\\|then"
  "Partial regular expression to recognize Scilab mid-block keywords.")

(defconst scilab-block-end-pre-if
  "end\\|endfunction\\|function"
  "Partial regular expression to recognize Scilab block-end keywords.")

(defconst scilab-block-end-pre-no-if
  "end"
  "Partial regular expression to recognize Scilab block-end keywords.")

(defun scilab-block-end-pre ()
  "Partial regular expression to recognize Scilab block-end keywords."
  (if scilab-indent-function
      scilab-block-end-pre-if
    scilab-block-end-pre-no-if))

;; Not used.
;;(defconst scilab-other-pre
;;  "function\\|return"
;;  "Partial regular express to recognize Scilab non-block keywords.")

(defconst scilab-endless-blocks
;  "case\\|else\\|elseif"
   "case"
  "Keywords which initialize new blocks, but don't have explicit ends.
Thus, they are endless.  A new case or else will end a previous
endless block, and and end will end this block, plus any outside normal
blocks.")

(defun scilab-block-re ()
  "Regular expression for keywords which begin Scilab blocks."
  (concat "\\(^\\|[;,]\\)\\s-*\\("
 	  (scilab-block-beg-pre) "\\|"
  	  scilab-block-mid-pre "\\|"
 	  (scilab-block-end-pre) "\\|"
 	  scilab-endless-blocks "\\)\\b"))
  
(defun scilab-block-scan-re ()
  "Expression used to scan over matching pairs of begin/ends."
  (concat "\\(^\\|[;,]\\)\\s-*\\("
 	  (scilab-block-beg-pre) "\\|"
 	  (scilab-block-end-pre) "\\)\\b"))

(defun scilab-block-beg-re ()
  "Expression used to find the beginning of a block."
  (concat "\\(" (scilab-block-beg-pre) "\\)"))

(defun scilab-block-mid-re ()
  "Expression used to find block center parts (like else)."
  (concat "\\(" scilab-block-mid-pre "\\)"))

(defun scilab-block-end-re ()
  "Expression used to end a block.  Usually just `end'."
  (concat "\\(" (scilab-block-end-pre) "\\)"))

(defun scilab-block-end-no-function-re ()
  "Expression representing and end if functions are excluded."
  (concat "\\<\\(" scilab-block-end-pre-no-if "\\)\\>"))

(defun scilab-endless-blocks-re ()
  "Expression of block starters that do not have associated ends."
  (concat "\\(" scilab-endless-blocks "\\)"))

(defconst scilab-cline-start-skip "[ \t]*//[ \t]*"
  "*The regular expression for skipping comment start.")

;;; Lists for scilab keywords =================================================

(defvar scilab-keywords-solo
  '("break" "case" "else" "elseif" "end" "for" "function" "if"
    "abort" "pause" "resume" "quit" "exit"  "return" "select" "while" "then")
  "Keywords that appear on a line by themselves.")


(defvar scilab-keywords-boolean
  '("and" "or" "exist" "isempty" "isequal" "ishold" "isfinite" "isglobal"
    "isinf"  "islogical" "isboolean ""isnan" "isprime" "isreal" "isspace"
    "logical" "isdatelst" "isassarray" "islist" "istlist" "isufun" "isstr" "ispoly" "islib" "isfun")
  "List of keywords that are typically used as boolean expressions.")

 (defvar scilab-core-properties
   '("ButtonDownFcn" "Children" "Clipping" "CreateFcn" "DeleteFcn"
     "BusyAction" "HandleVisibility" "HitTest" "Interruptible"
     "Parent" "Selected" "SelectionHighlight" "Tag" "Type"
     "UIContextMenu" "UserData" "Visible")
   "List of properties belonging to all HG objects.")

 (defvar scilab-property-lists
   '(
     ("xset\\|xget".
      ("alufunction" "background" "clipping" "colormap" "dashes" "default"
       "font" "foreground" "fpf" "hidden3d" "line mode" "mark" "pattern" 
       "pixmap" "thickness" "use color" "viewport" "wdim" "window" "wpos" 
       "wresize" "wshow" "wwpc" "wpos" "lastpattern"))

     ("text\\|title\\|xlabel\\|ylabel\\|zlabel\\|xtitle" .
      ("Color" "EraseMode" "Editing" "Extent" "FontAngle" "FontName"
       "FontSize" "FontUnits" "FontWeight" "HorizontalAlignment"
       "Position" "Rotation" "String" "Units" "Interpreter"
       "VerticalAlignment"))
     ("uicontextmenu" . ("Callback"))
     ("uicontrol" .
      ("BackgroundColor" "callback" "CData" "Enable" "Extent"
       "fontangle" "fontname" "fontsize" "fontunits" "fontweight"
       "ForegroundColor" "HorizontalAlignment" "ListboxTop" "Max" "Min"
       "Position" "String" "Style" "SliderStep" "TooltipString" "Units"
       "Value"))
     ("uimenu" .
      ("label" "windows" "operations" "callback" "ForegroundColor"
       "Label" "Position" "Separator"))
     ;; Flesh this out more later.

     )
   "List of property lists on a per object type basis.")

(defvar scilab-unknown-type-commands
  "[gs]et\\|findobj\\|waitfor"
  "Expression for commands that have unknown types.")

(defun scilab-all-known-properties ()
  "Return a list of all properties."
  (let ((lst scilab-core-properties)
	(tl scilab-property-lists))
    (while tl
      (setq lst (append lst (cdr (car tl)))
	    tl (cdr tl)))
    (scilab-uniquafy-list lst)))

(defvar scilab-all-known-properties (scilab-all-known-properties)
  "List of all the known properties.")


(defmacro scilab-property-function ()
  "Regexp of all builtin functions that take property lists."
  '(let ((r scilab-unknown-type-commands)
	 (tl scilab-property-lists))
     (while tl
       (setq r (concat r "\\|" (car (car tl)))
	     tl (cdr tl)))
     r))


;;; Navigation ===============================================================

(defvar scilab-scan-on-screen-only nil
  "When this is set to non-nil, then forward/backward sexp stops off screen.
This is so the block highlighter doesn't gobble up lots of time when
a block is not terminated.")

(defun scilab-backward-sexp (&optional autoend noerror)
  "Go backwards one balanced set of Scilab expressions.
If optional AUTOEND, then pretend we are at an end.
If optional NOERROR, then we return t on success, and nil on failure."
  (interactive "P")
  (scilab-navigation-syntax
    (if (and (not autoend)
	     (save-excursion (backward-word 1)
			     (or (not
				  (and (looking-at
					(scilab-block-end-no-function-re))
				       (scilab-valid-end-construct-p)))
				 (scilab-cursor-in-string-or-comment))))
	;; Go backwards one simple expression
	(forward-sexp -1)
      ;; otherwise go backwards recursively across balanced expressions
      ;; backup over our end
      (if (not autoend) (forward-word -1))
      (let ((done nil) (start (point)) (returnme t))
	(while (and (not done)
		    (or (not scilab-scan-on-screen-only)
			(pos-visible-in-window-p)))
	  (if (re-search-backward (scilab-block-scan-re) nil t)
	      (progn
		(goto-char (match-beginning 2))
		(if (looking-at (scilab-block-end-no-function-re))
		    (if (or (scilab-cursor-in-string-or-comment)
			    (not (scilab-valid-end-construct-p)))
			nil
		      ;; we must skip the expression and keep searching
		      (forward-word 1)
		      (scilab-backward-sexp))
		  (if (not (scilab-cursor-in-string-or-comment))
		      (setq done t))))
	    (goto-char start)
	    (if noerror
		(setq returnme nil)
	      (error "Unstarted END construct"))))
	returnme))))
  
(defun scilab-forward-sexp ()
    "Go forward one balanced set of Scilab expressions."
  (interactive)
  (scilab-navigation-syntax
    ;; skip over preceeding whitespace
    (skip-chars-forward " \t\n;")
    (if (or (not (looking-at (concat "\\("
				     (scilab-block-beg-pre)
				     "\\)\\>")))
	    (scilab-cursor-in-string-or-comment))
	;; Go forwards one simple expression
	(forward-sexp 1)
      ;; otherwise go forwards recursively across balanced expressions
      (forward-word 1)
      (let ((done nil) (s nil)
	    (expr-scan (scilab-block-scan-re))
	    (expr-look (scilab-block-beg-pre)))
	(while (and (not done)
		    (setq s (re-search-forward expr-scan nil t))
		    (or (not scilab-scan-on-screen-only)
			(pos-visible-in-window-p)))
	  (goto-char (match-beginning 2))
	  (if (looking-at expr-look)
	      (if (scilab-cursor-in-string-or-comment)
		  (forward-word 1)
		;; we must skip the expression and keep searching
		(scilab-forward-sexp))
	    (forward-word 1)
	    (if (and (not (scilab-cursor-in-string-or-comment))
		     (scilab-valid-end-construct-p))
		(setq done t))))
	(if (not s) (error "Unterminated block"))))))

(defun scilab-beginning-of-defun ()
  "Go to the beginning of the current function. If the current pointer is between two functions then go to the beginning of the above function" 
  (interactive)
  (end-of-line)
  (or (re-search-backward scilab-defun-regex nil t)
      (goto-char (point-min))))

(defun scilab-end-of-defun ()
  "Go to the end of the current function. If the current pointer is between
two functions go to the end of the above function"
  (interactive)
  (scilab-beginning-of-defun)
  (or (progn
	(if (looking-at scilab-defun-regex) (goto-char (match-end 0)))
	(if (re-search-forward (concat "\\(" scilab-defun-regex 
           "\\|" scilab-endfun-regex "\\)") nil t)
        (progn
          (beginning-of-line) 
          (if (looking-at scilab-endfun-regex)
           t
           (forward-line -1))
           (point) 
	   )))
      (goto-char (point-max))))

(defun scilab-beginning-of-prev-defun ()
  "Go to the beginning of the previous function."
  (interactive)
  (scilab-beginning-of-defun)
  (forward-line -1)
  (scilab-beginning-of-defun) 
)

(defun scilab-beginning-of-next-defun ()
  "Go to the beginning of the previous function."
  (interactive)
  (scilab-end-of-defun)
  (forward-line 1)
  (beginning-of-line)
  (or (re-search-forward scilab-defun-regex nil t)
      (goto-char (point-max)))
  (beginning-of-line)
)

(defun scilab-function-goto-line (nline)
  "Go to the Nth line  of the current function. If the number is more
then the number of lines into function - go to the end of the function"
  (interactive "NGoto the function's line: " )
  (setq nline (prefix-numeric-value nline))
  (scilab-end-of-defun)
  (let ((ep (point)))
  (scilab-beginning-of-defun)
  (forward-line (- nline 1))
  (if (< ep (point))
    (goto-char ep)))
)
 
(defun scilab-beginning-of-command ()
  "Go to the beginning of an sci command.
Travels across continuations."
  (interactive)
  (beginning-of-line)
  (let ((p nil)
	;; This restriction is a wild guess where to end reverse
	;; searching for array continuations.  The reason is that
	;; scilab-up-list is very slow, and most people would never
	;; put a blank line in a matrix.  Either way, it's worth the
	;; trade off to speed this up for large files.
	;; This list of keywords is NOT meant to be comprehensive.
	(r (save-excursion
	     (re-search-backward
	      "^\\s-*\\(//\\|if\\|else\\(if\\)\\|while\\|for\\|$\\)\\>"
	      nil t))))
    (while (and (or (save-excursion (and (scilab-prev-line)
					 (scilab-lattr-cont)))
		    (setq p (scilab-lattr-array-cont r)))
		(save-excursion (beginning-of-line) (not (bobp))))
      (if p (goto-char p) (scilab-prev-line))
      (setq p nil))
    (back-to-indentation)))

(defun scilab-end-of-command (&optional beginning)
  "Go to the end of an sci command.
Optional BEGINNING is where the command starts from."
  (interactive)
  (while (and (or (scilab-lattr-cont)
		  (save-excursion
		    (forward-line 1)
		    (and (not (eobp))
			 (or (scilab-ltype-continued-comm)
			     (scilab-lattr-array-cont beginning)))))
	      ;; This hack is a short circuit.  If a user did not
	      ;; correctly end a matrix, this will short-circuit
	      ;; as soon as somethng that would never appear in a matrix
	      ;; becomes visible.
	      (not (save-excursion
		     (beginning-of-line)
		     (looking-at (scilab-block-scan-re)))))
    (forward-line 1))
  (end-of-line))


;;; Line types and attributes =================================================

(defun scilab-ltype-empty ()		; blank line
  "Return t if current line is empty."
  (save-excursion
    (beginning-of-line)
    (if (equal (buffer-name) (concat "*" scilab-shell-buffer-name "*"))
        (looking-at (concat "^\\(" comint-prompt-regexp "\\)?[ \t]*$"))
    (looking-at "^[ \t]*$"))))

(defun scilab-ltype-comm ()		; comment line
  "Return t if current line is a SCILAB comment line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*//.*$")))

(defun scilab-ltype-comm-ignore ()	; comment out a region line
  "Return t if current line is a SCILAB comment region line."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "[ \t]*" scilab-comment-region-s))))

(defun scilab-ltype-help-comm ()
  "Return t if the current line is part of the SCILAB help comment."
  (save-excursion
    (if (not (scilab-ltype-comm))
	nil
      (while (and (scilab-ltype-comm) (not (bobp))
		  (scilab-prev-line))
	(beginning-of-line))
      (scilab-ltype-function-definition))))

(defun scilab-ltype-endfunction-comm ()
  "Return t if the current line is an ENDFUNCTION style comment."
  (save-excursion
    (if (not (scilab-ltype-comm))
	nil
      (beginning-of-line)
      (if (looking-at "^[ \t]*//[ \t]*endfunction")
	  t
	(while (and (or (scilab-ltype-comm)
			(scilab-ltype-empty))
		    (not (eobp)))
	  (forward-line 1))
	(scilab-ltype-function-definition)))))

(defun scilab-ltype-continued-comm ()
  "Return column of previous line's comment start, or nil."
  (save-excursion
    (beginning-of-line)
    (if (or (not (scilab-ltype-comm)) (bobp))
	nil
      ;; We use forward-line and not scilab-prev-line because
      ;; we want blank lines to terminate this indentation method.
      (forward-line -1)
      (let ((col  (scilab-lattr-comm)))
	(if col
	    (progn
	      (goto-char col)
	      (current-column))
	  nil)))))

(defun scilab-ltype-function-definition ()
  "Return t if the current line is a function definition."
  (save-excursion
    (beginning-of-line)
    (looking-at scilab-defun-regex)))

(defun scilab-ltype-code ()		; line of code
  "Return t if current line is a SCILAB code line."
  (and (not (scilab-ltype-empty)) (not (scilab-ltype-comm))))

(defun scilab-lattr-comm ()		; line has comment
  "Return t if current line contain a comment."
  (save-excursion (scilab-comment-on-line)))

(defun scilab-lattr-cont ()		; line has continuation
  "Return non-nil if current line ends in ... and optional comment."
  (save-excursion
    (beginning-of-line)
    (and (re-search-forward "[^ \t.][ \t]*\\.\\.+[ \t]*\\(//.*\\)?$"
			    (scilab-point-at-eol) t)
	 (progn (goto-char (match-beginning 0))
		(not (scilab-cursor-in-comment))))))

(defun scilab-lattr-array-cont (&optional restrict)
  "Return non-nil if current line is in an array.
If the entirety of the array is on this line, return nil.
Optional option RESTRICT is the distrance to restrict the search."
  (condition-case nil
      (save-excursion
	(beginning-of-line)
	(scilab-up-list -1 restrict)
	(and (looking-at "[[{]") (point)))
    (error nil)))

(defun scilab-lattr-array-end ()
  "Return non-nil if the current line closes an array.
by close, the first character is the end of an array."
  (save-excursion
    (back-to-indentation)
    (and (looking-at "[]}]") (scilab-lattr-array-cont))))

(defun scilab-lattr-block-cont (&optional eol)
  "Return a number representing the number of unterminated block constructs.
This is any block, such as if, or for that doesn't have an END on this line.
Optional EOL indicates a virtual end of line."
  (let ((v 0))
    (save-excursion
      (beginning-of-line)
      (save-restriction
	(narrow-to-region (point) (or eol (scilab-point-at-eol)))
	(scilab-navigation-syntax
	  (while (re-search-forward (concat "\\<" (scilab-block-beg-re) "\\>")
				    nil t)
	    (if (scilab-cursor-in-string-or-comment)
		;; Do nothing
		nil
	      ;; Increment counter, move to end.
	      (setq v (1+ v))
	      (let ((p (point)))
		(forward-word -1)
		(condition-case nil
		    (progn
		      (scilab-forward-sexp)
		      (setq v (1- v)))
		  (error (goto-char p))))))
	  (if (= v 0) nil v))))))

(defun scilab-lattr-middle-block-cont ()
  "Return the number of middle block continuations.
This should be 1 or nil, and only true if the line starts with one of these
special items."
  (save-excursion
    (back-to-indentation)
    (if (looking-at (concat (scilab-block-mid-re) "\\>"))
	1
      nil)))

(defun scilab-lattr-endless-block-cont ()
  "Return the number of middle block continuations.
This should be 1 or nil, and only true if the line starts with one of these
special items."
  (save-excursion
    (back-to-indentation)
    (if (looking-at (concat (scilab-endless-blocks-re) "\\>"))
	1
      nil)))

(defun scilab-lattr-block-close ()
  "Return the number of closing block constructs. (not used yet)."
  (let ((v 0))
    (save-excursion
      (end-of-line)
      (save-restriction
	(narrow-to-region (scilab-point-at-bol) (point))
	(while (and (re-search-backward (scilab-block-end-re) nil t)
		    (scilab-valid-end-construct-p))
	  (setq v (1+ v))
	  (condition-case nil
	      (progn
		(scilab-backward-sexp t)
		(setq v (1- v)))
	    (error nil)))
	(if (= v 0) nil v)))))

(defun scilab-lattr-local-end ()
  "Return t if this line begins with an end construct."
  (save-excursion
    (back-to-indentation)
    (let ((scilab-indent-function nil))
      (and (looking-at (concat "\\<" (scilab-block-end-re) "\\>"))
	   (scilab-valid-end-construct-p)))))

(defun scilab-lattr-semantics (&optional prefix)
  "Return the semantics of the current position.
Values are nil 'solo, 'value, and 'boolean.  Boolean is a subset of
value.  nil means there is no semantic content (ie, string or comment.)
If optional PREFIX, then return 'solo if that is the only thing on the
line."
  (let ((pref "") (str "") (pathstr nil) (yeslist t))
    (if prefix (setq pref prefix))
    (if (equal (buffer-name) (concat "*" scilab-shell-buffer-name "*"))
       (setq str comint-prompt-regexp))
;  (if prefix (setq prefix (regexp-quote prefix)))
    (cond ;((scilab-cursor-in-string-or-comment)
	 ;nil)
	((or (scilab-ltype-empty)
             (member (preceding-char) '(?  ?\t ?\n ?, ?\( ?\[ ?\' ?\"))
;            (save-excursion
;            (beginning-of-line)
;	    (looking-at ".*[]([,)}{]$"))
         )
  	 nil)
	((save-excursion
           (setq pathstr 
               (buffer-substring-no-properties
                 (save-excursion
                 (beginning-of-line)           	   
                 (point))
                 (point)))       
	   (string-match  (concat ".*" "[\t ,([]" scilab-path-type-regexp "$") pathstr))
        	 'disk)

	((and prefix (scilab-shell-active-p) (save-excursion
	   (beginning-of-line)
   	   (if (not (string-match "\\(%\\(\\w\\|[$_]\\)*\\|\\(\\w\\|[$_]\\)+\\)\\.\\(\\w\\|[$_.]\\)*" prefix))
                nil
                  (let* (( lst (match-string  1 prefix))
                   (scistr (concat "type(" lst ")==16"))
                   (res  (scilab-shell-collect-command-output scistr)))
                  (if (string-match "T" res) t (setq yeslist nil)))
           )
         ))
	 'list)
        ((and prefix (string-match "\\." prefix)) nil)
	((and prefix (save-excursion
			   (beginning-of-line)
			   (looking-at (concat "\\(" str "\\)?\\s-*" prefix "\\s-*$"))))
  	 'solo)
;;;	((save-excursion
;	   (scilab-beginning-of-command)
;;;	   (beginning-of-line)
;;;	   (looking-at (concat "\\(" str "\\)?\\s-*\\(if\\|elseif\\|while\\|then\\)\\>")))
;;;	 'boolean)
	((save-excursion
;	   (scilab-beginning-of-command)
	   (beginning-of-line)
	   (looking-at (concat "\\(" str 
                                "\\)?\\s-*\\(" 
                              (scilab-property-function)
	                       "\\)\\>")))
	 'property)
	(t
	 'value))))




(defun scilab-function-called-at-point ()
  "Return a string representing the function called nearby point."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "\\s-*\\([a-zA-Z]\\w+\\)[^=][^=]")
	   (match-string 1))
	  ((and (re-search-forward "=" (scilab-point-at-eol) t)
		(looking-at "\\s-*\\([a-zA-Z]\\w+\\)\\s-*[^=]"))
	   (match-string 1))
	  (t nil))))

(defun scilab-cursor-in-string-or-comment ()
  "Return t if the cursor is in a valid Scilab comment or string."
  ;; comment and string depend on each other.  Here is one test
  ;; that does both.
  (save-restriction
    (narrow-to-region (scilab-point-at-bol) (scilab-point-at-eol))
    (let ((p (1+ (point)))
	  (returnme nil)
	  (sregex (concat scilab-string-start-regexp "['\"]"))
          (prechar nil)
          (prechar2 nil))
      (save-excursion
	(goto-char (point-min))
	(while (and (re-search-forward
		     (concat "['\"]\\|//\\|" (regexp-quote scilab-elipsis-string))
		     nil t)
		    (<= (point) p))
          (setq prechar (preceding-char))
          (backward-char 1)
          (setq prechar2 (preceding-char))
          (backward-char -1)
	  (if (or (and (= ?/ prechar) (= ?/ prechar2))
		  (= ?. (preceding-char)))
	      ;; Here we are in a comment for the rest of it.
	      (progn
		(goto-char p)
		(setq returnme t))
	    ;; Here, we could be a string start, or transpose...
	    (if (or (= (current-column) 1)
		    (save-excursion (forward-char -2)
				    (looking-at sregex)))
		;; a valid string start, find the end
		(let ((f (re-search-forward scilab-string-end-regexp nil t)))
		  (if f
		      (setq returnme (> (point) p))
		    (setq returnme t)))
	      ;; Ooops, a transpose, keep going.
	      ))))
      returnme)))

(defun scilab-cursor-in-comment ()
  "Return t if the cursor is in a valid Scilab comment."
  (save-match-data
    (save-restriction
      (narrow-to-region (scilab-point-at-bol) (scilab-point-at-eol))
      (save-excursion
	(let ((prev-match nil))
	  (while (and (re-search-backward
		       (concat "//\\|" (regexp-quote scilab-elipsis-string) "+")
		       nil t)
		      (not (scilab-cursor-in-string)))
	    (setq prev-match (point)))
	  (if (and prev-match (scilab-cursor-in-string))
	      (goto-char prev-match))
	  (and (looking-at (concat "//\\|"
				   (regexp-quote scilab-elipsis-string)))
	       (not (scilab-cursor-in-string))))))))

(defun scilab-cursor-in-string (&optional incomplete)
  "Return t if the cursor is in a valid Scilab string.
If the optional argument INCOMPLETE is non-nil, then return t if we
are in what could be a an incomplete string."
  (let ((m (match-data))
	(returnme nil))
    (save-restriction
      (narrow-to-region (scilab-point-at-bol) (scilab-point-at-eol))
      (let ((p (1+ (point)))
	    (sregex (concat scilab-string-start-regexp "['\"]"))
	    (instring nil)
            (prechar nil)
            (prechar2 nil))
	(save-excursion
	  ;; Comment hunters need strings to not call the comment
	  ;; identifiers.  Thus, this routines must be savvy of comments
	  ;; without recursing to them.
	  (goto-char (point-min))
	  (while (or (and instring (looking-at "['\"]"))
		     (and (re-search-forward
			   (concat "['\"]\\|//\\|"
				   (regexp-quote scilab-elipsis-string))
			   nil t)
			  (<= (point) p)
			  ;; Short circuit to fix this.
			  (progn (setq instring nil) t)))
	    ;; The next line emulates re-search-foward
	    (if instring (goto-char (match-end 0)))
            (setq prechar (preceding-char))
            (backward-char 1)
            (setq prechar2 (preceding-char))
            (backward-char -1)
	    (if (or (and (= ?/ prechar) (= ?/ prechar2))
		  (= ?. (preceding-char)))
		;; Here we are in a comment for the rest of it.
		;; thus returnme is a force-false.
		(goto-char p)
	      ;; Here, we could be in a string start, or transpose...
	      (if (or (= (current-column) 1)
		      instring
		      (save-excursion (forward-char -2)
				      (looking-at sregex)))
		  ;; a valid string start, find the end
		  (let ((f (re-search-forward scilab-string-end-regexp nil t)))
		    (if (and (not f) incomplete)
			(setq returnme t)
		      (setq returnme (> (point) p))
		      (setq instring t)))
		;; Ooops, a transpose, keep going.
		))))))
    (set-match-data m)
    returnme))
  

(defun scilab-comment-on-line ()
  "Place the cursor on the beginning of a valid comment on this line.
If there isn't one, then return nil, point otherwise."
  (interactive)
  (let ((eol (scilab-point-at-eol))
	(p (point))
	(signal-error-on-buffer-boundary nil))
    (beginning-of-line)
    (while (and (re-search-forward "//" eol t)
		(save-excursion (forward-char -2) (scilab-cursor-in-string t))))
    (if (not (bolp)) (forward-char -2))
    (if (looking-at "//")
	(point)
      (goto-char p)
      nil)))

;;; Indent functions ==========================================================

(defun scilab-indent-line ()
  "Indent a line in `scilab-mode'."
  (interactive)
  (let ((i (scilab-calc-indent))
	(c (current-column)))
    (save-excursion
      (back-to-indentation)
      (if (= i (current-column))
	  nil
	(beginning-of-line)
	(delete-horizontal-space)
	(indent-to i))
      ;; If line contains a comment, format it.
      (if () (if (scilab-lattr-comm) (scilab-comment))))
    (if (<= c i) (move-to-column i))))

(defun scilab-indent-defun ()
  "Indent correspondently all lines in the current function body in `scilab-mode'"
  (interactive)
  (save-excursion 
    (let (
	  (start (scilab-beginning-of-defun))
	  (end  (scilab-end-of-defun)))
      (indent-region start end nil)
      )
))

(defun scilab-calc-indent ()
  "Return the appropriate indentation for this line as an integer."
  (interactive)
   (if (save-excursion 
       (beginning-of-line)
       (looking-at "^\\s-*endfunction"))
       (save-excursion
           (scilab-beginning-of-defun)
           (looking-at "\\s-*\\(function\\)")
           (- (match-beginning 1) (point))
           )
      (let* ((tli (save-excursion (scilab-beginning-of-command)
		     	      (scilab-point-at-bol)))
	      (ci (save-excursion (or (scilab-prev-line)
				 (progn (beginning-of-line)
					(forward-line -1)))
			         (scilab-beginning-of-command)
			          (scilab-next-line-indentation)))
	       (sem (scilab-calculate-indentation (if (>= ci tli) 0 ci))))
    ;; simplistic
       (nth 1 sem))
   )
)
    
(defun scilab-calculate-indentation (current-indentation)
  "Calculate out the indentation of the current line.
Return a list of descriptions for this line.  Return format is:
 '(TYPE DEPTHNUMBER)
where TYPE is one of (comment, code, function, blockstart, blockmid,
blockendless, blockend) DEPTHNUMBER is how many characters to indent
this line.
  Argument CURRENT-INDENTATION is what the previous line thinks
this line's indentation should be."
  (let ((ci current-indentation)
	(tmp nil))
    (cond
     ;; COMMENTS
     ((scilab-ltype-comm)
      (cond
       ;; HELP COMMENT and COMMENT REGION
       ((or (scilab-ltype-help-comm)
	    (scilab-ltype-comm-ignore))
	(list 'comment-help 0))
       ;; COMMENT Continued From Previous Line
       ((setq tmp (scilab-ltype-continued-comm))
	(list 'comment tmp))
       ;; END FUNCTION COMMENT
       ((scilab-ltype-endfunction-comm)
	(list 'comment-endfunction 0))
       (t
	(list 'comment ci))))
     ;; FUNCTION DEFINITION
     ((scilab-ltype-function-definition)
      (list 'function 0))
     ;; END keyword
     ((scilab-lattr-local-end)
      (list 'blockend (save-excursion
			(beginning-of-line)
			(condition-case nil
			    (progn
			      (scilab-backward-sexp t)
			      (if (scilab-ltype-function-definition) (error ""))
			      (forward-word 1) ;; skip this match when counting
			      (+ (current-indentation)
				 (* (1- (scilab-lattr-block-cont (point)))
				    scilab-indent-level)))
			  (error (error "Unmatched end"))))))
     ;; ELSE/CATCH keywords
     ((scilab-lattr-middle-block-cont)
      (let ((m (match-string 1)))
	(list 'blockmid
	      (condition-case nil
		  (save-excursion
		    (beginning-of-line)
		    (scilab-backward-sexp t)
		    (if (scilab-ltype-function-definition) (error ""))
		    (current-column))
		(error (error "Unmatched %s" m))))))
     ;; CASE/OTHERWISE keywords
     ((scilab-lattr-endless-block-cont)
      (list 'blockendless
	    (condition-case nil
		(save-excursion
		  (beginning-of-line)
		  (scilab-backward-sexp t)
		  (if (not (looking-at "select\\>")) (error ""))
		  (+ (current-column)
		     (if (listp scilab-case-level)
			 (car scilab-case-level)
		       scilab-case-level)))
	      (error (error "Unmatched case/else part")))))
     ;; End of a MATRIX
     ((scilab-lattr-array-end)
      (list 'array-end (save-excursion
			(back-to-indentation)
			(scilab-up-list -1)
			(let* ((fc (following-char))
			       (mi (assoc fc scilab-maximum-indents))
			       (max (if mi (if (listp (cdr mi))
					       (car (cdr mi)) (cdr mi))
				      nil))
			       (ind (if mi (if (listp (cdr mi))
					       (cdr (cdr mi)) (cdr mi))
				      nil)))
			  ;; apply the maximum limits.
			  (if (and ind (> (- (current-column) ci) max))
			      (1- ind) ; decor
			    (current-column))))))
     ;; Code lines
     ((save-excursion
	(beginning-of-line)
	(back-to-indentation)
	(= (point) (progn (scilab-beginning-of-command) (point))))
      ;; This means we are at the beginning of a command structure.
      ;; Always match up against the previous line.
      (list 'code ci))
     ;; Lines continued from previous statements.
     (t
      (list (if (scilab-ltype-empty) 'empty
	      (if (scilab-lattr-array-cont) 'array-cont 'code))
	    (condition-case nil
		;; Line up with opening paren/brace/bracket
		(let ((boc (save-excursion
			     (scilab-beginning-of-command)
			     (point))))
		  (save-excursion
		    (beginning-of-line)
		    (scilab-up-list -1)
		    (if (> boc (point)) (error nil))
		    ;; Ok, it MIGHT be that we are in a program
		    ;; statement, and this particular command is an HG
		    ;; statement that would look better if the
		    ;; following lines lined up AFTER the first
		    ;; argument.  Lets look.
		    (let ((parendepth (current-column)))
		      (cond 
; ;(
; ;                              (and (= (following-char) ?\( )
; ; 				  (save-excursion
; ; 				    (scilab-navigation-syntax
; ; 				      (forward-word -1)
; ; 				      (looking-at
; ; 				       scilab-indent-past-arg1-functions)))
; ; 				  (let ((start-paren (point)))
; ; 				    (while
; ; 					(and
; ; 					 (re-search-forward
; ; 					  "," (scilab-point-at-eol) t)
; ; 					 (save-excursion
; ; 					   (scilab-up-list -1)
; ; 					   (> (point) start-paren)))
; ; 				    (if (and
; ; 					 (= (preceding-char) ?,)
; ; 					 ;; Don't bother if we hit the EOL.
; ; 					 (not (looking-at

; ; 					       "\\s-*\\(\\.\\.\\.\\|$\\|)\\)")))
; ; 					t
; ; 				      (move-to-column parendepth)
; ; 				      nil)))
; ; 			     (skip-chars-forward " \t")
; ; 			     (if (> (- (current-column) parendepth)
; ; 				    scilab-arg1-max-indent-length)
; ; 				 (+ parendepth scilab-arg1-max-indent-length)
; ; 			       (current-column))))
			    (t
			     (let* ((fc (following-char))
				    (mi (assoc fc scilab-maximum-indents))
				    (max (if mi
					     (if (listp (cdr mi))
						 (car (cdr mi)) (cdr mi))
					   nil))
				    (ind (if mi
					     (if (listp (cdr mi))
						 (cdr (cdr mi)) (cdr mi))
					   nil)))
			       (forward-char 1)
			       (skip-chars-forward " \t")
			       ;; If we are at the end of a line and
			       ;; this open paren is there, then we
			       ;; DONT want to indent to it.  Use the
			       ;; standard indent.
			       (if (looking-at "\\.\\.\\.\\|$")
				   ;; This could happen in another set
				   ;; of matricies.  Find a current
				   ;; indentation based on the
				   ;; previous line.
				   (let ((cci (current-indentation)))
				     (+ cci scilab-cont-level))
				 ;; apply the maximum limits.
				 (if (and ind (> (- (current-column) ci) max))
				     ind
				   (current-column)))))))))
	      (error
	       ;; Line up to an equals sign.
	       (save-excursion
		 (scilab-beginning-of-command)
		 (while (and (re-search-forward "=" (scilab-point-at-eol) t)
			     (scilab-cursor-in-string-or-comment)))
		 (if (/= (preceding-char) ?=)
		     (+ ci scilab-cont-level)
		   (skip-chars-forward " \t")
		   (let ((cc (current-column))
			 (mi (assoc ?= scilab-maximum-indents)))
		     (if (looking-at "\\.\\.\\.\\|$")
			 ;; In this case, the user obviously wants the
			 ;; indentation to be somewhere else.
			 (+ ci (cdr (cdr mi)))
		       ;; If the indent delta is greater than the max,
		       ;; use the max + currenti
		       (if (and mi (> (- cc ci) (if (listp (cdr mi))
						    (car (cdr mi))
						  (cdr mi))))
			   (setq cc (+ ci (if (listp (cdr mi))
					      (cdr (cdr mi))
					    (cdr mi)))))
		       cc))))))))
     )))

(defun scilab-next-line-indentation ()
  "Calculate the indentation for lines preceeding this command line."
  (let ((bc (scilab-lattr-block-cont))
	(mc (scilab-lattr-middle-block-cont))
	(ec (scilab-lattr-endless-block-cont))
	(hc (and scilab-indent-function (scilab-ltype-help-comm))))
    (+ (current-indentation)
       (* scilab-indent-level (or bc 0))
       (* scilab-indent-level (or mc 0))
       (* (if (listp scilab-case-level)
	      (cdr scilab-case-level) scilab-case-level)
	  (or ec 0))
       (if hc scilab-indent-level 0))))




;;; The return key ============================================================

(defcustom scilab-return-function 'scilab-indent-end-before-ret
;;(defcustom scilab-return-function      'scilab-double-plain-ret
  "Function to handle return key.
Must be one of:
    'scilab-plain-ret
    'scilab-double-plain-ret
    'scilab-indent-after-ret
    'scilab-indent-end-before-ret
    'scilab-indent-before-ret"
  :group 'scilab
  :type '(choice (function-item scilab-plain-ret)
                 (function-item scilab-double-plain-ret)
		 (function-item scilab-indent-after-ret)
		 (function-item scilab-indent-end-before-ret)
		 (function-item scilab-indent-before-ret)))

(defun scilab-return ()
  "Handle carriage return in `scilab-mode'."
  (interactive)
  (funcall scilab-return-function))

(defun scilab-plain-ret ()
  "Vanilla new line."
  (interactive)
  (newline))
  
(defun scilab-indent-after-ret ()
  "Indent after new line."
  (interactive)
  (newline)
  (if scilab-dynamical-indent
  (scilab-indent-line)))

(defun scilab-double-plain-ret ()
  "Indent after new line."
  (interactive)
  (newline)
  (newline))

(defun scilab-indent-end-before-ret ()
  "Indent line if block end, start new line, and indent again."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at (concat "^\\s-*\\(" (scilab-block-end-re)
			    "\\|" (scilab-block-mid-re)
			    "\\|" (scilab-endless-blocks-re)
			    "\\)")))
      (condition-case nil
	  (if scilab-dynamical-indent (scilab-indent-line))
	(error nil)))
  (newline)
  (if scilab-dynamical-indent
  (scilab-indent-line)))

(defun scilab-indent-before-ret ()
  "Indent line, start new line, and indent again."
  (interactive)
  (if scilab-dynamical-indent (scilab-indent-line))
  (newline)
  (if scilab-dynamical-indent (scilab-indent-line)))

(defun scilab-linefeed ()
  "Handle line feed in `scilab-mode'.
Has effect of `scilab-return' with (not scilab-indent-before-return)."
  (interactive)
  (scilab-indent-line)
  (newline)
  (scilab-indent-line))

(defun scilab-comment-return ()
  "Handle carriage return for Scilab comment line."
  (interactive)
  (cond
   ((scilab-ltype-comm)
    (scilab-set-comm-fill-prefix) (newline) (if fill-prefix (insert fill-prefix))
    (scilab-reset-fill-prefix) (scilab-indent-line))
   ((scilab-lattr-comm)
    (newline) (indent-to comment-column)
    (insert scilab-comment-on-line-s))
   (t
    (newline) (scilab-comment) (scilab-indent-line))))

(defun scilab-comm-from-prev ()
  "If the previous line is a comment-line then set up a comment on this line."
  (save-excursion
    ;; If the previous line is a comment-line then set the fill prefix from
    ;; the previous line and fill this line.
    (if (and (= 0 (forward-line -1)) (scilab-ltype-comm))
	(progn
	  (scilab-set-comm-fill-prefix)
	  (forward-line 1) (beginning-of-line)
	  (delete-horizontal-space)
	  (if (looking-at "//") (delete-char 2))
	  (delete-horizontal-space)
	  (insert fill-prefix)
	  (scilab-reset-fill-prefix)))))

;;; Comment management========================================================
(defun scilab-comment ()
  "Add a comment to the current line."
  (interactive)
  (cond ((scilab-ltype-empty)		; empty line
	 (scilab-comm-from-prev)
	 (if (scilab-lattr-comm)
	     (skip-chars-forward " \t//")
	   (insert scilab-comment-line-s)
	   (scilab-indent-line)))
	((scilab-ltype-comm)		; comment line
	 (scilab-comm-from-prev)
	 (skip-chars-forward " \t//"))
	((scilab-lattr-comm)		; code line w/ comment
	 (beginning-of-line)
;	 (re-search-forward "[^//]//[ \t]")
	 (re-search-forward "//")
	 (forward-char -2)
	 (if (> (current-column) comment-column) (delete-horizontal-space))
	 (if (< (current-column) comment-column) (indent-to comment-column))
	 (skip-chars-forward "// \t"))
	(t				; code line w/o comment
	 (end-of-line)
	 (re-search-backward "[^ \t\n^]" 0 t)
	 (forward-char)
	 (delete-horizontal-space)
	 (if (< (current-column) comment-column)
	     (indent-to comment-column)
	   (insert " "))
	 (insert scilab-comment-on-line-s))))

(defun scilab-comment-line-break-function (&optional soft)
  "Break the current line, and if in a comment, continue it.
Optional argument SOFT indicates that the newline is soft, and not hard."
  (interactive)
  (if (not (scilab-cursor-in-comment))
      (scilab-return)
    ;; Will the below fn work in old emacsen?
    (if soft (insert-and-inherit ?\n) (newline 1))
    (insert "// ")
    (scilab-indent-line)
    (end-of-line)))

(defun scilab-comment-indent ()
  "Indent a comment line in `scilab-mode'."
  (scilab-calc-indent))

(defun scilab-comment-region (beg-region end-region)
  "Comments every line in the region.
Puts `scilab-comment-region-s' at the beginning of every line in the region.
BEG-REGION and END-REGION are arguments which specify the region boundaries.
With non-nil ARG, uncomments the region."
  (interactive "*r")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (insert scilab-comment-region-s)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert scilab-comment-region-s))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun scilab-uncomment-region (beg-region end-region)
  "Comments every line in the region.
Remove  `scilab-comment-region-s' at the beginning of every line in the region.
Do it only once. Opposite action to the `scilab-comment-region'. END-REGION are arguments which specify the region boundaries."
  (interactive "*r")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (let ((com (regexp-quote scilab-comment-region-s))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0)))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))



;;; Filling ===================================================================

(defun scilab-set-comm-fill-prefix ()
  "Set the `fill-prefix' for the current (comment) line."
  (interactive)
  (if (scilab-lattr-comm)
      (setq fill-prefix
	    (save-excursion
	      (beginning-of-line)
	      (let ((e (scilab-point-at-eol))
		    (pf nil))
		(while (and (re-search-forward "//[ \t]*\\($$$ \\)?" e t)
			    (scilab-cursor-in-string)))
		(setq pf (match-string 0))
		(concat (make-string (- (current-column) (length pf)) ? )
			pf))))))

(defun scilab-set-comm-fill-prefix-post-code ()
  "Set the `fill-prefix' for the current post-code comment line."
  (interactive)
  (scilab-set-comm-fill-prefix))

(defun scilab-reset-fill-prefix ()
  "Reset the `fill-prefix'."
  (setq fill-prefix nil))

(defun scilab-find-convenient-line-break ()
  "For the current line, position the cursor where we want to break the line.
Basically, spaces are best, then operators.  Always less than `fill-column'
unless we decide we can fudge the numbers.  Return nil if this line should
not be broken.  This function will ONLY work on code."
  ;; First of all, if this is a continuation, then the user is
  ;; requesting that we don't mess with his stuff.
  (if (scilab-lattr-cont)
      nil
    (save-restriction
      (narrow-to-region (scilab-point-at-bol) (scilab-point-at-eol))
      ;; get ourselves onto the fill-column.
      (move-to-column fill-column)
      (let ((pos nil)
	    (orig (point)))
	(or
	 ;; Next, if we have a trailing comment, use that.
	 (progn (setq pos (or (scilab-lattr-comm) (scilab-point-at-bol)))
		(goto-char pos)
		(if (and (> (current-column) (- fill-column scilab-fill-fudge))
			 (< (current-column) (+ fill-column scilab-fill-fudge)))
		    t
		  (goto-char orig)
		  nil))
	 ;; Now, lets find the nearest space (after or before fill column)
	 (let* ((after (save-excursion
			 (re-search-forward "[ \t]" nil t)))
		(before (save-excursion
			  (re-search-backward "[ \t]" nil t)))
		(afterd (- (or after (scilab-point-at-eol)) (point)))
		(befored (- (point) (or before (scilab-point-at-bol)))))
	   ;; Here, if "before" is actually the beginning of our
	   ;; indentation, then this is most obiously a bad place to
	   ;; break our lines.
	   (if before
	       (save-excursion
		 (goto-char before)
		 (if (<= (point) (save-excursion
				   (back-to-indentation)
				   (point)))
		     (setq before nil))))
	   (cond ((and after
		       (< afterd scilab-fill-fudge)
		       (< afterd befored))
		  (goto-char after)
		  t)
		 ((and before
		       (< befored scilab-fill-fudge)
		       (< befored afterd))
		  (goto-char before)
		  t)
		 (t (goto-char orig)
		    nil)))
	 ;; Now, lets find the nearest backwards
	 (progn
	   (re-search-backward "\\(\\s-\\|\\s.\\)+" nil t)
	   (while (and (looking-at "\\^\\|\\.\\|'")
		       (re-search-backward "\\(\\s-\\|\\s.\\)+" nil t)))
	   (if (or (not (looking-at "\\(\\s-\\|\\s.\\)+"))
		   (<= (point) (save-excursion
				 (back-to-indentation)
				 (point))))
	       (progn
		 ;; We failed in our mission to find anything, or fell
		 ;; of the edge of the earth.  If we are out of
		 ;; bounds, lets try again.
		 (goto-char orig)
		 (if (re-search-backward "\\s.+" nil t)
		     t
		   nil))
	     ;; Ok, we have a good location to break.  Check for column
	     ;; and ref against nearest list ending to predict a possibly
	     ;; better break point.
	     (forward-char 1)
	     (let ((okpos (current-column))
		   (startlst (save-excursion
			       (condition-case nil
				   (scilab-up-list -1)
				 (error nil))
			       (if (save-excursion
				     (forward-char -1)
				     (looking-at "\\w"))
				   (forward-word -1))
			       (current-column)))
		   (endlst (save-excursion
			     (condition-case nil
				 (scilab-up-list 1)
			       (error nil))
			     (current-column))))
	       ;; When evaluating list fudge factores, breaking on the
	       ;; edge of a list, or at the beginning of a function
	       ;; call can be more valuable than breaking on a symbol
	       ;; of a mid-sized list.  As such, allow double-fudge
	       ;; for lists.
	       (cond
		;; First, pick the end of a list.
		((and (< endlst scilab-fill-fudge-hard-maximum)
		      (<= endlst (+ fill-column scilab-fill-fudge))
		      (or (<= (* scilab-fill-fudge 2) (- endlst okpos))
			  (<= endlst fill-column))
		      (save-excursion
			(move-to-column endlst)
			(not (looking-at "\\^"))))
		 (move-to-column endlst)
		 t)
		;; Else, back up over this list and poke around
		((>= (* 2 scilab-fill-fudge) (- okpos startlst))
		 (move-to-column startlst)
		 t)
		;; Oh well, just do this symbol.
		(t (move-to-column okpos)
		   t)))))
	 ;; Well, this just sucks
	 (progn (goto-char orig)
		nil))))))

(defun scilab-auto-fill ()
  "Do auto filling.
Set variable `auto-fill-function' to this symbol to enable Scilab style auto
filling which will automatically insert `...' and the end of a line."
  (interactive)
  (let ((fill-prefix fill-prefix) ;; safe way of modifying fill-prefix.
	(fill-column (- fill-column
			(if scilab-fill-count-ellipsis-flag
			    (save-excursion
			      (move-to-column fill-column)
			      (if (not (bobp))
				  (forward-char -1))
			      (if (scilab-cursor-in-string 'incomplete)
				  4 3))
			  0))))
    (if (> (current-column) fill-column)
	(cond
	 ((scilab-ltype-comm-ignore)
	  nil)
	 ((or (scilab-ltype-comm)
	      (and (save-excursion (move-to-column fill-column)
				   (scilab-cursor-in-comment))
		   (scilab-lattr-comm)))
	  ;; If the whole line is a comment, do this.
	  (scilab-set-comm-fill-prefix) (do-auto-fill)
	  (scilab-reset-fill-prefix))
	 ((and (scilab-ltype-code)
	       (not (scilab-lattr-cont))
	       scilab-fill-code)
	  ;; If we are on a code line, we ellipsify before we fill.
	  (let ((m (make-marker)))
	    (move-marker m (point))
	    (set-marker-insertion-type m t)
	    (if (not (scilab-find-convenient-line-break))
		nil
	      (if (not (and (scilab-cursor-in-string 'incomplete)
			    (save-excursion
			      (forward-char -1)
			      (scilab-cursor-in-string 'incomplete))))
		  (progn
		    (delete-horizontal-space)
		    (insert " " scilab-elipsis-string "\n")
		    (scilab-indent-line))
		;; we are guaranteed to be in an incomplete string.
		(if scilab-fill-strings-flag
		    (let ((pos (point))
			  (pos2 nil))
		      (while (and (re-search-backward "['\"]" nil t)
				  (progn (forward-char -1)
					 (looking-at "['\"]['\"]"))))
		      (setq pos2 (point))
		      (if (not (looking-at "\\["))
			  (progn
			    (skip-chars-backward " \t")
			    (forward-char -1)))
		      (if (looking-at "\\[")
			  (goto-char pos)
			(goto-char pos2)
			(forward-char 1)
;			(insert "[")
			(goto-char pos)
			(forward-char 1))
		      (delete-horizontal-space)
		      (insert "\"+" scilab-elipsis-string "\n")
;		      (insert "'+" scilab-elipsis-string "\n")
		      (scilab-indent-line)
;		      (insert "' ")))))
		      (insert "\" ")))))
	    (goto-char m)))
	 ))))

(defun scilab-join-comment-lines ()
  "Join current comment line to the next comment line."
  ;; New w/ V2.0: This used to join the previous line, but I could find
  ;; no editors that had a "join" that did that.  I modified join to have
  ;; a behaviour I thought more inline with other editors.
  (interactive)
  (end-of-line)
  (if (looking-at "\n[ \t]*//")
      (replace-match " " t t nil)
    (error "No following comment to join with")))

(defun scilab-fill-region (beg-region end-region &optional justify-flag)
  "Fill the region between BEG-REGION and END-REGION.
Non-nil JUSTIFY-FLAG means justify comment lines as well."
  (interactive "*r\nP")
  (let ((end-reg-mk (make-marker)))
    (set-marker end-reg-mk end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (while (< (point) end-reg-mk)
      ;; This function must also leave the point at the end of the
      ;; justified line.
      (scilab-fill-paragraph justify-flag)
      (forward-line 1)
      (beginning-of-line))))

(defun scilab-fill-comment-line (&optional justify)
  "Fill the current comment line.
With optional argument, JUSTIFY the comment as well."
  (interactive)
  (if (not (scilab-comment-on-line))
      (error "No comment to fill"))
  (beginning-of-line)
  (if (not (looking-at scilab-cline-start-skip))
       (error "Don't fill the comment after the text"))
  ;; First, find the beginning of this comment...
  (while (and (looking-at scilab-cline-start-skip)
	      (not (bobp)))
    (forward-line -1)
    (beginning-of-line))
  (if (not (looking-at scilab-cline-start-skip))
      (forward-line 1))
  ;; Now scan to the end of this comment so we have our outer bounds,
  ;; and narrow to that region.
  (save-restriction
    (narrow-to-region (point)
		      (save-excursion
			(while (and (looking-at scilab-cline-start-skip)
				    (not (save-excursion (end-of-line) (eobp))))
			  (forward-line 1)
			  (beginning-of-line))
			(if (not (looking-at scilab-cline-start-skip))
			    (forward-line -1))
			(end-of-line)
			(point)))
    ;; Find the fill prefix...
    (scilab-comment-on-line)
    (looking-at "//[ \t]*")
    (let ((fill-prefix (concat (make-string (current-column) ? )
			       (match-string 0))))
      (fill-region (point-min) (point-max) justify))))

(defun scilab-justify-line ()
  "Delete space on end of line and justify."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)
    (justify-current-line)))

(defun scilab-fill-paragraph (arg)
  "When in a comment, fill the current paragraph.
Paragraphs are always assumed to be in a comment.
ARG is passed to `fill-paragraph' and will justify the text."
  (interactive "P")
  (cond ((or (scilab-ltype-comm)
	     (and (scilab-cursor-in-comment)
		  (not (scilab-lattr-cont))))
	 ;; We are in a comment, lets fill the paragraph with some
	 ;; nice regular expressions.
	 (let ((paragraph-separate "//[a-zA-Z]\\|//[ \t]*$\\|[ \t]*$")
	       (paragraph-start    "//[a-zA-Z]\\|//[ \t]*$\\|[ \t]*$")
	       (paragraph-ignore-fill-prefix nil)
	       (fill-prefix nil))
	   (scilab-set-comm-fill-prefix)
	   (fill-paragraph arg)))
	((scilab-ltype-code)
	 ;; Ok, lets get the outer bounds of this command, then
	 ;; completely refill it using the smart line breaking code.
	 (save-restriction
	   (narrow-to-region (save-excursion
			       (scilab-beginning-of-command)
			       (beginning-of-line)
			       (point))
			     (save-excursion
			       (scilab-end-of-command)
			       (point)))
	   ;; Remove all line breaks
	   (goto-char (point-min))
	   (while (and (re-search-forward "$" nil t)
		       (not (eobp)))
	     (delete-horizontal-space)
	     ;; Blow away continuation marks
	     (if (scilab-lattr-cont)
		 (progn
		   (goto-char (match-beginning 0))
		   (forward-char 1)
;		   (forward-char 2)
		   (delete-region (point) (scilab-point-at-eol))))
	     ;; Zap the CR
	     (if (not (eobp)) (delete-char 1))
	     ;; Clean up whitespace
	     (delete-horizontal-space)
	     ;; Clean up trailing comments
	     (if (and (looking-at "// *")
		      (scilab-cursor-in-comment))
		 (progn
		   (delete-char 2)
		   (delete-horizontal-space)))
	     (insert " "))
	   ;; Now fill till we are done
	   (goto-char (point-max))
	   (while (or (> (current-column) (+ fill-column scilab-fill-fudge))
		      (> (current-column) scilab-fill-fudge-hard-maximum))
	     (if (= (point)
		    (progn
		      (scilab-auto-fill)
		      (point)))
		 (error "Fill algorith failed!"))
	     (if arg (save-excursion
		       (forward-line -1)
		       (scilab-justify-line))))
	   (if arg (save-excursion
		     (forward-line -1)
		     (scilab-justify-line)))))
	(t
	 (message "Paragraph Fill not supported in this context."))))

;;; Semantic text insertion and management ====================================


(defun scilab-find-recent-variable-list (prefix)
  "Instead of looking through the session we just use Scilab who comand to 
get all variables"
(interactive)
(if (not (scilab-shell-active-p))
   nil
  (let ((fields (scilab-shell-collect-command-output 
                (concat "%tmp=lines();lines(0);"
                        "#=[who('global');who('local')];"
                        "##='" (regexp-quote prefix) "';"
                        "###=length(##);"
                        "write(%io(2),#(part(#,1:###)==##));"
                         "lines(%tmp(2),%tmp(1)); clear %tmp # ## ###"))))
   (if (not fields)
     nil
   (split-string fields)))
))



(defvar scilab-most-recent-variable-list nil
  "Maintained by `scilab-find-recent-variable'.")

(defun scilab-find-recent-variable (prefix &optional next)
  "Return the most recently used variable starting with PREFIX as a string.
See `scilab-find-recent-variable-list' for details.
In NEXT is non-nil, than continue through the list of elements."
  (if next
      (let ((next (car scilab-most-recent-variable-list)))
	(setq scilab-most-recent-variable-list
	      (cdr scilab-most-recent-variable-list))
	next)
    (let ((syms (scilab-find-recent-variable-list prefix))
	  (first nil))
      (if (eq scilab-completion-technique 'complete)
	  syms
	(setq first (car syms))
	(setq scilab-most-recent-variable-list (cdr syms))
	first))))

;; We do not use the next two functions. Instead we write  new functions
;; scilab-find-scifunctions-list and scilab-find-scifunctions

(defun scilab-find-user-functions-list (prefix)
  "Return a list of user defined functions that match PREFIX."
  (setq prefix (regexp-quote prefix))
  (scilab-navigation-syntax
    (let ((syms
	   (append
	    (save-excursion
	      (goto-char (point-min))
	      (let ((lst nil))
		(while (re-search-forward "^\\s-*function\\>" nil t)
		  (if (re-search-forward
		       (concat "\\(" prefix "\\w+\\)\\s-*\\($\\|(\\)")
		       (scilab-point-at-eol) t)
		      (setq lst (cons (match-string 1) lst))))
		(nreverse lst)))
	    (let ((lst nil)
		  (files (directory-files
			  default-directory nil
			  (concat "^" prefix
				  "[a-zA-Z][a-zA-Z0-9_]+\\.sci$"))))
	      (while files
		(setq lst (cons (progn (string-match "\\.sci" (car files))
				       (substring (car files) 0
						  (match-beginning 0)))
				lst)
		      files (cdr files)))
	      lst)))
	  (fl nil))
      (while syms
	(if (car syms) (setq fl (cons (car syms) fl)))
	(setq syms (cdr syms)))
      (scilab-uniquafy-list (nreverse fl)))))

(defvar scilab-user-function-list nil
  "Maintained by `scilab-find-user-functions'.")

(defvar scilab-shell-libfunc-isbuilt nil)

(defun scilab-find-scifunctions-list (prefix)
  "Return a list of library  functions that match PREFIX. The goal is
to find all sci functions. To make search over all directories everytime
is too time consuming. Instead we suggest to define a staic file, say libfunc
via Scilab or shell standard features. In shell we suggest to define a variable
SCILIBFUNC pointing on this file say in $SCIHOME/libfunc.This 
value is contained in the customized variable `scilab-libfunc-list-path'.
 The function first checks if the file exisits. If not it checks if $SCIHOME/libfunc file exists. If non of this exists and shell is active it builds this file. Otherwise it returns nil"
  (let ((curbuff (current-buffer))
        (allnames scilab-libfunc-list-path)
        (buff nil))
  (if (not allnames)
      (setq allnames (concat scilab-shell-initial-directory "/libfunc")))
  (if (not (scilab-shell-active-p))
      nil
      (if scilab-shell-libfunc-isbuilt
          nil
	  (if (file-exists-p allnames) 
	      (delete-file allnames)
	    )
	(scilab-shell-collect-command-output (concat 
             "##=who('local');"
             "###='type('+##+')==14';"
             "###(1)='?=['+###(1);###($)=###($)+'];';"
             "execstr(###); ##=##(?);?=[];"        
             "###='#tmp=string('+##+');?=[?;#tmp(2:$)]';"
             "execstr(###);"
              "#=file('open','" allnames "','unknown');"
              "write(#,?);file('close',#);clear # ## ### #tmp ?"))
	(message "File %s is updated..." allnames)
	(setq scilab-shell-libfunc-isbuilt t))
) 
  (if (not (file-exists-p allnames))
     nil
    (find-file  allnames)
    (setq buff (current-buffer))
    (setq case-fold-search nil)
    (save-excursion
     (goto-char (point-min))
     (let ((lst nil))
       (setq prefix (regexp-quote  prefix))    
       (while (re-search-forward
       (concat "^\\s-*\\(" prefix "\\(\\w\\|[_$]\\)*\\)\\s-*\n") nil t)
       (setq lst (cons (match-string 1) lst))
      )
     (kill-buffer buff)
     (switch-to-buffer curbuff)
     (if lst
     (scilab-uniquafy-list (nreverse lst)) nil))))))

(defun scilab-find-tlists-list (prefix)
  "Return a list of fields of prefix considired as tlist of scilab"
(string-match "\\(%?\\(\\w\\|[_$]\\)*\\)\\.\\([^.]*\\)$" prefix)
(let  (  (pref (match-string 1 prefix)) 
         (ff (match-string 3 prefix)) (i 0) (matchfields nil)
         fields  matchfields fil)
   (save-excursion
     (setq fields (scilab-shell-collect-command-output 
                    (concat "%tmp=lines();lines(0);" 
                        "write(%io(2)," pref "(1)(2:$),'(a)');" 
                        "lines(%tmp(2),%tmp(1)); clear %tmp;"
                    )
                 )
               ) 
     (setq fields (split-string fields))
     (if (not fields)
          nil
        (while (< i (length fields))
          (setq fil (nth i fields))
           (if (string-match (concat "\\(\\<" ff "\\(\\w\\|[_$]\\)*\\)") fil)
             (setq matchfields (cons (concat pref "." fil) matchfields)))
             (setq i  (+ i 1))
        )
     matchfields
))))


(defvar scilab-scifunction-list nil
  "Maintained by `scilab-find-scifunctions'.")

(defun scilab-find-user-functions (prefix &optional next)
  "Return a list of user defined functions that match PREFIX."
  (if next
      (let ((next (car scilab-user-function-list)))
	(setq scilab-user-function-list (cdr scilab-user-function-list))
	next)
    (let ((syms (scilab-find-user-functions-list prefix))
	  (first nil))
      (if (eq scilab-completion-technique 'complete)
	  syms
	(setq first (car syms))
	(setq scilab-user-function-list (cdr syms))
	first))))

(defun scilab-find-scifunctions (prefix &optional next)
 "Return a function from all available *.sci  that match PREFIX and return it.
If optional argument NEXT is non-nil, then return the next found
object. This "
  (if next
      (let ((next (car scilab-scifunction-list)))
	(setq scilab-scifunction-list (cdr scilab-scifunction-list))
	next)
    (let ((syms (scilab-find-scifunctions-list prefix))
	  (first nil))
      (if (eq scilab-completion-technique 'complete)
	  syms
	(setq first (car syms))
	(setq scilab-scifunction-list (cdr syms))
	first))))

(defvar scilab-tlists-list nil
  "Maintained by `scilab-find-tlists-fields'.")


(defun scilab-find-tlists-fields (prefix &optional next)
  "Return PREFIX matching elements for tlist  variable of scilab
If NEXT then the next patch from the list is used."
  (if next
      (let ((next (car scilab-tlists-list)))
	(setq scilab-tlists-list (cdr scilab-tlists-list))
	next)
    (let ((syms (scilab-find-tlists-list prefix))
	  (first nil))
      (if (eq scilab-completion-technique 'complete)
	  syms
	(setq first (car syms))
	(setq scilab-tlists-list (cdr syms))
	first))))






(defvar scilab-generic-list-placeholder nil
  "Maintained by `matalb-generic-list-expand'.
Holds sub-lists of symbols left to be expanded.")

(defun scilab-generic-list-expand (list prefix &optional next)
  "Return an element from LIST that start with PREFIX.
If optional NEXT argument is non nil, then the next element in the
list is used.  nil is returned if there are not matches."
  (if next
      (let ((next (car scilab-generic-list-placeholder)))
	(setq scilab-generic-list-placeholder
	      (cdr scilab-generic-list-placeholder))
	next)
    (let ((re (concat "^" (regexp-quote prefix)))
	  (first nil)
	  (fl nil))
      (while list
	(if (string-match re (car list))
	    (setq fl (cons (car list) fl)))
	(setq list (cdr list)))
      (setq fl (nreverse fl))
      (if (eq scilab-completion-technique 'complete)
	  fl
	(setq first (car fl))
	(setq scilab-generic-list-placeholder (cdr fl))
	first))))

(defun scilab-solo-completions (prefix &optional next)
  "Return PREFIX matching elements for solo symbols.
If NEXT then the next patch from the list is used."
  (scilab-generic-list-expand scilab-keywords-solo prefix next))

(defun scilab-builtin-completions (prefix &optional next)
  "Return PREFIX matching elements for value symbols.
If NEXT then the next patch from the list is used."
  (scilab-generic-list-expand scilab-builtin-list prefix next))

(defun scilab-boolean-completions (prefix &optional next)
  "Return PREFIX matching elements for boolean symbols.
If NEXT then the next patch from the list is used."
  (scilab-generic-list-expand scilab-keywords-boolean prefix next))

(defun scilab-property-completions (prefix &optional next)
  "Return PREFIX matching elements for property names in strings.
If NEXT then the next property from the list is used."
  (let ((f (scilab-function-called-at-point))
	(lst scilab-property-lists)
	(foundlst nil)
	(expandto nil))
    ;; Look for this function.  If it is a known function then we
    ;; can now use a subset of available properties!
    (while (and lst (not foundlst))
      (if (string= (car (car lst)) f)
	  (setq foundlst (cdr (car lst))))
      (setq lst (cdr lst)))
    (if foundlst
	(setq foundlst (append foundlst scilab-core-properties))
      (setq foundlst scilab-all-known-properties))
    (setq expandto (scilab-generic-list-expand foundlst prefix next))
    ;; This looks to see if we have a singular completion.  If so,
    ;; then return it, and also append the "'" to the end.
    (cond ((and (listp expandto) (= (length expandto) 1))
	   (setq expandto (list (concat (car expandto) "'"))))
	  ((stringp expandto)
	   (setq expandto (concat expandto "'"))))
    expandto))

(defvar scilab-last-prefix nil
  "Maintained by `scilab-complete-symbol'.
The prefix used for the first completion command.")
(defvar scilab-last-semantic nil
  "Maintained by `scilab-complete-symbol'.
The last type of semantic used while completing things.")
(defvar scilab-completion-search-state nil
  "List of searching things we will be doing.")

;(defvar scilab-complete-1-window  nil)

(defun scilab-complete-symbol (&optional arg)
  "Complete a partially typed symbol in a Scilab mode buffer.
If the previously entered command was also `scilab-complete-symbol'
then undo the last completion, and find a new one.
  The types of symbols tried are based on the semantics of the current
cursor position.  There are two types of symbols.  For example, if the
cursor is in an if statement, boolean style functions and symbols are
tried first.  If the line is blank, then flow control, or high level
functions are tried first.
  The completion technique is controlled with `scilab-completion-technique'
It defaults to incremental completion described above.  If a
completion list is preferred, then change this to 'complete.  If you
just want a completion list once, then use the universal argument ARG
to change it temporarily."
  (interactive "P")
;  (if (not (eq last-command 'scilab-complete-symbol))
;     (setq scilab-complete-1-window (one-window-p)))
  (if (eq (scilab-lattr-semantics) 'disk)
  (comint-dynamic-complete-filename)
  (scilab-navigation-syntax
    (let* ((cbf (buffer-name)) 
           (cpt (point)) 
           (locp (scilab-point-at-bol))
           (prefix (if (and (not (eq last-command 'scilab-complete-symbol))
			    (member (preceding-char) '(?  ?\t ?\n ?, ?\( ?\[ ?\' ?\")))
		       ""
		     (buffer-substring-no-properties
		      (save-excursion
                        (save-excursion
                           (beginning-of-line)
                           (if (looking-at (concat "^\\(" 
                                           comint-prompt-regexp "\\)"))
                            (setq locp (match-end 1))))
                      (forward-word -1) 
                      (while (eq (preceding-char) ?.) (forward-word -1))
                      (max locp (point)))
		      (point))))
	   (sem (scilab-lattr-semantics prefix))
	   (scilab-completion-technique
	    (if arg (cond ((eq scilab-completion-technique 'complete)
			   'increment)
			  (t 'complete))
	      scilab-completion-technique)))
      (if (not (eq last-command 'scilab-complete-symbol))
	  (setq scilab-last-prefix prefix
		scilab-last-semantic sem
		scilab-completion-search-state
		(cond ((eq sem nil) nil)
                      ((eq sem 'solo)
		       '(scilab-solo-completions
			 scilab-find-scifunctions
			 scilab-find-recent-variable
			 scilab-builtin-completions
;			 scilab-find-user-functions
                        ))
;;;		      ((eq sem 'boolean)
;;;		       '(scilab-find-recent-variable
;;;			 scilab-boolean-completions
;;;			 scilab-builtin-completions 
;;;			 scilab-find-scifunctions
;;;			 scilab-find-user-functions))
		      ((eq sem 'value)
		       '(scilab-find-recent-variable
			 scilab-builtin-completions
			 scilab-find-scifunctions
;			 scilab-find-user-functions
;			 scilab-boolean-completions
                      ))
		      ((eq sem 'property)
		       '(scilab-property-completions
;			 scilab-find-user-functions
			 scilab-find-recent-variable
			 scilab-builtin-completions))
		      ((eq sem 'list)
		       '(scilab-find-tlists-fields))
		      (t '(scilab-find-recent-variable
			   scilab-builtin-completions
			   scilab-find-scifunctions
;			   scilab-find-user-functions
;			   scilab-boolean-completions 
                       )))))
      (cond
       ((eq scilab-completion-technique 'increment)
	(let ((r nil) (donext (eq last-command 'scilab-complete-symbol)))
	  (while (and (not r) scilab-completion-search-state)
	    (message "Expand with %S" (car scilab-completion-search-state))
	    (setq r (funcall (car scilab-completion-search-state)
			     scilab-last-prefix donext))
	    (if (not r) (setq scilab-completion-search-state
			      (cdr scilab-completion-search-state)
			      donext nil)))
	  (goto-char cpt)
	  (delete-region (point) (progn (forward-char (- (length prefix)))
					(point)))
	  (if r
	      (insert r)
	    (insert scilab-last-prefix)
	    (message "No completions."))))
       ((eq scilab-completion-technique 'complete)
	(let ((allsyms (scilab-uniquafy-list (apply 'append
			      (mapcar (lambda (f) (funcall f prefix))
				      scilab-completion-search-state)))))
          (set-buffer cbf)
	  (cond ((or (null allsyms) (=(length allsyms) 0))
		 (goto-char cpt)
                 (if (equal prefix "")
		     (message "No completions")
		   (message "No completions for %s." prefix))
		 (if  (get-buffer "*Completions*") 
		     (if (get-buffer-window (get-buffer "*Completions*"))
                          (save-excursion
				(select-window (get-buffer-window 
						(get-buffer "*Completions*")))
;                                (if scilab-complete-1-window (delete-window)
				(bury-buffer))))
                 (select-window  (get-buffer-window cbf))
		 (ding))
		((= (length allsyms) 1)
		 (goto-char cpt)
		 (delete-region (point) (progn
					  (forward-char (- (length prefix)))
					  (point)))
                 (message "Solo completion for %s" prefix)
		 (insert (car allsyms))
                  (if  (get-buffer "*Completions*") 
		      (if (get-buffer-window (get-buffer "*Completions*"))
                          (save-excursion
				(select-window (get-buffer-window 
						(get-buffer "*Completions*")))
;                                (if scilab-complete-1-window (delete-window)
				(bury-buffer))))
		  (select-window  (get-buffer-window cbf))
		  )
		(t
		 (let* ((al (mapcar (lambda (a) (list a)) allsyms))
			(c (try-completion prefix al)))
		   ;; This completion stuff lets us expand as much as is
		   ;; available to us. When the completion is the prefix
		   ;; then we want to display all the strings we've
		   ;; encountered.
		   (if (and (stringp c) (not (string= prefix c)))
		       (progn
                         (goto-char cpt)
			 (delete-region
			  (point)
			  (progn (forward-char (- (length prefix)))
				 (point)))
			 (insert c))
          		 (goto-char cpt)


		     ;; `display-completion-list' does all the complex
		     ;; ui work for us.
;                     (if (or (not (get-buffer "*Completions*")) 
;			     (not (get-buffer-window (get-buffer "*Completions*"))))
;                     (split-window-vertically))
		     (with-output-to-temp-buffer "*Completions*"
		       (display-completion-list
			(scilab-uniquafy-list allsyms))))))))))))))

(defun scilab-insert-end-block (&optional reindent)
  "Insert and END block based on the current syntax.
Optional argument REINDENT indicates if the specified block should be re-indented."
  (interactive "P")
  (if (not (scilab-ltype-empty)) (progn (end-of-line) (insert "\n")))
  (let ((valid t) (begin nil))
    (save-excursion
      (condition-case nil
	  (progn
	    (scilab-backward-sexp t)
	    (setq begin (point)
		  valid (buffer-substring-no-properties
			 (point) (save-excursion
				   (re-search-forward "[\n;.]" nil t)
				   (point)))))
	(error (setq valid nil))))
    (if (not valid)
	(error "No block to end")
      (if (string-match "^function" valid)
          (insert "endfunction")
          (insert "end"))
      (scilab-indent-line)
      (if (stringp valid) (insert " // " valid))
      (scilab-indent-line)
      (if reindent (indent-region begin (point) nil)))))

(tempo-define-template
 "scilab-for"
 '("for " p " = " p  > n>
     r> &
     "end" >)
 "for"
 "Insert a Scilab for statement"
 'scilab-tempo-tags
 )

(tempo-define-template
 "scilab-while"
 '("while " p  > n>
     r> &
     "end" > )
 "while"
 "Insert a Scilab while statement"
 'scilab-tempo-tags
 )

(tempo-define-template
 "scilab-if"
 '("if " p " then" > n
     r>
     "end" > n)
 "if"
 "Insert a Scilab if statement"
 'scilab-tempo-tags
 )

(tempo-define-template
 "scilab-if-else"
 '("if " p " then" > n
     r>
     "else" > n
     "end" > n)
 "if"
 "Insert a Scilab if statement"
 'scilab-tempo-tags
 )

(tempo-define-template
 "scilab-try"
 '("try " > n
     r>
     "catch" > n
     p > n
     "end" > n)
 "try"
 "Insert a Scilab try catch statement"
 'scilab-tempo-tags
 )

(tempo-define-template
 "scilab-select"
 '("select " p  " then"> n
     "case "  p  " then"> n
     "else" > n
     r>
     "end" > n)
 "select"
 "Insert a Scilab select statement with region in the otherwise clause."
 'scilab-tempo-tags)

(defun scilab-insert-next-case ()
  "Insert a case statement inside this select statement."
  (interactive)
  ;; First, make sure we are where we think we are.
  (let ((valid t))
    (save-excursion
      (condition-case nil
	  (progn
	   (scilab-backward-sexp t)
	   (setq valid (looking-at "select")))
	(error (setq valid nil))))
    (if (not valid)
	(error "Not in a select statement")))
  (if (not (scilab-ltype-empty)) (progn (end-of-line) (insert "\n")))
  (indent-to 0)
  (insert "case   then")
  (scilab-indent-line))

(tempo-define-template
 "scilab-function"
 '("function "
     (P "output argument(s): " output t)
     ;; Insert brackets only if there is more than one output argument
     (if (string-match "," (tempo-lookup-named 'output))
	 '(l "[" (s output) "]")
       '(l (s output)))
     ;; Insert equal sign only if there is output argument(s)
     (if (= 0 (length (tempo-lookup-named 'output))) nil
       " = ")
     ;; The name of a function, as defined in the first line, should
     ;; be the same as the name of the file without .sci extension
     (if (= 1 (count-lines 1 (point)))
	 (tempo-save-named
	  'fname
	  (file-name-nondirectory (file-name-sans-extension
				   (buffer-file-name))))
       '(l (P "function name: " fname t)))
     (tempo-lookup-named 'fname)
     "("  (P "input argument(s): ") ")" n
     "// " (upcase (tempo-lookup-named 'fname)) " - " (P "H1 line: ") n
     "//  " p n)
 "function"
 "Insert a Scilab function statement"
 'scilab-tempo-tags
 )

(defun scilab-stringify-region (begin end)
  "Put Scilab 's around region, and quote all quotes in the string.
Stringification allows you to type in normal Scilab code, mark it, and
then turn it into a Scilab string that will output exactly what's in
the region.  BEGIN and END mark the region to be stringified."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (if (re-search-forward "\n" end t)
	(error
	 "You may only stringify regions that encompass less than one line"))
    (let ((m (make-marker)))
      (move-marker m end)
      (goto-char begin)
      (insert "'")
      (while (re-search-forward "'" m t)
	(insert "'"))
      (goto-char m)
      (insert "'"))))

(defun scilab-ispell-strings-region (begin end)
  "Spell check valid strings in region with Ispell.
Argument BEGIN and END mark the region boundary."
  (interactive "r")
  (require 'ispell)
  (save-excursion
    (goto-char begin)
    ;; Here we use the font lock function for finding strings.
    ;; Its cheap, fast, and accurate.
    (while (and (scilab-font-lock-string-match-normal end)
		(ispell-region (match-beginning 2) (match-end 2))))))

(defun scilab-ispell-strings ()
  "Spell check valid strings in the current buffer with Ispell.
Calls `scilab-ispell-strings-region'"
  (interactive)
  (scilab-ispell-strings-region (point-min) (point-max)))

(defun scilab-ispell-comments (&optional arg)
  "Spell check comments in the current buffer with Ispell.
Optional ARG means to only check the current comment."
  (interactive "P")
  (let ((beg (point-min))
	(end (point-max)))
  (if (and arg (scilab-ltype-comm))
      (setq beg (save-excursion (scilab-beginning-of-command) (point))
	    end (save-excursion (scilab-end-of-command) (point))))
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (while (and (scilab-font-lock-comment-match end)
		(ispell-region (match-beginning 1) (match-end 1)))))))


;;; Block highlighting ========================================================

(defvar scilab-block-highlighter-timer nil
  "The timer representing the block highlighter.")
(defvar scilab-shell-delay (if running-gnuemacs 1 3)
"The delay running scilab command  when restarting"
) 

(defun scilab-enable-block-highlighting (&optional arg)
  "Start or stop the block highlighter.
Optional ARG is 1 to force enable, and -1 to disable.
If ARG is nil, then highlighting is toggled."
  (interactive "P")
  (if (not (fboundp 'scilab-run-with-idle-timer))
      (setq scilab-highlight-block-match-flag nil))
  ;; Only do it if it's enabled.
  (if (not scilab-highlight-block-match-flag)
      nil
    ;; Use post command idle hook as a local hook to dissuade too much
    ;; cpu time while doing other things.
    ;;(make-local-hook 'post-command-hook)
    (if (not arg)
	(setq arg
	      (if (member 'scilab-start-block-highlight-timer
			  post-command-hook)
		  -1 1)))
    (if (> arg 0)
	(add-hook 'post-command-hook 'scilab-start-block-highlight-timer)
      (remove-hook 'post-command-hook 'scilab-start-block-highlight-timer))))

(defvar scilab-block-highlight-overlay nil
  "The last highlighted overlay.")
(make-variable-buffer-local 'scilab-block-highlight-overlay)

(defvar scilab-block-highlight-timer nil
  "Last started timer.")
(make-variable-buffer-local 'scilab-block-highlight-timer)

(defun scilab-start-block-highlight-timer ()
  "Set up a one-shot timer if we are in Scilab mode."
  (if (eq major-mode 'scilab-mode)
      (progn
	(if scilab-block-highlight-overlay
	    (unwind-protect
		(scilab-delete-overlay scilab-block-highlight-overlay)
	      (setq scilab-block-highlight-overlay nil)))
	(if scilab-block-highlight-timer
	    (unwind-protect
		(scilab-cancel-timer scilab-block-highlight-timer)
	      (setq scilab-block-highlight-timer nil)))
	(setq scilab-block-highlight-timer
	      (scilab-run-with-idle-timer
	       1 nil 'scilab-highlight-block-match)))))
  
(defun scilab-highlight-block-match ()
  "Highlight a matching block if available."
  (setq scilab-block-highlight-timer nil)
  (let ((inhibit-quit nil)		;turn on G-g
	(scilab-scan-on-screen-only t))
    (if scilab-show-periodic-code-details-flag
	(scilab-show-line-info))
    (if (not (scilab-cursor-in-string-or-comment))
	(save-excursion
	  (if (or (bolp)
		  (looking-at "\\s-")
		  (save-excursion (forward-char -1) (looking-at "\\s-")))
	      nil
	    (forward-word -1))
	  (if (and (looking-at (concat (scilab-block-beg-re) "\\>"))
		   (not (looking-at "function")))
	      (progn
		;; We scan forward...
		(scilab-forward-sexp)
		(backward-word 1)
		(if (not (looking-at "end"))
		    nil ;(message "Unterminated block, or end off screen.")
		  (setq scilab-block-highlight-overlay
			(scilab-make-overlay (point)
					     (progn (forward-word 1)
						    (point))
					     (current-buffer)))
		  (scilab-overlay-put scilab-block-highlight-overlay
				      'face 'scilab-region-face)))
	    (if (and (looking-at (concat (scilab-block-end-pre) "\\>"))
		     (not (looking-at "function"))
		     (scilab-valid-end-construct-p))
		(progn
		  ;; We scan backward
		  (forward-word 1)
		  (condition-case nil
		      (progn
			(scilab-backward-sexp)
			(if (not (looking-at (scilab-block-beg-re)))
			    nil ;(message "Unstarted block at cursor.")
			  (setq scilab-block-highlight-overlay
				(scilab-make-overlay (point)
						     (progn (forward-word 1)
							    (point))
						     (current-buffer)))
			  (scilab-overlay-put scilab-block-highlight-overlay
					      'face 'scilab-region-face)))
		    (error (message "Unstarted block at cursor."))))
	      ;; do nothing
	      ))))))

;;; M Code verification & Auto-fix ============================================

(defun scilab-mode-verify-fix-file-fn ()
  "Verify the current buffer from `write-contents-hooks'."
  (if scilab-verify-on-save-flag
      (scilab-mode-verify-fix-file (> (point-max)
				      scilab-block-verify-max-buffer-size)))
  ;; Always return nil.
  nil)

(defun scilab-mode-verify-fix-file (&optional fast)
  "Verify the current buffer satisfies all sci things that might be useful.
We will merely loop across a list of verifiers/fixers in
`scilab-mode-verify-fix-functions'.
If optional FAST is non-nil, do not perform usually lengthy checks."
  (interactive)
  (let ((p (point))
	(l scilab-mode-verify-fix-functions))
    (while l
      (funcall (car l) fast)
      (setq l (cdr l)))
    (goto-char p))
  (if (interactive-p)
      (message "Done.")))

;;
;; Add more auto verify/fix functions here!
;;
(defun scilab-mode-vf-functionname (&optional fast)
  "Verify/Fix the function name of this file.
Optional argument FAST is ignored."
  (scilab-navigation-syntax
    (goto-char (point-min))
    (while (and (or (scilab-ltype-empty) (scilab-ltype-comm))
		(/= (scilab-point-at-eol) (point-max)))
      (forward-line 1))
    (let ((func nil)
	  (bn (file-name-sans-extension
	       (file-name-nondirectory (buffer-file-name)))))
    (if (looking-at
	 ;; old function was too unstable.
	 ;;"\\(^function\\s-+\\)\\([^=\n]+=[ \t\n.]*\\)?\\(\\sw+\\)"
	 (concat "\\(^\\s-*function\\b[ \t\n.]*\\)\\(\\(\\[[^]]*\\]\\|\\sw+\\)"
		 "[ \t\n.]*=[ \t\n.]*\\)?\\(\\sw+\\)"))
	;; The expression above creates too many numeric matches
	;; to apply a known one to our function.  We cheat by knowing that
	;; match-end 0 is at the end of the function name.  We can then go
	;; backwards, and get the extents we need.  Navigation syntax
	;; lets us know that backward-word really covers the word.
	(let ((end (match-end 0))
	      (begin (progn (goto-char (match-end 0))
			    (forward-word -1)
			    (point))))
	  (setq func (buffer-substring begin end))
	  (if (not (string= func bn))
	      (if (not (scilab-mode-highlight-ask
			begin end
			"Function and file names are different. Fix?"))
		  nil
		(goto-char begin)
		(delete-region begin end)
		(insert bn))))))))

(defun scilab-mode-vf-block-matches-forward (&optional fast)
  "Verify/Fix unterminated (or un-ended) blocks.
This only checks block regions like if/end.
Optional argument FAST causes this check to be skipped."
  (goto-char (point-min))
  (let ((go t)
	(expr (concat "\\<\\(" (scilab-block-beg-pre) "\\)\\>")))
    (scilab-navigation-syntax
      (while (and (not fast) go (re-search-forward expr nil t))
	(forward-word -1)		;back over the special word
	(let ((s (point)))
	  (condition-case nil
	      (if (and (not (scilab-cursor-in-string-or-comment))
		       (not (looking-at "function")))
		  (progn
		    (scilab-forward-sexp)
		    (forward-word -1)
		    (if (not (looking-at "end\\>")) (setq go nil)))
		(forward-word 1))
	    (error (setq go nil)))
	  (if (and (not go) (goto-char s)
		   (not (scilab-mode-highlight-ask
			 (point) (save-excursion (forward-word 1) (point))
			 "Unterminated block.  Continue anyway?")))
	      (error "Unterminated Block found!")))
	(message "Block-check: %d%%" (/ (/ (* 100 (point)) (point-max)) 2))))))
  
(defun scilab-mode-vf-block-matches-backward (&optional fast)
  "Verify/fix unstarted (or dangling end) blocks.
Optional argument FAST causes this check to be skipped."
  (goto-char (point-max))
  (let ((go t) (expr (concat "\\<\\(" (scilab-block-end-no-function-re)
			     "\\)\\>")))
    (scilab-navigation-syntax
      (while (and (not fast) go (re-search-backward expr nil t))
	(forward-word 1)
	(let ((s (point)))
	  (condition-case nil
	      (if (and (not (scilab-cursor-in-string-or-comment))
		       (scilab-valid-end-construct-p))
		  (scilab-backward-sexp)
		(backward-word 1))
	    (error (setq go nil)))
	  (if (and (not go) (goto-char s)
		   (not (scilab-mode-highlight-ask
			 (point) (save-excursion (backward-word 1) (point))
			 "Unstarted block.  Continue anyway?")))
	      (error "Unstarted Block found!")))
	(message "Block-check: %d%%"
		 (+ (/ (/ (* 100 (- (point-max) (point))) (point-max)) 2) 50))))))

;;; Utility for verify/fix actions if you need to highlight
;;  a section of the buffer for the user's approval.
(defun scilab-mode-highlight-ask (begin end prompt)
  "Highlight from BEGIN to END while asking PROMPT as a yes-no question."
  (let ((mo (scilab-make-overlay begin end (current-buffer)))
	(ans nil))
    (condition-case nil
	(progn
	  (scilab-overlay-put mo 'face 'scilab-region-face)
	  (setq ans (y-or-n-p prompt))
	  (scilab-delete-overlay mo))
      (quit (scilab-delete-overlay mo) (error "Quit")))
    ans))

;;; Quietafy an sci file to remove accidental display of ANS during a run.
;;  Useful if you have random outputs and you don't know where they are from,
;;  or before compiling to standalone where some functions now have outputs
;;  that did not have outputs earlier.
;;
;;  You probably don't want this as a default verify function
(defun scilab-mode-vf-quietafy-buffer (&optional fast)
  "Find all commands that do not end in ;, and add one.
This has the effect of removing any extraneous output that may not be
desired.  Optional argument FAST is not used."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((msgpos 0) (dir .2))
      (while (not (save-excursion (end-of-line) (eobp)))
	(message (aref [ "Scanning o...." "Scanning .o..." "Scanning ..o.."
			 "Scanning ...o." "Scanning ....o" ] (floor msgpos)))
	(setq msgpos (+ msgpos dir))
	(if (or (> msgpos 5) (< msgpos 0)) (setq dir (- dir)
						 msgpos (+ (* 2 dir) msgpos)))
	(scilab-end-of-command (point))
	(if (scilab-cursor-in-comment)
	    (progn
	      (scilab-comment-on-line)
	      (skip-chars-backward " \t")))
	(if (and (not (= (preceding-char) ?\;))
		 (not (scilab-cursor-in-string t))
		 (not (save-excursion
			(beginning-of-line)
			(looking-at
			 "\\s-*\\(function\\|for\\|while\\|\
select\\|case\\|break\\|if\\|else\\|end\\|then \\|return\\|disp\\|\
$\\|//\\)"))))
             (let ((p (point)))
	      (skip-chars-backward " \t")
	      (if (/= p (point))
		  (progn
		    (delete-region p (point))
		    (forward-line -1))
	    (if (scilab-mode-highlight-ask (point) (+ 1 (point))
					   "Add Semi colon here? ")
		(insert ";")))))
	(forward-line 1))))
  (message "Scanning .... done"))
  


;;; V19 stuff =================================================================

(defun scilab-mode-hilit ()
  "Set up hilit19 support for `scilab-mode'."
  (interactive)
  (cond (window-system
	 (setq hilit-mode-enable-list  '(not text-mode)
	       hilit-background-mode   'light
	       hilit-inhibit-hooks     nil
	       hilit-inhibit-rebinding nil)
	 (require 'hilit19)
	 (hilit-set-mode-patterns 'scilab-mode scilab-hilit19-patterns))))

(defvar scilab-mode-menu-keymap nil
  "Keymap used in Scilab mode to provide a menu.")

(defun scilab-frame-init ()
  "Initialize Emacs 19+ menu and Xemacs system. in `scilab-mode' (editing mode)"
  (interactive)
  ;; make a menu keymap
;  (setq active (if running-xemacs "included" "active"))
  (easy-menu-define scilab-insert-menu 
   scilab-mode-map
   "Insert"
   '("Insert"
      ["Complete name" scilab-complete-symbol t]
      ["Set Completion Type" 
            (customize-variable 'scilab-completion-technique) t] 
      "----"
      ["Comment region" scilab-comment-region t]
      ["UnComment region" scilab-uncomment-region t]
      "----"
      "Templates"
      ["if end" tempo-template-scilab-if t]
      ["if else end" tempo-template-scilab-if-else t]
      ["for end" tempo-template-scilab-for t]
      ["select else end" tempo-template-scilab-select t]
      ["Next case" scilab-insert-next-case t]
      ["while end" tempo-template-scilab-while t]
      ["End of block" scilab-insert-end-block t]
      ["Function" tempo-template-scilab-function t]
      )
)
  (easy-menu-define scilab-navigate-menu 
   scilab-mode-map
   "Navigate"
   '("Navigate"
     ["Find sc[ie] file|func" scilab-find-file-on-path t]
     ["Change Dir"   scilab-shell-cd (scilab-shell-active-p)]
     "----"
      
      ["Beginning of Command" scilab-beginning-of-command t]
      ["End of Command" scilab-end-of-command t]
      ["Forward Block" scilab-forward-sexp t]
      ["Backward Block" scilab-backward-sexp t]
     "----"
      ["Previous Function"   scilab-beginning-of-prev-defun t]
      ["Beginning of Function" scilab-beginning-of-defun t]
      ["Go to line of Function" scilab-function-goto-line t]
      ["Whereami" scilab-whereami t]
      ["End of Function" scilab-end-of-defun t]
      ["Next Function"   scilab-beginning-of-next-defun t]
      "----"
      ["Add index menu"  imenu-add-menubar-index 
        (and (scilab-function-file-p) 
        (not (and (boundp 'imenu--last-menubar-index-alist)
		  imenu--last-menubar-index-alist)))]
      ["Which func mode on|off" which-func-mode 
                 :included
                    (and (fboundp `which-func-mode) 
                         (scilab-function-file-p))
                 :active ;;added for compatability of other easy-meny
                    (and (fboundp `which-func-mode) 
                         (scilab-function-file-p))
                 :style toggle 
                 :selected which-func-mode]

)
)
 ;(setq getf-exec (if (scilab-function-file-p) "Save and getf" "Save and exec"))
 (easy-menu-define scilab-mode-menu 
   scilab-mode-map
   "Scilab menu"
   '("Scilab"
     ["Show Version" scilab-show-version t]
     "----"     
     ["Save and getf" scilab-shell-save-and-getf-or-run 
                :included  (scilab-function-file-p)
                :active ;;added for compatability of old easy-menu
                 (scilab-shell-active-p)] 
     ["Save and exec" scilab-shell-save-and-getf-or-run 
                 :included (not (scilab-function-file-p))
                :active (scilab-shell-active-p)] 
                        
     ["Run Region" scilab-shell-run-region (scilab-shell-active-p) ]
     "----"
     ["Find sc[ie] file" scilab-find-file-on-path t]
     ["Change Dir"   scilab-shell-cd (scilab-shell-active-p)]
     "----"     
     ("Auto Fix & Format"
      ["Verify/Fix source" scilab-mode-verify-fix-file t]
      ["Spell check strings" scilab-ispell-strings t]
      ["Spell check comments" scilab-ispell-comments t]
      ["Quietafy source" scilab-mode-vf-quietafy-buffer t]
      "----"
      ["Justify Line" scilab-justify-line t]
      ["Fill Region" scilab-fill-region t]
      ["Fill Comment Paragraph" scilab-fill-paragraph
       (save-excursion (scilab-comment-on-line))]
      ["Join Comment" scilab-join-comment-lines
       (save-excursion (scilab-comment-on-line))]
      ["Indent Current Function" scilab-indent-defun t]

)
     "----"
     ["Run scilab Command" scilab-shell-run-command (scilab-shell-active-p)]
     ["Describe Variable" scilab-shell-describe-variable (scilab-shell-active-p)]
     ["Help on Function" scilab-shell-describe-command t]
     ["Command Apropos" scilab-shell-apropos t]
     ["Topic Browser" scilab-shell-topic-browser t]
     ["Separate help buff" (custom-set-variables
      '(scilab-help-separate-buffer (not scilab-help-separate-buffer)))
       :style toggle :selected scilab-help-separate-buffer]
     "----"     
     ("Edit options" 
      ["Dynamical Indent" 
       (custom-set-variables '(scilab-dynamical-indent (not scilab-dynamical-indent)))
       :style toggle :selected scilab-dynamical-indent]
      ("Indent parameters" :included scilab-dynamical-indent 
      ["Indent Function Body"
       (custom-set-variables '(scilab-indent-function (not scilab-indent-function)))
       :style toggle :selected scilab-indent-function]
      ["Basic Indent " (customize-variable  `scilab-indent-level) t]
      ["After \"...\" Indent" (customize-variable  `scilab-cont-level) t]
      ["Case Indent" (customize-variable  `scilab-case-level) t])
      "----"
      ["Verify File on Save"
       (custom-set-variables '(scilab-verify-on-save-flag (not scilab-verify-on-save-flag)))
       :style toggle :selected scilab-verify-on-save-flag]
     ["Max Verify Buffer"
         (customize-variable  `scilab-block-verify-max-buffer-size) t]
       "----"
      ["Auto Fill does Code"
       (custom-set-variables '(scilab-fill-code (not scilab-fill-code)))
       :style toggle :selected scilab-fill-code ]
      ("Fill parameters"
       ["Fudge"  (customize-variable  `scilab-fill-fudge) t]
       ["Fudge Max"  (customize-variable  `scilab-fill-fudge-hard-maximum) t]
       ["Count \"...\""        (custom-set-variables '(scilab-fill-count-ellipsis-flag  (not scilab-fill-count-ellipsis-flag )))
       :style toggle :selected scilab-fill-count-ellipsis-flag ]
       ["Strings"   
         (custom-set-variables 
          '(scilab-fill-strings-flag  
              (not scilab-fill-strings-flag )))
       :style toggle :selected scilab-fill-strings-flag ])
      "----"
      ("Comments"
       ["Right margin to" (customize-variable  `scilab-comment-column) t]
       ["Region comm-string" (customize-variable  `scilab-comment-region-s) t])
      "---"
      ["Periodic Code Details"
       (custom-set-variables '(scilab-show-periodic-code-details-flag
	     (not scilab-show-periodic-code-details-flag)))
       :style toggle :selected scilab-show-periodic-code-details-flag ]
      ["Highlight Matching Blocks"
       (scilab-enable-block-highlighting)
       :style toggle :selected (member 'scilab-start-block-highlight-timer
				       post-command-hook) ])
     ["Restart mode"  (scilab-mode)]
     ["Customize Faces"  (customize-group 'scilab-face)]
     ["All Preferences"  (customize-group 'scilab)]
      "---"
     ["Start Scilab" scilab-shell (not (scilab-shell-active-p))]
     ["Exit Scilab" scilab-shell-remote-exit (scilab-shell-active-p) ]
     ))
  (if running-xemacs
    (progn
    (easy-menu-add scilab-mode-menu scilab-mode-map)
    (easy-menu-add scilab-navigate-menu scilab-mode-map)
    (easy-menu-add scilab-insert-menu scilab-mode-map)
))

)

;;; Scilab shell =============================================================

(defgroup scilab-shell nil
  "Scilab shell mode."
  :prefix "scilab-shell-"
  :group 'scilab)

(defgroup scilab-setup nil 
  "This group contains variables of `scilab' and `scilab-shell' groups 
   used for the initial setup"  
  :group 'scilab)

(defcustom scilab-launch-automatically nil
  "Not-nil means *Launch `scilab-shell' automatically  in `scilab-mode'
   if any scilab file is open "
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'boolean)

(defcustom scilab-shell-mode-hook nil 
  "*List of functions to call on entry to Scilab shell mode."
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'hook)

(defcustom scilab-shell-command 
  (if (getenv "SCI") 
      (concat (getenv "SCI") "/bin/scilex")
      (if (file-exists-p "/usr/lib/scilab-2.6/bin/scilex")
          "/usr/lib/scilab-2.6/bin/scilex"
          nil))
  "*The name of the command to be run which will start the Scilab process.
  To avoid paths problems it is better to put here the full path command.
  You can use standard \"scilab\" or \"scilex\" utilities or your own 
  shell-scripts running scilab. I do not recommend to put here any flags.
  You wil have variable `scilab-shell-command-switches' for this" 
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'string)

(defcustom scilab-shell-command-switches "-nw"
  "*Command line parameters run with `scilab-shell-command'. The standard
  flag is \"-nw\". If you remove it you will not launch scilab into emacs.
  You can add here your flags"
  :group 'scilab-shell
  :group 'scilab-setup
  :type '(repeat (string :tag "Switch: ")))

(defcustom scilab-shell-main-directory (getenv "SCI")
  "*Main scilab directory usually defined as $SCI. If you occacionally
  did not define it then  here is your chance"
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'string)

;(defcustom scilab-shell-global-key "\C-cs"
;  "Global key for `scilab-shell' command \"^C\" means Ctrl-c, \"^X\" 
;means Ctrl-x,etc" 
;  :group 'scilab-shell
;  :group 'scilab-setup
;  :type 'string)

;;;;###autoload
;(defun scilab-shell-get-global-key ()
;scilab-shell-global-key 
;)

(defcustom scilab-shell-initial-directory  
(if (getenv "SCIHOME") (getenv "SCIHOME") (getenv "HOME"))
  "*Default initial user directory. Emacs will be here in *Scilab* buffer 
at the first time. Default value is  the value of $SCIHOME variable in shell. If $SCIHOME is not defined then $HOME is used. But you can redefine it to your
preference if your want"
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'string)

(defcustom scilab-topics-file-path (if (getenv "MANCHAPTERS") (getenv "MANCHAPTERS") "/tmp/manChapters")
"Name from the variable $MANCHAPTERS. If this variable is not defined then \"/tmp/manChapters\""
 :group 'scilab-shell
 :group 'scilab-setup
 :type 'string)

(defcustom scilab-shell-echoes t
"If non-nil, assume that  `scilab-shell-command' echoes any input.
If so, delete one copy of the input so that only one copy eventually
appears in the buffer. Thus briefly if nil you will get echo for each command."
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'boolean)

;; I  disable customization of this variable unless gud-mode will be
;; implemented for scilab
(defvar scilab-shell-enable-gud-flag nil
  "*Non-nil means to use GUD mode when running the Scilab shell.
  Now it is set to nil since it is not implement yet well in Scilab"
;  :group 'scilab-shell
;  :type 'boolean)
)
(defcustom scilab-shell-server-start nil
 "*Non-nil means that run `scilab-server-start' if it is possible. For Xemacs this is
always `gnuserv-start'. For GNU emacs server `server-start'  is the standard server.
However, `gnuserv-start' with gnuserver  can be installed for emacs too.
 It is very useful if you want to open files and evalulate any lisp commands
directlry from `scilab-shell'. Notice that in the case of `gnuserv-start' it is run
with additional hacks which are ajust this gnuserver to unix shell of scilab personally. Thus many emaces can run many scilab-gnuservers without problems. The server is run
only if scilab-shell is active."
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'boolean)

;;;###autoload
(defun scilab-mode-setup()
"Setup function. When you just got the file scilab.el, it helps to 
configure your scilab with emacs"
(interactive)
(customize-group  'scilab-setup)
;(find-file user-init-file)
;(setq initfile (current-buffer))
;(re-search-forward   
;(global-set-key scilab-shell-global-key 'scilab-shell)
)



(defvar scilab-shell-buffer-name "Scilab"
  "Name used to create `scilab-shell' mode buffers.
This name will have *'s surrounding it.")

(defun scilab-shell-active-p ()
  "Return t if the Scilab shell is active."
  (if (get-buffer (concat "*" scilab-shell-buffer-name "*"))
      (save-excursion
	(set-buffer (concat "*" scilab-shell-buffer-name "*"))
	(if (comint-check-proc (current-buffer))
	    (current-buffer)))))

(defun scilab-shell-active-in-current-buffer-p ()
  "Return t if the Scilab shell is active in the current buffer."
  (if (comint-check-proc (current-buffer))
	    (current-buffer)))

(defvar scilab-shell-mode-map ()
  "Keymap used in `scilab-shell-mode'.")

(defvar scilab-shell-font-lock-keywords-1
  (append scilab-font-lock-keywords scilab-shell-font-lock-keywords)
  "Keyword symbol used for font-lock mode.")

(defvar scilab-shell-font-lock-keywords-2
  (append scilab-shell-font-lock-keywords-1 scilab-gaudy-font-lock-keywords)
  "Keyword symbol used for gaudy font-lock symbols.")

(defvar scilab-shell-font-lock-keywords-3
  (append scilab-shell-font-lock-keywords-2
          scilab-font-lock-solo-keywords
	  scilab-really-gaudy-font-lock-keywords)
  "Keyword symbol used for really gaudy font-lock symbols.")

(eval-when-compile (require 'gud) (require 'comint) (require 'shell))

;;;###autoload
(defun scilab-shell ()
  "Create a buffer with Scilab running as a subprocess."

  (interactive)
  ;; Scilab shell does not work by default on the Windows platform.  Only
  ;; permit it's operation when the shell command string is different from
  ;; the default value.  (True when the engine program is running.)
  (if (and (or (eq window-system 'pc) (eq window-system 'w32))
	   (string= scilab-shell-command "scilab"))
      (error "Scilab cannot be run as a inferior process.  \
Try C-h f scilab-shell RET"))

  (require 'shell)
  (require 'term)
  (require 'gud)

  ;; Make sure this is safe...
  (if (fboundp 'gud-def)
      ;; We can continue using GUD
      nil
;    (message "Sorry, your emacs cannot use the Scilab Shell GUD features.")
    (setq scilab-shell-enable-gud-flag nil))

  (switch-to-buffer (concat "*" scilab-shell-buffer-name "*"))
  (if (scilab-shell-active-p)
      nil
    ;; Build keymap here in case someone never uses comint mode
    (if scilab-shell-mode-map
	()
      (setq scilab-shell-mode-map
	    (let ((km (make-sparse-keymap 'scilab-shell-mode-map)))
;	      (if (fboundp 'set-keymap-parent)
;		  (set-keymap-parent km comint-mode-map)
;		;; 19.31 doesn't have set-keymap-parent
;		(setq km (nconc km comint-mode-map)))
  (define-key km "\ep" 'comint-previous-input)
  (define-key km "\en" 'comint-next-input)
  (define-key km [(control up)] 'comint-previous-input)
  (define-key km [(control down)] 'comint-next-input)
  (define-key km "\er" 'comint-previous-matching-input)
  (define-key km "\es" 'comint-next-matching-input)
  (define-key km [?\C-c ?\M-r] 'comint-previous-matching-input-from-input)
  (define-key km [?\C-c ?\M-s] 'comint-next-matching-input-from-input)
  (define-key km "\e\C-l" 'comint-show-output)
  (define-key km "\C-m" 'comint-send-input)
  (define-key km "\C-d" 'comint-delchar-or-maybe-eof)
  (define-key km "\C-c " 'comint-accumulate)
  (define-key km "\C-c\C-x" 'comint-get-next-from-history)
  (define-key km "\C-c\C-a" 'comint-bol-or-process-mark)
  (define-key km "\C-c\C-u" 'comint-kill-input)
  (define-key km "\C-c\C-w" 'backward-kill-word)
  (define-key km "\C-c\C-c" 'comint-interrupt-subjob)
  (define-key km "\C-c\C-z" 'comint-stop-subjob)
  (define-key km "\C-c\C-\\" 'comint-quit-subjob)
  (define-key km "\C-c\C-m" 'comint-copy-old-input)
  (define-key km "\C-c\C-o" 'comint-kill-output)
  (define-key km "\C-c\C-r" 'comint-show-output)
  (define-key km "\C-c\C-e" 'comint-show-maximum-output)
  (define-key km "\C-c\C-l" 'comint-dynamic-list-input-ring)
  (define-key km "\C-c\C-n" 'comint-next-prompt)
  (define-key km "\C-c\C-p" 'comint-previous-prompt)
  (define-key km "\C-c\C-d" 'comint-send-eof)

	      (substitute-key-definition 'next-error 'scilab-shell-last-error
					 km global-map)
	      (define-key km [(control h) return]	scilab-help-map)
;	      (define-key km [(tab)] 'comint-dynamic-complete-filename)
	      (define-key km [(tab)] 'scilab-complete-symbol)
	      (define-key km [(control up)] 'comint-previous-matching-input-from-input)
	      (define-key km [(control down)] 'comint-next-matching-input-from-input)
;	      (define-key km [up] 'comint-previous-matching-input-from-input)
;	      (define-key km [down] 'comint-next-matching-input-from-input)
	      (define-key km [(control backspace)] 'comint-kill-input)
	      (define-key km "\C-c\C-a" 'scilab-abort-subjob)
	      (define-key km "\C-c\C-c" 'comint-interrupt-subjob)
	      (define-key km "\C-c\C-e" 'scilab-shell-exit)
	      (define-key km "\C-c\C-r" 'scilab-shell-restart)
	      (define-key km "\C-c\C-k" 'comint-kill-subjob)
              (define-key km [(control c) ?.] 'scilab-find-file-on-path)
              (define-key km "\C-c/" 'scilab-shell-cd)
              (define-key km "\C-cd" 'scilab-shell-cd)
              (define-key km "\C-x\\" 'scilab-shell-after-pause)
              (define-key km "\C-cc" 'scilab-shell-run-command)
              (define-key km "\C-cr" 'scilab-shell-run-region)
 	      (define-key km [(control return)] 'scilab-continue-subjob)
	      (define-key km "\C-?" 'scilab-shell-delete-backwards-no-prompt)
	      (define-key km [(backspace)] 'scilab-shell-delete-backwards-no-prompt)
	      km)))
    (if running-xemacs 
       (progn 
	 (if (boundp 'system-uses-terminfo)
           (setq scilab-system-uses-terminfo system-uses-terminfo)
	   (setq scilab-system-uses-terminfo nil))
           (setq system-uses-terminfo t))
    )

    (switch-to-buffer
     (make-comint scilab-shell-buffer-name scilab-shell-command
		  nil scilab-shell-command-switches))
   (if running-xemacs 
   (setq system-uses-terminfo scilab-system-uses-terminfo)
   )
   (comint-mode)
     (if running-xemacs 
       (progn
         (delete-menu-item '("Complete"))
         (delete-menu-item '("In/Out"))
         (delete-menu-item '("Signals"))
))
    (setq shell-dirtrackp t)
    (if scilab-shell-enable-gud-flag
	(progn
	  (gud-mode)
	  (make-local-variable 'gud-marker-filter)
	  (setq gud-marker-filter 'gud-scilab-marker-filter)
	  (make-local-variable 'gud-find-file)
	  (setq gud-find-file 'gud-scilab-find-file)
	  (make-local-variable 'gud-format-command)
	  (setq gud-find-file 'gud-scilab-format-command)
	  (set-process-filter (get-buffer-process (current-buffer))
			      'gud-filter)
	  (set-process-sentinel (get-buffer-process (current-buffer))
				'gud-sentinel)
	  (gud-set-buffer))
      ;; What to do when there is no GUD
      ;(set-process-filter (get-buffer-process (current-buffer))
;			  'scilab-shell-process-filter)
      )
  
;;;server-start. For running any emacs commands from scilab
; (if scilab-shell-server-start (scilab-server-start))
       

 
    ;; Comint and GUD both try to set the mode.  Now reset it to
    ;; scilab mode.
 (setq scilab-shell-libfunc-isbuilt nil)
 (scilab-shell-mode)

))

(defun scilab-shell-versions ()
"Save in scilab variabe %version versions of scilab, emacs,scilab-mode
 and client-mode"
   (interactive)
   (setq emacsname 
         (concat (if running-xemacs  "'XEmacs-" "'GNU Emacs-") version-emacs "'"))
   (setq client (scilab-server-status))
   (setq vv (concat emacsname ";'scilab-mode " scilab-mode-version "';'" client "'"))  
   (setq p (point-max))
   (scilab-shell-collect-command-output
        (concat "if ~exists('%version') then \n"  
                 "errcatch(4,'continue','nomessage');%version=getversion();\n"
                 "if iserror(4), errclear(4);%version='scilab < 2.5.1';end\n"
                 "errcatch(4,'kill'); end\n"
                 "%version=[%version(1);" vv  "];\n"
))
   (setq scilab-mode-all-versions 
         (substring 
	  (scilab-shell-collect-command-output
	   "write(%io(2),strcat(%version([3,2,4]),' '));")
          0 -3)
	 )
     (message "Updated scilab variable %%version: %s" scilab-mode-all-versions)
)
(defcustom scilab-shell-logo (concat (getenv "SCI") "/X11_defaults/scilab.xpm")
  "*The Scilab logo file."
  :group 'scilab-shell
  :type '(choice (const :tag "None" nil)
		 (file :tag "File" "")))

 
(defun scilab-shell-hack-logo (str)
  "Replace the text logo with a real logo.
STR is passed from the commint filter."
  (when (string-match "S c i l a b" str)
    (save-excursion
      (when (re-search-backward 
                "===========\n[ \t]+S c i l a b\n[ \t]+==========="
                 (point-min) t)
 	(delete-region (match-beginning 0) (match-end 0))
        (goto-char (match-beginning 0))
 	(set-extent-begin-glyph (make-extent (point) (point))
 				(make-glyph scilab-shell-logo))))
    ;; Remove this function from `comint-output-filter-functions'
    (remove-hook 'comint-output-filter-functions
 		 'scilab-shell-hack-logo)))

(defun scilab-shell-mode ()
  "Run Scilab as a subprocess in an Emacs buffer.

This mode will allow standard Emacs shell commands/completion to occur
with Scilab running as an inferior process.  Additionally, this shell
mode is integrated with `scilab-mode', a major mode for editing sci
code.

> From an sci file buffer:
\\<scilab-mode-map>
\\[scilab-shell-save-and-getf-or-run] - Save the current sci file, and getf it in a \
Scilab shell. If this is script then runs it.

> From Shell mode:
\\<scilab-shell-mode-map>
\\[scilab-shell-mode] - restart scilab shell mode
\\[scilab-shell-last-error] - find location of last Scilab runtime error \
in the offending sci file.
\\[scilab-shell-after-pause] - find the current pause-line 
\\[scilab-shell-active-p] - check either scilab is active in Scilab buffer
\\[scilab-shell-active-in-current-buffer-p] - check either scilab is active in the current buffer
\\[scilab-shell-additional] - run additional scilabs as subprocesses in 
emacs buffers. Auxiliary shells has much less features but sometimes they are useful
> From an sci file, or from Shell mode:
\\[scilab-shell-whereami] - Display current instruction calling tree
\\[scilab-shell-cd] - Change directory both in scilab and the current buffer
\\[scilab-shell-demos] - Runs scilab demos
\\[scilab-shell] - start scilab 
\\[scilab-shell-exit] - Exist scilab 
\\[scilab-shell-restart] - restart scilab 
\\<scilab-mode-map>
\\[scilab-shell-run-command] - Run COMMAND and show result in a popup buffer.
\\[scilab-shell-describe-variable] - Show variable contents in a popup buffer.
\\[scilab-shell-describe-command] - Show online documentation for a command \
in a popup buffer.
\\[scilab-shell-apropos] - Show output from apropos command in a popup buffer.
\\[scilab-shell-topic-browser] - Topic browser using HELP.

> Keymap:
\\{scilab-mode-map}"
  (setq major-mode 'scilab-shell-mode
	mode-name "Scilab-Shell"
	comint-prompt-regexp "-[1-100]?-> *"
	comint-delimiter-argument-list (list [ 59 ]) ; semi colon
	comint-dynamic-complete-functions '(comint-replace-by-expanded-history)
	comint-process-echoes scilab-shell-echoes
	)
  ;;(add-hook 'comint-input-filter-functions 'shell-directory-tracker)
  ;; Add a spiffy logo if we are running XEmacs
  (if (and running-xemacs window-system
	   (stringp scilab-shell-logo)
	   (file-readable-p scilab-shell-logo))
      (add-hook 'comint-output-filter-functions 'scilab-shell-hack-logo))
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (use-local-map scilab-shell-mode-map)
  (set-syntax-table scilab-mode-syntax-table)
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender (function scilab-simple-send))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((scilab-shell-font-lock-keywords-1
			      scilab-shell-font-lock-keywords-2
			      scilab-shell-font-lock-keywords-3
)
			     t nil ((?_ . "w"))))
  (make-local-variable 'gud-marker-acc)
  (setq  scilab-shell-number-graphic-window 0)
  (if window-system (scilab-shell-frame-init)) 

   (if scilab-shell-enable-gud-flag
       (progn
        (gud-scilab-make-debug-menu)
 	(set-marker comint-last-output-start (point-max))
 	))
  (make-local-variable 'next-line-add-newlines)
  (setq next-line-add-newlines nil)
  (run-hooks 'scilab-shell-mode-hook)
  (while (not (scilab-on-empty-prompt-p))
	(accept-process-output (get-buffer-process (current-buffer)))
  )
  (scilab-shell-cd scilab-shell-initial-directory)
  (scilab-show-version)
;;;server-start. For running any emacs commands from scilab
  (if scilab-shell-server-start 
      (scilab-server-start)
    (scilab-shell-versions)
  )

 ;  (let ((allnames (if scilab-libfunc-list-path 
;                     scilab-libfunc-list-path 
; 		   (concat scilab-shell-initial-directory "/libfunc"))))
;   (if (file-exists-p allnames) 
;       (delete-file allnames)))
 (if (and (boundp 'font-lock-mode) (fboundp 'font-lock-mode))
    (if scilab-font-lock-mode 
        (turn-on-font-lock)  
        (font-lock-mode -1)))
  

)
(defun scilab-shell-frame-init()
  "Initialize Emacs 19+ menu and Xemacs system. in `scilab-shell-mode' (editing mode)"
;;I grabbed this menu from the standard comint I/O menu, but many stuffs
;; I removed as not useful (by my private opinion)
  (use-local-map scilab-shell-mode-map)
 (easy-menu-remove-item nil  nil "SciI/O") 
 (easy-menu-define
   scilab-shell-IO-menu
   scilab-shell-mode-map
   "Scilab shell I/O menu"
   '("SciI/O"
     ["Expand History Before Point"  comint-replace-by-expanded-history]
     ["List Input History"   comint-dynamic-list-input-ring]
     ["Previous Input"   comint-previous-input]
     ["Next Input"   comint-next-input]   
     ["Previous Matching Current Input"   comint-previous-matching-input-from-input]
     ["Next Matching Current Input"  comint-next-matching-input-from-input]
     ["Previous Matching Input..."   comint-previous-matching-input]
     ["Next Matching Input..."   comint-next-matching-input]
     ["Backward Matching Input..."   comint-backward-matching-input]
     ["Forward Matching Input..."   comint-forward-matching-input]
     ["Copy Old Input"  comint-copy-old-input]
     ["Kill Current Input"  comint-kill-input]
  ))
 (easy-menu-remove-item nil  nil "Complete") 
 (easy-menu-define
   scilab-shell-complete-menu
   scilab-shell-mode-map
   "Scilab shell complete menu"
   '("Complete"
     ["Complete Name"         scilab-complete-symbol]
     ["Expand File Name"       comint-replace-by-expanded-filename]
     ["Complete Before Point"  comint-dynamic-complete]
     ["Set Completion Type" (customize-variable 'scilab-completion-technique)] 
  
))
 (easy-menu-remove-item nil  nil "SciCtrl") 
(easy-menu-define
   scilab-shell-control-menu
   scilab-shell-mode-map
   "Scilab shell control menu"
   '("SciCtrl"
     ["Break" comint-interrupt-subjob :included(scilab-shell-active-p)
                                      :active(scilab-shell-active-p)]
     ["Abort"  scilab-abort-subjob :included(scilab-shell-active-p)
                                   :active(scilab-shell-active-p)]
     ["Resume" scilab-continue-subjob :included(scilab-shell-active-p) 
                                     :active(scilab-shell-active-p)] 
     "----"
     ["Start" scilab-shell (not (scilab-shell-active-p))]
     ["Restart" scilab-shell-restart (scilab-shell-active-p)]
     ["Exit" scilab-shell-exit (scilab-shell-active-p)]
     ["Quit" comint-quit-subjob (scilab-shell-active-p)]
     ["Kill" comint-kill-subjob (scilab-shell-active-p)]
     "----"
     ["More scilabs" scilab-shell-additional]
     "----"
     ["Server start" scilab-server-start
                 :included
		 (and (scilab-shell-active-p) (not scilab-server-status))
                 :active ;;added for compatability of other easy-meny
		 (and (scilab-shell-active-p) (not scilab-server-status))]
     ["Server kill" scilab-server-kill
                 :included
		 (and scilab-server-status (scilab-shell-active-p))
                 :active ;;added for compatability of other easy-meny
		 (and scilab-server-status (scilab-shell-active-p))]
)

)
 (easy-menu-remove-item nil  nil scilab-shell-graphic-menu-title) 
 (setq scilab-shell-graphic-menu-title (concat
      "Graphic "      
      (int-to-string  scilab-shell-number-graphic-window)))
 (easy-menu-define
   scilab-shell-graphic-menu
   scilab-shell-mode-map
   "Scilab shell graphic windows  menu"
   `(,scilab-shell-graphic-menu-title 
     ["Open Window" scilab-shell-open-graphic-window 
             :included(scilab-shell-active-p) 
             :active(scilab-shell-active-p) ]
     ["Next" (scilab-shell-plus-graphic-window)
             :included(scilab-shell-active-p) 
             :active(scilab-shell-active-p) ]
     ["Previous" (scilab-shell-minus-graphic-window)
             :included(scilab-shell-active-p) 
             :active(scilab-shell-active-p) ]
     ["Close Window" scilab-shell-close-current-figure 
             :included(scilab-shell-active-p)
             :active(scilab-shell-active-p)]
     ["Close All" scilab-shell-close-figures 
             :included(scilab-shell-active-p)
             :active(scilab-shell-active-p)]
  ))

 (easy-menu-remove-item nil nil "Scilab")
  (easy-menu-define
   scilab-general-menu
   scilab-shell-mode-map
   "Scilab general menu"
   '("Scilab"
     ["Goto last error" scilab-shell-last-error t]
     ["Goto after pause" scilab-shell-after-pause t]
     ["Run Command" scilab-shell-run-command :included(scilab-shell-active-p)
                                             :active(scilab-shell-active-p)]
     ["Run Region" scilab-shell-run-region :included(scilab-shell-active-p)
                                             :active(scilab-shell-active-p)]
     "----"
     ["Find sc[ie] file|func" scilab-find-file-on-path t]
     ["Change Dir"   scilab-shell-cd (scilab-shell-active-p)]
     "----"
     ["Shell whereami" scilab-shell-whereami (scilab-shell-active-p)]
     ["Describe Variable" scilab-shell-describe-variable 
            :included(scilab-shell-active-p)
            :active(scilab-shell-active-p)]
     ["Help on Function" scilab-shell-describe-command t]
     ["Apropos (Key) Command" scilab-shell-apropos t]
     ["Topic Browser" scilab-shell-topic-browser t]
     ["Separate help buff" (custom-set-variables
      '(scilab-help-separate-buffer (not scilab-help-separate-buffer)))
       :style toggle :selected scilab-help-separate-buffer]
     "----"
     ["Demos" scilab-shell-demos 
             :included(scilab-shell-active-p)
             :active(scilab-shell-active-p)]
     "----"
     ["Open  Graphic Window" scilab-shell-open-graphic-window 
             :included(scilab-shell-active-p) 
             :active(scilab-shell-active-p) ]
     "----"
     ["Restart mode"  (scilab-shell-mode)]
     ["Setup mode"  (scilab-mode-setup)]
     ["Customize Faces"  (customize-group 'scilab-face)]
     "----"
     ["More scilabs" scilab-shell-additional]
))

(if running-xemacs 
  (progn
    (easy-menu-add scilab-general-menu scilab-shell-mode-map)
    (easy-menu-add scilab-shell-graphic-menu scilab-shell-mode-map)
    (easy-menu-add scilab-shell-control-menu scilab-shell-mode-map)
    (easy-menu-add scilab-shell-complete-menu scilab-shell-mode-map)
    (easy-menu-add scilab-shell-IO-menu scilab-shell-mode-map)
 ))
;;Dummy action
(scilab-shell-dummy-action)
(message "Menus updated")
)

(defun scilab-shell-dummy-action()
"Dummy action. Does nothing"

  (if (not (scilab-on-prompt-p))
       nil
    (save-excursion
      (let ((pos nil)
	    (lastcmd))
      (goto-char (point-max))
      (beginning-of-line)
      (re-search-forward comint-prompt-regexp)
      (setq lastcmd (buffer-substring (point) (scilab-point-at-eol)))
      (delete-region (point) (scilab-point-at-eol))
      (setq pos (point))
      (comint-send-string (get-buffer-process (current-buffer)) "\n")
      (while (or (>= pos (point)) (not (scilab-on-empty-prompt-p)))
	(accept-process-output (get-buffer-process (current-buffer)))
	(goto-char (point-max))
      )
 	(goto-char pos)
	(beginning-of-line)
	(delete-region pos (point-max))
        (insert lastcmd))
)))  

(defun scilab-shell-additional ()
  "Create an additional buffer with additional Scilab running as a subprocess.
If in the main scilab buffer there is no process then rerun the basic scilab"
(interactive)
(if (not (scilab-shell-active-p)) (scilab-shell)
  (let ((nobuff t) (num 1) (buffname nil))
    (while nobuff
      (setq buffname (concat scilab-shell-buffer-name "-" (int-to-string num)))
      (switch-to-buffer (concat "*" buffname "*"))
      (if (scilab-shell-active-in-current-buffer-p) 
          (setq num (+ num 1))
          (setq nobuff nil)))
   (comint-mode)
   (switch-to-buffer (make-comint buffname  scilab-shell-command
		  nil scilab-shell-command-switches))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((scilab-shell-font-lock-keywords-1
			      scilab-shell-font-lock-keywords-2
			      scilab-shell-font-lock-keywords-3
  )))    
  (make-local-variable 'next-line-add-newlines)
  (setq next-line-add-newlines nil)
  (make-local-variable 'comint-process-echoes)
  (setq comint-process-echoes scilab-shell-echoes)
  (run-hooks 'scilab-shell-mode-hook)
  )))
 
       
(defun gud-scilab-make-debug-menu()
"Make `gud-def's and runs `gud-make-debug-menu'"
	(gud-def gud-break  (concat "setbpt('" (scilab-which-function) "'," (scilab-get-number-line-in-function) ")") "\C-b" "Set breakpoint at current line.")
;(gud-def gud-break  (concat "setbpt('%y',%L)" "\C-b" "Set breakpoint at current line.")
;	(gud-def gud-break (concat "setbpt('" (substring %f  0 (- (length %f) 3)) "'," "%l")  "\C-b" "Set breakpoint at current line.")	
;     (gud-def gud-remove "delbpt at %l in %f"     "\C-d" "Remove breakpoint at current line")
     (gud-def gud-remove  (concat "delbpt('" (scilab-which-function) "'," (scilab-get-number-line-in-function) ")")    "\C-d" "Remove breakpoint at current line")

	(gud-def gud-step   "dbstep %p"           "\C-s" "Step one source line with display.")
	(gud-def gud-cont   "resume"              "\C-r" "Continue with display.");;;
	(gud-def gud-finish "dbquit"              "\C-f" "Finish executing current function.")
	(gud-def gud-up     "dbup %p"             "<"    "Up N stack frames (numeric arg).")
	(gud-def gud-down   "dbdown %p"           ">"    "Down N stack frames (numeric arg).")
	(gud-def gud-print  "%e"                  "\C-p" "Evaluate sci expression at point.")
	(if (fboundp 'gud-make-debug-menu)
	    (gud-make-debug-menu))
	(if (fboundp 'gud-overload-functions)
	    (gud-overload-functions
	     '((gud-massage-args . gud-scilab-massage-args)
	       (gud-marker-filter . gud-scilab-marker-filter)
	       (gud-find-file . gud-scilab-find-file))))
	;; XEmacs doesn't seem to have this concept already.  Oh well.
	(setq gud-marker-acc nil)
	)

(defvar gud-scilab-marker-regexp-1 "^K-[1-100]?->"
  "Regular expression for finding a file line-number.")

(defvar gud-scilab-marker-regexp-2
  "^> In \\([-.a-zA-Z0-9_/@]+\\) \\((\\w+) \\|\\)at line \\([0-9]+\\)[ \n]+"
  "Regular expression for finding a file line-number.
Please note: The > character represents the current stack frame, so if there
are several frames, this makes sure we pick the right one to popup.")

(defvar gud-scilab-marker-regexp-prefix "^> In "
  "A prefix to scan for to know if output might be scarfed later.")

;; The regular expression covers the following form:
;; Errors:  !--error <number>
;;          error_string
;;          at line <line_number> of function <fun_name> called by:
;;          <command line>
(defvar gud-scilab-error-regexp
  (concat "\\s-*\\(!--error\\).*\n"
  "\\(.*\n\\)+"
  "at line[ \t]+\\([0-9]+\\) of function \\(\\S-+\\)"
;\\s-+called by:\\s-*\n"
  )
  "Regular expression finding where an error occurred.")

(defvar gud-scilab-pause-regexp
  "\\s-*\\(%?[a-zA-Z0-9#_]+\\)[ \t]+called at line\\s-+\\([0-9]+\\)\\s-+of macro\\s-+\\(%?[a-zA-Z0-9#_]+\\>\\)"
  "Regular expression finding where a pause  occurred.")

(defvar gud-scilab-pause-regexp2
  "\\s-*\\(Stop after row\\)\\s-+\\([0-9]+\\)\\s-+in function\\s-+\\(%?[a-zA-Z0-9#_]+\\>\\)"
  "Regular expression type 2 finding where a pause  occurred.")
(defvar gud-scilab-pause-regexp3
  "\\(at line\\|line\\)[ \t]+\\([0-9]+\\) of function \\(\\S-+\\)")

(defvar scilab-last-frame-returned nil
  "Store the previously returned frame for Scilabs difficult debugging output.
It is reset to nil whenever we are not prompted by the K-[1-100]?-> output.")

(defvar scilab-one-db-request nil
  "Set to t if we requested a debugger command trace.")

(defun gud-scilab-massage-args (file args)
  "Argument massager for starting scilab file.
I don't think I have to do anything, but I'm not sure.
FILE is ignored, and ARGS is returned."
  args)

(defun gud-scilab-marker-filter (string)
  "Filters STRING for the Unified Debugger based on Scilab output.
Swiped ruthlessly from GDB mode in gud.el"
  (let ((garbage (concat "\\(" (regexp-quote "\C-g") "\\|"
 			 (regexp-quote "\033[H0") "\\|"
 			 (regexp-quote "\033[H\033[2J") "\\|"
 			 (regexp-quote "\033H\033[2J") "\\)")))
    (while (string-match garbage string)
      (if (= (aref string (match-beginning 0)) ?\C-g)
	  (beep t))
      (setq string (replace-match "" t t string))))
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output "") (frame nil))

    ;; Remove output from one stack trace...
    (if (eq scilab-one-db-request t)
	(if (string-match "db[a-z]+[ \n]+" gud-marker-acc)
	    (setq gud-marker-acc (substring gud-marker-acc (match-end 0))
		  scilab-one-db-request 'prompt)))

    ;; Process all the complete markers in this chunk.
    (while (and (not (eq scilab-one-db-request t))
		(string-match gud-scilab-marker-regexp-2 gud-marker-acc))

      (setq

       ;; Extract the frame position from the marker.
       frame (cons (match-string 1 gud-marker-acc)
		   (string-to-int (substring gud-marker-acc
					     (match-beginning 3)
					     (match-end 3))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       ;; If this is not a requested piece of text, then include
       ;; it into the output.
       output (concat output
		      (substring gud-marker-acc 0
				 (if scilab-one-db-request
				     (match-beginning 0)
				   (match-end 0))))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    (if frame
	(progn
	  ;; We have a frame, so we don't need to do extra checking.
	  (setq scilab-last-frame-returned frame)
	  )
      (if (and (not scilab-one-db-request)
	       (string-match gud-scilab-marker-regexp-1 gud-marker-acc))
	  (progn
	    ;; Here we know we are in debug mode, so find our stack, and
	    ;; deal with that later...
	    (setq scilab-one-db-request t)
	    (process-send-string (get-buffer-process gud-comint-buffer)
				 "dbstack\n"))))

    ;; Check for a prompt to nuke...
    (if (and (eq scilab-one-db-request 'prompt)
	     (string-match "^K?-[1-100]?-> $" gud-marker-acc))
	(setq scilab-one-db-request nil
	      output ""
	      gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Finish off this part of the output.  None of our special stuff
    ;; ends with a \n, so display those as they show up...
    (while (string-match "^[^\n]*\n" gud-marker-acc)
      (setq output (concat output (substring gud-marker-acc 0 (match-end 0)))
	    gud-marker-acc (substring gud-marker-acc (match-end 0))))

    (if (string-match gud-scilab-marker-regexp-prefix gud-marker-acc)
	;; We could be collecting something.  Wait for a while.
	nil
      (setq output (concat output gud-marker-acc)
	    gud-marker-acc "")
      ;; Check our output for a prompt, and existence of a frame.
      ;; If t his is true, throw out the debug arrow stuff.
      (if (and (string-match "^-[1:100]?-> $" output)
	       gud-last-last-frame)
	  (progn
	    (setq overlay-arrow-position nil
		  gud-last-last-frame nil)
	    (sit-for 0)
	    )))

    (if frame (setq gud-last-frame frame))

    ;;(message "[%s] [%s]" output gud-marker-acc)

    output))
;;We write here the new version of function gud-scilab-find-file taking in account
;; that scilab accepts many function in one file. The old version is commented.
(defun gud-scilab-find-file (f)
  "Find file F or function when debugging frames in Scilab."
  (save-excursion
      (scilab-find-file-on-path f)
      (if scilab-shell-enable-gud-flag 
        (gud-scilab-make-debug-menu))     
      (current-buffer)))


;;;(defun gud-scilab-find-file (f)
;;;  "Find file F when debugging frames in Scilab."
;;;  (save-excursion
;;;    (let* ((realfname (if (string-match "\\.\\(p\\)$" f)
;;;			  (progn
;;;			    (aset f (match-beginning 1) ?m)
;;;			    f)
;;;			f))
;;;	   (buf (find-file-noselect realfname)))
;;;     (set-buffer buf)
;;;      (if (fboundp 'gud-make-debug-menu)
;;;	  (gud-make-debug-menu))
;;;      buf)))

;;Scilab version of gud-format-command, extend format to catch functions names
;; and lines into functions not in file
(defun gud-scilab-format-command (str arg)
"Local version of `gud-format-command'. Add format "

  (let ((insource (not (eq (current-buffer) gud-comint-buffer)))
	(frame (or gud-last-frame gud-last-last-frame))
	result)
    (while (and str (string-match "\\([^%]*\\)%\\([adeflpLy]\\)" str))
      (let ((key (string-to-char (substring str (match-beginning 2))))
	    subst)
	(cond
	 ((eq key ?f)
	  (setq subst (file-name-nondirectory (if insource
						  (buffer-file-name)
						(car frame)))))
	 ((eq key ?F)
	  (setq subst (file-name-sans-extension
		       (file-name-nondirectory (if insource
						   (buffer-file-name)
						 (car frame))))))
	 ((eq key ?y)
	  (setq subst (if insource (scilab-which-function)
						 (car frame))))

	 ((eq key ?d)
	  (setq subst (file-name-directory (if insource
					       (buffer-file-name)
					     (car frame)))))
	 ((eq key ?l)
	  (setq subst (if insource
			  (save-excursion
			    (beginning-of-line)
			    (save-restriction (widen)
					      (1+ (count-lines 1 (point)))))
			(cdr frame))))
	 ((eq key ?L)
	  (setq subst (if insource
			(scilab-get-number-line-in-function)
			(cdr frame))))

	 ((eq key ?e)
	  (setq subst (gud-find-c-expr)))
	 ((eq key ?a)
	  (setq subst (gud-read-address)))
	 ((eq key ?p)
	  (setq subst (if arg (int-to-string arg) ""))))
	(setq result (concat result
			     (substring str (match-beginning 1) (match-end 1))
			     subst)))
      (setq str (substring str (match-end 2))))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun scilab-shell-delete-backwards-no-prompt (&optional arg)
  "Delete one char backwards without destroying the scilab prompt.
Optional argument ARG describes the number of chars to delete."
  (interactive "P")
  (let ((promptend (save-excursion
		     (beginning-of-line)
		     (if (looking-at "K?-[1:100]?-> ")
			 (match-end 0)
		       (point))))
	(numchars (if (integerp arg) (- arg) -1)))
    (if (<= promptend (+ (point) numchars))
	(delete-char numchars)
      (error "Beginning of line"))))


;;; Scilab Shell Commands =====================================================

(defun scilab-read-word-at-point ()
  "Get the word closest to point, but do not change position.
Has a preference for looking backward when not directly on a symbol.
Snatched and hacked from dired-x.el"
  (let ((word-chars "a-zA-Z0-9_")
	(bol (scilab-point-at-bol))
	(eol (scilab-point-at-eol))
        start)
    (save-excursion
      ;; First see if just past a word.
      (if (looking-at (concat "[" word-chars "]"))
	  nil
	(skip-chars-backward (concat "^" word-chars "{}()\[\]") bol)
	(if (not (bobp)) (backward-char 1)))
      (if (numberp (string-match (concat "[" word-chars "]")
				 (char-to-string (following-char))))
          (progn
            (skip-chars-backward word-chars bol)
            (setq start (point))
            (skip-chars-forward word-chars eol))
        (setq start (point)))		; If no found, return empty string
      (buffer-substring start (point)))))

(defun scilab-read-line-at-point ()
  "Get the line under point, if command line."
  (if (eq major-mode 'scilab-shell-mode)
      (save-excursion
	(beginning-of-line)
	(if (not (looking-at (concat comint-prompt-regexp)))
	    ""
	  (search-forward-regexp comint-prompt-regexp)
	  (buffer-substring (point) (scilab-point-at-eol))))
    (save-excursion
      ;; In scilab buffer, find all the text for a command.
      ;; so back over until there is no more continuation.
      (while (save-excursion (forward-line -1) (scilab-lattr-cont))
	(forward-line -1))
      ;; Go forward till there is no continuation
      (beginning-of-line)
      (let ((start (point)))
	(while (scilab-lattr-cont) (forward-line 1))
	(end-of-line)
	(buffer-substring start (point))))))

(defun scilab-non-empty-lines-in-string (str)
  "Return number of non-empty lines in STR."
  (let ((count 0)
	(start 0))
    (while (string-match "^.+$" str start)
      (setq count (1+ count)
	    start (match-end 0)))
    count))

(defun scilab-output-to-temp-buffer (buffer output)
  "Print output to temp buffer, or a message if empty string.
BUFFER is the buffer to output to, and OUTPUT is the text to insert."
  (let ((lines-found (scilab-non-empty-lines-in-string output)))
    (cond ((= lines-found 0)
	   (message "(Scilab command completed with no output)"))
	  ((= lines-found 1)
	   (string-match "^.+$" output)
	   (message (substring output (match-beginning 0)(match-end 0))))
	  (t (with-output-to-temp-buffer buffer (princ output))
	     (save-excursion
	       (set-buffer buffer)
               (if (equal buffer "*SciApropos*")
               (scilab-shell-topic-mode)
	       (scilab-shell-help-mode))
)))))

(defun scilab-shell-run-command (command)
  "Run COMMAND and display result in a buffer.
This command requires an active Scilab shell."
  (interactive (list (read-from-minibuffer
 		      "Scilab command line: "
 		      (cons (scilab-read-line-at-point) 0))))
;  (let ((doc (scilab-shell-collect-command-output command)))
 (scilab-shell-collect-command-output "%tmp=lines();lines(0)")
 (let ((doc (scilab-shell-collect-command-output command)))
 (scilab-output-to-temp-buffer "*SciCommand*" doc))
 (scilab-shell-collect-command-output "lines(%tmp(2),%tmp(1)); clear %tmp"))

(defun scilab-shell-run-graphic (command)
  "Run COMMAND and display result in a buffer.
This command requires an active Scilab shell."
  (interactive (list (read-from-minibuffer
 		      "Scilab graphic command: "
 		      (cons (scilab-read-line-at-point) 0))))
;  (let ((doc (scilab-shell-collect-command-output command)))
 (scilab-shell-collect-command-output command))


(defun scilab-shell-describe-variable (variable)
  "Get the contents of VARIABLE and display them in a buffer.
This uses the wh (My utilita) command to find viable commands.
This command requires an active Scilab shell."
  (interactive (list (read-from-minibuffer
 		      "Scilab variable: "
 		      (cons (scilab-read-word-at-point) 0))))
(let ( (err (scilab-shell-collect-command-output (concat
         "[#obj,#err]=evstr('" variable "'); write(%io(2),#err,'(i4)');")))
       (vartype (string-to-int (scilab-shell-collect-command-output
        "write(%io(2),type(#obj),'(i2)');clear #err;")))
        vardescr)
(setq err (string-to-int err))
(cond ((= err 4) 
  (setq vardescr (concat "Variable " variable " is not defined")))
     ((= err 25)
     (setq vardescr (concat "Primitive (builtin) function\n\n    "  variable)))
     ((= vartype 1)
     (setq vardescr (scilab-shell-collect-command-output 
        "disp('Numeric (float) Matrix');disp(size(#obj));clear #obj")))
     ((= vartype 2)
     (setq vardescr (scilab-shell-collect-command-output 
        "disp('Polynomial Matrix');disp(size(#obj));clear #obj")))
     ((= vartype 4)
     (setq vardescr (scilab-shell-collect-command-output 
        "disp('Boolean Matrix');disp(size(#obj));clear #obj")))
     ((= vartype 5)
     (setq vardescr (scilab-shell-collect-command-output 
        "disp('Sparse Matrix');disp(size(#obj));clear #obj")))
     ((= vartype 8)
     (setq vardescr (scilab-shell-collect-command-output 
        "disp('Integer Matrix');disp(size(#obj));clear #obj")))
     ((= vartype 10)
     (setq vardescr (scilab-shell-collect-command-output (concat
             "disp('String (Matrix)');"
             "if and(size(#obj)==[1 1]),disp(length(#obj)); else " 
             "disp(size(#obj)); end; clear #obj"))))
    ((= vartype 11)
     (setq vardescr (scilab-shell-collect-command-output (concat 
        "disp('Un-compiled function');##=macrovar(#obj);"
        "disp('['+string(strcat(##(2),','))+']="  variable 
        "('+strcat(string(##(1)),',')+')'); clear ## #obj"))))
    ((= vartype 13)
     (setq vardescr (scilab-shell-collect-command-output (concat 
        "disp('Compiled function');##=macrovar(#obj);"
        "disp('['+string(strcat(##(2),','))+']="  variable 
        "('+strcat(string(##(1)),',')+')'); clear ## #obj"))))
    ((= vartype 14)
     (setq vardescr (scilab-shell-collect-command-output 
        "disp('Function Library');disp(#obj);clear #obj")))
    ((= vartype 15)
     (setq vardescr (scilab-shell-collect-command-output 
        "disp('List');disp(length(#obj));clear #obj")))
   ((= vartype 16)
     (setq vardescr (scilab-shell-collect-command-output (concat 
        "disp('Typed List (tlist): '+#obj(1)(1));"
        "disp('Number of fields: '+string(length(#obj)-1));clear #obj;"))))
   ((= vartype 17)
     (setq vardescr (scilab-shell-collect-command-output
        "#=getfield(1,#obj); disp('Mlist: '+#(1));disp(#); clear #  #obj")))
   (t  (setq vardescr "Error:Unknown type"))      
)
  (scilab-output-to-temp-buffer "*SciVariable*" vardescr)
))


(defcustom scilab-help-separate-buffer nil
"If non-nil Display help on function into separate for each command buffer
with the name '*SciHelp: <functionname>*'. Otherwise general buffer *SciHelp* for all function"
 :group 'scilab
 :type 'boolean
)

(defalias 'scilab-help-function  'scilab-shell-describe-command)
;;;###autoload
(defun scilab-shell-describe-command (command)
  "Describe COMMAND textually by fetching it's doc from the Scilab shell.
This uses the lookfor command to find viable commands.
This command does not require an active Scilab shell."
  (interactive (list (read-from-minibuffer
 		      "Help on SciLab-Function: "
 		      (cons (scilab-read-word-at-point) 0))))
   (if (not (file-exists-p scilab-topics-file-path))
       (scilab-shell-topic-browser))
   (let ((doc (shell-command-to-string (concat 
"export MANCHAPTERS=" scilab-topics-file-path ";"  
"scilab -help " command))))
     (if (equal (substring doc 0 15) "No manual entry")
            (scilab-shell-apropos command)      
(if scilab-help-separate-buffer
    (scilab-output-to-temp-buffer (concat "*SciHelp: " command "*") doc)
    (scilab-output-to-temp-buffer "*SciHelp*" doc)))))

(defalias 'scilab-apropos-function  'scilab-shell-apropos)
;;;###autoload
(defun scilab-shell-apropos (scilabregex)
  "Look for any active commands in SCILAB matching SCILABREGEX.
This uses the apropos  command to find viable commands."
  (interactive (list (read-from-minibuffer
 		      "Scilab command subexpression: "
 		      (cons (scilab-read-word-at-point) 0))))
  (if (not (file-exists-p scilab-topics-file-path))
       (scilab-shell-topic-browser))
  (let ((ap (shell-command-to-string
	     (concat 
	      "export MANCHAPTERS=" scilab-topics-file-path ";"  
	      "scilab -k " scilabregex))))
    
    (scilab-output-to-temp-buffer "*SciApropos*" ap)
;    (scilab-shell-topic-mode)
))
  
;;;(defun scilab-on-prompt-p ()
;;;  "Return t if we Scilab can accept input."
;;;  (save-excursion
;;;    (end-of-line)
;;;    (if (not (eq (point-max) (point))) (kill-line))
;;;    (goto-char (point-max))
;;;    (beginning-of-line)
;;;    (looking-at comint-prompt-regexp)
;;;))

(defun scilab-on-prompt-p ()
  "Return t if we Scilab can accept input."
 (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (looking-at comint-prompt-regexp)))

(defun scilab-make-not-busy ()
  "Sometimes prompt is perturbed by noises. It makes some procedures,
for instance competion buggy. The function regularises the situation"
  (save-excursion
   (goto-char (point-max))
    (beginning-of-line)
    (if (not (looking-at comint-prompt-regexp))
     (comint-send-input)
    )
))



(defun scilab-on-empty-prompt-p ()
  "Return t if we Scilab is on an empty prompt."
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (looking-at (concat comint-prompt-regexp "\\s-*$"))))

(defun scilab-shell-buffer-barf-not-running ()
  "Return a running Scilab buffer iff it is currently active."
  (or (scilab-shell-active-p)
      (error "You need to run the command `scilab-shell' to do that!")))

(defun scilab-shell-test ()
(scilab-shell-collect-command-output "a=1")
(scilab-shell-collect-command-output "plot(1)")
(scilab-shell-collect-command-output "b=1")
)
(defun scilab-shell-lisp2scilab (lispvar scivar)
"Write lisp-variable to a suitable scilab object with name that scivar 
string provides. If there is no corresponding scilab variable, then scilab
variable gets [] "
(interactive)
(setq type-of-lispvar (type-of lispvar))
(cond ((or (eq type-of-lispvar 'float) 
           (eq type-of-lispvar 'integer))
           (scilab-shell-collect-command-output (concat
           scivar "=" (number-to-string lispvar) ";")))
       ((or (eq type-of-lispvar 'string) | (eq type-of-lispvar 'symbol))
           (if (eq type-of-lispvar 'symbol)
             (setq lispvar (symbol-name lispvar)))
;           (string-match "\"" lispvar)
;           (setq lispvar (replace-match "\\\"\"" t t lispvar))
           (scilab-shell-collect-command-output (concat
           scivar "='" lispvar "';")))

)
t
) 

(defun scilab-shell-collect-command-output (command)
  "If there is a Scilab shell, run the Scilab COMMAND and return it's output.
It's output is returned as a string with no face properties.  The text output
of the command is removed from the Scilab buffer so there will be no
indication that it ran."
 (let ((msbn (scilab-shell-buffer-barf-not-running))
	(pos nil)
	(str nil)
	(lastcmd)
)
    (save-excursion
      (set-buffer msbn)
;      (if (not (scilab-on-prompt-p))
;          (scilab-make-not-busy))
      (if (not (scilab-on-prompt-p))
	  (error "Scilab shell must be non-busy to do that"))

      ;; Save the old command
      (goto-char (point-max))
      (beginning-of-line)
      (re-search-forward comint-prompt-regexp)
      (setq lastcmd (buffer-substring (point) (scilab-point-at-eol)))
      (delete-region (point) (scilab-point-at-eol))
      ;; We are done error checking, run the command.
      (setq pos (point))
      (comint-send-string (get-buffer-process (current-buffer))
			  (concat command "\n"))
;      (message "Scilab ... Executing command.")
      (goto-char (point-max))
     ; (message "Scilab reading...")
      (while (or (>= pos (point)) (not (scilab-on-empty-prompt-p)))
	(accept-process-output (get-buffer-process (current-buffer)))
	(goto-char (point-max))
      )
     ; (message "Scilab reading...done")
      (save-excursion
	(goto-char pos)
	(beginning-of-line)
	(setq str (buffer-substring-no-properties (save-excursion
						    (goto-char pos)
						    (beginning-of-line)
						    (forward-line 1)
						    (point))
						  (save-excursion
						    (goto-char (point-max))
						    (beginning-of-line)
						    (point))))
	(delete-region pos (point-max)))
      (insert lastcmd)
  )
;;;    (goto-char currpt)  
     str
))

(defun scilab-shell-send-invisible-string (string)
  "Send STRING to the currently running scilab process."
  (if (not scilab-shell-echoes)
      (let ((proc (get-buffer-process (current-buffer))))
	(goto-char (point-max))
	(insert string)
	(set-marker (process-mark proc) (point))))
  (if (not (string-match "^!.*\n" string))
  (send-invisible string)))

(defun scilab-shell-send-string (string)
  "Send STRING to the currently running scilab process."
  (if (not scilab-shell-echoes)
      (let ((curbuff (current-buffer)) (proc (get-buffer-process (current-buffer))))
	(goto-char (point-max))
	(insert string)
	(set-marker (process-mark proc) (point))))
;    (let ((curbuff (current-buffer)))
      (if (not (string-match "^!.*\n" string))
; ;          (progn 
; ;              (if (string-match "^\\*\\*" string)
; ; 		(save-excursion
; ; 		  (eval (car  (read-from-string (substring string 2))))
; ; 					;		(switch-to-buffer curbuff)
; ; ;		  (setq string (concat "//LISP//" (substring string 2)))
; ; 		  (setq string "\n")
; ; 		  )
; ; 	      )
	    (comint-send-string (get-buffer-process (current-buffer)) string))

	)
; ;     )
; ;    )


(defvar scilab-shell-save-and-go-history '("()")
  "Keep track of parameters passed to the Scilab shell.")
(defun scilab-function-file-p()
"Checks if the given buffer is a scilab function or set of scilab functions."
(interactive)
(let ((p (point)) (p2 nil))
  (goto-char (point-max))
  (setq p2 (re-search-backward scilab-function-head-regexp 1 t))
  (goto-char p)
  (if p2 t nil)))

(defun scilab-which-function()
"Gets the name of the current function"
(interactive) 
(save-excursion 
  (scilab-beginning-of-defun)
  (beginning-of-line)
  (re-search-forward  scilab-function-head-regexp nil t)
  (message (match-string 3))
  )
)

(defun scilab-whereami()
"Gets the name of the current function and the number line"
(interactive)
(message (concat (scilab-which-function) ", Line " (int-to-string (scilab-get-number-line-in-function))))
)
(defun scilab-shell-whereami()
"Display current instruction calling tree"
(interactive)
(scilab-shell-send-string "whereami();\n")
(ring-insert comint-input-ring "whereami();")
)

(defun scilab-get-number-line-in-function()
"Get the line number into a function body"
 (interactive)
 (let ((p (point)))
   (save-excursion 
     (beginning-of-line)
     (if (not (looking-at scilab-function-head-regexp))  
       (re-search-backward scilab-function-head-regexp nil t))
     (beginning-of-line)
     (setq scilab-number-line-in-function (count-lines (point) (+ p 1))))
 )
)



(defun scilab-shell-save-and-getf-or-run ()
  "Save this sci, file, and getf it in a Scilab shell. Recognize if this
file is function or script. If script -just run it"
  (interactive)
  (if (not (eq major-mode 'scilab-mode))
      (error "Save and getf is only useful in a Scilab buffer!"))
  (let ((fn-name (buffer-file-name))
        (short-fn-name (buffer-name))
	(msbn (concat "*" scilab-shell-buffer-name "*"))
	(param "getf "))
    (if (not (scilab-function-file-p))
         (setq param "exec "))
    (save-buffer)
    ;; No buffer?  Make it!
    (if (not (get-buffer msbn)) (scilab-shell))
    ;; Ok, now fun the function in the scilab shell
    (if (get-buffer-window msbn t)
	(select-window (get-buffer-window msbn t))
        (switch-to-buffer-other-window msbn))
    (scilab-shell-send-string (concat
 param fn-name ";\n"))
    (ring-insert comint-input-ring (concat param fn-name ";"))
    (if (eq param "getf ")
       (message "Loading %s..."  short-fn-name)
       (message "Executing %s..."  short-fn-name))
     ))

(defun scilab-shell-cd (dir)
  "Change directory in emacs and in Scilab. before syncronises directories to emacs's directory "
  (interactive   (list (read-file-name "Change current directory: "
			 default-directory default-directory
			 (and (member cd-path '(nil ("./")))
			      (null (getenv "CDPATH"))))))
  (cd dir)
  (let ((absdr (cd-absolute dir)))
    (if (equal (substring absdr 0 1) "~")
        (if (equal (substring absdr 0 2) "~/")
          (setq absdr (concat (getenv "HOME") (substring absdr 1)))
          (setq absdr (concat (getenv "HOME") "/../" (substring absdr 1)))))
    (scilab-shell-collect-command-output (concat "chdir " absdr))
    (pwd)
  )
)


(defun scilab-shell-run-region (beg end)
  "Run region from BEG to END and display result in Scilab shell.
This command requires an active Scilab shell."
  (interactive "r")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((command nil)
        (pos nil)
        (str (concat (buffer-substring-no-properties beg end) "\n"))
 	(msbn (scilab-shell-buffer-barf-not-running))
 	(lastcmd)
       )
    (save-excursion
      (set-buffer msbn)
      (if (not (scilab-on-prompt-p))
 	  (error "Scilab shell must be non-busy to do that"))
      ;; Save the old command
     (goto-char (point-max))
     (beginning-of-line)
      (re-search-forward comint-prompt-regexp)
      (setq lastcmd (buffer-substring (point) (scilab-point-at-eol)))
      (delete-region (point) (scilab-point-at-eol))
     (while (string-match "\n\\([ \t]*\n\\)*" str)
       (setq command (substring str 0 (match-beginning 0)))
       (setq str (substring str (match-end 0)))
       (if (string-match (concat "^[ \t]*" comint-prompt-regexp)  command)
         (setq command (substring command (match-end 0))))
;       (scilab-shell-collect-command-output command)
       (setq pos (point))
       (scilab-shell-send-string (concat command "\n"))
      (goto-char (point-max))
     (while (or (>= pos (point)) (not (scilab-on-empty-prompt-p)))
	(accept-process-output (get-buffer-process (current-buffer)))
	(goto-char (point-max))
       )
      )
;      (scilab-shell-dummy-action)
     (insert lastcmd)
   )
    (set-buffer msbn)
    (goto-char (point-max))
    (display-buffer msbn)
    ))
 
(defun scilab-shell-last-error ()
  "In     Scilab interactive buffer, find the last Scilab error, and go there.
To reference old errors, put the cursor just after the error text."
  (interactive)
   (let (eb el)
     (save-excursion
      (end-of-line) ;; In case we are before the linenumber 1998/06/05 16:54sk
      (if (not (re-search-backward gud-scilab-error-regexp nil t))
	  (error "No errors found!"))
       (setq eb (buffer-substring-no-properties
 		(match-beginning 4) (match-end 4))
 	    el (buffer-substring-no-properties
 		(match-beginning 3) (match-end 3))))
     (scilab-find-file-on-path eb)
     (scilab-function-goto-line (string-to-int el))
 ))

(defun scilab-shell-after-pause ()
  "In     Scilab interactive buffer, on the line
\"foo called at line ### of foo2\" go to foo2 at line ###" 
  (interactive)
     (save-excursion
      (beginning-of-line) 
      (if (not 
          (or (looking-at gud-scilab-pause-regexp) 
              (looking-at gud-scilab-pause-regexp2)
              (looking-at gud-scilab-pause-regexp3)))
	  (error "This line does not seem to be the line after a pause")
       (let ((eb (buffer-substring-no-properties
 		(match-beginning 3) (match-end 3)))
 	    (el (buffer-substring-no-properties
 		(match-beginning 2) (match-end 2))))
     (scilab-find-file-on-path eb)
     (scilab-function-goto-line (string-to-int el)))))
 )
;re-search-backward gud-scilab-error-regexp nil t))

; ; ; ; (defun scilab-shell-dbstop-error ()
; ; ; ;   "Stop on errors."
; ; ; ;   (interactive)
; ; ; ;   (comint-send-string (get-buffer-process (current-buffer))
; ; ; ; 		      "dbstop if error\n"))

; ; ; ; (defun scilab-shell-dbclear-error ()
; ; ; ;   "Don't stop on errors."
; ; ; ;   (interactive)
; ; ; ;   (comint-send-string (get-buffer-process (current-buffer))
; ; ; ; 		      "dbclear if error\n"))

(defun scilab-shell-demos ()
  "Scilab demos."
  (interactive)
;  (scilab-shell-collect-command-output "demos();"))
  (comint-send-string (get-buffer-process (current-buffer)) "demos();\n")
  (ring-insert comint-input-ring "demos();")
)



(defvar scilab-shell-number-graphic-window 0
"Number of the active current graphic window")
(defvar scilab-shell-graphic-menu-title "Graphic 0"
"Graphic menu title")
 
; (defun  scilab-shell-graphic-menu-redefine () 
; "Redefine title of the graphic menu in accordance with the 
; value of `scilab-shell-number-graphic-window'"
; (interactive)
; ;;(easy-menu-remove-item nil nil "Graphic")
; (setq open (concat "Open Window " 
;      (int-to-string  scilab-shell-number-graphic-window)))
; (setq close (concat "Close Window " 
;      (int-to-string  scilab-shell-number-graphic-window)))
; (easy-menu-define
;    scilab-shell-graphic-menu
;    scilab-shell-mode-map
;    "Scilab shell graphic windows  menu"
;    '("Graphic"
;      [open scilab-shell-open-graphic-window 
;              :included(scilab-shell-active-p) 
;              :active(scilab-shell-active-p) ]
;      ["Next " (scilab-shell-plus-graphic-window)
;              :included(scilab-shell-active-p) 
;              :active(scilab-shell-active-p) ]
;      ["Previos" (scilab-shell-minus-graphic-window)
;              :included(scilab-shell-active-p) 
;              :active(scilab-shell-active-p) ]
;      [close scilab-shell-close-current-figure 
;              :included(scilab-shell-active-p)
;              :active(scilab-shell-active-p)]
;      ["Close All" scilab-shell-close-figures 
;              :included(scilab-shell-active-p)
;              :active(scilab-shell-active-p)]
;   ))
; ;;(easy-menu-add scilab-shell-graphic-menu scilab-shell-mode-map)
; )



(defun scilab-shell-plus-graphic-window ()
  (setq scilab-shell-number-graphic-window 
        (+ scilab-shell-number-graphic-window 1)
  )
  (scilab-shell-frame-init)
)

(defun scilab-shell-minus-graphic-window ()
  (setq scilab-shell-number-graphic-window 
    (max (- scilab-shell-number-graphic-window 1) 0)
  )
  (scilab-shell-frame-init)
)

(defun scilab-shell-next-graphic-window ()
  (scilab-shell-plus-graphic-window)
  (scilab-shell-open-graphic-window)
)

(defun scilab-shell-previous-graphic-window ()
  (scilab-shell-minus-graphic-window)
  (scilab-shell-open-graphic-window)
)


(defun scilab-shell-open-graphic-window ()
  "Open scilab graphic window (if it does not exists)."
  (interactive)
;  (comint-send-string (get-buffer-process (current-buffer)) "xget('window');\n")
  (scilab-shell-collect-command-output (concat "xset('window'," (int-to-string scilab-shell-number-graphic-window) ");"))
)

(defun scilab-shell-close-current-figure ()
  "Close current figure."
  (interactive)
;  (comint-send-string (get-buffer-process (current-buffer)) "xdel()\n"))
  (scilab-shell-collect-command-output (concat "xdel(" (int-to-string scilab-shell-number-graphic-window) ");"))
  (setq scilab-shell-number-graphic-window 0)
  (scilab-shell-frame-init)
)

(defun scilab-shell-close-figures ()
  "Close any open figures."
  (interactive)
;;;  (comint-send-string (get-buffer-process (current-buffer)) "close all\n"))
  (scilab-shell-collect-command-output "while winsid()~=[], xdel(); end")
;  (comint-send-string (get-buffer-process (current-buffer)) "while winsid()~=[], xdel(); end\n"))
  (setq scilab-shell-number-graphic-window 0)
  (scilab-shell-frame-init)
)


(defun scilab-shell-exit ()
  "Exit Scilab shell."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "exit\n"))


(defun scilab-shell-remote-exit ()
  "Exit Scilab shell."
  (interactive)
  (switch-to-buffer (concat "*" scilab-shell-buffer-name "*"))
  (comint-send-string (get-buffer-process (current-buffer)) "exit\n"))


(defun scilab-shell-soft-restart ()
  "Restart Scilab shell."
  (interactive)
  (switch-to-buffer (concat "*" scilab-shell-buffer-name "*"))
  (comint-send-string (get-buffer-process (current-buffer)) "abort;\n exec('SCI/scilab.star',-1);\n"))

(defun scilab-shell-saferestart ()
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "abort;\n save('/tmp/'+date()+'.scilab')\n; exit\n")
  (switch-to-buffer (concat "*" scilab-shell-buffer-name "*"))
  (scilab-shell))

(defun scilab-shell-restart ()
  (interactive)
  (if (scilab-shell-active-p)
    (scilab-shell-remote-exit)
  )
  (while (scilab-shell-active-p)
	(accept-process-output (get-buffer-process (current-buffer)))
)
;  (scilab-run-with-timer 0.1 nil 'scilab-shell)
  (scilab-shell)
)



;;; scilab-shell based Topic Browser and Help =================================

(defcustom scilab-shell-topic-mode-hook nil
  "*Scilab shell topic hook."
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'hook)

(defvar scilab-shell-topic-current-topic nil
  "The currently viewed topic in a Scilab shell topic buffer.")

(defalias  'scilab-help 'scilab-shell-topic-browser)
;;;###autoload
(defun scilab-shell-topic-browser ()
  "Create a topic browser by querying an active Scilab shell using HELP.
Maintain state in our topic browser buffer."
  (interactive)
  ;; Reset topic browser if it doesn't exist.
  (if (not (get-buffer "*Scilab Topic*"))
      (setq scilab-shell-topic-current-topic nil))
  (let ((b (get-buffer-create "*Scilab Topic*")))
    (switch-to-buffer-other-window b)
    (if (string= scilab-shell-topic-current-topic "")
	nil
      (scilab-shell-topic-mode)
      (scilab-shell-topic-browser-create-contents "")
)))

(defvar scilab-shell-topic-mouse-face-keywords
  '(;; These are subtopic fields...
   ("^\\(\\w+/\\w+\\)[ \t]+-" 1 scilab-constant-face)
 ;; These are functions...
   ("^[ \t]*\\([A-Za-z$#_%][A-Za-z0-9$#_]*\\)[ \t]+-" 1 scilab-function-name-face) 
   ("^[ \t]*\\([A-Za-z$#_%][A-Za-z0-9$#_]*[ \t]+(.*)\\)[ \t]+-" 1 scilab-function-name-face) 
    ;; Here is a See Also line...
   ("[ \t]+See also "
   ("[ \t]*\\(\\w+\\)\\([,.]\\| and\\|$\\) *" nil nil (1 scilab-constant-face)))
   ("[ \t]*SEE ALSO[ \t]*\n[ \t]*"
   ("\\(\\w+\\)\\([,.]\\| and\\|$\\)*" nil nil (1 scilab-function-name-face)))
)
  "These are keywords we also want to put mouse-faces on.")

(defvar scilab-shell-topic-font-lock-keywords
  (append scilab-shell-topic-mouse-face-keywords
	  '(("@\\(.*\\)" 1 scilab-keyword-face)
	    ;; These are subheadings...
;	    ("^[ \t]+\\([^.\n]+[a-zA-Z.]\\)$" 1 'underline)
;	    ("\\<[/~$]\\(\\(\\w\\|[$_.-]\\)+/\\(\\w\\|[$_.-]\\)*\\)+\\s-" 0 scilab-constant-face)
	    ("\\([\'\" ]?[~/.$]/*[a-zA-Z0-9_./%$-]*\\)" 0 scilab-constant-face)
	    ("[\'\" ]?[~/.$]/*[a-zA-Z0-9_./%$-]*\\s-+\\(.*\\)" 1 scilab-keyword-face)
	    )
)
  "Keywords useful for highlighting a Scilab TOPIC buffer.")

(defvar scilab-shell-help-font-lock-keywords
  (append scilab-shell-topic-mouse-face-keywords
          scilab-shell-font-lock-keywords-2
 	  '(
; 	    ("[ \t]*\\(\\[[A-Za-Z]+\\]\\)\\s-*=\\s-*\\([A-Za-z]+[0-9]*\\)("
; 	     (1 scilab-variable-name-face)
; 	     (2 scilab-function-name-face))
;           ;; Variables into [...]
;             ("\\[\\([^][)(]*\\)\\]"
; 	      (1 scilab-variable-name-face))
;           ;; Variables
;             ("[A-Za-z$_%][A-Za-z0-9$_]*(\\([^()]*\\))"
; 	      1 scilab-variable-name-face)
; 	    ;; Reference uppercase words
 	    ("\\<\\([A-Z][A-Z]+\\)\\>[^(]" 1 scilab-constant-face)
; 	    ("\\([A-Za-z$_%][A-Za-z0-9$_]*\\)("
; ;	     1 'bold)
;              1 scilab-function-name-face)
; ;	     1 font-lock-doc-string-face)
;            ;;strings
; ;            ((concat scilab-string-start-regexp
; ;	   "\\(['\"]" scilab-string-end-regexp "\\)"
; ;	   "\\([^'\"]\\|$\\)") 1 scilab-string-face)
; ;            ;;comments
; ;            ("\\(//.*\\)" 1 scilab-comment-face)
	     )
)

  "Keywords for regular help buffers.")

;; View-major-mode is an emacs20 thing.  This gives us a small compatibility
;; layer.
(if (not (fboundp 'view-major-mode)) (defalias 'view-major-mode 'view-mode))

;;;###autoload
(defun scilab-unboldunderline-region (start end)
  "Remove all boldness  (overstruck characters) and  underlining
 (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on. This is comination of unbold-region
and ununderline-region functions"
  (interactive "r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (re-search-forward "\b" end1 t)
	(if (eq (following-char) (char-after (- (point) 2)))
	    (delete-char -2)))
      (goto-char (min start end))
      (while (re-search-forward "_\b\\|\b_" end1 t)
       (delete-char -2)))))


(define-derived-mode scilab-shell-help-mode
  view-major-mode "Sci Help"
  "Major mode for viewing Scilab help text.
Entry to this mode runs the normal hook `scilab-shell-help-mode-hook'.

Commands:
\\{scilab-shell-help-mode-map}"
  (scilab-unboldunderline-region (point-min) (point-max))
  (make-local-variable 'font-lock-defaults)
;   (setq font-lock-defaults '((scilab-font-lock-keywords
;			      scilab-gaudy-font-lock-keywords
;			      scilab-really-gaudy-font-lock-keywords
;			      )
;			     t ; do not do string/comment highlighting
;			     nil ; keywords are case sensitive.
;			     ;; This puts _ as a word constituent,
;			     ;; simplifying our keywords significantly
;			     ((?_ . "w"))))
 (setq font-lock-defaults '((scilab-shell-help-font-lock-keywords)
			     t nil ((?_ . "w"))))
  ;; This makes sure that we really enter font lock since
  ;; kill-all-local-variables is not used by old view-mode.
  (easy-menu-add scilab-shell-help-mode-menu scilab-shell-help-mode-map)
  (scilab-shell-topic-mouse-highlight-subtopics)
   (if (and (boundp 'font-lock-mode) (fboundp 'font-lock-mode))
;    (progn 
;      (font-lock-mode -1)  
      (if scilab-font-lock-mode (font-lock-mode 1) (font-lock-mode -1))
;    )
   )
  )

(define-key scilab-shell-help-mode-map [return] 'scilab-shell-topic-choose)
(define-key scilab-shell-help-mode-map "t" 'scilab-shell-topic-browser)
(define-key scilab-shell-help-mode-map "q" 'bury-buffer)
(define-key scilab-shell-help-mode-map [(control h) return] scilab-help-map)
(define-key scilab-shell-help-mode-map "\C-cr" 'scilab-shell-run-region)


(if running-xemacs
    (define-key scilab-shell-help-mode-map [button2] 'scilab-shell-topic-click)
  (define-key scilab-shell-help-mode-map [mouse-2] 'scilab-shell-topic-click))

(easy-menu-define
 scilab-shell-help-mode-menu scilab-shell-help-mode-map
 "Scilab shell topic menu"
 '("Scilab Help"
   ["Describe This Function" scilab-shell-topic-choose t]
   "----"
   ["Describe Function" scilab-shell-describe-command t]
   ["Describe Variable" scilab-shell-describe-variable t]
   ["Command Apropos" scilab-shell-apropos t]
   ["Topic Browser" scilab-shell-topic-browser t]
   ["Separate help buff" (custom-set-variables
      '(scilab-help-separate-buffer (not scilab-help-separate-buffer)))
       :style toggle :selected scilab-help-separate-buffer]
   "----"
   ["Run Region" scilab-shell-run-region (scilab-shell-active-p) ]
   ["Exit Help" bury-buffer t]))



(define-derived-mode scilab-shell-topic-mode
  scilab-shell-help-mode "Sci-Topic"
  "Major mode for browsing Scilab HELP topics.
The output of the Scilab command HELP with no parameters creates a listing
of known help topics at a given installation.  This mode parses that listing
and allows selecting a topic and getting more help for it.
Entry to this mode runs the normal hook `scilab-shell-topic-mode-hook'.

Commands:
\\{scilab-shell-topic-mode-map}"
  (setq font-lock-defaults '((scilab-shell-topic-font-lock-keywords)
			     t nil ((?_ . "w"))))
;  (if running-xemacs
;      (setq mode-motion-hook 'scilab-shell-topic-highlight-line)
;)

  (easy-menu-add scilab-shell-topic-mode-menu scilab-shell-topic-mode-map)
  (if (and (boundp 'font-lock-mode) (fboundp 'font-lock-mode))
      (progn
       (font-lock-mode -1)
       (if scilab-font-lock-mode (font-lock-mode 1)))
    )
  )


(easy-menu-define
 scilab-shell-topic-mode-menu scilab-shell-topic-mode-map
 "Scilab shell topic menu"
 '("Scilab Topic"
   ["Select This Topic" scilab-shell-topic-choose t]
   ["Top Level Topics" scilab-shell-topic-browser t]
   "----"
   ["Exit" bury-buffer t]))



(defun scilab-shell-topic-browser-create-contents (subtopic)
  "Fill in a topic browser with the output from SUBTOPIC."
  (toggle-read-only -1)
   (switch-to-buffer "*Scilab Topic*")
  (erase-buffer)
  (let (
        (standardname 
          (concat scilab-shell-main-directory  "/man/Chapters"))
        (chaptername scilab-topics-file-path)
        str)
  (if (string= chaptername standardname)
;    (if (file-exists-p (concat scilab-shell-initial-directory "/man/"))
 ;   (setq chaptername (concat scilab-shell-initial-directory "/man/Chapters"))
    (setq chaptername (concat scilab-shell-initial-directory "/manChapters")))
  (if (and (scilab-shell-active-p) (scilab-on-empty-prompt-p))
         (scilab-shell-collect-command-output (concat
           "#=max(length(%helps(:,1)))-length(%helps(:,1));"
           "##=file('open','" chaptername "' ,'unknown');"
           "###=%helps(:,1);"
           "for ?=1:size(###,'*');"
           "??=' '; ??=strcat(??(ones(1,#(?)+2)));"
           "###(?)=###(?)+??+strsubst(%helps(?,2),'/','|');"
           "end;"
           "write(##,###,'(a)');"
           "file('close',##);"
           "clear # ## ### ? ??;"))
  (if (and (not (file-exists-p chaptername)) (file-exists-p standardname))
      (progn
        (copy-file standardname chaptername)
        (message "copy %s to %s" standardname chaptername))
  (if (not (file-exists-p chaptername))
    (error"No manual Chapters file. Run scilab-shell, or build manullay")
  ))
)
  (if (equal subtopic "")
      (insert-file chaptername)
      (setq str (substring 
                   (shell-command-to-string 
                      (concat  "echo " subtopic  "/whatis")) 0 -1))
      (insert-file str)

)

  (goto-char (point-min))
;;  (forward-line 1)
;;  (delete-region (point-min) (point))
  (setq scilab-shell-topic-current-topic subtopic)
 (if (not running-xemacs)
      (scilab-shell-topic-mouse-highlight-subtopics) 
)
  (toggle-read-only 1)
  )
)

(defun scilab-shell-topic-click (e)
  "Click on an item in a Scilab topic buffer we want more information on.
Must be bound to event E."
  (interactive "e")
  (mouse-set-point e)
  (scilab-shell-topic-choose))
(defun scilab-shell-topic-choose ()
  "Choose the topic to expand on that is under the cursor.
This can fill the topic buffer with new information.  If the topic is a
command, use `scilab-shell-describe-command' instead of changing the topic
buffer."
  (interactive)
  (let ((topic nil) (fun nil) (p (point)) (locp nil))
    (save-excursion
     (beginning-of-line)
;       (if (looking-at "^\\w+/\\(\\S-+\\)[ \t]+")  
        (if (looking-at (concat "\\(" scilab-path-type-regexp "\\)[ \t]+"))
	  (setq topic (match-string 1))
          (goto-char p)
          (setq locp (re-search-backward scilab-not-variable-symbol 1 t))
          (if locp
          (goto-char (+ 1 locp))
          (goto-char 1))
         (if (looking-at "\\(%?[a-zA-Z0-9_]+\\)\\W")
	    (setq fun (match-string 1))
	    (error "You did not click on a subtopic, function or reference"))))
    (message "Opening item %s..." (or topic fun))
    (if topic
	(scilab-shell-topic-browser-create-contents topic)
      (scilab-shell-describe-command fun))
    ))

(defun scilab-shell-topic-mouse-highlight-subtopics ()
  "Put a `mouse-face' on all clickable targets in this buffer."
  (save-excursion
    (let ((el scilab-shell-topic-mouse-face-keywords))
      (while el
	(goto-char (point-min))
	(while (re-search-forward (car (car el)) nil t)
	  (let ((cd (car (cdr (car el)))))
	    (if (numberp cd)
		(put-text-property (match-beginning cd) (match-end cd)
				   'mouse-face 'highlight)
	      (while (re-search-forward (car cd) nil t)
		(put-text-property (match-beginning (car (nth 3 cd)))
				   (match-end (car (nth 3 cd)))
				   'mouse-face 'highlight)))))
	(setq el (cdr el))))))

(defun scilab-shell-topic-highlight-line (event)
  "A value of `mode-motion-hook' which will highlight topics under the mouse.
EVENT is the user mouse event."
  (let* ((buffer (event-buffer event))
	 (point (and buffer (event-point event))))
    (if (and buffer (not (eq buffer mouse-grabbed-buffer)))
	(save-excursion
	  (save-window-excursion
	    (set-buffer buffer)
	    (mode-motion-ensure-extent-ok event)
	    (if (not point)
		(detach-extent mode-motion-extent)
	      (goto-char point)
	      (end-of-line)
	      (setq point (point))
	      (beginning-of-line)
	      (if (or (looking-at "^\\w+/\\(\\w+\\)[ \t]+-")
		      (looking-at "^[ \t]+\\(\\(\\w\\|_\\)+\\)[ \t]+-"))
		  (set-extent-endpoints mode-motion-extent (point) point)
		(detach-extent mode-motion-extent))))))))


;;; Sci File path stuff ======================================================
(defcustom scilab-mode-install-path 
 (list 
  (if (getenv "SCIHOME") 
      (if (file-exists-p (concat (getenv "SCIHOME") "/macros"))
          (concat (getenv "SCIHOME") "/macros")
	(getenv "SCIHOME"))   
    (getenv "HOME"))
  (concat (getenv "SCI") "/macros"))

  "Base path pointing to the locations of all the sci files used by scilab.
All directories under each element of `scilab-mode-install-path' are
checked, so only top level toolbox directories need be added.The first default
directory is the user's  scilab path, defined usually in $SCIHOME, the second
is the main scilab-path, and the rest is 
what a user wants."
  :group 'scilab
  :group 'scilab-setup
  :type '(repeat (string :tag "Path: ")))

(defun scilab-find-file-under-path (path filename)
  "Return the pathname or nil of FILENAME under PATH."
  (if (file-exists-p (concat path filename))
      (concat path filename)
    (let ((dirs (if (file-directory-p path)
		    ;; Not checking as a directory first fails on XEmacs
		    ;; Stelios Kyriacou <kyriacou@cbmv.jhu.edu>
		    (directory-files path t nil t)))
	  (found nil))
      (while (and dirs (not found))
	(if (and (car (file-attributes (car dirs)))
		 ;; don't redo our path names
		 (not (string-match "/\\.\\.?$" (car dirs)))
		 ;; don't find files in object directories.
		 (not (string-match "@" (car dirs))))
	    (setq found
		  (scilab-find-file-under-path (concat (car dirs) "/")
					       filename)))
	(setq dirs (cdr dirs)))
      found)))

(defun scilab-find-function-over-buffers(funname)
  "Find FUNNAAME over all buffers"
(setq currbf (current-buffer))
(setq lst (buffer-list))
(setq success nil)
(setq flag t)
(while  (and  (> (length lst) 0) (not success))
   (setq bfn (buffer-name (car lst)))
   (if (string-match "\\.sc[ie]$" bfn)
   (progn 
      (set-buffer (car lst))
    (if (equal  (buffer-name (current-buffer)) funname)
    (setq success t)
    (goto-char (point-max))
    (setq success (re-search-backward (concat 
        "^\\s-*\\(function\\)\\>[ \t\n.]*"
           scilab-output-function-regexp "?[ \t\n.]*"
           funname "\\s-*(")  nil t))
    (if success (goto-char success)
      )))) 
   (setq lst (cdr lst)))
   (if success (switch-to-buffer-other-window  bfn) nil)
)

(defun scilab-find-file-on-path (filename)
  "Find FILENAME on the current Scilab path.
The Scilab path is determined by `scilab-mode-install-path' and the
current directory.  You must add user-installed paths into
`scilab-mode-install-path' if you would like to have them included."
  (interactive
   (list
    (let ((default (save-excursion
		     (if (or (bolp)
			     (looking-at "\\s-")
			     (save-excursion (forward-char -1)
					     (looking-at "\\s-")))
			 nil
		       (forward-word -1))
		     (scilab-navigation-syntax
		       (if (looking-at "\\sw+\\>")
			   (match-string 0))))))
      (if default
	  (let ((s (read-string (concat "File (default " default "): "))))
	    (if (string= s "") default s))
	(read-string "File: ")))))
  (if (string= filename "")
      (error "You must specify an sci file"))
  (if (scilab-find-function-over-buffers filename)
      nil
  (if (scilab-find-library-function  filename)
      nil
  (let ((fname nil) (fnameb nil)  
	(dirs scilab-mode-install-path)
        (filenamebin nil) (filenamesci filename))  
        (if (not (string-match "\\.sc[ie]$" filename))
           (setq filenamebin  (concat filename ".bin") 
                filenamesci  (concat filename ".sci")))
;; here we add the possibility for multifunction files. 
;;File *.sci may not exis
;; But if *.bin exists  it may be link on the true *.sci file
        (if (file-exists-p (concat default-directory filenamesci))
	  (setq fname (concat default-directory filenamesci))
          (if filenamebin
            (if (file-exists-p (concat default-directory filenamebin))
	      (setq fnameb (file-truename 
                             (concat default-directory filenamebin)) 
                    fnameb (concat 
                             (substring 
                                fnameb 0 (- (length fnameb) 3)) "sci")))))
        (while (and (not fname) (not fnameb) dirs)
          (if (stringp (car dirs))
	    (if filenamebin 
	      (progn
		(message "Searching for %s or %s in %s" 
                     filenamesci filenamebin (car dirs))
		(setq fname (scilab-find-file-under-path 
                                (car dirs) filenamesci) 
		      fnameb (scilab-find-file-under-path 
                                (car dirs) filenamebin)))
              (progn 
	        (message "Searching for %s  in %s" filenamesci  (car dirs))
	        (setq fname (scilab-find-file-under-path 
                            (car dirs) filenamesci)))))
         (setq dirs (cdr dirs)))
         (if fname (find-file-other-window fname)
           (if (not fnameb)
	     (error "File or function %s not found on any known paths.  \
Check `scilab-mode-install-path'" filename)
	     (setq fnameb (file-truename fnameb) 
	           fname (concat 
                         (substring fnameb 0 (- (length fnameb) 3)) "sci"))
	     (find-file-other-window  fname)))
         (if (equal (substring filenamesci -4) ".sce")
            (message "File %s is found." filenamesci)
            (scilab-find-function-in-file filename))
    ))))

(defun scilab-find-function-in-file (funname)
"Find function in the current file"
        (goto-char (point-max))
        (setq fun (re-search-backward (concat 
          "^\\s-*\\(function\\)\\>[ \t\n.]*"
            scilab-output-function-regexp "?[ \t\n.]*"
            funname "*[(\n]")  nil t))
        (if (not fun) 
          (message "Strange: there is no function %s in the chosen file.." 
           funname)
          (goto-char fun) 
          (message "Function %s is found." funname)
        )
)

(defun scilab-find-library-function (funname)
;"Find library scilab function. Uses whereis scilab function"
  (interactive)
   (if (string-match "\\.sci$" funname)
      (setq funname (substring funname 0 -4))
   )
    
    (if (or (not (scilab-shell-active-p))
            (string-match "[\"\'()\n\t -]" funname))
        nil
    (setq pth (scilab-shell-collect-command-output 
	       (concat 
		"#=string(evstr(whereis('" funname "')));"
		"#=#(1);write(%io(2),#);clear #;"
                )
               )
	  )
    (setq pth (substring pth 0 -2) )
    (if (string-match "^\\(SCI\\)" pth)
        (setq pth (replace-match scilab-shell-main-directory t nil pth)))
    (if (equal pth "") nil
      (setq pth (substring pth 0 -1))
      (setq pthf (concat pth funname ".sci"))
      (if (file-exists-p pthf) (find-file-other-window pthf)
	(setq pthf (file-truename (concat pth funname ".bin")))
	(if (not pthf) nil 
	  (find-file-other-window (concat (substring pthf 0 -3) "sci"))
	  (scilab-find-function-in-file funname)
	  )
	)
      )
    )
)

  


(defun scilab-find-file-click (e)
  "Find the file clicked on with event E on the current path."
  (interactive "e")
  (mouse-set-point e)
  (let ((f (save-excursion
	     (if (or (bolp)
		     (looking-at "\\s-")
		     (save-excursion (forward-char -1)
				     (looking-at "\\s-")))
		 nil
	       (forward-word -1))
	     (scilab-navigation-syntax
	       (if (looking-at "\\sw+\\>")
		   (match-string 0))))))
    (if (not f) (error "To find an sci file, click on a word"))
    (scilab-find-file-on-path f)))


;;; scilab-mode debugging =====================================================

(defun scilab-show-line-info ()
  "Display type and attributes of current line.  Used in debugging."
  (interactive)
  (let ((msg "line-info:")
	(indent (scilab-calculate-indentation (current-indentation)))
	(nexti (scilab-next-line-indentation)))
    (setq msg (concat msg
		      " Line type: " (symbol-name (car indent))
		      " This Line: " (int-to-string (nth 1 indent))
		      " Next Line: " (int-to-string nexti)))
    (if (scilab-lattr-cont)
	(setq msg (concat msg " w/cont")))
    (if (scilab-lattr-comm)
	(setq msg (concat msg " w/comm")))
    (message msg)))

(defvar scilab-ptyp t
  "Non-nil if communications via pty; false if by pipe.  Buffer local.
This is to work around a bug in Emacs process signaling.")

(defun scilab-interrupt-subjob ()
  "Interrupt the current subjob.
This command also kills the pending input
between the process-mark and point."
  (interactive)
  (comint-kill-input)
  (interrupt-process nil t))

(defun scilab-kill-subjob ()
  "Send kill signal to the current subjob.
This command also kills the pending input
between the process-mark and point."
  (interactive)
  (comint-kill-input)
  (kill-process nil scilab-ptyp))

(defun scilab-quit-subjob ()
  "Send quit signal to the current subjob.
This command also kills the pending input
between the process-mark and point."
  (interactive)
  (comint-kill-input)
  (quit-process nil scilab-ptyp))

(defun scilab-abort-subjob ()
  "Stop the current subjob.
This command also kills the pending input
between the process-mark and point.

WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer. If you accidentally do
this, use \\[scilab-continue-subjob] to resume the process. (This
is not a problem with most shells, since they ignore this signal.)"
  (interactive)
  (comint-kill-input)
  (interrupt-process nil t)
  (comint-send-string (get-buffer-process (current-buffer)) "\n")
;  (scilab-shell-collect-command-output "%tmp=mode();mode(-1)")
 ; (scilab-shell-collect-command-output "error('The command is aborted')\n")
  (comint-send-string (get-buffer-process (current-buffer)) "abort\n")
  ;(comint-send-string (get-buffer-process (current-buffer)) doc)
 ; (scilab-shell-collect-command-output "mode(%tmp);clear %tmp;")) 
)


(defun scilab-continue-subjob ()
  "Send CONT signal to process buffer's process group.
Useful if you accidentally suspend the top-level process."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "resume\n"))

(provide 'scilab)


;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF THE BODY ;;;;;;;;;;;;;;;;;;;;



;;; Change log
;;;date: 2001/03/06 12:28:22;  author: sasha;  state: Exp;
;;;wh function does not depend on scilab version now
;;;libfunc file instead of AllNames
;;; 
;;;date: 2001/03/07; author Alexander Vigodner 
;;;Due to Bruno's request:
;;; 1) Menues are changed. Navigator and Insert menues now are main
;;; 2) Navigators functions are added
;;; 3) Part of menues is removed as more related to matlab  
;;; 4) Uncomment function is added
;;; Besides completion is fixed for scilab-mode. Working during editing now
;;; scilab.el ends here
;;;date: 2001/03/08; author Alexander Vigodner 
;;;function scilab-completion-symbol is fixed. imenu index  from Navigate.
;;;date: 2001/03/11; author Alexander Vigodner 
;;;  1)Completion now works (with less features)  when shell is not active 
;;;  2) Topic Browser is always well defined
;;;  3) Which func mode is added
;;;  4) New customization features.
;;;  5) scilab-shell-after-pause ("C-x \") is added
;;;date: 2001/03/20; author Alexander Vigodner 
;;;  1) Graphic navigation functions and menu are added
;;;  2) wh function is fixed (long string problem)
;;   3)Version 1.8.6     
;;;date: 2001/03/22; author Alexander Vigodner 
;;;  1) scilb-shell-run-region - better version C-c r everywhere now
;;   2)Version 1.8.6.1     
;;;date: 2001/03/25; author Alexander Vigodner 
;;;  1) scilab-after-pause-regexp3 is added 
;;   2)scilab-dynamical-indent variable is added. In big files indentaion is too slow.Now
;;   we can dynamically enable/disable this
;;   3)Version 1.8.6.2     
;;;date: 2001/04/01; author Alexander Vigodner 
;;;  1) customized menus are extended in scilab-mode
;;   2) scilab-mode-setup is added
;;   3) Extended initial lines to .emacs and this is added at the beginning of 
;;   file this 
;;   3)Version 1.8.6.6     

;;;date: 2001/04/03; author Alexander Vigodner 
;;;  1) next-line-add-newlines variable is alweays nil in shell now
;;   2) Grammar error emacslient-> emacsclient 
;;   2)Version 1.8.6.7    

;;;date: 2001/04/04; author Alexander Vigodner 
;;;  1) scilab-shell-logo for xemacs
;;   2)scilab-shell-additional is added: additional scilab shells (with less features)
;;   3)scilab-shell-active-in-current-buffer-p is added
;;   4) Customization faces is more friendly
;;   5)Version 1.8.6.8    
;;;date: 2001/04/10; author Alexander Vigodner 
;;   1)customized faces in menus
;;   2) New customized group scilab-face 
;;   2)output and input variables in function definition has the same colors without
;;     brackets.
;;   3) Version 1.8.7      

;;;date: 2001/04/15; author Alexander Vigodner 
;;   1)Fixing customized faces for xemacs
;;   2)%version is updated smoothly now. 
;;   3)Restart function is fixed
;;   4) New group scilab-edit is built. Some editing options are moved there
;;   5) Version 1.8.9      

;;;date: 2001/04/15; author Alexander Vigodner 
;;   1)Add face to tlist fields
;;   2) Version 1.9.0      

;;;date: 2001/04/22; author Alexander Vigodner 
;;   1)scilab-find-function-in-file is fixed
;;   2) Version 1.9.1   

;;;date: 2001/05/02; author Alexander Vigodner 
;;   1)Save Getf menu is modified, to show that it is disable when shell
;;    is passive
;;   2) Version 1.9.2   

;;;date: 2001/05/08; author Alexander Vigodner 
;;   1)scilab-shell-cd instead of cd on startup
;;   2) %version is shown on the startup
;;   3) Save Getf menu is fixed to be changed in sce/sci file  
;;   4)Version 1.9.3

;;;date: 2001/05/14; author Alexander Vigodner 
;;   1)$MACHAPTERS now is assigned to shell locally. (We do not have to define it in sh;;ell)
;;;  2)Version 1.9.4

;;;date: 2001/05/15; author Alexander Vigodner 
;;   1)scilb-mode-install-path is added to scilab-setup group and moved from scilab-she;;  ll group to general scilab group
;;;  2)Version 1.9.5

;;;date: 2001/05/27; author Alexander Vigodner 
;;   1)scilab-shell-version is fixed for the case whith no emacsclient
;;;  2) topci-browser is build if MANCHAPTER file does not exist for the first
;;;  call of any help function
;;;  2)Version 1.9.6

;;;date: 2001/05/29; author Alexander Vigodner 
;;   1)$SCIHOME/libfunc file now is removed on startup. Useful when libraries are updated
;;;  2)Version 1.9.7

;;;date: 2001/05/30; author Alexander Vigodner 
;;   1)getf,exec,demos, whereami  are added to command-history
;;   2)Fudge max is added to edit options in scilab-edit mode
;;;  2)Version 1.9.8

;;;date: 2001/05/31; author Alexander Vigodner 
;;   1)(require 'cus-face is added for compatability for xemacs.
;;   2) scilab-startup.el file is built and inserted commented at the beginning
;;   3) Installation instructions are changed (simplified)
;;   4) global-key value is acceptable from scilab.el via autoload
;;;  5)Version 1.9.9

;;;date: 2001/06/02; author Alexander Vigodner 
;;   1) global-key value is added to startup file and removed from scilab.el
;;;  2)Version 2.0.0

;;;date: 2001/06/11; author Alexander Vigodner 
;;   1) hidlighting of function header without output argument is fixed
;;;  2) scilb-font-lock-mode is now customizedd variable. t means font-lock
;;;     for all scilab buffers. nill - no font-lock for all scilab buffers.
;;;     can be overrided by global-font-lock-mode for emacs for some buffers.
;;;  2)Version 2.0.1

;;;date: 2001/06/12; author Alexander Vigodner 
;;   1) hidlighting of function header - pathalogy cases
;;;  2)Version 2.0.2

;;;date: 2001/06/14; author Alexander Vigodner 
;;   1) Default values are more flexible for command-scilab and forinstall-paths
;;   2) scilab-fill- stuffs are fixed, seems much better now thanks to 
;;   Lydia van Dijk . 
;;;  3)Version 2.0.3

;;;date: 2001/06/17; author Alexander Vigodner 
;;   1) comment-start-skip local variable is changed to fix indent-for-comment behavior
;;   thanks to Lydia van Dijk . 
;;;  2)Version 2.0.4

;;;date: 2001/06/17; author Alexander Vigodner 
;;   1) scilab-indent-defun is added (also in menu)
;;   thanks to Lydia van Dijk . 

;;;date: 2001/06/18; author Alexander Vigodner 
;;   1) garbage-filter
;;;  2)Version 2.0.6

;;;date: 2001/06/25; author Alexander Vigodner 
;;   1) scilab-server-start and scilab-server-kill are introduced
;;   They mimic gnuserv when it is possible (for xemacs it is the standard, for
;;   emacs server is the standard, but gnuserv can be installed. gnuserv is much 
;;   stonger then server. So scilab-server-start/kill run/kill on of the serevers
;;  and addtionally update %version scilab variable
;;;  2)Version 2.0.7

;;;date: 2001/06/25; author Alexander Vigodner 
;;   1) scilab-comment is fixed, thanks Lydia
;;;  2)Version 2.0.8

;;;date: 2001/06/28; author Alexander Vigodner 
;;   1) No necessary to define TERM=dumb for xemacs now. Graphics works well
;;    Thanks to B. Levitan for pointing mea again on this problem
;;;  2)Version 2.0.9

;;;date: 2001/06/28; author Alexander Vigodner 
;;   1) Start interface lisp from scilab
;;;  2)Version 2.1.0
;; 

;;;date: 2001/07/03; author Alexander Vigodner 
;;   1) builtins are highlighted now 
;;;  2)Version 2.1.1
;; 
;;;date: 2001/07/03; author Alexander Vigodner 
;;   1) otherwise, catch, try are removed from keywords
;;;  2)Version 2.1.1b
;; 

;;;date: 2001/07/05; author Alexander Vigodner 
;;   1) Revision hidlighting
;;;  2)Version 2.1.2
;; 

;;;date: 2001/07/05; author Alexander Vigodner 
;;   1) Builtins _ libfunctions are hudhligthed now. This is may be done disbale  if too slow
;;   2) Fixing switching-buffers .Thanks to B. Levitan 
;;;  2)Version 2.1.3
;; 

;;;date: 2001/07/08; author Alexander Vigodner 
;;   1) Fixed mess of windows during completion
;;;  2)Version 2.1.4
;; 

;;;date: 2001/07/08; author Alexander Vigodner 
;;   1) scilab-server-status. gnuserv is proper now.(TMPDIR)
;;;  2)Version 2.1.4


;;;date: 2001/07/10; author Alexander Vigodner 
;;   1) Added hack GNU_PORT for gnuserver 
;;   2) Selected the refer. buffer back during completion    
;;;  3)Version 2.1.5
;; 

;;;date: 2001/07/10; author Alexander Vigodner 
;;   1) Exit/Start scilab moved to the end of menu in scilab-mode 
;;   after B.Levitan request
;;   2) Number of emacs/Xemacs is added to to %version
;;;  3)Version 2.1.7
;; 





