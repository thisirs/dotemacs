(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/apel-10.7"))
(load "elscreen" "ElScreen" t)

(require 'auto-install)

(require 'instant)

(require 'notify-send)

;; abandonne le minibuffer quand on le quitte
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (let* ((tramp-prefix "/sudo:root@localhost:")
	  (l (length tramp-prefix)))
    (if (and
	  (= arg 1)
	  (buffer-file-name)
	  (not (eq t (string=
		       tramp-prefix
		       (substring buffer-file-name 0 l)))))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
		   (ido-read-file-name "File: "))))))

(defvar trans-term-p t "Check if fullscreen is on or off")

(defun non-trans-term ()
  (interactive)
  (set-frame-parameter nil 'alpha '(100 100))
  (tool-bar-mode 1)
  (menu-bar-mode 1)
  (jump-to-register 'r)
  ) 

(defun trans-term ()
  (interactive)
  (frame-configuration-to-register 'r)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (delete-other-windows)
  (set-frame-parameter nil 'alpha '(50 50))
  )

(defun toggle-trans-term ()
  (interactive)
  (setq trans-term-p (not trans-term-p))
  (if trans-term-p
    (non-trans-term)
    (trans-term)))

(require 'linkd)

(require 'anything)
(require 'anything-config)
(global-set-key (kbd "C-x C-a") 'anything-for-files)

(require 'magit)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs
  (lambda (e)
    (y-or-n-p-with-timeout
      "Really exit Emacs (automatically exits in 5 secs)? " 5 t)))

;; Laisser le curseur en place lors d'un défilement par pages. Par
;; défaut, Emacs place le curseur en début ou fin d'écran selon le
;; sens du défilement.
(setq scroll-preserve-screen-position t)

;; sélection avec SHIFT
;;(custom-set-variables '(pc-selection-mode t nil (pc-select)))

(load-library "paren")
(show-paren-mode 1)

(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
    (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)


;; sélection avec les flèches dans buffer-list
;;(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
  (quote (("default"
	    ("Org" ;; all org-related buffers
	      (mode . org-mode))
	    ("Mail"
	      (or ;; mail-related buffers
		(mode . message-mode)
		(mode . mail-mode)
		;; etc.;; all your mail related modes
		))
	    ("THISKEY's programming"
	      (filename . "/media/THISKEY/programming/"))
	    ("Programming" 
	      (or
		(mode . c-mode)
		(mode . perl-mode)
		(mode . python-mode)
		(mode . emacs-lisp-mode)
		(mode . ruby-mode)
		;; etc
		))
	    ("crap" (or
		      (name . "^\\*trace")
		      (name . "^\\*completions")
		      (name . "^\\*Quail")
		      (name . "^\\*magit")
		      (name . "^\\*Backtrace\\*$")
		      (name . "^\\*compilation\\*$")
		      (name . "^\\*scratch\\*$")
		      (name . "^\\*Messages\\*$")))
	    ("ERC" (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; indenter automatiquement le code collé :
(defadvice yank (after indent-region activate)
  (if (member major-mode '(ruby-mode emacs-lisp-mode scheme-mode lisp-mode
			    c-mode c++-mode objc-mode
			    latex-mode plain-tex-mode
			    python-mode))
    (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(ruby-mode emacs-lisp-mode scheme-mode lisp-mode
			    c-mode c++-mode objc-mode
			    latex-mode plain-tex-mode
			    python-mode))
    (indent-region (region-beginning) (region-end) nil)))



;; kill-ring-save (M-w) copie la ligne si aucune region active,
;; copie le groupe si au dessus d'un délimiteur
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
    (cond
      (mark-active (list (region-beginning) (region-end)))
      ((looking-at "[])}]") (message "group copied!")
	(list (1+ (point)) (save-excursion (forward-char) (backward-list))))
      ((looking-at "[[({]") (message "group copied!")
	(list (point) (save-excursion (forward-list))))
      (t (message "line copied!") (list (line-beginning-position)
				    (line-beginning-position 2))))))

;; kill-region (C-w) coupe la ligne courante si aucune région active
;; coupe le groupe si au dessus d'un délimiteur
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (cond
      (mark-active (list (region-beginning) (region-end)))
      ((looking-at "[])}]")
	(list (1+ (point)) (save-excursion (forward-char) (backward-list))))
      ((looking-at "[[({]")
	(list (point) (save-excursion (forward-list))))
      (t (list (line-beginning-position)
	   (line-beginning-position 2))))))


;;; la commande kill supprime automatiquement les espaces d'indentations si besoin
(defadvice kill-line (before check-position activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
			    c-mode c++-mode objc-mode
			    latex-mode plain-tex-mode
			    ruby-mode python-mode))
    (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	(just-one-space 0)
	(backward-char 1)))))


;;move between windows with meta-arrows
(windmove-default-keybindings 'meta)

;; Mettre un titre aux fenêtres
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; Non au défilement qui accélère
(setq mouse-wheel-progressive-speed nil)

;; sélection de tout le buffer
(global-set-key "\C-c\C-a" 'mark-whole-buffer)

;; pas de file<2> quand 2 buffers ont le même nom
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; put something different in the scratch buffer
(setq initial-scratch-message
  ";; scratch buffer created -- happy hacking\n\n")

;; backups
(setq make-backup-files t ;; do make backups
  backup-by-copying t     ;; and copy them here
  backup-directory-alist '(("." . "~/.emacs.d/emacs.backups"))
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)

;; pas d'autosave qui pue
(setq auto-save-default nil)


;; ouverture rapide avec la touche windows
(global-set-key (kbd "s-s s") ;; scratch
  (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-s e") ;; .emacs
  (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "s-s o") ;; .emacs
  (lambda () (interactive)
    (find-file-existing "/media/THISKEY/Documents/Org/TODO.org")))
(global-set-key (kbd "C-x à") 'delete-other-windows)
(global-set-key (kbd "C-x C-à") 'delete-other-windows)

;; intégration de remember dans org
(global-set-key (kbd "C-c r") 'remember)
(org-remember-insinuate)

(setq org-remember-templates
  (list
    '("Todo" ?t "* TODO %?\n  %i\n" "/media/THISKEY/Documents/Org/notes.org" "Tâches")
    '("Anniv" ?a "* %^{Birthday}t Anniversaire de %^{prompt}!\n" "/media/THISKEY/Documents/Org/birthday.org")
))
  

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-agenda-files (list "/media/THISKEY/Documents/Org/agenda.org"
			 "/media/THISKEY/Documents/Org/someday.org"
			 "/media/THISKEY/Documents/Org/birthday.org"
			 ))

(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")

;; se rappelle ou je suis dans un fichier
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace) ;; get the package


;; Indentation du buffer
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;; Numérotation des lignes dans la marge
(require 'linum)
(global-linum-mode 1)

;; Correction orthographique
(setq ispell-dictionary "francais")
(load-file "~/.emacs.d/flyspell-1.7n.el")

;; Cedet
(load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ECB
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)

;; color theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)

;; Scilab
;;(add-to-list 'load-path "~/.emacs.d/scilabelisp")
;;(load "scilab-startup")
;;(setq auto-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\)" . scilab-mode) auto-mode-alist))
;;(add-hook 'scilab-mode-hook '(lambda () (setq fill-column 90)))

;; Auctex
(add-to-list 'load-path "~/.emacs.d/auctex-11.85")
(load "auctex.el" nil t t)
(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))

;; indentation correcte des items
(setq LaTeX-item-indent 0)

(add-hook 'LaTeX-mode-hook
  '(lambda ()
     (reftex-mode)
     (flyspell-mode)))

(defun latex-accent () (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-string "\\'{e}" "é")
    (goto-char (point-min))
    (replace-string "\\'e" "é")
    (goto-char (point-min))
    (replace-string "\\`{e}" "è")
    (goto-char (point-min))
    (replace-string "\\`e" "è")
    (goto-char (point-min))
    (replace-string "\\`{a}" "à")
    (goto-char (point-min))
    (replace-string "\\`a" "à")
    (goto-char (point-min))
    (replace-string "\\`{u}" "ù")
    (goto-char (point-min))
    (replace-string "\\`u" "ù")
    (goto-char (point-min))
    (replace-string "\\^{e}" "ê")
    (goto-char (point-min))
    (replace-string "\\^e" "ê")
    (goto-char (point-min))
    (replace-string "\\^{o}" "ô")
    (goto-char (point-min))
    (replace-string "\\^{u}" "û")
    (goto-char (point-min))
    (replace-string "\\^u" "û")
    (goto-char (point-min))
    (replace-string "\\\"{i}" "ï")
    (goto-char (point-min))
    (replace-string "\\\"i" "ï")
    (goto-char (point-min))
    (replace-string "\\c{c}" "ç")
    (goto-char (point-min))
    (replace-string "\\^{i}" "î")
    (goto-char (point-min))
    (replace-string "\\^i" "î")))


;; pour naviguer facilement entre les buffers avec C-x b
;; affiche la liste des buffers et l'autocomplétion fait le reste
;; BUG ido-execute command marche pas quand c'est la première chose qu'on fait en entrant dans emacs, si on ouvre un fichier avant alors ça marche
;; Use C-f during file selection to switch to regular find-file
(require 'ido)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-execute-command-cache nil)

;; complétion à la ido avec M-x
(defun ido-execute-command ()
  (interactive)
  (call-interactively
    (intern
      (ido-completing-read
	"M-x "
	(progn
	  (unless ido-execute-command-cache
	    (mapatoms (lambda (s)
			(when (commandp s)
			  (setq ido-execute-command-cache
			    (cons (format "%S" s) ido-execute-command-cache))))))
	  ido-execute-command-cache)))))

(global-set-key "\M-x" 'ido-execute-command)




;; Autoriser la transparence
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
	(cadr (find 'alpha (frame-parameters nil) :key #'car))
	100)
    (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(60 60))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Ouverture des fichiers récents (menu en plus dans la barre de menu)
(require 'recentf)
(recentf-mode 1)
(setq recentf-arrange-by-rules-min-items 0
  recentf-arrange-by-rule-others nil
  recentf-arrange-rules
  '
  (
    ("Elisp files (%d)" ".\\.el\\(.gz\\)?$" "^\\.?emacs-")
    ("Ruby files (%d)" ".\\.rb$")
    ("Scilab files (%d)" ".\\.\\(sci\\|sce\\)$")
    ("Java files (%d)" ".\\.java$")
    ("C/C++ files (%d)" ".\\.c\\(pp\\)?$")
    ("PHP files (%d)" ".\\.\\(php\\|php3\\)$")
    ("Configuration files (%d)" "rc$\\|/\\.")
    ("Ada files (%d)" ".\\.ad[sb]$")
    ("TeX/LaTeX files (%d)" ".\\.\\(tex\\|bib\\|sty\\)$")
    ("Scripts (%d)" ".\\.\\(sh\\|pl\\)$")
    ("Documentation (%d)" "/doc/")
    ("Po files (%d)" "\\.po\\'\\|\\.po\\.")
    ("Lisp files (%d)" ".\\.lisp$")
    )
  recentf-max-saved-items 50
  recentf-max-menu-items 30
  recentf-menu-path nil
  recentf-exclude '(".emacs-customize$"
		     ".newsrc"
		     ".etags$"
		     ".emacs.bmk$"
		     ".bbdb$"
		     ".log$"
		     "^/tmp/")
  recentf-menu-filter 'recentf-arrange-by-rule
  recentf-menu-title "Recentf"
  )

(defun recentf-interactive-complete ()
  "find a file in the recently open file using ido for completion"
  (interactive)
  (let* ((all-files recentf-list)
	  (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
	  (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
	  (ido-make-buffer-list-hook
	    (lambda ()
	      (setq ido-temp-list filename-list)))
	  (filename (ido-read-buffer "Find Recent File: "))
	  (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
	  (result-length (length result-list)))
    (find-file
      (cond
	((= result-length 0) filename)
	((= result-length 1) (car result-list))
	( t
	  (let ( (ido-make-buffer-list-hook
		   (lambda ()
		     (setq ido-temp-list result-list))))
	    (ido-read-buffer (format "%d matches:" result-length))))
	))))


;; pouvoir ouvrir la liste des fichiers récents au clavier
;;(global-set-key "\C-x\C-r" 'recentf-open-files-complete)
(global-set-key "\C-x\C-r" 'recentf-interactive-complete)


;; Prise en charge de la molette de la souris
;; Utilisée seule, la rotation de la molette provoque un défilement de
;; 5 lignes par cran. Combinée à la touche Shift, le défilement est
;; réduit à une ligne. Combinée à la touche Control, le défilement
;; s'effectue page (1 hauteur de fenêtre) par page.
(require 'mwheel)
(mouse-wheel-mode 1)


;; Bind last command to F12
(global-set-key [(f12)] 'repeat-complex-command)

;; pouvoir utiliser la complétion sous emacs en ignorant la
;; casse ça évite de passer à côté d'une alternative parce qu'on ne se
;; souvenait pas qu'il y avait un caractère en majuscules...
(setq completion-ignore-case t)

;; filenames too, to browse with dired for example...
(setq read-file-name-completion-ignore-case t)

;; Désactivation des boites de dialogue
(setq use-file-dialog nil) 
(setq use-dialog-box nil)


;; l'auto-insert permet d'insérer selon l'extension d'un
;; fichier un contenu de fichier statique
(require 'autoinsert)
(auto-insert-mode t) ; Adds hook to find-files-hook
(setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/"))
(setq auto-insert-query nil)
(setq auto-insert-alist
  '(
     ("\\.rb$" . ["autoinsert.ruby" (lambda () (goto-char (point-max)))])
     ("\\.sh$"   . ["autoinsert.bash" (lambda () (goto-char (point-max)))])
     ("\\.tex$" . ["autoinsert.tex" (lambda () (goto-line 13))])
     ))
(setq auto-insert 'other)

;; (define-auto-insert "\.sh" " autoinsert.bash")
;; (define-auto-insert "\.rb" "autoinsert.ruby")


(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix)
  )


(global-set-key [(f2)] 'change-to-utf-8)

(require 'starter-kit-ruby)
;;(require 'ruby-electric)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(ecb-layout-name "left14")
  '(ecb-options-version "2.40")
  '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
  '(ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "my-layout2")))
  '(ecb-source-path (quote (("/media/KROKEY/programming" "/"))))
  '(ecb-tip-of-the-day nil)
  '(ecb-windows-width 0.2)
  '(gnuserv-program "/usr/lib/xemacs-21.0/i386-pc-linux/gnuserv")
  '(inhibit-startup-screen t)
  '(org-agenda-files nil)
  '(scilab-shell-command "/usr/bin/scilab"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )





(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/bin/bash")
(shell-pop-set-window-height 60) ; the number for the percentage of the selected window. 
(global-set-key [f1] 'shell-pop)


(server-start)

;; minibuffer history
;;(savehist-mode t)

;; Always add a final newline
(setq require-final-newline t)

;; Drive out the mouse when it's too near to the cursor.
(if (display-mouse-p) (mouse-avoidance-mode 'animate))


(setq x-select-enable-clipboard t        ; copy-paste should work ...
  interprogram-paste-function            ; ...with...
  'x-cut-buffer-or-selection-value)      ; ...other X clients


;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Customized Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 
  '(lambda ()
     (local-set-key (kbd "RET") 'newline-and-indent)))

(require 'eldoc)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 
  (lambda()
    (setq mode-name "ELisp")
    (local-set-key (kbd "C-<f7>") ;; overrides global C-f7 (compile) 
      '(lambda()(interactive) 
         (let ((debug-on-error t)) 
           (eval-buffer)
           (message "buffer evaluated")))) ; 

    ;; complete lisp symbols as well
    (make-local-variable 'hippie-expand-try-functions-list)
    (setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
	 try-complete-lisp-symbol))

    (linum-mode t)
    (setq lisp-indent-offset 2) ; indent with two spaces, enough for lisp
    (require 'folding nil 'noerror)
    (font-lock-add-keywords nil '(("^[^;\n]\\{80\\}\\(.*\\)$"
                                    1 font-lock-warning-face prepend)))
    (font-lock-add-keywords nil 
      '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 
          1 font-lock-warning-face prepend)))  
    (font-lock-add-keywords nil 
      '(("\\<\\(add-hook\\|setq\\)" 
          1 font-lock-keyword-face prepend)))))

;; Delete the selected region when something is typed or with DEL
(delete-selection-mode t)

;; Text selection highlighted by default on Emacs 23
;;(transient-mark-mode t)

;; Use system trash (for emacs 23)
(setq delete-by-moving-to-trash t)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; This was installed by package-install.el.
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
;; Move this code earlier if you want to reference
;; packages in your .emacs.
(when
  (load
    (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;; additional menu
(require 'easymenu)
(setq my-encoding-map (make-sparse-keymap "Encoding Menu"))
(easy-menu-define my-encoding-menu my-encoding-map
  "Encoding Menu."
  '("Change File Encoding"
     ["UTF8 - Unix (LF)" (set-buffer-file-coding-system 'utf-8-unix) t]
     ["UTF8 - Mac (CR)" (set-buffer-file-coding-system 'utf-8-mac) t]
     ["UTF8 - Win (CR+LF)" (set-buffer-file-coding-system 'utf-8-dos) t]
     ["--" nil nil]
     ["Shift JIS - Mac (CR)" (set-buffer-file-coding-system 'sjis-mac) t]
     ["Shift JIS - Win (CR+LF)" (set-buffer-file-coding-system 'sjis-dos) t]
     ["--" nil nil]
     ["EUC - Unix (LF)" (set-buffer-file-coding-system 'euc-jp-unix) t]
     ["JIS - Unix (LF)" (set-buffer-file-coding-system 'junet-unix) t]
     ))
(define-key-after menu-bar-file-menu [my-file-separator]
  '("--" . nil) 'kill-buffer)
(define-key-after menu-bar-file-menu [my-encoding-menu]
  (cons "File Encoding" my-encoding-menu) 'my-file-separator)

