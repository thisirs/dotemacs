(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))
(require 'auto-install)
;(require 'anything)
;(require 'anything-config)

;(when (fboundp 'winner-mode)
;      (winner-mode 1))

;(load "elscreen" "ElScreen" t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs
      (lambda (e)
        (y-or-n-p-with-timeout
         "Really exit Emacs (automatically exits in 5 secs)? " 5 t)))

;; Laisser le curseur en place lors d'un défilement par pages. Par
;; défaut, Emacs place le curseur en début ou fin d'écran selon le
;; sens du défilement.
(setq scroll-preserve-screen-position t)

; sélection avec SHIFT
;(custom-set-variables '(pc-selection-mode t nil (pc-select)))

(load-library "paren")
(show-paren-mode 1)

; sélection avec les flèches dans buffer-list
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(global-set-key (kbd "C-S-k") 'kill-whole-line)

; copie de la ligne avec M-w
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

; coupe de la ligne avec C-w
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;;move between windows with meta-arrows
(windmove-default-keybindings 'meta)

; Mettre un titre aux fenêtres
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

; Non au défilement qui accélère 
(setq mouse-wheel-progressive-speed nil)

; pas de file<2> quand 2 buffers ont le même nom
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; pas de backup n'importe où...
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs.backups")))

; ouverture rapide avec la touche windows
(global-set-key (kbd "s-S") ;; scratch
  (lambda()(interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-E") ;; .emacs
  (lambda()(interactive)(find-file "~/.emacs")))

; se rappelle ou je suis dans un fichier
;(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
;(setq-default save-place t)                   ;; activate it for all buffers
;(require 'saveplace)                          ;; get the package


; Indentation du buffer
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


; Numérotation des lignes dans la marge
(require 'linum)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
;marche pas
;(setq minor-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\|\\.tex\\)" . (linum-mode 1)) minor-mode-alist))

; Correction orthographique
(setq ispell-dictionary "francais")
(load-file "~/.emacs.d/flyspell-1.7n.el")

; Cedet
(load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

; ECB
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)

; color theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")

; Scilab
;(add-to-list 'load-path "~/.emacs.d/scilabelisp")
;(load "scilab-startup")
;(setq auto-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\)" . scilab-mode) auto-mode-alist))
;(add-hook 'scilab-mode-hook '(lambda () (setq fill-column 90)))

; Auctex
(add-to-list 'load-path "~/.emacs.d/auctex-11.85")
(load "auctex.el" nil t t)
(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))

; indentation correcte des items
(setq LaTeX-item-indent 0)

(add-hook 'LaTeX-mode-hook
'(lambda ()
(reftex-mode)
(flyspell-mode)))

(defun  latex-accent () (interactive)
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

(add-hook 'LaTeX-mode-hook (lambda ()
      (local-set-key  (kbd "M-b") '(lambda () (interactive) (insert "{\\bf }") (backward-char)))
      (local-set-key  (kbd "M-c") '(lambda () (interactive) (insert "{\\tt }") (backward-char)))))

;; pour naviguer facilement entre les buffers avec C-x B
;; affiche la liste des buffers et l'autocomplétion fait le reste
(require 'ido)
(ido-mode t)

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


;; l'auto-insert permet d'insérer selon l'extension d'un
;; fichier un contenu de fichier statique
(require 'autoinsert)
(auto-insert-mode t)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/"))
(setq auto-insert-query nil)
(setq auto-insert-alist
      '(
	("\\.rb$" . ["autoinsert.ruby" (goto-char (point-max))])
	("\\.sh$"   . ["autoinsert.bash" (goto-char (point-max))])
	))
(setq auto-insert 'other)

;(define-auto-insert "\\.sh\\'" "autoinsert.bash")
;(define-auto-insert "\\.rb\\'" "autoinsert.ruby")


(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix)
)


(global-set-key [(f2)]          'change-to-utf-8)


; directory to put various el files into
;(add-to-list 'load-path "~/.emacs.d")

; loads ruby mode when a .rb file is opened.
;(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
;(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
;(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))


(require 'ruby-electric)

;(setq inhibit-startup-message t)

;(split-window-horizontally)   ;; want two windows at startup 
;(other-window 1)              ;; move to other window
;(shell)                       ;; start a shell
;(rename-buffer "shell-first") ;; rename it
;(other-window 1)              ;; move back to first window 

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
; '(ecb-auto-activate t)
 '(ecb-layout-name "left14")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "my-layout2")))
 '(ecb-source-path (quote (("/media/KROKEY/programming" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.2)
 '(global-linum-mode nil)
 '(gnuserv-program "/usr/lib/xemacs-21.0/i386-pc-linux/gnuserv")
 '(inhibit-startup-screen t)
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

; minibuffer history
;(savehist-mode t)

;; Always add a final newline
(setq require-trailing-newline t)

;;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)

(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; eldoc for quick reference
(require 'eldoc)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)


;; Delete the selected region when something is typed or with DEL
(delete-selection-mode 1)

;; Use system trash (for emacs 23)
(setq delete-by-moving-to-trash t)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
