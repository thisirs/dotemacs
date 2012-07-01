;;; Auctex
(add-to-list 'load-path "~/.emacs.d/auctex-cvs")
(add-to-list 'load-path "~/.emacs.d/auctex-cvs/preview")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))
(setq TeX-PDF-mode t)
(setq TeX-save-query nil) ; autosave before compiling

(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;; Needed to use external programs such as gnuplot
(setq LaTeX-command "latex --shell-escape")

;; indentation correcte des items
(setq LaTeX-item-indent 0)

;; newline and indent in tex files
(setq TeX-newline-function 'newline-and-indent)

;; disable fill in env
(eval-after-load "latex"
  '(mapc (lambda (env) (add-to-list 'LaTeX-indent-environment-list (list env)))
         '("tikzpicture" "scope" "figure")))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (when buffer-file-name
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-set-cite-format "~\\cite{%l}"))
            (setq comment-auto-fill-only-comments nil)
            (flyspell-mode)
            (TeX-source-correlate-mode 1))) ; Source Specials
;;(add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)"))))

;; add styles location, francais.el is not loaded :(
(eval-after-load "latex"
  '(add-to-list 'TeX-style-path
                (expand-file-name "~/dotemacs/dotemacs/.emacs.d/auctex-11.86/style")))

;; enable fr dictionary when using package frenchb
(add-hook 'TeX-language-fr-hook
          (lambda () (ispell-change-dictionary "fr")))

(setq TeX-view-program-list '(("Evince" "evince --page-label=%(outpage) %o")))

(provide 'init-auctex)
