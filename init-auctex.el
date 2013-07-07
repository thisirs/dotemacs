(when (file-exists-p "~/repositories/auctex/")
  (add-to-list 'load-path "~/repositories/auctex")
  (add-to-list 'load-path "~/repositories/auctex/preview")
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t))

(load "preview.el" nil t t)

(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))

(TeX-global-PDF-mode 1)

(setq TeX-save-query nil) ; autosave before compiling

(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;; Needed to use external programs such as gnuplot
(setq LaTeX-command "latex --shell-escape")

;; Correct indentation
(setq LaTeX-item-indent 0)

;; Newline and indent in tex files
(setq TeX-newline-function 'newline-and-indent)

;; Don't prompt for choosing ref label style
(setq reftex-ref-macro-prompt nil)

;; Derive a label name for figure and table environments as well
(setq reftex-insert-label-flags '("sft" "sft"))

;; Add reftex support for my custom environment
(add-to-list 'reftex-label-alist
             '("prop" ?m "prop:" "~\\ref{%s}" nil ("proposition") nil))

(add-to-list 'reftex-label-alist
             '("thm" ?m "thm:" "~\\ref{%s}" nil ("theorem" "théorème") nil))

(add-to-list'reftex-label-alist
 '("table" 116 "tab:" "~\\ref{%s}" caption
   (regexp "tables?" "tab\\." "Tabellen?")))

;; Disable fill in env
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
            (TeX-source-correlate-mode 1))) ; Source Specials
;;(add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)"))))

;; Enable fr dictionary when using package frenchb
(add-hook 'TeX-language-fr-hook
          (lambda () (ispell-change-dictionary "fr")))

(setq TeX-view-program-list '(("Evince" "evince --page-label=%(outpage) %o")))

;; hook function to use in `TeX-command-list' list
(defun TeX-run-Make-or-TeX (name command file)
  (let* ((master (TeX-master-directory)))
    (cond
     ((and (file-exists-p (concat master "Makefile"))
           (executable-find "make"))
      (TeX-run-command name "make" file))
     ((and (file-exists-p (concat master "Rakefile"))
           (executable-find "rake"))
      (TeX-run-command name "rake" file))
     (t
      (TeX-run-TeX name command file)))))

(eval-after-load "latex"
  '(add-to-list 'TeX-command-list
                '("Make" "%`%l%(mode)%' %t"
                  TeX-run-Make-or-TeX nil
                  (latex-mode doctex-mode)
                  :help "Run Make or LaTeX if no Makefile")))

;; needs to be extended to handle rake
(defadvice TeX-command-query (before check-make activate)
  (let ((default-directory (TeX-master-directory)))
    (unless (eq 0 (call-process "make" nil nil nil "-q"))
      (TeX-process-set-variable (ad-get-arg 0)
                                'TeX-command-next
                                TeX-command-default))))


(setq LaTeX-includegraphics-read-file
      'LaTeX-includegraphics-read-file-relative-helm)

(defun LaTeX-includegraphics-read-file-relative-helm ()
  "Function to read an image file. Disable
`TeX-search-files-kpathsea' and allow helm completion."
  (require 'helm-mode)
  (file-relative-name
   (helm-completing-read-default-1
    "Image file: "
    (delete-dups
     (mapcar 'list
             (letf (((symbol-function 'TeX-search-files-kpathsea)
                     (lambda (extensions nodir strip))))
               (TeX-search-files (list (concat (TeX-master-directory) "img/"))
                                 LaTeX-includegraphics-extensions t))))
    nil nil nil nil nil nil
    "Image file"
    nil)
   (TeX-master-directory)))

(defun LateX-insert-image-path ()
  (interactive)
  (insert (LaTeX-includegraphics-read-file-relative-helm)))

(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "C-c C-i") 'LateX-insert-image-path))

(provide 'init-auctex)
