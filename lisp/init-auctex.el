(load "auctex.el" nil t t)
(load "preview.el" nil t t)

(TeX-global-PDF-mode 1)

;; Autosave before compiling
(setq TeX-save-query nil)

;; Enable parse on load
(setq TeX-parse-self t)

;; Enable parse on save
(setq TeX-auto-save t)

;; Avoid auto/ directory when shared
(defun disable-automatic-parsing ()
  (when (and (buffer-file-name)
             (string-prefix-p "~/ownCloud/Shared/" (abbreviate-file-name (buffer-file-name))))
    (set (make-local-variable 'TeX-parse-self) nil)
    (set (make-local-variable 'TeX-auto-save) nil)))
(add-hook 'LaTeX-mode-hook 'disable-automatic-parsing)

;; shell-escape needed to use external programs such as gnuplot
;; file-line-error fixes bug where errors were not correctly parsed
(setq LaTeX-command-style
      '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))

(setq TeX-engine-alist
      '((xetex_sh "XeTeX shell escape"
                  "xetex --file-line-error --shell-escape"
                  "xelatex --file-line-error --shell-escape"
                  "xetex")))

;; Correct indentation
(setq LaTeX-item-indent 0)

;; Newline and indent in tex files
(setq TeX-newline-function 'newline-and-indent)

;; Don't prompt for choosing ref label style
(setq reftex-ref-macro-prompt nil)

;; Derive a label name for figure and table environments as well
(setq reftex-insert-label-flags '("sft" "sft"))

;; Setup entry in reftex-label-alist, using all defaults for equations
(setq reftex-label-alist '((detect-multiple-label ?e nil nil nil nil)))

(defun detect-multiple-label (bound)
  "Reftex custom label detection. When using conditionnal
compilation in latex with, for example, \\ifCLASSOPTIONonecolumn
labels might have to be defined multiple times. We factor out
that definition with \\def\onelabel{\\label{eq:22}} and use it
mutiple times."
  (if (re-search-backward "label{\\\\label{" bound t) (point)))

;; Add reftex support for my custom environment
(add-to-list 'reftex-label-alist
             '("prop" ?m "prop:" "~\\ref{%s}" nil ("proposition") nil))

(add-to-list 'reftex-label-alist
             '("thm" ?m "thm:" "~\\ref{%s}" nil ("theorem" "théorème") nil))

(add-to-list 'reftex-label-alist
             '("subnumcases" 101 nil "~\\eqref{%s}" eqnarray-like))

(add-to-list 'reftex-label-alist
             '("table" 116 "tab:" "~\\ref{%s}" caption
               (regexp "tables?" "tab\\." "Tabellen?")))

;; Disable fill in env
(eval-after-load "latex"
  '(mapc (lambda (env)
           (add-to-list 'LaTeX-indent-environment-list
                        (if (listp env)
                            env
                          (list env))))
         '("tikzpicture"
           "scope"
           "figure"
           ("minted" current-indentation))))

(add-to-list 'LaTeX-verbatim-environments "minted")

(defun latex-auto-fill-everywhere ()
  (when comment-auto-fill-only-comments
    (set (make-local-variable 'comment-auto-fill-only-comments)
         nil)))

(add-hook 'LaTeX-mode-hook 'latex-auto-fill-everywhere)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (when buffer-file-name
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-set-cite-format "~\\cite{%l}"))
            (TeX-source-correlate-mode 1))) ; Source Specials
;;(add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)"))))

;; Enable fr dictionary when using package frenchb
(add-hook 'TeX-language-fr-hook
          (lambda () (with-demoted-errors (ispell-change-dictionary "fr"))))

;; (setq TeX-view-program-list '(("Evince" "evince --page-label=%(outpage) %o")))

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
               (TeX-search-files (list "~/CloudStation/Sylvain/recherche/data"
                                       (concat (TeX-master-directory) "img/"))
                                 LaTeX-includegraphics-extensions t))))
    nil nil nil nil nil nil
    "Image file"
    nil)
   (TeX-master-directory)))

(defun LateX-insert-image-path ()
  (interactive)
  (insert (LaTeX-includegraphics-read-file-relative-helm)))

(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "C-c C-i") #'LateX-insert-image-path))

;;; Taken from http://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments
(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                            LaTeX-indent-level-item-continuation)
                       (* 2 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))

(defcustom LaTeX-indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

(eval-after-load "latex"
  '(setq LaTeX-indent-environment-list
         (nconc '(("itemize" LaTeX-indent-item)
                  ("enumerate" LaTeX-indent-item)
                  ("description" LaTeX-indent-item))
                LaTeX-indent-environment-list)))

(provide 'init-auctex)
