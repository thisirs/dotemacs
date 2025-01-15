;; http://www.gnu.org/software/auctex/
(use-package latex                      ; Integrated environment for *TeX*
  :ensure (auctex :repo "https://git.savannah.gnu.org/git/auctex.git"
                  :pre-build (("./autogen.sh") ("./configure" "--without-texmf-dir" "--with-lispdir=.") ("make")))
  :init
  (require 'tex-site)
  :preface
  ;; Remove eqnarray and eqnarray* from known environments
  (defun LaTeX-remove-eqnarray ()
    (mapc (lambda (env)
            (setq LaTeX-environment-list
                  (assq-delete-all
                   (car (assoc env (LaTeX-environment-list)))
                   LaTeX-environment-list)))
          '("eqnarray" "eqnarray*")))

  (defun LaTeX-auto-fill-maybe ()
    "Return non-nil if current environment should be filled."
    (let ((value (assoc (LaTeX-current-environment) LaTeX-indent-environment-list)))
      (or (null value) (cdr value))))

  (defun LaTeX-auto-fill-config ()
    "Setup auto-fill for LaTeX.

Auto-fill is enabled everywhere except for environments specified
in `LaTeX-indent-environment-list' whose second element is nil.
This corresponds to environments that are not supposed to be
filled by AUCTeX functions."
    (turn-on-auto-fill)
    (advice-add 'auto-fill-function :before-while #'LaTeX-auto-fill-maybe)
    (when comment-auto-fill-only-comments
      (set (make-local-variable 'comment-auto-fill-only-comments) nil)))

  :hook
  ;; Revert buffer visiting pdf file after compilation
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  (LaTeX-mode-hook . LaTeX-remove-eqnarray)
  (LaTeX-mode-hook . LaTeX-auto-fill-config)

  :custom
  (TeX-engine 'luatex)

  (TeX-source-correlate-mode t)

  ;; Autosave before compiling
  (TeX-save-query nil)

  ;; Correct indentation
  (LaTeX-item-indent -2)
  (LaTeX-indent-level 2)

  ;; Newline and indent in tex files
  (TeX-newline-function 'newline-and-indent)

  :config

  (defun TeX-command-shell-escape-p (arg)
    (string= arg "-shell-escape"))
  (put 'TeX-command-extra-options 'safe-local-variable 'TeX-command-shell-escape-p)

  ;; Add some more verbatim environments
  (add-to-list 'LaTeX-verbatim-environments "minted")
  (add-to-list 'LaTeX-verbatim-environments "CVerbatim")
  (add-to-list 'LaTeX-verbatim-environments "Verbatim")
  (add-to-list 'LaTeX-verbatim-environments "pycode")
  (add-to-list 'LaTeX-verbatim-environments "knitr")

  (add-to-list 'LaTeX-verbatim-macros-with-braces "rinline")

  ;; Custom indentation for some environments
  (add-to-list 'LaTeX-indent-environment-list '("minted" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("CVerbatim" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("Verbatim" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("pycode" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("knitr" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("scope"))
  (add-to-list 'LaTeX-indent-environment-list '("figure"))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))


  ;; Add a Make command in to `TeX-command-list'. Arguments to Make can be
  ;; supplied by the buffer-local variable `TeX-make-arguments'.

  (defvar-local TeX-make-arguments "") ; make arguments as a buffer-local variable
  (put 'TeX-make-arguments 'safe-local-variable 'stringp)

  ;; Add makeargs expansion string
  (add-to-list 'TeX-expand-list
               '("%(makeargs)" (lambda () (or TeX-make-arguments ""))))

  (defun make-TeX-sentinel (process name)
    (let ((buffer (process-buffer process)))
      (if (zerop (process-exit-status process))
          (message (concat name ": finished successfully."))
        (message  (concat name " failed.  Type `%s' to display output.")
                  (substitute-command-keys
                   "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))))

  (defun TeX-run-Make (name command file)
    (TeX-run-command name command file)
    (setq TeX-sentinel-function #'make-TeX-sentinel))

  (add-to-list 'TeX-command-list
               '("Make" "make %(makeargs)"
                 TeX-run-Make nil
                 (latex-mode doctex-mode)
                 :help "Run Make")))


(use-package reftex
  :ensure nil
  :demand :after latex
  :preface
  (defun LaTeX-setup-reftex ()
    (when buffer-file-name
      (turn-on-reftex)
      (reftex-set-cite-format "~\\cite{%l}")))

  :hook (LaTeX-mode-hook . LaTeX-setup-reftex)

  :custom
  ;; Don't prompt for choosing ref label style
  (reftex-ref-macro-prompt nil)

  ;; Derive a label name for figure and table environments as well
  (reftex-insert-label-flags '("sft" "sft"))

  (reftex-plug-into-AUCTeX t)

  :config

  ;; Add reftex support for my custom environment
  (add-to-list 'reftex-label-alist
               '("prop" ?m "prop:" "~\\ref{%s}" nil ("proposition") nil))

  (add-to-list 'reftex-label-alist
               '("thm" ?m "thm:" "~\\ref{%s}" nil ("theorem" "théorème") nil))

  (add-to-list 'reftex-label-alist
               '("subnumcases" 101 nil "~\\eqref{%s}" eqnarray-like))

  (add-to-list 'reftex-label-alist
               '("table" 116 "tab:" "~\\ref{%s}" caption
                 (regexp "tables?" "tab\\." "Tabellen?"))))


[(use-package tex-site               ; Integrated environment for *TeX*
   :ensure auctex
   :config
   (use-package latex
     :ensure nil
     :defer t
     :defines TeX-sentinel-function
     :functions (TeX-run-command TeX-run-TeX)
     :config
     (require 'tex-buf)
     (TeX-global-PDF-mode 1)
     (TeX-source-correlate-mode 1)

     ;; Enable parse on load
     (setq TeX-parse-self t)

     ;; Enable parse on save
     (setq TeX-auto-save t)

     ;; Avoid generating auto/ directory when working on shared documents
     (defun disable-automatic-parsing ()
       (when (shared-directory)
         (setq-local TeX-parse-self nil)
         (setq-local TeX-auto-save nil)))

     (add-hook 'LaTeX-mode-hook #'disable-automatic-parsing)

     (setq TeX-engine-alist
           '((xetex_sh "XeTeX shell escape"
                       "xetex --file-line-error --shell-escape"
                       "xelatex --file-line-error --shell-escape")))

     ;; Run knitr on tex files
     (add-to-list 'TeX-command-list
                  '("knitr + LaTeX" "%`%l%(mode) -jobname=%s %' %s_knitr.tex"
                    TeX-run-knitr-and-TeX nil
                    (latex-mode doctex-mode)
                    :help "Run knitr and LaTeX"))

     (defun TeX-run-consecutive (defun-name-cmd-file-list)
       "Run consecutive asynchronous commands."
       (let ((args (car defun-name-cmd-file-list)))
         (when args
           (let ((buf (current-buffer))
                 (process (apply #'funcall args)))
             (if (processp process)
                 (setq TeX-sentinel-function
                       `(lambda (process name)
                          (if (eq 0 (process-exit-status process))
                              (let ((buffer-name ,(TeX-process-buffer-name (nth 3 args)))
                                    (log-buffer ,(concat "*" (abbreviate-file-name (expand-file-name (nth 3 args))) " output for " (nth 1 args) "*")))
                                (with-current-buffer buffer-name
                                  (copy-to-buffer (get-buffer-create log-buffer) nil nil))
                                (with-current-buffer ,buf
                                  (message "Process %s succeeded" name)
                                  (TeX-run-consecutive ',(cdr defun-name-cmd-file-list))))
                            (message "Process %s failed" name))))
               (TeX-run-consecutive (cdr defun-name-cmd-file-list)))))))

     (defun TeX-run-knitr-and-TeX (name command file)
       (let ((knitr-file (concat file "_knitr")))
         (TeX-run-consecutive
          `((TeX-run-command
             "knitr"
             ,(format "Rscript -e \"library(knitr); knitr::opts_knit\\$set(progress = TRUE, verbose = TRUE, concordance = TRUE); knit('%s', output = '%s')\""
                      (concat file ".tex")
                      (concat knitr-file ".tex"))
             ,file)
            (TeX-run-command
             "tangle"
             ,(format "Rscript -e \"library(knitr); knit('%s', output = '%s', tangle = TRUE)\""
                      (concat file ".tex")
                      (concat file ".R"))
             ,file)
            (TeX-run-TeX ,name ,command ,file)
            (TeX-run-function
             "patchSynctex"
             ,(let (print-level print-length)
                (format "%S"
                        `(when (or t (file-exists-p ,(concat knitr-file "-concordance.tex")))
                           (cl-letf (((symbol-function 'messagea)
                                      (lambda (&rest args)
                                        nil)))
                             (TeX-run-shell nil ,(format "Rscript -e \"library(patchSynctex); patchSynctex('%s')\""
                                                         knitr-file)
                                            nil)
                             (shell-command (format "gunzip < \"%s\" | sed s/_knitr\\.Rnw/\\.tex/g | gzip | sponge \"%s\""
                                                    ,(concat file ".synctex.gz")
                                                    ,(concat file ".synctex.gz")))
                             ;; make evince reload with patched  synctex file
                             (shell-command ,(format "cat \"%s\" | sponge \"%s\"" (concat file ".pdf") (concat file ".pdf")))))))
             nil)))))

     (defun TeX-run-TeX-pythontex-and-TeX (name command file)
       (TeX-run-consecutive
        `((TeX-run-command "Remove cache files"
                           ,(format "rm -rf pythontex-files-%s" file)
                           ,file)
          ;; (TeX-run-TeX ,name ,command ,file)
          (TeX-run-command "pythontex"
                           ,(format "pythontex \"%s\"" file)
                           ,file)
          (TeX-run-TeX ,name ,command ,file))))

     ;; Run knitr on tex files
     (add-to-list 'TeX-command-list
                  '("pythontex + LaTeX" "%`%l%(mode) -jobname=%s %' %s.tex"
                    TeX-run-TeX-pythontex-and-TeX nil
                    (latex-mode doctex-mode)
                    :help "Run pythontex and LaTeX"))



     (defun latex-auto-fill-everywhere ()
       (when comment-auto-fill-only-comments
         (set (make-local-variable 'comment-auto-fill-only-comments)
              nil)))

     (add-hook 'LaTeX-mode-hook #'latex-auto-fill-everywhere)

     ;; Enable fr dictionary when using package frenchb
     (add-hook 'TeX-language-fr-hook (lambda () (ignore-errors (ispell-change-dictionary "fr"))))

     ;; Adapted from http://emacs.stackexchange.com/questions/23867/inform-auctex-about-index-style-file
     (defvar TeX-index-options "")
     (make-variable-buffer-local 'TeX-index-options)
     (put 'TeX-index-options 'safe-local-variable 'stringp)
     ;; Add new expansion string
     (add-to-list 'TeX-expand-list
                  '("%(indexopts)" (lambda () TeX-index-options)))
     ;; Add new command.
     (setcdr (assoc "Index" TeX-command-list)
             '("makeindex %(indexopts) %s"
               TeX-run-index nil t
               :help "Run makeindex to create index file"))

     ;; hook function to use in `TeX-command-list' list
     (defvar-local TeX-make-arguments "")
     (put 'TeX-make-arguments 'safe-local-variable 'stringp)

     (add-to-list 'TeX-expand-list
                  '("%(makeargs)" (lambda () (or TeX-make-arguments ""))))

     (defun make-knitr-TeX-sentinel (process name)
       (let ((buffer (process-buffer process)))
         (if (zerop (process-exit-status process))
             (message (concat name ": finished successfully."))
           (message  (concat name " failed.  Type `%s' to display output.")
                     (substitute-command-keys
                      "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))))

     (defun TeX-run-Make (name command file)
       (TeX-run-command name command file)
       (setq TeX-sentinel-function #'make-knitr-TeX-sentinel))

     (add-to-list 'TeX-command-list
                  '("Make" "make %(makeargs)"
                    TeX-run-Make nil
                    (latex-mode doctex-mode)
                    :help "Run Make"))

     ;; Viewer: prefer pdf-tools when predicate pdf-tools-running is true
     (setq TeX-view-program-selection
           (cons '((output-pdf pdf-tools-running) "PDF Tools")
                 TeX-view-program-selection))

     ;; Prefer pdf-tools when only one window in current frame or pdf
     ;; is already showed in a window
     (add-to-list 'TeX-view-predicate-list
                  '(pdf-tools-running
                    (let ((master (TeX-active-master)))
                      (or (= 1 (count-windows))
                          (when-let ((buf (find-buffer-visiting (concat master ".pdf"))))
                            (get-buffer-window buf))))))

     ;; No special fontification for script
     (face-spec-set 'font-latex-script-char-face '((t (:foreground nil))) nil)

     ;; needs to be extended to handle rake
     ;; (defadvice TeX-command-query (before check-make activate)
     ;;   (let ((default-directory (TeX-master-directory)))
     ;;     (unless (eq 0 (call-process "make" nil nil nil "-q"))
     ;;       (TeX-process-set-variable (ad-get-arg 0)
     ;;                                 'TeX-command-next
     ;;                                 TeX-command-default))))



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
                        (TeX-search-files (list "~/SynologyDrive/Sylvain/recherche/data"
                                                (concat (TeX-master-directory) "img/"))
                                          LaTeX-includegraphics-extensions t))))
         nil nil nil nil nil nil
         "Image file"
         nil)
        (TeX-master-directory)))

     (setq LaTeX-includegraphics-read-file
           'LaTeX-includegraphics-read-file-relative-helm)

     (defun LateX-insert-image-path ()
       (interactive)
       (insert (LaTeX-includegraphics-read-file-relative-helm)))

     (define-key LaTeX-mode-map (kbd "C-c C-i") #'LateX-insert-image-path)

     ;; Taken from
     ;; http://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments
     ;; Breaks paragraph filling
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

     ;; Fix of the fix: filling in itemize works now
     (defun LaTeX-fill-region-as-paragraph-fix (oldfun from to &optional justify-flag)
       (let ((LaTeX-indent-environment-list LaTeX-indent-environment-list))
         (setq LaTeX-indent-environment-list
               (remove '("enumerate" LaTeX-indent-item) LaTeX-indent-environment-list))
         (setq LaTeX-indent-environment-list
               (remove '("itemize" LaTeX-indent-item) LaTeX-indent-environment-list))
         (funcall oldfun from to justify-flag)))

     (advice-add 'LaTeX-fill-region-as-paragraph
                 :around
                 'LaTeX-fill-region-as-paragraph-fix)

     (defun LaTeX-fill-region-as-para-do-fix (oldfun from end-marker justify-flag)
       (let ((LaTeX-indent-environment-list LaTeX-indent-environment-list))
         (add-to-list 'LaTeX-indent-environment-list '("itemize" LaTeX-indent-item))
         (add-to-list 'LaTeX-indent-environment-list '("enumerate" LaTeX-indent-item))
         (funcall oldfun from end-marker justify-flag)))

     (advice-add 'LaTeX-fill-region-as-para-do
                 :around
                 'LaTeX-fill-region-as-para-do-fix)


     (add-to-list 'LaTeX-indent-environment-list '("itemize" LaTeX-indent-item))
     (add-to-list 'LaTeX-indent-environment-list '("enumerate" LaTeX-indent-item))
     (add-to-list 'LaTeX-indent-environment-list '("description" LaTeX-indent-item))))]

(provide 'init-auctex)

;;; init-auctex.el ends here
