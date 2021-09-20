;; http://www.gnu.org/software/auctex/
(use-package latex                      ; Integrated environment for *TeX*
  :straight auctex
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
filled by AuCTeX functions."
    (turn-on-auto-fill)
    (advice-add auto-fill-function :before-while #'LaTeX-auto-fill-maybe)
    (when comment-auto-fill-only-comments
      (set (make-local-variable 'comment-auto-fill-only-comments) nil)))

  :hook
  ;; Revert buffer visiting pdf file after compilation
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  (LaTeX-mode-hook . LaTeX-remove-eqnarray)
  (LaTeX-mode-hook . LaTeX-auto-fill-config)

  :custom

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

(provide 'init-auctex)

;;; init-auctex.el ends here
