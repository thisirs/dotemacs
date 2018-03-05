;; http://ess.r-project.org
(use-package ess-site                   ; Emacs Speaks Statistics
  :ensure ess
  :preface
  ;; No special behaviour of comments starting with #, ## or ###
  (setq ess-indent-with-fancy-comments nil)
  :config
  ;; No double sharp sign when starting a comment
  (setq ess-r-customize-alist
        (append ess-r-customize-alist '((comment-add . 0))))

  ;; Override TeX commands Sweave -> KnitR
  (defun ess-swv-add-TeX-commands ()
    "Add commands to AUCTeX's \\[TeX-command-list]."
    (unless (and (featurep 'tex-site) (featurep 'tex))
      (error "AUCTeX does not seem to be loaded"))
    (add-to-list 'TeX-command-list
                 '("Knit" "Rscript -e \"library(knitr); all_patterns$tex$$chunk.code <- '^\\s*%+'; knit_patterns$set(all_patterns[['tex']]); knit('%t')\""
                   TeX-run-command nil (latex-mode) :help
                   "Run Knitr") t)
    (add-to-list 'TeX-command-list
                 '("LaTeXKnit" "%l %(mode) %s"
                   TeX-run-TeX nil (latex-mode) :help
                   "Run LaTeX after Knit") t)
    (setq TeX-command-default "Knit")
    (mapc (lambda (suffix)
            (add-to-list 'TeX-file-extensions suffix))
          '("nw" "Snw" "Rnw")))

  (defun ess-swv-remove-TeX-commands (x)
    "Helper function: check if car of X is one of the Knitr strings"
    (let ((swv-cmds '("Knit" "LaTeXKnit")))
      (unless (member (car x) swv-cmds) x)))

  (setq ess-swv-plug-into-AUCTeX-p t)

  ;; Trigger plugging with right hooks
  (if (use-package tex-site)
      (ess-swv-plug-into-AUCTeX))

  (defun tidy-R-buffer (&optional beg end formatR-opts)
    "Tidy current buffer with the R library formatR."
    (interactive "r\nMformatR options: ")
    (unless (zerop (shell-command "Rscript -e \"quit(status = ifelse(require(formatR), 0, 1))\""))
      (user-error "Need formatR library"))
    (save-excursion
      (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
             (end (if (region-active-p) (region-end) (point-max)))
             (buf (current-buffer))
             (prefix (save-excursion
                       (goto-char beg)
                       (current-indentation)))
             (command (concat "Rscript" " -e " (format "\"library(formatR); tidy_source(file('stdin', 'r'), arrow = TRUE, %s)\"" (concat formatR-opts))))
             (temp-buffer (generate-new-buffer " *temp*")))
        (unwind-protect
            (progn
              (shell-command-on-region beg end command temp-buffer)
              (with-current-buffer temp-buffer
                (goto-char (point-min))
                (when (> prefix 0)
                  (while (not (eobp))
                    (skip-chars-forward " \t")
                    (unless (eolp)                ;ignore blank lines
                      (let ((i (current-column)))
                        (delete-region (line-beginning-position) (point))
                        (indent-to (+ i prefix))))
                    (forward-line))))
              (with-current-buffer buf
                (delete-region beg end)
                (insert-buffer-substring temp-buffer))))
        (kill-buffer temp-buffer))))

  (defun tidy-Rtex-chunks ()
    "Tidy all the R chunks delimited by begin.rcode/end.rcode."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^ *\\(%+\\) *begin\\.rcode *" nil t)
        (let* ((column (progn
                         (goto-char (match-beginning 1))
                         (current-column)))
               (beg (progn
                      (forward-line 1)
                      (point-at-bol)))
               (end (progn
                      (re-search-forward "end\\.rcode" nil t)
                      (forward-line -1)
                      (point-at-eol)))
               (code (delete-and-extract-region beg end))
               (new-code (progn
                           (make-temp-file "foo")
                           (with-temp-buffer
                             (insert code)
                             (goto-char (point-min))
                             (while (re-search-forward " *%+ *" nil t)
                               (replace-match ""))
                             (write-file (make-temp-file "foo"))
                             (tidy-R-buffer nil nil "indent = 2, arrow = TRUE, width.cutoff = 500")
                             (goto-char (point-min))
                             (while (re-search-forward "^\\(.\\)" nil t)
                               (replace-match (concat
                                               (make-string column ?\ )
                                               "% "
                                               "\\1")))
                             (save-buffer)
                             (string-trim-right (buffer-string))))))
          (goto-char beg)
          (insert new-code))))))

(provide 'init-ess)
