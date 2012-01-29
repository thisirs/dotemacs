(defun er/add-LaTeX-mode-expansions ()
  "Adds LaTeX-specific expansions for buffers in LaTeX-mode"
  (set (make-local-variable 'er/try-expand-list)
       (append
        er/try-expand-list
        '(LaTeX-mark-environment)
        '(LaTeX-mark-enclosing-$))))

(defun LaTeX-mark-enclosing-$ ()
  "Mark enclosing $"
  (er--setup)
  (when (and (texmathp) (equal (car texmathp-why) "$"))
      (set-mark (cdr texmathp-why))
      (re-search-forward "\\$" nil t)
      (exchange-point-and-mark)))

(add-hook 'LaTeX-mode-hook 'er/add-LaTeX-mode-expansions)

(provide 'LaTeX-mode-expansions)
