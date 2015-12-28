;; Auto-indent pasted code
(defun maybe-indent-on-paste (&optional arg)
  "Indent the region when in prog mode. Make an undo boundary to
cancel the indentation if needed."
  (when (or (and (not (derived-mode-p 'makefile-mode 'python-mode))
                 (derived-mode-p 'prog-mode))
            (memq major-mode '(latex-mode plain-tex-mode matlab-mode)))
    (undo-boundary)
    (indent-region (region-beginning) (region-end))))

(advice-add 'yank :after #'maybe-indent-on-paste)
(advice-add 'yank-pop :after #'maybe-indent-on-paste)

(defun kill-region-or-backward ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-line 0)))

(global-set-key (kbd "C-w") #'kill-region-or-backward)

;; Taken from https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el
(defun flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun save-region-or-current-line ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (flash-region (line-beginning-position) (line-beginning-position 2))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))
    (message "line copied")))

(global-set-key (kbd "M-w") #'save-region-or-current-line)

(defun join-to-next-line ()
  (interactive)
  (join-line t))

(global-set-key (kbd "M-j") #'join-to-next-line)

;; Copy when in read only buffer
(setq kill-read-only-ok t)

(defun move-beginning-of-line-or-text (&optional N)
  (interactive)
  (if (bolp)
      (beginning-of-line-text N)
    (move-beginning-of-line N)))

(global-set-key (kbd "C-a") #'move-beginning-of-line-or-text)

;; From https://github.com/purcell/emacs.d
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun replace-string-swap (swap1 swap2)
  "Swap SWAP1 and SWAP2 in current buffer."
  (interactive "sString A: \nsString B: ")
  (replace-regexp (format "\\(%s\\)\\|\\(%s\\)"
                          (regexp-quote swap1)
                          (regexp-quote swap2))
                  `(replace-eval-replacement . (if (match-string 1) ,swap2 ,swap1))
                  nil
                  (if (region-active-p) (region-beginning))
                  (if (region-active-p) (region-end))))

(defalias 'remove-duplicate-lines 'delete-duplicate-lines)

(defun indent-as-in (mode beg end &optional arg)
  "Indent selected region according to MODE.

Useful in Org code blocks or in minted and lstlistings
environments in LaTeX. With prefix argument, keep first line
indentation."
  (interactive "SMode: \nr\nP")
  (let ((txt (delete-and-extract-region beg end)))
    (condition-case e
        (insert
         (with-temp-buffer
           (insert txt)
           (funcall (intern (format "%s-mode" mode)))
           (indent-region (if (not arg)
                              (point-min)
                            (goto-char (point-min))
                            (forward-line 1)
                            (point))
                          (point-max))
           (buffer-string)))
      (error
       (insert txt)
       (message "Mode `%s' fails with: %S" (format "%s-mode" mode) (nth 1 e))))))

(defun fix-programming-punctuation (arg)
  "Fix common punctuation programming errors.

If ARG is non-nil, ask for confirmation on each match."
  (interactive "P")
  (save-excursion
    (mapc (lambda (entry)
            (when (or (not arg) (yes-or-no-p (apply 'format "Query replace %s with %s ? " entry)))
              (goto-char (point-min))
              (if arg
                  (query-replace-regexp (car entry) (cadr entry))
                (while (re-search-forward (car entry) nil t)
                  (replace-match (cadr entry))))))
          '((" *\\(=+\\) *" " \\1 ")
            (" *\\(,\\) *" ", ")
            (" *\\(+\\) *" " + ")
            (" *\\(\\*\\) *" " * ")))))

(defun indent-region-or-buffer ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-buffer))))

(provide 'init-editing)
