;; auto-indent pasted code
(dolist (func '(yank yank-pop))
  (ad-add-advice
   func
   `(,(intern (format "%s-advice" func)) nil t
     (advice . (lambda ()
                 "Auto indent on paste"
                 (maybe-indent-on-paste))))
   'after
   'last)
  (ad-activate func))

(defun maybe-indent-on-paste ()
  "Indent the region when in prog mode. Make an undo boundary to
cancel the indentation if needed."
  (when (or (derived-mode-p 'prog-mode)
            (memq major-mode '(ruby-mode
                               emacs-lisp-mode scheme-mode
                               lisp-interaction-mode sh-mode
                               lisp-mode c-mode c++-mode objc-mode
                               latex-mode plain-tex-mode
                               python-mode matlab-mode)))
    (undo-boundary)
    (indent-region (region-beginning) (region-end))))

(defun kill-region-or-backward ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-line 0)))

(global-set-key (kbd "C-w") 'kill-region-or-backward)

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

(global-set-key (kbd "M-w") 'save-region-or-current-line)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line t)))

;; copy when in read only buffer
(setq kill-read-only-ok t)

(defun beginning-of-line-or-text ()
  (interactive)
  (if (bolp)
      (beginning-of-line-text)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'beginning-of-line-or-text)

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

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun replace-string-swap (swap1 swap2)
  "Swap SWAP1 and SWAP2 in current buffer."
  (interactive "sString A: \nsString B: ")
  (replace-regexp (format "\\(%s\\)\\|\\(%s\\)"
                          (regexp-quote swap1)
                          (regexp-quote swap2))
                  `(replace-eval-replacement . (if (match-string 1) swap2 swap1))
                  nil
                  (if (region-active-p) (region-beginning))
                  (if (region-active-p) (region-end))))

;; Taken from Emacswiki
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

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
  (mapc (lambda (entry)
          (when (or (not arg) (yes-or-no-p (apply 'format "Query replace %s with %s ? " entry)))
            (goto-char (point-min))
            (if arg
                (query-replace-regexp (car entry) (cadr entry))
              (while (re-search-forward (car entry) nil t)
                (replace-match (cadr entry))))))
        '((" *\\(=+\\) *" " \\1 ")
          (" *\\(,\\) *" ", ")
          (" *\\(+\\) *" " + "))))


(provide 'init-editing)
