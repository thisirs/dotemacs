;; Auto-indent pasted code
(defun maybe-indent-on-paste (&optional arg)
  "Indent the region when in prog mode. Make an undo boundary to
cancel the indentation if needed."
  (when (or (and (not (derived-mode-p 'makefile-mode 'python-mode))
                 (derived-mode-p 'prog-mode))
            (memq major-mode '(latex-mode plain-tex-mode matlab-mode ess-mode)))
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

(global-set-key [remap kill-ring-save] #'save-region-or-current-line)

(defun join-to-next-line ()
  (interactive)
  (join-line t))

(global-set-key (kbd "M-j") #'join-to-next-line)

;; Copy when in read only buffer
(setq kill-read-only-ok t)

(defun beginning-of-line-or-indentation (&optional arg)
  "Move point back to indentation of beginning of line.
Move point to the beginning of line. If point is already there,
move to the first non-whitespace character on this line.
Effectively toggle between the first non-whitespace character and
the beginning of the line. If ARG is not nil or 1, move forward
ARG - 1 lines first. If point reaches the beginning or end of the
buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

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

(defun replace-string-swap (beg end s1 s2)
  "Swap strings S1 and S1 in current region or buffer."
  (interactive (append (if (use-region-p) (list (region-beginning) (region-end))
                         (list (point-min) (point-max)))
                       (list (read-string "String A: ")
                             (read-string "String B: "))))
  (let ((rx (format "\\(%s\\)\\|\\(%s\\)" (regexp-quote s1) (regexp-quote s2))))
    (save-excursion
      (save-restriction
        (narrow-to-region (or beg (point-min)) (or end (point-max)))
        (goto-char (point-min))
        (while (re-search-forward rx nil t)
          (replace-match (if (match-string 1) s2 s1)))))))

(defalias 'remove-duplicate-lines 'delete-duplicate-lines)

(defun remove-empty-lines ()
  (interactive)
  (flush-lines "^$"))

(defalias 'flush-empty-lines 'remove-empty-lines)
(defalias 'delete-empty-lines 'remove-empty-lines)

(defun indent-as-in (mode beg end &optional arg)
  "Indent selected region according to MODE.

Useful in Org code blocks or in minted and lstlistings
environments in LaTeX. With prefix argument, keep first line
indentation."
  (interactive
   (list (completing-read
          "Mode: "
          (let (sym-list)
            (mapatoms (lambda (sym)
                        (if (string-match "-mode$" (symbol-name sym))
                            (setq sym-list (cons sym sym-list)))))
            sym-list))
         (if (use-region-p) (region-beginning) (point-min))
         (if (use-region-p) (region-end) (point-max))
         current-prefix-arg))
  (let ((txt (delete-and-extract-region beg end)))
    (condition-case-unless-debug e
        (insert
         (with-temp-buffer
           (insert txt)
           (funcall (intern mode))
           (goto-char (point-min))
           (if arg (forward-line))
           (indent-region (point) (point-max))
           (buffer-string)))
      (error
       (insert txt)
       (message "Mode `%s' fails with: %S" mode (nth 1 e))))))

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

(defun insert-euro ()
  "Insert a Euro currency symbol in utf-8."
  (interactive)
  (insert-char #x20ac))

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(provide 'init-editing)
