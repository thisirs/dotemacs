(require 'yasnippet)

;; Use only own snippets, do not use bundled ones
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))

(yas/global-mode 1)

(setq yas/triggers-in-field t)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
         (position (yas/field-end (yas/snippet-active-field snippet))))
    (goto-char position)))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
         (position (yas/field-start (yas/snippet-active-field snippet))))
    (goto-char position)))

(define-key yas/keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas/keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; C-k in a field
(defun yas/clear-current-field ()
  (interactive)
  (let ((field (and yas/active-field-overlay
                    (overlay-buffer yas/active-field-overlay)
                    (overlay-get yas/active-field-overlay 'yas/field))))
    (and field (delete-region (point) (yas/field-end field)))))

(define-key yas/keymap (kbd "C-k") 'yas/clear-current-field)

(provide 'init-yasnippet)
