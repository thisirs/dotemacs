(require 'yasnippet)

;; Use only own snippets, do not use bundled ones
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode 1)

(setq yas-triggers-in-field t)

(setq yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# key: $2
# --
$0")

;; Inter-field navigation
(defun yas-goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (goto-char position)))

(defun yas-goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (goto-char position)))

(define-key yas-keymap (kbd "C-e") 'yas-goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas-goto-start-of-active-field)

;; C-k in a field
(defun yas-clear-current-field ()
  (interactive)
  (let ((field (and yas--active-field-overlay
                    (overlay-buffer yas--active-field-overlay)
                    (overlay-get yas--active-field-overlay 'yas--field))))
    (and field (delete-region (point) (yas--field-end field)))))

(define-key yas-keymap (kbd "C-k") 'yas-clear-current-field)

(defvar yas-prefix-key "C-c y"
  "The key `yas-command-prefix' is bound to in the global map.")

(defvar yas-snippet-chars
  '("1" "2" "C-v" "<f1>")
  "Keystroke to st")

(defvar yas-disposable-snippets
  (make-vector (length yas-snippet-chars) nil))

(defvar yas-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'yas-new-snippet)
    (define-key map "r" 'yas-reload-all)
    (define-key map "v" 'yas-visit-snippet-file)
    (define-key map [t] 'yas-create-or-expand)
    map)
  "Yasnippet's global keymap")

(define-prefix-command 'yas-command-prefix)
(fset 'yas-command-prefix yas-map)
(setq  yas-command-prefix yas-map)
(global-set-key (kbd yas-prefix-key) yas-command-prefix)

(defun yas-create-or-expand ()
  "Expand the snippet recorded in a register or record the
currently selected region as new one."
  (interactive)
  (let* ((e (member (vector last-input-event)
                    (mapcar (lambda (key)
                              (read-kbd-macro key 'vect))
                            yas-snippet-chars)))
         (index (if e (- (length yas-snippet-chars) (length e)))))
    (if index
        (if (use-region-p)
            (let ((snippet (substring-no-properties
                            (delete-and-extract-region (region-beginning) (region-end)))))
              (aset yas-disposable-snippets index snippet)
              (message "Snippet stored in register %s"
                       (key-description (vector last-input-event))))
          (yas-expand-snippet (aref yas-disposable-snippets index)))
      (undefined))))

;; Don't use yasnippet with terminal
(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode -1)))

(provide 'init-yasnippet)
