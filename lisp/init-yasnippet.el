;; http://github.com/joaotavora/yasnippet
(use-package yasnippet                  ; Yet another snippet extension for Emacs.
  :diminish yas-minor-mode
  :bind (("C-c y TAB" . yas-expand)
         ("C-c y r" . yas-reload-all)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         :map yas-keymap
         ("C-a" . yas-goto-start-of-active-field)
         ("C-e" . yas-goto-end-of-active-field)
         ("C-k" . yas-clear-current-field))
  :commands
  :init
  ;; Inter-field navigation
  (defun yas-goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas-active-snippets)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (goto-char position)))

  (defun yas-goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas-active-snippets)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (goto-char position)))

  ;; C-k in a field
  (defun yas-clear-current-field ()
    (interactive)
    (let ((field (and yas--active-field-overlay
                      (overlay-buffer yas--active-field-overlay)
                      (overlay-get yas--active-field-overlay 'yas--field))))
      (and field (delete-region (point) (yas--field-end field)))))

  (defun auto-insert-yasnippet-expand (snippet)
    "Expand yasnippet's SNIPPET in current buffer."
    (with-demoted-errors
        (save-window-excursion
          (require 'yasnippet)
          ;; make buffer visible before yasnippet
          ;; which might ask the user for something
          (switch-to-buffer (current-buffer))
          (yas-expand-snippet snippet))))

  (defmacro auto-insert-add-from-yasnippet (mode key &rest actions)
    (declare (indent defun))
    (mapc (lambda (dir)
            (if (file-directory-p (expand-file-name (symbol-name mode) dir))
                (yas--load-directory-1
                 (expand-file-name (symbol-name mode) dir)
                 mode)))
          (yas-snippet-dirs))
    (let ((snippets (mapcan #'(lambda (table)
                                (yas--fetch table key))
                            (let ((major-mode mode))
                              (yas--get-snippet-tables)))))
      (if snippets
          (cons 'progn (mapcar
                        (lambda (template)
                          `(push (cons (cons ',mode ,(car template))
                                       '(lambda ()
                                          (auto-insert-yasnippet-expand
                                           ,(yas--template-content (cdr template)))))
                                 auto-insert-alist))
                        (mapcan #'(lambda (table)
                                    (yas--fetch table key))
                                (let ((major-mode mode))
                                  (yas--get-snippet-tables)))))
        (error "No snippet with key \"%s\" in mode %s" key mode))))

  :config
  (auto-insert-add-from-yasnippet latex-mode "hdr"
    (TeX-normal-mode 1))
  (auto-insert-add-from-yasnippet sh-mode "sb"
    (normal-mode))
  (auto-insert-add-from-yasnippet org-mode "hdr")

  ;; Suppress excessive log messages
  (setq yas-verbosity 1)

  ;; Use only own snippets, do not use bundled ones
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" personal-emacs-directory))

  (setq yas-triggers-in-field t)

  ;; Don't use yasnippet with terminal
  (add-hook 'term-mode-hook
            (lambda ()
              (yas-minor-mode -1)))

  (yas-global-mode 1)

  (defvar yas-snippet-chars
    '("1" "2" "3")
    "Keystroke to st")

  (defvar yas-disposable-snippets
    (make-vector (length yas-snippet-chars) nil))

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
)


(defvar yas-snippet-chars
  '("1" "2" "3")
  "Keystroke to st")

(defun yas-flash-bind-flash-keys ()
  (mapc (lambda (key)
          (define-key global-map (kbd (concat "C-c y " key)) #'yas-create-or-expand))
        yas-snippet-chars))

(yas-flash-bind-flash-keys)

(provide 'init-yasnippet)
