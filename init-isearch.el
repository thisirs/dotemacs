;; Put the cursor in an intelligent place when searching
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match"
  (and isearch-forward isearch-other-end
       (not mark-active)
       (not isearch-mode-end-hook-quit)
       (goto-char isearch-other-end)))

;; occur-mode
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Staying in isearch mode when typing M-< M-> C-l
(defun isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(define-key isearch-mode-map "\M-<" 'isearch-beginning-of-buffer)

(defun isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

;; Taken from https://github.com/magnars/.emacs.d.git
(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (buffer-substring (region-beginning)
                                                   (region-end)))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (buffer-substring (region-beginning)
                                                   (region-end)))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(global-set-key [remap isearch-forward] 'isearch-forward-use-region)
(global-set-key [remap isearch-backward] 'isearch-backward-use-region)

;; Add newline to lax isearch
(setq search-whitespace-regexp "[[:space:]\n]+")

(define-key isearch-mode-map "\M->" 'isearch-end-of-buffer)

(define-key isearch-mode-map (kbd "C-c") 'isearch-toggle-case-fold)

(define-key isearch-mode-map "\C-l" 'recenter-top-bottom)

(define-key isearch-mode-map (kbd "C-h") 'isearch-mode-help)

(provide 'init-isearch)
