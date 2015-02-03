;; autoloads
(require 'matlab-load)

(with-eval-after-load "matlab"
  (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))

  ;; Don't switch to figure window
  (setenv "WINDOWID" (frame-parameter (selected-frame) 'outer-window-id))

  ;; From a patched version of matlab.el
  (if (boundp 'matlab-change-current-directory)
      (setq-default matlab-change-current-directory t))

  (setq-default matlab-functions-have-end t)
  (setq-default matlab-indent-function-body 'guess)
  (setq-default matlab-indent-function nil)

  (defun matlab-mode-preferences ()
    (local-set-key "\M-j" #'join-to-next-line)
    (local-set-key "\M-;" 'comment-dwim)
    (auto-fill-mode -1))

  (add-hook 'matlab-mode-hook 'matlab-mode-preferences)

  (defun matlab-shell-fix-slowness ()
    (remove-hook 'comint-output-filter-functions 'matlab-shell-render-html-anchor t)
    (remove-hook 'comint-output-filter-functions 'matlab-shell-render-errors-as-anchor t))

  (add-hook 'matlab-shell-mode-hook 'matlab-shell-fix-slowness))

(provide 'init-matlab)
