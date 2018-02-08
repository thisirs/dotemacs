(use-package elpy
  :ensure
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq python-indent-guess-indent-offset-verbose nil)
  (elpy-enable)
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "-i --simple-prompt")

  (when (use-package flycheck)
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(provide 'init-python)
