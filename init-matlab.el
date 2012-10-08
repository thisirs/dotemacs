;;; matlab mode
;; (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(add-to-list 'load-path "~/.emacs.d/vendor/matlab-emacs")
(require 'matlab-load)
(setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))

(add-hook 'matlab-mode-hook
          (lambda ()
            (setq fill-column 76)                       ; where auto-fill should wrap
            (turn-on-auto-fill)
            (setq-default matlab-functions-have-end t)
            (setq-default matlab-indent-function-body t)
            (setq matlab-change-current-directory t)
            (local-set-key "\M-;" 'comment-dwim)
            (auto-fill-mode -1)
            (setq-default matlab-indent-function t)))

(provide 'init-matlab)
