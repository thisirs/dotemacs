(use-package matlab
  :mode ("\\.m$" . matlab-mode)
  :config
  (setq matlab-shell-command "matlab")
  (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))

  ;; Don't switch to figure window
  (setenv "WINDOWID" (frame-parameter (selected-frame) 'outer-window-id))

  ;; From a patched version of matlab.el
  (if (boundp 'matlab-change-current-directory)
      (setq-default matlab-change-current-directory t))

  (setq-default matlab-functions-have-end t)
  (setq-default matlab-indent-function-body 'guess)

  (defun matlab-mode-preferences ()
    (local-set-key "\M-j" #'join-to-next-line)
    (local-set-key "\M-;" 'comment-dwim)
    (auto-fill-mode -1))

  (add-hook 'matlab-mode-hook 'matlab-mode-preferences)

  (defun matlab-shell-fix-slowness ()
    (remove-hook 'comint-output-filter-functions 'matlab-shell-render-html-anchor t)
    (remove-hook 'comint-output-filter-functions 'matlab-shell-render-errors-as-anchor t))

  (add-hook 'matlab-shell-mode-hook 'matlab-shell-fix-slowness))

;; (setq matlab-shell-command "/home/sylvain/CloudStation/Sylvain/scripts/matlab-chroot.sh")
;; (setq matlab-shell-command-switches nil)

;; (setq matlab-shell-command "ssh")
;; (setq matlab-shell-command-switches '("srousseau@lagis12cores"
;;                                       "/Applications/MATLAB_R2014a.app/bin/matlab"
;;                                       "-nodesktop"
;;                                       "-nosplash"))

;; (defadvice matlab-shell-save-and-go (before rsync activate)
;;   (if (zerop (shell-command "bash -c 'ps aux | grep [M]ATLAB_R2014a.app' &> /dev/null"))
;;       ;; (shell-command "~/CloudStation/Sylvain/scripts/")
;;       (shell-command "bash -c 'rsync -e \"ssh -i /home/sylvain/.ssh/id_rsa\" -avz --relative \"/home/sylvain/./CloudStation/Sylvain/recherche/wip\" \"srousseau@lagis12cores:/Users/srousseau/\"' &> /dev/null")))

(provide 'init-matlab)
