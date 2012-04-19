;; boss key!

(defvar boss-window-configuration nil
  "Window configuration to switch to when the boss comes!")

(defvar my-window-configuration nil
  "Window configuration to switch back to!")

(defvar boss-mode nil)

(defun boss-save nil
  "Define current window configuration as boss's one"
  (interactive)
  (setq boss-window-configuration (current-window-configuration))
  (message "Boss window configuration set to current!"))

(defun boss-toggle nil
  "Toggle boss window configuration. Switch to *scratch* buffer
if `boss-window-configuration' is nil."
  (interactive)
  (if boss-mode
      (set-window-configuration my-window-configuration)
    (setq my-window-configuration (current-window-configuration))
    (if boss-window-configuration
        (set-window-configuration boss-window-configuration)
      (delete-other-windows)
      (switch-to-buffer "*scratch*")))
  (setq boss-mode (null boss-mode)))

(global-set-key (kbd "²") 'boss-toggle)
(global-set-key (kbd "C-²") 'boss-save)


(provide 'init-boss-key)
