(defvar notify-send-sound-default
  "/usr/share/sounds/gnome/default/alert/sonar.ogg"
  "Son par défaut pour notify-send")

(defvar notify-send-icon-default
  "/usr/share/icons/gnome/scalable/status/appointment-missed.svg"
  "Icone par défault pour notify-send")

(defun notify-send (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"
  (interactive "sTitle: \nsMessage: ")
  (when sound
    (progn
      (when (eq t sound) (setq sound notify-send-sound-default))
      (shell-command (concat "mplayer " sound " 2> /dev/null"))))
  (when (eq window-system 'x)
    (when (eq t icon) (setq icon notify-send-icon-default))
    (shell-command (concat "notify-send "
		     (if icon (concat "-i " icon) "")
                     " '" title "' '" msg "'")))
    ;; text only version
    (message (concat title ": " msg)))

(provide 'notify-send)
