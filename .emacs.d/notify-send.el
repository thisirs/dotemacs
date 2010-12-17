(defvar notify-send-sound-default
  "/usr/share/sounds/gnome/default/alert/sonar.ogg"
  "Son par défaut pour notify-send")

(defvar notify-send-icon-default
  "/usr/share/icons/gnome/scalable/status/appointment-missed.svg"
  "Icone par défault pour notify-send")

(defun notify-send (title &optional msg icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"
  (interactive "sTitle: \nsMessage: ")
  (and sound
    (start-process-shell-command
      "mplayer"
      nil
      (concat
	"mplayer "
	(if (stringp sound)
	  sound notify-send-sound-default)
	" 2>&1 /dev/null")))
  (or (stringp msg) (setq msg title))
  (and (eq window-system 'x)
    (start-process-shell-command
      "notify-send"
      nil
      (concat "notify-send -i "
	(if (stringp icon)
	  icon notify-send-icon-default)
	" '" title "' '" msg "'"))))

(provide 'notify-send)
