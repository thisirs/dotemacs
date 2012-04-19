;; allow midnight to nuke old buffers though the sessions
(add-to-list 'desktop-locals-to-save 'buffer-display-time)

(require 'midnight)
(cancel-timer midnight-timer)

;; nuke old buffer after running emacs
(run-with-idle-timer 10 nil
                     (lambda ()
                       (run-hooks 'midnight-hook)))

(provide 'init-midnight)
