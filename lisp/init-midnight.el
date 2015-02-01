(require 'midnight)
(cancel-timer midnight-timer)

;; Nuke old buffer after running emacs
(run-with-idle-timer 20 nil
                     (lambda ()
                       (run-hooks 'midnight-hook)))

(provide 'init-midnight)
