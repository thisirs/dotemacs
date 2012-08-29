(defun try-complete-flyspell (old)
  (when (not old)
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (setq he-next-expand 0)
    (he-init-string (he-dabbrev-beg) (point))
    ;; now check spelling of word.
    (ispell-send-string "%\n")        ;put in verbose mode
    (ispell-send-string (concat "^" he-search-string "\n"))
    ;; wait until ispell has processed word
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (if (consp ispell-filter)
        (setq he-expand-list (ispell-parse-output (car ispell-filter))
              he-expand-list (if (consp he-expand-list)
                                 (nth 2 (ispell-parse-output (car ispell-filter)))))))
  (while (and he-expand-list
              (or (not (car he-expand-list))
                  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

;;; hippie-expand
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        ;;try-expand-list
        ;;try-expand-line
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(global-set-key (kbd "S-SPC") 'hippie-expand)
(global-set-key (kbd "C-S-SPC") (lambda () (interactive) (hippie-expand -1)))

(provide 'init-hippie-expand)
