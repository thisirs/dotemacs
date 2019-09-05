(use-package wcheck-mode                ; General interface for text checkers
  :functions (wcheck-query-language-data wcheck-parser-ispell-suggestions)
  :bind (("C-c w w" . wcheck-mode)
         ("C-c w f" . wcheck-fr)
         ("C-c w e" . wcheck-en))
  :config
  (setq wcheck-language-data
        '(("fr"
           (program . "/usr/bin/enchant")
           (args "-l" "-d" "fr_FR")
           (action-program . "/usr/bin/enchant")
           (action-args "-a" "-d" "fr_FR")
           (action-parser . enchant-suggestions-menu))
          ("en"
           (program . "/usr/bin/enchant")
           (args "-l" "-d" "en_GB")
           (action-program . "/usr/bin/enchant")
           (action-args "-a" "-d" "en_GB")
           (action-parser . enchant-suggestions-menu))
          ("us"
           (program . "/usr/bin/enchant")
           (args "-l" "-d" "us_US")
           (action-program . "/usr/bin/enchant")
           (action-args "-a" "-d" "us_US")
           (action-parser . enchant-suggestions-menu))))

  (put 'wcheck-language 'safe-local-variable 'stringp)
  (setq wcheck-language "en")

  (defun wcheck-en ()
    (interactive)
    (wcheck-mode 1)
    (wcheck-change-language "en"))

  (defun wcheck-fr ()
    (interactive)
    (wcheck-mode 1)
    (wcheck-change-language "fr"))

  (defun enchant-suggestions-menu (marked-text)
    (cons (cons "[Add to dictionary]" 'enchant-add-to-dictionary)
          (wcheck-parser-ispell-suggestions)))

  (defvar enchant-dictionaries-dir "~/.config/enchant")

  (defun enchant-add-to-dictionary (marked-text)
    (let* ((word (aref marked-text 0))
           (language (aref marked-text 4))
           (file (let ((code (nth 1 (member "-d" (wcheck-query-language-data
                                                  language 'action-args)))))
                   (when (stringp code)
                     (concat (file-name-as-directory enchant-dictionaries-dir)
                             code ".dic")))))

      (when (and file (file-writable-p file))
        (with-temp-buffer
          (insert word) (newline)
          (append-to-file (point-min) (point-max) file)
          (message "Added word \"%s\" to the %s dictionary"
                   word language))))))

(provide 'init-wcheck)

;;; init-wcheck.el ends here
