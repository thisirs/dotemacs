;; (setq ispell-dictionary-base-alist nil)

;; english spell checking by default
(ignore-errors
  (ispell-change-dictionary "en" t))

(put 'ispell-local-dictionary 'safe-local-variable 'string-or-null-p)

;; save the personal dictionary without confirmation
(setq ispell-silently-savep t)

(when (and (executable-find "hunspell")
           (file-exists-p "~/.dictionary/fr-toutesvariantes.dic"))
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist
        `(("francais"
           "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[\"]"
           nil ("-d" ,(concat (getenv "HOME") "/.dictionary/fr-toutesvariantes")) nil utf-8))))

(require 'flyspell)

(defun switch-dictionary ()
  "Switch between en and fr dictionaries."
  (interactive)
  (ispell-change-dictionary
   (if (string=
        (or ispell-local-dictionary ispell-dictionary)
        "fr")
       "en" "fr"))
  (when flyspell-mode
    (flyspell-delete-all-overlays)
    (flyspell-buffer)))

;; flyspell comments and strings in programming modes
;; (preventing it from finding mistakes in the code)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'init-ispell)
