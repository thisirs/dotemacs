(defvar TeX-newthm-regexp
  '("\\\\newtheorem{\\([a-zA-Z]+\\)}"
    1 TeX-auto-multi)
  "Matches \newtheorem definitions.")

(defvar TeX-auto-multi nil
  "Temporary for parsing \\newmacro definitions.")

(defun TeX-macro-cleanup ()
  "Move symbols from `TeX-auto-multi' to `TeX-auto-symbol'."
  (mapcar (lambda (thmenv)
            (add-to-list 'reftex-env-or-mac-alist
                         `(,thmenv "x" ,(concat "\\\\begin{" thmenv "}\\|\\\\\\\\")
                                   ,(concat "\\label{" thmenv ":%s}") nil nil nil))

            (add-to-list 'reftex-label-env-list thmenv))
          TeX-auto-multi)
  (message "<<< In cleanup hook defun %s" TeX-auto-multi))


(defun TeX-macro-prepare ()
  "Clear `Tex-auto-multi' before use."
  (message "<<< In prepare hook defun %s" TeX-auto-multi)
  (setq TeX-auto-multi nil))

(add-hook 'TeX-auto-prepare-hook 'TeX-macro-prepare)
(add-hook 'TeX-auto-cleanup-hook 'TeX-macro-cleanup)

(TeX-add-style-hook
 "amsthm"
 (lambda ()
   (message "<<< hook")
   (add-to-list 'reftex-typekey-to-prefix-alist '("x" . ""))
   (TeX-auto-add-regexp TeX-newthm-regexp)))
