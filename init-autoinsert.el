;; Using modified version of autoinsert to allow multiple autoinsert
;; https://github.com/thisirs/auto-insert-multiple.git

(require 'autoinsert)

;; Adds hook to find-files-hook
(auto-insert-mode t)

(setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/"))
(setq auto-insert-query nil)

(setq auto-insert-alist
      '(
        ("\\.rb$"  "Ruby shebang" (auto-insert-yasnippet "shebang"))
        ("\\.sh$" "Bash shebang" (auto-insert-yasnippet "shebang"))
        ("\\.tex$"
         ("Latex article"
          (progn
            (auto-insert-yasnippet "headerlatex")
            (TeX-normal-mode 1)))
         ("Standalone TikZ"
          (progn
            (auto-insert-yasnippet "headerTS")
            (TeX-normal-mode 1)))
         ("Letter"
          (progn
            (auto-insert-yasnippet "ll")
            (TeX-normal-mode 1))))))

(setq auto-insert 'other)

(provide 'init-autoinsert)
