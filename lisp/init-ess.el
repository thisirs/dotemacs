(require 'ess-site)

(setq ess-swv-plug-into-AUCTeX-p t)

(defun ess-swv-add-TeX-commands ()
  "Add commands to AUCTeX's \\[TeX-command-list]."
  (unless (and (featurep 'tex-site) (featurep 'tex))
    (error "AUCTeX does not seem to be loaded"))
  (add-to-list 'TeX-command-list
               '("Knit" "Rscript -e \"library(knitr); knit('%t')\""
                 TeX-run-command nil (latex-mode) :help
                 "Run Knitr") t)
  (add-to-list 'TeX-command-list
               '("LaTeXKnit" "%l %(mode) %s"
                 TeX-run-TeX nil (latex-mode) :help
                 "Run LaTeX after Knit") t)
  (setq TeX-command-default "Knit")
  (mapc (lambda (suffix)
          (add-to-list 'TeX-file-extensions suffix))
        '("nw" "Snw" "Rnw")))

(defun ess-swv-remove-TeX-commands (x)
  "Helper function: check if car of X is one of the Knitr strings"
  (let ((swv-cmds '("Knit" "LaTeXKnit")))
    (unless (member (car x) swv-cmds) x)))

(provide 'init-ess)