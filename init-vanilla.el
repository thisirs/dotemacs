;; Vanilla emacs + features
;; Example: emacs -Q -l ~/repositories/dotemacs/init-vanilla-org.el
;; --eval (vanilla 'org)

(defun vanilla (&rest features)
  (if (symbolp features)
      (setq features (list features)))
  (dolist (feat features)
    (funcall (intern (concat "vanilla-"
                             (if (symbolp feat)
                                 (symbol-name feat)
                               feat))))))


(defun vanilla-org ()
  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
  (require 'org))
