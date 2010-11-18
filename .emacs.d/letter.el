(defvar letter-templates-location nil
  "Location of letter templates.")

(defvar letter-location nil
  "Location of letters")


(setq letter-templates-location "/media/THISKEY/Documents/lettre/modèles")

(setq letter-location "/media/THISKEY/Documents/lettre")

(defun find-letter ()
  (interactive)
  (let ((template (completing-read "Choose letter template: "
    (directory-files
      letter-templates-location
      nil
      "\.tex$")))
	 (name (read-from-minibuffer "Choose a letter name: ")))
    (make-directory (concat letter-location "/" name))
    (copy-file (concat letter-templates-location "/" template)
      (concat letter-location "/" name "/" name ".tex"))
    (find-file (concat letter-location "/" name "/" name ".tex"))))

(provide 'letter)
