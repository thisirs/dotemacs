(defun org-sort-by-year ()
  "Return the first year found in the headline, \"0\" otherwise"
  (save-excursion
    (if (re-search-forward "[0-9]\\{4\\}" (line-end-position) t)
	(match-string-no-properties 0)
      "0")))


(defun note-org-bib-function (s)
  "Insert Org entry for a given Bibtex id."
  (if (equal (file-name-nondirectory  buffer-file-name) "notes.org")
      (let ((s0 (mapconcat 'identity (org-split-string s "[ \t\r\n]+") " "))
	    (pos (car (org-map-entries '(point)
				       (concat "BIB=\"" s "\"")))))
	(cond
	 (pos
          (goto-char pos)
          (null (show-children)))
	 (t
	  (y-or-n-p "No match - create as a new article note? ")
	  (null (let (year title author)
                  (with-current-buffer "refs.bib"
                    (or (bibtex-search-entry s)
			(error "No BibTeX entry for %s!" s))
                    (setq year (bibtex-text-in-field "year"))
                    (setq title (or (bibtex-text-in-field "title") "not found"))
                    (setq author (or (bibtex-text-in-field "author") "not found")))
                  (goto-char (point-max))
                  (or (bolp) (newline))
                  (insert "* "
			  (clean-authors author)
			  " - "
			  (if year (concat year " - ") "")
			  (replace-regexp-in-string "[{}]+" "" title) "\n"
			  "  :PROPERTIES:\n"
			  "  :BIB: " s "\n"
			  "  :END:\n"
			  "** Abstract\n"))))))))


(add-hook 'org-execute-file-search-functions 'note-org-bib-function)


(defun clean-authors (authors-string)
  "Take a BibTeX authors string and return comma separated surnames."
  (mapconcat
   (lambda (s)
     (let* ((names (split-string s "[, \f\t\n\r\v]+" t))
	    (name (mapcar
		   (lambda (ss)
		     (or
		      (numberp (string-match "\\([A-Z]\\.\\)+" ss))
		      ss))
		   names)))
       (cond
	;; Knuth, choosing Knuth
	((eq (length name) 1)
	 (elt name 0))
	;; D. Knuth or Knuth D., choosing Knuth
	((and (eq (length name) 2) (memq t name))
	 (if (eq t (elt name 0))
	     (elt name 1)
	   (elt name 0)))
	;; Donald Knuth, choosing Knuth
	((eq (length name) 2)
	 (elt name 1))
	;; choose last
	(t
	 (elt name 2)))))
   (split-string
    (with-temp-buffer
      (insert authors-string)
      (latex-escape-or-unescape-accented-characters t)
      (buffer-string))
    "\\<and\\>" t)
   ", "))


(provide 'org-bib-workflow)
