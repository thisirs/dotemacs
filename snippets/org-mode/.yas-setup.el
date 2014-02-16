(defun org-outline-level-visible ()
  "Compute the outline level of the heading at point.
If this is called at a normal headline, the level is the number of stars.
Use `org-reduced-level' to remove the effect of `org-odd-levels'."
  (save-excursion
    (if (not (condition-case nil
                 (org-back-to-heading nil)
               (error nil)))
        0
      (looking-at org-outline-regexp)
      (1- (- (match-end 0) (match-beginning 0))))))
