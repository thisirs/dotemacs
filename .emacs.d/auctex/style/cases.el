(TeX-add-style-hook "cases"
                    (function
                     (lambda ()
                       (add-to-list 'font-latex-math-environments "subnumcases")
                       (LaTeX-add-environments
                        '("subnumcases" "Before")))))
