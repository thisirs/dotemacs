(TeX-add-style-hook
 "tikz"
 (lambda ()
   (TeX-add-symbols
    '("tikz"))
   (LaTeX-add-environments
    '("tikzpicture"))))
