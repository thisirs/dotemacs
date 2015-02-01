(eval-after-load "octave"
  '(progn
     (defun octave-send-buffer ()
       (interactive)
       (octave-send-region (point-min) (point-max)))

     (define-key octave-mode-map "\C-c\C-r" 'octave-send-region)

     (define-key octave-mode-map "\C-c\C-s" 'octave-send-buffer)))

(provide 'init-octave)
