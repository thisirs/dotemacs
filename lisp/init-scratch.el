;; Put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n\n")

(defun scratch-message-qwertee ()
  (let* ((image1 (create-image "~/.emacs.d/var/qwertee/0.jpg"))
         (image2 (create-image "~/.emacs.d/var/qwertee/1.jpg"))
         (image3 (create-image "~/.emacs.d/var/qwertee/2.jpg"))
         (text "aaa"))
    (add-text-properties 0 1 `(display ,image1
                                       rear-nonsticky (display)
                                       keymap ,image-map)
                         text)
    (add-text-properties 1 2 `(display ,image2
                                       rear-nonsticky (display)
                                       keymap ,image-map)
                         text)
    (add-text-properties 2 3 `(display ,image3
                                       rear-nonsticky (display)
                                       keymap ,image-map)
                         text)
    (scratch-message-insert text)))

;; https://github.com/thisirs/scratch-message.git
(use-package scratch-message            ; Changing message in your scratch buffer
  :demand
  :straight `(scratch-message :type git
                              :local-repo ,(expand-file-name "scratch-message" projects-directory))
  :config
  (defun scratch-message-random ()
    (let ((funs '(scratch-message-print-quote
                  scratch-message-contrepet
                  scratch-message-qwertee)))
      (funcall (elt funs (random (length funs))))))

  (defun scratch-message-fortune ()
    (require 'fortune)
    (with-demoted-errors "Error: %S"
      (fortune-in-buffer t "/usr/share/games/fortunes/english-idioms")
      (scratch-message-insert
       (with-current-buffer fortune-buffer-name
         (buffer-string)))))

  (defun scratch-message-contrepet ()
    (require 'fortune)
    (with-demoted-errors "Error: %S"
      (fortune-in-buffer t "/usr/share/games/fortunes/contrepetries")
      (scratch-message-insert
       (with-current-buffer fortune-buffer-name
         (buffer-string)))))

  (defun scratch-message-print-quote ()
    (if (executable-find "print-quote")
        (let* ((message-buffer-name "*print-quote*")
               (message-buffer (or (get-buffer message-buffer-name)
                                   (generate-new-buffer message-buffer-name)))
               (proc (start-process "print-quote" message-buffer-name "print-quote" "SCMB")))
          (with-current-buffer message-buffer-name
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= "finished\n" event)
               (scratch-message-insert
                (with-current-buffer "*print-quote*"
                  (with-fill-line-by-line
                   (fill-region (point-min) (point-max)))
                  (buffer-string)))))))
      (user-error "print-quote not installed")))


  (defmacro with-fill-line-by-line (&rest body)
    "Executes BODY with line by line filling settings."
    `(let ((comment-start (or comment-start ""))
           (paragraph-start "^")
           (paragraph-separate "\n")
           (fill-prefix ""))
       (progn ,@body)))

  (setq scratch-message-function 'scratch-message-random)

  (scratch-message-mode))

(provide 'init-scratch)
