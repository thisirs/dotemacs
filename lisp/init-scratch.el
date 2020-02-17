;; Put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n\n")

(defun scratch-message-qwertee ()
  (let* ((image1 (create-image "~/.emacs.d/cache/0.jpg"))
         (image2 (create-image "~/.emacs.d/cache/1.jpg"))
         (image3 (create-image "~/.emacs.d/cache/2.jpg"))
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
  :config
  (defun scratch-message-random ()
    (pcase (random 5)
      (0 (scratch-message-contrepet))
      (1 (scratch-message-SCMB))
      (2 (scratch-message-DTC))
      (3 (scratch-message-fortune))
      (4 (scratch-message-qwertee))))

  (defun scratch-message-fortune ()
    (require 'fortune)
    (fortune-in-buffer t "/usr/share/fortune/english-idioms")
    (scratch-message-insert
     (with-current-buffer fortune-buffer-name
       (buffer-string))))

  (defun scratch-message-contrepet ()
    (require 'fortune)
    (with-demoted-errors "Error: %S"
      (fortune-in-buffer t "/usr/share/fortune/contrepetries")
      (scratch-message-insert
       (with-current-buffer fortune-buffer-name
         (buffer-string)))))

  (defun scratch-message-DTC ()
    (if (executable-find "ruby")
        (let* ((message-buffer-name "*DTC*")
               (message-buffer (or (get-buffer message-buffer-name)
                                   (generate-new-buffer message-buffer-name)))
               (proc (start-process "DTC" message-buffer-name "ruby" "DTC")))
          (with-current-buffer message-buffer-name
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= "finished\n" event)
               (scratch-message-insert
                (with-current-buffer "*DTC*"
                  (with-fill-line-by-line
                   (fill-region (point-min) (point-max)))
                  (buffer-string)))))))
      (user-error "Ruby is not installed")))

  (defun scratch-message-SCMB ()
    (if (executable-find "ruby")
        (let* ((message-buffer-name "*SCMB*")
               (message-buffer (or (get-buffer message-buffer-name)
                                   (generate-new-buffer message-buffer-name)))
               (proc (start-process "SCMB" message-buffer-name "ruby" "SCMB")))
          (with-current-buffer message-buffer-name
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= "finished\n" event)
               (scratch-message-insert
                (with-current-buffer "*SCMB*"
                  (with-fill-line-by-line
                   (fill-region (point-min) (point-max)))
                  (buffer-string)))))))
      (user-error "Ruby is not installed")))

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
