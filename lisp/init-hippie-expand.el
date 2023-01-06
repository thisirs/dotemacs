(require 'hippie-exp)

(defun try-complete-wcheck (old)
  (when (bound-and-true-p wcheck-mode)
    (when (not old)
      (he-init-string (he-dabbrev-beg) (point))
      (let ((marked-text
             (vector he-search-string (he-dabbrev-beg) (point) nil "fr")))
        (setq he-search-loc2 0
              he-expand-list
              (cdr (mapcar #'cdr
                           (wcheck--get-actions marked-text)))))))

  (while (and he-expand-list
              (or (not (car he-expand-list))
                  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun try-complete-flyspell (old)
  (when (not old)
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (setq he-search-loc2 0)
    (he-init-string (he-dabbrev-beg) (point))
    ;; now check spelling of word.
    (ispell-send-string "%\n")        ;put in verbose mode
    (ispell-send-string (concat "^" he-search-string "\n"))
    ;; wait until ispell has processed word
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (if (consp ispell-filter)
        (setq he-expand-list (ispell-parse-output (car ispell-filter))
              he-expand-list (if (consp he-expand-list)
                                 (nth 2 (ispell-parse-output (car ispell-filter)))))))
  (while (and he-expand-list
              (or (not (car he-expand-list))
                  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun try-complete-ispell (old)
  (when (not old)
    (require 'ispell)
    (he-init-string (he-dabbrev-beg) (point))
    (ispell-set-spellchecker-params)    ; Initialize variables and dicts alists
    (ispell-accept-buffer-local-defs)   ; use the correct dictionary
    (ispell-send-string "%\n")  ; put in verbose mode
    (ispell-send-string (concat "^" he-search-string "\n"))
    (while (progn
             (ispell-accept-output)
             (not (string= "" (car ispell-filter)))))
    (setq ispell-filter (cdr ispell-filter)) ; remove extra \n

    (or ispell-filter
        (setq ispell-filter '(*)))
    (if (consp ispell-filter)
        (setq he-expand-list (ispell-parse-output (car ispell-filter))
              he-expand-list (if (consp he-expand-list)
                                 (nth 2 (ispell-parse-output (car ispell-filter)))))))

  (while (and he-expand-list
              (or (not (car he-expand-list))
                  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

;; From https://gist.github.com/magnars/4060654
(defvar he-search-loc-backward (make-marker))
(defvar he-search-loc-forward (make-marker))

(defun try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string). It returns t if a new expansion is found, nil otherwise."
  (let (expansion)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-dabbrev-search he-search-string t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-dabbrev-search he-search-string nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          nil)
      (progn
        (he-substitute-string expansion t)
        t))))

;;; hippie-expand
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        ;;try-expand-list
        ;;try-expand-line
        try-expand-dabbrev-closest-first
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-ispell))

(add-to-list 'hippie-expand-ignore-buffers "^.*\\.gpg$")

(global-set-key (kbd "S-SPC") #'hippie-expand)
(global-set-key (kbd "C-S-SPC") (lambda () (interactive) (hippie-expand -1)))

(provide 'init-hippie-expand)
