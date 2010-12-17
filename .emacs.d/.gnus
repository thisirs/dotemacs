;; (setq mail-sources nil)

;; (setq gnus-nntp-server nil
;;   gnus-read-active-file nil
;;   gnus-save-newsrc-file nil
;;   gnus-read-newsrc-file nil
;;   gnus-check-new-newsgroups nil)

(setq user-mail-address "thisirs@gmail.com")

(setq user-full-name "Sylvain Rousseau")

;;(setq gnus-select-method '(nnml ""))

;;

;; make gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-select-method
  '(nnimap "gmail"
     (nnimap-address "imap.gmail.com")
     (nnimap-server-port 993)
     (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
  smtpmail-auth-credentials '(("smtp.gmail.com" 587 "thisirs@gmail.com" nil))
  smtpmail-default-smtp-server "smtp.gmail.com"
  smtpmail-smtp-server "smtp.gmail.com"
  smtpmail-smtp-service 587
  smtpmail-local-domain nil)

(setq gnus-permanently-visible-groups "gmail")
