;;; early-init.el --- -*- lexical-binding: t; -*-

;; Don't garbage collect during startup; restore a sane threshold once
;; we are up.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

;; Strip UI chrome through `default-frame-alist' so that it is never
;; drawn in the first place. `display-graphic-p' is unreliable here as
;; no frame exists yet.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; Keep the mode variables consistent with the frame parameters above
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name (convert-standard-filename "var/eln-cache/")
                            user-emacs-directory)))

(setq package-enable-at-startup nil)
