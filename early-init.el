;;; early-init.el --- loaded before the GUI frame and package system -*- lexical-binding: t -*-

;; Avoid GC churn during startup; restored to sane values after init (see init.el end).
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Skip regexp file-handler matching (tramp, jka-compr...) while loading init files.
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist my--file-name-handler-alist)))
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

;; straight.el manages packages; don't let package.el initialize.
(setq package-enable-at-startup nil)

;; straight.el: don't `find' across every repo at startup (cost ~2s).
;; Edits to repos made from within Emacs are still detected on save;
;; after a `git pull' outside Emacs use M-x straight-rebuild-package / straight-rebuild-all.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Configure the initial frame up front instead of resizing/redrawing it later.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
(push '(height . 58) default-frame-alist)
(push '(width . 120) default-frame-alist)
(push '(font . "Source Code Pro-13:weight=medium") default-frame-alist)
(setq tool-bar-mode nil
      scroll-bar-mode nil)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
