;;; early-init.el --- my early-init-file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(setq garbage-collection-messages t)
(setq inhibit-redisplay t)
(setq inhibit-message t)

(defun my/restore-variables-after-init ()
  "Restore variables changed in `early-init.el'."
  (setq file-name-handler-alist my/saved-file-name-handler-alist)
  (setq gc-cons-threshold (* 128 1024 1024))
  (setq gc-cons-percentage 0.2)
  (setq inhibit-redisplay nil)
  (setq inhibit-message nil)
  (redisplay)
  (garbage-collect))
(add-hook 'after-init-hook #'my/restore-variables-after-init)

(setq load-prefer-newer t)
(setq read-process-output-max (* 1024 1024))
(setq-default native-comp-async-report-warnings-errors 'silent)
(setq-default native-comp-async-jobs-number 8)
(setq-default native-comp-speed 2)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)
;; (push '(fullscreen . maximized) default-frame-alist)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-install-upgrade-built-in t)
(when (fboundp 'native-comp-available-p)
  (when (native-comp-available-p)
    (setq package-native-compile t)))

(setenv "LSP_USE_PLISTS" "true")
(provide 'early-init)
;;; early-init.el ends here
