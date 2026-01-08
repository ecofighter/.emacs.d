;;; early-init.el --- my early-init-file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setopt file-name-handler-alist nil)
(setopt gc-cons-threshold most-positive-fixnum)
(setopt garbage-collection-messages t)
(setopt inhibit-redisplay t)
(setopt inhibit-message t)
(setopt custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defun my/restore-variables-after-init ()
  "Restore variables changed in `early-init.el'."
  (setopt file-name-handler-alist my/saved-file-name-handler-alist)
  (setopt gc-cons-threshold (* 128 1024 1024))
  (setopt gc-cons-percentage 0.2)
  (setopt inhibit-redisplay nil)
  (setopt inhibit-message nil)
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
(push '(width . 130) default-frame-alist)
(push '(height . 40) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)
;; (push '(fullscreen . maximized) default-frame-alist)

(require 'package)
(setq package-enable-at-startup t)
(setq package-install-upgrade-built-in nil)
(when (fboundp 'native-comp-available-p)
  (when (native-comp-available-p)
    (setq package-native-compile t)))

(setenv "LSP_USE_PLISTS" "true")
(provide 'early-init)
;;; early-init.el ends here
