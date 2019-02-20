;;; 00-init.el -- init setting; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; (setenv "LANG" "en_US.UTF-8")
;; (require 'recentf)
;; (add-hook 'after-init-hook #'recentf-mode)
(setq make-backup-files nil)
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard-manager t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq split-width-threshold 80)

(setq vc-handled-backends '())
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

(provide '00-init)
;;; 00-init.el ends here
