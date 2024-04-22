;;; early-init.el --- my early-init-file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024))
(setq garbage-collection-messages t)

(setq-default native-comp-async-report-warnings-errors 'silent)
(setq-default native-comp-async-jobs-number 4)
(setq-default native-comp-speed 2)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-redisplay t)
(setq inhibit-message t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-redisplay nil)
			(setq inhibit-message nil)
			(redisplay)))

(provide 'early-init)
;;; early-init.el ends here
