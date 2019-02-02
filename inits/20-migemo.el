;;; 20-migemo.el -- search japanese string with Roman; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'migemo)

(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(add-hook 'after-init-hook #'migemo-init)

(provide '20-migemo)
;;; 20-migemo.el ends here
