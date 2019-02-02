;;; 20-highlight-indent-guides.el -- show indent levels; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'highlight-indent-guides)

(setq-default highlight-indent-guides-method 'column)
(add-hook 'after-init-hook #'highlight-indent-guides-mode)

(provide '20-highlight-indent-guides)
;;; 20-highlight-indent-guides.el ends here
