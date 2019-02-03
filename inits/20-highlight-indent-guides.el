;;; 20-highlight-indent-guides.el -- show indent levels; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'highlight-indent-guides)

(add-hook 'highlight-indent-guides-mode-hook #'highlight-indent-guides-auto-set-faces)
(setq-default highlight-indent-guides-method 'fill)

(provide '20-highlight-indent-guides)
;;; 20-highlight-indent-guides.el ends here
