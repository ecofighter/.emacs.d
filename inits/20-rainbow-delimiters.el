;;; 20-rainbow-delimiters.el -- Colorize parens; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'rainbow-delimiters)

(add-hook 'smartparens-enabled-hook #'rainbow-delimiters-mode)

(provide '20-rainbow-delimiters)
;;; 20-rainbow-delimiters.el ends here
