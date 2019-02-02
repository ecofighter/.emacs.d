;;; 20-smartparens.el -- edit with balanced parens; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'smartparens)
(install-when-compile 'evil-smartparens)

(add-hook 'after-init-hook #'smartparens-global-mode)
(eval-after-load "evil"
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(provide '20-smartparens)
;;; 20-smartparens.el ends here
