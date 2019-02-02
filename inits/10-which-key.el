;;; 10-which-key.el -- show key bindings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'which-key)

(add-hook 'after-init-hook #'(lambda ()
                               (which-key-mode 1)
                               (which-key-setup-side-window-bottom)))

(provide '10-which-key)
;;; 10-which-key.el ends here
