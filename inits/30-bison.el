;;; 30-bison.el -- to write bison files; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'bison-mode)

(autoload 'bison-mode "bison-mode")

(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . bison-mode))

(provide '30-bison)
;;; 30-bison.el ends here
