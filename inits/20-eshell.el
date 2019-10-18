;;; 20-eshell.el -- eshell; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'eshell)

(with-eval-after-load "evil-leader"
  (evil-leader/set-key
    "'" 'eshell))

(provide '20-eshell)
;;; 20-eshell ends here
