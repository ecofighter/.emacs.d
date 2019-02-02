;;; 10-hl-todo.el -- highlight keywords like TODO; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'hl-todo)

(add-hook 'after-init-hook #'global-hl-todo-mode)

(provide '10-hl-todo)
;;; 10-hl-todo.el ends here
