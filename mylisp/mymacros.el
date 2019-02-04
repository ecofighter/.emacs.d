;;; mymacros.el --- my macros ; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'package)

(defvar *my/package-refreshed* nil)
(defmacro install-when-compile (package)
  "Install PACKAGE when compile."
  `(list
    (add-to-list 'package-selected-packages ,package)
    (eval-when-compile
      (progn
        (unless (package-installed-p ,package)
          (unless *my/package-refreshed*
            (package-refresh-contents)
            (setq *my/package-refreshed* t))
          (package-install ,package))))))

(provide 'mymacros)
;;; mymacros.el ends here
