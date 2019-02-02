;;; 30-fsharp.el -- fsharp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'fsharp-mode)

(with-eval-after-load "fsharp-mode"
  (eval-after-load "company-mode"
    (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))
  (setq-default fsharp-indent-offset 2)
  (setq-default fsharp-indent-level 2)
  (setq-default inferior-fsharp-program "fsi")
  (setq-default fsharp-compiler "fsc"))

(provide '30-fsharp)
;;; 30-fsharp.el ends here
