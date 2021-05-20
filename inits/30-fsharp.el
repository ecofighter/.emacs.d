;;; 30-fsharp.el -- fsharp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'fsharp-mode)
(install-when-compile 'dotnet)

(add-hook 'fsharp-mode-hook #'dotnet-mode)
(add-hook 'fsharp-mode-hook #'highlight-indent-guides-mode)
;; (add-hook 'fsharp-mode-hook #'(lambda ()
;;                                 (setq tab-width 2)
;;                                 (setq fsharp-indent-offset 2)))
(with-eval-after-load "fsharp-mode"
  (require 'eglot-fsharp)
  (eval-after-load "company-mode"
    (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))
  (setq-default fsharp-indent-offset 4)
  (setq-default fsharp-indent-level 4))
;; (setq-default inferior-fsharp-program "fsharpi --readline-")
;; (setq-default fsharp-compiler "fsharpc"))

(provide '30-fsharp)
;;; 30-fsharp.el ends here
