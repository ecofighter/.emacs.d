;;; 30-purescript --- purescript settings
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'purescript-mode)
(install-when-compile 'psc-ide)

(add-hook 'purescript-mode-hook
          #'(lambda ()
              (psc-ide-mode)
              (company-mode-on)
              (flycheck-mode-on-safe)
              (turn-on-purescript-indentation)))

(provide '30-purescript)
;;; 30-purescript.el ends here
