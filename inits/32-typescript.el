;;; 32-typescript -- typescript setting
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'typescript-mode)

(add-hook 'typescript-mode-hook
          #'(lambda ()
              (setq tab-width 4)
              (company-mode-on)
              (yas-minor-mode-on)
              (flycheck-mode-on-safe)
              (lsp)))

(provide '32-typescript)
;;; 32-typescript.el ends here
