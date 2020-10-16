;;; 32-scala.el -- scala configs
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '31-lsp)
(install-when-compile 'scala-mode)

(add-hook 'scala-mode-hook #'(lambda ()
                               (lsp)))

(provide '32-scala)
;;; 32-scala.el ends here
