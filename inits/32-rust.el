;;; 32-rust.el -- rust
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'rust-mode)
(install-when-compile 'cargo)
(install-when-compile 'lsp-mode)
(install-when-compile 'lsp-ui)

(autoload 'lsp "lsp-mode")
(add-hook 'rust-mode-hook #'lsp)
(with-eval-after-load "rust-mode"
  (setq-default rust-indent-offset 4))

(provide '32-rust)
;;; 32-rust.el ends here
