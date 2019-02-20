;;; 32-rust.el -- rust
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '31-lsp)
(install-when-compile 'rust-mode)
(install-when-compile 'cargo)

;; (autoload 'lsp "lsp-mode")
(add-hook 'rust-mode-hook #'(lambda ()
                              (highlight-indent-guides-mode)
                              (lsp)
                              (flycheck-add-mode 'lsp-ui 'rust-mode)
                              (set (make-local-variable 'company-backends)
                                   '((company-lsp company-yasnippet company-dabbrev-code)))))

(with-eval-after-load "rust-mode"
  (setq-default rust-indent-offset 4))

(provide '32-rust)
;;; 32-rust.el ends here
