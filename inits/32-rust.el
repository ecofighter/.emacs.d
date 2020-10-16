;;; 32-rust.el -- rust
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '31-lsp)
(install-when-compile 'rust-mode)
(install-when-compile 'cargo)

;; (autoload 'lsp "lsp-mode")
(add-hook 'rust-mode-hook
          #'(lambda ()
              (setq tab-width 4)
              (yas-minor-mode-on)
              ;; (highlight-indent-guides-mode)
              (require 'lsp-rust)
              (require 'lsp-ui)
              (flycheck-add-mode 'lsp-ui 'rust-mode)
              (lsp)))
              ;; (require 'company)
              ;; (set (make-local-variable 'company-backends)
              ;;      '((company-lsp company-yasnippet)))))

(with-eval-after-load "rust-mode"
  (setq-default rust-indent-offset 4))

(provide '32-rust)
;;; 32-rust.el ends here
