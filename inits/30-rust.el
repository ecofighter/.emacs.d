(install-when-compile 'rust-mode)
(install-when-compile 'cargo)
(install-when-compile 'lsp-mode)
(install-when-compile 'lsp-ui)
(install-when-compile 'company-lsp)
;; (install-when-compile 'lsp-rust)

(setup-expecting "lsp-mode"
  (add-hook 'rust-mode-hook #'lsp))
(setup-after "flycheck"
  (setup-after "lsp-ui-flycheck"
    (flycheck-add-mode 'lsp-ui 'rust-mode)))

(provide-file)
