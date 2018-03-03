(install-when-compile 'rust-mode)
(install-when-compile 'racer)
(install-when-compile 'flycheck-rust)
(install-when-compile 'cargo)

(setup-lazy (rust-mode) "rust-mode")
(setup-expecting "rust-mode"
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))
(setup-expecting "racer"
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))
(setup-expecting "flycheck-rust"
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))
(setup-expecting "cargo"
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(provide-file)
