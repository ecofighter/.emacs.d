(install-when-compile 'rust-mode)
(install-when-compile 'racer)
(install-when-compile 'flycheck-rust)
(install-when-compile 'cargo)
(install-when-compile 'lsp-mode)
(install-when-compile 'company-lsp)
(install-when-compile 'lsp-rust)

(setup-lazy (rust-mode) "rust-mode"
  (setup "lsp-rust"
    (add-hook 'rust-mode-hook #'lsp-rust-enable)))
(setup-expecting "rust-mode"
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))
;; (setup-expecting "racer"
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode))
;; (setup-expecting "flycheck-rust"
;;   (add-hook 'rust-mode-hook #'flycheck-rust-setup))
;; (setup-expecting "cargo"
;;   (add-hook 'rust-mode-hook #'cargo-minor-mode))
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'company
    (setup "company-lsp")
    (push 'company-lsp company-backends))
  (setup "lsp-imenu")
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(provide-file)
