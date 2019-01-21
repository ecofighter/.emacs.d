(install-when-compile 'lsp-mode)
(install-when-compile 'lsp-ui)

(setup-expecting "lsp-mode"
  (setq-default lsp-prefer-flymake nil))
(setup-after "lsp-ui"
  (setup "lsp-ui-flycheck"
    (add-hook 'lsp-ui-mode-hook #'(lambda ()
                                    (flycheck-select-checker 'lsp-ui)))
    (define-key lsp-ui-mode-map (kbd "C-c l") #'lsp-ui-flycheck-list))
  (define-key lsp-ui-mode-map (kbd "C-c d") #'lsp-execute-code-action)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(provide-file)
