(install-when-compile 'rainbow-delimiters)

(setup-expecting "rainbow-delimiters"
  (add-hook 'smartparens-mode-hook #'rainbow-delimiters-mode))

(provide-file)
