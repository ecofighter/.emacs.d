(install-when-compile 'rainbow-delimiters)

(setup-include "rainbow-delimiters"
  (add-hook 'smartparens-mode-hook #'rainbow-delimiters-mode))

(provide-file)
