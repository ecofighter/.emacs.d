(install-when-compile 'proof-general)
(install-when-compile 'company-coq)
(with-eval-after-load "proof-general"
  (add-hook 'coq-mode-hook #'company-coq-mode))
(provide-file)
