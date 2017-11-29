(install-when-compile 'smartparens)
(install-when-compile 'evil-smartparens)

(setup-include "smartparens-config"
  (smartparens-global-mode 1))

(setup-include "evil-smartparens"
  (evil-smartparens-mode 1))

(provide-file)
