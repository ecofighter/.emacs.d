(install-when-compile 'powerline)
(install-when-compile 'powerline-evil)

(setup-include "powerline")

(setup-include "powerline-evil"
  (powerline-evil-vim-color-theme))

(provide-file)
