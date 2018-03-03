(setup-include "recentf")
(setq make-backup-files nil)
(add-hook 'after-init-hook #'recentf-mode)
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard-manager t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defalias 'c-basic-offset 'tab-width)

(provide-file)
