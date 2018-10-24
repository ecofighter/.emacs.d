(install-when-compile 'smartparens)
(install-when-compile 'evil-smartparens)

(setup-expecting "smartparens"
  (add-hook 'after-init-hook #'smartparens-global-mode)
  (setup-expecting "evil-smartparens"
    (setup-after "evil"
      (evil-smartparens-mode 1))))


(provide-file)
