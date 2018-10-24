(install-when-compile 'which-key)

(setup-expecting "which-key"
  (add-hook 'after-init-hook #'(lambda ()
                                 (which-key-mode 1)
                                 (which-key-setup-side-window-bottom))))

(provide-file)
