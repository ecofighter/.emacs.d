(install-when-compile 'magit)

(setup-expecting "magit"
  (setup-expecting "evil-leader"
    (global-set-key (kbd "C-x g") 'magit-status)))

(provide-file)
