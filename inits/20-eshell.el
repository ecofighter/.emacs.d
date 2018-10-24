(install-when-compile 'eshell)

(setup-expecting "eshell"
  (setup-expecting "evil-leader"
    (evil-leader/set-key
      "'" 'eshell)))

(provide-file)
