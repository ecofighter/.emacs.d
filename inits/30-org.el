(install-when-compile 'org)
(install-when-compile 'org-evil)
(install-when-compile 'org-pomodoro)

(setup-after "org"
  (plist-put org-format-latex-options :scale 2.0)
  (setup-expecting "org-evil"
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              #'(lambda ()
                  (evil-org-set-key-theme)))
    (setup "evil-org-agenda"
      (evil-org-agenda-set-keys))))

(provide-file)
