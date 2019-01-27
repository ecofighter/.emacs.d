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
      (evil-org-agenda-set-keys)))
  (add-hook 'org-mode-hook
            #'(lambda ()
                (add-hook 'completion-at-point-functions
                          #'pcomplete-completions-at-point nil t))))

(with-eval-after-load 'org-clock
  (defun my:org-clock-out-and-save ()
    "save buffers and stop clock when clocking."
    (when (org-clocking-p)
      (org-clock-out)
      (save-some-buffers)))
  (add-hook 'kill-emacs-hook #'my:org-clock-out-and-save))

(provide-file)
