(install-when-compile 'rg)
(setup-expecting "rg"
  (evil-leader/set-key
    "g r" 'rgrep
    "g p" 'rg-project
    "g d" 'rg-dwim-project-dir
    "g l" 'rg-list-searches))

(provide-file)
