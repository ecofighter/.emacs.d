(install-when-compile 'markdown-mode)

(setup-expecting "markdown-mode"
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(setup-after "markdown-mode"
  (setq markdown-command "pandoc -f markdown -t html5 --template=GitHub.html5 -s --self-contained")
  (setq markdown-open-command "firefox --new-window"))

(provide-file)
