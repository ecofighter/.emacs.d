(install-when-compile 'markdown-mode)
(setup-lazy '(gfm-mode markdown-mode) "markdown-mode")

(setup-expecting "markdown-mode"
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(provide-file)
