(install-when-compile 'yasnippet)

(setup-lazy '(yas-global-mode yas-minor-mode-on) "yasnippet")
(setup-expecting "yasnippet"
  (add-to-list 'company-backend 'company-yasnippet))

(provide-file)
