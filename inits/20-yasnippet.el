(install-when-compile 'yasnippet)

(use-package yasnippet
	:defer t
	:commands (yas-global-mode yas-minor-mode-on)
	:config
	(add-to-list 'company-backend 'company-yasnippet))

(provide-file)
