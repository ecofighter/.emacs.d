(install-when-compile 'which-key)

(setup-lazy '(which-key-mode) "which-key"
	:prepare (add-hook 'after-init-hook #'which-key-mode)
	(which-key-setup-side-window-bottom))


(provide-file)
