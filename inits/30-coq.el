(install-when-compile 'company-coq)
(load "~/.emacs.d/lisp/PG/generic/proof-site")
(add-hook 'coq-mode-hook #'company-coq-mode)
(provide-file)
