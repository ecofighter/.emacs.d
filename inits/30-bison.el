(install-when-compile 'bison-mode)

(setup-lazy '(bison-mode jison-mode) "bison-mode")

(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.jison\\'" . jison-mode))

(setup-after "bison-mode"
  (add-to-list 'bison-mode-hook #'(lambda () (irony-mode -1))))

(provide-file)
