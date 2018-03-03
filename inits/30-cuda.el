(install-when-compile 'cuda-mode)

(setup-lazy '(cuda-mode) "cuda-mode")
(setup-expecting "cuda-mode"
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode)))

(provide-file)
