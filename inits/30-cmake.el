(install-when-compile 'cmake-mode)
(setup-expecting "cmake-mode"
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist)))

(provide-file)
