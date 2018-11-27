(install-when-compile 'fsharp-mode)

(setup-after "fsharp-mode"
  (setup-after "company-mode"
    (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))
  (setq-default fsharp-indent-offset 2)
  (setq-default fsharp-indent-level 2)
  (setq inferior-fsharp-program "fsi")
  (setq fsharp-compiler "fsc"))

(provide-file)
