(install-when-compile 'tuareg)
(install-when-compile 'merlin)
(install-when-compile 'flycheck-ocaml)
(install-when-compile 'ocp-indent)
(install-when-compile 'utop)


(setup-expecting "tuareg"
  (add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mlp\\'" . tuareg-mode)))

(setup-expecting "merlin"
  (setup-expecting "tuareg"
    (add-hook 'tuareg-mode-hook #'merlin-mode)))
(setup-after "merlin"
  (setup "merlin-company")
  (setup-expecting "flycheck-ocaml"
    (setq merlin-error-after-save nil)
    (flycheck-ocaml-setup))
  (setup-after "company"
    (add-to-list 'company-backends 'merlin-company-backend)))
(setup-expecting "utop"
  (setup-expecting "tuareg"
    (add-hook 'tuareg-mode-hook #'utop-minor-mode)))

(setup-expecting "ocp-indent"
  (add-hook 'tuareg-mode-hook #'ocp-setup-indent))

(setup-after "smartparens"
  (setup-expecting "tuareg"
    (sp-local-pair '(tuareg-mode) "'" "'" :actions nil)))
(provide-file)
