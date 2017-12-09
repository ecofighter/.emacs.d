(install-when-compile 'tuareg)
(install-when-compile 'merlin)
;; (install-when-compile 'utop)

(setup-lazy '(tuareg-mode) "tuareg")
(setup-expecting "tuareg"
  (add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mlp\\'" . tuareg-mode)))

(setup-lazy '(merlin-mode) "merlin"
  (setup "merlin-company"))
(setup-expecting "merlin"
  (setup-expecting "tuareg"
    (add-hook 'tuareg-mode-hook #'merlin-mode)))

;; (setup-lazy '(utop-minor-mode) "utop")
;; (setup-expecting "utop"
;;   (setup-expecting "tuareg"
;;     (add-hook 'tuareg-mode-hook #'utop-minor-mode)))

(provide-file)
