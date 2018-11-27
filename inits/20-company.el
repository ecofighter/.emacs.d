(install-when-compile 'company)

(setup-after "company"
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (setq company-selection-wrap-around t)
  (setq company-backends '(company-capf company-files company-dabbrev))
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)
  (define-key company-active-map (kbd "C-i") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(add-hook 'after-init-hook 'global-company-mode)

(provide-file)
