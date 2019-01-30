(install-when-compile 'company)

(with-eval-after-load "company"
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (setq company-selection-wrap-around t)
  (setq company-backends '(company-capf company-files company-dabbrev-code))
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (setq company-selection-wrap-around t)
  ;; (define-key company-active-map (kbd "C-i") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(add-hook 'after-init-hook 'global-company-mode)

(provide-file)
