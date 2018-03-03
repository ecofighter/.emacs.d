(install-when-compile 'company)

(setup-include "company"
  (global-company-mode 1)
  (setq company-selection-wrap-around t)
	(setup-include "company-capf")
	(setup-include "company-files")
	(setup-include "company-dabbrev")
  (setq company-backends '(company-capf company-files company-dabbrev))
	(setq company-minimum-prefix-length 2)
	(setq company-idle-delay 0.1)
  (add-to-list 'company-backends 'company-yasnippet t)
  (define-key company-active-map (kbd "C-i") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(provide-file)
