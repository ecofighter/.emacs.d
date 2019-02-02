(install-when-compile 'proof-general)

(with-eval-after-load "coq"
  (defun my/coq-mode-hooks ()
    (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "<tab>") 'company-select-next-if-tooltip-visible-or-complete-selection)
    (company-coq-initialize))
  (add-hook 'coq-mode-hook #'my/coq-mode-hooks))

(provide-file)
