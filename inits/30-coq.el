;;; 30-coq.el -- coq
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'proof-general)
(install-when-compile 'company-coq)

(eval-after-load "company"
  (with-eval-after-load "coq"
    (defun my/coq-mode-hooks ()
      (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
      (define-key company-active-map (kbd "<tab>") 'company-select-next-if-tooltip-visible-or-complete-selection)
      (company-coq-initialize))
    (add-hook 'coq-mode-hook #'my/coq-mode-hooks)))

(provide '30-coq)
;;; 30-coq.el ends here
