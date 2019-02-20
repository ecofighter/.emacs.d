;;; 30-coq.el -- coq
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'proof-general)
(install-when-compile 'company-coq)

(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'coq-mode-hook #'(lambda ()
                               ;; (setq abbrev-expand-function #'ignore)
                               (setq proof-three-window-mode-policy 'hybrid)))

(provide '30-coq)
;;; 30-coq.el ends here
