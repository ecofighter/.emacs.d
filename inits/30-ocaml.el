;;; 30-ocaml -- ocaml; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'tuareg)
(install-when-compile 'merlin)
(install-when-compile 'flycheck-ocaml)
(install-when-compile 'ocp-indent)
(install-when-compile 'utop)

(eval-after-load "tuareg"
  (add-hook 'tuareg-mode-hook #'merlin-mode))

(with-eval-after-load "merlin"
  (require 'merlin-company)
  (require 'flycheck-ocaml)
  (setq-default merlin-error-after-save nil)
  (add-hook 'tuareg-mode-hook #'flycheck-ocaml-setup)
  (eval-after-load "company"
    (add-to-list 'company-backends 'merlin-company-backend)))
(add-hook 'tuareg-mode-hook #'utop-minor-mode)

(add-hook 'tuareg-mode-hook #'ocp-setup-indent)

(let ((ok (require 'smartparens nil t)))
  (when ok
    (sp-local-pair (list 'tuareg-mode) "'" "'" :actions nil)))

(provide '30-ocaml)
;;; 30-ocaml.el ends here
