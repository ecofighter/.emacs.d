;;; 30-ocaml -- ocaml; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'tuareg)
(install-when-compile 'merlin)
(install-when-compile 'flycheck-ocaml)
(install-when-compile 'ocp-indent)
(install-when-compile 'utop)

(autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code." t)
(autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
(autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger." t)

(with-eval-after-load "tuareg"
  (require 'flycheck-ocaml)
  (add-hook 'tuareg-mode-hook #'flycheck-ocaml-setup)
  (add-hook 'tuareg-mode-hook #'ocp-setup-indent)
  (add-hook 'tuareg-mode-hook #'utop-minor-mode)
  (add-hook 'tuareg-mode-hook #'merlin-mode))

(with-eval-after-load "merlin"
  (require 'merlin-company)
  (setq-default merlin-error-after-save nil)
  (eval-after-load "company"
    (add-to-list 'company-backends 'merlin-company-backend)))

(eval-after-load "smart-parens-mode"
  (sp-local-pair (list 'tuareg-mode) "'" "'" :actions nil))

(provide '30-ocaml)
;;; 30-ocaml.el ends here
