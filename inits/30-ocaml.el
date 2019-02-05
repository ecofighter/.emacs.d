;;; 30-ocaml -- ocaml; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'tuareg)
(install-when-compile 'merlin)
(install-when-compile 'flycheck-ocaml)
(install-when-compile 'ocp-indent)
(install-when-compile 'utop)

(delete '("\\.ml\\'" . lisp-mode) auto-mode-alist)
(add-hook 'tuareg-mode-hook #'flycheck-ocaml-setup)
(add-hook 'tuareg-mode-hook #'ocp-setup-indent)
(add-hook 'tuareg-mode-hook #'utop-minor-mode)
(add-hook 'tuareg-mode-hook #'merlin-mode)

(with-eval-after-load "merlin"
  (setq-default merlin-error-after-save nil)
  (with-eval-after-load "company"
    (require 'merlin-company)
    (require 'yasnippet)
    (require 'company-yasnippet)
    (add-to-list 'company-backends 'merlin-company-backend)))

(eval-after-load "smart-parens-mode"
  (sp-local-pair (list 'tuareg-mode) "'" "'" :actions nil))

;; Settings for Dune Builde Manager
(add-to-list 'load-path
             (substitute-in-file-name "$OPAM_SWITCH_PREFIX/share/emacs/site-lisp"))
(autoload 'dune-mode "dune")
(add-to-list 'auto-mode-alist '("\\.dune\\'" . dune-mode))
(add-hook 'dune-mode-hook #'(lambda ()
                              (require 'flycheck)
                              (require 'dune-flymake)
                              (flycheck-mode -1)
                              (flymake-mode-on)
                              (dune-flymake-init)))

(provide '30-ocaml)
;;; 30-ocaml.el ends here
