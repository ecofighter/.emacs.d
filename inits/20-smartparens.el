;;; 20-smartparens.el -- edit with balanced parens; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'smartparens)
(install-when-compile 'evil-smartparens)

(add-hook 'after-init-hook #'smartparens-global-mode)
(add-hook 'after-init-hook #'show-smartparens-global-mode)
(eval-after-load "evil"
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(with-eval-after-load "smartparens"
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "'" nil :actions nil))
  (defun my/sp-wrap-dquote ()
    (interactive)
    (sp-wrap-with-pair "\""))
  (with-eval-after-load "evil-leader"
    (evil-leader/set-key
      "p f s" 'sp-forward-slurp-sexp
      "p f b" 'sp-forward-barf-sexp
      "p b s" 'sp-backward-slurp-sexp
      "p b b" 'sp-backward-barf-sexp
      "p b u" 'sp-backward-unwrap-sexp
      "p w (" 'sp-wrap-round
      "p w [" 'sp-wrap-square
      "p w {" 'sp-wrap-curly
      "p w \"" 'my/sp-wrap-dquote)))

(provide '20-smartparens)
;;; 20-smartparens.el ends here
