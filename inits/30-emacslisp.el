;;; 30-emacslisp.el --- my settings to write emacs-lisp; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(with-eval-after-load "company"
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda ()
                (flycheck-mode 1)
                (require 'yasnippet)
                (yas-minor-mode)
                (add-to-list (make-local-variable 'company-backends) '(company-yasnippet company-elisp)))))

(provide '30-emacslisp)
;;; 30-emacslisp.el ends here
