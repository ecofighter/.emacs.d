;;; 30-emacslisp.el --- my settings to write emacs-lisp; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(with-eval-after-load "company"
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda ()
                (yas-minor-mode)
                (add-to-list (make-local-variable 'company-backends) '(company-yasnippet company-elisp)))))

(with-eval-after-load "flycheck"
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(provide '30-emacslisp)
;;; 30-emacslisp.el ends here
