;;; 20-yasnippet.el --- yasnippet settings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'yasnippet)
(install-when-compile 'yasnippet-snippets)

(eval-after-load "company"
  (with-eval-after-load "yasnippet"
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "M-TAB") 'yas-expand)
    (add-hook 'yas-minor-mode-hook
              #'(lambda ()
                  (add-to-list 'company-backends 'company-yasnippet)))))

(provide '20-yasnippet)
;;; 20-yasnippet.el ends here
