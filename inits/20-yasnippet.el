;;; 20-yasnippet.el --- yasnippet settings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(install-when-compile 'yasnippet)
(install-when-compile 'yasnippet-snippets)

(with-eval-after-load "company"
  (with-eval-after-load "yasnippet"
    (define-key yas-minor-mode-map (kbd "M-TAB") 'yas-expand)
    (add-hook 'yas-global-mode-hook
              #'(lambda ()
                  (add-to-list 'company-backends 'company-yasnippet)))))

(provide '20-yasnippet)
;;; 20-yasnippet.el ends here
