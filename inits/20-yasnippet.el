;;; 20-yasnippet.el --- yasnippet settings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'yasnippet)
(install-when-compile 'yasnippet-snippets)

(with-eval-after-load "yasnippet"
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "M-TAB") 'yas-expand))

(provide '20-yasnippet)
;;; 20-yasnippet.el ends here
