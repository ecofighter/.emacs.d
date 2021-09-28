;;; 32-c++.el -- settings to write C++; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'mymacros))
(install-when-compile 'meson-mode)
(install-when-compile 'ccls)
(require '20-yasnippet)
(require '31-lsp)

(dolist (hook '(c++-mode-hook c-mode-hook))
  (add-hook hook #'(lambda ()
                     (highlight-indent-guides-mode)
                     (require 'lsp-clangd)
                     ;; (require 'lsp)
                     ;; (require 'lsp-clients)
                     ;; (setq-default lsp-clients-clangd-args '("-compile-commands-dir=build"))
                     (lsp)
                     (dap-mode 1)
                     (dap-ui-mode 1))))

(dolist (hook '(c++-mode-hook c-mode-hook))
  (add-hook hook #'yas-minor-mode-on))

;; (dolist (hook '(c++-mode-hook c-mode-hook))
;;   (add-hook hook #'flycheck-mode-on-safe))

(provide '32-c++)
;;; 32-c++.el ends here
