;;; 30-c++.el -- settings to write C++; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'mymacros))
(install-when-compile 'cmake-ide)
(install-when-compile 'ccls)

(dolist (hook '(c++-mode-hook c-mode-hook))
  (add-hook hook #'(lambda ()
                     (highlight-indent-guides-mode)
                     (setq-default cmake-ide-build-dir "build")
                     (cmake-ide-setup)
                     (require 'ccls)
                     (lsp)
                     (flycheck-add-mode 'lsp-ui 'c-mode)
                     (flycheck-add-mode 'lsp-ui 'c++-mode)
                     (set (make-local-variable 'company-backends)
                          '((company-lsp company-yasnippet company-dabbrev-code))))))

(eval-after-load "yasnippet"
  (dolist (hook '(c++-mode-hook c-mode-hook))
    (add-hook hook #'yas-minor-mode-on)))

(eval-after-load "flycheck"
  (dolist (hook '(c++-mode-hook c-mode-hook))
    (add-hook hook #'flycheck-mode-on-safe)))

(eval-after-load "cmake-ide"
  (eval-after-load "evil-leader"
    (dolist (mode '(c-mode c++-mode))
      (evil-leader/set-key-for-mode mode
        "m c" 'cmake-ide-compile))))

(provide '32-c++)
;;; 32-c++.el ends here
