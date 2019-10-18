;;; 31-lsp.el -- lsp mode
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'lsp-mode)
(install-when-compile 'lsp-ui)
(install-when-compile 'dap-mode)
(install-when-compile 'company-lsp)

(with-eval-after-load "lsp-mode"
  (setq-default lsp-enable-snippet nil)
  (setq-default lsp-prefer-flymake nil)
  (eval-after-load "company"
    (add-hook 'lsp-mode-hook #'(lambda ()
                                 (add-to-list 'company-backends #'company-lsp)))))
(with-eval-after-load "lsp-ui"
  (require 'lsp-ui-flycheck)
  ;; (add-hook 'lsp-ui-mode-hook #'(lambda ()
  ;;                                 (dolist (mode (haskell-mode rust-mode))
  ;;                                   (flycheck-add-mode 'lsp-ui mode))
  ;;                                 (flycheck-select-checker 'lsp-ui)))
  ;; (define-key lsp-ui-mode-map (kbd "C-c l") #'lsp-ui-flycheck-list)
  (define-key lsp-ui-mode-map (kbd "C-c d") #'lsp-execute-code-action)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; (add-hook 'lsp-ui-doc-frame-hook
  ;;           (lambda (frame _w)
  ;;             (set-face-attribute 'default frame
  ;;                                 :height 100)))
  (setq lsp-ui-doc-enable nil)
  (set-face-attribute 'markdown-code-face nil
                      :inherit 'default)
  (setq lsp-ui-sideline-ignore-duplicate nil)
  (setq lsp-ui-sideline-show-code-actions nil))

(provide '31-lsp)
;;; 31-lsp.el ends here
