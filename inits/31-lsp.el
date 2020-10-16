;;; 31-lsp.el -- lsp mode
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'lsp-mode)
(install-when-compile 'lsp-ui)
(install-when-compile 'dap-mode)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(with-eval-after-load "lsp-mode"
  (setq-default lsp-enable-snippet nil)
  (setq-default lsp-prefer-flymake nil)
  (setq-default lsp-completion-provider :capf))
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
  (setq lsp-ui-doc-enable t)
  ;; (set-face-attribute 'markdown-code-face nil
  ;;                     :inherit 'default)
  ;; (setq lsp-ui-sideline-ignore-duplicate nil)
  ;; (setq lsp-ui-sideline-show-code-actions nil))
  )

(provide '31-lsp)
;;; 31-lsp.el ends here
