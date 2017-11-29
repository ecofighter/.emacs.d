(install-when-compile 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setup-lazy '(flycheck-mode-on-safe global-flycheck-mode) "flycheck"
  (defconst flycheck-error-list-format [("Line" 4 flycheck-error-list-entry-< :right-align t)
                                        ("Col" 3 nil :right-align t)
                                        ("Level" 8 flycheck-error-list-entry-level-<)
                                        ("ID" 20 t)
                                        ("Message (Checker)" 0 t)])
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window)
                 (side            . bottom)
                 (window-height   . 0.33)))
  (global-flycheck-mode)
  (evil-leader/set-key
    "e l" 'flycheck-list-errors))

(provide-file)
