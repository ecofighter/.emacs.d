(install-when-compile 'haskell-mode)
(install-when-compile 'lsp-mode)
(install-when-compile 'lsp-haskell)
;; (install-when-compile 'company-lsp)

(add-hook 'haskell-mode-hook #'(lambda ()
                                 (setq tab-width 2)
                                 (setq indent-tabs-mode nil)))

(defun haskell-indentation-advice ()
  (when (and (< 1 (line-number-at-pos))
             (save-excursion
               (forward-line -1)
               (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
    (delete-region (line-beginning-position) (point))))

(setup-after "lsp-haskell"
  (advice-add 'haskell-indentation-newline-and-indent
              :after 'haskell-indentation-advice))

(setup-expecting "lsp-haskell"
  (add-hook 'haskell-mode-hook #'(lambda ()
                                   (setup "lsp-mode"
                                     ;; (setup-after "company"
                                     ;;   (setup "company-lsp"
                                     ;;     (setf company-backends (cons #'company-lsp (delete #'company-capf company-backends)))))
                                     (setup-after "flycheck"
                                       (setup "lsp-flycheck"
                                         (lsp-enable-flycheck))))
                                   (setup "lsp-haskell")
                                   (lsp-haskell-enable))))

(setup-expecting "lsp-haskell"
  (add-hook 'haskell-mode-hook #'haskell-indent-mode))
(provide-file)
