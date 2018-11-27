(install-when-compile 'haskell-mode)
(install-when-compile 'intero)
(install-when-compile 'hindent)
(install-when-compile 'lsp-mode)
(install-when-compile 'lsp-ui)
(install-when-compile 'lsp-haskell)
(install-when-compile 'company-lsp)

(add-hook 'haskell-mode-hook
          #'(lambda ()
              (setq tab-width 2)
              (setq indent-tabs-mode nil)))
(setup-include "s")
(defun haskell-indentation-advice ()
  (when (and (< 1 (line-number-at-pos))
             (save-excursion
               (forward-line -1)
               (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
    (delete-region (line-beginning-position) (point))))

(advice-add 'haskell-indentation-newline-and-indent
            :after 'haskell-indentation-advice)

(defun haskell-evil-open-above ()
  (interactive)
  (evil-digit-argument-or-evil-beginning-of-line)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

(defun haskell-evil-open-below ()
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))

(evil-define-key 'normal haskell-mode-map
  "o" 'haskell-evil-open-below
  "O" 'haskell-evil-open-above)

;; (setup "lsp-haskell"
;;   (setenv "cabal_helper_libexecdir" "/home/haneta/.local/libexec")
;;   (add-hook 'lsp-mode-hook #'lsp-ui-mode)
;;   (add-hook 'haskell-mode-hook #'lsp-haskell-enable))

(setup-expecting "intero"
  (add-hook 'haskell-mode-hook #'intero-mode))
(setup-expecting "flycheck"
  (add-hook 'intero-mode-hook #'(lambda ()
                                  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))

(provide-file)
