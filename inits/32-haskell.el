;;; 32-haskell.el -- haskell
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'haskell-mode)
(install-when-compile 'lsp-haskell)

(require 's)
(add-hook 'haskell-mode-hook
          #'(lambda ()
              (setq tab-width 2)
              (setq indent-tabs-mode nil)))
(defun haskell-indentation-advice ()
  "Advice for indent when evil open."
  (when (and (< 1 (line-number-at-pos))
             (save-excursion
               (forward-line -1)
               (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
    (delete-region (line-beginning-position) (point))))

(eval-after-load "haskell-mode"
  (advice-add 'haskell-indentation-newline-and-indent
              :after 'haskell-indentation-advice))

(with-eval-after-load "haskell-mode"
  (defun haskell-evil-open-above ()
    "Evil open above and indent."
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))
  (defun haskell-evil-open-below ()
    "Evil open below and indent."
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))
  (eval-after-load "evil"
    (evil-define-key 'normal haskell-mode-map
      "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above)))

(autoload 'lsp "lsp-mode")
(add-hook 'haskell-mode-hook #'(lambda ()
                                 (highlight-indent-guides-mode)
                                 (require 'lsp-haskell)
                                 (lsp)))

(provide '32-haskell)
;;; 32-haskell.el ends here
