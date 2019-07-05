;;; 32-haskell.el -- haskell
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '31-lsp)
(install-when-compile 'haskell-mode)
(install-when-compile 'lsp-haskell)

;; (setq-default haskell-indent-offset 2)
;; (setq-default haskell-indent-spaces 2)
;; (setq-default haskell-indentation-where-post-offset 2)
;; (setq-default haskell-indentation-where-pre-offset 2)
(require 's)
(add-hook 'haskell-mode-hook
          #'(lambda ()
              (setq tab-width 2)
              (setq indent-tabs-mode nil)
              (turn-on-haskell-indentation)
              (setq indent-line-function 'indent-relative)))
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

(add-hook 'haskell-mode-hook
          #'(lambda ()
              (highlight-indent-guides-mode)
              (lsp)
              (add-hook 'lsp-ui-mode-hook #'(lambda ()
                                              (require 'lsp-ui-flycheck)
                                              (flycheck-mode 1)
                                              (flycheck-select-checker 'lsp-ui)))
              (set (make-local-variable 'company-backends)
                   '((company-lsp company-yasnippet company-dabbrev-code)))))

(provide '32-haskell)
;;; 32-haskell.el ends here
