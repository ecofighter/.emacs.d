;;; 32-haskell.el -- haskell -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '31-lsp)
(install-when-compile 'haskell-mode)
(install-when-compile 'haskell-snippets)
(install-when-compile 'lsp-haskell)
(install-when-compile 'reformatter)
(install-when-compile 'ormolu)

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

;; (defgroup brittany nil
;;   "Integration with brittany Haskell formatter"
;;   :prefix "brittany-"
;;   :group 'haskell)
;; (defvar brittany-mode-map (make-sparse-keymap)
;;   "Local keymap for `brittany-format-on-save-mode`.")

;; (require 'reformatter)
;; (reformatter-define brittany-format
;;   :program "brittany"
;;   :args '("--indent" "2" "--columns" "80" "--write-mode" "display" "/dev/stdin")
;;   :group 'brittany
;;   :lighter " br"
;;   :keymap brittany-mode-map)

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
      "O" 'haskell-evil-open-above))
  (define-key haskell-mode-map (kbd "C-c r") #'brittany-format-buffer))

(add-hook 'haskell-mode-hook
          #'(lambda ()
              ;; (interactive-haskell-mode)
              ;; (yas-minor-mode-on)
              ;; (company-mode-on)
              ;; (flycheck-mode-on-safe)
              (highlight-indent-guides-mode)
              (require 'lsp)
              (require 'lsp-haskell)
              ;; (setq lsp-haskell-process-path-hie "ghcide")
              ;; (setq lsp-haskell-process-args-hie '())
              ;; (setq lsp-document-sync-method 'full)
              ;; (setq lsp-haskell-process-path-hie "hie-8.6.5")
              ;; (setq lsp-haskell-process-args-hie
              ;;       (append lsp-haskell-process-args-hie '("+RTS" "-M1.5G" "-RTS")))
              (lsp)
              ;; (add-hook 'lsp-ui-mode-hook #'(lambda ()
              ;;                                 (require 'lsp-ui-flycheck)
              ;;                                 (flycheck-mode 1)
              ;;                                 (flycheck-select-checker 'lsp-ui)))
              ;; (set (make-local-variable 'company-backends)
              ;;      '((company-lsp company-yasnippet company-dabbrev-code)))
              ))

(defvar *my/lsp-remote-haskell* nil
  "Haskell lsp client on remote.")

(with-eval-after-load 'lsp-haskell
  (setq *my/lsp-remote-haskell*
        (make-lsp--client
         :new-connection (lsp-tramp-connection "haskell-language-server-wrapper")
         ;; Should run under haskell-mode and haskell-literate-mode. We need to list the
         ;; latter even though it's a derived mode of the former
         :major-modes '(haskell-mode haskell-literate-mode)
         ;; This is arbitrary.
         :server-id 'lsp-haskell-remote
         ;; We need to manually pull out the configuration section and set it. Possibly in
         ;; the future lsp-mode will asssociate servers with configuration sections more directly.
         :initialized-fn (lambda (workspace)
                           (with-lsp-workspace workspace
                             (lsp--set-configuration (lsp-configuration-section "haskell"))
                             (setq lsp-haskell-server-args '("-d" "-l" "/tmp/hls.log"))))
         ;; This is somewhat irrelevant, but it is listed in lsp-language-id-configuration, so
         ;; we should set something consistent here.
         :language-id "haskell"
         ;; This is required for completions to works inside language pragma statements
         :completion-in-comments? t
         :remote? t))
  (lsp-register-client *my/lsp-remote-haskell*))

(provide '32-haskell)
;;; 32-haskell.el ends here
