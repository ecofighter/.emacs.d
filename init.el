;;; init.el -- my config
;;; Commentary:
;;; Code:
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq garbage-collection-messages t)

(require 'package)
(customize-set-variable 'gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-enable-at-startup nil)
(when (fboundp 'native-comp-available-p)
  (when (native-comp-available-p)
    (customize-set-variable 'package-native-compile t)))
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa"        . "https://melpa.org/packages/")
                       ("org"          . "https://orgmode.org/elpa/")
                       ("gnu"          . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords
    :ensure t
    :config
    (leaf blackout :ensure t)
    (leaf-keywords-init))
  (leaf leaf-convert
    :ensure t))

;; (eval-when-compile
;; (when (boundp 'package-pinned-packages)
;;   (setq package-pinned-packages
;;         '((evil      . "melpa-stable")))))

(add-to-list 'load-path "~/.emacs.d/mylisp")
(require 'mymacros)
(require 'myutils)
(add-to-list 'load-path "~/.emacs.d/inits")
(require '00-init)
(leaf *line-numbers
  :global-minor-mode global-display-line-numbers-mode
  :config
  (leaf *resize
    :defvar text-scale-mode text-scale-mode-step text-scale-mode-amount
    :defun face-remap-add-relative face-remap-remove-relative my/resize-line-number
    :after face-remap
    :config
    (defvar-local my/line-number-remapping nil)
    (defun my/resize-line-number (&optional inc)
      (when my/line-number-remapping
        (face-remap-remove-relative my/line-number-remapping))
      (setq-local my/line-number-remapping
            (and text-scale-mode
                 (face-remap-add-relative 'line-number
                                          :height
                                          (expt text-scale-mode-step
                                                text-scale-mode-amount))))
      (force-window-update (current-buffer)))
    (advice-add 'text-scale-mode :after #'my/resize-line-number)))
(require '01-graphics)
(require '02-exec-path-from-shell)
(leaf evil
  :ensure t
  :require t
  :custom
  ((evil-want-C-i-jump . t)
   (evil-normal-state-tag . "<N>")
   (evil-insert-state-tag . `,(propertize "<I>" 'face '((:background "#076678"))))
   (evil-visual-state-tag . `,(propertize "<V>" 'face '((:background "#fe8019"))))
   (evil-replace-state-tag . `,(propertize "<R>" 'face '((:background "#8f3f71"))))
   (evil-mode-line-format . '(before . mode-line-front-space)))
  :global-minor-mode evil-mode
  :defvar evil-normal-state-map
  :bind
  (:evil-insert-state-map
   ("C-h" . evil-delete-backward-char))
  (:evil-motion-state-map
   ("j" . evil-next-visual-line)
   ("gj" . evil-next-line)
   ("k" . evil-previous-visual-line)
   ("gk" . evil-previous-line))
  ;; :defer-config
  ;; (eval-and-compile
  ;;   (defmacro my/swap-key-in-map (map key1 key2)
  ;;     "Swap KEY1 and KEY2 in MAP."
  ;;     `(let ((def1 (lookup-key ,map ,key1))
  ;;            (def2 (lookup-key ,map ,key2)))
  ;;        (define-key ,map ,key1 def2)
  ;;        (define-key ,map ,key2 def1)))
  ;;   (my/swap-key-in-map evil-motion-state-map "j" "gj")
  ;;   (my/swap-key-in-map evil-motion-state-map "k" "gk"))
  :defer-config
  (define-key evil-normal-state-map (kbd "M-.")
    `(menu-item "" evil-repeat-pop :filter
                ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))
  :config
  (leaf undo-fu
    :emacs< 28
    :ensure t
    :require t
    :custom
    ((evil-undo-system . 'undo-fu)))
  (leaf *emacs-undo
    :emacs>= 28
    :custom
    ((evil-undo-system . 'undo-redo)))
  (leaf evil-leader
    :ensure t
    :after evil
    :global-minor-mode global-evil-leader-mode
    :defun evil-leader/set-leader
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "C-i" 'previous-buffer)
    (evil-leader/set-key "<backtab>" 'next-buffer)
    (evil-leader/set-key "<SPC>" 'counsel-M-x)
    (evil-leader/set-key
      "q q" 'my/exit
      "q Q" 'save-buffers-kill-emacs
      "q f" 'delete-frame
      "q t" 'toggle-frame-maximized)
    (evil-leader/set-key
      "k k" 'kill-buffer-and-window
      "k b" 'kill-buffer))
  (leaf evil-anzu
    :ensure t
    :require t
    :after evil
    :global-minor-mode global-anzu-mode
    :blackout anzu-mode)
  (leaf evil-terminal-cursor-changer
    :ensure t
    :unless (window-system)
    :after evil
    :require t
    :config
    (etcc-on)))
(require '10-ivy)
(require '10-shackle)
(require '10-winner)
(require '10-which-key)
;; (require '10-hl-todo)
;; (require '10-editorconfig)
(require '10-smart-mode-line)
(require '10-tramp)
;; (require '10-ripgrep)
(require '20-eshell)
(require '20-ddskk)
;; (require '20-migemo)
;; (require '20-fcitx)
;; (require '20-uim)
(require '20-company)
(require '20-yasnippet)
;; (require '20-flymake)
(require '20-flycheck)
(require '20-smartparens)
(require '20-rainbow-delimiters)
(require '20-highlight-indent-guides)
(require '20-magit)
;; (require '20-google-translate)
;; (require '30-bison)
;; (require '30-cmake)
(require '30-org)
(require '30-yaml)
(require '30-emacslisp)
;; (require '30-common-lisp)
(require '30-scheme)
;; (require '30-agda)
;; (require '30-ocaml)
(require '30-sml)
(require '30-fsharp)
(require '30-markdown)
(require '30-purescript)
(require '30-coq)
(require '30-maude)
;; (require '30-lean)
;; (require '30-pdf)
(require '31-lsp)
;; (require '31-eglot)
(require '32-c++)
(require '32-rust)
(require '32-haskell)
(require '32-scala)
(require '32-typescript)
(require '32-latex)

(install-when-compile 'package-utils)
(garbage-collect)
(provide 'init)
;;; init.el ends here
