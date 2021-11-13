;;; init.el -- my config -*- lexical-binding: t -*-
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
    (leaf-keywords-init)))

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
  (leaf *line-numbers-resize-advice
    :defvar text-scale-mode text-scale-mode-step text-scale-mode-amount
    :defun face-remap-add-relative face-remap-remove-relative my/resize-line-number
    :after face-remap
    :config
    (defvar my/line-number-remapping nil)
    (make-local-variable 'my/line-number-remapping)
    (defun my/resize-line-number (&rest _rest)
      "Advice to resize line numbers."
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
(leaf *graphics
  :config
  (leaf *font
    :config
    (setq use-default-font-for-symbols nil)
    (add-to-list 'default-frame-alist '(font . "Ricty Diminished-14"))
    (set-face-attribute 'default nil :font "Ricty Diminished-14")
    ;; (set-fontset-font t 'ascii (font-spec :family "Ricty Diminished" :size 14))
    ;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))
    (set-fontset-font t 'unicode (font-spec :family "Noto Sans") nil 'append)))
;; (require '02-exec-path-from-shell)
(leaf exec-path-from-shell
  :ensure t
  :unless (equal system-type 'windows-nt)
  :require exec-path-from-shell
  :custom ((exec-path-from-shell-arguments . nil)
           (exec-path-from-shell-check-startup-files . nil))
  :config
  ;;(add-to-list 'exec-path-from-shell-variables "CAML_LD_LIBRARY_PATH")
  (exec-path-from-shell-initialize))
(leaf evil
  :ensure t
  :require t
  :custom
  ((evil-want-C-i-jump . t)
   (evil-normal-state-tag . "<N>")
   (evil-insert-state-tag . `,(propertize "<I>" 'face '((:background "#076678"))))
   (evil-visual-state-tag . `,(propertize "<V>" 'face '((:background "#fe8019" :foreground "#232323"))))
   (evil-replace-state-tag . `,(propertize "<R>" 'face '((:background "#8f3f71"))))
   (evil-emacs-state-tag . `,(propertize "<E>" 'face '((:background "#ba45ea" :foreground "#efefef"))))
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
    :defun etcc-on
    :config
    (etcc-on)))
(leaf treemacs
  :ensure t
  :config
  (add-hook 'treemacs-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  (leaf treemacs-evil
    :ensure t
    :after evil
    :require t)
  (leaf *treemacs-evil-keyconfig
    :after evil-leader
    :config
    (evil-leader/set-key
      "t t" 'treemacs
      "t s" 'treemacs-select-window)))
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
;;(require '20-ddskk)
(leaf ddskk
  :ensure t
  :defvar skk-isearch-mode-enable
  :custom
  ((skk-kutouten-type . '("．" . "，"))
   (skk-use-azik . t)
   (skk-isearch-start-mode . 'latin)
   (skk-isearch-mode-enable . t)
   (default-input-method . "japanese-skk")
   ;;(skk-large-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
   (skk-itaiji-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.itaiji")
   (skk-cdb-large-jisyo . "~/.emacs.d/SKK-JISYO.myL.cdb"))
  :bind (("C-x j" . skk-mode)
         ("C-x J" . skk-auto-fill-mode))
  :init
  (leaf *skk-isearch
    :after skk
    :config
    (add-hook 'isearch-mode-hook
              #'(lambda ()
                  (when (and (boundp 'skk-mode)
                             skk-mode
                             skk-isearch-mode-enable)
                    (skk-isearch-mode-setup))))
    (add-hook 'isearch-mode-end-hook
              #'(lambda ()
                  (when (and (featurep 'skk-isearch)
                             skk-isearch-mode-enable)
                    (skk-isearch-mode-cleanup))))))
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
(leaf posframe
  :ensure t
  :config
  (leaf company-posframe
    :ensure t
    :after company
    :global-minor-mode t)
  (leaf ddskk-posframe
    :ensure t
    :after skk
    :global-minor-mode t))
(require '30-org)
(require '30-yaml)
;; (require '30-emacslisp)
(leaf *emacslisp
  :ensure smartparens
  :require smartparens-config
  :defun my/elisp-mode-hook-fun
  :config
  (defun my/elisp-mode-hook-fun ()
    (hs-minor-mode 1)
    (smartparens-strict-mode 1)
    (flycheck-mode 1))
  (add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-hook-fun))
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
;; (require '31-lsp)
(leaf *languages
  :config
  (leaf *latex
    :config
    (leaf auctex
      :custom
      ((TeX-engine . 'luatex))
      :ensure t
      :config
      (leaf auctex-cluttex
        :ensure t
        :config
        (add-hook 'LaTeX-mode-hook #'auctex-cluttex-mode)))))

(leaf lsp-mode
  :ensure t
  :custom
  ((lsp-auto-guess-root . t)
   (lsp-enable-snippet . t)
   (lsp-diagnostics-provider . :flycheck)
   (lsp-enable-completion . t)
   (lsp-completion-provider . :capf)
   (lsp-modeline-diagnostics-scope . :file))
  :config
  (leaf *lsp-keybinds
    :custom
    ((lsp-keymap-prefix . "C-c l"))
    :bind
    (:lsp-mode-map
     ("C-c l" . lsp-command-map))
    ;;(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
    :config
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (leaf lsp-ui
    :ensure t
    :custom
    ((lsp-ui-doc-enable . t)
     (lsp-ui-doc-use-childframe . t)
     (lsp-ui-doc-use-webkit . nil))
    :bind ((:lsp-ui-mode-map
            ([remap xref-find-definitions]
             . lsp-ui-peek-find-definitions)
            ([remap xref-find-references]
             . lsp-ui-peek-find-references)))
    :config
    (add-hook 'lsp-mode-hook #'lsp-ui-mode))
  (leaf lsp-treemacs
    :ensure t
    :after treemacs
    :config
    (add-hook 'lsp-mode-hook #'lsp-treemacs-sync-mode))
  (leaf lsp-latex
    :ensure t
    :commands lsp-latex-build lsp-latex-forward-search
    :defun my/start-server
    :custom
    ((lsp-latex-build-executable . "cluttex")
     (lsp-latex-build-args . '("-e" "lualatex" "-interaction=nonstopmode" "-synctex=1" "%f"))
     (lsp-latex-forward-search-executable . `,(pcase system-type
                                                ('windows-nt "C:\\Users\\ecofi\\AppData\\Local\\SumatraPDF\\SumatraPDF.exe")
                                                ('gnu/linux "zathura")))
     (lsp-latex-forward-search-args . `,(pcase system-type
                                          ('windows-nt '("-reuse-instance" "%p" "-forward-search" "%f" "%l"))
                                          ('gnu/linux '("--synctex-forward" "%l:1:%f" "%p")))))
    :init
    (defun my/start-server ()
      (unless (server-running-p)
        (server-start)))
    (add-hook 'LaTeX-mode-hook #'my/start-server)
    (add-hook 'plain-TeX-mode-hook #'lsp)
    (add-hook 'LaTeX-mode-hook #'lsp)
    (add-hook 'bibtex-mode-hook #'lsp)
    (leaf *lsp-latex-keybinds
      :after evil-leader
      :config
      `,(dolist (mode '(latex-mode tex-mode))
          (evil-leader/set-key-for-mode mode
            "m b" 'lsp-latex-build
            "m f" 'lsp-latex-forward-search)))))

;;(require '31-eglot)
(require '32-c++)
(require '32-rust)
(require '32-haskell)
(require '32-scala)
(require '32-typescript)
;;(require '32-latex)

(install-when-compile 'package-utils)
(garbage-collect)
(provide 'init)
;;; init.el ends here
