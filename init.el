;;; Init.el -- my config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(defconst is-linux `,(eq system-type 'gnu/linux))
(defconst is-darwin `,(eq system-type 'darwin))
(defconst is-windows `,(eq system-type 'windows-nt))
(defconst is-wsl `,(and is-linux (getenv "WSL_DISTRO_NAME")))
(custom-set-variables
 '(package-archives '(("melpa"        . "https://melpa.org/packages/")
                      ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                      ("gnu-devel"    . "https://elpa.gnu.org/devel/")
                      ("gnu"          . "https://elpa.gnu.org/packages/")))
 '(package-archive-priorities '(("gnu" . 1)
                                ("nongnu" . 2)
                                ("melpa" . 3))))
(when (version< emacs-version "29.1")
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))
(require 'use-package)
(use-package emacs
  :custom
  (make-backup-files nil)
  (backup-inhibited nil)
  (create-lockfiles nil)
  (fast-but-imprecise-scrolling t)
  (process-adaptive-read-buffering t)
  (indent-tabs-mode nil)
  (select-enable-clipboard t)
  (use-file-dialog nil)
  (use-short-answers t)
  (window-min-height 10)
  (window-min-width 70)
  (split-width-threshold 120)
  (split-height-threshold 20)
  (vc-handled-backends '(Git))
  (fill-column 100)
  (tab-width 2)
  (truncate-lines nil)
  (truncate-partial-width-windows nil)
  (inhibit-startup-screen nil)
  (inhibit-x-resources t)
  (inhibit-startup-buffer-menu t)
  (blink-matching-paren nil)
  (auto-mode-case-fold nil)
  (bidi-inhibit-bpa t)
  (enable-recusive-minibuffers t)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (ring-bell-function 'ignore)
  :config
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'cperl-indent-level 'tab-width)
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (defun my/reopen-with-sudo ()
    "Reopen buffer with sudo."
    (interactive)
    (find-file (concat "/sudo::"
                       (expand-file-name (buffer-file-name)))))
  (defun my/split-window-sensibly-prefer-horizontally (&optional window)
    "Alternative `split-window-sensibly'."
    (let ((window (or window (selected-window))))
      (or (and (window-splittable-p window t)
	             (with-selected-window window
	               (split-window-right)))
          (and (window-splittable-p window)
	             (with-selected-window window
	               (split-window-below)))
	        (and
           (let ((frame (window-frame window)))
             (or
              (eq window (frame-root-window frame))
              (catch 'done
                (walk-window-tree (lambda (w)
                                    (unless (or (eq w window)
                                                (window-dedicated-p w))
                                      (throw 'done nil)))
                                  frame nil 'nomini)
                t)))
	         (not (window-minibuffer-p window))
	         (let ((split-height-threshold 0))
	           (when (window-splittable-p window t)
	             (with-selected-window window
	               (split-window-right))))))))
  (declare-function my/split-window-sensibly-prefer-horizontally "init")
  (advice-add #'split-window-sensibly :override #'my/split-window-sensibly-prefer-horizontally))
(use-package meow
  :ensure t
  :demand t
  :autoload (meow-motion-define-key
             meow-leader-define-key
             meow-normal-define-key)
  :custom
  (meow-use-clipboard t)
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
  (defun meow-setup ()
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("/" . isearch-forward-regexp)
     '("<escape>" . ignore)))
  (declare-function meow-setup "init")
  (meow-setup)
  (meow-global-mode +1))
(use-package repeat
  :ensure nil
  :init
  (repeat-mode +1))
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode +1))
(use-package recentf
  :ensure nil
  :custom
  (recentf-exclude `(,(locate-user-emacs-file "bookmarks")
                     ,(locate-user-emacs-file "elpa")
                     "/tmp.*"))
  :init
  (recentf-mode +1))
(use-package savehist
  :ensure nil
  :init
  (savehist-mode +1))
(use-package autorevert
  :ensure nil
  :init
  (global-auto-revert-mode +1))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode +1))
(use-package reformatter
  :ensure t)
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :custom
  (treesit-font-lock-level 4))

(use-package alert
  :ensure t
  :custom
  (alert-default-style `,(cond
                          ((and is-linux
                                (not is-wsl))
                           'notifications)
                          (is-darwin
                           'osx-notifier)))
  :config
  (use-package alert-toast
    :when (or is-windows
              is-wsl)
    :ensure t
    :demand t
    :custom
    (alert-default-style 'toast)))
(use-package fcitx
  :ensure t
  :when is-linux
  :custom
  (fcitx-use-dbus 'fcitx5)
  :config
  (setq-default fcitx-remote-command "fcitx5-remote")
  (if (display-graphic-p)
      (fcitx-aggressive-setup)
    (add-hook 'server-after-make-frame-hook #'fcitx-aggressive-setup)))
`,(cond
   ((and is-linux
         (not is-wsl))
    ;; credit: yorickvP on Github
    (when (executable-find "wl-copy")
      (defvar wl-copy-process nil)
      (defun wl-copy (text)
        (let ((default-directory temporary-file-directory))
          (setq wl-copy-process (make-process :name "wl-copy"
                                              :buffer nil
                                              :command '("wl-copy" "-f" "-n")
                                              :connection-type 'pipe
                                              :noquery t))
          (process-send-string wl-copy-process text)
          (process-send-eof wl-copy-process)))
      (defun wl-paste ()
        (let ((default-directory temporary-file-directory))
          (if (and wl-copy-process (process-live-p wl-copy-process))
              nil ; should return nil if we're the current paste owner
            (with-temp-buffer
              (process-file "wl-paste" nil t nil "-n")
              (goto-char (point-min))
              (while (search-forward "\r" nil t)
                (replace-match ""))
              (buffer-string)))))
      (setq interprogram-cut-function 'wl-copy)
      (setq interprogram-paste-function 'wl-paste)))
   (is-darwin
    (custom-set-variables
     '(ns-command-modifier 'meta)
     '(ns-alternate-modifier 'option)))
   ((or is-windows
        is-wsl)
    (when is-windows
      (setopt file-name-coding-system 'cp932
              default-process-coding-system '(utf-8-dos . japanese-cp932-dos)))
    (with-eval-after-load 'browse-url
      (defun my/browse-url-via-powershell (url &rest _args)
        (shell-command (concat "powershell.exe start \"" url "\"")))
      (declare-function my/browse-url-via-powershell "init")
      (setf browse-url-browser-function #'my/browse-url-via-powershell))))
(use-package auto-compile
  :ensure t
  :custom (auto-compile-native-compile t)
  :hook
  (emacs-lisp-mode . auto-compile-on-save-mode))
;; theme
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(use-package mood-line
  :ensure t
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  :config
  (mood-line-mode +1))
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  (use-package nerd-icons-completion
    :ensure t
    :hook
    (marginalia-mode . nerd-icons-completion-marginalia-setup)
    :init
    (nerd-icons-completion-mode +1))
  (use-package nerd-icons-dired
    :ensure t
    :hook (dired-mode . nerd-icons-dired-mode)))
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package whitespace
  :ensure nil
  :custom
  (show-trailing-whitespace t)
  (whitespace-style '(face trailing))
  :init
  (global-whitespace-mode +1))
(use-package display-line-numbers
  :ensure nil
  :hook
  ((prog-mode conf-mode text-mode) . display-line-numbers-mode))
(use-package lin
  :ensure t
  :custom
  (lin-mode-hooks '(prog-mode-hook
                    conf-mode-hook
                    text-mode-hook
                    dired-mode-hook
                    git-rebase-mode-hook
                    grep-mode-hook
                    ibuffer-mode-hook
                    ilist-mode-hook
                    log-view-mode-hook
                    magit-log-mode-hook
                    occur-mode-hook
                    org-agenda-mode-hook
                    pdf-outline-buffer-mode-hook
                    proced-mode-hook
                    tabulated-list-mode-hook))
  :config
  (lin-global-mode +1))
(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode +1))
(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 110)
  :hook
  ((prog-mode conf-mode text-mode) . olivetti-mode))
(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths '( :internal-border-width 4
                              :header-line-width 8
                              :mode-line-width 4
                              :right-divider-width 24
                              :scroll-bar-width 8))
  (spacious-padding-subtle-frame-lines `( :mode-line-active spacious-padding-line-active
                                          :mode-line-inactive spacious-padding-line-inactive
                                          :header-line-active spacious-padding-line-active
                                          :header-line-inactive spacious-padding-line-inactive))
  :config
  (spacious-padding-mode +1))
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode +1))
(use-package modus-themes
  :ensure t
  :autoload modus-themes-load-theme
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-italic-constructs t)
  :init
  (add-to-list 'custom-theme-load-path (locate-user-emacs-file "theme/"))
  (modus-themes-load-theme 'kanagawa-wave))
(use-package fontaine
  :ensure t
  :autoload
  (fontaine-mode
   fontaine-set-preset
   fontaine-restore-latest-preset)
  :custom
  (inhibit-compacting-font-caches t)
  (fontaine-latest-state-file `,(locate-user-emacs-file "fontaine-latest-state.eld"))
  (fontaine-presets '((source-han
                       :default-family "Source Han Code JP"
                       :variable-pitch-family "Source Han Sans")
                      (udev
                       :default-family "UDEV Gothic"
                       :variable-pitch-family "BIZ UDPGothic")
                      (udev35
                       :default-family "UDEV Gothic 35"
                       :variable-pitch-family "BIZ UDPGothic")
                      (ibmplex
                       :default-family "IBM Plex Mono"
                       :variable-pitch-family "IBM Plex Sans")
                      (plemol
                       :default-family "PlemolJP"
                       :variable-pitch-family "IBM Plex Sans")
                      (plemol35
                       :default-family "PlemolJP35"
                       :variable-pitch-family "IBM Plex Sans")
                      (0xproto
                       :default-family "0xProto"
                       :variable-pitch-family "IBM Plex Sans")
                      (sarasa
                       :default-family "Sarasa Mono J"
                       :variable-pitch-family "Sarasa Gothic J")))
  :init
  (fontaine-mode +1)
  :config
  (if (or (display-graphic-p)
          (daemonp))
      (fontaine-set-preset (or (fontaine-restore-latest-preset) 'ibmplex))))
(use-package exec-path-from-shell
  :ensure t
  :unless is-windows
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH" "LD_LIBRARY_PATH")))
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
(use-package eldoc
  :init
  (global-eldoc-mode +1))
(use-package vertico
  :ensure t
  :bind
  (:map vertico-map
        ("C-m" . vertico-exit)
        ("C-j" . vertico-exit-input))
  :init
  (vertico-mode +1)
  :config
  (use-package vertico-directory
    :ensure nil
    :after vertico
    :bind
    (:map vertico-directory-map
          ("C-h" . vertico-directory-up))
    :hook
    (rfn-eshadow-update-overlay . vertico-directory-tidy))
  (use-package vertico-multiform
    :ensure nil
    :after vertico
    :custom
    (vertico-multiform-categories '((file (:keymap . vertico-directory-map))))
    :config
    (vertico-multiform-mode +1)))
(use-package consult
  :ensure t
  :bind
  ("C-c i i" . consult-imenu)
  ("C-c i l" . consult-line)
  ("C-c i p" . consult-yank-from-kill-ring)
  ("C-c i b" . consult-buffer)
  ("C-c i g" . consult-ripgrep)
  :config
  (use-package consult-eglot
    :ensure t
    :after (consult eglot)
    :bind
    ("C-c i s" . consult-eglot-symbols)))
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode +1))
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (use-package embark-consult
    :ensure t
    :after (consult embark)))
;; completion
(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))
(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("<return>" . nil))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.2)
  (corfu-preselect 'directory)
  (corfu-quit-no-match t)
  (corfu-on-exact-match 'show)
  :init
  (global-corfu-mode +1)
  :config
  (use-package corfu-terminal
    :ensure t
    :when (version< emacs-version "31")
    :after corfu
    :unless (display-graphic-p)
    :config
    (corfu-terminal-mode +1))
  (use-package corfu-popupinfo
    :ensure nil
    :after corfu
    :hook
    (corfu-mode . corfu-popupinfo-mode))
  (use-package corfu-history
    :ensure nil
    :after corfu
    :init
    (corfu-history-mode))
  (use-package nerd-icons-corfu
    :ensure t
    :after (corfu nerd-icons)
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))
(use-package cape
  :ensure t
  :custom
  (cape-dabbrev-check-other-buffers nil)
  :bind ("C-c TAB" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-tex)
  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive))
  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)))
(use-package yasnippet
  :ensure t
  :autoload (yas-expand)
  :bind
  (:map yas-minor-mode-map
        ("C-<return>" . #'yas-expand))
  :init
  (yas-global-mode +1)
  :config
  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet)
  (use-package yasnippet-capf
    :ensure t
    :after yasnippet
    :init
    (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project" "flake.nix")))
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show 1)
  :init
  (tab-bar-mode +1))
(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key `,(kbd "C-x x"))
  (persp-sort 'created)
  :init
  (persp-mode +1)
  :config
  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources persp-consult-source)))
(use-package vundo
  :ensure t
  :bind
  ("C-c u" . #'vundo))
(use-package avy
  :ensure t
  :bind
  ("C-c j j" . avy-goto-word-or-subword-1)
  ("C-c j l" . avy-goto-line))
(use-package ace-window
  :ensure t
  :bind
  ("C-c w" . ace-window))
(use-package which-key
  :ensure t
  :init
  (which-key-mode +1))
(use-package tramp
  :ensure t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(use-package vterm
  :unless is-windows
  :ensure t
  :config
  (let* ((desc (package-get-descriptor 'vterm)))
    (when (string= (package-desc-status desc) "external")
      (add-to-list 'package-pinned-packages '(vterm . "manual")))))
(use-package eshell
  :ensure t
  :bind
  ("C-c '" . eshell))
(use-package ddskk
  :disabled t
  :ensure t
  :bind
  ("C-x j" . skk-mode)
  ("C-x J" . skk-auto-fill-mode)
  :custom
  (skk-kutouten-type '("．" . "，"))
  (skk-use-azik t)
  (skk-isearch-start-mode 'latin)
  (skk-isearch-mode-enable t)
  (default-input-method "japanese-skk")
  (skk-status-indicator nil)
  (skk-show-tooltip t)
  (skk-show-inline nil)
  (skk-show-mode-show t)
  (skk-show-mode-style 'inline)
  (skk-inline-show-face nil)
  :custom-face
  (skk-show-mode-inline-face . '((t (:inherit default :background "white smoke" :foreground "SlateGray4"))))
  :init
  (use-package ddskk-posframe
    :ensure t
    :hook (skk-mode . ddskk-posframe-mode))
  :config
  (eval-when-compile
    (when (package-installed-p 'ddskk)
      (require 'skk-vars)))
  (with-eval-after-load 'skk-vars
    (setq skk-get-jisyo-directory (locate-user-emacs-file "skk-get-jisyo/"))
    (setq skk-large-jisyo (expand-file-name "SKK-JISYO.L" skk-get-jisyo-directory))
    (setq skk-itaiji-jisyo (expand-file-name "SKK-JISYO.itaiji" skk-get-jisyo-directory))
    (setq skk-cdb-large-jisyo (expand-file-name "SKK-JISYO.L.cdb" skk-get-jisyo-directory)))
  (use-package skk-isearch
    :ensure nil
    :autoload (skk-isearch-mode-setup
               skk-isearch-mode-cleanup)
    :config
    (add-hook 'isearch-mode-hook #'(lambda ()
                                     (when (and (boundp 'skk-mode)
                                                skk-mode
                                                skk-isearch-mode-enable)
                                       (skk-isearch-mode-setup))))
    (add-hook 'isearch-mode-end-hook #'(lambda ()
                                         (when (and (featurep 'skk-isearch)
                                                    skk-isearch-mode-enable)
                                           (skk-isearch-mode-cleanup))))))
(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode)
  :config
  (use-package consult-flycheck
    :ensure t
    :bind
    ("C-c i e" . consult-flycheck)))
(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "hunspell")
  (ispell-really-hunspell t)
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary "~/Documents/ispell_persional.dict")
  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(use-package paren
  :ensure nil
  :init
  (show-paren-mode +1))
(use-package elec-pair
  :ensure nil
  :init
  (electric-pair-mode +1))
(use-package puni
  :ensure t
  :hook (vterm-mode . puni-disable-puni-mode)
  :bind
  ("C-c p w r" . puni-wrap-round)
  ("C-c p w s" . puni-srap-square)
  ("C-c p w c" . puni-wrap-curly)
  ("C-c p w a" . puni-wrap-angle)
  ("C-c p s" . puni-splice)
  :init
  (puni-global-mode +1))
(use-package highlight-indent-guides
  :ensure t
  :hook
  ((prog-mode conf-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-odd-face-perc 15)
  (highlight-indent-guides-auto-even-face-perc 20)
  (highlight-indent-guides-auto-stack-odd-face-perc 40)
  (highlight-indent-guides-auto-stack-even-face-perc 45)
  (highlight-indent-guides-auto-top-odd-face-perc 50)
  (highlight-indent-guides-auto-top-even-face-perc 55)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-method 'fill))
(use-package magit
  :ensure t
  :bind
  ("C-x g" . #'magit)
  :config
  (use-package difftastic
    :ensure t
    :after magit
    :init
    (use-package transient
      :autoload (transient-get-suffix
                 transient-parse-suffix))
    (use-package magit-blame
      :ensure nil
      :bind
      (:map magit-blame-read-only-mode-map
            ("M-RET" . #'difftastic-magit-show))
      :config
      (let ((suffix '("M-RET" "Difftastic show" difftastic-magit-show)))
        (unless (equal (transient-parse-suffix 'magit-blame suffix)
                       (transient-get-suffix 'magit-blame "b"))
          (transient-append-suffix 'magit-blame "b" suffix)))
      (use-package magit-diff
        :ensure magit
        :config
        (let ((suffix [("M-d" "Difftastic diff (dwim)" difftastic-magit-diff)
                       ("M-c" "Difftastic show" difftastic-magit-show)]))
          (unless (equal (transient-parse-suffix 'magit-diff suffix)
                         (transient-get-suffix 'magit-diff '(-1 -1)))
            (transient-append-suffix 'magit-diff '(-1 -1) suffix)))))))
;; org
(defconst my/org-inbox-file "inbox.org"
  "Org file to use with `org-capture'.")
(defvar my/org-prefix nil
  "Prefix map for org commands.")
(define-prefix-command 'my/org-prefix)
(use-package org
  :ensure t
  :custom
  (org-directory "~/Documents/org")
  (org-startup-indented t)
  (org-startup-truncated t)
  (org-todo-keywords '((sequence "TODO" "PROG" "|" "DONE")))
  (org-babel-load-languages '((emacs-lisp . t)
                              (calc . t)))
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-clocked-in-display 'frame-title)
  (org-capture-templates `(("t" "Todo" entry
                            (file+headline ,(expand-file-name my/org-inbox-file org-directory)
                                           "Tasks")
                            "* TODO %?\n  %i\n  %a")
                           ("n" "Note" entry
                            (file+headline ,(expand-file-name my/org-inbox-file org-directory)
                                           "Notes")
                            "* %?\n  %i\n  %a")))
  (org-edit-src-content-indentation 0)
  :bind
  ("C-c o" . my/org-prefix)
  (:map my/org-prefix
        ("a" . org-agenda)
        ("o" . my/open-org-inbox)
        ("O" . my/open-org-dir)
        ("p" . org-pomodoro)
        ("l" . org-web-tools-insert-link-for-url)
        ("c" . org-capture)
        ("r t" . org-roam-buffer-toggle)
        ("r f" . org-roam-node-find)
        ("r i" . org-roam-node-insert)
        ("r c" . org-roam-capture))
  :config
  (let ((inbox-file-name (expand-file-name my/org-inbox-file org-directory)))
    (unless (file-exists-p inbox-file-name)
      (with-temp-file inbox-file-name
        (insert "#+TITLE: inbox\n"))))
  (defun my/open-org-dir ()
    "Open `org-directory'."
    (interactive)
    (find-file org-directory))
  (defun my/open-org-inbox ()
    "Open the inbox file."
    (interactive)
    (find-file (expand-file-name my/org-inbox-file org-directory)))
  (use-package org-agenda
    :ensure nil
    :custom
    (org-agenda-span 'day)
    (org-agenda-files `(,(expand-file-name my/org-inbox-file org-directory)
                        ,(file-name-as-directory (expand-file-name "tasks/" org-directory))))
    (org-agenda-skip-scheduled-if-done t)
    (org-agenda-include-deadlines t)
    (org-agenda-include-diary t)
    (org-agenda-block-separator nil)
    (org-agenda-compact-blocks t)
    :config
    (use-package org-indent
      :ensure nil
      :after org
      :hook
      (org-mode . org-indent-mode))
    (use-package org-modern
      :ensure t
      :after org
      :custom
      (org-modern-star t)
      (org-modern-table nil)
      :hook
      (org-mode . org-modern-mode)
      (org-agenda-finalize . org-modern-agenda))
    (use-package valign
      :ensure t
      :after org
      :custom
      (valign-fancy-bar nil)
      :hook
      (org-mode . valign-mode))
    (use-package org-tidy
      :ensure t
      :after org)
    (use-package org-web-tools
      :ensure t
      :after org)
    (use-package org-super-agenda
      :ensure t
      :init
      (org-super-agenda-mode +1)
      :custom
      (org-super-agenda-groups '((:name "Past"
                                        :scheduled past
                                        :deadline past)
                                 (:name "Due Today"
                                        :and (:deadline today :scheduled nil))
                                 (:name "Today"
                                        :scheduled today
                                        :time-grid t)
                                 (:name "Future"
                                        :scheduled future)
                                 (:name "Not Scheduled"
                                        :scheduled nil)))
      (org-agenda-custom-commands '(("n" "Agenda and all TODOs"
                                     ((agenda #1="")
                                      (alltodo #1#)))
                                    ("d" "Day agenda"
                                     ((agenda "" ((org-agenda-span 'day)))))
                                    ("w" "Week agenda"
                                     ((agenda "" ((org-agenda-span 'week)))))
                                    ("q" "4 Weeks agenda"
                                     ((agenda "" ((org-agenda-span 28)
                                                  (org-deadline-warning-days 56)))))
                                    ("c" "Super"
                                     ((agenda ""
                                              ((org-agenda-span 'day)
                                               (org-agenda-prefix-format
                                                "%i %-12:c%?-12t% s")
                                               (org-deadline-warning-days 28)
                                               (org-super-agenda-groups
                                                '((:name "Past but not finished"
                                                         :scheduled past
                                                         :deadline past)
                                                  (:name "Today"
                                                         :time-grid t
                                                         :deadline today
                                                         :scheduled today)
                                                  (:name "Soon"
                                                         :anything t)))))
                                      (alltodo ""
                                               ((org-agenda-overriding-header "")
                                                (org-agenda-prefix-format
                                                 "%i %-12:c%?-12t% s")
                                                (org-deadline-warning-days 28)
                                                (org-super-agenda-groups
                                                 '((:name "Finishied"
                                                          :todo "DONE")
                                                   (:name "Past"
                                                          :scheduled past
                                                          :deadline past)
                                                   (:name "Due Today"
                                                          :deadline today)
                                                   (:name "Not scheduled"
                                                          :and (:scheduled nil :deadline t))
                                                   (:name "No Deadline"
                                                          :and (:scheduled nil :deadline nil))
                                                   (:name "Future"
                                                          :scheduled future)))))))))))
  (use-package org-roam
    :ensure t
    :init
    (org-roam-db-autosync-mode +1)
    :custom
    (org-roam-directory `,(expand-file-name "roam" org-directory))
    (org-roam-db-location `,(locate-user-emacs-file "org-roam.db"))
    (org-roam-capture-templates '(("p" "Permanent Note" plain "%?"
                                   :target (file+head
                                            "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
                                            "#+title: ${title}\n#+filetags: :Permanent:"))
                                  ("f" "Fleet Note" plain "%?"
                                   :target (file+head
                                            "fleet/%<%Y%m%d%H%M%S>-${slug}.org"
                                            "#+title: ${title}\n#+filetags: :Fleet:"))
                                  ("l" "Literature Note" plain "%?"
                                   :target (file+head
                                            "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                            "#+title: ${title}\n#+filetags: :Literature:"))))
    (org-roam-node-display-template `,(concat "${title:*} "
                                              (propertize "${tags:10}" 'face 'org-tag)))))
(use-package graphviz-dot-mode
  :ensure t
  :config
  (with-eval-after-load 'org
    (require 'ob-dot)))
(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-page)
  :init
  (pdf-loader-install)
  :config
  (let* ((desc (package-get-descriptor 'pdf-tools)))
    (when (string= (package-desc-status desc) "external")
      (add-to-list 'package-pinned-packages '(pdf-tools . "manual")))))
(use-package eglot
  :ensure t
  :config
  (use-package eglot-signature-eldoc-talkative
    :ensure t
    :autoload eglot-signature-eldoc-talkative
    :after (eglot eldoc)
    :init
    (defun my/eglot-specific-eldoc ()
      "Add `eglot-signature-eldoc-talkative' to `eldoc-documentation-functions'."
      (add-to-list 'eldoc-documentation-functions #'eglot-signature-eldoc-talkative))
    (declare-function my/eglot-specific-eldoc "init")
    (add-hook 'eglot-managed-mode-hook #'my/eglot-specific-eldoc))
  (use-package flycheck-eglot
    :ensure t
    :after (flycheck eglot)
    :config
    (global-flycheck-eglot-mode +1)))
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-file-watchers t)
  (lsp-enable-folding nil)
  (lsp-enable-snippet t)
  (lsp-enable-on-type-formatting t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-document-sync-method nil)
  (lsp-response-timeout 5)
  (lsp-use-plist t)
  (lsp-log-io nil)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-snippet nil)
  (lsp-diagnostics-provider :auto)
  (lsp-enable-completion t)
  (lsp-completion-provider :capf)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration))
;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc --from gfm --to html5 --katex --standalone"))
;; web
(use-package web-mode
  :ensure t
  :mode ("\\.csp\\'" "\\.razor\\'" "\\.html?\\'"))
;; c/c++
(when (treesit-available-p)
  (when (fboundp 'treesit-ready-p)
    (unless (treesit-ready-p 'c)
      (add-to-list 'treesit-language-source-alist '(c . ("https://github.com/tree-sitter/tree-sitter-c"
                                                         nil nil nil nil)))
      (treesit-install-language-grammar 'c))
    (unless (treesit-ready-p 'cpp)
      (add-to-list 'treesit-language-source-alist '(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"
                                                           nil nil nil nil)))
      (treesit-install-language-grammar 'cpp)))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))
(use-package bison-mode
  :ensure t
  :custom
  (bison-all-electricity-off t))
(use-package cmake-mode
  :ensure t)
;; haskell
(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . eglot-ensure))
;; ocaml
(use-package tuareg
  :ensure t)
(use-package ocamlformat
  :ensure t
  :after tuareg
  :custom
  (ocamlformat-command '("ocamlformat")))
(use-package dune
  :ensure t)
(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :custom
  (ocaml-eglot-syntax-checker 'flycheck)
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))
;; nix
(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook
  (nix-ts-mode . eglot-ensure)
  :init
  (when (treesit-available-p)
    (when (fboundp 'treesit-ready-p)
      (unless (treesit-ready-p 'nix)
        (add-to-list 'treesit-language-source-alist '(nix . ("https://github.com/nix-community/tree-sitter-nix"
                                                             nil nil nil nil)))
        (treesit-install-language-grammar 'nix)))))
;; standard ml
(use-package sml-mode
  :ensure t)
(use-package smlfmt
  :ensure t
  :hook
  (sml-mode . smlfmt-format-on-save-mode))
;; rust
(use-package rust-mode
  :ensure t
  :custom
  (rust-indent-offset 4)
  (rust-mode-treesitter-derive t)
  :hook
  (rust-mode . eglot-ensure)
  :config
  (when (treesit-available-p)
    (when (fboundp 'treesit-ready-p)
      (unless (treesit-ready-p 'rust)
        (add-to-list 'treesit-language-source-alist '(rust . ("https://github.com/tree-sitter/tree-sitter-rust"
                                                              nil nil nil nil)))
        (treesit-install-language-grammar 'rust)))))
(use-package cargo-mode
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))
;; python
(use-package python-mode
  :ensure t
  :config
  (when (treesit-available-p)
    (when (fboundp 'treesit-ready-p)
      (unless (treesit-ready-p 'python)
        (add-to-list 'treesit-language-source-alist '(python . ("https://github.com/tree-sitter/tree-sitter-python"
                                                                nil nil nil nil)))
        (treesit-install-language-grammar 'python)))
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))))
;; dotnet
(use-package sharper
  :ensure t
  :bind
  ("C-c n" . sharper-main-transient))
(use-package csharp-mode
  :ensure t)
(use-package fsharp-mode
  :ensure t
  :hook
  (fsharp-mode . eglot-ensure)
  :config
  (use-package eglot-fsharp
    :ensure t
    :after eglot
    :config
    (when-let* ((executable (executable-find "fsautocomplete"))
                (bindir (file-name-directory executable)))
      (setq-default eglot-fsharp-server-path bindir)
      (setq-default eglot-fsharp-server-install-dir nil))))
;; lean4
(use-package lean4-mode
  :commands lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode"
            :rev :newest))
;; latex
(defgroup latexindent nil
  "Indent with latexindent."
  :group 'extensions
  :prefix 'latexindent-)
(reformatter-define latexindent
  :program "latexindent")
(use-package tex
  :ensure auctex
  :autoload (TeX-revert-document-buffer
             TeX-source-correlate-mode
             TeX-active-master
             TeX-output-extension)
  :custom
  (TeX-engine 'luatex)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  :config
  (TeX-source-correlate-mode +1)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (use-package reftex
    :ensure t
    :hook
    (LaTeX-mode . turn-on-reftex)
    :custom
    (reftex-plug-into-AUCTeX t)
    (reftex-ref-style-default-list '("Cleveref" "Default")))
  (use-package auctex-cluttex
    :ensure t
    :autoload (auctex-cluttex--TeX-ClutTeX-sentinel)
    :hook
    (LaTeX-mode . auctex-cluttex-mode)
    :config
    (defun my/run-after-compilation-finished-funcs (&rest _args)
      "run AUCTeX's TeX-after-compilation-finished-functions hook. Ignore all ARGS"
      (unless TeX-error-list
        (run-hook-with-args 'TeX-after-compilation-finished-functions
                            (with-current-buffer TeX-command-buffer
                              (expand-file-name
                               (TeX-active-master (TeX-output-extension)))))))
    (declare-function my/run-after-compilation-finished-funcs "init")
    (advice-add #'auctex-cluttex--TeX-ClutTeX-sentinel :after #'my/run-after-compilation-finished-funcs)))

(provide 'init)
;;; init.el ends here
