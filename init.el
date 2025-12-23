;;; Init.el -- my config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (when (file-exists-p custom-file)
;;   (load custom-file))
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa"        . "https://melpa.org/packages/")
                       ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                       ("gnu-devel"    . "https://elpa.gnu.org/devel/")
                       ("gnu"          . "https://elpa.gnu.org/packages/")))
  (customize-set-variable
   'Package-archive-priorities '(("gnu" . 1)
                                 ("nongnu" . 2)
                                 ("melpa" . 3)))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)
(use-package bind-key
  :demand t)
(use-package emacs
  :custom
  (make-backup-files nil)
  (backup-inhibited nil)
  (create-lockfiles nil)
  (fast-but-imprecise-scrolling t)
  (process-adaptive-read-buffering t)
  (indent-tabs-mode nil)
  (select-enable-clipboard t)
  (x-select-enable-clipboard-manager t)
  (use-file-dialog nil)
  (use-short-answers t)
  (window-min-height 10)
  (window-min-width 70)
  (split-width-threshold 140)
  (split-height-threshold 20)
  (vc-handled-backends '(Git))
  (fill-column 80)
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
  (advice-add #'split-window-sensibly :override #'my/split-window-sensibly-prefer-horizontally)
  (use-package treesit
    :ensure nil
    :when (treesit-available-p)
    :custom
    (treesit-font-lock-level 4)
    (treesit-language-source-alist '((rust . ("https://github.com/tree-sitter/tree-sitter-rust"
                                              nil nil nil nil))
                                     (nix . ("https://github.com/nix-community/tree-sitter-nix"
                                             nil nil nil nil))
                                     (c . ("https://github.com/tree-sitter/tree-sitter-c"
                                           nil nil nil nil))
                                     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"
                                             nil nil nil nil))))))
(progn ; platform-spec
  (use-package alert
    :ensure t
    :config
    (cond
     ((eq system-type 'gnu/linux)
      (custom-set-variables
       '(alert-default-style 'libnotify)))
     ((or (getenv "WSL_DISTRO_NAME")
          (eq system-type 'windows-nt))
      (use-package alert-toast
        :ensure t
        :custom
        (alert-default-style 'toast)))))
  (cond
   ((eq system-type 'gnu/linux)
    (use-package fcitx
      :ensure t
      :demand t
      :custom
      (fcitx-use-dbus 'fcitx5)
      :config
      (setq fcitx-remote-command "fcitx5-remote")
      (if (display-graphic-p)
          (fcitx-aggressive-setup)
        (add-hook 'server-after-make-frame-hook #'fcitx-aggressive-setup)))
    ;; credit: yorickvP on Github
    (defvar wl-copy-process nil)
    (defun wl-copy (text)
      (setq wl-copy-process (make-process :name "wl-copy"
                                          :buffer nil
                                          :command '("wl-copy" "-f" "-n")
                                          :connection-type 'pipe
                                          :noquery t))
      (process-send-string wl-copy-process text)
      (process-send-eof wl-copy-process))
    (defun wl-paste ()
      (if (and wl-copy-process (process-live-p wl-copy-process))
          nil ; should return nil if we're the current paste owner
        (shell-command-to-string "wl-paste -n | tr -d \r")))
    (setq interprogram-cut-function 'wl-copy)
    (setq interprogram-paste-function 'wl-paste))
   ((eq system-type 'darwin)
    (custom-set-variables
     '(ns-command-modifier 'meta)
     '(ns-alternate-modifier 'option)))
   ((or (getenv "WSL_DISTRO_NAME")
        (eq system-type 'windows-nt))
    (with-eval-after-load 'browse-url
      (defun my/browse-url-via-powershell (url &rest _args)
        (shell-command (concat "powershell.exe start \"" url "\"")))
      (declare-function my/browse-url-via-powershell "init")
      (setf browse-url-browser-function #'my/browse-url-via-powershell)))))
(use-package auto-compile
  :ensure t
  :defer t
  :custom (auto-compile-native-compile t)
  :hook
  (emacs-lisp-mode . auto-compile-on-save-mode))
(progn ; theme
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (use-package doom-modeline
    :ensure t
    :demand t
    :init
    (doom-modeline-mode +1)
    :custom
    (doom-modeline-window-width-limit 60)
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-buffer-modification-icon t)
    (doom-modeline-buffer-state-icon t)
    (doom-modeline-buffer-encoding t)
    (doom-modeline-buffer-major-mode t)
    (doom-modeline-major-mode-icon t)
    (doom-modeline-major-mode-color-icon nil)
    (doom-modeline-buffer-minor-modes nil)
    (doom-modeline-indent-info nil)
    (doom-modeline-lsp t)
    (doom-modeline-github nil)
    (doom-modeline-gnus nil)
    (doom-modeline-irc nil)
    (doom-modeline-mu4e nil)
    (doom-modeline-persp-name t)
    (doom-modeline-persp-icon t)
    (doom-modeline-project-detection 'auto)
    (doom-modeline-unicode-fallback nil))
  (use-package nerd-icons
    :ensure t
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono")
    :config
    (use-package nerd-icons-completion
      :ensure t
      :demand t
      :hook
      (marginalia-mode . nerd-icons-completion-marginalia-setup)
      :init
      (nerd-icons-completion-mode +1))
    (use-package nerd-icons-dired
      :ensure t
      :defer t
      :hook (dired-mode . nerd-icons-dired-mode)))
  (use-package rainbow-delimiters
    :ensure t
    :demand t
    :hook (prog-mode . rainbow-delimiters-mode))
  (use-package whitespace
    :custom
    (show-trailing-whitespace t)
    (whitespace-style '(face trailing))
    :init
    (global-whitespace-mode +1))
  (use-package display-line-numbers
    :hook
    (prog-mode . display-line-numbers-mode)
    (text-mode . display-line-numbers-mode)
    (conf-mode . display-line-numbers-mode))
  (use-package hl-line
    :init
    (global-hl-line-mode +1))
  (use-package hl-todo
    :ensure t
    :init
    (global-hl-todo-mode +1))
  (use-package perfect-margin
    :ensure t
    :demand t
    :custom
    (perfect-margin-visible-width 120)
    (perfect-margin-disable-in-splittable-check t)
    :config
    (declare-function perfect-margin-mode "perfect-margin")
    (perfect-margin-mode +1)
    (with-eval-after-load 'doom-modeline
      (eval-when-compile (require 'doom-modeline nil t))
      (setq mode-line-right-align-edge 'window)))
  (use-package spacious-padding
    :ensure t
    :demand t
    :custom
    (spacious-padding-width '( :internal-border-width 15
                               :header-line-width 4
                               :mode-line-width 6
                               :right-divider-width 30
                               :scroll-bar-width 8))
    (spacious-padding `( :mode-line-active 'default
                         :mode-line-inactive vertical-border))
    :config
    (spacious-padding-mode +1))
  (use-package dashboard
    :disabled t
    :ensure t
    :defer t
    :custom
    (dashboard-force-refresh t)
    (dashboard-center-content t)
    (dashboard-vertically-center-content t)
    (dashboard-display-icons t)
    (dashboard-icon-type 'nerd-icons)
    (dashboard-set-heading-icons t)
    (dashboard-set-file-icons t)
    (dashboard-startup-banner 2)
    (dashboard-items '((agenda . 10)
                       (projects . 5)
                       (bookmarks . 5)
                       (recents . 10)))
    :config
    (when (daemonp)
      (setq initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*"))))
    (dashboard-setup-startup-hook))
  (use-package modus-themes
    :ensure t
    :demand t
    :config
    (declare-function modus-themes-load-theme "modus-themes")
    (add-to-list 'custom-theme-load-path (locate-user-emacs-file "theme/"))
    (load-theme 'kanagawa-wave :no-confirm))
  (use-package ef-themes
    :disabled t
    :ensure t
    :demand t
    :custom
    (modus-themes-mixed-fonts t)
    (modus-themes-italic-constructs t)
    :init
    (ef-themes-take-over-modus-themes-mode +1)
    :config
    (declare-function modus-themes-load-theme "modus-themes")
    (modus-themes-load-theme 'ef-owl))
  (use-package doom-themes
    :disabled t
    :ensure t
    :demand t
    :custom
    (doom-themes-enable-bold t)
    (doom-themes-enable-italic t)
    :config
    (load-theme 'doom-monokai-pro t)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))
  (use-package fontaine
    :ensure t
    :demand t
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
        (fontaine-set-preset (or (fontaine-restore-latest-preset) 'ibmplex)))))
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :unless (eq system-type 'windows-nt)
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH" "LD_LIBRARY_PATH")))
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
(use-package recentf
  :custom
  (recentf-exclude `(,(locate-user-emacs-file "bookmarks")
                     ,(locate-user-emacs-file "elpa")
                     "/tmp.*")))
(use-package eldoc
  :ensure t
  :init
  (global-eldoc-mode +1))
(use-package vertico
  :ensure t
  :demand t
  :bind (:map vertico-map
              ("C-h" . vertico-directory-up)
              ("C-m" . vertico-exit)
              ("C-j" . vertico-exit-input))
  :init
  (vertico-mode +1))
(use-package consult
  :ensure t
  :demand t
  :bind
  ;; ("C-c i e" . consult-flymake)
  ("C-c i i" . consult-imenu)
  ("C-c i l" . consult-line)
  ("C-c i p" . consult-yank-from-kill-ring)
  ("C-c i b" . consult-buffer)
  :config
  (use-package consult-eglot
    :ensure t
    :defer t
    :after (consult eglot)
    :bind
    ("C-c i s" . consult-eglot-symbols))
  (use-package consult-flycheck
    :ensure t
    :defer t
    :after (consult flycheck)
    :bind
    ("C-c i e" . consult-flycheck)))
(use-package marginalia
  :ensure t
  :demand t
  :init
  (marginalia-mode +1))
(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(progn ; completion
  (use-package company
    :ensure t
    :demand t
    :config
    (global-company-mode +1)
    (use-package company-box
      :ensure t
      :demand t
      :hook
      (company-mode . company-box-mode))
    (use-package company-fuzzy
      :ensure t
      :demand t
      :preface
      (use-package liquidmetal
        :ensure t)
      :custom
      (company-fuzzy-sorting-backend 'liquidmetal)
      (company-fuzzy-reset-selection t)
      (company-fuzzy-prefix-on-top nil)
      (company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
      :hook (company-mode . company-fuzzy-mode)))
  (use-package cape
    :ensure t
    :demand t
    :custom
    (cape-dabbrev-check-other-buffers nil)
    :config
    (mapc (lambda (item)
            (add-to-list 'completion-at-point-functions item t))
          '(cape-dabbrev
            cape-keyword
            cape-file
            cape-tex))))
(use-package centaur-tabs
  :ensure t
  :demand t
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward)
  ("M-S-<left>" . centaur-tabs-move-current-tab-to-left)
  ("M-S-<right>" . centaur-tabs-move-current-tab-to-right)
  :config
  (centaur-tabs-mode t)
  (declare-function centaur-tabs-get-group-name "centaur-tabs-functions")
  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((derived-mode-p 'eshell-mode)
       "EShell")
      ((derived-mode-p 'dashboard-mode)
       "Dashboard")
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs")
      ((memq major-mode '(magit-process-mode
                          magit-status-mode
                          magit-diff-mode
                          magit-log-mode
                          magit-file-mode
                          magit-blob-mode
                          magit-blame-mode))
       "Magit")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))
(use-package perspective
  :ensure t
  :demand t
  :custom
  (persp-mode-prefix-key `,(kbd "C-x x"))
  (persp-sort 'created)
  :init
  (persp-mode +1)
  :config
  (with-eval-after-load 'consult
    (eval-when-compile
      (require 'consult)
      (declare-function consult--customize-put "ext:consult.el"))
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))
(use-package vundo
  :ensure t
  :demand t)
(use-package avy
  :ensure t
  :demand t
  :bind
  ("C-c j j" . avy-goto-word-or-subword-1)
  ("C-c j l" . avy-goto-line))
(use-package ace-window
  :ensure t
  :demand t
  :bind
  ("C-c w" . ace-window))
(use-package which-key
  :ensure t
  :demand t
  :init
  (which-key-mode +1))
(use-package tramp
  :ensure t
  :defer t
  :config
  (eval-when-compile
    (defvar tramp-remote-path))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(use-package vterm
  :unless (eq system-type 'windows-nt)
  :ensure t
  :defer t)
(use-package eshell
  :ensure t
  :defer t
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
    :config
    (declare-function skk-isearch-mode-setup "skk-isearch")
    (declare-function skk-isearch-mode-cleanup "skk-isearch")
    (add-hook 'isearch-mode-hook #'(lambda ()
                                     (when (and (boundp 'skk-mode)
                                                skk-mode
                                                skk-isearch-mode-enable)
                                       (skk-isearch-mode-setup))))
    (add-hook 'isearch-mode-end-hook #'(lambda ()
                                         (when (and (featurep 'skk-isearch)
                                                    skk-isearch-mode-enable)
                                           (skk-isearch-mode-cleanup))))))
(use-package flymake
  :disabled t
  :ensure t
  :defer t
  :hook (prog-mode . flymake-mode)
  :config
  (use-package flymake-collection
    :ensure t
    :demand t
    :after flymake)
  (use-package flymake-popon
    :disabled t
    :ensure t
    :defer t
    :hook (flymake-mode . flymake-popon-mode)))
(use-package flycheck
  :ensure t
  :demand t
  :hook
  (after-init . global-flycheck-mode)
  :config
  (use-package flycheck-eglot
    :ensure t
    :defer t
    :after (flycheck eglot)
    :hook
    (eglot-managed-mode . flycheck-eglot-mode)))
(use-package ispell
  :defer t
  :custom
  (ispell-program-name "hunspell")
  (ispell-really-hunspell t)
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary "~/Documents/ispell_persional.dict"))
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
  :demand t
  :hook (vterm-mode . puni-disable-puni-mode)
  :init
  (puni-global-mode +1))
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook
  ((prog-mode conf-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-odd-face-perc 10)
  (highlight-indent-guides-auto-even-face-perc 15)
  (highlight-indent-guides-auto-top-odd-face-perc 40)
  (highlight-indent-guides-auto-top-even-face-perc 45)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'column))
(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-x g" . #'magit)
  :config
  (use-package difftastic
    :ensure t
    :defer t
    :bind
    (:map magit-blame-read-only-mode-map
          ("D" . 'difftastic-magit-show)
          ("S" . 'difftastic-magit-show))
    :config
    (transient-append-suffix 'magit-diff '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
       ("S" "Difftastic show" difftastic-magit-show)])))
(progn ; org
  (defconst my/org-inbox-file "inbox.org"
    "Org file to use with org-capture.")
  (defvar my/org-prefix nil
    "Prefix map for org commands.")
  (define-prefix-command 'my/org-prefix)
  (use-package org
    :ensure t
    :defer t
    :custom
    (org-directory "~/Documents/org")
    (org-startup-indented nil)
    (org-startup-truncated nil)
    (org-todo-keywords '((sequence "TODO" "PROG" "|" "DONE")))
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
      (use-package org-super-agenda
        :ensure t
        :defer t
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
    (use-package org-modern
      :ensure t
      :defer t
      :custom
      (org-modern-star nil)
      (org-modern-table nil)
      :hook
      (org-mode . org-modern-mode)
      (org-agenda-finalize . org-modern-agenda))
    (use-package valign
      :ensure t
      :defer t
      :custom
      (valign-fancy-bar t)
      :hook
      (org-mode . valign-mode))
    (use-package org-tidy
      :ensure t
      :defer t)
    (use-package org-web-tools
      :ensure t
      :defer t)
    (use-package org-roam
      :ensure t
      :defer t
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
                                                (propertize "${tags:10}" 'face 'org-tag))))))
(use-package pdf-tools
  :ensure t
  :defer t
  :init
  (pdf-loader-install))
(use-package literate-calc-mode
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'org
    (eval-when-compile (require 'org))
    (require 'literate-calc-mode)))
(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'org
    (eval-when-compile (require 'org))
    (setf (alist-get 'dot org-babel-load-languages) t))
  (with-eval-after-load 'org-src
    (eval-when-compile (require 'org-src))
    (setf (alist-get "dot" org-src-lang-modes nil nil #'string=) 'graphviz-dot-mode)))
(use-package yasnippet
  :ensure t
  :defer t
  :hook
  (prog-mode . yas-minor-mode-on)
  :config
  (use-package yasnippet-capf
    :ensure t
    :defer t
    :config
    (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
(use-package dape
  :ensure t
  :demand t
  :custom
  (dape-buffer-window-arrangement 'right))
(use-package eglot
  :ensure t
  :defer t)
(use-package lsp-mode
  :ensure t
  :defer t
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-file-watchers t)
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting t)
  (lsp-document-sync-method nil)
  (lsp-response-timeout 5)
  (lsp-use-plist t)
  (lsp-log-io nil)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-snippet nil)
  (lsp-diagnostics-provider :auto)
  (lsp-enable-completion t)
  (lsp-completion-provider :none)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :preface
  (setenv "LSP_USE_PLISTS" "true")
  :config
  (require 'lsp-sml)
  (add-to-list 'lsp-language-id-configuration '(sml-mode . "sml"))
  (use-package lsp-ui
    :disabled t
    :ensure t
    :defer t
    :custom
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    (lsp-ui-doc-position 'at-point)
    (lsp-ui-doc-show-with-cursor t)
    (lsp-ui-doc-show-with-mouse nil)
    (lsp-ui-doc-alignment 'window)
    (lsp-ui-flycheck-enable t)
    (lsp-ui-imenu-enable t)
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-fontify 'on-demand)
    :hook
    (lsp-mode . lsp-ui-mode)
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references))))
(progn ; languages
  (use-package text-mode
    :custom
    (text-mode-ispell-word-completion nil))
  (progn ; c/cpp
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
      :defer t
      :custom
      (bison-all-electricity-off t))
    (use-package cmake-mode
      :ensure t
      :defer t))
  (use-package markdown-ts-mode
    :mode ("\\.md\\'" . markdown-ts-mode)
    :defer t
    :config
    (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
    (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))
  (use-package web-mode
    :ensure t
    :defer t
    :mode ("\\.csp\\'" "\\.razor\\'" "\\.html?\\'"))
  (progn ; haskell
    (use-package haskell-mode
      :ensure t
      :defer t)
    (use-package lsp-haskell
      :disabled t
      :ensure t
      :defer t))
  (progn ; ocaml
    (use-package tuareg
      :ensure t
      :defer t)
    (use-package ocamlformat
      :ensure t
      :defer t
      :after tuareg
      :custom
      (ocamlformat-command '("ocamlformat")))
    (use-package dune
      :ensure t
      :defer t)
    (use-package ocaml-eglot
      :ensure t
      :after tuareg
      :defer t
      :hook
      (ocaml-eglot . eglot-ensure)
      :custom
      (ocaml-eglot-syntax-checker 'flymake)
      :config
      ;; (with-eval-after-load "eglot"
      ;;   (add-to-list 'eglot-server-programs
      ;;                '(tuareg-mode . ("dune" "tools" "exec" "ocamllsp"))))
      )
    ;; (with-eval-after-load "lsp-ocaml"
    ;;   (custom-set-variables
    ;;    '(lsp-ocaml-lsp-server-command '("dune" "tools" "exec" "ocamllsp"))))
    )
  (progn ; nix
    (use-package nix-ts-mode
      :ensure t
      :defer t
      :mode "\\.nix\\'"
      :init
      (when (treesit-available-p)
        (when (fboundp 'treesit-ready-p)
          (unless (treesit-ready-p 'nix)
            (add-to-list 'treesit-language-source-alist '(nix . ("https://github.com/nix-community/tree-sitter-nix"
                                                                 nil nil nil nil)))
            (treesit-install-language-grammar 'nix))))))
  (progn ; sml
    (use-package sml-mode
      :ensure t
      :defer t)
    (use-package smlfmt
      :ensure t
      :defer t
      :hook
      (sml-mode . smlfmt-format-on-save-mode)))
  (progn ; rust
    (use-package rust-mode
      :ensure t
      :defer t
      :custom
      (rust-indent-offset 4)
      (rust-mode-treesitter-derive t)
      :config
      (when (treesit-available-p)
        (when (fboundp 'treesit-ready-p)
          (unless (treesit-ready-p 'rust)
            (add-to-list 'treesit-language-source-alist '(rust . ("https://github.com/tree-sitter/tree-sitter-rust"
                                                               nil nil nil nil)))
            (treesit-install-language-grammar 'rust)))))
    (use-package cargo-mode
      :ensure t
      :defer t
      :hook
      (rust-mode . cargo-minor-mode)))
  (progn ; python
    (use-package python-mode
      :ensure t
      :defer t
      :hook
      (python-mode . python-ts-mode)))
  (progn ; dotnet
    (use-package sharper
      :ensure t
      :demand t
      :bind
      ("C-c n" . sharper-main-transient))
    (use-package csharp-mode
      :ensure t
      :defer t)
    (use-package fsharp-mode
      :ensure t
      :defer t
      :config
      (use-package eglot-fsharp
        :ensure t
        :after eglot
        :demand t)
      (with-eval-after-load 'lsp-fsharp
        (custom-set-variables
         '(lsp-fsharp-use-dotnet-tool-for-fsac nil)))
      (with-eval-after-load 'company
        (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))))
  (progn ; lean4
    (use-package lean4-mode
      :commands lean4-mode
      :vc (:url "https://github.com/leanprover-community/lean4-mode"
                :rev :newest)))
  (progn ; latex
    (use-package auctex
      :ensure t
      :custom
      (TeX-engine 'luatex)
      (LaTeX-using-Biber t)
      (TeX-PDF-mode t)
      (TeX-source-correlate-mode t)
      (TeX-source-correlate-method 'synctex)
      (TeX-source-correlate-start-server t)
      (TeX-parse-self t)
      (TeX-auto-save t)
      (TeX-view-program-selection '((output-pdf "PDF Tools")))
      (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
      :hook
      (LaTeX-mode . flyspell-mode)
      :config
      (declare-function TeX-revert-document-buffer "tex")
      (with-eval-after-load "tex"
        (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
      (use-package reftex
        :ensure t
        :hook
        (LaTeX-mode . turn-on-reftex)
        :custom
        (reftex-plug-into-AUCTeX t)
        (reftex-ref-style-default-list '("Cleveref" "Default")))
      (use-package auctex-cluttex
        :ensure t
        :after tex
        :hook
        (LaTeX-mode . auctex-cluttex-mode)
        :config
        (declare-function TeX-active-master "tex")
        (declare-function TeX-output-extension "tex")
        (declare-function auctex-cluttex--TeX-ClutTeX-sentinel "auctex-cluttex")
        (defun my/run-after-compilation-finished-funcs (&rest _args)
          "run AUCTeX's TeX-after-compilation-finished-functions hook. Ignore all ARGS"
          (unless TeX-error-list
            (run-hook-with-args 'TeX-after-compilation-finished-functions
                                (with-current-buffer TeX-command-buffer
                                  (expand-file-name
                                   (TeX-active-master (TeX-output-extension)))))))
        (declare-function my/run-after-compilation-finished-funcs "init")
        (advice-add #'auctex-cluttex--TeX-ClutTeX-sentinel :after #'my/run-after-compilation-finished-funcs)))))
(use-package meow
  :ensure t
  :demand t
  :custom (meow-use-clipboard t)
  :init
  (require 'meow)
  (declare-function meow-motion-define-key "meow-helpers")
  (declare-function meow-leader-define-key "meow-helpers")
  (declare-function meow-normal-define-key "meow-helpers")
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
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
  (meow-global-mode +1)
  :config
  (meow-leader-define-key
   '("q" . previous-buffer)
   '("Q" . next-buffer)
   '("u" . vundo)
   '("I" . imenu-list)))

(provide 'init)
;;; init.el ends here
