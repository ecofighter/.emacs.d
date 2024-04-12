;;; init.el -- my config -*- lexical-binding: t -*-

;;; Commentary:
;; using leaf.el

;;; Code:
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'package)
(setq package-enable-at-startup nil)
(setq package-install-upgrade-built-in t)
(when (fboundp 'native-comp-available-p)
  (when (native-comp-available-p)
    (setq package-native-compile t)))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa"        . "https://melpa.org/packages/")
                       ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                       ("gnu-devel"    . "https://elpa.gnu.org/devel/")
                       ("gnu"          . "https://elpa.gnu.org/packages/")))
  (customize-set-variable
   'package-archive-priorities '(("gnu-devel" . 3)
                                 ("melpa" . 2)
                                 ("nongnu" . 1)))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))

(leaf leaf
  :ensure t
  :require t
  :config
  (leaf leaf-tree
    :ensure t
    :require t))
(leaf auto-compile
  :ensure t
  :custom ((auto-compile-native-compile . t))
  :global-minor-mode (auto-compile-on-load-mode auto-compile-on-save-mode)
  :config
  (setq load-prefer-newer t))
(leaf *mymacros
  :init
  (defvar *my/package-refreshed* nil)
  (defmacro install-when-compile (package)
    "Install PACKAGE when compile."
    `(list
      (add-to-list 'package-selected-packages ,package)
      (eval-when-compile
        (progn
          (unless (package-installed-p ,package)
            (unless *my/package-refreshed*
              (package-refresh-contents)
              (setq *my/package-refreshed* t))
            (package-install ,package)))))))
(leaf server
  :ensure nil
  :require t
  :defun server-edit server-delete-client
  :defvar server-clients)
(leaf *myutils
  :defun modified-buffers-exist my/exit
  :config
  (defun my/reopen-with-sudo ()
    "Reopen buffer with sudo."
    (interactive)
    (find-file (concat "/sudo::"
                       (expand-file-name (buffer-file-name)))))
  (defun modified-buffers-exist ()
    "Check to see if there are any buffers that have been modified.
It will return true if there are and nil otherwise.
Buffers that have `buffer-offer-save' set to nil are ignored."
    (let (modified-found)
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   (buffer-modified-p buffer)
                   (not (buffer-base-buffer buffer))
                   (or
                    (buffer-file-name buffer)
                    (progn
                      (set-buffer buffer)
                      (and buffer-offer-save (> (buffer-size) 0)))))
          (setq modified-found t)))
      modified-found))
  (defun my/exit ()
    "Exit from Emacs window.
this sxec `server-edit' when in client, or `save-buffers-kill-emacs'."
    (interactive)
    (if server-clients
        (server-edit)
      (save-buffers-kill-emacs)))
  (defun client-save-kill-emacs (&optional display)
    "Save buffers and shutdown the Emacs daemon, use DISPLAY if passed.
It should be called using emacsclient -e `(client-save-kill-emacs)'.
This function will check to see if there are any modified buffers or active
clients or frame.  If so an x window will be opened and the user will
be prompted."

    (let (new-frame modified-buffers active-clients-or-frames)
                                        ; Check if there are modified buffers or active clients or frames.
      (setq modified-buffers (modified-buffers-exist))
      (setq active-clients-or-frames ( or (> (length server-clients) 1)
                                       (> (length (frame-list)) 1)))
                                        ; Create a new frame if prompts are needed.
      (when (or modified-buffers active-clients-or-frames)
        (when (not display) (setq display (getenv "DISPLAY")))
        (message "Opening frame on display: %s" display)
        (select-frame (make-frame-on-display display)))
                                        ; Save the current frame.
      (setq new-frame (selected-frame))

                                        ; When displaying the number of clients and frames:
                                        ; subtract 1 from the clients for this client.
                                        ; subtract 2 from the frames this frame (that we just created) and the default frame.
      (when (or (not active-clients-or-frames)
                (yes-or-no-p (format "There are currently %d clients and %d frames.  Exit anyway?"
                                     (- (length server-clients) 1)
                                     (- (length (frame-list)) 2))))
                                        ; If the user quits during the save dialog then don't exit emacs.
                                        ; Still close the terminal though.
        (let((inhibit-quit t))
                                        ; Save buffers
          (with-local-quit
            (save-some-buffers))
          (if quit-flag
              (setq quit-flag nil)
                                        ; Kill all remaining clients
            (progn
              (dolist (client server-clients)
                (server-delete-client client))
              (kill-emacs)))))
      (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame)))))
;;(add-to-list 'load-path "~/.emacs.d/inits")
(leaf emacs
  :custom ((make-backup-files . nil)
           (indent-tabs-mode . nil)
           (select-enable-clipboard . t)
           (x-select-enable-clipboard-manager . t)
           (use-file-dialog . nil)
           (use-short-answers . t)
           (split-width-threshold . 80)
           (vc-handled-backends . '(Git))
           (fill-column . 80)
           (tab-width . 4)
           (truncate-lines . t)
           (truncate-partial-width-windows . t)
           (inhibit-startup-screen . t)
           (inhibit-x-resources . t)
           (inhibit-compacting-font-caches . t)
           (inhibit-startup-buffer-menu . t)
           (blink-matching-paren . nil)
           (auto-mode-case-fold . nil)
           (bidi-inhibit-bpa . t)
           (enable-recusive-minibuffers . t)
           (completion-cycle-threshold . 3)
           (tab-always-indent . 'complete))
  :init
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'cperl-indent-level 'tab-width)
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (leaf treesit
    :custom (treesit-font-lock-level . 4)
    :config
    (leaf treesit-auto
      :ensure t
      :custom ((treesit-auto-install . 'prompt))
      :global-minor-mode global-treesit-auto-mode)))
(leaf *theme
  :config
  (leaf *bars
    :config
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1))
  (leaf modus-themes
    :disabled t
    :ensure t
    :require t
    :config
    (load-theme 'modus-vivendi-tinted :no-confirm))
  (leaf ef-themes
    :ensure t
    :require t
    :custom
    (ef-themes-headings .'((0 variable-pitch light 1.9)
                           (1 variable-pitch light 1.8)
                           (2 variable-pitch regular 1.7)
                           (3 variable-pitch regular 1.6)
                           (4 variable-pitch regular 1.5)
                           (5 variable-pitch 1.4) ; absence of weight means `bold'
                           (6 variable-pitch 1.3)
                           (7 variable-pitch 1.2)
                           (t variable-pitch 1.1)))
    (ef-themes-mixed-fonts . t)
    (ef-themes-variable-pitch-ui . t)
    :config
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'ef-winter :no-confirm))
  (leaf nerd-icons
    :ensure t
    :config
    (leaf nerd-icons-completion
      :ensure t
      :global-minor-mode nerd-icons-completion-mode)
    (leaf nerd-icons-dired
      :ensure t
      :hook (dired-mode-hook . nerd-icons-dired-mode)))
  (leaf doom-modeline
    :ensure t
    :global-minor-mode doom-modeline-mode
    :custom
    ((doom-modeline-height . 25)
     (doom-modeline-bar-width . 4)
     (doom-modeline-buffer-file-name-style . 'truncate-with-project)
     (doom-modeline-icon . t)
     (doom-modeline-buffer-modification-icon . t)
     (doom-modeline-buffer-state-icon . t)
     (doom-modeline-buffer-encoding . t)
     (doom-modeline-buffer-major-mode . t)
     (doom-modeline-major-mode-icon . t)
     (doom-modeline-major-mode-color-icon . t)
     (doom-modeline-buffer-minor-modes . nil)
     (doom-modeline-indent-info . nil)
     (doom-modeline-lsp . t)
     (doom-modeline-github . nil)
     (doom-modeline-gnus . nil)
     (doom-modeline-irc . nil)
     (doom-modeline-mu4e . nil)
     (doom-modeline-persp-name . t)
     (doom-modeline-persp-icon . t)
     (doom-modeline-project-detection . 'auto)
     (doom-modeline-unicode-fallback . nil)))
  (leaf hide-mode-line
    :ensure t
    :hook
    (treemacs-mode-hook . hide-mode-line-mode)
    (imenu-list-minor-mode-hook . hide-mode-line-mode))
  (leaf minions
    :ensure t
    :global-minor-mode minions-mode)
  (leaf rainbow-delimiters
    :ensure t
    :hook (prog-mode-hook . rainbow-delimiters-mode))
  (leaf whitespace
    :global-minor-mode global-whitespace-mode
    :custom
    (show-trailing-whitespace . t)
    (whitespace-style . '(face trailing)))
  (leaf display-line-numbers
    :global-minor-mode global-display-line-numbers-mode)
  (leaf hl-line
    :global-minor-mode global-hl-line-mode)
  (leaf hl-todo
    :ensure t
    :global-minor-mode global-hl-todo-mode)
  (leaf perfect-margin
    :ensure t
    :custom (perfect-margin-visible-width . 100)
    :global-minor-mode perfect-margin-mode))
(leaf *graphics
  :when (display-graphic-p)
  :config
  (leaf fontaine
    :ensure t
    :require t
    :global-minor-mode fontaine-mode
    :hook (enable-theme-functions . fontaine-apply-current-preset)
    :custom
    (fontaine-latest-state-file . `,(locate-user-emacs-file "fontaine-latest-state.eld"))
    (fontaine-presets . '((regular
                           :default-family "Moralerspace Neon"
                           :fixed-pitch-family "Moralerspace Neon"
                           :variable-pitch-family "IBM Plex Sans JP"
                           :italic-family "Moralerspace Neon")
                          (large
                           :default-family "Moralerspace Neon"
                           :variable-pitch-family "IBM Plex Sans JP")))
    :config
    (let ((table (make-char-table nil)))
      (set-char-table-range table t `(["[-.,:;A-Z_a-z><=!&|+?/\\]+" 0 font-shape-gstring]))
      (set-char-table-parent table composition-function-table)
      (setq composition-function-table table))
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))
  (leaf ligature
    :ensure t
    :global-minor-mode global-ligature-mode)
  (leaf spacious-padding
    :ensure t
    :global-minor-mode spacious-padding-mode
    :custom
    (spacious-padding-widths .
                             '(:internal-border-width 15
                                                      :header-line-width 4
                                                      :mode-line-width 6
                                                      :tab-width 4
                                                      :right-divider-width 30
                                                      :scroll-bar-width 8
                                                      :fringe-width 8))
    ( spacious-padding-subtle-mode-line .
      '(:mode-line-active 'default
                          :mode-line-inactive vertical-border))))
(leaf *platform-spec
  :config
  (leaf *wsl-url-handler
    :when (or
           (getenv "WSL_DISTRO_NAME")
           (eq system-type 'windows-nt))
    :after browse-url
    :defun my/browse-url-via-powershell
    :config
    (defun my/browse-url-via-powershell (url &rest _args)
      (shell-command (concat "powershell.exe start \"" url "\"")))
    (setf browse-url-browser-function #'my/browse-url-via-powershell)))
(leaf exec-path-from-shell
  :ensure t
  :unless (eq system-type 'windows-nt)
  :defun exec-path-from-shell-initialize
  :hook (after-init-hook . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments . nil)
  (exec-path-from-shell-check-startup-files . nil)
  (exec-path-from-shell-variables . '("PATH" "MANPATH" "LD_LIBRARY_PATH")))
(leaf vertico
  :ensure t
  :bind (:vertico-map (("C-h" . 'vertico-directory-up)
                       ("C-m" . 'vertico-exit)
                       ("C-j" . 'vertico-exit-input)))
  :global-minor-mode vertico-mode)
(leaf embark
  :ensure t
  :bind
  ("C-." . 'embark-act)
  ("C-," . 'embark-dwim))
(leaf consult
  :ensure t
  :defun
  consult-customize
  consult--customize-put
  :defvar consult--source-buffer consult-buffer-sources
  :bind
  (("<leader>is" . consult-line)
   ("<leader>ii" . consult-imenu)
   ("<leader>iI" . consult-imenu-multi)
   ("<leader>ip" . consult-yank-from-kill-ring)
   ("<leader>il" . consult-compile-error)
   ("<leader>ig" . consult-ripgrep))
  :config
  (leaf embark-consult
    :ensure t
    :hook ((embark-collect-mode-hook . consult-preview-at-point-mode))))
(leaf marginalia
  :ensure t
  :global-minor-mode marginalia-mode)
(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(substring orderless basic))
  (completion-category-overrides . '((file (styles basic partial-completion)))))
(leaf *completion
  :config
  (leaf corfu
    :ensure t
    :defun corfu-quit
    :defvar corfu-map corfu-margin-formatters
    :global-minor-mode global-corfu-mode
    :hook (corfu-mode-hook . corfu-popupinfo-mode)
    :custom ((corfu-auto . t)
             (corfu-auto-delay . 0.3)
             (corfu-auto-prefix . 3)
             (corfu-cycle . t))
    :config
    (leaf *corfu-meow-espace
      :after meow
      :config
      (eval-when-compile
        (declare-function meow-escape-or-normal-modal "meow-command"))
      (define-key corfu-map (kbd "<escape>")
                  (lambda ()
                    (interactive)
                    (corfu-quit)
                    (meow-escape-or-normal-modal))))
    (leaf nerd-icons-corfu
      :ensure t
      :after nerd-icons
      :defun nerd-icons-corfu-formatter
      :config
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
    (leaf corfu-terminal
      :ensure t
      :unless (display-graphic-p)
      :vc (corfu-terminal
           :url "https://codeberg.org/akib/emacs-corfu-terminal.git")
      :after corfu
      :hook (corfu-mode-hook . corfu-terminal-mode)))
  (leaf cape
    :ensure t
    :config
    (defvar my/merged-capf)
    (let* ((noncachedfuns '(#'cape-dabbrev))
           (cachedfuns '(#'cape-file #'cape-rfc1345 #'cape-tex))
           (mergedfuns (eval `(cape-capf-super (cape-capf-buster (cape-capf-super ,@noncachedfuns)) (cape-capf-super ,@cachedfuns)))))
      (setq my/merged-capf mergedfuns)
      (add-to-list 'completion-at-point-functions my/merged-capf)))
  (leaf eglot
    :disabled nil
    :ensure t
    :custom ((eglot-autoshutdown . t)))
  (leaf realgud
    :disabled t
    :ensure t
    :config
    (leaf realgud-lldb
      :ensure t
      :require t))
  (leaf lsp-mode
    :ensure t
    :custom
    ((lsp-auto-guess-root . t)
     (lsp-document-sync-method . 'incremental)
     (lsp-response-timeout . 5)
     (lsp-use-plist . t)
     (lsp-log-io . nil)
     (lsp-semantic-tokens-enable . t)
     (lsp-enable-snippet . t)
     (lsp-diagnostics-provider . :flymake)
     (lsp-enable-completion . t)
     (lsp-completion-provider . :none)
     (lsp-modeline-diagnostics-scope . :file))
    :defun lsp-enable-which-key-integration
    :config
    (leaf *lsp-keybinds
      :custom
      ((lsp-keymap-prefix . "C-c l"))
      :bind
      ;; (:lsp-mode-map
      ;;  ("C-c l" . lsp-command-map))
      ;;(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
      :config
      (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
    (leaf lsp-ui
      :ensure t
      :custom
      ((lsp-ui-doc-enable . t)
       (lsp-ui-doc-header . t)
       (lsp-ui-doc-include-signature . t)
       (lsp-ui-doc-use-childframe . t)
       (lsp-ui-doc-use-webkit . t)
       (lsp-ui-flycheck-enable . nil)
       (lsp-ui-imenu-enable . nil)
       (lsp-ui-peek-enable . t)
       (lsp-ui-peek-fontify . 'on-demand))
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
      (add-hook 'lsp-mode-hook #'lsp-treemacs-sync-mode))))
(leaf perspective
  :ensure t
  :defvar persp-consult-source
  :custom
  (persp-mode-prefix-key . `,(kbd "C-x x"))
  (persp-sort . 'created)
  :global-minor-mode persp-mode
  :config
  (leaf *perspective-consult
    :after consult perspective
    :config
    (eval-when-compile
      (require 'consult))
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))
(leaf treemacs
  :ensure t
  :hook (treemacs-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :bind
  ("C-x t t" . treemacs-select-window)
  ("C-x t q" . treemacs)
  :config
  (leaf treemacs-magit
    :ensure t
    :after magit)
  (leaf treemacs-perspective
    :ensure t
    :after perpective))
(leaf imenu-list
  :ensure t)
(leaf vundo
  :ensure t)
(leaf apheleia
  :ensure t
  :global-minor-mode apheleia-global-mode)
(leaf shackle
  :ensure t
  :global-minor-mode shackle-mode
  :custom
  (shackle-rules . '((compilation-mode :align below :ratio 0.2)
                     ("*Flycheck errors*" :align 'below :ratio 0.2)
                     ("*Help*" :align right :ratio 0.5 :select t)
                     ("*Completions*" :align below :ratio 0.3)
                     ("*latex-math-preview-expression*" :align below :ratio 0.3 :noselect t))))
(leaf ace-window
  :ensure t)
(leaf winner
  :ensure nil
  :global-minor-mode winner-mode
  :bind
  (("<leader>ws" . delete-other-windows)
   ("<leader>wu" . winner-undo)
   ("<leader>wr" . winner-redo)
   ("<leader>w>" . enlarge-window-horizontally)
   ("<leader>w<" . shrink-window-horizontally)
   ("<leader>w." . enlarge-window)
   ("<leader>w," . shrink-window)
   ("<leader>w=" . balance-windows)))
(leaf which-key
  :ensure t
  :global-minor-mode which-key-mode
  :config
  (leaf which-key-posframe
    :ensure t
    :init
    (eval-when-compile
      (declare-function posframe-poshandler-frame-bottom-center "poframe"))
    :custom
    (which-key-posframe-poshandler . #'posframe-poshandler-frame-bottom-center)
    (which-key-posframe-border-width . 5)
    (which-key-posframe-parameters . '((left-fringe . 2)
                                       (right-fringe . 2)))
    :hook (which-key-mode-hook . which-key-posframe-mode)))
(leaf tramp
  :ensure t)
(leaf vterm
  :ensure t)
(leaf eshell
  :ensure t
  :config
  (leaf eshell-vterm
    :ensure t))
(leaf ddskk
  :ensure t
  :defvar skk-isearch-mode-enable
  :custom
  ((skk-kutouten-type . '("．" . "，"))
   (skk-use-azik . t)
   (skk-isearch-start-mode . 'latin)
   (skk-isearch-mode-enable . t)
   (default-input-method . "japanese-skk")
   (skk-large-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
   (skk-itaiji-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.itaiji")
   (skk-cdb-large-jisyo . "~/.emacs.d/SKK-JISYO.XL.cdb"))
  :bind (("C-x j" . skk-mode)
         ("C-x J" . skk-auto-fill-mode))
  :config
  (leaf ddskk-posframe
    :ensure t posframe
    :global-minor-mode t)
  (leaf *skk-isearch
    :hook
    (isearch-mode-hook . #'(lambda ()
                             (when (and (boundp 'skk-mode)
                                        skk-mode
                                        skk-isearch-mode-enable)
                               (skk-isearch-mode-setup))))
    (isearch-mode-end-hook . #'(lambda ()
                                 (when (and (featurep 'skk-isearch)
                                            skk-isearch-mode-enable)
                                   (skk-isearch-mode-cleanup))))))
;; (require '20-migemo)
;; (require '20-fcitx)
;; (require '20-uim)
;; (require '20-company)
(leaf flymake
  :ensure t
  :hook (prog-mode-hook . flymake-mode)
  :config
  (leaf flymake-collection
    :ensure t
    :require t
    :after flymake)
  (leaf flymake-diagnostic-at-point
    :ensure t
    :hook (flymake-mode-hook . flymake-diagnostic-at-point-mode)))
(leaf paren
  :ensure nil
  :global-minor-mode show-paren-mode)
(leaf elec-pair
  :ensure nil
  :global-minor-mode electric-pair-mode)
(leaf puni
  :ensure t
  :global-minor-mode puni-global-mode
  :hook (vterm-mode-hook . puni-disable-puni-mode))
(leaf highlight-indent-guides
  :ensure t
  :hook
  ((prog-mode-hook conf-mode-hook) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive . t)
  (highlight-indent-guides-method . 'fill))
(leaf transient
  :ensure t)
(leaf git-commit
  :ensure t)
(leaf magit
  :ensure t
  :bind
  (("C-x g" . #'magit-status))
  :init
  (leaf difftastic
    :ensure t
    :after magit
    :bind
    (:magit-blame-read-only-mode-map
     ("D" . 'difftastic-magit-show)
     ("S" . 'difftastic-magit-show)))
  :config
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show" difftastic-magit-show)]))
(leaf projectile
  :ensure t
  :hook ((prog-mode-hook . projectile-mode)
         (text-mode-hook . projectile-mode))
  :bind
  (:projectile-mode-map
   ("<leader>pp" . #'projectile-command-map)
   ("<leader>pf" . #'projectile-find-file)
   ("<leader>ps" . #'projectile-switch-project)
   ("<leader>pb" . #'projectile-switch-to-buffer)
   ("<leader>pd" . #'projectile-dired)
   ("<leader>pg" . #'projectile-ripgrep)
   ("<leader>pc" . #'projectile-compile-project)
   ("<leader>pr" . #'projectile-replace)))
;; (require '20-google-translate)
;; (require '30-bison)
;; (require '30-cmake)
(leaf posframe
  :ensure t
  :config
  )
(leaf tempel
  :ensure t
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (leaf tempel-collection
    :ensure t
    :require t
    :after tempel))
(leaf org
  :ensure t
  :custom
  (org-startup-indented . t)
  :config
  (leaf org-modern
    :ensure t
    :hook (org-mode-hook . org-modern-mode))
  (leaf org-modern-indent
    :ensure t
    :vc (org-modern-indent
         :url "https://github.com/jdtsmith/org-modern-indent")
    :config
    (add-hook 'org-mode-hook #'org-modern-indent-mode 90))
  (leaf org-roam
    :ensure t
    :custom
    ((org-roam-directory . "~/org/roam/")
     (org-roam-db-location . "~/.emacs.d/org-roam.db"))
    :config
    (org-roam-db-autosync-mode)))
(leaf pdf-tools
  :ensure t
  :hook (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1))))
(leaf *docker
  :config
  (leaf docker
    :ensure t)
  (leaf lsp-docker
    :ensure t)
  (leaf dockerfile-mode
    :ensure t))
;; (require '30-yaml)
;; (require '30-emacslisp)
;; (require '30-common-lisp)
;; (require '30-scheme)
;; (require '30-agda)
;; (require '30-ocaml)
;; (require '30-sml)
;; (require '30-fsharp)
;; (require '30-markdown)
;; (require '30-purescript)
;; (require '30-coq)
;; (require '30-maude)
;; (require '30-lean)
;; (require '30-pdf)
;; (require '31-lsp)
(leaf *languages
  :config
  (leaf nix-mode
    :ensure t)
  (leaf *c/cpp
    :ensure nil
    :config
    (leaf cmake-mode
      :ensure t))
  (leaf web-mode
    :ensure t
    :mode ("\\.csp\\'" "\\html\\'"))
  (leaf *elisp
    :config)
  (leaf *haskell
    :config
    (leaf haskell-mode
      :ensure t)
    (leaf haskell-snippets
      :ensure t))
  (leaf *rust
    :config
    (leaf rust-mode
      :ensure t
      :require t
      ;; :hook 'eglot-ensure
      :custom
      ((rust-indent-offset . 4)))
    (leaf cargo
      :ensure t))
  (leaf *fsharp
    :config
    (leaf fsharp-mode
      :ensure t
      :config
      (leaf *fsharp-company
        :after company
        :disabled t
        :config
        (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)))
    (leaf eglot-fsharp
      :ensure t
      :after fsharp-mode
      :require t))
  (leaf *sagemath
    :config
    (leaf sage-shell-mode
      :ensure t
      :custom
      ((sage-shell:use-prompt-toolkit . nil)
       (sage-shell:use-simple-prompt . t))))
  (leaf *latex
    :config
    (leaf auctex
      :custom
      ((TeX-engine . 'luatex)
       ;;(TeX-engine-alist . '((luatex "LuaTeX" "luatex.exe" "lualatex.exe --jobname=%(s-filename-only)" "luatex.exe")))
       (LaTeX-using-Biber . t)
       (TeX-PDF-mode . t)
       (TeX-source-correlate-mode . t)
       (TeX-source-correlate-method . 'synctex)
       (TeX-source-correlate-start-server . t)
       (TeX-parse-self . t)
       (TeX-auto-save . t)
       (reftex-plug-into-AUCTeX . t))
      :ensure t
      :defvar TeX-view-program-list TeX-error-list TeX-command-buffer
      :defun TeX-revert-document-buffer TeX-active-master TeX-output-extension
      :hook (LaTeX-mode-hook . turn-on-reftex)
      :config
      (leaf pdf-tools
        :ensure t
        :hook (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
        :custom
        ((TeX-view-program-selection . '((output-pdf "PDF Tools")))
         (TeX-view-program-list . '(("PDF Tools" TeX-pdf-tools-sync-view))))
        :config
        (pdf-tools-install))
      (leaf auctex-cluttex
        :ensure t
        ;; :custom
        ;; ((auctex-cluttex-program . "cluttex.exe")
        ;;  (auctex-cluttex-ClutTeX-command . '("ClutTeX" "cluttex.exe -e %(cluttexengine) %(cluttexbib) %(cluttexindex) %S %t" auctex-cluttex--TeX-run-ClutTeX nil
        ;;                                      (plain-tex-mode latex-mode)
        ;;                                      :help "Run ClutTeX")))
        :hook (LaTeX-mode-hook . auctex-cluttex-mode)
        :defun
        auctex-cluttex--TeX-ClutTeX-sentinel
        :advice
        (:after auctex-cluttex--TeX-ClutTeX-sentinel my/run-after-compilation-finished-funcs)
        :config
        (defun my/run-after-compilation-finished-funcs (&rest _args)
          "run AUCTeX's TeX-after-compilation-finished-functions hook. Ignore all ARGS"
          (unless TeX-error-list
            (run-hook-with-args 'TeX-after-compilation-finished-functions
                                (with-current-buffer TeX-command-buffer
                                  (expand-file-name
                                   (TeX-active-master (TeX-output-extension)))))))))
    (leaf *latex-lsp
      :disabled t
      :config
      (add-hook 'LaTeX-mode-hook #'lsp-bridge-mode)
      (add-hook 'plain-TeX-mode-hook #'lsp-bridge-mode))))
(leaf editorconfig
  :ensure t)
(leaf gptel
  :ensure t
  :defun gptel-api-key-from-auth-source
  :bind
  (("<leader>gg" . gptel-menu))
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source))
(leaf copilot
  :ensure t
  :vc (copilot
       :url "https://github.com/copilot-emacs/copilot.el")
  :custom
  (copilot-indent-offset-warning-disable . t)
  :hook
  (prog-mode-hook . copilot-mode)
  (conf-mode-hook . copilot-mode)
  :bind (:copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)))
(leaf meow
  :ensure t
  :require t
  :custom (meow-use-clipboard . t)
  :defun
  (meow-setup . init)
  (meow-motion-overwrite-define-key . meow-helpers)
  (meow-leader-define-key . meow-helpers)
  (meow-normal-define-key . meow-helpers)
  :defvar
  meow-cheatsheet-layout
  meow-cheatsheet-layout-qwerty
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
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
  (meow-setup)
  (meow-global-mode 1)
  (leaf *meow-leader-maps
    :config
    (meow-leader-define-key
     '("=" . apheleia-format-buffer)
     '("q" . previous-buffer)
     '("Q" . next-buffer)
     '("w" . ace-window)
     '("e" . embark-act)
     '("u" . vundo)
     '("I" . imenu-list)
     '("i e" . consult-flymake)
     '("i i" . consult-imenu)
     '("i p" . consult-yank-from-kill-ring)
     '("a a" . avy-goto-char)
     '("a l" . avy-goto-line))))

(load custom-file)
(setq file-name-handler-alist my/saved-file-name-handler-alist)
(setq gc-cons-threshold 16777216)
(garbage-collect)
(provide 'init)
;;; init.el ends here
