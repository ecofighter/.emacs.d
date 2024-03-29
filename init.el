;;; init.el -- my config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq garbage-collection-messages t)
(setq package-enable-at-startup nil)
(require 'package)
(when (fboundp 'native-comp-available-p)
  (when (native-comp-available-p)
    (customize-set-variable 'package-native-compile t)))

(eval-and-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa"        . "https://melpa.org/packages/")
                       ;;("org"          . "https://orgmode.org/elpa/")
                       ("gnu"          . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords
    :ensure t
    :config
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    (leaf-keywords-init)))

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
(leaf *myutils
  :config
  (defun my/reopen-with-sudo ()
    "Reopen buffer with sudo."
    (interactive)
    (find-file (concat "/sudo::"
                       (expand-file-name (buffer-file-name)))))
  (require 'server)
  (defun my/exit ()
    "Exit from Emacs window.
this sxec 'server-edit' when in client, or 'save-buffers-kill-emacs'."
    (interactive)
    (if server-clients
        (server-edit)
      (save-buffers-kill-emacs)))

  (defun client-save-kill-emacs (&optional display)
    "Save buffers and shutdown the Emacs daemon, use DISPLAY if passed.
It should be called using emacsclient -e '(client-save-kill-emacs)'.
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
        (when (not (eq window-system 'x))
          (message "Initializing x windows system.")
          (x-initialize-window-system))
        (when (not display) (setq display (getenv "DISPLAY")))
        (message "Opening frame on display: %s" display)
        (select-frame (make-frame-on-display display '((window-system . x)))))
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

      (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))))

  (defun modified-buffers-exist ()
    "Check to see if there are any buffers that have been modified.
It will return true if there are and nil otherwise.
Buffers that have 'buffer-offer-save' set to nil are ignored."
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
      modified-found)))
;;(add-to-list 'load-path "~/.emacs.d/inits")
(leaf emacs
  :custom ((make-backup-files . nil)
           (indent-tabs-mode . nil)
           (select-enable-clipboard . t)
           (x-select-enable-clipboard-manager . t)
           (split-width-threshold . 80)
           (vc-handled-backends quote nil)
           (fill-column . 80)
           (tab-width . 4)
           (truncate-lines . t)
           (truncate-partial-width-windows . t)
           (inhibit-startup-screen . t)
           (enable-recusive-minibuffers . t)
           (completion-cycle-threshold . 3)
           (tab-always-indent . 'complete))
  :init
  ;; credit: yorickvP on Github
                                        ; (defvar my/wl-copy-process nil)
                                        ; (defun my/wl-copy (text)
                                        ;   (setq my/wl-copy-process (make-process :name "wl-copy"
                                        ;                                          :buffer nil
                                        ;                                          :command '("wl-copy" "-f" "-n")
                                        ;                                          :connection-type 'pipe))
                                        ;   (process-send-string my/wl-copy-process text)
                                        ;   (process-send-eof my/wl-copy-process))
                                        ; (defun my/wl-paste ()
                                        ;   (if (and my/wl-copy-process (process-live-p my/wl-copy-process))
                                        ;       nil ; should return nil if we're the current paste owner
                                        ;     (shell-command-to-string "wl-paste -n | tr -d \r")))
                                        ; (setq interprogram-cut-function #'my/wl-copy)
                                        ; (setq interprogram-paste-function #'my/wl-paste)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'cperl-indent-level 'tab-width)
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (leaf whitespace
    :require t
    :global-minor-mode global-whitespace-mode
    :blackout t
    :custom
    ((show-trailing-whitespace . t)
     (whitespace-style . '(face trailing indentation tab-mark))))
  (leaf display-line-numbers
    :ensure nil
    :global-minor-mode global-display-line-numbers-mode)
  (leaf hl-line
    :global-minor-mode global-hl-line-mode)
  (leaf hl-todo
    :ensure t
    :global-minor-mode global-hl-todo-mode)
  (leaf treesit
    :ensure nil
    :custom (treesit-font-lock-level . 4)
    :config
    (leaf treesit-auto
      :ensure t
      :global-minor-mode global-treesit-auto-mode))
  (leaf *bars
    :config
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)))
(leaf *graphics
  :init
  (leaf *theme
    :config
    (leaf mood-line
      :disabled t
      :ensure t
      :init
      (mood-line-mode))
    (leaf modus-themes
      :ensure t
      :require t
      :config
      (load-theme 'modus-vivendi-tinted :no-confirm)))
  (leaf *font
    :init
    (setq use-default-font-for-symbols t)
    ;; (add-to-list 'default-frame-alist '(font . "Moralerspace Neon-11"))
    (set-face-attribute 'default nil :family "Moralerspace Neon" :height 110)
    ;; (set-fontset-font t 'ascii (font-spec :family "Ricty Diminished" :size 14))
    ;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))
    ;; (set-fontset-font t 'unicode (font-spec :family "Noto Sans CJK JP") nil 'append)
    (leaf *composition-table
      :init
      (let ((table (make-char-table nil)))
        (set-char-table-range table t `(["[-.,:;A-Z_a-z><=!&|+?/\\]+" 0 font-shape-gstring]))
        (set-char-table-parent table composition-function-table)
        (setq composition-function-table table))
      ;; (set-char-table-range composition-function-table t `(["[,-.;A-Z_a-z]+" 0 font-shape-gstring]))
      ;; (defun set-buffer-local-composition-table (value)
      ;;   (let ((table (make-char-table nil)))
      ;;     (set-char-table-range table t `([,value 0 font-shape-gstring]))
      ;;     (set-char-table-parent table composition-function-table)
      ;;     (setq-local composition-function-table table)))
      ;; (defun set-prog-mode-table ()
      ;;   (set-buffer-local-composition-table "[-.,:;A-Z_a-z><=!&|+?/\\]+"))
      ;; (add-hook 'prog-mode-hook #'set-prog-mode-table)
      )
    (leaf ligature
      :ensure t
      :global-minor-mode global-ligature-mode)))
(leaf *platform-spec
  :config
  (leaf *wsl-url-handler
    :when (or
           (equal system-name "waltraute")
           t)
    :after browse-url
    :config
    (defun my/browse-url-via-powershell (url &rest args)
      (shell-command (concat "powershell.exe start \"" url "\"")))
    (setf browse-url-browser-function #'my/browse-url-via-powershell)))
(leaf exec-path-from-shell
  :ensure t
  :unless (equal system-type 'windows-nt)
  :require exec-path-from-shell
  :defun exec-path-from-shell-initialize
  :custom ((exec-path-from-shell-arguments . nil)
           (exec-path-from-shell-check-startup-files . nil)
           (exec-path-from-shell-variables . '("PATH" "MANPATH" "LD_LIBRARY_PATH")))
  :config
  ;;(add-to-list 'exec-path-from-shell-variables "CAML_LD_LIBRARY_PATH")
  (exec-path-from-shell-initialize))
(leaf tab-bar-mode
  :global-minor-mode t)
(leaf evil
  :ensure t
  :require t
  :custom
  ((evil-want-C-i-jump . t)
   (evil-want-keybinding . nil)
   (evil-overriding-maps . nil)
   (evil-want-integration . t)
   (evil-want-keybinding . nil)
   (evil-normal-state-tag . "<N>")
   (evil-insert-state-tag . `,(propertize "<I>" 'face '((:background "#076678"))))
   (evil-visual-state-tag . `,(propertize "<V>" 'face '((:background "#fe8019" :foreground "#232323"))))
   (evil-replace-state-tag . `,(propertize "<R>" 'face '((:background "#8f3f71"))))
   (evil-operator-state-tag . `,(propertize "<O>" 'face '((:background "#3f3f3f"))))
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
   ("gk" . evil-previous-line)
   ("gc" . tab-bar-new-tab))
  (:evil-normal-state-map
   ("<leader><leader>" . execute-extended-command)
   ("<leader>C-i" . previous-buffer)
   ("<leader><backtab>" . next-buffer)
   ("<leader>kk" . kill-buffer-and-window)
   ("<leader>kb" . kill-buffer)
   ("<leader>kf" . delete-frame)
   ("<leader>kt" . toggle-frame-maximized)
   ("<leader>qq" . my/exit))
  :config
  (evil-set-leader '(normal visual) (kbd "SPC"))
  (define-key evil-normal-state-map (kbd "M-.")
              `(menu-item "" evil-repeat-pop :filter
                          ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))
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
  (leaf evil-collection
    :ensure t
    :after evil
    :defun evil-collection-init
    :custom
    :config
    (evil-collection-init))
  (leaf evil-anzu
    :ensure t
    :after evil
    :require t
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
  :hook (treemacs-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :config
  (leaf treemacs-evil
    :ensure t
    :after evil
    :require t
    :bind
    (("<leader>tt" . treemacs)
     ("<leader>ts" . treemacs-select-window))))
(leaf *fido
  :config
  (leaf vertico
    :ensure t
    :bind (:vertico-map (("C-h" . 'vertico-directory-up)))
    :global-minor-mode vertico-mode)
  (leaf embark
    :bind
    (("<leader>e" . embark-act)))
  (leaf consult
    :ensure t
    :after vertico
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
    :require t
    :custom
    ((completion-styles . '(substring orderless basic))
     (completion-category-overrides . '((file (styles basic partial-completion)))))))
(leaf *completion
  :config
  (leaf corfu
    :ensure t
    :global-minor-mode global-corfu-mode
    :hook (corfu-mode-hook . corfu-popupinfo-mode)
    :custom ((corfu-auto . t)
             (corfu-auto-delay . 0.3)
             (corfu-auto-prefix . 3)
             (corfu-cycle . t))
    :config
    (leaf corfu-terminal
      :straight (corfu-terminal
                 :type git
                 :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
      :after corfu
      :hook (corfu-mode-hook . corfu-terminal-mode)))
  (leaf cape
    :ensure t
    :require t
    :config
    (defvar my/merged-capf)
    (let* ((noncachedfuns '(#'cape-dabbrev))
           (cachedfuns '(#'cape-file #'cape-rfc1345 #'cape-tex))
           (mergedfuns (eval `(cape-capf-super (cape-capf-buster (cape-capf-super ,@noncachedfuns)) (cape-capf-super ,@cachedfuns)))))
      (setq my/merged-capf mergedfuns)
      (add-to-list 'completion-at-point-functions my/merged-capf)))
  (leaf lsp-bridge
    :disabled t
    :straight (lsp-bridge
               :host github
               :repo "manateelazycat/lsp-bridge"
               :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
               :build (:not compile))
    :ensure nil
    :defun global-lsp-bridge-mode
    ;; :init (global-lsp-bridge-mode)
    :hook ((lsp-bridge-mode-hook . (lambda () (corfu-mode -1)))
           (LaTeX-mode-hook . lsp-bridge-mode))
    :custom ((lsp-bridge-tex-lsp-server . "digestif")
             (lsp-bridge-c-lsp-server . "clangd")
             (acm-candidate-match-function . 'orderless-flex)
             (acm-enable-copilot . nil)
             (lsp-bridge-multi-lang-server-mode-list . '(((python-mode python-ts-mode) . lsp-bridge-python-multi-lsp-server)
                                                         ((qml-mode qml-ts-mode) . "qmlls_javascript")
                                                         ((js-mode javascript-mode) . "typescript_eslint"))))
    :config
    (leaf *lsp-bridge-evil-state
      :after evil
      :bind
      (("<leader>ld" . lsp-bridge-diagnostic-list))
      :config
      (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs))
    (leaf markdown-mode
      :ensure t))
  (leaf eglot
    :disabled nil
    :ensure t
    :custom ((eglot-autoshutdown . t))
    :config
    (leaf flycheck-eglot
      :disabled t
      :ensure t
      :after (flycheck eglot)
      :custom ((flycheck-eglot-exclusive . nil))
      :global-minor-mode global-flycheck-eglot-mode))
  (leaf realgud
    :ensure t
    :config
    (leaf realgud-lldb
      :ensure t
      :require t))
  (leaf lsp-mode
    :disabled t
    :ensure nil
    :custom
    ((lsp-auto-guess-root . t)
     (lsp-use-plist . t)
     (lsp-semantic-tokens-enable . t)
     (lsp-enable-snippet . t)
     (lsp-diagnostics-provider . :flycheck)
     (lsp-enable-completion . t)
     (lsp-completion-provider . :capf)
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
      (add-hook 'lsp-mode-hook #'lsp-treemacs-sync-mode))))
(leaf *aspell
  :config
  (leaf flycheck-aspell
    :ensure t))
(leaf shackle
  :ensure t
  :global-minor-mode shackle-mode
  :custom
  (shackle-rules . '((compilation-mode :align below :ratio 0.2)
                     ("*Flycheck errors*" :align 'below :ratio 0.2)
                     ("*Help*" :align right :ratio 0.5 :select t)
                     ("*Completions*" :align below :ratio 0.3)
                     ("*latex-math-preview-expression*" :align below :ratio 0.3 :noselect t))))
(leaf switch-window
  :ensure t
  :custom (switch-window-shortcut-style . 'qwert)
  :bind
  ("C-x o" . #'switch-window))
(leaf ace-window
  :ensure t
  :bind
  (("<leader>a" . #'ace-window)))
(leaf winner
  :ensure nil
  :require t
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
  :init
  (leaf which-key-posframe
    :ensure t
    :after which-key
    :custom
    ((which-key-posframe-poshandler . #'posframe-poshandler-frame-bottom-center)
     (which-key-posframe-border-width . 5)
     (which-key-posframe-parameters . '((left-fringe . 2)
                                        (right-fringe . 2))))
    :hook (which-key-mode-hook . which-key-posframe-mode)))
;; (require '10-hl-todo)
;; (require '10-editorconfig)
;; (require '10-smart-mode-line)
;; (require '10-tramp)
(leaf tramp
  :ensure t
  :config
  (leaf *tramp-wslhost-from-wsl
    :when (and
           (equal system-type 'gnu/linux)
           (equal system-name "waltraute"))
    :after tramp
    :config
    (let ((winhost-addr (shell-command-to-string
                         "ip route | grep 'default via' | grep -Eo '[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}' | tr -d '\n'")))
      (setf (alist-get "wslhost" tramp-methods nil nil #'equal)
            `((tramp-login-program "ssh")
              (tramp-login-args (("-l" "%u")
                                 ("-p" "%p")
                                 ("-e" "none")
                                 ("-t")
                                 ,winhost-addr))
              ;;(tramp-async-args (("-q")))
              ;;(tramp-direct-async t)
              (tramp-remote-shell "powershell.exe")
              (tramp-remote-shell-login ("-File -"))
              (tramp-remote-shell-args ("-Command"))
              (tramp-connection-timeout 5)
              (tramp-session-timeout 5))))))
;; (require '10-ripgrep)
;; (require '20-eshell)
(leaf vterm
  :ensure t)
(leaf eshell
  :bind
  (("<leader>'" . eshell))
  :config
  (leaf eshell-vterm
    :ensure t))
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
   (skk-large-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
   (skk-itaiji-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.itaiji")
   (skk-cdb-large-jisyo . "~/.emacs.d/SKK-JISYO.XL.cdb"))
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
;; (require '20-company)
(leaf company
  :disabled t
  :ensure nil
  :custom ((company-selection-wrap-around . t)
           ;; (company-backends . '(company-capf company-yasnippet company-files company-dabbrev-code))
           (company-minimum-prefix-length . 2)
           (company-idle-delay . 0.3))
  :bind
  ("C-c c t" . #'company-mode)
  (:company-active-map
   ("<tab>" . #'company-select-next-if-tooltip-visible-or-complete-selection)
   ("TAB" . #'company-select-next-if-tooltip-visible-or-complete-selection))
  :config
  (leaf company-posframe
    :ensure t
    :after company
    :hook (company-mode-hook . company-posframe-mode)
    :blackout t)
  (leaf company-box
    :hook (company-mode-hook . company-box-mode)))
(leaf flymake
  :ensure t
  :config
  (leaf flymake-diagnostic-at-point
    :ensure t
    :hook (flymake-mode-hook . flymake-diagnostic-at-point-mode)))
(leaf flycheck
  :ensure t)
(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))
(leaf smartparens
  :ensure t
  :global-minor-mode smartparens-global-mode
  :defun my/sp-wrap-dquote sp-wrap-with-pair
  :require smartparens-config
  :config
  (defun my/sp-wrap-dquote ()
    (interactive)
    (sp-wrap-with-pair "\""))
  (leaf evil-smartparens
    :ensure t
    :after evil
    :hook (smartparens-enabled-hook . evil-smartparens-mode)
    :bind
    (("<leader>sfs" . sp-forward-slurp-sexp)
     ("<leader>sfb" . sp-forward-barf-sexp)
     ("<leader>sbs" . sp-backward-slurp-sexp)
     ("<leader>sbb" . sp-backward-barf-sexp)
     ("<leader>sbu" . sp-backward-unwrap-sexp)
     ("<leader>suu" . sp-unwrap-sexp)
     ("<leader>sub" . sp-backward-unwrap-sexp))
    (:evil-visual-state-map
     ("<leader>sw(" . sp-wrap-round)
     ("<leader>sw[" . sp-wrap-square)
     ("<leader>sw{" . sp-wrap-curly)
     ("<leader>sw\"" . sp-wrap-dquote))))
(leaf highlight-indent-guides
  :ensure t
  :require t
  :blackout t
  :hook
  ((prog-mode-hook . highlight-indent-guides-mode)
   (highlight-indent-guides-mode-hook . highlight-indent-guides-auto-set-faces))
  :custom ((highlight-indent-guides-method . 'fill)))
(leaf visual-fill-column
  :ensure t
  :hook (visual-line-mode-hook . visual-fill-column-mode)
  :custom ((visual-fill-column-width . 80)
           (visual-fill-column-center-text . t)))
(leaf transient
  :ensure t)
(leaf git-commit
  :ensure t)
(leaf magit
  :ensure t
  :bind
  (("C-x g" . #'magit-status)))
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
  (leaf ddskk-posframe
    :ensure t
    :after skk
    :global-minor-mode t
    :blackout t))
(leaf yasnippet
  :ensure t
  :global-minor-mode yas-global-mode
  :bind ((:yas-minor-mode-map
          ("M-TAB" . #'yas-expand)))
  :config
  (leaf yasnippet-snippets
    :ensure t))
;;(require '30-org)
(leaf org
  :ensure t
  :config
  ;; (leaf org-plus-contrib
  ;;   :ensure t)
  (leaf evil-org
    :ensure t
    :hook ((org-mode-hook . evil-org-mode))
    :defun evil-org-set-key-theme
    :config
    (evil-org-set-key-theme
     '(navigation insert textobjects additional calendar))))
(leaf pdf-tools
  :ensure t
  :hook (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1))))
(leaf *docker
  :config
  (leaf docker
    :ensure t)
  (leaf lsp-docker
    :ensure t)
  (leaf docker-tramp
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
    :ensure smartparens
    :require smartparens-config
    :defun my/elisp-mode-hook-fun
    :config
    (defun my/elisp-mode-hook-fun ()
      (hs-minor-mode 1)
      (smartparens-strict-mode 1)
      (flycheck-mode -1))
    (add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-hook-fun))
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
      :defvar TeX-view-program-list
      :defun TeX-revert-document-buffer
      :config
      (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
      (leaf pdf-tools
        :ensure t
        :require t
        :custom
        ((TeX-view-program-selection . '((output-pdf "PDF Tools")))
         (TeX-view-program-list . '(("PDF Tools" TeX-pdf-tools-sync-view))))
        :config
        (pdf-tools-install)
        (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))
    (leaf auctex-cluttex
      :ensure t
      :custom
      ;; ((auctex-cluttex-program . "cluttex.exe")
      ;;  (auctex-cluttex-ClutTeX-command . '("ClutTeX" "cluttex.exe -e %(cluttexengine) %(cluttexbib) %(cluttexindex) %S %t" auctex-cluttex--TeX-run-ClutTeX nil
      ;;                                      (plain-tex-mode latex-mode)
      ;;                                      :help "Run ClutTeX")))
      :hook ((LaTeX-mode-hook . auctex-cluttex-mode))
      :config
      (defun my/run-after-compilation-finished-funcs (&rest args)
        "run AUCTeX's TeX-after-compilation-finished-functions hook. Ignore all ARGS"
        (unless TeX-error-list
          (run-hook-with-args 'TeX-after-compilation-finished-functions
                              (with-current-buffer TeX-command-buffer
                                (expand-file-name
                                 (TeX-active-master (TeX-output-extension)))))))
      (advice-add #'auctex-cluttex--TeX-ClutTeX-sentinel :after #'my/run-after-compilation-finished-funcs))
    (leaf *latex-lsp
      :disabled t
      :config
      (add-hook 'LaTeX-mode-hook #'lsp-bridge-mode)
      (add-hook 'plain-TeX-mode-hook #'lsp-bridge-mode))))
(leaf editorconfig
  :ensure t)
(leaf gptel
  :ensure t
  :bind
  (("<leader>gg" . gptel-menu))
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source))
(leaf copilot
  :straight (copilot
             :host github
             :repo "copilot-emacs/copilot.el"
             :files ("dist" "*.el"))
  ;; :hook (prog-mode-hook . copilot-mode)
  :bind (:copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(leaf package-utils
  :ensure t)

(load custom-file)
(garbage-collect)
(provide 'init)
;;; init.el ends here
