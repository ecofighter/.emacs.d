;;; Init.el -- my config -*- lexical-binding: t -*-

;;; Commentary:
;; using leaf.el

;;; Code:
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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
  :require t)
(leaf auto-compile
  :ensure t
  :custom (auto-compile-native-compile . t)
  :hook (emacs-lisp-mode-hook . auto-compile-on-save-mode))
(leaf server
  :ensure nil
  :defun server-edit server-delete-client
  :defvar server-clients)
(leaf *myutils
  :defun
  my/modified-buffers
  my/exit
  :config
  (defun my/reopen-with-sudo ()
    "Reopen buffer with sudo."
    (interactive)
    (find-file (concat "/sudo::"
                       (expand-file-name (buffer-file-name)))))
  (defun my/modified-buffers ()
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
      (setq modified-buffers (my/modified-buffers))
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
           (backup-inhibited . nil)
           (create-lockfiles . nil)
           (fast-but-imprecise-scrolling . t)
           (process-adaptive-read-buffering . t)
           (indent-tabs-mode . nil)
           (select-enable-clipboard . t)
           (x-select-enable-clipboard-manager . t)
           (use-file-dialog . nil)
           (use-short-answers . t)
           (window-min-height . 10)
           (window-min-width . 70)
           (split-width-threshold . 140)
           (split-height-threshold . 20)
           (vc-handled-backends . '(Git))
           (fill-column . 80)
           (tab-width . 4)
           (truncate-lines . nil)
           (truncate-partial-width-windows . nil)
           (inhibit-startup-screen . nil)
           (inhibit-x-resources . t)
           (inhibit-compacting-font-caches . nil)
           (inhibit-startup-buffer-menu . t)
           (blink-matching-paren . nil)
           (auto-mode-case-fold . nil)
           (bidi-inhibit-bpa . t)
           (enable-recusive-minibuffers . t)
           (completion-cycle-threshold . 3)
           (tab-always-indent . 'complete))
  :config
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'cperl-indent-level 'tab-width)
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
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
  (leaf treesit
    :ensure nil
    :custom (treesit-font-lock-level . 4))
  (leaf treesit-auto
    :ensure t
    :defun treesit-auto-add-to-auto-mode-alist
    :custom (treesit-auto-install . 'prompt)
    :global-minor-mode global-treesit-auto-mode
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)))
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
    :custom
    (modus-themes-italic-constructs . t)
    (modus-themes-bold-constructs . t)
    (modus-themes-mixed-fonts . t)
    (modus-themes-variable-pitch-ui . t)
    (modus-themes-custom-auto-reload . t)
    (modus-themes-disable-other-themes . t)
    (modus-themes-prompts . '(italic bold))
    (modus-themes-completions . '((matches . (extrabold))
                                  (selection . (semibold text-also))))
    (modus-themes-headings . '((1 . (variable-pitch 1.5))
                               (2 . (1.3))
                               (agenda-date . (1.3))
                               (agenda-structure . (variable-pitch light 1.8))
                               (t . (1.1))))
    :config
    (load-theme 'modus-operandi-tinted :no-confirm))
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
    (ef-themes-to-toggle . '(ef-elea-light ef-elea-dark))
    :config
    (mapc #'disable-theme custom-enabled-themes)
    (ef-themes-select 'ef-elea-light))
  (leaf nord-theme
    :ensure t)
  (leaf nerd-icons
    :ensure t
    :config
    (leaf nerd-icons-completion
      :ensure t
      :global-minor-mode nerd-icons-completion-mode)
    (leaf nerd-icons-dired
      :ensure t
      :hook (dired-mode-hook . nerd-icons-dired-mode)))
  (leaf *nano
    :config
    (leaf nano-theme
      :disabled t
      :ensure t
      :config
      (load-theme 'nano-dark t nil))
    (leaf nano-modeline
      :ensure t
      :defun nano-modeline-text-mode
      :require t
      :custom
      ;; (nano-modeline-position . #'nano-modeline-footer)
      (mode-line-format . nil)
      :hook
      (prog-mode-hook . nano-modeline-prog-mode)
      (conf-mode-hook . nano-modeline-prog-mode)
      (org-mode-hook . nano-modeline-org-mode)
      (pdf-view-mode-hook . nano-modeline-pdf-mode)
      (term-mode-hook . nano-modeline-term-mode)
      (xwidget-webkit-mode-hook . nano-modeline-xwidget-mode)
      (messages-buffer-mode-hook . nano-modeline-message-mode)
      (org-capture-mode-hook . nano-modeline-org-capture-mode)
      (org-agenda-mode-hook . nano-modeline-org-agenda-mode)
      :config
      (nano-modeline-text-mode t)))
  (leaf *doom
    :disabled t
    :config
    (leaf doom-themes
      :ensure t
      :config
      (load-theme 'doom-spacegrey t nil))
    (leaf doom-modeline
      :ensure t
      :global-minor-mode doom-modeline-mode
      :custom
      (doom-modeline-window-width-limit . 60)
      (doom-modeline-buffer-file-name-style . 'truncate-with-project)
      (doom-modeline-icon . t)
      (doom-modeline-buffer-modification-icon . t)
      (doom-modeline-buffer-state-icon . t)
      (doom-modeline-buffer-encoding . t)
      (doom-modeline-buffer-major-mode . t)
      (doom-modeline-major-mode-icon . t)
      (doom-modeline-major-mode-color-icon . nil)
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
    (imenu-list-major-mode-hook . hide-mode-line-mode))
  (leaf minions
    :disabled t
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
    :hook
    (prog-mode-hook . display-line-numbers-mode)
    (conf-mode-hook . display-line-numbers-mode))
  (leaf hl-line
    :global-minor-mode global-hl-line-mode)
  (leaf hl-todo
    :ensure t
    :global-minor-mode global-hl-todo-mode)
  (leaf perfect-margin
    :ensure t
    :custom (perfect-margin-visible-width . 140)
    :global-minor-mode perfect-margin-mode)
  (leaf spacious-padding
    :ensure t
    :global-minor-mode spacious-padding-mode)
  (leaf dashboard
    :ensure t
    :defun dashboard-insert-startupify-lists
    :custom
    (dashboard-force-refresh . t)
    (dashboard-center-content . t)
    (dashboard-vertically-center-content .t)
    (dashboard-display-icons . t)
    (dashboard-icon-type . 'nerd-icons)
    (dashboard-set-heading-icons . t)
    (dashboard-set-file-icons . t)
    (dashboard-startup-banner . 2)
    (dashboard-items . '((agenda . 10)
                         (projects . 10)
                         (bookmarks . 10)
                         (recents . 10)))
    :config
    (when (daemonp)
      (setq initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*"))))
    (dashboard-setup-startup-hook)))
(leaf fontaine
  :ensure t
  :defvar fontaine-current-preset
  :defun (my/check-font-preset . init.el)
  :global-minor-mode fontaine-mode
  :custom
  (fontaine-latest-state-file . `,(locate-user-emacs-file "fontaine-latest-state.eld"))
  (fontaine-presets . '((source-han
                         :default-family "Source Han Code JP"
                         :fixed-pitch-family "Source Han Code JP"
                         :variable-pitch-family "Source Han Sans")
                        (udev
                         :default-family "UDEVGothic"
                         :fixed-pitch-family "UDEVGothic"
                         :variable-pitch-family "Sarasa Gothic J"
                         :line-spacing 0.2)
                        (udev35
                         :default-family "UDEVGothic35"
                         :fixed-pitch-family "UDEVGothic35"
                         :variable-pitch-family "Sarasa Gothic J"
                         :line-spacing 0.2)
                        (ibmplex
                         :default-family "PlemolJP"
                         :fixed-pitch-family "PlemolJP"
                         :variable-pitch-family "IBM Plex Sans JP"
                         :line-spacing 0.2)
                        (ibmplex35
                         :default-family "PlemolJP35"
                         :fixed-pitch-family "PlemolJP35"
                         :variable-pitch-family "IBM Plex Sans JP"
                         :line-spacing 0.2)
                        (sarasa
                         :default-family "Sarasa Mono J"
                         :fixed-pitch-family "Sarasa Mono J"
                         :variable-pitch-family "Sarasa Gothic J"
                         :line-spacing 0.2)
                        (sarasaterm
                         :default-family "Sarasa Term J"
                         :fixed-pitch-family "Sarasa Term J"
                         :variable-pitch-family "Sarasa Gothic J"
                         :line-spacing 0.2)))
  :config
  (defun my/check-font-preset ()
    (unless fontaine-current-preset
      (fontaine-set-preset (or (fontaine-restore-latest-preset) 'udev))))
  (add-hook 'window-setup-hook #'my/check-font-preset))
(leaf *platform-spec
  :config
  (leaf alert
    :when (eq system-type 'linux)
    :ensure t
    :require t
    :custom
    (alert-default-style . 'libnotify))
  (leaf alert-toast
    :when (or (getenv "WSL_DISTRO_NAME")
              (eq system-type 'windows-nt))
    :ensure t
    :require t
    :custom
    (alert-default-style . 'toast))
  (leaf *wayland-clipboard
    :when (getenv "WAYLAND_DISPLAY")
    :defvar wl-copy-process
    :config
    ;; credit: yorickvP on Github
    (setq wl-copy-process nil)
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
  (leaf *pwsh-url-handler
    :when (or (getenv "WSL_DISTRO_NAME")
              (eq system-type 'windows-nt))
    :after browse-url
    :defun (my/browse-url-via-powershell . init)
    :config
    (defun my/browse-url-via-powershell (url &rest _args)
      (shell-command (concat "powershell.exe start \"" url "\"")))
    (setf browse-url-browser-function #'my/browse-url-via-powershell)))
(leaf exec-path-from-shell
  :ensure t
  :unless (eq system-type 'windows-nt)
  :defun exec-path-from-shell-initialize
  :hook (emacs-startup-hook . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments . nil)
  (exec-path-from-shell-check-startup-files . nil)
  (exec-path-from-shell-variables . '("PATH" "MANPATH" "LD_LIBRARY_PATH")))
(leaf recentf
  :ensure nil
  :custom
  (recentf-exclude . `(,(locate-user-emacs-file "bookmarks")
                       ,(locate-user-emacs-file "elpa")
                       "/tmp.*")))
(leaf eldoc
  :ensure nil
  :global-minor-mode global-eldoc-mode
  :config
  (leaf eldoc-box
    :ensure t
    :config
    (defun my/enable-eldoc-box-hover-mode-when-graphical ()
      (when (display-graphic-p)
        (eldoc-box-hover-mode 1)))
    :hook (eldoc-mode-hook . my/enable-eldoc-box-hover-mode-when-graphical)))
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
  :defvar consult--source-buffer consult-buffer-sources
  :bind
  ("C-c i e" . consult-flymake)
  ("C-c i i" . consult-imenu)
  ("C-c i l" . consult-line)
  ("C-c i p" . consult-yank-from-kill-ring)
  ("C-c i b" . consult-buffer)
  :config
  (leaf embark-consult
    :ensure t
    :hook (embark-collect-mode-hook . consult-preview-at-point-mode)))
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
    :custom
    (corfu-auto . t)
    (corfu-auto-delay . 0.3)
    (corfu-auto-prefix . 3)
    (corfu-cycle . t)
    :bind
    (:corfu-map
     ("TAB" . corfu-insert)
     ("<tab>" . corfu-insert)
     ("RET" . nil)
     ("<return>" . nil))
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
      :defun
      corfu-terminal-mode
      (my/toggle-corfu-terminal . init.el)
      :vc (corfu-terminal
           :url "https://codeberg.org/akib/emacs-corfu-terminal.git")
      :config
      (when (daemonp)
        (defun my/toggle-corfu-terminal ()
          "disable corfu-terminal-mode in graphical frames"
          (if (display-graphic-p)
              (corfu-terminal-mode -1)
            (corfu-terminal-mode +1)))
        (add-hook 'server-after-make-frame-hook #'my/toggle-corfu-terminal))))
  (leaf cape
    :ensure t
    :custom
    (cape-dabbrev-check-other-buffers . nil)
    :config
    (mapc (lambda (item)
            (add-to-list 'completion-at-point-functions item t))
          '(cape-dabbrev
            cape-keyword
            cape-file
            cape-tex))))
(leaf perspective
  :ensure t
  :defvar persp-consult-source
  :custom
  (persp-mode-prefix-key . `,(kbd "C-x x"))
  (persp-sort . 'created)
  :global-minor-mode persp-mode
  :config
  (with-eval-after-load 'consult
    (eval-when-compile
      (require 'consult)
      (declare-function consult--customize-put "ext:consult.el"))
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))
(leaf treemacs
  :disabled t
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
(leaf avy
  :ensure t
  :bind
  ("C-c j j" . avy-goto-word-or-subword-1)
  ("C-c j l" . avy-goto-line))
(leaf ace-window
  :ensure t
  :custom-face
  (aw-leading-char-face . '((t :height 4.0)))
  :config
  (leaf ace-window-posframe
    :ensure posframe
    :when (display-graphic-p)
    :hook (window-setup-hook . ace-window-posframe-mode)))
(leaf winner
  :ensure nil
  :global-minor-mode winner-mode
  :bind
  (:winner-mode-map
   ("C-c <left>" . winner-undo)
   ("C-c <right>" . winner-redo)))
(leaf which-key
  :ensure t
  :global-minor-mode which-key-mode
  :config
  (leaf which-key-posframe
    :disabled t
    :ensure t
    :defun (posframe-poshandler-frame-bottom-center . posframe)
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
  :bind
  ("C-x j" . skk-mode)
  ("C-x J" . skk-auto-fill-mode)
  :custom
  (skk-kutouten-type . '("．" . "，"))
  (skk-use-azik . t)
  (skk-isearch-start-mode . 'latin)
  (skk-isearch-mode-enable . t)
  (default-input-method . "japanese-skk")
  (skk-status-indicator . nil)
  (skk-show-tooltip . t)
  (skk-show-inline . nil)
  (skk-show-mode-show . t)
  (skk-show-mode-style . 'inline)
  (skk-inline-show-face . nil)
  :config
  (with-eval-after-load 'skk-vars
    (eval-when-compile (require 'skk-vars))
    (setq skk-get-jisyo-directory (locate-user-emacs-file "skk-get-jisyo/"))
    (setq skk-large-jisyo (expand-file-name "SKK-JISYO.L" skk-get-jisyo-directory))
    (setq skk-itaiji-jisyo (expand-file-name "SKK-JISYO.itaiji" skk-get-jisyo-directory))
    (setq skk-cdb-large-jisyo (expand-file-name "SKK-JISYO.L.cdb" skk-get-jisyo-directory)))
  (custom-set-faces '(skk-show-mode-inline-face ((t (:inherit default :background "white smoke" :foreground "SlateGray4")))))
  (leaf ddskk-posframe
    :ensure t
    :after skk
    :hook (skk-mode-hook . ddskk-posframe-mode)))
(leaf skk-isearch
  :ensure nil
  :defun
  skk-isearch-mode-setup
  skk-isearch-mode-cleanup
  :config
  (add-hook 'isearch-mode-hook #'(lambda ()
                                   (when (and (boundp 'skk-mode)
                                              skk-mode
                                              skk-isearch-mode-enable)
                                     (skk-isearch-mode-setup))))
  (add-hook 'isearch-mode-end-hook #'(lambda ()
                                       (when (and (featurep 'skk-isearch)
                                                  skk-isearch-mode-enable)
                                         (skk-isearch-mode-cleanup)))))
(leaf flymake
  :ensure t
  :defun flymake-eldoc-function
  :hook (prog-mode-hook . flymake-mode)
  :config
  (leaf flymake-collection
    :ensure t
    :require t
    :after flymake)
  (leaf flymake-popon
    :ensure t
    :hook (flymake-mode-hook . flymake-popon-mode)))
(leaf flycheck
  :disabled t
  :ensure t
  :global-minor-mode global-flycheck-mode
  :config
  (leaf flycheck-posframe
    :ensure t
    :hook (flycheck-mode-hook . flycheck-posframe-mode)))
(leaf ispell
  :custom
  (ispell-program-name . "hunspell")
  (ispell-really-hunspell . t)
  (ispell-dictionary . "en_US"))
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
  (highlight-indent-guides-auto-odd-face-perc . 10)
  (highlight-indent-guides-auto-even-face-perc . 15)
  (highlight-indent-guides-auto-top-odd-face-perc . 40)
  (highlight-indent-guides-auto-top-even-face-perc . 45)
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive . 'top)
  (highlight-indent-guides-method . 'column))
(leaf magit
  :ensure t difftastic
  :bind
  ("C-x g" . #'magit-status)
  (:magit-blame-read-only-mode-map
   ("D" . 'difftastic-magit-show)
   ("S" . 'difftastic-magit-show))
  :config
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show" difftastic-magit-show)]))
(leaf *org
  :config
  (defconst my/org-inbox-file "inbox.org"
    "Org file to use with org-capture.")
  (defvar my/org-prefix nil
    "Prefix map for org commands.")
  (define-prefix-command 'my/org-prefix)
  (leaf org
    :ensure t
    :defvar org-directory
    :custom
    (org-directory . "~/Documents/org")
    (org-startup-indented . nil)
    (org-startup-truncated . nil)
    (org-todo-keywords . '((sequence "TODO" "PROG" "|" "DONE")))
    (org-clock-out-remove-zero-time-clocks . t)
    (org-clock-clocked-in-display . 'frame-title)
    (org-capture-templates . `(("t" "Todo" entry
                                (file+headline ,(expand-file-name my/org-inbox-file org-directory)
                                               "Tasks")
                                "* TODO %?\n  %i\n  %a")
                               ("n" "Note" entry
                                (file+headline ,(expand-file-name my/org-inbox-file org-directory)
                                               "Notes")
                                "* %?\n  %i\n  %a")))
    :bind
    ("C-c a" . org-agenda)
    ("C-c o" . my/org-prefix)
    (:my/org-prefix
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
    (leaf org-pomodoro
      :ensure t
      :custom
      (org-pomodoro-audio-player . "play.sh")
      (org-pomodoro-start-sound-p . t))
    (leaf org-agenda
      :ensure nil
      :custom
      (org-agenda-span . 'day)
      (org-agenda-files . `(,(expand-file-name my/org-inbox-file org-directory)
                            ,(file-name-as-directory (expand-file-name "tasks/" org-directory))))
      (org-agenda-skip-scheduled-if-done . t)
      (org-agenda-include-deadlines . t)
      (org-agenda-include-diary . t)
      (org-agenda-block-separator . nil)
      (org-agenda-compact-blocks . t)
      :config
      (leaf org-super-agenda
        :ensure t
        :global-minor-mode org-super-agenda-mode
        :custom
        (org-super-agenda-groups . '((:name "Past but not finished"
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
        (org-agenda-custom-commands . '(("n" "Agenda and all TODOs"
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
                                                     '((:name "Past but not finished"
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
    (leaf org-modern
      :ensure t
      :custom
      (org-modern-star . 'fold)
      (org-modern-table . nil)
      :hook
      (org-mode-hook . org-modern-mode)
      (org-agenda-finalize-hook . org-modern-agenda))
    (leaf valign
      :ensure t
      :custom
      (valign-fancy-bar . t)
      :hook
      (org-mode-hook . valign-mode))
    (leaf org-tidy
      :ensure t)
    (leaf org-web-tools
      :ensure t)
    (leaf org-roam
      :ensure t emacsql-sqlite-builtin consult-org-roam
      :defvar org-roam-node-display-template
      :global-minor-mode org-roam-db-autosync-mode
      :custom
      (org-roam-directory . `,(expand-file-name "roam" org-directory))
      (org-roam-db-location . `,(locate-user-emacs-file "org-roam.db"))
      (org-roam-database-connector . 'sqlite-builtin)
      (org-roam-capture-templates . '(("p" "Permanent Note" plain "%?"
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
      (org-roam-node-display-template . `,(concat "${title:*} "
                                                  (propertize "${tags:10}" 'face 'org-tag))))))
(leaf literate-calc-mode
  :ensure t
  :config
  (with-eval-after-load 'org
    (eval-when-compile (require 'org))
    (require 'literate-calc-mode)))
(leaf graphviz-dot-mode
  :ensure t
  :config
  (with-eval-after-load 'org
    (eval-when-compile (require 'org))
    (setf (alist-get 'dot org-babel-load-languages) t))
  (with-eval-after-load 'org-src
    (eval-when-compile (require 'org-src))
    (setf (alist-get "dot" org-src-lang-modes nil nil #'string=) 'graphviz-dot-mode)))
(leaf pdf-tools
  :ensure t
  :config
  (pdf-tools-install))
(leaf *docker
  :disabled t
  :config
  (leaf docker
    :ensure t)
  (leaf dockerfile-mode
    :ensure t))
(leaf yasnippet
  :ensure t
  :global-minor-mode yas-global-mode
  :config
  (leaf yasnippet-capf
    :ensure t
    :config
    (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
(leaf eglot
  :ensure t
  :defun
  eglot-completion-at-point
  eglot-hover-eldoc-function
  :custom
  (eglot-autoshutdown . t)
  :hook (eglot-managed-mode-hook . my/eglot-capf)
  :config
  (leaf eglot-signature-eldoc-talkative
    :ensure t
    :advice
    (:override eglot-signature-eldoc-function eglot-signature-eldoc-talkative))
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf
                       #'cape-file
                       #'cape-tex)))))
(leaf lsp-mode
  :disabled t
  :ensure t
  :defvar lsp--sync-full lsp--sync-incremental
  :custom
  (lsp-auto-guess-root . t)
  (lsp-enable-file-watchers . nil)
  (lsp-enable-folding . nil)
  (lsp-enable-on-type-formatting . nil)
  (lsp-document-sync-method . nil)
  (lsp-response-timeout . 5)
  (lsp-use-plist . t)
  (lsp-log-io . nil)
  (lsp-semantic-tokens-enable . t)
  (lsp-enable-snippet . nil)
  (lsp-diagnostics-provider . :auto)
  (lsp-enable-completion . t)
  (lsp-completion-provider . :none)
  (lsp-modeline-diagnostics-scope . :workspace)
  (lsp-keymap-prefix . "C-c l")
  :hook (lsp-mode-hook . lsp-enable-which-key-integration)
  :init
  (setenv "LSP_USE_PLISTS" "true")
  :config
  (leaf lsp-ui
    :ensure t
    :custom
    (lsp-ui-doc-enable . t)
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-use-childframe . t)
    (lsp-ui-doc-use-webkit . nil)
    (lsp-ui-doc-position . 'at-point)
    (lsp-ui-doc-show-with-cursor . t)
    (lsp-ui-doc-show-with-mouse . nil)
    (lsp-ui-doc-alignment . 'window)
    (lsp-ui-flycheck-enable . t)
    (lsp-ui-imenu-enable . nil)
    (lsp-ui-peek-enable . t)
    (lsp-ui-peek-fontify . 'on-demand)
    :hook (lsp-mode-hook . lsp-ui-mode)
    :bind (:lsp-ui-mode-map
           ([remap xref-find-definitions]
            . lsp-ui-peek-find-definitions)
           ([remap xref-find-references]
            . lsp-ui-peek-find-references)))
  (leaf lsp-treemacs
    :ensure t
    :after treemacs
    :hook (lsp-mode-hook . lsp-treemacs-sync-mode)))
(leaf *languages
  :config
  (leaf text-mode
    :custom
    (text-mode-ispell-word-completion . nil))
  (leaf nix-mode
    :disabled t
    :ensure t)
  (leaf *c/cpp
    :config
    (leaf cmake-mode
      :ensure t))
  (leaf markdown-mode
    :ensure t)
  (leaf web-mode
    :ensure t
    :mode ("\\.csp\\'" "\\html\\'"))
  (leaf *elisp
    :config)
  (leaf *haskell
    :config
    (leaf haskell-mode
      :ensure t))
  (leaf *rust
    :config
    (leaf rust-mode
      :ensure t
      :defvar rust-mode-hook
      :custom
      (rust-indent-offset . 4)
      :hook
      (rust-mode-hook . eglot-ensure))
    (leaf rust-ts-mode
      :ensure t
      :custom
      (rust-ts-flymake-command . nil)
      :defer-config
      (setq-default rust-ts-mode-hook rust-mode-hook))
    (leaf cargo
      :ensure t))
  (leaf *fsharp
    :disabled t
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
    :disabled t
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
      (TeX-engine . 'luatex)
      (LaTeX-using-Biber . t)
      (TeX-PDF-mode . t)
      (TeX-source-correlate-mode . t)
      (TeX-source-correlate-method . 'synctex)
      (TeX-source-correlate-start-server . t)
      (TeX-parse-self . t)
      (TeX-auto-save . t)
      (reftex-plug-into-AUCTeX . t)
      :ensure t
      :defvar TeX-view-program-list TeX-error-list TeX-command-buffer
      :defun TeX-revert-document-buffer TeX-active-master TeX-output-extension
      :hook (LaTeX-mode-hook . turn-on-reftex)
      :config
      (leaf *preview-with-pdf-tools
        :after pdf-tools
        :hook (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
        :custom
        (TeX-view-program-selection . '((output-pdf "PDF Tools")))
        (TeX-view-program-list . '(("PDF Tools" TeX-pdf-tools-sync-view))))
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
      (add-hook 'LaTeX-mode-hook #'eglot-ensure)
      (add-hook 'plain-TeX-mode-hook #'eglot-ensure))))
(leaf editorconfig
  :ensure t
  :global-minor-mode editorconfig-mode)
(leaf gptel
  :ensure t
  :defun gptel-api-key-from-auth-source
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
  ;; (prog-mode-hook . copilot-mode)
  ;; (conf-mode-hook . copilot-mode)
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
     '("I" . imenu-list))))

(provide 'init)
;;; init.el ends here
