;;; init.el -- my config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
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
  (load bootstrap-file nil 'nomessage))

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
      (when ( or (not active-clients-or-frames)
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
(add-to-list 'load-path "~/.emacs.d/inits")
(leaf emacs
  :custom ((make-backup-files . nil)
           (indent-tabs-mode . nil)
           (select-enable-clipboard . t)
           (x-select-enable-clipboard-manager . t)
           (split-width-threshold . 80)
           (vc-handled-backends quote nil)
           (fill-column . 80)
           (tab-width . 2)
           (truncate-lines . t)
           (truncate-partial-width-windows . t)
           (inhibit-startup-screen . t)
           (enable-recusive-minibuffers . t))
  :config
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
  (leaf hl-line
    :global-minor-mode global-hl-line-mode)
  (leaf *bars
    :config
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)))
(leaf *graphics
  :config
  (leaf *theme
    :config
    (leaf mood-line
      :ensure t
      :init
      (mood-line-mode))
    (leaf modus-themes
      :ensure t
      :require t
      :config
      (load-theme 'modus-vivendi-tinted :no-confirm)))
  (leaf *font
    :config
    (setq use-default-font-for-symbols nil)
    (add-to-list 'default-frame-alist '(font . "Source Han Code JP-12"))
    (set-face-attribute 'default nil :font "Source Han Code JP-12")
    ;; (set-fontset-font t 'ascii (font-spec :family "Ricty Diminished" :size 14))
    ;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))
    (set-fontset-font t 'unicode (font-spec :family "Noto Sans CJK JP") nil 'append)))
(leaf exec-path-from-shell
  :ensure t
  :unless (equal system-type 'windows-nt)
  :require exec-path-from-shell
  :defun exec-path-from-shell-initialize
  :custom ((exec-path-from-shell-arguments . nil)
           (exec-path-from-shell-check-startup-files . nil))
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
  :config
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
  (leaf evil-leader
    :ensure t
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
;;(require '10-ivy)
(leaf *fido
  :config
  ;;(fido-vertical-mode 1)
  (leaf vertico
    :ensure t
    :bind (:vertico-map (("C-h" . 'vertico-directory-up)))
    :global-minor-mode vertico-mode)
  (leaf embark
    :ensure t
    :bind
    (("C-." . 'embark-act)
     ("C-;" . 'embark-dwim)))
  (leaf consult
    :ensure t
    :after vertico
    :config
    (leaf *consult-leader-key
      :after evil-leader
      :config (evil-leader/set-key
                "i s" 'consult-line
                "i i" 'consult-imenu
                "i I" 'consult-imenu-multi
                "i p" 'consult-yank-from-kill-ring
                "i l" 'consult-compile-error
                "i g" 'consult-ripgrep))
    (leaf embark-consult
      :ensure t
      :hook ((embark-collect-mode-hook . consult-preview-at-point-mode))))
  (leaf marginalia
    :ensure t
    :global-minor-mode marginalia-mode)
  (leaf orderless
    :ensure t
    :custom
    ((completion-styles . '(substring orderless basic)))))
(leaf shackle
  :ensure t
  :global-minor-mode shackle-mode
  :custom
  (shackle-rules . '((compilation-mode :align below :ratio 0.2)
                     ("*Flycheck errors*" :align 'below :ratio 0.2)
                     ("*Help*" :align right :ratio 0.5 :select t)
                     ("*Completions*" :align below :ratio 0.3)
                     ("*latex-math-preview-expression*" :align below :ratio 0.3 :noselect t))))
(leaf winner
  :ensure t
  :global-minor-mode winner-mode
  :config
  (leaf *winner-evil-leader
    :after evil-leader
    :config
    (evil-leader/set-key
      "w s" 'delete-other-windows
      "w u" 'winner-undo
      "w r" 'winner-redo
      "w >" 'enlarge-window-horizontally
      "w <" 'shrink-window-horizontally
      "w ." 'enlarge-window
      "w ," 'shrink-window
      "w =" 'balance-windows)))
(leaf which-key
  :ensure t
  :global-minor-mode which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (leaf which-key-posframe
    :ensure t
    :hook (which-key-mode-hook . which-key-posframe-mode)))
;; (require '10-hl-todo)
;; (require '10-editorconfig)
;; (require '10-smart-mode-line)
;; (require '10-tramp)
;; (require '10-ripgrep)
;; (require '20-eshell)
(leaf vterm
  :ensure t)
(leaf eshell
  :config
  (leaf eshell-vterm
    :ensure t)
  (leaf *eshell-evil-leader
    :after evil-leader
    :config
    (evil-leader/set-key
      "'" 'eshell)))
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
  (leaf company-box
    :hook (company-mode-hook . company-box-mode)))

;; (require '20-yasnippet)
;; (require '20-flymake)
;; (require '20-flycheck)
;; (require '20-smartparens)
;; (require '20-rainbow-delimiters)
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
    :config
    (leaf *smartparens-evil-leader
      :after evil-leader
      :config
      (evil-leader/set-key
        "p f s" 'sp-forward-slurp-sexp
        "p f b" 'sp-forward-barf-sexp
        "p b s" 'sp-backward-slurp-sexp
        "p b b" 'sp-backward-barf-sexp
        "p b u" 'sp-backward-unwrap-sexp
        "p w (" 'sp-wrap-round
        "p w [" 'sp-wrap-square
        "p w {" 'sp-wrap-curly
        "p w \"" 'my/sp-wrap-dquote
        "p u u" 'sp-unwrap-sexp
        "p u b" 'sp-backward-unwrap-sexp))))
;; (require '20-highlight-indent-guides)
(leaf highlight-indent-guides
  :ensure t
  :require t
  :blackout t
  :hook
  ((prog-mode-hook . highlight-indent-guides-mode)
   (highlight-indent-guides-mode-hook . highlight-indent-guides-auto-set-faces))
  :custom ((highlight-indent-guides-method . 'fill)))
;; (require '20-magit)
(leaf git-commit
  :ensure t)
(leaf magit
  :ensure t
  :bind
  (("C-x g" . #'magit-status)))
;; (require '20-google-translate)
;; (require '30-bison)
;; (require '30-cmake)
(leaf posframe
  :ensure t
  :config
  (leaf company-posframe
    :ensure t
    :after company
    :hook (company-mode-hook . company-posframe-mode)
    :blackout t)
  (leaf ddskk-posframe
    :ensure t
    :after skk
    :global-minor-mode t
    :blackout t))
(leaf yasnippet
  :ensure t
  :global-minor-mode yas-global-mode
  :bind ((:yas-minor-mode-map
          ("M-TAB" . #'yas-expand))))
(leaf lsp-bridge
  ;; :straight (lsp-bridge
  ;;            :host github
  ;;            :repo "manateelazycat/lsp-bridge"
  ;;            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  ;;            :build (:not compile))
  :after yasnippet
  :defun global-lsp-bridge-mode
  :init (global-lsp-bridge-mode)
  :custom ((lsp-bridge-tex-lsp-server . "digestif"))
  :config
  (leaf markdown-mode
    :ensure t))
(leaf eglot
  :disabled t
  :ensure nil
  :config
  (leaf flycheck-eglot
    :ensure t
    :after (flycheck eglot)
    :custom ((flycheck-eglot-exclusive . nil))
    :global-minor-mode global-flycheck-eglot-mode))
(leaf lsp-mode
  :disabled t
  :ensure nil
  :custom
  ((lsp-auto-guess-root . t)
   (lsp-use-plist . t)
   (lsp-enable-snippet . t)
   (lsp-diagnostics-provider . :flycheck)
   (lsp-enable-completion . t)
   (lsp-completion-provider . :capf)
   (lsp-modeline-diagnostics-scope . :file))
  :defun lsp-enable-which-key-integration
  :defer-config
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
    (add-hook 'lsp-mode-hook #'lsp-treemacs-sync-mode)))
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
        :ensure nil
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
      :hook ((LaTeX-mode-hook . auctex-cluttex-mode)))
    (leaf *latex-lsp
      :disabled t
      :config
      (add-hook 'LaTeX-mode-hook #'eglot-ensure)
      (add-hook 'plain-TeX-mode-hook #'eglot-ensure))))
;; (leaf lsp-latex
;;     :ensure t
;;     :commands lsp-latex-build lsp-latex-forward-search
;;     :defun my/start-server
;;     :custom
;;     ((lsp-latex-build-executable . "cluttex")
;;      (lsp-latex-build-args . '("-e" "lualatex" "-interaction=nonstopmode" "-synctex=1" "%f"))
;;      (lsp-latex-forward-search-executable . `,(pcase system-type
;;                                                 ('windows-nt "C:\\Users\\ecofi\\AppData\\Local\\SumatraPDF\\SumatraPDF.exe")
;;                                                 ('gnu/linux "zathura")))
;;      (lsp-latex-forward-search-args . `,(pcase system-type
;;                                           ('windows-nt '("-reuse-instance" "%p" "-forward-search" "%f" "%l"))
;;                                           ('gnu/linux '("--synctex-forward" "%l:1:%f" "%p")))))
;;     :init
;;     (defun my/start-server ()
;;       (unless (server-running-p)
;;         (server-start)))
;;     (add-hook 'LaTeX-mode-hook #'my/start-server)
;;     (add-hook 'plain-TeX-mode-hook #'lsp)
;;     (add-hook 'LaTeX-mode-hook #'lsp)
;;     (add-hook 'bibtex-mode-hook #'lsp)
;;     (leaf *lsp-latex-keybinds
;;       :after evil-leader
;;       :config
;;       `,(dolist (mode '(latex-mode tex-mode))
;;           (evil-leader/set-key-for-mode mode
;;             "m b" 'lsp-latex-build
;;             "m f" 'lsp-latex-forward-search))))
;;(require '31-eglot)
;; (require '32-c++)
;; (require '32-rust)
;; (require '32-haskell)
;; (require '32-scala)
;; (require '32-typescript)
;; (require '32-latex)

(leaf editorconfig
  :disabled t
  :ensure t)
(leaf copilot
  :straight (copilot
             :host github
             :repo "zerolfx/copilot.el"
             :files ("dist" "*.el"))
  :bind (:copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(leaf package-utils
  :ensure t)
(garbage-collect)
(provide 'init)
;;; init.el ends here
