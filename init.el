;;; init.el --- my init.el           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(setq
 package-archives
 '(("melpa"        . "http://melpa.org/packages/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("org"          . "http://orgmode.org/elpa/")
   ("gnu"          . "http://elpa.gnu.org/packages/")))

;; (eval-when-compile
;; (when (boundp 'package-pinned-packages)
;;   (setq package-pinned-packages
;;         '((evil      . "melpa-stable")))))
(package-initialize)

(defun file-name-to-symbol (filename)
  "FILENAME to symbol."
  (intern
   (file-name-nondirectory
    (file-name-sans-extension filename))))

(defun provide-file ()
  "Provide filename."
  (provide
   (file-name-to-symbol
    (or load-file-name
        (buffer-file-name)))))

(add-to-list 'load-path "~/.emacs.d/mylisp")
(require 'mymacros)
(add-to-list 'load-path "~/.emacs.d/inits")
(require '00-init)
(require '01-graphics)
(require '02-exec-path-from-shell)
(require '10-evil)
(require '10-ivy)
(require '10-shackle)
(require '10-winner)
(require '10-which-key)
(require '10-hl-todo)
(require '10-smart-mode-line)
;; (require '10-ripgrep)
(require '20-eshell)
(require '20-ddskk)
(require '20-migemo)
(require '20-company)
(require '20-yasnippet)
(require '20-flycheck)
(require '20-smartparens)
(require '20-rainbow-delimiters)
(require '20-highlight-indent-guides)
(require '20-magit)
(require '30-emacslisp)
;; (require '30-c++)
;; (require '30-bison)
;; (setup-include "30-cmake")
(require '30-latex)
(require '30-org)
(require '30-common-lisp)
(require '30-agda)
(require '30-ocaml)
(require '30-fsharp)
(require '30-markdown)
(require '30-coq)
(require '30-lean)
(require '31-lsp)
(require '32-rust)
(require '32-haskell)

(install-when-compile 'package-utils)
(provide 'init)
;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(avy-migemo-function-names
   (quote
    (swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "595617a3c537447aa7e76ce05c8d43146a995296ea083211225e7efc069c598f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (org-plus-contrib proof-general yasnippet-snippets ess company-reftex evil-terminal-cursor-changer latex-math-preview evil-org multi-term evil-collection org-pomodoro cargo rust-mode yaml-mode omnisharp highlight-indent-guides fsharp-mode eglot sml-mode company-coq cmake-mode lsp-ui winner-mode rg edit-indirect ripgrep company-lean lean-mode migemo which-key utop use-package tuareg spaceline smart-mode-line slime-company shackle rainbow-delimiters powerline-evil package-utils ocp-indent nlinum markdown-mode magit lsp-ocaml lsp-haskell key-chord ivy-rtags hl-todo gruvbox-theme flycheck-ocaml flycheck-irony exec-path-from-shell evil-tabs evil-smartparens evil-leader evil-escape ddskk counsel company-rtags company-quickhelp company-lsp company-irony-c-headers company-irony company-auctex cmake-ide clang-format bison-mode auctex-latexmk)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#32302f")))
 '(winner-mode t))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "#E6DB74"))))
 '(agda2-highlight-field-face ((t (:foreground "#A6E22E"))))
 '(agda2-highlight-function-face ((t (:foreground "#A6E22E"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#F92672"))))
 '(agda2-highlight-keyword-face ((t (:foreground "#66D9EF"))))
 '(agda2-highlight-module-face ((t (:foreground "#AE81FF"))))
 '(agda2-highlight-number-face ((t (:foreground "#AE81FF"))))
 '(agda2-highlight-postulate-face ((t (:foreground "#E6DB74"))))
 '(agda2-highlight-primitive-face ((t (:foreground "#CE4045"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "#E6DB74"))))
 '(font-lock-comment-face ((t (:foreground "#75715E")))))
