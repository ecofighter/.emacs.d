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
;; (require '20-migemo)
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
;; (require '30-cmake)
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
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets yaml-mode which-key utop tuareg sml-mode smart-mode-line slime-company shut-up shackle rustic rg rainbow-delimiters proof-general package-utils org-pomodoro org-plus-contrib org-evil omnisharp ocp-indent nlinum magit lsp-ui lsp-haskell latex-math-preview ivy-rtags hl-todo highlight-indent-guides gruvbox-theme ghub fsharp-mode flycheck-ocaml flycheck-irony exec-path-from-shell evil-terminal-cursor-changer evil-smartparens evil-org evil-leader evil-escape evil-collection ess eglot ddskk cuda-mode counsel-projectile company-reftex company-lsp company-lean company-irony-c-headers company-irony company-coq company-auctex cmake-mode cmake-ide clang-format cargo bison-mode avy-migemo auctex-latexmk))))
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
