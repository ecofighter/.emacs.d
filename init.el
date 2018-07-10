;;; init.el --- my init.el
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/setup")
(require 'setup)
(setup-initialize)

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

(eval-when-compile
  (defvar *my/package-refreshed* nil)
  (defmacro install-when-compile (package)
    `(eval-when-compile
       (progn
         (unless (package-installed-p ,package)
           (unless *my/package-refreshed*
             (package-refresh-contents)
             (setq *my/package-refreshed* t))
           (package-install ,package))))))

(defun file-name-to-symbol (file-name)
  (intern
   (file-name-nondirectory
    (file-name-sans-extension file-name))))

(defun provide-file ()
  (provide
   (file-name-to-symbol
    (or load-file-name
        (buffer-file-name)))))

;; (install-when-compile 'use-package)
;; (eval-when-compile
;;   (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/inits")
(setup-include "00-init")
(setup-include "01-graphics")
(setup-include "02-exec-path-from-shell")
(setup-include "10-evil")
(setup-include "10-ivy")
(setup-include "10-shackle")
(setup-include "10-which-key")
;; (setup-include "10-powerline")
(setup-include "10-hl-todo")
(setup-include "10-gruvbox-theme")
(setup-include "10-smart-mode-line")
;; (setup-include "10-elscreen")
(setup-include "20-eshell")
(setup-include "20-ddskk")
(setup-include "20-migemo")
(setup-include "20-company")
(setup-include "20-yasnippet")
(setup-include "20-flycheck")
(setup-include "20-smartparens")
(setup-include "20-rainbow-delimiters")
(setup-include "20-magit")
(setup-include "30-c++")
(setup-include "30-bison")
(setup-include "30-latex")
(setup-include "30-org")
(setup-include "30-common-lisp")
(setup-include "30-haskell")
(setup-include "30-agda")
(setup-include "30-cuda")
(setup-include "30-ocaml")
(setup-include "30-markdown")
(setup-include "30-rust")
(setup-include "30-cmake")
(setup-include "30-coq")
(setup-include "30-sml")

(install-when-compile 'package-utils)
(eval-when-compile
  (progn
    (require 'package-utils)
    (if *my/package-refreshed*
        (package-utils-upgrade-all-no-fetch)
      (package-utils-upgrade-all))))
(provide 'init)
;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-migemo-function-names
   (quote
    (swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full)))
 '(lsp-haskell-process-path-hie "hie-8.2.2")
 '(package-selected-packages
   (quote
    (migemo which-key utop use-package tuareg spaceline smart-mode-line slime-company shackle rainbow-delimiters powerline-evil package-utils ocp-indent nlinum markdown-mode magit lsp-ocaml lsp-haskell key-chord ivy-rtags hl-todo gruvbox-theme flycheck-ocaml flycheck-irony exec-path-from-shell evil-tabs evil-smartparens evil-leader evil-escape ddskk counsel company-rtags company-quickhelp company-lsp company-irony-c-headers company-irony company-auctex cmake-ide clang-format bison-mode auctex-latexmk))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
