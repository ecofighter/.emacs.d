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

(eval-when-compile
  (when (boundp 'package-pinned-packages)
    (setq package-pinned-packages
          '((evil      . "melpa-stable")))))
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

(install-when-compile 'use-package)
(eval-when-compile
  (require 'use-package))

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
(setup-include "20-ddskk")
(setup-include "20-company")
(setup-include "20-yasnippet")
(setup-include "20-flycheck")
(setup-include "20-smartparens")
(setup-include "20-rainbow-delimiters")
(setup-include "20-magit")
(setup-include "30-c++")
(setup-include "30-latex")
(setup-include "30-common-lisp")
(setup-include "30-haskell")
(setup-include "30-ocaml")
(setup-include "30-markdown")

(install-when-compile 'package-utils)
;; (eval-when-compile
;;   (progn
;;     (require 'package-utils)
;;     (if *my/package-refreshed*
;;         (package-utils-upgrade-all-no-fetch)
;;       (package-utils-upgrade-all))))
;; (provide 'init)
;;; init.el ends here
