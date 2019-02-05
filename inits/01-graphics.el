;;; 01-graphics -- graphic settings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'gruvbox-theme)
(install-when-compile 'zenburn-theme)
(install-when-compile 'nlinum)
(install-when-compile 'hl-line)

;; (defconst *my/fontset-name* "myfontset")
;; (defconst *my/asciifont* "Inconsolata-13")
;; (defconst *my/jpfont* "Noto Sans Mono CJK JP")
;; ;; (defconst *my/greekfont* "DejaVu Sans Mono")
;; ;; (defconst *my/unicodefont* "XITS Math")
;; (defun my/setup-font (&optional frame)
;;   "Make fontset and apply it to FRAME."
;;   (with-selected-frame (or frame (selected-frame))
;;     (when window-system
;;       (let* ((fsn (concat "fontset-" *my/fontset-name*))
;;              (elt (list (cons 'font fsn))))
;;         (create-fontset-from-ascii-font *my/asciifont* nil *my/fontset-name*)
;;         ;; (set-fontset-font fsn 'greek (font-spec :name *my/greekfont*) nil 'append)
;;         ;; (set-fontset-font fsn 'unicode (font-spec :name *my/unicodefont*) nil 'append)
;;         (set-fontset-font fsn 'unicode (font-spec :name *my/jpfont*) nil 'append)
;;         ;; (set-fontset-font fsn 'japanese-jisx0213.2004-1 (font-spec :family *my/jpfont*) nil 'append)
;;         ;; (set-fontset-font fsn ?の (font-spec :family *my/jpfont*) nil 'prepend)
;;         (set-frame-font fsn)
;;         (add-to-list 'initial-frame-alist elt)
;;         (add-to-list 'default-frame-alist elt)
;;         (set-frame-parameter (selected-frame) 'font fsn)
;;         (remove-hook 'after-init-hook #'my/setup-font)
;;         (remove-hook 'after-make-frame-functions #'my/setup-font)))))
;; ;;; add hook to set font
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions #'my/setup-font)
;;   (when window-system
;;     (add-hook 'after-init-hook #'my/setup-font)))
(add-to-list 'default-frame-alist '(font . "Noto Sans Mono CJK JP-13"))

;;; load theme
(defvar *my/selected-theme* 'zenburn)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme *my/selected-theme* t))))
  (load-theme *my/selected-theme* t))

(setq inhibit-startup-screen t)

(add-hook 'after-init-hook #'(lambda ()
                               (progn
                                 (tool-bar-mode -1)
                                 (menu-bar-mode -1)
                                 (scroll-bar-mode -1))))

(require 'whitespace)
(setq show-trailing-whitespace t)
(setq whitespace-style '(face
                         trailing
                         indentation
                         tab-mark))
(set-face-background 'trailing-whitespace "#af3a03")
(add-hook 'after-init-hook #'global-whitespace-mode)

(add-hook 'after-init-hook #'global-nlinum-mode)
(setq-default nlinum-format "%4d")

(add-hook 'after-init-hook #'global-hl-line-mode)

(add-hook 'after-init-hook #'show-paren-mode)
(setq-default show-paren-style 'mixed)

(provide '01-graphics)
;;; 01-graphics.el ends here
