;;; 01-graphics -- graphic settings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'gruvbox-theme)
(install-when-compile 'zenburn-theme)
;; (install-when-compile 'nlinum)
(install-when-compile 'hl-line)

(defun my/make-fontset (&optional frame)
  "Make my fontset not use FRAME."
  (with-selected-frame (or frame (selected-frame))
    (pcase system-type
      ('windows-nt
       (add-to-list 'default-frame-alist '(font . "Cascadia Mono")))
      ('gnu/linux
       (let* ((fsnn "myfont")
              (fsn (concat "fontset-" fsnn))
              (asciifont "Ricty-12:weight=normal:slant=normal")
              (jpfont "Ricty")
              (unicodefont "XITS Math"))
         (create-fontset-from-ascii-font asciifont nil fsnn)
         (set-fontset-font fsn 'unicode (font-spec :family jpfont) nil 'append)
         (set-fontset-font fsn 'unicode (font-spec :family unicodefont) nil 'append)
         ;; (set-fontset-font fsn 'greek (font-spec :family jpfont) nil 'prepend)
         ;; (set-fontset-font fsn 'unicode (font-spec :family jpfont) nil 'append)
         ;; (set-fontset-font fsn ?の (font-spec :family jpfont) nil 'prepend)
         ;; (set-fontset-font fsn 'japanese-jisx0213.2004-1 (font-spec :name jpfont) nil 'prepend)
         ;; (add-to-list 'face-font-rescale-alist '(*my/jpfont* . 0.85))
         (add-to-list 'default-frame-alist '(font . "fontset-myfont")))))
    (remove-hook 'after-make-frame-functions #'my/make-fontset)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/make-fontset)
  (when window-system
    (my/make-fontset)))

;;; load theme
(defvar *my/selected-theme* 'zenburn)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme *my/selected-theme* t))))
  (load-theme *my/selected-theme* t))

(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(require 'whitespace)
(setq show-trailing-whitespace t)
(setq whitespace-style '(face
                         trailing
                         indentation
                         tab-mark))
;; (set-face-background 'trailing-whitespace "#af3a03")
(global-whitespace-mode 1)

;; (add-hook 'after-init-hook #'global-nlinum-mode)
;; (setq-default nlinum-format "%4d")

(global-hl-line-mode 1)

;; (add-hook 'after-init-hook #'show-paren-mode)
;; (setq-default show-paren-style 'mixed)

(provide '01-graphics)
;;; 01-graphics.el ends here
