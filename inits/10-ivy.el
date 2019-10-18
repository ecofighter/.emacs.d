;;; 10-ivy.el -- ivy settings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'ivy)
(install-when-compile 'counsel)
(install-when-compile 'ivy-rich)
(install-when-compile 'amx)
(install-when-compile 'projectile)
(install-when-compile 'counsel-projectile)

(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'after-init-hook #'amx-initialize)
(with-eval-after-load "ivy"
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done))
(with-eval-after-load "evil-leader"
  (evil-leader/set-key
    "i i" 'counsel-imenu
    "i f" 'counsel-find-file
    "i r" 'counsel-recentf
    "i b" 'ivy-switch-buffer
    "i r" 'counsel-rg
    "i g" 'counsel-git
    "i y" 'counsel-yank-pop
    "i p p" 'counsel-projectile-switch-project
    "i p f" 'counsel-projectile-find-file
    "i p b" 'counsel-projectile-switch-to-buffer
    "i M-x" 'counsel-M-x
    "i s" 'swiper))

(provide '10-ivy)
;;; 10-ivy.el ends here
