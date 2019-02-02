;;; 10-ivy.el -- ivy settings; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'ivy)
(install-when-compile 'counsel)
(install-when-compile 'avy-migemo)
(install-when-compile 'projectile)
(install-when-compile 'counsel-projectile)

(add-hook 'after-init-hook #'ivy-mode)
(setq-default ivy-use-virtual-buffers t)
(setq-default enable-recursive-minibuffers t)
(add-hook 'ivy-mode-hook #'avy-migemo-mode)
(with-eval-after-load "avy-migemo"
  (require 'avy-migemo-e.g.zzz-to-char)
  (require 'avy-migemo-e.g.counsel)
  (require 'avy-migemo-e.g.ivy)
  (require 'avy-migemo-e.g.swiper))
(with-eval-after-load "evil-leader"
  (evil-leader/set-key
    "i i" 'counsel-imenu
    "i f" 'counsel-find-file
    "i r" 'counsel-recentf
    "i b" 'ivy-switch-buffer
    "i r" 'counsel-rg
    "i g" 'counsel-git
    "i p p" 'counsel-projectile-switch-project
    "i p f" 'counsel-projectile-find-file
    "i p b" 'counsel-projectile-switch-to-buffer
    "i M-x" 'counsel-M-x
    "i s" 'swiper))
(with-eval-after-load "ivy"
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done))

(provide '10-ivy)
;;; 10-ivy.el ends here
