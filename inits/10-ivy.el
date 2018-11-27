(install-when-compile 'ivy)
(install-when-compile 'counsel)
(install-when-compile 'avy-migemo)
(install-when-compile 'projectile)
(install-when-compile 'counsel-projectile)

(setup-expecting "ivy"
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setup "avy-migemo"
    (setup "avy-migemo-e.g.swiper")
                                 (avy-migemo-mode 1))
  (evil-leader/set-key
    "i i" 'counsel-imenu
    "i f" 'counsel-find-file
    "i r" 'counsel-recentf
    "i b" 'ivy-switch-buffer
    "i g" 'counsel-git-grep
    "i p p" 'counsel-projectile-switch-project
    "i p f" 'counsel-projectile-find-file
    "i p b" 'counsel-projectile-switch-to-buffer
    "i M-x" 'counsel-M-x
    "i s" 'swiper)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done))
;; (setup-expecting "counsel")
;; (setup-expecting "swiper")

(provide-file)
