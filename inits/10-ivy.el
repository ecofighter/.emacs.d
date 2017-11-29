(install-when-compile 'ivy)
(install-when-compile 'counsel)

(setup-include "ivy"
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (evil-leader/set-key
    "i f" 'counsel-find-file
    "i r" 'counsel-recentf
    "i b" 'ivy-switch-buffer
    "i g" 'counsel-rg
    "i M-x" 'counsel-M-x
    "i s" 'swiper)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done))
(setup-include "counsel")
(setup-include "swiper")

(provide-file)
