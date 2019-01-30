(install-when-compile 'gruvbox-theme)
(install-when-compile 'nlinum)
(install-when-compile 'hl-line)

(add-to-list 'default-frame-alist '(alpha . 100))
(add-to-list 'default-frame-alist '(font . "Noto Sans Mono CJK JP-12"))
(set-face-attribute 'default t :font "Noto Sans Mono CJK JP-12")
(setq inhibit-startup-screen t)
(add-hook 'after-init-hook #'(lambda ()
                               (progn
                                 (tool-bar-mode -1)
                                 (menu-bar-mode -1)
                                 (scroll-bar-mode -1))))

(setup-include "whitespace"
  (setq-default show-trailing-whitespace t)
  (setq-default whitespace-style '(face
                           trailing
                           indentation
                           tab-mark))
  (set-face-background 'trailing-whitespace "#af3a03")
  (add-hook 'after-init-hook #'global-whitespace-mode))

(setup-expecting "nlinum"
  (add-hook 'after-init-hook #'global-nlinum-mode)
  (setq-default nlinum-format "%4d"))

(setup-expecting "hl-line"
  (add-hook 'after-init-hook #'global-hl-line-mode))

(setup-expecting "gruvbox-theme"
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme 'gruvbox t))))
    (load-theme 'gruvbox t)))

(add-hook 'after-init-hook #'(lambda () (progn
                                          (show-paren-mode 1)
                                          (setq show-paren-style 'mixed))))

(provide-file)
