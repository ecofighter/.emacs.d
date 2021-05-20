;;; 10-evil.el -- my evil configs; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '00-init)
(install-when-compile 'evil)
(install-when-compile 'evil-leader)
(install-when-compile 'evil-escape)
(install-when-compile 'evil-anzu)
(install-when-compile 'evil-terminal-cursor-changer)

(defun evil-swap-key (map key1 key2)
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

;; (setq-default evil-want-keybinding nil)
(setq-default evil-want-abbrev-expand-on-insert-exit nil)
(setq-default evil-want-C-i-jump nil)
;; (add-hook 'evil-mode-hook #'evil-collection-init)
(add-hook 'after-init-hook #'evil-mode)
(with-eval-after-load "evil"
  (require 'evil-anzu)
  (customize-set-variable 'evil-undo-system 'undo-fu)
  (setq evil-normal-state-tag "<N>")
  (setq evil-insert-state-tag (propertize "<I>" 'face '((:background "#076678"))))
  (setq evil-visual-state-tag (propertize "<V>" 'face '((:background "#fe8019"))))
  (setq evil-replace-state-tag (propertize "<R>" 'face '((:background "#8f3f71"))))
  (setq evil-mode-line-format '(before . mode-line-front-space))
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char)
  ;; (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")
  (define-key evil-normal-state-map (kbd "M-.")
    `(menu-item "" evil-repeat-pop :filter
                ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd)))))

(autoload 'evil-leader/set-leader "evil-leader.el")
(add-hook 'evil-mode-hook #'(lambda ()
                              (global-evil-leader-mode 1)
                              (evil-leader/set-leader "<SPC>")
                              (evil-leader/set-key "C-i" 'previous-buffer)
                              (evil-leader/set-key "<backtab>" 'next-buffer)
                              (evil-leader/set-key "<SPC>" 'counsel-M-x)
                              (evil-leader/set-key
                                "q q" 'my/exit
                                "q Q" 'save-buffers-kill-emacs
                                "q f" 'delete-frame
                                "q t" 'toggle-frame-maximized)))

(add-hook 'evil-mode-hook #'evil-escape-mode)
(setq-default evil-escape-key-sequence "fd")
(global-set-key (kbd "<escape>") 'evil-escape)

(setq-default evil-normal-state-cursor 'box)
(add-hook 'after-init-hook
          (lambda ()
            (require 'evil-terminal-cursor-changer)
            (etcc-on)))

(provide '10-evil)
;;; 10-evil.el ends here
