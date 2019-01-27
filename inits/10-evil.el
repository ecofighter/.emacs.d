(install-when-compile 'evil)
(install-when-compile 'evil-leader)
(install-when-compile 'evil-escape)
(install-when-compile 'evil-collection)
(install-when-compile 'evil-terminal-cursor-changer)

(defun evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(setup-expecting "evil"
  (setq-default evil-want-keybinding nil)
  (setq-default evil-want-C-i-jump nil)
  (add-hook 'after-init-hook #'evil-mode)
  (add-hook 'evil-mode-hook #'evil-collection-init))
(setup-after "evil"
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")
  (define-key evil-normal-state-map (kbd "M-.")
    `(menu-item "" evil-repeat-pop :filter
                ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd)))))

(setup-expecting "evil-leader"
  (add-hook 'after-init-hook #'(lambda ()
                                 (global-evil-leader-mode 1)
                                 (evil-leader/set-leader "<SPC>")
                                 (evil-leader/set-key "C-i" 'previous-buffer)
                                 (evil-leader/set-key "<backtab>" 'next-buffer)
                                 (evil-leader/set-key "<SPC>" 'counsel-M-x)
                                 (evil-leader/set-key
                                   "q q" 'save-buffers-kill-emacs
                                   "q f" 'delete-frame))))

(setup-expecting "evil-escape"
  (add-hook 'after-init-hook #'evil-escape-mode)
  (setq-default evil-escape-key-sequence "fd")
  (global-set-key (kbd "<escape>") 'evil-escape))

(setup-expecting "evil-terminal-cursor-changer"
  (setq-default evil-normal-state-cursor 'box)
  (add-hook 'after-init-hook
            (lambda ()
              (require 'evil-terminal-cursor-changer)
              (etcc-on))))

(provide-file)
