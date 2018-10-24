(install-when-compile 'evil)
(install-when-compile 'evil-leader)
(install-when-compile 'evil-escape)

(defun evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(setup-expecting "evil"
  (add-hook 'after-init-hook #'evil-mode))
(setup-after "evil"
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk"))

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

(provide-file)
