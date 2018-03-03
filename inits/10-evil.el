(install-when-compile 'evil)
(install-when-compile 'evil-leader)
(install-when-compile 'evil-escape)
(install-when-compile 'evil-tabs)

(defun evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(setup "evil"
  (add-hook 'after-init-hook #'evil-mode)
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk"))

(setup "evil-leader"
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "C-i" 'previous-buffer)
  (evil-leader/set-key "<backtab>" 'next-buffer)
  (evil-leader/set-key "<SPC>" 'counsel-M-x)
  (evil-leader/set-key "q q" 'save-buffers-kill-emacs)
  (evil-leader/set-key "q f" 'delete-frame))

(setup "evil-escape"
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "fd")
  (global-set-key (kbd "<escape>") 'evil-escape))

;; (setup-include "evil-tabs"
;;   (global-evil-tabs-mode))

(provide-file)
