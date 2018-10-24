(add-hook 'after-init-hook #'winner-mode)
(setup-after "evil-leader"
  (evil-leader/set-key
    "w s" 'delete-other-windows
    "w u" 'winner-undo
    "w r" 'winner-redo))

(provide-file)
