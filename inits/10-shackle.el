(install-when-compile 'shackle)

(setup-expecting "shackle"
  (setq shackle-rules
        '((compilation-mode :align below :ratio 0.2)
          ("*Help*" :align right :ratio 0.5 :select t)
          ("*Completions*" :align below :ratio 0.3)))
  (add-hook 'after-init-hook #'shackle-mode))

(provide-file)
