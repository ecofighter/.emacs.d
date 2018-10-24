(install-when-compile 'flycheck)

(setup-expecting "flycheck"
  (defun flycheck-toggle-window ()
    (interactive)
    (let ((window (flycheck-get-error-list-window)))
      (if window
          (quit-window nil window)
        (flycheck-list-errors))))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (defconst flycheck-error-list-format [("Line" 4 flycheck-error-list-entry-< :right-align t)
                                        ("Col" 3 nil :right-align t)
                                        ("Level" 8 flycheck-error-list-entry-level-<)
                                        ("ID" 15 t)
                                        ("Message (Checker)" 0 t)])
  (setup-after "evil-leader"
    (evil-leader/set-key
      "e l" 'flycheck-toggle-window
      "e p" 'flycheck-previous-error
      "e n" 'flycheck-next-error)))
  (setup-after "shackle"
    (add-to-list 'shackle-rules '("*Flycheck errors*" :align below :ratio 0.2)))

(provide-file)
