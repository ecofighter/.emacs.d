(install-when-compile 'eshell)

(setup-lazy '(eshell) "eshell"
  ;; (setq eshell-prompt-function
  ;;       (lambda ()
  ;;         (concat "["
  ;;                 (eshell/pwd)
  ;;                 (if (= (user-uid) 0) "]\n# " "]\n$ "))))
  )
(setup-expecting "eshell"
  (setup-expecting "evil-leader"
    (evil-leader/set-key
      "'" 'eshell)))

(provide-file)
