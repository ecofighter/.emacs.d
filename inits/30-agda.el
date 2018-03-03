;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

(autoload 'agda2-mode (! (let ((coding-system-for-read 'utf-8))
                           (shell-command-to-string "agda-mode locate"))))

(add-to-list 'auto-mode-alist '("\\.agda$" . agda2-mode))

(provide-file)
