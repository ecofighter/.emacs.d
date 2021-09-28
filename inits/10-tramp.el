;;; 10-tramp.el -- tramp config; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)

(with-eval-after-load 'tramp
  (setq tramp-verbose 10)
  (pcase system-type
    ('windows-nt
     ;;(setq tramp-default-method 'scpx)
     ;; (customize-set-variable 'tramp-process-connection-type nil)
     ;; (customize-set-variable 'tramp-encoding-shell "powershell")
     ;;(setq w32-start-process-share-console t)
     ;;(setq w32-start-process-show-window t)
     ;; (customize-set-variable 'tramp-encoding-command-switch "-c")
     ;; (customize-set-variable 'tramp-encoding-command-interactive "-i")
     ;;(modify-coding-system-alist 'process "pwsh" '(utf-8-dos . utf-8-unix))
     ;;(modify-coding-system-alist 'process "wsl" '(utf-8-dos . utf-8-unix))
     ;;(customize-set-variable 'tramp-rsh-end-of-line "\r\n")
     ;;(customize-set-variable 'tramp-process-connection-type nil)
     (customize-set-variable 'tramp-sh-extra-args
                             '(("/bash\\'" . "-noediting -norc -noprofile -i")
                               ("/zsh\\'" . "-f +Z -V -i")))
     (setf (alist-get "wsl" tramp-methods nil nil #'equal)
           '((tramp-login-program "wsl")
             (tramp-login-args (("-u" "%u")
                                ("/bin/sh" "-i")))
             (tramp-remote-shell "/bin/bash")
             (tramp-remote-shell-login ("-l"))
             (tramp-connection-timeout 5)
             (tramp-session-timeout 5)
             (tramp-remote-shell-args ("-i" "-c")))))
             ;;(tramp-connection-timeout 5)
             ;;(tramp-session-timeout 5))))
    (_
     (add-to-list 'tramp-remote-path "~/.ghcup/bin"))))

;; tramp.sh l:5041

(provide '10-tramp)
;;; 10-tramp.el ends here
