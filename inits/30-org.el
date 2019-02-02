;;; 30-org -- My org-mode config; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(install-when-compile 'org-plus-contrib)
(install-when-compile 'org-evil)
(install-when-compile 'org-pomodoro)

(setup-after "org"
  (plist-put org-format-latex-options :scale 2.0)
  (setup-expecting "org-evil"
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              #'(lambda ()
                  (evil-org-set-key-theme)))
    (setup "evil-org-agenda"
      (evil-org-agenda-set-keys)))
  (add-hook 'org-mode-hook
            #'(lambda ()
                (org-babel-do-load-languages 'org-babel-load-languages
                                             '((emacs-lisp . t)
                                               (R . t)
                                               (julia . t)))
                (add-hook 'completion-at-point-functions
                          #'pcomplete-completions-at-point nil t))))

(with-eval-after-load 'org-clock
  (defun my:org-clock-out-and-save (&rest _rest)
    "save buffers and stop clock when clocking."
    (when (org-clocking-p)
      (org-clock-out)
      (save-some-buffers)))
  (add-hook 'delete-frame-hook #'my:org-clock-out-and-save))

(with-eval-after-load 'org-pomodoro
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (defun my:org-pomodoro-kill-when-active (&rest _rest)
    (when (org-pomodoro-active-p)
      (org-pomodoro-kill)))
  (add-hook 'delete-frame-functions #'my:org-pomodoro-kill-when-active))

(provide-file)
