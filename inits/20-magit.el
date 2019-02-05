;;; 20-magit -- git tool;
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'git-commit)
(install-when-compile 'magit)

(add-hook 'after-init-hook #'global-git-commit-mode)
(global-set-key (kbd "C-x g") 'magit-status)

(provide '20-magit)
;;; 20-magit.el ends here
