;;; 20-magit -- git tool;
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(provide '20-magit)
;;; 20-magit.el ends here
