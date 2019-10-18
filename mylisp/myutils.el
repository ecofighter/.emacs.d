;;; myutils.el -- my utility funcs
;;; Commentary:
;;; Code:
(defun my/reopen-with-sudo ()
  "Reopen buffer with sudo."
  (interactive)
  (find-file (concat "/sudo::"
                     (expand-file-name (buffer-file-name)))))
(provide 'myutils)
;;; myutils.el ends here
