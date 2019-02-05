;;; 10-smart-mode-line.el -- pretty mode line; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'smart-mode-line)

(with-eval-after-load "smart-mode-line"
  (column-number-mode 1)
  (setq sml/name-width '(5 . 25))
  (setq sml/mode-width 'full)
  (setq sml/hidden-modes '(" Server" " WK" " ivy" "fd" " ElDoc" " Undo-Tree" " WS" " SP" " company")))

(setq-default sml/no-confirm-load-theme t)
(if (daemonp)
    (add-hook 'after-make-frame-functions #'(lambda (frame)
                                              (with-selected-frame frame 
                                                (sml/setup))))
  (add-hook 'after-init-hook #'sml/setup))


(provide '10-smart-mode-line)
;;; 10-smart-mode-line.el ends here
