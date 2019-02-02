;;; 10-smart-mode-line.el -- pretty mode line; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'smart-mode-line)

(with-eval-after-load "smart-mode-line"
  (column-number-mode 1)
  (setq sml/name-width '(5 . 25))
  (setq sml/mode-width 'full)
  (setq sml/hidden-modes '(" Server" " WK" " ivy" "fd" " ElDoc" " Undo-Tree" " WS" " SP" " company"))
  (setq evil-normal-state-tag "<N>")
  (setq evil-insert-state-tag (propertize "<I>" 'face '((:background "#076678"))))
  (setq evil-visual-state-tag (propertize "<V>" 'face '((:background "#fe8019"))))
  (setq evil-replace-state-tag (propertize "<R>" 'face '((:background "#8f3f71"))))
  (setq evil-mode-line-format '(before . mode-line-front-space)))

(setq-default sml/no-confirm-load-theme t)
(if (daemonp)
    (add-hook 'after-make-frame-functions #'(lambda (frame)
                                              (with-selected-frame frame 
                                                (sml/setup))))
  (add-hook 'after-init-hook #'sml/setup))


(provide '10-smart-mode-line)
;;; 10-smart-mode-line.el ends here
