;;; 10-winner.el -- Undo/Redo window manipulations; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'winner)

(add-hook 'after-init-hook #'winner-mode)

(with-eval-after-load "evil-leader"
  (evil-leader/set-key
    "w s" 'delete-other-windows
    "w u" 'winner-undo
    "w r" 'winner-redo))

(provide '10-winner)
;;; 10-winner.el ends here
