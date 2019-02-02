;;; 10-shackle.el -- control popup windows; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'shackle)

(setq-default shackle-rules
              '((compilation-mode :align below :ratio 0.2)
                ("*Flycheck errors*" :align 'below :ratio 0.2)
                ("*Help*" :align right :ratio 0.5 :select t)
                ("*Completions*" :align below :ratio 0.3)
                ("*latex-math-preview-expression*" :align below :ratio 0.3 :noselect t)))
(add-hook 'after-init-hook #'shackle-mode)

(provide '10-shackle)
;;; 10-shackle.el ends here
