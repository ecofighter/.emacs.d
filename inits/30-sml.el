;;; 30-sml -- Standard ML settings
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'sml-mode)

(with-eval-after-load "sml-mode"
  (setq-default sml-program-name "poly"))

(provide '30-sml)
;;; 30-sml.el ends here
