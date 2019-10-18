;;; 30-scheme.el -- setting to write scheme
;;; Commentary:
;;; Code:
(require 'mymacros)

(install-when-compile 'geiser)

;; R7RS module file
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))

;; key bind for run geiser
(with-eval-after-load "evil-leader"
  (evil-leader/set-key-for-mode 'scheme-mode
    "m m" 'geiser
    "m RET" 'run-geiser))

;; set variables in geiser-impl
(progn
  (setq-default geiser-active-implementations '(chicken chez)))
;; set variables in geiser-chicken
(progn
  (setq-default geiser-chicken-binary "chicken-csi"))

(provide '30-scheme)
;;; 30-scheme.el ends here
