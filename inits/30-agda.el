;;; 30-agda.el -- agda; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let* ((coding-system-for-read 'utf-8)
       (file (shell-command-to-string "agda-mode locate")))
  (autoload 'agda2-mode file))

(add-to-list 'auto-mode-alist '("\\.agda\\'" . agda2-mode))

;; (eval-after-load 'agda2-mode
;;   (custom-set-faces
;;    ;; custom-set-faces was added by Custom.
;;    ;; If you edit it by hand, you could mess it up, so be careful.
;;    ;; Your init file should contain only one such instance.
;;    ;; If there is more than one, they won't work right.
;;    '(agda2-highlight-datatype-face ((t (:foreground "#E6DB74"))))
;;    '(agda2-highlight-field-face ((t (:foreground "#A6E22E"))))
;;    '(agda2-highlight-function-face ((t (:foreground "#A6E22E"))))
;;    '(agda2-highlight-inductive-constructor-face ((t (:foreground "#F92672"))))
;;    '(agda2-highlight-keyword-face ((t (:foreground "#66D9EF"))))
;;    '(agda2-highlight-module-face ((t (:foreground "#AE81FF"))))
;;    '(agda2-highlight-number-face ((t (:foreground "#AE81FF"))))
;;    '(agda2-highlight-postulate-face ((t (:foreground "#E6DB74"))))
;;    '(agda2-highlight-primitive-face ((t (:foreground "#CE4045"))))
;;    '(agda2-highlight-primitive-type-face ((t (:foreground "#E6DB74"))))
;;    '(font-lock-comment-face ((t (:foreground "#75715E"))))))

(provide '30-agda)
;;; 30-agda.el ends here
