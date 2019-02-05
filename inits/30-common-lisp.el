;;; 30-common-lisp.el -- common lisp; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'slime)
(install-when-compile 'slime-company)

(with-eval-after-load "slime"
  (setf slime-lisp-implementations
        `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
          (roswell ("ros" "-Q" "run"))))
  (setf slime-default-lisp 'roswell)
  (setf inferior-lisp-program "ros -Q run")
  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(rx bos "*slime-repl roswell*" eos)
  ;;                (display-buffer-reuse-window)
  ;;                (side            . right)
  ;;                (window-height   . 0.4)))
  (slime-setup '(slime-repl slime-fancy slime-company)))

(with-eval-after-load "shackle"
  (add-to-list 'shackle-rules '(slime-repl-mode :align right
                                           :size 0.4))
  ;; (add-to-list 'shackle-rules '("*inferior-lisp*" :noselect t :align bottom))
  (add-to-list 'shackle-rules '("*slime-repl-help*" :popup t)))

(eval-after-load "evil-leader"
  (evil-leader/set-key-for-mode 'lisp-mode
    "m '" 'slime))

(provide '30-common-lisp)
;;; 30-common-lisp.el ends here
