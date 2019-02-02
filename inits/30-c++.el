;;; 30-c++.el -- settings to write C++; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'cmake-ide)
(install-when-compile 'clang-format)

(with-eval-after-load "yasnippet"
  (add-hook 'c-mode-hook #'yas-minor-mode-on)
  (add-hook 'c++-mode-hook #'yas-minor-mode-on))
(with-eval-after-load "flycheck"
  (add-hook 'c-mode-hook #'flycheck-mode-on-safe)
  (add-hook 'c++-mode-hook #'flycheck-mode-on-safe))

(defmacro do-c-and-c++-mode (&rest body)
  "Anaphoric macro provide mode to eval BODY for C and C++ mode."
  `(dolist (mode '(c++-mode c-mode))
     ,@body))

(dolist (hook '(c++-mode-hook c-mode-hook))
  (add-hook hook #'(lambda ()
                     (setq-default cmake-ide-build-dir "build")
                     (cmake-ide-setup))))

(eval-after-load "cmake-ide"
  (eval-after-load "evil-leader"
    (do-c-and-c++-mode
     (evil-leader/set-key-for-mode mode
       "m c" 'cmake-ide-compile))))

(eval-after-load "cmake-ide"
  (irony-cdb-autosetup-compile-options))
(dolist (hook '(c++-mode-hook c-mode-hook))
  (add-hook hook #'irony-mode))

(do-c-and-c++-mode
 (evil-leader/set-key-for-mode mode
   "m =" 'clang-format-buffer))

(provide '30-c++)
;;; 30-c++.el ends here
