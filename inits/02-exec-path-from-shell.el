;;; 02-exec-path-from-shell -- get PATH from shell; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'exec-path-from-shell)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments nil)
(setq exec-path-from-shell-check-startup-files nil)
(add-to-list 'exec-path-from-shell-variables "CAML_LD_LIBRARY_PATH")
(exec-path-from-shell-initialize)

(provide '02-exec-path-from-shell)
;;; 02-exec-path-from-shell.el ends here
