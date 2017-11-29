(install-when-compile 'exec-path-from-shell)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments '("-i"))
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(require 'el-init)
(el-init-provide)
