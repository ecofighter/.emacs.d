(install-when-compile 'exec-path-from-shell)

(setup-in-idle "exec-path-from-shell")
(setup-after "exec-path-from-shell"
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-check-startup-files nil)
  (add-to-list 'exec-path-from-shell-variables "CAML_LD_LIBRARY_PATH")
  (exec-path-from-shell-initialize))

(provide-file)
