(install-when-compile 'exec-path-from-shell)

(setup-include "exec-path-from-shell"
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-copy-env "CAML_LD_LIBRARY_PATH")
  (exec-path-from-shell-initialize))

(provide-file)
