;;; early-init.el --- my early-init-file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setopt custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setopt file-name-handler-alist nil)
(setopt gc-cons-threshold most-positive-fixnum)
(setopt garbage-collection-messages t)
(setopt inhibit-redisplay t)
(setopt inhibit-message t)

(defun my/restore-variables-after-init ()
  "Restore variables changed in `early-init.el'."
  (setopt file-name-handler-alist my/saved-file-name-handler-alist)
  (setopt gc-cons-threshold (* 128 1024 1024))
  (setopt gc-cons-percentage 0.2)
  (setopt inhibit-redisplay nil)
  (setopt inhibit-message nil)
  (redisplay)
  (garbage-collect))
(add-hook 'after-init-hook #'my/restore-variables-after-init)

(setopt load-prefer-newer t)
(setopt read-process-output-max (* 1024 1024))
(setopt native-comp-async-report-warnings-errors 'silent)
(setopt native-comp-async-jobs-number 8)
(setopt native-comp-speed 2)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(width . 130) default-frame-alist)
(push '(height . 40) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)
;; (push '(fullscreen . maximized) default-frame-alist)

(require 'package)
(setopt package-enable-at-startup t)

;; Never use `package-quickstart'.  Packages are supplied from outside
;; `package-user-dir' -- under Nix, `emacsWithPackagesFromUsePackage' installs
;; them into a store path that shows up in `package-directory-list' -- and
;; quickstart is actively harmful then: `package-activate-all' short-circuits to
;; `load'ing the quickstart file, so `package-load-all-descriptors' never runs
;; and `package-alist' stays empty.  In that state `package-installed-p'
;; consults only `package-activated-list' and never sees the externally supplied
;; packages, so every `:ensure t' looks uninstalled and gets re-downloaded from
;; ELPA on each startup.  Note that the short-circuit tests for the quickstart
;; *file*, not the variable, so a stale file has to be removed as well --
;; otherwise clearing `package-quickstart' changes nothing.  This runs before
;; `package-activate-all', which is what makes the deletion effective.
(setopt package-quickstart nil)
(dolist (file (list (concat package-quickstart-file "c")
                    package-quickstart-file))
  (when (file-exists-p file)
    (with-demoted-errors "Cannot remove stale package quickstart file: %S"
      (delete-file file))))

;; Supplying packages is Nix's job.  The `:ensure' keywords in `init.el' exist
;; for `emacsWithPackagesFromUsePackage' to parse; they are not a licence to
;; download anything at runtime.  Neutralise the runtime half of `:ensure' so
;; that an Emacs which never received the wrapper's `EMACSLOADPATH' -- an
;; unwrapped `Emacs.app' left behind in the LaunchServices database, say, whose
;; `package-directory-list' therefore misses the Nix store -- says so instead of
;; quietly refetching every package into `package-user-dir' and appearing to
;; work.  The declarations stay in the file, so what Nix parses is unchanged.
(require 'use-package-ensure)

(defvar my/packages-not-supplied nil
  "Packages `:ensure'd by `init.el' that were not supplied to this Emacs.")

(defun my/use-package-ensure-noop (name args &rest _)
  "Report the packages ARGS asks for as missing rather than installing them.
Used as `use-package-ensure-function'.  ARGS is the list of `:ensure'
values, and is destructured exactly as `use-package-ensure-elpa' does it,
so that `:ensure nil' on a built-in stays silent.  Reporting has to happen
here and not from `after-init-hook', because a missing package usually
breaks the rest of `init.el' long before that hook runs."
  (dolist (ensure args)
    (let ((package (or (and (eq ensure t) (use-package-as-symbol name))
                       ensure)))
      (when (and package (not (package-installed-p package)))
        (my/report-package-not-supplied package)))))

(defun my/report-package-not-supplied (package)
  "Warn that PACKAGE was not supplied, and will not be installed."
  (unless my/packages-not-supplied
    (display-warning
     'init
     (format "`package-alist' holds %d package(s).  When that is 0, this \
Emacs was started without the `emacsWithPackages' wrapper that sets \
`EMACSLOADPATH', so `package-directory-list' never picked up the Nix store \
-- look at which Emacs binary is running rather than at this file."
             (length package-alist))
     :error))
  (push package my/packages-not-supplied)
  (display-warning
   'init (format "%s is not supplied; refusing to install it" package) :error))

(setopt use-package-ensure-function #'my/use-package-ensure-noop)

(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
;;; early-init.el ends here
