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

(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
;;; early-init.el ends here
