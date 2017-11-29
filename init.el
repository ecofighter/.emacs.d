(require 'package)
(setq package-enable-at-startup nil)
(setq
  package-archives
  '(("melpa"       . "http://melpa.org/packages/")
    ("org"         . "http://orgmode.org/elpa/")
    ("gnu"         . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar *my/package-refreshed* nil)

(defmacro install-when-compile (package)
  `(eval-when-compile
     (progn
       (unless (package-installed-p ,package)
         (unless *my/package-refreshed*
           (package-refresh-contents)
           (setq *my/package-refreshed* t))
         (package-install ,package)))))

; (install-when-compile 'use-package)
; (eval-when-compile
;   (require 'use-package))
; (require 'bind-key)

(install-when-compile 'el-init)
(el-init-load "~/.emacs.d/inits"
              :subdirectories '("."))

