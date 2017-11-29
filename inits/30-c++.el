(install-when-compile 'cmake-ide)
(install-when-compile 'rtags)
(install-when-compile 'ivy-rtags)
(install-when-compile 'irony)
(install-when-compile 'company-irony)
(install-when-compile 'company-irony-c-headers)
(install-when-compile 'flycheck-irony)
(install-when-compile 'clang-format)

(add-hook 'c++-mode-hook #'yas-minor-mode-on)
(add-hook 'c++-mode-hook #'flycheck-mode-on-safe)


;; (use-package cmake-ide
;; 	:defer t
;; 	:init (add-hook 'c++-mode-hook #'(lambda ()
;; 																		 (eval-after-load 'rtags
;; 																			 (cmake-ide-setup))))
;; 	:config
;; 	(setq cmake-ide-build-dir "build")
;; 	(evil-leader/set-key-for-mode 'c++-mode
;; 		"m c" 'cmake-ide-compile))

(setup-lazy '(cmake-ide-setup) "cmake-ide")
(setup-expecting "cmake-ide"
  (add-hook 'c++-mode-hook #'(lambda ()
                               (setup "rtags"
                                 (setq cmake-ide-build-dir "build")
                                 (cmake-ide-setup)))))

(setup-after "cmake-ide"
  (evil-leader/set-key-for-mode 'c++-mode
    "m c" 'cmake-ide-compile))

;; (use-package rtags
;; 	:defer t
;; 	:init
;; 	(evil-leader/set-key-for-mode 'c++-mode
;; 		"m r s" 'rtags-find-symbol-at-point)
;; 	(evil-leader/set-key-for-mode 'c++-mode
;; 		"m r r" 'rtags-find-references-at-point)
;; 	:config
;; 	(require 'ivy-rtags)
;; 	(setq rtags-display-result-backend 'ivy)
;; 	(setq rtags-autostart-diagnostics t))

(setup-after "rtags"
  (setup "ivy-rtags")
  (setq rtags-display-result-backend 'ivy)
  (setq rtags-autostart-diagnostics t)
  (evil-leader/set-key-for-mode 'c++-mode
    "m r s" 'rtags-find-symbol-at-point)
  (evil-leader/set-key-for-mode 'c++-mode
    "m r r" 'rtags-find-references-at-point))

;; (use-package irony
;; 	:defer t
;; 	:init (add-hook 'c++-mode-hook #'irony-mode)
;; 	:config
;; 	(eval-after-load 'cmake-ide (irony-cdb-autosetup-compile-options)))

(setup-lazy '(irony-mode) "irony"
  (setup-after "cmake-ide"
    (irony-cdb-autosetup-compile-options)))
(setup-expecting "irony"
  (add-hook 'c++-mode-hook #'irony-mode))

;; (use-package company-irony
;; 	:defer t
;; 	:init (progn
;; 					(add-hook 'c++-mode-hook
;; 										#'(lambda ()
;; 												(eval-after-load 'company
;; 													'(add-to-list
;; 														'company-backends 'company-irony))))
;; 					(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))
;; 	:config
;; 	(setq company-backends (delete 'company-semantic company-backends)))

(setup-after "irony"
  (setup-after "company"
    (setup "company-irony"
      (add-hook 'irony-mode-hook
                #'(lambda ()
                    (add-to-list 'company-backends 'company-irony)
                    (setq company-backends
                          (delete 'company-semantic company-backends))
                    (company-irony-setup-begin-commands))))))

;; (use-package company-irony-c-headers
;; 	:defer t
;; 	:init (add-hook 'c++-mode-hook
;; 									#'(lambda ()
;; 											(eval-after-load 'company
;; 												'(add-to-list 'company-backends 'company-irony-c-headers)))))

(setup-after "irony"
  (setup-after "company"
    (setup "company-irony-c-headers"
      (add-hook 'irony-mode-hook
                #'(lambda ()
                    (add-to-list 'company-backends 'company-irony-c-headers))))))

;; (use-package flycheck-irony
;;   :defer t
;;   :init (add-hook 'c++-mode-hook
;;                   #'(lambda ()
;;                       (eval-after-load 'flycheck
;;                         '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))))

(setup-lazy '(flycheck-irony-setup) "flycheck-irony")
(setup-after "flycheck"
  (add-hook 'irony-mode-hook #'flycheck-irony-setup))

;; (use-package clang-format
;;   :defer t
;;   :commands (clang-format-buffer)
;;   :init
;;   (evil-leader/set-key-for-mode 'c++-mode
;;     "m =" 'clang-format-buffer)
;;   :config
;;   (setq clang-format-style "file"))

(setup-lazy '(clang-format-buffer) "clang-format")
(evil-leader/set-key-for-mode 'c++-mode
  "m =" 'clang-format-buffer)

(provide-file)
