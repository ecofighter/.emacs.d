(install-when-compile 'cmake-ide)
(install-when-compile 'rtags)
(install-when-compile 'ivy-rtags)
(install-when-compile 'irony)
(install-when-compile 'company-irony)
(install-when-compile 'company-irony-c-headers)
(install-when-compile 'flycheck-irony)
(install-when-compile 'clang-format)


(add-hook 'c-mode-hook #'yas-minor-mode-on)
(add-hook 'c-mode-hook #'flycheck-mode-on-safe)
(add-hook 'c++-mode-hook #'yas-minor-mode-on)
(add-hook 'c++-mode-hook #'flycheck-mode-on-safe)

(defmacro do-c-and-c++-mode (&rest body)
  "Anaphoric macro provide mode to eval BODY for C and C++ mode."
  `(dolist (mode '(c++-mode c-mode))
     ,@body))

(setup-lazy '(cmake-ide-setup) "cmake-ide")
(setup-expecting "cmake-ide"
  (dolist (hook '(c++-mode-hook c-mode-hook))
    (add-hook hook #'(lambda ()
                       (setup "rtags"
                         (setq-default cmake-ide-build-dir "build")
                         (cmake-ide-setup))))))

(setup-after "cmake-ide"
  (setup-after "evil-leader"
    (do-c-and-c++-mode
     (evil-leader/set-key-for-mode mode
       "m c" 'cmake-ide-compile))))

(setup-after "rtags"
  (setup "ivy-rtags")
  (setq rtags-display-result-backend 'ivy)
  (setq rtags-autostart-diagnostics t)
  (do-c-and-c++-mode
   (evil-leader/set-key-for-mode mode
     "m r s" 'rtags-find-symbol-at-point)
   (evil-leader/set-key-for-mode mode
     "m r r" 'rtags-find-references-at-point)))

(setup-lazy '(irony-mode) "irony"
  (setup-after "cmake-ide"
    (irony-cdb-autosetup-compile-options)))

(setup-expecting "irony"
  (dolist (hook '(c++-mode-hook c-mode-hook))
    (add-hook hook #'irony-mode)))

(setup-after "irony"
  (setup-after "company"
    (setup "company-irony"
      (add-hook 'irony-mode-hook
                #'(lambda ()
                    (add-to-list 'company-backends 'company-irony)
                    (setq company-backends
                          (delete 'company-semantic company-backends))
                    (company-irony-setup-begin-commands))))))

(setup-after "irony"
  (setup-after "company"
    (setup "company-irony-c-headers"
      (add-hook 'irony-mode-hook
                #'(lambda ()
                    (add-to-list 'company-backends 'company-irony-c-headers))))))

(setup-lazy '(flycheck-irony-setup) "flycheck-irony")
(setup-after "flycheck"
  (add-hook 'irony-mode-hook #'flycheck-irony-setup))

(setup-lazy '(clang-format-buffer) "clang-format")
(do-c-and-c++-mode
 (evil-leader/set-key-for-mode mode
   "m =" 'clang-format-buffer))

(provide-file)
