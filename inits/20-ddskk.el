(install-when-compile 'ddskk)

(setup-lazy '(skk-mode) "skk"
  (setq skk-kutouten-type 'en)
  (setq skk-use-azik t)
  (setq skk-user-directory "~/.skk-jisyo")
  (setq skk-server-host "127.0.0.1")
  (setq skk-server-portnum 1178))

(setup-after "skk"
  (setup "context-skk"
    (add-hook 'skk-load-hook #'(lambda ()
                                 (context-skk-mode 1)))))

(setup-expecting "skk"
  (setq default-input-method "japanese-skk"))

(provide-file)
