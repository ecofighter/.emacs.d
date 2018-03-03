(install-when-compile 'ddskk)

(setup-expecting "skk"
  (setq default-input-method "japanese-skk"))

(setup-lazy '(skk-mode) "skk"
  (setq-default skk-kutouten-type '("．" . "，"))
  (setq-default skk-use-azik t)
  (setq-default skk-user-directory "~/.skk-jisyo")
  (setq-default skk-server-host "127.0.0.1")
  (setq-default skk-server-portnum 1178))

(setup-after "skk"
  (setup "context-skk")
  (add-hook 'skk-load-hook #'(lambda ()
                               (context-skk-mode 1))))

(setup-lazy '(context-skk-mode
              context-skk-context-check
              context-skk-insert
              context-skk-customize) "context-skk")

(provide-file)
