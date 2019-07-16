
(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook #'global-flycheck-mode)
  (add-hook 'go-mode-hook 'electric-pair-mode)

  ;; load company with go backend when go-mode is loaded
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))

  :config
   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
   (local-set-key (kbd "C-c C-g") 'go-goto-imports)
   (local-set-key (kbd "C-c C-k") 'godoc)

   (add-hook 'go-mode-hook #'lsp-deferred)
   )

(use-package company-go)

(provide 'init-golang)
;;; init-golang.el ends here
