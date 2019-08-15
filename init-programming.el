;;; package --- Summary
;;; Commentary:
;;; Code:

(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck
  :init
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)


  :hook ((typescript-mode . flycheck-mode)
         (rjsx-mode . flycheck-mode)
         (elisp-mode . flycheck-mode))

  :config
  ;; auto flycheck on buffer save
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (setq flycheck-temp-prefix ".flycheck")

  (flycheck-add-mode 'typescript-tslint 'typescript-mode)

  (flycheck-valid-checker-p 'typescript-tslint)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-valid-checker-p 'javascript-eslint)

  (add-to-list 'flycheck-disabled-checkers 'jsx-tide)

  (add-hook 'rjsx-mode-hook #'use-eslint-from-node-modules)

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))

  ;; configure size and format of error buffer
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15))))

(use-package lsp-mode
  :commands lsp
  :hook (typescript-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config

  ;; ensures typescript built-in linter is run after lsp-ui linter
  ;; in the same buffer. the typescript-language-server does no provide linting
  ;; but is still aggressively loaded by lsp-mode
  (flycheck-add-next-checker 'lsp-ui 'typescript-tslint)

  ;; (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-ignore-duplicate t)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-quickhelp
  :after (company)
  :config
  (company-quickhelp-mode))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
        company-lsp-cache-candidates 'auto
        company-lsp-enable-recompletion t))

(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

(provide 'init-programming)
;;; init-programming.el ends here
