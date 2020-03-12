;;; package --- Summary
;;; Commentary:
;;; Code:


(general-define-key
 :prefix "C-c j"
 "t" 'jest-popup)

(use-package tide
  :hook (before-save . tide-format-before-save)
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  :config
  (general-define-key
   :keymaps 'tide-mode-map
   "C-c C-?" 'tide-references)

  ;; start with rjsx
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)
  ;; (add-hook 'before-save-hook 'editorconfig-apply)
  )

(use-package json-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package jest)

;; required for rjsx-mode as that extends js2
(use-package js2-mode)

;; used in place of js2 until emacs 27 is released
(use-package rjsx-mode
  :mode (("\\.js$" . rjsx-mode))
  :after (add-node-modules-path)
  :config
  (setq js-indent-level 2
        js-chain-indent t)

  ;; refactoring hydra menu
  (general-define-key
   :prefix "C-x"
   "r" 'hydra-js2-refactor/body)

  (electric-pair-mode 1)

  ;; these are required to turn off js2 mode linting as this
  ;; interferes and prevents the javascript-eslint linter from
  ;; being applied
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'tide-mode-hook #'js2-refactor-mode)
  (add-hook 'rjsx-mode-hook #'js2-refactor-mode))

;; todo write function that can switch this..
;; or that find the newest version
;; or that finds the latest LTS
(setq exec-path (append exec-path '("/usr/local/opt/node@10/bin")))

;; TODO move to misc file
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (exec-path-from-shell-initialize))


;; Make sure the local node_modules/.bin/ can be found (for eslint)
;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path)

(use-package nvm)

(use-package prettier-js
  :config
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

(use-package graphql-mode
  :mode "\\.gql$")

(use-package web-mode
  :mode "\\.ejs\\'")

(provide 'init-javascript)
;;; init-javascript.el ends here
