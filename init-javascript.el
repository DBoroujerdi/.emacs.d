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
  (add-hook 'rjsx-mode-hook #'js2-refactor-mode)

  (defhydra hydra-js2-refactor (:color blue :hint nil)
    "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
    ("ee" js2r-expand-node-at-point)
    ("cc" js2r-contract-node-at-point)
    ("ef" js2r-extract-function)
    ("em" js2r-extract-method)
    ("tf" js2r-toggle-function-expression-and-declaration)
    ("ta" js2r-toggle-arrow-function-and-expression)
    ("ip" js2r-introduce-parameter)
    ("lp" js2r-localize-parameter)
    ("wi" js2r-wrap-buffer-in-iife)
    ("ig" js2r-inject-global-in-iife)
    ("ag" js2r-add-to-globals-annotation)
    ("ev" js2r-extract-var)
    ("iv" js2r-inline-var)
    ("rv" js2r-rename-var)
    ("vt" js2r-var-to-this)
    ("ao" js2r-arguments-to-object)
    ("ti" js2r-ternary-to-if)
    ("sv" js2r-split-var-declaration)
    ("ss" js2r-split-string)
    ("uw" js2r-unwrap)
    ("lt" js2r-log-this)
    ("dt" js2r-debug-this)
    ("sl" js2r-forward-slurp)
    ("ba" js2r-forward-barf)
    ("k" js2r-kill)
    ("q" nil)
    )

  (global-set-key (kbd "C-x jr") 'hydra-js2-refactor/body)
  )

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
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(use-package graphql-mode
  :mode "\\.gql$")

(use-package web-mode
  :mode "\\.ejs\\'")

(provide 'init-javascript)
;;; init-javascript.el ends here
