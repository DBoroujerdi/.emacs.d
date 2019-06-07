

;; keys

(general-define-key
 :prefix "C-c j"
 "t" 'jest-popup)

(use-package tide
  :ensure t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  :config

  (general-define-key
   :keymaps 'tide-mode-map
   "C-c C-?" 'tide-references)

  (add-hook 'before-save-hook 'editorconfig-apply))


(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package jest
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode))
  :config
  ;; have 2 space indentation by default
  (general-define-key
   :prefix "C-x"
   "r" 'hydra-js2-refactor/body)

  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t)
  (electric-pair-mode 1)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  )



(use-package js2-refactor
  :ensure t
  :config

  (add-hook 'tide-mode-hook #'js2-refactor-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)

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

(use-package nvm
  :ensure t)

(use-package rjsx-mode
  :ensure t)

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))


(provide 'init-javascript)
