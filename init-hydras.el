;;; package --- Summary
;;; Commentary:
;;; Code:

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

(defhydra hydra-window-resize (:color red
                                      :hint nil)
  "
 Shrink horizontal: _l_
 Enlarge horizontal: _h_
 Shrink vertically: _j_
 Enlarge vertically: _k_
"
  ("l" 'shrink-window-horizontally)
  ("h" 'enlarge-window-horizontally)
  ("j" 'shrink-window)
  ("k" 'enlarge-window))

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-hint-display-type 'lv) (flycheck-list-errors))
        :post (progn (setq hydra-hint-display-type nil) (quit-windows-on "*Flycheck errors*"))
        :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

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

(general-define-key
 :keymaps 'flycheck-mode-map
 "C-c ! f" 'hydra-flycheck/body)

(general-define-key
 :keymaps 'rjsx-mode-map
 "C-c C-r" 'hydra-js2-refactor/body)

(provide 'init-hydras)
;;; init-hydras.el ends here
