;; Code:

(use-package general
  :ensure t
  :config
  (general-define-key
   ;; replace default keybindings
   "C-'" 'avy-goto-word-1
   )

  (general-define-key
   :prefix "C-x"
   ;; bind to simple key press
   "b" 'ivy-switch-buffer  ; change buffer, chose using ivy
   )

  (general-define-key
   :prefix "C-c"
   ;; bind to simple key press
   "b" 'ivy-switch-buffer  ; change buffer, chose using ivy
   )
  )

;; todo c-p, c-n to cycle through comapny
;; todo manual invoke company with m-/
;; todo if no company available m-/ should delegate to default emacs behaviour - or map that to another key binding
;; todo hydra for edebug so i can remember the key binding. must hook into expression execution

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (which-key-add-key-based-replacements
    "C-c /" "git grep"
    "C-x b" "buffers"
    "C-c b" "buffers"
    "C-c ff" "find file"
    "C-c fr" "recently edited"
    "C-c p" "project"))

(use-package hydra
  :ensure t
  :defer 2
  :config

  (general-define-key
   :prefix "C-h"
   "a" '(hydra-apropos/body :which-key "apropos (hydra)"))

  (general-define-key
   :keymaps 'Buffer-menu-mode-map
   "." '(hydra-buffer-menu/body :which-key "buffer menu"))

  (defhydra hydra-apropos (:color blue :foreign-keys warn)
    "Apropos"
    ("a" apropos "apropos")
    ("c" apropos-command "cmd")
    ("d" apropos-documentation "doc")
    ("e" apropos-value "val")
    ("l" apropos-library "lib")
    ("o" apropos-user-option "option")
    ("u" apropos-user-option "option")
    ("v" apropos-variable "var")
    ("i" info-apropos "info")
    ("t" tags-apropos "tags")
    ("z" hydra-customize-apropos/body "customize")
    ("q" nil "cancel"))

  (defhydra hydra-customize-apropos (:color blue)
    "Apropos (customize)"
    ("a" customize-apropos "apropos")
    ("f" customize-apropos-faces "faces")
    ("g" customize-apropos-groups "groups")
    ("o" customize-apropos-options "options"))

  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

  (defhydra hydra-buffer-menu (:color pink :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :post (deactivate-mark))
    "
  ^_p_^     _d_elete    _s_tring
_b_   _f_   _q_quit     _y_ank
  ^_n_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _k_ill
"
    ("b" rectangle-backward-char nil)
    ("f" rectangle-forward-char nil)
    ("p" rectangle-previous-line nil)
    ("n" rectangle-next-line nil)
    ("e" hydra-ex-point-mark nil)
    ("c" copy-rectangle-as-kill nil)
    ("d" delete-rectangle nil)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) nil)
    ("y" yank-rectangle nil)
    ("u" undo nil)
    ("s" string-rectangle nil)
    ("k" kill-rectangle nil)
    ("q" nil "quit"))

  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
  )

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package magit
  :ensure t
  :config
  (general-define-key
   :prefix "C-c m"
   "" '(nil :which-key "magit")
   "s" '(magit-status :which-key "status")
   "l" '(magit-log :which-key "log")
   "c" '(magit-checkout :which-key "checkout")
   "b" '(magit-blame :which-key "blame")
   ))

;; (use-package auto-highlight-symbol
;;   :ensure t
;;   :config
;;   (global-auto-highlight-symbol-mode 1))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package counsel
  :ensure t
  :config
  (general-define-key
   ;; replace default keybindings
   "M-i" 'counsel-imenu
   "M-x" 'counsel-M-x        ; replace default M-x with ivy backend
   "C-s" 'swiper             ; search for string in current buffer
   )

  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)

  (general-define-key
   :prefix "C-h"
   "f" 'counsel-describe-function
   "v" 'counsel-describe-variable
   "S" 'counsel-info-lookup-symbol)

  (general-define-key
   :prefix "C-x"
   "C-f" 'counsel-find-file
   )

  (general-define-key
   :prefix "C-c"
   "r" 'ivy-resume
   "fl" 'counsel-find-library
   "gg" 'counsel-git-grep
   "ag" 'counsel-ag
   "l" 'counsel-locate
   "/" 'counsel-git-grep   ; find string in git project
   "ff" 'counsel-find-file  ; find file using ivy
   "fr"	'counsel-recentf    ; find recently edited files
   "pf" '(counsel-git :whgich-key "find file in git dir")
   ))

(use-package swiper
  :ensure t)

(use-package avy
  :ensure t
  :commands (avy-goto-word-1) ; only load config when avy is first called.
  )

(use-package multiple-cursors
  :ensure t
  :config
  :bind (("C-S-e C-S-e" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("S-<f6>" . mc/mark-all-like-this)))

(use-package paredit
  :ensure t
  :init (progn (add-hook 'clojure-mode-hook (lambda () (paredit-mode 1))))
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package rainbow-mode
  :ensure t
  :delight
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package projectile
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-after-switch-project-hook 'neotree-projectile-action)
  ;; (general-define-key
  ;;  :prefix "C-p"
  ;;  "p" 'projectile-
  ;;  )
  )

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package eldoc :diminish eldoc-mode)

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  ;; Use Company for completion
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)

  (setq company-tooltip-align-annotations t)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-auto-complete nil)

  (general-define-key
   "M-/" 'company-manual-begin)

  (general-define-key
   :keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous)

  :diminish company-mode)

;; todo icons in the company completion dropdown
;;
;; (use-package quelpa-use-package
;;   :ensure t)

;; (use-package font-lock+
;;   :ensure t
;;   :quelpa
;;   (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

(use-package rainbow-delimiters
  :ensure t
  :delight)

(use-package yaml-mode
  :ensure t)

;; javascript
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

;; (use-package indium
;;   :ensure t
;;   :after js2-mode
;;   :bind (:map js2-mode-map
;;               ("C-c C-l" . indium-eval-buffer))
;;   :hook (((js2-mode typescript-mode) . indium-interaction-mode)))


(use-package elixir-mode
  :defer t
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'lsp)
  ;; (add-hook 'elixir-mode-hook 'elixir-add-electric-pairs)
  ;; (add-hook 'elixir-mode-hook 'flycheck-elixir)
  ;; (add-hook 'elixir-mode-hook #'yas-minor-mode)
  ;; (add-hook 'elixir-mode-hook 'flycheck-mode)
  :config
  ;; smart parens - reuse some ruby functionality
  ;; (sp-with-modes '(elixir-mode)
  ;;   (sp-local-pair "fn" "end"
  ;;     	     :when '(("SPC" "RET"))
  ;;     	     :actions '(insert navigate))
  ;;   (sp-local-pair "do" "end"
  ;;     	     :when '(("SPC" "RET"))
  ;;     	     :post-handlers '(sp-ruby-def-post-handler)
  ;;     	     :actions '(insert navigate)))
  )

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'flycheck-mode))

(use-package lsp-elixir
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'lsp))

(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

(use-package neotree
  :ensure t
  :config
  (general-define-key
   :prefix "C-x"
   "n" 'neotree-toggle))

(use-package all-the-icons
  :ensure t)

(add-to-list 'load-path "~/.local/share/icons-in-terminal/")


(provide 'init-packages)
;;; init-packages.el ends here
