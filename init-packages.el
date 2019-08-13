;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package general
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
   "y" 'ivy-yasnippet
   )
  )

;; todo c-p, c-n to cycle through comapny
;; todo manual invoke company with m-/
;; todo if no company available m-/ should delegate to default emacs behaviour - or map that to another key binding
;; todo hydra for edebug so i can remember the key binding. must hook into expression execution

(use-package which-key
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


;; (use-package flyspell-correct-ivy
;;   :after flyspell
;;   :bind (:map flyspell-mode-map
;;         ("C-c ;" . flyspell-correct-word-generic))
;;   :custom (flyspell-correct-interface 'flyspell-correct-ivy))

;; aggresively disable flyspell by redifining the function
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

(use-package ace-window
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package magit
  :config
  (general-define-key
   :prefix "C-c m"
   "" '(nil :which-key "magit")
   "s" '(magit-status :which-key "status")
   "l" '(magit-log :which-key "log")
   "c" '(magit-checkout :which-key "checkout")
   "b" '(magit-blame :which-key "blame")
   ))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; (use-package auto-highlight-symbol
;;   :config
;;   (global-auto-highlight-symbol-mode 1))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package counsel
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

(use-package docker)

(use-package dockerfile-mode)

(use-package swiper)

(use-package avy
  :commands (avy-goto-word-1) ; only load config when avy is first called.
  )

(use-package multiple-cursors
  :config
  :bind (("C-S-e C-S-e" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("S-<f6>" . mc/mark-all-like-this)))

(use-package paredit
  :init (progn (add-hook 'clojure-mode-hook (lambda () (paredit-mode 1))))
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package rainbow-mode
  :delight
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package projectile
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
  :config
  (counsel-projectile-mode))

(use-package eldoc :diminish eldoc-mode)

(use-package company
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

;; (use-package font-lock+
;;   :quelpa
;;   (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package rainbow-delimiters
  :delight)

(use-package yaml-mode)

(use-package elixir-mode
  :defer t
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
  :config
  (add-hook 'elixir-mode-hook 'flycheck-mode))

;; (use-package lsp-elixir
;;   :config
;;   (add-hook 'elixir-mode-hook 'lsp))

(use-package minions
  :config
  (minions-mode 1)
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

(use-package neotree
  :config
  (general-define-key
   :prefix "C-x"
   "n" 'neotree-toggle)

  (setq neo-hidden-regexp-list '("^\\.flycheck" "~$" "^#.*#$" "\\.elc$" "\\.o$"))
  (setq-default neo-show-hidden-files nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package all-the-icons)

(add-to-list 'load-path "~/.local/share/icons-in-terminal/")

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company-terraform
  :config (company-terraform-init))

(use-package eyebrowse
  :config
  (eyebrowse-mode t))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-to-list 'load-path "~/.emacs.d/es6-snippets")
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippet"
          "~/.emacs.d/es6-snippets/snippets"))
  (yas-reload-all)
  )

(use-package ivy-yasnippet)

(use-package multiple-cursors
  :bind (("C-S-e C-S-e" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("S-<f6>" . mc/mark-all-like-this)))

(use-package iedit)

(use-package emojify
  :config
  (add-hook 'after-init-hook #'global-emojify-mode)
  (emojify-download-emoji-maybe))

(use-package all-the-icons)

(use-package vterm)
(provide 'init-packages)
;;; init-packages.el ends here
