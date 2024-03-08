;;; package --- Summary
;;; Commentary:
;;; Code:

;;
;; Bootstrap package management
;;

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;
;; Bootstrap package management
;;

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(package-initialize)

;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(when (not package-archive-contents)
    (package-refresh-contents))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

(setq use-package-ensure-function 'quelpa)

;; Keep custom-set-variables and friends out of my init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; don't show errors in custom file
(load custom-file 'noerror 'nomessage)

;;
;; End of package management bootstrap
;;

(message "Running as user %s .."
	 ((lambda ()
	    (getenv
	     (if (equal system-type 'windows-nt) "USERNAME" "USER")))))

;;
;; Config
;;

;; automatically load buffer when file changes outside of emacs
(global-auto-revert-mode t)

;; This makes my Emacs startup time ~35% faster.
(setq gc-cons-threshold 100000000)

;; inserts newline on C-n when on last line in the buffer
(setq next-line-add-newlines t)

;; prefer spaces over tabs
(setq-default indent-tabs-mode nil)

;; reload TAGS file automatically
;; stops that annoying popup dialogue box
(setq tags-revert-without-query 1)

;; typed text replaces selected
(delete-selection-mode 1)

(if (eq system-type 'darwin)
    (progn
      ;; Cmd as meta
      ;; (setq mac-option-key-is-meta nil)
      ;; (setq mac-command-key-is-meta t)
      ;; (setq mac-option-modifier 'super) ; make opt key do Super
      ;; (setq mac-command-modifier 'meta)
      ))

(defalias 'yes-or-no-p 'y-or-n-p)

;; disable toolbar
(tool-bar-mode -1)

;; solid cursor
(blink-cursor-mode 0)

;; disable scroll bar
(scroll-bar-mode -1)

;; no tool bar
(tool-bar-mode 0)

;; no menu bar
(menu-bar-mode 0)

;; no default start up screen
(setq inhibit-startup-screen t)

;; no initial scratch text
(setq initial-scratch-message nil)

;; buffer line spacing
(setq-default line-spacing 5)

;; go full screen on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; limit the number of times a frame can split
(setq split-width-threshold 200)
(setq split-height-threshold 120)

(setq delete-old-versions -1)

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; delete trailing whitespace on save action
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

;; Mousewheel scrolling can be quite annoying, lets fix it to scroll smoothly.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


;; suppress bell sounds
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; delete excess backup versions silently
(setq delete-old-versions -1)

;; use version control
(setq version-control t)

;; make backups file even when in version controlled dir
(setq vc-make-backup-files t)

;; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t)

;; transform backups file name
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; sentence SHOULD end with only a point.
(setq sentence-end-double-space nil)

;; toggle wrapping text at the 80th character
(setq default-fill-column 80)

;; quick fix to get around the exceeding max-lisp-eval-depth errors
(setq max-lisp-eval-depth 10000)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (visual-line-mode 1)
                   )))

(use-package exec-path-from-shell
  :ensure t)

;; default ansi-term binary
(setq explicit-shell-file-name "/bin/zsh")

;; load path from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq display-line-numbers-type 'relative)

(global-goto-address-mode)
(global-display-line-numbers-mode)

;; Remeber minibufer history
;; cycle through history with M-p and M-n when in minibuffer
(setq history-length 25)
(savehist-mode 1)

;; Recent files
(recentf-mode 1)

;; Save place between sessions
(save-place-mode 1)

;; don't use graphical dialogs
(setq use-dialog-box nil)

(setq epg-gpg-program "gpg")

;; Treesitter

;; language grammars
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (hcl "https://github.com/mitchellh/tree-sitter-hcl.git")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "master" "typescript/src")))

;; remap major modes to treesitter

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (terraform-mode . hcl-ts-mode)))

;;
;; Packages
;;

(defconst leader-key "SPC")

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
   (progn
      (setq evil-undo-system 'undo-tree)
      (setq evil-want-keybinding nil))
  :config
  (evil-mode 1))

(use-package eldoc
  :diminish eldoc-mode)

(use-package undo-tree
  :quelpa (undo-tree :fetcher gitlab :repo "tsc25/undo-tree")
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (general-def 'normal
    "U" 'undo-tree-visualize)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d./.cache"))))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :ensure t
  :config
  (evil-collection-init))

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  (general-create-definer general-leader-def
    :prefix leader-key)

  (general-mmap "C-h" 'windmove-left)
  (general-mmap "C-l" 'windmove-right)
  (general-mmap "C-j" 'windmove-down)
  (general-mmap "C-k" 'windmove-up))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))

(use-package vertico-prescient
  :ensure t
  :after vertico prescient)

(use-package prescient
  :ensure t
  :after vertico
  :config (progn
            (prescient-persist-mode +1)
            (vertico-prescient-mode)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (projectile-mode +1)
    (general-leader-def 'normal 'override
      "p p" 'projectile-commander
      "p f" 'projectile-find-file
      "p g" 'projectile-grep
      "p v" 'projectile-run-vterm)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (progn
    (which-key-setup-side-window-bottom))
  :init
  (which-key-mode))

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :config (progn
            (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
            (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
            (define-key copilot-completion-map (kbd "M-]") 'copilot-next-completion)
            (define-key copilot-completion-map (kbd "M-[") 'copilot-previous-completion)))

(use-package prog-mode
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
(require 'grep)
     (setq-local grep-find-ignored-directories
                 (cons "node_modules" (default-value 'grep-find-ignored-directories)))
     ))))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-is-never-other-window t)

  (general-leader-def 'normal 'override
    "t t" 'treemacs
    "t s" 'treemacs-select-window))

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

(require 'project)

(use-package vertico
  :ensure t
  :quelpa (vertico :fetcher github
                   :repo "minad/vertico"
                   :branch "main"
                   :files ("*.el" "extensions/*.el"))
  :init (vertico-mode)
  )

(use-package magit
  :ensure t
  :config
  (general-def 'normal "C-r" 'magit-refresh)
  (general-leader-def 'normal 'override
   "g g" 'magit-status
   "g s" 'magit-status
   "g l" 'magit-log))

(use-package magit-todos
  :after magit
  :ensure t
  :init
  (magit-todos-mode))


(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15)

  (general-leader-def 'normal 'override
    "s p" 'fzf-projectile
    "s g" 'fzf-git-grep))

(use-package terraform-mode
  :ensure t
  :init (progn
          (setq terraform-format-on-save 't)
  :config (progn
            (with-eval-after-load 'eglot
              (add-to-list 'eglot-server-programs
                           '(terraform-mode . ("terraform-ls" "serve"))))
            (add-hook 'terraform-mode-hook #'eglot-ensure))))


;; not working..
(use-package vterm
  :ensure t
  :config (progn
            (print "vterm config")
            (add-hook 'vterm-mode-hook (lambda ()
                                         (print "vterm hook 2")
                                         (setq-local display-line-numbers-mode nil)))))

;;
;; LSP
;;

(use-package eglot)

(use-package typescript-mode
  :ensure t
  :after eglot
  :init
  (setq eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio")))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package spotlight
  :ensure t)

(use-package prettier-js
  :after typescript-mode
  :ensure t
  :hook (typescript-mode . prettier-js-mode)
  :hook (json-ts-mode . prettier-js-mode))

(use-package diminish
  :ensure t)

(use-package tree-sitter
  :ensure t
  :config
  :after tree-sitter-langs
  (progn
    (require 'tree-sitter-langs)
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package doom-themes
  :ensure t
  :after all-the-icons
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;(set-frame-font "SFMono" nil t)


(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; set modeline font
;;(set-face-attribute 'mode-line nil :family "SFMono")

(load "~/.emacs.d/init-functions.el")
(load "~/.emacs.d/init-keys.el")

(server-start)
(put 'dired-find-alternate-file 'disabled nil)
