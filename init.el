;;; package --- Summary
;;; Commentary:
;;; Code:

;;
;; Bootstrap package management
;;

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)

(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))))

(package-initialize)
(setq package-enable-at-startup nil)(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))


;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
  (message "Refreshing ELPA package archives...")
  (package-refresh-contents))


;; straight - for checking out packages with git, for local hacking and contributing
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))


;; Install `use-package`
;; This can be done 1 of 3 ways

;; 1. Simple way - from `package-install`
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

;; so we don't have to add :ensure t to every use-package invocation
(setq use-package-always-ensure t)

;; 2. Git-submodule - when you want to install a specific commit hash, maybe
;; due to a bug
;; (eval-when-compile
;;   ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;   (add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "vendor/use-package"))))

;; 3. With straight - you can then use straight flags in `use-package` commands
;; (straight-use-package 'use-package)

;; finally, load use-package
(require 'use-package)
(setq use-package-minimum-reported-time 0
      use-package-verbose t)

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

;; Disable toolbar
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

;; display line numbers
;; (setq linum-format "%d ")
;; (global-linum-mode t)

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200) ; chars
              (height . 60) ; lines
              ))

      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200)
              (height . 60)
              )))
  (progn
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)))
    (setq default-frame-alist
          '(
            (tool-bar-lines . 0)))))


;; limit the number of times a frame can split
(setq split-width-threshold 200)
(setq split-height-threshold 120)

(setq c-default-style "linux" c-basic-offset 8)

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

(exec-path-from-shell-copy-envs '("NPM_TOKEN"))

;;
;; Load other modules
;;

;; todo
;; user-emacs-directory
(load "~/.emacs.d/init-packages")
(load "~/.emacs.d/init-lisp")
(load "~/.emacs.d/init-keys.el")
(load "~/.emacs.d/init-functions")
(load "~/.emacs.d/init-keys.el")
;; (load "~/.emacs.d/init-golang.el")
;; (load "~/.emacs.d/init-programming.el")
;; (load "~/.emacs.d/init-javascript.el")
;; (load "~/.emacs.d/init-lsp.el")
;; (load "~/.emacs.d/init-golang.el")
;; (load "~/.emacs.d/init-typescript.el")

(use-package spacemacs-theme
 :ensure t
 :defer t)

(load-theme 'spacemacs-light 'no-confirm)

;; for new frames and emacs client..
;; (setq default-frame-alist ((font . "Inconsolata")))

;; set default font
(set-face-attribute 'default nil
                     :family "Inconsolata"
                     :height 140
                     :weight 'normal
                     :width 'normal)

;; set modeline font
(set-face-attribute 'mode-line nil :family "Inconsolata")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "4a61dd8fd1fd94f118e43314abbd86873868a12c5f5d73de9f8558d0d2b33c46" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(exec-path-from-shell-check-startup-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
