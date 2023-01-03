
;; ###############################
;; #                             #
;; #           CONFIG            #
;; #                             #
;; ###############################

;; Initialize package sources
(require 'package)

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("melpa" . "https://melpa.org/packages/")
                         ))

(package-initialize)

(unless package-archive-contents
(package-refresh-contents))


(setq user-full-name "Paul Mayer"
      user-mail-address "p@mayer-zuffenhausen.de")

(setq inhibit-startup-message t)        ;; thanks but no

(set-face-attribute 'default nil :font "Comic Code Ligatures" :height 125)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq-default tab-width 4)
(setq warning-minimum-level :emergency)

(global-display-line-numbers-mode)           ;; line numbers
(setq display-line-numbers-type 'relative)   ;; relative line numbers

;; disable line numbers for:
(dolist (mode '(org-mode-hook
		term-mode-hook
		doc-view-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; mac specific settings
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (setq mac-right-command-modifier 'none)
  (setq mac-option-key-is-meta nil)
  (setq mac-option-modifier nil)
  (setq mac-control-modifier 'meta)
  (setq mac-command-modifier 'control)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq visible-bell nil)
  )

(when (not (eq system-type 'darwin))
  (setq visible-bell t)
  )

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Since evil wants to use C-u
(global-set-key (kbd "C-M-u") 'universal-argument)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-search-module 'evil-search)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)                           ;; thanks but yes
  ;; use visual line motions even when not in visual line mode buffers
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)           ;; changes behaviour of y 2 j" to "y 1 j" which kinda sucks...
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;;(evil-set-initial-state 'message-buffer-mode 'normal)
  ;;(evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  )

;; Text Completion Framework
(use-package company
  :init (company-mode 1))
(add-hook 'after-init-hook 'global-company-mode)

;; Syntax Checking
(use-package flycheck
  :config (global-flycheck-mode)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

(use-package all-the-icons)
;; then run this command once:
;; M-x all-the-icons-install-fonts

(use-package autothemer
  :ensure t)

;; (setq custom-theme-directory "~/.emacs.d/themes/")
;; (load-theme 'doom-catppuccin t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package general)
(general-create-definer mayerpa/control-leader
 :prefix "C-c"
 )

(general-create-definer mayerpa/space-leader
  :states 'normal
  :prefix "SPC"
  )
(mayerpa/control-leader
 "p" '(projectile-command-map :which-key "projectile")
 )
(mayerpa/space-leader
	"."  '(dired :which-key "find file")
	"SPC" '(projectile-find-file :which-key "find file in project")
	"fe"  '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "init file")
	"f3"  '((lambda () (interactive) (find-file "~/.config/i3/config")) :which-key "i3 config")
	"fi"  '((lambda () (interactive) (find-file "~/.config/nvim/init.vim")) :which-key "init.vim")
	"fz"  '((lambda () (interactive) (find-file "~/.zshrc")) :which-key "zsh config")

	"m"   '(magit :which-key "magit")

	"d"   '(:ignore t :which-key "dired")
	"d."  '(dired :which-key "Here")
	"dh"  '((lambda () (interactive) (dired "~")) :which-key "Home")
	"dn"  '((lambda () (interactive) (dired "~/Documents")) :which-key "Documents")
	"do"  '((lambda () (interactive) (dired "~/Downloads")) :which-key "Downloads")
	"dp"  '((lambda () (interactive) (dired "~/Pictures")) :which-key "Pictures")
	"dv"  '((lambda () (interactive) (dired "~/Videos")) :which-key "Videos")
	"dd"  '((lambda () (interactive) (dired "~/.dotfiles")) :which-key "dotfiles")
	"de"  '((lambda () (interactive) (dired "~/.emacs.d")) :which-key ".emacs.d")
	)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)

(evil-define-key 'insert helm-map (kbd "C-k") 'helm-previous-line)
(evil-define-key 'insert helm-map (kbd "C-j") 'helm-next-line)

;; Set Emacs state modes
(dolist (mode '(custom-mode
		eshell-mode
		git-rebase-mode
		term-mode))
(add-to-list 'evil-emacs-state-modes mode))

;; TODO; learn projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom ((projectile-completion-system 'helm)))

(use-package magit)

;; DASHBOARD
(use-package dashboard
  :after (all-the-icons)
  :ensure t
  :config (dashboard-setup-startup-hook))

(setq
    dashboard-banner-logo-title "greetings, traveler"
	dashboard-startup-banner 'logo
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-items '((recents . 5)
					  (bookmarks . 5)
;;                      (agenda . 5)
					  (projects . 5)))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; (use-package yasnippet
;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.yasnippets"))
;;   (yas-global-mode 1))

(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file :noerror)

(provide 'init)
;;; init.el ends here
