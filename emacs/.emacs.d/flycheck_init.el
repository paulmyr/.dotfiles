;;; package --- summary
;
; ################################
; #                              #
; #            CONFIG            #
; #                              #
; #        generated from        #
; #          emacs.org           #
; #                              #
; ################################
;
;
;;; Commentary:
; Look at emacs.org.

;; Initialize package sources

  (require 'package)

  (setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ))

; (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/selfmanaged_plugins/")

(setq user-full-name "Paul Mayer"
      user-mail-address "p@mayer-zuffenhausen.de")

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

;  (when (not (eq system-type 'darwin))
;    (setq visible-bell t)
;    )

(when (eq system-type 'darwin)
  (setq pm/path_to_gdrive "~/Google Drive/My Drive"))
(when (not (eq system-type 'darwin))
  (setq pm/path_to_gdrive "~/Insync/paul.jofly@gmail.com/Google Drive"))

(setq pm/path_to_dotfiles "~/.dotfiles")
(setq pm/path_to_documents "~/Documents")
(setq pm/path_to_pictures "~/Pictures")
(setq pm/path_to_projects "~/Projects")
(setq pm/path_to_videos "~/Videos")
(setq pm/path_to_emacsd (concat pm/path_to_dotfiles "/emacs/.emacs.d"))

(setq pm/path_to_bibliography (concat pm/path_to_gdrive "/Bibliothek/Notes/bibliography.bib"))
(setq pm/path_to_paperlib (concat pm/path_to_gdrive "/Bibliothek/Notes/Library"))
(setq pm/path_to_zettelkasten (concat pm/path_to_gdrive "/Bibliothek/SecondBrain/Zettelkasten"))
(setq pm/path_to_screenshots "~/Pictures/screenshots/")

(setq inhibit-startup-message t)        ;; thanks but no

  (set-face-attribute 'default nil :font "Comic Code Ligatures" :height 125)

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 10)
  (setq-default tab-width 4)
  (setq warning-minimum-level :emergency)


  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'find-file-hook #'display-line-numbers-mode)
; (global-display-line-numbers-mode)           ;; line numbers
  (setq display-line-numbers-type 'relative)   ;; relative line numbers


  ;; disable line numbers for:
  (dolist (mode '(term-mode-hook
                  doc-view-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(load "beacon/beacon.el")
(beacon-mode 1)

(setq beacon-blink-when-window-scrolls t)
(setq beacon-blink-when-window-changes t)
(setq beacon-blink-when-pointer-moves t)
(setq beacon-blink-when-point-moves-vertically 5)
(setq beacon-blink-when-point-moves-horizontally nil)
(setq beacon-dont-blink-commands '())
(setq beacon-color "#f2d5cf")

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

;; Set Emacs state modes
(dolist (mode '(custom-mode
                eshell-mode
                git-rebase-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  )

(use-package helm-bibtex
  :after helm)
(setq bibtex-completion-display-formats
      '((t . "${=key=:20}  ${title:*} ${author: 40}  ${year:4}  ${=has-pdf=:1} ${=has-note=:1}  ${=type=:20}")))

(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")
(setq helm-bibtex-full-frame nil)

(setq helm-bibtex-pdf-open-function
      (lambda (fpath)
        (start-process "zathura" "helm-bibtex-zathura" "xdg-open"
                       fpath)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (when (file-directory-p pm/path_to_projects)
    (setq projectile-project-search-path `(,pm/path_to_projects)))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom ((projectile-completion-system 'helm)))

(use-package helm-projectile
  :after projectile)
(helm-projectile-on)

(use-package magit)

(defun pm/flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.
Uses `flyspell-prog-mode' for modes derived from `prog-mode',
so only strings and comments get checked.
All other buffers get `flyspell-mode' to check all text.
If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
(progn
  (if (derived-mode-p 'prog-mode)
    (progn
      (message "Flyspell on (code)")
      (flyspell-prog-mode))
    ;; else
    (progn
      (message "Flyspell on (text)")
      (flyspell-mode 1)))
  ;; I tried putting (flyspell-buffer) here but it didn't seem to work
  )))

(defun pm/flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.
When turning on, it uses `flyspell-on-for-buffer-type'
so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
  (progn ; flyspell is on, turn it off
    (message "Flyspell off")
    (flyspell-mode -1))
  ; else - flyspell is off, turn it on
  (pm/flyspell-on-for-buffer-type)))

(use-package flycheck
  :config (global-flycheck-mode)
  )

(use-package company
  :init (company-mode 1))
(add-hook 'after-init-hook 'global-company-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)
(use-package dap-python)

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ; default viewer
  (setq TeX-view-program-selection '((output-pdf "Zathura"))))

(defun pm/org-babel-tangle-emacsorg ()
  "Checks if current buffer is emacs.org, if yes tangle it."
  (message "Tangle %s..." (buffer-file-name))
  (when (string-equal buffer-file-name (expand-file-name (concat pm/path_to_emacsd "/emacs.org")))
    (org-babel-tangle-file (buffer-file-name))
    ))

(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (add-hook 'after-save-hook
                                   'pm/org-babel-tangle-emacsorg
                                   'run-at-end 'only-in-org-mode)))
  :config
  (setq org-ellipsis " ▾")
  ;;  (setq org-hide-emphasis-markers t)
  )

(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ))

(setq org-confirm-babel-evaluate nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory pm/path_to_zettelkasten)
  (org-roam-completion-everywhere t)
                                        ;(org-return-follows-link  t)                          ;; See comment above
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:\n\n")
      :unnarrowed t)
     ("r" "bibliography reference" plain
      "%?"
      :target
      (file+head "references/${citekey}.org" "#+title: ${citekey}: ${title}\n#+filetags: :paper:\n\n")
      :unnarrowed t)))
  :config
  (org-roam-setup)
  )

(setq org-roam-node-display-template "${title:200}${tags}")

(setq org-roam-dailies-directory "dailies/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))))

(require 'ox-md)  ; backend for markdown
(require 'ox-man) ; backend for manpages

(use-package org-ref)
(require 'oc-basic)
(require 'oc-csl)
(require 'oc-biblatex)

(setq bibtex-completion-bibliography
        `(,pm/path_to_bibliography))

  (setq bibtex-completion-library-path `(,pm/path_to_paperlib))
  (setq bibtex-completion-pdf-field "File")

  (setq org-cite-global-bibliography
        `(,pm/path_to_paperlib))

;  (setq org-cite-csl-styles-dir "~/.emacs.d/.cslstyles/")
  (setq org-cite-export-processors
        '((latex biblatex)                                 ; For humanities
          (t csl)))                   ; Fallback

(use-package org-roam-bibtex
  :after org-roam
  :ensure t
  :config
  (require 'org-ref)
  )

(org-roam-bibtex-mode)

(defun get-newest-file-from-dir  (path)
  "Get latest file (including directory) in PATH."
  (car (directory-files path 'full nil #'file-newer-than-file-p)))

(defun insert-org-image ()
  "Moves image from Dropbox folder to ./media, inserting org-mode link"
  (interactive)
  (let* ((indir (expand-file-name pm/path_to_screenshots))
         (infile (get-newest-file-from-dir indir))
         (outdir (concat (file-name-directory (buffer-file-name)) "./media"))
         (outfile (expand-file-name (file-name-nondirectory infile) outdir)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (rename-file infile outfile)
    (insert (concat (concat "#+org_attr: :width 30%\n[[./media/" (file-name-nondirectory outfile)) "]]")))
  (newline)
  (newline))

(require 'org-inlinetask)

(use-package multi-term)
(global-set-key (kbd "C-c t") 'multi-term)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; unbind RET from evil
;(with-eval-after-load 'evil-maps
;  (define-key evil-motion-state-map (kbd "RET") nil))
;; Since evil wants to use C-u
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-c f") 'flyspell-toggle )
(add-hook 'find-file-hook 'pm/flyspell-on-for-buffer-type)

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

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
  "fe"  '((lambda () (interactive) (find-file (concat pm/path_to_emacsd "/init.el"))) :which-key "init file")
  "f3"  '((lambda () (interactive) (find-file (concat pm_path_to_dotfiles "i3/.config/i3/config"))) :which-key "i3 config")
  "fi"  '((lambda () (interactive) (find-file (concat pm_path_to_dotfiles "neovim/.config/nvim/init.vim"))) :which-key "init.vim")
  "fz"  '((lambda () (interactive) (find-file (concat pm_path_to_dotfiles "zsh/.zshrc"))) :which-key "zsh config")

  "m"   '(magit :which-key "magit")

  "n"   '(:ignore t :which-key "notes")
  "nn"  '(org-roam-dailies-capture-today :which-key "capture note")
  "nt"  '(org-roam-dailies-goto-today :which-key "goto todays notes")
  "nd"  '(org-roam-dailies-goto-today :which-key "goto note of date")

  "D"   '(dashboard-refresh-buffer :which-key "dashboard")

  "d"   '(:ignore t :which-key "dired")
  "d."  '(dired :which-key "Here")
  "dh"  '((lambda () (interactive) (dired "~")) :which-key "Home")
  "dn"  '((lambda () (interactive) (dired pm/path_to_documents)) :which-key "Documents")
  "do"  '((lambda () (interactive) (dired pm/path_to_downloads)) :which-key "Downloads")
  "dp"  '((lambda () (interactive) (dired pm/path_to_pictures)) :which-key "Pictures")
  "dv"  '((lambda () (interactive) (dired pm/path_to_videos)) :which-key "Videos")
  "dd"  '((lambda () (interactive) (dired pm/path_to_dotfiles)) :which-key "dotfiles")
  "de"  '((lambda () (interactive) (dired pm/path_to_emacsd)) :which-key ".emacs.d")

  "b"  '(helm-bibtex :which-key "helm bibtex")
  )

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(evil-define-key 'insert helm-map (kbd "C-k") 'helm-previous-line)
(evil-define-key 'insert helm-map (kbd "C-j") 'helm-next-line)

(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n c") 'org-ref-cite-insert-helm)
(global-set-key (kbd "C-c i i") 'insert-org-image)
(global-set-key (kbd "C-c i t") 'org-toggle-inline-images)

;; (use-package yasnippet
;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.yasnippets"))
;;   (yas-global-mode 1))

(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file :noerror)

(provide 'init)
;;; init.el ends here
