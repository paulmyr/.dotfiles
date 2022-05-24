
;; ###############################
;; #                             #
;; #           CONFIG            #
;; #                             #
;; ###############################

(setq user-full-name "Paul Mayer"
      user-mail-address "p@mayer-zuffenhausen.de")

(setq inhibit-startup-message t)        ;; thanks but no

(set-face-attribute 'default nil :font "Comic Code Ligatures" :height 125)

;; mac specific settings
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (setq mac-right-command-modifier 'none)
  (setq mac-option-modifier 'alt)
  (setq mac-control-modifier 'meta)
  (setq mac-command-modifier 'control)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq visible-bell nil)
  )

(when (not (eq system-type 'darwin))
  (setq visible-bell t)
  )


;; Initialize package sources
(require 'package)

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("melpa" . "https://melpa.org/packages/")
                         ))

(package-initialize)

(unless package-archive-contents
(package-refresh-contents))

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
  :config
  (evil-mode 1)                           ;; thanks but yes
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

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
  :config
  (evil-collection-init))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

;;; Spelling package
;; (use-package ispell)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-histroy))
  :config (setq ivy-initial-inputs-alist nil))

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package all-the-icons)
;; then run this command once:
;; M-x all-the-icons-install-fonts

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("w" nil "finished" :exit t))

(defhydra hydra-window-scale (:timeout 4)
    "scale text"
    ("<" evil-window-increase-width "increase width")
    (">" evil-window-decrease-width "decrease width")
    ("+" evil-window-increase-height "increase height")
    ("-" evil-window-decrease-height "decrease height")
("w" nil "finished" :exit t))

(use-package general
  :config
  (general-create-definer mayerpa/leader-key
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")

  (mayerpa/leader-key
    "."  '(dired :which-key "find file")
	"SPC" '(projectile-find-file :which-key "find file in project")
    "fe"  '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "init file")
    "fi"  '((lambda () (interactive) (find-file "~/.config/i3/config")) :which-key "i3 config")
    "fz"  '((lambda () (interactive) (find-file "~/.zshrc")) :which-key "zsh config")

    "t"   '(:ignore t :which-key "themes")
    "tt"  '(counsel-load-theme :which-key "choose-theme")
    "ts"  '(hydra-text-scale/body :which-key "scale text")

    "w"  '(hydra-window-scale/body :which-key "scale window")

    "d"   '(:ignore t :which-key "dired")
    "dd"  '(dired :which-key "Here")
    "d."  '(dired :which-key "Here")
    "dh"  '((lambda () (interactive) (dired "~")) :which-key "Home")
    "dn"  '((lambda () (interactive) (dired "~/Documents")) :which-key "Documents")
    "do"  '((lambda () (interactive) (dired "~/Downloads")) :which-key "Downloads")
    "dp"  '((lambda () (interactive) (dired "~/Pictures")) :which-key "Pictures")
    "dv"  '((lambda () (interactive) (dired "~/Videos")) :which-key "Videos")
    "d."  '((lambda () (interactive) (dired "~/.dotfiles")) :which-key "dotfiles")
    "de"  '((lambda () (interactive) (dired "~/.emacs.d")) :which-key ".emacs.d")

    "b"   '(:ignore t :which-key "buffer")
    "bk"  '((lambda () (interactive) (kill-current-buffer)) :which-key "kill current buffer")
    "bb"  '((lambda () (interactive) (counsel-ibuffer)) :which-key "choose buffer")
    "bo"  '(evil-window-next :which-key "Next Window")

    "e"   '(ebib :which-key "ebib")

    "p"   '(:ignore t :which-key "projetile")
    "pp"  '((lambda () (interactive) (projectile-switch-project)) :which-key "switch project")
    "po"  '((lambda () (interactive) (projectile-switch-open-project)) :which-key "switch open project")
    ))


(global-display-line-numbers-mode)             ;; line numbers
(setq display-line-numbers-type 'relative)   ;; relative line numbers

;; disable line numbers for:
(dolist (mode '(org-mode-hook
		term-mode-hook
		doc-view-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Set Emacs state modes
(dolist (mode '(custom-mode
		dashboard-mode
		eshell-mode
		git-rebase-mode
		term-mode))
(add-to-list 'evil-emacs-state-modes mode))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (when (file-directory-p "~/Documents/university")
    (setq projectile-project-search-path '("~/Documents/university" "~/Projects/")))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("C-c C-p" . projectile-command-map))

(which-key-add-key-based-replacements
  "C-c p" "projectile")
(which-key-add-key-based-replacements
  "C-c C-p" "projectile")


;;; Ebib config
(use-package ebib)
(require 'ebib)

(global-set-key (kbd "C-c e") 'ebib)
(global-set-key (kbd "C-c C-e") 'ebib)
(which-key-add-key-based-replacements
  "C-c e" "ebib")
(which-key-add-key-based-replacements
  "C-c C-e" "ebib")
(setq ebib-preload-bib-files '("/home/mayerpa/Documents/university/bachelor_thesis/paperkasten.bib"))
(setq bibtex-completion-bibliography '("/home/mayerpa/Documents/university/bachelor_thesis/paperkasten.bib"))

;;; Spelling
;; (after! ispell
;;   (setq ispell-really-hunspell t
;;         ispell-program-name "hunspell"
;;         ispell-dictionary "en_US,de_DE")
;;   (ispell-set-spellchecker-params)
;;   (ispell-hunspell-add-multi-dic "en_US,de_DE"))


;; DASHBOARD
;; Getting pretty icons 
(use-package all-the-icons)

(use-package dashboard
  :after (all-the-icons)
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Greetings, traveler"
	dashboard-startup-banner 'logo
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-items '((recents . 5)
					  (bookmarks . 5)
                      (agenda . 5)
					  (projects . 5))))

;; :bind (("C-c r" . dashboard-refresh-buffer)	
;;      ("C-c C-r" . dashboard-refresh-buffer))

(define-key dashboard-mode-map (kbd "C-c r") 'dashboard-refresh-buffer)
(define-key dashboard-mode-map (kbd "C-c C-r") 'dashboard-refresh-buffer)

(which-key-add-key-based-replacements
  "C-c r" "refresh dashboard")

(which-key-add-key-based-replacements
  "C-c C-r" "refresh dashboard")

(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file :noerror)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Comic Code Ligatures" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'variable-pitch nil :font "Comic Code Ligatures")
  )

(use-package org
  :hook
  (org-mode . efs/org-mode-setup)
  (org-mode . (lambda () (display-line-numbers-mode -1)))

  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
  ;; Show hidden emphasis markers

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("mpartcl" "\\documentclass{mpartcl}
                  [NO-DEFAULT-PACKAGES]
                  [EXTRA]"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(require 'org-inlinetask)

(use-package ob-ipython)
(use-package ob-kotlin)
(use-package ob-typescript)
(use-package ob-latex-as-png)

;; (setq org-cite-export-processors '((latex biblatex) (t csl)))
(setq org-cite-export-processors '((t csl)))
(setq org-cite-csl-styles-dir "/home/mayerpa/.csl-citestyles/")

(use-package org-ref
   :ensure nil
   :init
	(require 'bibtex)
	(require 'oc-biblatex)
	(setq bibtex-autokey-year-length 4
		bibtex-autokey-name-year-separator "-"
		bibtex-autokey-year-title-separator "-"
		bibtex-autokey-titleword-separator "-"
		bibtex-autokey-titlewords 2
		bibtex-autokey-titlewords-stretch 1
		bibtex-autokey-titleword-length 5))

(use-package ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
(setq org-reveal-reveal-js-version 4)
(setq org-reveal-theme "serif")

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body)
(define-key bibtex-mode-map (kbd "C-c b") 'org-ref-bibtex-hydra/body)
(define-key org-mode-map (kbd "C-c [") 'org-cite-insert)
(define-key evil-normal-state-map (kbd "C-c &") 'org-mark-ring-goto)
(define-key evil-normal-state-map (kbd "C-c %") 'org-mark-ring-push)

;;(setq org-publish-project-alist
;;      '(("blog-content"
;;         :base-directory "~/Projects/blog/posts"
;;         :html-extension "html"
;;         :base-extension "org"
;;         :publishing-directory "~/Projects/blog/publish"
;;         :publishing-function (org-html-publish-to-html)
;;         :recursive t                     ; descend into sub-folders?
;;         :section-numbers nil             ; don't create numbered sections
;;         :with-toc nil                    ; don't create a table of contents
;;	 :with-latex t                    ; do use MathJax for awesome formulas!
;;	 :html-link-home "/home/mayerpa/Projects/blog/"
;;         ;;:html-head-extra "<!-- Latest compiled and minified CSS --> <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap.min.css\" integrity=\"sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu\" crossorigin=\"anonymous\">"              ; extra <head> entries go here
;;         :html-preamble ""                ; this stuff is put before your post
;;         :html-postamble nil              ; this stuff is put after your post
;;	 :author nil
;;	 :timestamp nil
;;	 :todo-keywords nil
;;	 :headline-levels 4
;;	 :export-with-tags nil
;;	 :table-of-contents nil
;;)))

(use-package flymake)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.yasnippets"))
  (yas-global-mode 1))


;; spell checking
(dolist (hook '(text-mode-hook org-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq-default tab-width 4)
(setq warning-minimum-level :emergency)
