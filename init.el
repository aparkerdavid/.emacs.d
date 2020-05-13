;; Default settings for frame appearance
(setq initial-frame-alist
      '((width . 100)
        (height . 40)))
(setq-default line-spacing 0.125)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(tool-bar-mode -1)
(menu-bar-mode -1)


;;  Bootstrap straight.el
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


;; Hook up use-package to straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; Packages

(use-package flx
  :ensure t)


(use-package counsel
  :ensure t
  :after (flx)
  :config (ivy-mode 1)
  :custom (ivy-re-builders-alist '((t . ivy--regex-fuzzy))))


;; (use-package helm
;;   :ensure t
;;   :config
;;     (require 'helm-config)
;;     ;; (helm-mode 1)
;;     (global-set-key (kbd "M-x") #'helm-M-x)
;;     (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;     (global-set-key (kbd "C-x C-f") #'helm-find-files)
;;     (define-key minibuffer-local-map (kbd "M-j") #'next-line)
;;     (define-key helm-map (kbd "M-k") #'previous-line)
;;   )


(use-package flycheck
  :ensure t)

(use-package avy
  :ensure t)


(use-package deft
  :ensure t
  :custom
    (deft-extensions '("org" "md" "txt"))
    (deft-default-extension "org")
    (deft-directory "~/notes")
    (deft-use-filename-as-title t))


(use-package zetteldeft
  :ensure t
  :after (deft avy)
  :config (zetteldeft-set-classic-keybindings))


(use-package magit
  :ensure t
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (global-set-key (kbd "C-c g") 'magit-status))

(use-package ranger
  :ensure t)

(use-package projectile
  :ensure t
  :after (ivy)
  :custom (projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package hydra
  :ensure t)


;; Languages

(use-package tide
  :after (tide company)
  :ensure t
  :config
  (defun setup-tide-mode ()
    "Setup function for tide."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  (setq company-tooltip-align-annotations t)

  (add-hook 'js-mode-hook #'setup-tide-mode))

(use-package paredit
  :ensure t)

(use-package god-mode
  :ensure t
  :config
    (add-hook 'prog-mode-hook 'god-local-mode)
    (add-hook 'text-mode-hook 'god-local-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package lsp-mode
  :ensure t)


(use-package web-mode
  :ensure t)


(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode))

(use-package org
  :ensure t)

(use-package expand-region
  :ensure t)


;; Theme: Gruvbox, extensively customized.
(use-package base16-theme
  :ensure t
  :init
  (setq custom-safe-themes t)
  (load-theme 'base16-gruvbox-dark-hard)
  (add-to-list 'default-frame-alist '(background-color . "#1d1d1d"))
  (add-to-list 'default-frame-alist '(foreground-color . "#fbf1c7"))

  ;; Vars for all the gruvbox colors!
  (defvar gruv-red "#cc241d")
  (defvar gruv-green "#98971a")
  (defvar gruv-yellow "#d79921")
  (defvar gruv-blue "#458588")
  (defvar gruv-purple "#b16286")
  (defvar gruv-aqua "#689d6a")
  (defvar gruv-orange "#d65d0e")

  (defvar gruv-dark-red "#9d0006")
  (defvar gruv-dark-green "#79740e")
  (defvar gruv-dark-yellow "#b57614")
  (defvar gruv-dark-blue "#076678")
  (defvar gruv-dark-purple "#8f3f71")
  (defvar gruv-dark-aqua "#427b58")
  (defvar gruv-dark-orange "#af3a03")

  (defvar gruv-light-red "#fb4934")
  (defvar gruv-light-green "#b8bb26")
  (defvar gruv-light-yellow "#fabd2f")
  (defvar gruv-light-blue "#83a598")
  (defvar gruv-light-purple "#d3869b")
  (defvar gruv-light-aqua "#8ec07c")
  (defvar gruv-light-orange "#fe8019")

  ;; Some color cusotmizations: less syntax highlighting, comments in red, cooler colors.
  (set-face-foreground 'font-lock-comment-face gruv-light-red)
  (set-face-foreground 'font-lock-comment-delimiter-face gruv-light-red)
  (set-face-foreground 'font-lock-doc-face gruv-light-red)
  (set-face-foreground 'font-lock-variable-name-face nil)
  (set-face-foreground 'font-lock-keyword-face gruv-light-aqua)
  (set-face-foreground 'font-lock-builtin-face gruv-light-blue)
  (set-face-foreground 'font-lock-function-name-face nil)
  (set-face-foreground 'font-lock-constant-face nil)
  (set-face-background 'line-number nil)


  ;; Fancier modeline
  
  ;; (set-face-attribute 'mode-line nil :background gruv-dark-purple)
  (set-face-attribute 'mode-line nil :foreground "#fbf1c7")
  (set-face-attribute 'mode-line nil :background gruv-dark-purple)
  (set-face-attribute 'mode-line nil :box `(:line-width 4 :color ,gruv-dark-purple))
  )


;; (straight-use-package 'counsel)
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(setq shift-select-mode nil)

;; (ivy-mode 1)

;; The following 2 settings are recommended by Ivy "for new users"; I don't know what they do.
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")

;; Custom funs
(defun newline-below ()
  (interactive)
  (save-excursion
    (call-interactively 'move-end-of-line)
    (newline)))

(defun newline-above ()
  (interactive)
    (save-excursion
    (call-interactively 'move-beginning-of-line)
    (newline)))

(defun insert-above ()
       (interactive)
       (progn
	 (call-interactively 'move-beginning-of-line)
	 (newline)
	 (call-interactively 'previous-line)
	 (call-interactively 'god-local-mode 0)))

(defun insert-below ()
  (interactive)
  (progn
    (call-interactively 'move-end-of-line)
    (newline)
    (call-interactively 'god-local-mode 0)))

(defun wrath ()
  "cut the current region and leave god mode; cf 'c' in vim"
  (interactive)
  (if
      (use-region-p)
       (progn
	 (call-interactively 'kill-region)
	 (call-interactively 'god-local-mode 0))
    (call-interactively 'god-local-mode 0)))

(defun mark-toggle ()
  "toggle the mark; cf visual mode in vim"
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (call-interactively 'set-mark-command)))

(defun comment-toggle ()
  "toggle comment status on one or more lines."
  (interactive)
  (if (use-region-p)
      (call-interactively 'comment-line)
    (if (= (line-beginning-position) (line-end-position))
	(call-interactively 'comment-dwim)
	(comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

(defun line-beginning-smart ()
  "go to the beginning of the line; if already there, go to the first nonwhitespace character."
  (interactive)
  (if (= 0 (current-column))
      (call-interactively 'back-to-indentation)
    (call-interactively 'move-beginning-of-line)))

(defun kill-region-smart ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)
    (call-interactively 'delete-char)))

(defun select-line ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'move-end-of-line)
    (progn
      (call-interactively 'move-beginning-of-line)
      (call-interactively 'set-mark-command)
      (call-interactively 'move-end-of-line))))

;; Keybindings
(global-set-key (kbd "<escape>") (lambda () (interactive) (god-local-mode 1)))
;; isearch handling in god-mode
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<return>") #'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<return>") #'god-mode-isearch-disable)
(define-key god-mode-isearch-map (kbd "<escape>") #'isearch-exit)


;; Emacs-like navigation for god-mode
(global-set-key (kbd "C-f") #'forward-word)
(global-set-key (kbd "M-f") #'forward-char)
(global-set-key (kbd "C-b") #'backward-word)
(global-set-key (kbd "M-b") #'backward-char)
(global-set-key [remap set-mark-command] #'mark-toggle)

;; vim-like navigation for god-mode
;; (define-key god-local-mode-map (kbd "h") #'backward-word)
;; (define-key god-local-mode-map (kbd "H") #'backward-char)
;; (define-key god-local-mode-map (kbd "j") #'next-line)
;; (define-key god-local-mode-map (kbd "k") #'previous-line)
;; (define-key god-local-mode-map (kbd "l") #'forward-word)
;; (define-key god-local-mode-map (kbd "L") #'forward-char)
(define-key god-local-mode-map (kbd "i") #'wrath)
(define-key god-local-mode-map (kbd "C-S-i") (lambda () (interactive) (god-local-mode 0)))
;; (define-key god-local-mode-map (kbd "m") #'mark-toggle)
;; (define-key god-local-mode-map (kbd "o") #'insert-below)
;; (define-key god-local-mode-map (kbd "C-S-o") #'insert-above)

(define-key god-local-mode-map (kbd "[") #'newline-above)
(define-key god-local-mode-map (kbd "]") #'newline-below)
(global-set-key (kbd "C-a") #'line-beginning-smart)
(global-set-key (kbd "C-d") #'kill-region-smart)
(define-key paredit-mode-map (kbd "C-d") #'smart-kill-region)
(define-key paredit-mode-map [remap kill-region] #'paredit-kill-region)
;; (global-set-key (kbd "M-SPC")
;; 		(defhydra utility-hydra (:pre (god-local-mode 0)
;; 				:post (god-local-mode 1))
;; 		  "Utility Hydra"
;; 		  ("h" windmove-left "window left")
;; 		  ("j" windmove-down "window down")
;; 		  ("k" windmove-up "window up")
;; 		  ("l" windmove-right "window right")
;; 		  ("H" split-window-horizontally "split window horizontally")
;; 		  ("J" split-window-vertically "split window vertically")
;; 		  ("d" delete-window "delete window")
;; 		  ("r" ranger "ranger" :color blue)
;; 		  ("C" (find-file user-init-file) "Edit init file" :color blue)
;; 		  ("Q" kill-emacs "kill emacs")
;; 		  ("<escape>" nil "quit")
;; 		  ))
(global-set-key (kbd "M-SPC")
		(defhydra utility-hydra (:pre (god-local-mode 0)
				:post (god-local-mode 1))
		  "Utility Hydra"
		  ("b" windmove-left "window left")
		  ("n" windmove-down "window down")
		  ("p" windmove-up "window up")
		  ("f" windmove-right "window right")
		  ("F" split-window-horizontally "split window horizontally")
		  ("N" split-window-vertically "split window vertically")
		  ("d" delete-window "delete window")
		  ("r" ranger "ranger" :color blue)
		  ;; ("g" magit-status "magit status" :color blue)
		  ("!" eshell "eshell" :color blue)
		  ("C" (find-file user-init-file) "Edit init file" :color blue)
		  ("Q" kill-emacs "kill emacs")
		  ("<escape>" nil "quit")))



(defhydra barf-hydra ()
	 ("f" sp-forward-barf-sexp "barf forward" :color blue)
	 ("b" sp-backward-barf-sexp "barf backward" :color blue))


(defhydra slurp-hydra ()
	 ("f" sp-forward-slurp-sexp "slurp forward" :color blue)
	 ("b" sp-backward-slurp-sexp "slurp backward" :color blue))


(defhydra sp-hydra (:pre (god-local-mode 0)
			 :post (god-local-mode 1))
  ("b" barf-hydra/body "barf" :color blue)
  ("s" slurp-hydra/body "slurp" :color blue)
  ("m" sp-mark-sexp "mark sexp" :color blue))


(define-key smartparens-mode-map (kbd "C-c n") #'sp-hydra/body)

(global-set-key (kbd "C-w") #'er/expand-region)
(global-set-key (kbd "C-;") #'avy-goto-char-2)

;; Better commenting function
(global-set-key (kbd "C-x C-;") #'comment-toggle)

;; Cursors
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(add-hook 'window-setup-hook (lambda () (set-cursor-color gruv-light-yellow)))
(defun god-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(set-cursor-color gruv-light-yellow)
(add-hook 'activate-mark-hook (lambda () (set-cursor-color gruv-red)))
(add-hook 'deactivate-mark-hook (lambda () (set-cursor-color gruv-light-yellow)))

(add-hook 'god-mode-enabled-hook 'god-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-update-cursor)

;; open links in msedge
(setq browse-url-generic-program 
    "/mnt/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe" 
    browse-url-browser-function 'browse-url-generic)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "350dc341799fbbb81e59d1e6fff2b2c8772d7000e352a5c070aa4317127eee94" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" "304c39b190267e9b863c0cf9c989da76dcfbb0649cbcb89592e7c5c08348fce9" "ec3e6185729e1a22d4af9163a689643b168e1597f114e1cec31bdb1ab05aa539" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "bcfc77fcc3e012941eb47d5037f0fac767e23fd2dae039214e5fa856ac8bdfdd" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "428bdd4b98d4d58cd094e7e074c4a82151ad4a77b9c9e30d75c56dc5a07f26c5" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "a61109d38200252de49997a49d84045c726fa8d0f4dd637fce0b8affaa5c8620" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "bac3d6b6cb2476c13e1127c58350c2f61a1eaa14108b83c78d0c1b587d97803f" "3a33d2fa0cec573b282d334de6d289ed05997fb807953da296e8f0fda962dce4" "cdf97d077725855aeb1fa2b00b378542ace6b7cd0164e4a947b2a9fab6fc29ad" default))
 '(deft-default-extension "org" t)
 '(deft-directory "~/notes")
 '(deft-extensions '("org" "md" "txt"))
 '(deft-use-filename-as-title t)
 '(ivy-re-builders-alist '((t . ivy--regex-fuzzy)) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
