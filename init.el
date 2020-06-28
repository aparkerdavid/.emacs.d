(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("48763e929baf691a86d192b408024e3c8a3ca0bd9926469a816661dc3abc16ef" default))
 '(deft-default-extension "org" t)
 '(deft-directory "~/notes")
 '(deft-extensions '("org" "md" "txt"))
 '(deft-use-filename-as-title t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-re-builders-alist '((swiper . ivy--regex-plus) (t . ivy--regex-fuzzy)) t)
 '(ivy-use-virtual-buffers t)
 '(projectile-completion-system 'ivy))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
