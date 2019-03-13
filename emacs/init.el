;; Custom
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cursor-type (quote (bar . 2)))
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-theme-load-path (quote (custom-theme-directory t)) t)
 '(debug-on-error nil)
 '(display-time-mode nil)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-recentf-fuzzy-match t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message
   (concat "#+OPTIONS: toc:nil num:nil \\n:nil ::t -:t

* "
	   (shell-command-to-string "printf '%s' $(date +%Y%m%d)")
	   ": "))
 '(irony-additional-clang-options (quote ("-pthread" "-std=c++11")))
 '(line-spacing 2)
 '(linum-format "%4d│")
 '(menu-bar-mode nil)
 '(org-clock-into-drawer 2)
 '(org-entities-user (quote (("chcl" "" nil "&#x2610;" "" "" ""))))
 '(org-export-headline-levels 4)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(org-reverse-note-order t)
 '(org-use-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
    (ox-jira smooth-scrolling flycheck-irony company-c-headers company-irony company transpose-frame sr-speedbar helm auto-package-update diminish use-package)))
 '(recentf-mode nil)
 '(scroll-bar-mode nil)
 '(server-mode nil)
 '(show-paren-mode t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 15)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(speedbar-verbosity-level 0)
 '(sr-speedbar-default-width 30)
 '(sr-speedbar-right-side nil)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil))


;; ========================================================================== ;;
;; Package Init/Install 
;;


(load "~/.emacs.d/init-use-package.el")


;; ========================================================================== ;;
;; EMACS configurations 
;;


;; Set variables
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      frame-title-format (list "GNU Emacs • %b • " (getenv "USER"))
      tab-width 4
      c-default-style "linux"
      indent-tabs-mode nil)

;; Add to list
;;
(add-to-list 'default-frame-alist '(height . 40)) ; -------- default window size
(add-to-list 'default-frame-alist '(width . 83))
(add-to-list 'command-switch-alist '("diff" . command-line-diff)) ; --- diff cmd

;; Global keyboard shortcuts
;;
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<S-mouse-2>") 'menu-bar-mode)
(global-set-key (kbd "M-q") 'visual-line-mode)
(global-set-key (kbd "s-<up>") 'move-line-up)
(global-set-key (kbd "s-<down>") 'move-line-down) 
(global-set-key (kbd "M-p") (kbd "C-u 2 M-v")) ; ----- scroll page behind cursor
(global-set-key (kbd "M-n") (kbd "C-u 2 C-v"))
(global-set-key (kbd "C-c C-8") 'narrow-window)
(global-set-key (kbd "C-x n f") 'narrow-to-eof)
(global-set-key (kbd "C-c M-d") 'date-command-on-buffer)
(global-set-key (kbd "C-x M-s") 'file-name-on-clipboard)
(global-set-key (kbd "C-S-r") 'reload-emacs-init-file)
(global-set-key (kbd "C-b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-x C-g") 'comment-or-uncomment-region-or-line)

;; Font
;;
(set-default-font "SF Mono:pixelsize=12:weight=medium")
;; (set-default-font "Fantasque Sans Mono:pixelsize=15")
;; (set-default-font "Roboto Mono:pixelsize=13:weight=regular")


(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))
(add-hook 'text-mode-hook 'flyspell-mode)


;; ========================================================================== ;;
;; Custom Lisp Functions
;;


(load "~/.emacs.d/init-lisp-functions.el")


;; Org-mode ====================================================================
;;


(define-key global-map (kbd "C-c d")
  (lambda () (interactive) (org-capture nil "d")))
(define-key global-map (kbd "C-c t")
  (lambda () (interactive) (org-capture nil "t")))
(define-key global-map (kbd "C-c C-t")
  (lambda () (interactive) (find-file org-todos-file)))
(define-key global-map (kbd "C-c C-d")
  (lambda () (interactive) (find-file org-default-diary-file)))
(define-key global-map (kbd "C-c C-o")
  (lambda () (org-open-at-point) (delete-window)))

(global-set-key (kbd "C-c C-1")
                (lambda () (interactive) (refile-to "Today")))
(global-set-key (kbd "C-c C-2")
                (lambda () (interactive) (refile-to "This Week")))
(global-set-key (kbd "C-c C-3")
                (lambda () (interactive) (refile-to "Next Week")))
(global-set-key (kbd "C-c C-4")
                (lambda () (interactive) (refile-to "Low Priority")))


;; Customize ===================================================================
;;


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "#c0c0c0"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(speedbar-button-face ((t (:foreground "green4" :height 70 :family "IBM Plex Sans"))))
 '(speedbar-directory-face ((t (:foreground "dim gray" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-file-face ((t (:foreground "cyan4" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-highlight-face ((t (:background "light gray" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-selected-face ((t (:foreground "red" :height 90 :family "IBM Plex Sans")))))
(put 'narrow-to-region 'disabled nil)
