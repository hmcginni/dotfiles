;;; init.el --- Emacs initialization file.

;;; Commentary:
;;    Emacs init.

;;; Code:

(package-initialize)


;; ─────────────────────────────────────────────────────────
;;; Customize variables:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-use-header-line nil)
 '(adaptive-wrap-extra-indent 3)
 '(column-number-mode t)
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(cursor-type (quote (bar . 1)))
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-theme-load-path (quote (custom-theme-directory t)) t)
 '(dap-inhibit-io nil)
 '(dap-python-executable "python3")
 '(display-time-mode nil)
 '(fill-column 80)
 '(flycheck-checker-error-threshold 500)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-flake8rc "~/.config/flake8/.flake8")
 '(flycheck-matlab-mlint-executable nil)
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-pylint-executable "pylint3")
 '(flycheck-shellcheck-follow-sources nil)
 '(font-use-system-font nil)
 '(fringe-mode (quote (10 . 20)) nil (fringe))
 '(git-gutter:update-interval 1)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-semantic-idle-scheduler-mode t)
 '(helm-completion-style (quote emacs))
 '(helm-echo-input-in-header-line t)
 '(helm-mode t)
 '(inhibit-startup-screen t)
 '(initial-org-scratch-message
   (concat "#+OPTIONS: toc:nil num:nil \\n:nil ::t -:t
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"/home/hrm/org/org.css\" />

* "
		   (shell-command-to-string "printf '%s' $(date +%Y%m%d)")
		   ": "))
 '(irony-additional-clang-options (quote ("-pthread" "-std=c++11")))
 '(line-spacing 0.12)
 '(linum-format " %3d ")
 '(lsp-clients-clangd-executable "clangd-8")
 '(lsp-document-highlight-delay 1)
 '(lsp-pyls-plugins-pycodestyle-enabled t)
 '(lsp-pyls-plugins-pycodestyle-ignore (quote ("E117" "W191" "D200")))
 '(lsp-pyls-plugins-pycodestyle-max-line-length 80)
 '(lsp-pyls-plugins-pycodestyle-select nil)
 '(lsp-pyls-plugins-pydocstyle-add-ignore nil)
 '(lsp-pyls-plugins-pydocstyle-enabled t)
 '(lsp-pyls-plugins-pydocstyle-ignore (quote ("D200" "D203" "D213" "D406" "D407")))
 '(lsp-pyls-plugins-pylint-args [--disable=W0312 (\, C0301)])
 '(lsp-pyls-plugins-pylint-enabled t)
 '(lsp-pyls-plugins-yapf-enabled nil)
 '(lsp-response-timeout 5)
 '(lsp-treemacs-theme "all-the-icons")
 '(lsp-ui-doc-delay 1)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 20)
 '(lsp-ui-doc-max-width 80)
 '(lsp-ui-imenu-kind-position (quote left))
 '(matlab-functions-have-end t)
 '(matlab-indent-function-body t)
 '(matlab-shell-command-switches (quote ("-nojvm")))
 '(menu-bar-mode nil)
 '(mlint-programs (quote ("/opt/matlab/2017a/bin/glnxa64/mlint")))
 '(mode-line-format
   (quote
	("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info
	 (vc-mode vc-mode)
	 mode-line-end-spaces)))
 '(neo-auto-indent-point t)
 '(neo-autorefresh nil)
 '(neo-mode-line-type (quote none))
 '(neo-show-updir-line t)
 '(neo-theme (quote icons))
 '(neo-window-fixed-size nil)
 '(neo-window-width 18)
 '(org-babel-load-languages
   (quote
	((python . t)
	 (emacs-lisp . t)
	 (matlab . t)
	 (shell . t))))
 '(org-babel-python-command "python3")
 '(org-clock-into-drawer 2)
 '(org-confirm-babel-evaluate nil)
 '(org-entities-user (quote (("chcl" "" nil "&#x2610;" "" "" ""))))
 '(org-export-headline-levels 4)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-file-apps
   (quote
	((auto-mode . emacs)
	 ("\\.mm\\'" . default)
	 ("\\.x?html?\\'" . default)
	 ("\\.pdf\\'" . "evince %s"))))
 '(org-html-htmlize-output-type (quote css))
 '(org-reverse-note-order t)
 '(org-use-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
	(sphinx-doc modern-cpp-font-lock gnu-elpa-keyring-update company-shell helm-xref dap-mode helm-gtags git-gutter org-present epresent flycheck helm dap-python company-lsp lsp-ui lsp-mode htmlize coffee-mode all-the-icons fill-column-indicator visual-fill-column magit delight ox-gfm adaptive-wrap-mode format-all github-theme dired-toggle sudo-edit matlab-mode markdown-mode json-mode company-box csv-mode cmake-font-lock cmake-mode systemd neotree adaptive-wrap ox-jira smooth-scrolling transpose-frame auto-package-update diminish use-package)))
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-menu-items 20)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(semantic-idle-breadcrumbs-format-tag-function (quote semantic-format-tag-canonical-name))
 '(semantic-idle-breadcrumbs-format-tag-list-function (quote semantic-idle-breadcrumbs--format-linear))
 '(semantic-idle-breadcrumbs-header-line-prefix "      •  ")
 '(semantic-idle-breadcrumbs-separator (quote mode-specific))
 '(show-paren-mode t)
 '(tab-width 4)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))



;; ─────────────────────────────────────────────────────────
;;; Custom variables:

(defgroup hrm nil
  "My custom variables."
  :group 'custom)

(defcustom initial-org-scratch-message nil
  "Initial message displayed in Org-scratch buffers."
  :group 'hrm
  :type '(sexp))


;; ─────────────────────────────────────────────────────────
;;; Global configurations:

;; Load custom libraries

(load "~/.emacs.d/hrm-use-package.el")
(load "~/.emacs.d/hrm-lisp-functions.el")

;; Startup functions
(server-start)
(global-auto-revert-mode t)
(hrm/set-theme nil)

;; Set variables
(setq-default c-default-style "stroustrup"
              ediff-window-setup-function 'ediff-setup-windows-plain
              frame-title-format (list "GNU Emacs " emacs-version " • %b")
			  prettify-symbols-alist '(("lambda" . 955)
									   ("->" . 129034)
									   ("=>" . 8658)))

(setq gc-cons-threshold 10000000)


;; Hooks and Associative Lists
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Font
;; (if (display-graphic-p)
;;     (hrm/dpi/set-scaled-font "Input" "regular"))



;; ─────────────────────────────────────────────────────────
;;; Keyboard shortcuts:

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-;") 'visual-line-mode)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer)
(global-set-key (kbd "C-j") 'fill-paragraph)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-<f12>") 'highlight-symbol-at-point)
(global-set-key (kbd "C-<f11>") (lambda () (interactive) (unhighlight-regexp t)))
(global-set-key (kbd "<C-prior>") 'backward-page) ; ----- Ctrl+Alt+PageUp
(global-set-key (kbd "<C-next>") 'forward-page) ; ----- Ctrl+Alt+PageDown
(global-set-key (kbd "C-M-#") 'count-matches)

(global-set-key (kbd "M-#") 'hrm/count-thing-at-point)
(global-set-key (kbd "s-<up>") 'hrm/move-line-up)
(global-set-key (kbd "s-<down>") 'hrm/move-line-down)
(global-set-key (kbd "C-c C-<left>") (lambda () (interactive) (hrm/resize "narrow")))
(global-set-key (kbd "C-c C-<right>") (lambda () (interactive) (hrm/resize "wide")))
(global-set-key (kbd "C-c C-<down>") (lambda () (interactive) (hrm/resize "half")))
(global-set-key (kbd "C-c C-<up>") (lambda () (interactive) (hrm/resize "half")))
(global-set-key (kbd "C-S-r") 'hrm/reload-emacs-init-file)
(global-set-key (kbd "C-b") 'hrm/switch-to-previous-buffer)
(global-set-key (kbd "C-x C-g") 'hrm/toggle-comment-region)
(global-set-key (kbd "<f8>") 'hrm/toggle-theme)
(global-set-key (kbd "C-\\") 'hrm/scratch)
(global-set-key (kbd "C-|") 'hrm/org-scratch)
(global-set-key (kbd "C-x n f") 'hrm/narrow-to-eof)
(global-set-key (kbd "C-x n i") 'hrm/narrow-to-defun-indirect)

(defvar hrm/insert-map)
(define-prefix-command 'hrm/insert-map)
(global-set-key (kbd "<C-insert>") hrm/insert-map)
(define-key hrm/insert-map (kbd "C--") 'hs-hide-all)
(define-key hrm/insert-map (kbd "C-=") 'hs-show-all)
(define-key hrm/insert-map (kbd "C-<insert>") 'hs-toggle-hiding)
(define-key hrm/insert-map (kbd "C-d") 'hrm/date-command-on-buffer)
(define-key hrm/insert-map (kbd "C-g") 'hrm/new-comment-section)
(define-key hrm/insert-map (kbd "C-b")
  (lambda () (interactive) (hrm/inline-code "bash")))
(define-key hrm/insert-map (kbd "C-p")
  (lambda () (interactive) (hrm/inline-code "python")))
(define-key hrm/insert-map (kbd "C-m")
  (lambda () (interactive) (hrm/inline-code "matlab")))


;; ─────────────────────────────────────────────────────────
;;; Hooks:

(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)

;; ─────────────────────────────────────────────────────────
;;; Customize faces:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:inherit mode-line :background "2f343f" :foreground "dark gray" :weight bold :height 0.9 :family "IBM Plex Sans Condensed"))))
 '(highlight-thing ((t (:inherit (quote hl-line)))))
 '(linum ((t (:inherit (shadow default) :foreground "#707070" :height 0.75 :family "Roboto Mono"))))
 '(lsp-ui-doc-background ((t (:background "#272A36"))))
 '(lsp-ui-doc-header ((t (:background "dim gray" :foreground "black" :family "IBM Plex Sans Condensed"))))
 '(lsp-ui-sideline-global ((t (:family "IBM Plex Sans Condensed"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(neo-banner-face ((t (:weight bold :height 0.9 :family "IBM Plex Sans Condensed"))))
 '(neo-button-face ((t (:underline nil :height 0.9 :family "IBM Plex Sans Condensed"))))
 '(neo-dir-link-face ((t (:height 0.9 :family "IBM Plex Sans Condensed"))))
 '(neo-expand-btn-face ((t (:height 0.9 :family "IBM Plex Sans Condensed"))))
 '(neo-file-link-face ((t (:height 0.9 :family "IBM Plex Sans Condensed"))))
 '(neo-header-face ((t (:height 0.9 :family "IBM Plex Sans Condensed"))))
 '(neo-root-dir-face ((t (:weight bold :height 0.9 :family "IBM Plex Sans Condensed"))))
 '(which-func ((t nil))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)


;; ─────────────────────────────────────────────────────────
;;; End:

(provide 'init)
;;; init.el ends here
