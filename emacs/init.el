;;; init.el --- Emacs initialization file.

;;; Commentary:
;;    Emacs init.

;;; Code:


;; ─────────────────────────────────────────────────────────
;;; Customize variables:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-use-header-line t)
 '(adaptive-wrap-extra-indent 3)
 '(column-number-mode t)
 '(cursor-type (quote (bar . 1)))
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-theme-load-path (quote (custom-theme-directory t)) t)
 '(debug-on-error nil)
 '(delete-selection-mode t)
 '(display-line-numbers t)
 '(display-line-numbers-width 3)
 '(display-time-mode nil)
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flycheck-checkers
   (quote
	(lsp-ui ada-gnat asciidoctor asciidoc bazel-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint)))
 '(flycheck-clang-language-standard "c++17")
 '(flycheck-disabled-checkers (quote (python-flake8 python-pycompile python-pylint)))
 '(flycheck-flake8rc "~/.config/flake8/.flake8")
 '(flycheck-idle-change-delay 0.9)
 '(flycheck-mode-hook (quote (flycheck-mode-set-explicitly)))
 '(flycheck-mode-line-prefix "Issues")
 '(flycheck-pycheckers-checkers (quote (pyflakes mypy2 mypy3)))
 '(flycheck-pycheckers-max-line-length 80)
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-mypy-config "~/.config/mypy/mypy.ini")
 '(flycheck-python-mypy-executable "mypy")
 '(flycheck-python-mypy-ini "~/.config/mypy/mypy.ini")
 '(flycheck-python-pylint-executable "pylint3")
 '(flycheck-shellcheck-follow-sources nil)
 '(font-use-system-font t)
 '(git-gutter:update-interval 1)
 '(global-flycheck-mode t)
 '(global-hl-line-mode nil)
 '(global-visual-line-mode t)
 '(helm-completion-style (quote emacs))
 '(helm-lisp-fuzzy-completion t)
 '(helm-locate-fuzzy-match nil)
 '(helm-recentf-fuzzy-match t)
 '(hrm/initial-org-scratch-message
   (format "#+options: toc:nil num:nil \\n:nil ::t -:t
#+html_head: <link rel=\"stylesheet\" href=\"/home/mcginh2/org/org.css\" />
#+title:
#+author: %s
#+date: %s

" user-full-name
(string-trim-right
 (shell-command-to-string "date -d \"monday this week\"  +%Y%m%d"))))
 '(hrm/weekly-org-notes-dir "/home/mcginh2/Documents/org/weekly/")
 '(hs-isearch-open t)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(line-spacing 0.2)
 '(lsp-document-highlight-delay 1)
 '(lsp-headerline-breadcrumb-enable t)
 '(lsp-headerline-breadcrumb-face (quote mode-line))
 '(lsp-headerline-breadcrumb-segments (quote (symbols)))
 '(lsp-pyls-plugins-flake8-enabled nil)
 '(lsp-pyls-plugins-jedi-completion-fuzzy t)
 '(lsp-pyls-plugins-pycodestyle-enabled t)
 '(lsp-pyls-plugins-pycodestyle-ignore (quote ("E117" "W191" "D200")))
 '(lsp-pyls-plugins-pycodestyle-max-line-length 80)
 '(lsp-pyls-plugins-pycodestyle-select nil)
 '(lsp-pyls-plugins-pydocstyle-add-ignore nil)
 '(lsp-pyls-plugins-pydocstyle-enabled t t)
 '(lsp-pyls-plugins-pydocstyle-ignore (quote ("D200" "D203" "D213" "D406" "D407")))
 '(lsp-pyls-plugins-pyflakes-enabled nil t)
 '(lsp-pyls-plugins-pylint-args [--disable=W0312 (\, C0301)])
 '(lsp-pyls-plugins-pylint-enabled nil t)
 '(lsp-pyls-plugins-rope-completion-enabled nil)
 '(lsp-pyls-plugins-yapf-enabled t)
 '(lsp-pyls-rename-backend (quote jedi))
 '(lsp-pyls-server-command (quote ("~/.local/bin/pyls")))
 '(lsp-response-timeout 5)
 '(lsp-ui-doc-delay 0.5)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 20)
 '(lsp-ui-doc-max-width 80)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-doc-show-with-mouse t)
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-diagnostic-max-line-length 85)
 '(lsp-ui-sideline-enable nil)
 '(matlab-align-to-paren nil)
 '(matlab-block-indent-tic-toc-flag t)
 '(matlab-fill-code t)
 '(matlab-highlight-cross-function-variables nil)
 '(matlab-indent-function-body t)
 '(matlab-shell-command-switches (quote ("-nojvm")))
 '(menu-bar-mode nil)
 '(mlint-programs (quote ("/opt/matlab/2019b/bin/glnxa64/mlint")))
 '(mode-line-format
   (quote
	("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "    " mode-line-position "			" mode-line-modes mode-line-misc-info
	 (vc-mode vc-mode)
	 mode-line-end-spaces)))
 '(org-agenda-custom-commands
   (quote
	(("A" "TODOs: PRIORITY \"A\" (due today)" tags-todo "+PRIORITY=\"A\"&-notes"
	  ((org-agenda-overriding-header "Today"))
	  nil)
	 ("T" "TODOs (by Priority)"
	  ((tags-todo "+PRIORITY=\"A\"&-notes"
				  ((org-agenda-overriding-header "Today")))
	   (tags-todo "+PRIORITY=\"B\"&-notes"
				  ((org-agenda-overriding-header "Tomorrow")))
	   (tags-todo "+PRIORITY=\"C\"&-notes"
				  ((org-agenda-overriding-header "This Week")))
	   (tags-todo "+PRIORITY=\"D\"&-notes"
				  ((org-agenda-overriding-header "Backlog"))))
	  ((org-overriding-columns-format "%60ITEM %TAGS"))
	  nil))))
 '(org-agenda-files (quote ("~/org/todos.org" "~/Dropbox/org/todos.org")))
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-babel-load-languages
   (quote
	((python . t)
	 (emacs-lisp . t)
	 (matlab . t)
	 (shell . t))))
 '(org-babel-python-command "python3")
 '(org-beamer-frame-level 2)
 '(org-beamer-theme "metropolis")
 '(org-checkbox-hierarchical-statistics nil)
 '(org-clock-into-drawer 2)
 '(org-confirm-babel-evaluate nil)
 '(org-ellipsis " ▼")
 '(org-entities-user (quote (("chcl" "" nil "&#x2610;" "" "" ""))))
 '(org-export-headline-levels 4)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-fancy-priorities-list (quote ("↑" "=" "-" "↓")))
 '(org-file-apps
   (quote
	((auto-mode . emacs)
	 ("\\.mm\\'" . default)
	 ("\\.x?html?\\'" . default)
	 ("\\.pdf\\'" . "evince %s"))))
 '(org-html-htmlize-output-type (quote css))
 '(org-html-indent nil)
 '(org-html-postamble t)
 '(org-html-postamble-format
   (quote
	(("en" "<div class=\"org-postamble\"><p>Author: %a</p>
<p>Last Updated: %T</p>
<p>HTML Generated with <a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> using <a href=\"https://orgmode.org/\">Org mode</a></p></div>"))))
 '(org-indent-indentation-per-level 1)
 '(org-link-frame-setup
   (quote
	((vm . vm-visit-folder-other-frame)
	 (vm-imap . vm-visit-imap-folder-other-frame)
	 (gnus . org-gnus-no-new-news)
	 (file . find-file)
	 (wl . wl-other-frame))))
 '(org-list-indent-offset 2)
 '(org-lowest-priority 68)
 '(org-modules
   (quote
	(org-ctags ol-docview ol-info org-mouse org-tempo ox-slack ox-jira)))
 '(org-priority-lowest 68)
 '(org-reverse-note-order t)
 '(org-startup-indented t)
 '(org-tags-column 0)
 '(org-tags-sort-function (quote org-string-collate-lessp))
 '(org-time-stamp-custom-formats (quote ("<%Y%m%d>" . "<%m/%d/%y %a %H:%M>")))
 '(org-use-property-inheritance t)
 '(org-use-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
	(hyperbole magit nord-theme page-break-lines doom-modeline org-fancy-priorities org ox-slack org-ql python-black company-box ox-md ox-odt company all-the-icons dump-jump sphinx-doc modern-cpp-font-lock gnu-elpa-keyring-update helm-xref helm-gtags org-present flycheck helm dap-python lsp-ui lsp-mode htmlize coffee-mode fill-column-indicator visual-fill-column delight ox-gfm adaptive-wrap-mode format-all github-theme dired-toggle sudo-edit matlab-mode markdown-mode json-mode csv-mode cmake-font-lock cmake-mode systemd adaptive-wrap ox-jira smooth-scrolling transpose-frame auto-package-update diminish use-package)))
 '(python-black-extra-args (quote ("-l 80")))
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-menu-items 50)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sr-speedbar-default-width 20)
 '(sr-speedbar-max-width 40)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(tab-width 4)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))

;; ─────────────────────────────────────────────────────────
;;; Custom variables:

(defgroup hrm nil
  "My custom variables."
  :group 'custom)

;; ─────────────────────────────────────────────────────────
;;; Hooks:

(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'server-done-hook 'lower-frame)
(add-hook 'server-switch-hook
		  (lambda ()
			(unless (mapcan (lambda (frame)
							  (assq 'display (frame-parameters frame)))
							(frame-list))
			  (make-frame '((window-system . x)
							(wait-for-wm . nil))))))


;; ─────────────────────────────────────────────────────────
;;; Global configurations:

;; Load custom libraries

(load "~/.emacs.d/hrm-use-package.el")
(load "~/.emacs.d/hrm-lisp-functions.el")

;; Startup functions
(helm-mode t)

;; Set variables
(setq-default c-default-style "stroustrup"
              ediff-window-setup-function 'ediff-setup-windows-plain
              frame-title-format (list "GNU Emacs " emacs-version " • %b")
			  prettify-symbols-alist '(("lambda" . ?λ)
									   ("->" . ?→)))
(setq visible-bell t
	  ring-bell-function #'ignore
	  frame-resize-pixelwise t
	  window-resize-pixelwise t)

;; Hooks and Associative Lists
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'default-frame-alist '(window-system . x))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(font . "SF Mono-9"))

;; Appearance (font and color theme)
(hrm/set-theme t)
(unless font-use-system-font
  (message "Using SF Mono")
  (hrm/dpi/scale-font "SF Mono" "Regular"))

(setq left-margin-width 2)
(setq right-margin-width 2)
(setq header-line-format " ")
;; (set-window-buffer nil (current-buffer))

;; ─────────────────────────────────────────────────────────
;;; Keyboard shortcuts

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-;") 'visual-line-mode)
(global-set-key (kbd "C-j") 'fill-paragraph)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-<f12>") 'highlight-symbol-at-point)
(global-set-key (kbd "C-<f11>") (lambda () (interactive) (unhighlight-regexp t)))
(global-set-key (kbd "<C-~>") 'backward-page)
(global-set-key (kbd "<C-!>") 'forward-page)
(global-set-key (kbd "C-M-#") 'count-matches)
(global-set-key (kbd "C-<f1>") 'xref-find-definitions)
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)
(global-set-key (kbd "<drag-mouse-9>") 'next-buffer)
(global-set-key (kbd "M-\\") 'just-one-space)
(global-set-key (kbd "C-<f4>") 'kill-current-buffer)
(global-set-key (kbd "M-#") 'hrm/count-thing-at-point)
(global-set-key (kbd "C-s-<up>") 'hrm/move-line-up)
(global-set-key (kbd "C-s-<down>") 'hrm/move-line-down)
(global-set-key (kbd "<f5>") 'hrm/reload-emacs-init-file)
(global-set-key (kbd "C-<escape>") 'hrm/switch-to-previous-buffer)
(global-set-key (kbd "C-x C-g") 'hrm/toggle-comment-region)
(global-set-key (kbd "C-\\") 'hrm/scratch)
(global-set-key (kbd "C-|") 'hrm/org-scratch)
(global-set-key (kbd "C-x n f") 'hrm/narrow-to-eof)
(global-set-key (kbd "C-x n i") 'hrm/narrow-to-defun-indirect)
(global-set-key (kbd "C-<prior>") 'hrm/previous-comment-section) ; ----- Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'hrm/next-comment-section) ; ----- Ctrl+PageUp
(define-key occur-mode-map (kbd "C-g") 'quit-window)

(defvar hrm/general-map)
(define-prefix-command 'hrm/general-map)
(global-set-key (kbd "C-z") 'hrm/general-map)
(define-key hrm/general-map (kbd "C-b") 'hrm/switch-to-previous-buffer)
(define-key hrm/general-map (kbd "C-c") 'kill-emacs)
(define-key hrm/general-map (kbd "C-t") 'hrm/org-show-todos)
(define-key hrm/general-map (kbd "C-T") 'hrm/org-show-todos-today)
(define-key hrm/general-map (kbd "C-z") 'hrm/toggle-theme)
(define-key hrm/general-map (kbd "C-s") 'hrm/occur-thing-at-point)
(define-key hrm/general-map (kbd "C-<left>") (lambda () (interactive) (hrm/resize "narrow")))
(define-key hrm/general-map (kbd "C-<right>") (lambda () (interactive) (hrm/resize "wide")))
(define-key hrm/general-map (kbd "C-<down>") (lambda () (interactive) (hrm/resize "half")))
(define-key hrm/general-map (kbd "C-<up>") (lambda () (interactive) (hrm/resize "half")))

(defvar hrm/insert-map)
(define-prefix-command 'hrm/insert-map)
(global-set-key (kbd "<C-insert>") 'hrm/insert-map)
(define-key hrm/insert-map (kbd "C--") 'hs-hide-block)
(define-key hrm/insert-map (kbd "C-+") 'hs-show-block)
(define-key hrm/insert-map (kbd "C-<insert>") 'hs-toggle-hiding)
(define-key hrm/insert-map (kbd "C-d") 'hrm/insert-date)
(define-key hrm/insert-map (kbd "C-t") 'hrm/insert-date-time)
(define-key hrm/insert-map (kbd "C-g") 'hrm/new-comment-section)
(define-key hrm/insert-map (kbd "C-b") (lambda () (interactive) (hrm/inline-code "bash")))
(define-key hrm/insert-map (kbd "C-p") (lambda () (interactive) (hrm/inline-code "python")))
(define-key hrm/insert-map (kbd "C-m") (lambda () (interactive) (hrm/inline-code "matlab")))


;; ─────────────────────────────────────────────────────────
;;; Customize faces:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:inherit default :box nil :family "sans"))))
 '(highlight-thing ((t (:inherit (quote hl-line)))))
 '(line-number ((t (:inherit (shadow default) :foreground "#626d82" :height 0.8))))
 '(line-number-current-line ((t (:inherit line-number :foreground "#00d5f7"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(matlab-cellbreak-face ((t (:inherit font-lock-builtin-face :overline t :weight bold))))
 '(mode-line ((t (:background "#7E7E7E" :foreground "#EAEAEA" :height 80 :family "sans"))))
 '(org-document-info ((((class color) (background light)) (:foreground "cyan4")) (((class color) (background dark)) (:foreground "pale turquoise")) (t nil)))
 '(org-ellipsis ((t (:slant normal))))
 '(org-priority ((t (:inherit outline-7)))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(face-attribute 'font-lock-builtin-face :foreground)

;; ─────────────────────────────────────────────────────────
;;; End:

(provide 'init)
;;; init.el ends here
