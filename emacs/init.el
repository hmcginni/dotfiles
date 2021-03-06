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
 '(dap-inhibit-io nil)
 '(dap-python-executable "python3")
 '(display-time-mode nil)
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
 '(fringe-mode (quote (10 . 20)) nil (fringe))
 '(git-gutter:update-interval 1)
 '(global-flycheck-mode t)
 '(global-hl-line-mode nil)
 '(global-linum-mode t)
 '(helm-completion-style (quote emacs))
 '(helm-lisp-fuzzy-completion t)
 '(helm-locate-fuzzy-match nil)
 '(helm-recentf-fuzzy-match t)
 '(inhibit-startup-screen t)
 '(initial-org-scratch-message
   (substitute-in-file-name "#+OPTIONS: toc:nil num:nil \\n:nil ::t -:t
#+HTML_HEAD: <link rel=\"stylesheet\" href=\"$HOME/org/org.css\" />
#+TITLE:

"))
 '(irony-additional-clang-options (quote ("-pthread" "-std=c++11")))
 '(line-spacing 0.1)
 '(linum-format " %3d ")
 '(lsp-document-highlight-delay 1)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-headerline-breadcrumb-face (quote mode-line))
 '(lsp-pyls-configuration-sources ["flake8"])
 '(lsp-pyls-plugins-flake8-config "~/.config/flake8/.flake8")
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
 '(lsp-ui-doc-show-with-mouse nil)
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-diagnostic-max-line-length 85)
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
	(("A" "TODOs: PRIORITY \"A\" (due today)" tags-todo "+PRIORITY=\"A\""
	  ((org-agenda-overriding-header "Today"))
	  nil)
	 ("T" "TODOs (by Priority)"
	  ((tags-todo "+PRIORITY=\"A\""
				  ((org-agenda-overriding-header "Today")))
	   (tags-todo "+PRIORITY=\"B\""
				  ((org-agenda-overriding-header "This Week")))
	   (tags-todo "+PRIORITY=\"C\""
				  ((org-agenda-overriding-header "This Month"))))
	  nil nil))))
 '(org-agenda-todo-keyword-format "%-11s")
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
 '(org-export-backends (quote (ascii html md odt org)))
 '(org-export-headline-levels 4)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-fancy-priorities-list (quote ("[#today]" "[#week]" "[#month]" "[#tbd]")))
 '(org-file-apps
   (quote
	((auto-mode . emacs)
	 ("\\.mm\\'" . default)
	 ("\\.x?html?\\'" . default)
	 ("\\.pdf\\'" . "evince %s"))))
 '(org-html-htmlize-output-type (quote css))
 '(org-html-indent t)
 '(org-html-postamble t)
 '(org-html-postamble-format
   (quote
	(("en" "<div class=\"org-postamble\"><p>Author: %a</p>
<p>Last Updated: %T</p>
<p>Created with <a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> using <a href=\"https://orgmode.org/\">Org mode</a></p></div>"))))
 '(org-lowest-priority 68)
 '(org-modules
   (quote
	(ol-docview ol-eww ol-gnus org-habit ol-info ol-irc org-tempo ol-w3m ox-slack)))
 '(org-priority-lowest 68)
 '(org-reverse-note-order t)
 '(org-tags-column -70)
 '(org-tags-sort-function (quote org-string-collate-lessp))
 '(org-time-stamp-custom-formats (quote ("%a" . "<%m/%d/%y %a %H:%M>")))
 '(org-use-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
	(doom-modeline org-fancy-priorities org ox-slack org-ql python-black company-box ox-md ox-odt company all-the-icons dump-jump sphinx-doc modern-cpp-font-lock gnu-elpa-keyring-update helm-xref helm-gtags org-present epresent flycheck helm dap-python lsp-ui lsp-mode htmlize coffee-mode fill-column-indicator visual-fill-column delight ox-gfm adaptive-wrap-mode format-all github-theme dired-toggle sudo-edit matlab-mode markdown-mode json-mode csv-mode cmake-font-lock cmake-mode systemd adaptive-wrap ox-jira smooth-scrolling transpose-frame auto-package-update diminish use-package)))
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
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'server-done-hook 'lower-frame)
(add-hook 'server-switch-hook
		  (lambda ()
			(unless (mapcan
					 (lambda (frame)
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
(linum-mode)

;; Set variables
(setq-default c-default-style "stroustrup"
			  linum-mode t
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
(add-to-list 'default-frame-alist '((window-system . x)
									(inhibit-double-buffering . t)))

;; Appearance (font and color theme)
(hrm/set-theme nil)
(unless font-use-system-font
  (hrm/dpi/scale-font "Roboto Mono" "normal"))

;; (setq left-margin-width 2)
;; (setq right-margin-width 2)
;; (setq header-line-format " ")
;; (set-window-buffer nil (current-buffer))

;; ─────────────────────────────────────────────────────────
;;; Keyboard shortcuts

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-;") 'visual-line-mode)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer)
(global-set-key (kbd "C-j") 'fill-paragraph)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-<f12>") 'highlight-symbol-at-point)
(global-set-key (kbd "C-<f11>") (lambda () (interactive) (unhighlight-regexp t)))
(global-set-key (kbd "<C-~>") 'backward-page)
(global-set-key (kbd "<C-!>") 'forward-page)
(global-set-key (kbd "C-M-#") 'count-matches)
(global-set-key (kbd "C-<f1>") 'xref-find-definitions)
(global-set-key (kbd "M-\\") 'just-one-space)

(global-set-key (kbd "M-#") 'hrm/count-thing-at-point)
(global-set-key (kbd "C-s-<up>") 'hrm/move-line-up)
(global-set-key (kbd "C-s-<down>") 'hrm/move-line-down)
(global-set-key (kbd "C-S-r") 'hrm/reload-emacs-init-file)
(global-set-key (kbd "C-b") 'hrm/switch-to-previous-buffer)
(global-set-key (kbd "C-x C-g") 'hrm/toggle-comment-region)
(global-set-key (kbd "C-\\") 'hrm/scratch)
(global-set-key (kbd "C-|") 'hrm/org-scratch)
(global-set-key (kbd "C-x n f") 'hrm/narrow-to-eof)
(global-set-key (kbd "C-x n i") 'hrm/narrow-to-defun-indirect)
(global-set-key (kbd "C-<prior>") 'hrm/previous-comment-section) ; ----- Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'hrm/next-comment-section) ; ----- Ctrl+PageUp
(global-set-key (kbd "C-c C-t") 'hrm/org-show-todos)

(defvar hrm/appearance-map)
(define-prefix-command 'hrm/appearance-map)
(global-set-key (kbd "C-`") hrm/appearance-map)
(define-key hrm/appearance-map (kbd "C-`") 'hrm/toggle-theme)
(define-key hrm/appearance-map (kbd "C-<left>") (lambda () (interactive) (hrm/resize "narrow")))
(define-key hrm/appearance-map (kbd "C-<right>") (lambda () (interactive) (hrm/resize "wide")))
(define-key hrm/appearance-map (kbd "C-<down>") (lambda () (interactive) (hrm/resize "half")))
(define-key hrm/appearance-map (kbd "C-<up>") (lambda () (interactive) (hrm/resize "half")))

(defvar hrm/insert-map)
(define-prefix-command 'hrm/insert-map)
(global-set-key (kbd "<C-insert>") hrm/insert-map)
(define-key hrm/insert-map (kbd "C--") 'hs-hide-block)
(define-key hrm/insert-map (kbd "C-+") 'hs-show-block)
(define-key hrm/insert-map (kbd "C-<insert>") 'hs-toggle-hiding)
(define-key hrm/insert-map (kbd "C-d") 'hrm/insert-date)
(define-key hrm/insert-map (kbd "C-t") 'hrm/insert-date-time)
(define-key hrm/insert-map (kbd "C-g") 'hrm/new-comment-section)
(define-key hrm/insert-map (kbd "C-b")
  (lambda () (interactive) (hrm/inline-code "bash")))
(define-key hrm/insert-map (kbd "C-p")
  (lambda () (interactive) (hrm/inline-code "python")))
(define-key hrm/insert-map (kbd "C-m")
  (lambda () (interactive) (hrm/inline-code "matlab")))


;; ─────────────────────────────────────────────────────────
;;; Customize faces:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-thing ((t (:inherit (quote hl-line)))))
 '(linum ((t (:inherit (shadow default) :foreground "#626d82" :height 0.8))))
 '(lsp-ui-sideline-symbol ((t (:foreground "grey" :box nil :height 0.9))))
 '(lsp-ui-sideline-symbol-info ((t (:height 0.9))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(mode-line ((t (:background "#7E7E7E" :foreground "#EAEAEA" :height 0.9 :family "Tex Gyre Heros"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(org-priority ((t (:inherit outline-7 :height 0.9))))
 '(variable-pitch ((t (:family "IBM Plex Sans")))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)


;; ─────────────────────────────────────────────────────────
;;; End:

(provide 'init)
;;; init.el ends here
