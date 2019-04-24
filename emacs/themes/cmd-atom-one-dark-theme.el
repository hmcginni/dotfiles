;;; cmd-atom-one-dark-theme.el --- Atom One Dark color theme
;;
;; Copyright 2015 Jonathan Chu
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/cmd-atom-one-dark-theme
;; Version: 0.3.1
;;
;;; Commentary:
;;
;; An Emacs port of the Atom One Dark theme from Atom.io.
;;
;;; Code:

(deftheme cmd-atom-one-dark
  "Atom One Dark - An Emacs port of the Atom One Dark theme from Atom.io.")

(defvar cmd-atom-one-dark-colors-alist
  '(("cmd-atom-one-dark-accent"   . "#528BFF")
    ;; ("cmd-atom-one-dark-fg"       . "#ABB2BF")
    ("cmd-atom-one-dark-fg"       . "#F0F0F0")
    ("cmd-atom-one-dark-bg"       . "#2F343F")
    ("cmd-atom-one-dark-bg-1"     . "#121417")
    ("cmd-atom-one-dark-bg-hl"    . "#2F343D")
    ("cmd-atom-one-dark-mono-1"   . "#ABB2BF")
    ("cmd-atom-one-dark-mono-2"   . "#828997")
    ("cmd-atom-one-dark-mono-3"   . "#5C6370")
    ("cmd-atom-one-dark-cyan"     . "#56B6C2")
    ("cmd-atom-one-dark-blue"     . "#61AFEF")
    ("cmd-atom-one-dark-purple"   . "#C678DD")
    ("cmd-atom-one-dark-green"    . "#98C379")
    ("cmd-atom-one-dark-red-1"    . "#E06C75")
    ("cmd-atom-one-dark-red-2"    . "#BE5046")
    ("cmd-atom-one-dark-orange-1" . "#D19A66")
    ("cmd-atom-one-dark-orange-2" . "#E5C07B")
    ("cmd-atom-one-dark-gray"     . "#3E4451")
    ("cmd-atom-one-dark-silver"   . "#AAAAAA")
    ("cmd-atom-one-dark-black"    . "#0F1011"))
  "List of Atom One Dark colors.")

(defmacro cmd-atom-one-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    cmd-atom-one-dark-colors-alist))
     ,@body))

(cmd-atom-one-dark-with-color-variables
  (custom-theme-set-faces
   'cmd-atom-one-dark
   `(default ((t (:foreground ,cmd-atom-one-dark-fg))))
   `(success ((t (:foreground ,cmd-atom-one-dark-green))))
   `(warning ((t (:foreground ,cmd-atom-one-dark-orange-2))))
   `(error ((t (:foreground ,cmd-atom-one-dark-red-1 :weight bold))))
   `(link ((t (:foreground ,cmd-atom-one-dark-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,cmd-atom-one-dark-blue :underline t :weight normal))))
   `(cursor ((t (:background ,cmd-atom-one-dark-accent))))
   `(region ((t (:background ,cmd-atom-one-dark-gray))))
   `(highlight ((t (:background ,cmd-atom-one-dark-gray))))
   `(hl-line ((t (:background ,cmd-atom-one-dark-bg-hl))))
   `(vertical-border ((t (:foreground ,cmd-atom-one-dark-mono-3))))
   `(secondary-selection ((t (:background ,cmd-atom-one-dark-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,cmd-atom-one-dark-silver))))

   `(font-lock-builtin-face ((t (:foreground ,cmd-atom-one-dark-cyan))))
   `(font-lock-comment-face ((t (:foreground ,cmd-atom-one-dark-mono-3))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,cmd-atom-one-dark-blue))))
   `(font-lock-keyword-face ((t (:foreground ,cmd-atom-one-dark-purple))))
   `(font-lock-preprocessor-face ((t (:foreground ,cmd-atom-one-dark-mono-2))))
   `(font-lock-string-face ((t (:foreground ,cmd-atom-one-dark-green))))
   `(font-lock-type-face ((t (:foreground ,cmd-atom-one-dark-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,cmd-atom-one-dark-orange-1))))
   `(font-lock-variable-name-face ((t (:foreground ,cmd-atom-one-dark-red-1))))
   `(font-lock-warning-face ((t (:foreground ,cmd-atom-one-dark-mono-3 :bold t))))

   ;; mode-line
   `(mode-line ((t (:background ,cmd-atom-one-dark-black :foreground ,cmd-atom-one-dark-silver))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,cmd-atom-one-dark-gray))))
   
   ;; company-mode
   `(company-tooltip ((t (:foreground ,cmd-atom-one-dark-fg :background ,cmd-atom-one-dark-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,cmd-atom-one-dark-mono-2 :background ,cmd-atom-one-dark-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,cmd-atom-one-dark-fg :background ,cmd-atom-one-dark-gray))))
   `(company-tooltip-mouse ((t (:background ,cmd-atom-one-dark-gray))))
   `(company-tooltip-common ((t (:foreground ,cmd-atom-one-dark-orange-2 :background ,cmd-atom-one-dark-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,cmd-atom-one-dark-orange-2 :background ,cmd-atom-one-dark-gray))))
   `(company-preview ((t (:background ,cmd-atom-one-dark-bg))))
   `(company-preview-common ((t (:foreground ,cmd-atom-one-dark-orange-2 :background ,cmd-atom-one-dark-bg))))
   `(company-scrollbar-fg ((t (:background ,cmd-atom-one-dark-mono-1))))
   `(company-scrollbar-bg ((t (:background ,cmd-atom-one-dark-bg-1))))

   ;; compilation
   `(compilation-face ((t (:foreground ,cmd-atom-one-dark-fg))))
   `(compilation-line-number ((t (:foreground ,cmd-atom-one-dark-mono-2))))
   `(compilation-column-number ((t (:foreground ,cmd-atom-one-dark-mono-2))))

   ;; isearch
   `(isearch ((t (:foreground ,cmd-atom-one-dark-bg :background ,cmd-atom-one-dark-purple))))
   `(isearch-fail ((t (:foreground ,cmd-atom-one-dark-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,cmd-atom-one-dark-purple :background ,cmd-atom-one-dark-bg-1 :underline ,cmd-atom-one-dark-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ;; helm
   `(helm-header ((t (:foreground ,cmd-atom-one-dark-mono-2
                      :background ,cmd-atom-one-dark-bg
                      :underline nil
                      :box (:line-width 6 :color ,cmd-atom-one-dark-bg)))))
   `(helm-source-header ((t (:foreground ,cmd-atom-one-dark-orange-2
                             :background ,cmd-atom-one-dark-bg
                             :underline nil
                             :weight bold
                             :box (:line-width 6 :color ,cmd-atom-one-dark-bg)))))
   `(helm-selection ((t (:background ,cmd-atom-one-dark-gray))))
   `(helm-selection-line ((t (:background ,cmd-atom-one-dark-gray))))
   `(helm-visible-mark ((t (:foreground ,cmd-atom-one-dark-bg :foreground ,cmd-atom-one-dark-orange-2))))
   `(helm-candidate-number ((t (:foreground ,cmd-atom-one-dark-green :background ,cmd-atom-one-dark-bg-1))))
   `(helm-separator ((t (:background ,cmd-atom-one-dark-bg :foreground ,cmd-atom-one-dark-red-1))))
   `(helm-M-x-key ((t (:foreground ,cmd-atom-one-dark-orange-1))))
   `(helm-bookmark-addressbook ((t (:foreground ,cmd-atom-one-dark-orange-1))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,cmd-atom-one-dark-purple))))
   `(helm-bookmark-info ((t (:foreground ,cmd-atom-one-dark-green))))
   `(helm-bookmark-man ((t (:foreground ,cmd-atom-one-dark-orange-2))))
   `(helm-bookmark-w3m ((t (:foreground ,cmd-atom-one-dark-purple))))
   `(helm-match ((t (:foreground ,cmd-atom-one-dark-orange-2))))
   `(helm-ff-directory ((t (:foreground ,cmd-atom-one-dark-cyan :background ,cmd-atom-one-dark-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,cmd-atom-one-dark-fg :background ,cmd-atom-one-dark-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,cmd-atom-one-dark-green :background ,cmd-atom-one-dark-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,cmd-atom-one-dark-red-1 :background ,cmd-atom-one-dark-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,cmd-atom-one-dark-orange-2 :background ,cmd-atom-one-dark-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,cmd-atom-one-dark-bg :background ,cmd-atom-one-dark-orange-2 :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,cmd-atom-one-dark-red-1))))
   `(helm-buffer-process ((t (:foreground ,cmd-atom-one-dark-mono-2))))
   `(helm-buffer-saved-out ((t (:foreground ,cmd-atom-one-dark-fg))))
   `(helm-buffer-size ((t (:foreground ,cmd-atom-one-dark-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,cmd-atom-one-dark-purple))))
   `(helm-grep-cmd-line ((t (:foreground ,cmd-atom-one-dark-cyan))))
   `(helm-grep-file ((t (:foreground ,cmd-atom-one-dark-fg))))
   `(helm-grep-finish ((t (:foreground ,cmd-atom-one-dark-green))))
   `(helm-grep-lineno ((t (:foreground ,cmd-atom-one-dark-mono-2))))
   `(helm-grep-finish ((t (:foreground ,cmd-atom-one-dark-red-1))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,cmd-atom-one-dark-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,cmd-atom-one-dark-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,cmd-atom-one-dark-orange-2 :weight bold))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,cmd-atom-one-dark-red-1 :background ,cmd-atom-one-dark-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,cmd-atom-one-dark-gray :weight bold))))

   ;; web-mode
   `(web-mode-symbol-face ((t (:foreground ,cmd-atom-one-dark-orange-1))))


;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'cmd-atom-one-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; cmd-atom-one-dark-theme.el ends here
