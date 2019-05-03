;;; cmd-atom-one-light-theme.el --- Atom One Light color theme
;;
;; Copyright 2016 Jonathan Chu
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/cmd-atom-one-light-theme
;; Version: 0.1.0
;;
;;; Commentary:
;;
;; An Emacs port of the Atom One Light theme from Atom.io.
;;
;;; Code:

(deftheme cmd-atom-one-light
  "Atom One Light - An Emacs port of the Atom One Light theme from Atom.io.")

(defvar cmd-atom-one-light-colors-alist
  '(("cmd-atom-one-light-accent"   . "#526FFF")
    ("cmd-atom-one-light-fg"       . "#383A42")
    ("cmd-atom-one-light-bg"       . "#FAFAFA")
    ("cmd-atom-one-light-bg-1"     . "#E5E5E6")  ;; MEBEE?
    ("cmd-atom-one-light-bg-hl"    . "#F0F0F0")  ;; MEBEE?
    ("cmd-atom-one-light-modeline" . "#7E7E7E")
    ("cmd-atom-one-light-mono-1"   . "#383A42")
    ("cmd-atom-one-light-mono-2"   . "#696C77")
    ("cmd-atom-one-light-mono-3"   . "#A0A1A7")
    ("cmd-atom-one-light-cyan"     . "#0184BC")
    ("cmd-atom-one-light-blue"     . "#4078F2")
    ("cmd-atom-one-light-purple"   . "#A626A4")
    ("cmd-atom-one-light-green"    . "#50A14F")
    ("cmd-atom-one-light-red-1"    . "#E45649")
    ("cmd-atom-one-light-red-2"    . "#CA1243")
    ("cmd-atom-one-light-orange-1" . "#986801")
    ("cmd-atom-one-light-orange-2" . "#C18401")
    ("cmd-atom-one-light-gray"     . "#EAEAEA")  ;; MEBEE?
    ("cmd-atom-one-light-silver"   . "#AAAAAA")  ;; TODO
    ("cmd-atom-one-light-black"    . "#0F1011")
    ("cmd-atom-one-light-newhl"    . "#5796B6")) ;; TODO
  "List of Atom One Light colors.")

(defmacro cmd-atom-one-light-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    cmd-atom-one-light-colors-alist))
     ,@body))

(cmd-atom-one-light-with-color-variables
 (custom-theme-set-faces
  'cmd-atom-one-light
  `(default ((t (:foreground ,cmd-atom-one-light-fg))))
  `(success ((t (:foreground ,cmd-atom-one-light-green))))
  `(warning ((t (:foreground ,cmd-atom-one-light-orange-2))))
  `(error ((t (:foreground ,cmd-atom-one-light-red-1 :weight bold))))
  `(link ((t (:foreground ,cmd-atom-one-light-blue :underline t :weight bold))))
  `(link-visited ((t (:foreground ,cmd-atom-one-light-blue :underline t :weight normal))))
  `(cursor ((t (:background ,cmd-atom-one-light-accent))))
  `(fringe ((t (:background ,cmd-atom-one-light-bg))))
  `(region ((t (:background ,cmd-atom-one-light-newhl))))
  `(highlight ((t (:background ,cmd-atom-one-light-newhl))))
  `(hl-line ((t (:background ,cmd-atom-one-light-bg-hl))))
  `(vertical-border ((t (:foreground ,cmd-atom-one-light-mono-3))))
  `(secondary-selection ((t (:background ,cmd-atom-one-light-bg-1))))
  `(query-replace ((t (:inherit (isearch)))))
  `(minibuffer-prompt ((t (:foreground ,cmd-atom-one-light-silver))))

  `(font-lock-builtin-face ((t (:foreground ,cmd-atom-one-light-cyan))))
  `(font-lock-comment-face ((t (:italic t :foreground ,cmd-atom-one-light-mono-3))))
  `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
  `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
  `(font-lock-function-name-face ((t (:italic t :foreground ,cmd-atom-one-light-blue))))
  `(font-lock-keyword-face ((t (:foreground ,cmd-atom-one-light-purple))))
  `(font-lock-preprocessor-face ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(font-lock-string-face ((t (:foreground ,cmd-atom-one-light-green))))
  `(font-lock-type-face ((t (:foreground ,cmd-atom-one-light-orange-2))))
  `(font-lock-constant-face ((t (:foreground ,cmd-atom-one-light-orange-1))))
  `(font-lock-variable-name-face ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(font-lock-warning-face ((t (:foreground ,cmd-atom-one-light-mono-3 :bold t))))

  ;; mode-line
  `(mode-line ((t (:background ,cmd-atom-one-light-modeline :foreground ,cmd-atom-one-light-gray))))
  `(mode-line-buffer-id ((t (:weight bold))))
  `(mode-line-emphasis ((t (:weight bold))))
  `(mode-line-inactive ((t (:background ,cmd-atom-one-light-gray))))

  ;; ido
  `(ido-first-match ((t (:foreground ,cmd-atom-one-light-purple :weight bold))))
  `(ido-only-match ((t (:foreground ,cmd-atom-one-light-red-1 :weight bold))))
  `(ido-subdir ((t (:foreground ,cmd-atom-one-light-blue))))
  `(ido-virtual ((t (:foreground ,cmd-atom-one-light-mono-3))))

  ;; ace-jump
  `(ace-jump-face-background ((t (:foreground ,cmd-atom-one-light-mono-3 :background ,cmd-atom-one-light-bg-1 :inverse-video nil))))
  `(ace-jump-face-foreground ((t (:foreground ,cmd-atom-one-light-red-1 :background ,cmd-atom-one-light-bg-1 :inverse-video nil))))

  ;; company-mode
  `(company-tooltip ((t (:foreground ,cmd-atom-one-light-fg :background ,cmd-atom-one-light-bg-1))))
  `(company-tooltip-annotation ((t (:foreground ,cmd-atom-one-light-mono-2 :background ,cmd-atom-one-light-bg-1))))
  `(company-tooltip-selection ((t (:foreground ,cmd-atom-one-light-fg :background ,cmd-atom-one-light-newhl))))
  `(company-tooltip-mouse ((t (:background ,cmd-atom-one-light-newhl))))
  `(company-tooltip-common ((t (:foreground ,cmd-atom-one-light-orange-2 :background ,cmd-atom-one-light-bg-1))))
  `(company-tooltip-common-selection ((t (:foreground ,cmd-atom-one-light-orange-2 :background ,cmd-atom-one-light-newhl))))
  `(company-preview ((t (:background ,cmd-atom-one-light-bg))))
  `(company-preview-common ((t (:foreground ,cmd-atom-one-light-orange-2 :background ,cmd-atom-one-light-bg))))
  `(company-scrollbar-fg ((t (:background ,cmd-atom-one-light-mono-1))))
  `(company-scrollbar-bg ((t (:background ,cmd-atom-one-light-bg-1))))

  ;; compilation
  `(compilation-face ((t (:foreground ,cmd-atom-one-light-fg))))
  `(compilation-line-number ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(compilation-column-number ((t (:foreground ,cmd-atom-one-light-mono-2))))

  ;; isearch
  `(isearch ((t (:foreground ,cmd-atom-one-light-bg :background ,cmd-atom-one-light-purple))))
  `(isearch-fail ((t (:foreground ,cmd-atom-one-light-red-2 :background nil))))
  `(lazy-highlight ((t (:foreground ,cmd-atom-one-light-purple :background ,cmd-atom-one-light-bg-1 :underline ,cmd-atom-one-light-purple))))

  ;; diff-hl (https://github.com/dgutov/diff-hl)
  '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
  '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
  '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

  ;; dired-mode
  '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
  '(dired-flagged ((t (:inherit (diff-hl-delete)))))
  '(dired-symlink ((t (:foreground "#FD5FF1"))))

  ;; helm
  `(helm-header ((t (:foreground ,cmd-atom-one-light-mono-2
                                 :background ,cmd-atom-one-light-bg
                                 :underline nil
                                 :box (:line-width 6 :color ,cmd-atom-one-light-bg)))))
  `(helm-source-header ((t (:foreground ,cmd-atom-one-light-orange-2
                                        :background ,cmd-atom-one-light-bg
                                        :underline nil
                                        :weight bold
                                        :box (:line-width 6 :color ,cmd-atom-one-light-bg)))))
  `(helm-selection ((t (:background ,cmd-atom-one-light-gray))))
  `(helm-selection-line ((t (:background ,cmd-atom-one-light-gray))))
  `(helm-visible-mark ((t (:foreground ,cmd-atom-one-light-bg :foreground ,cmd-atom-one-light-orange-2))))
  `(helm-candidate-number ((t (:foreground ,cmd-atom-one-light-green :background ,cmd-atom-one-light-bg-1))))
  `(helm-separator ((t (:background ,cmd-atom-one-light-bg :foreground ,cmd-atom-one-light-red-1))))
  `(helm-M-x-key ((t (:foreground ,cmd-atom-one-light-orange-1))))
  `(helm-bookmark-addressbook ((t (:foreground ,cmd-atom-one-light-orange-1))))
  `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
  `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
  `(helm-bookmark-gnus ((t (:foreground ,cmd-atom-one-light-purple))))
  `(helm-bookmark-info ((t (:foreground ,cmd-atom-one-light-green))))
  `(helm-bookmark-man ((t (:foreground ,cmd-atom-one-light-orange-2))))
  `(helm-bookmark-w3m ((t (:foreground ,cmd-atom-one-light-purple))))
  `(helm-match ((t (:foreground ,cmd-atom-one-light-orange-2))))
  `(helm-ff-directory ((t (:foreground ,cmd-atom-one-light-cyan :background ,cmd-atom-one-light-bg :weight bold))))
  `(helm-ff-file ((t (:foreground ,cmd-atom-one-light-fg :background ,cmd-atom-one-light-bg :weight normal))))
  `(helm-ff-executable ((t (:foreground ,cmd-atom-one-light-green :background ,cmd-atom-one-light-bg :weight normal))))
  `(helm-ff-invalid-symlink ((t (:foreground ,cmd-atom-one-light-red-1 :background ,cmd-atom-one-light-bg :weight bold))))
  `(helm-ff-symlink ((t (:foreground ,cmd-atom-one-light-orange-2 :background ,cmd-atom-one-light-bg :weight bold))))
  `(helm-ff-prefix ((t (:foreground ,cmd-atom-one-light-bg :background ,cmd-atom-one-light-orange-2 :weight normal))))
  `(helm-buffer-not-saved ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(helm-buffer-process ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(helm-buffer-saved-out ((t (:foreground ,cmd-atom-one-light-fg))))
  `(helm-buffer-size ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(helm-buffer-directory ((t (:foreground ,cmd-atom-one-light-purple))))
  `(helm-grep-cmd-line ((t (:foreground ,cmd-atom-one-light-cyan))))
  `(helm-grep-file ((t (:foreground ,cmd-atom-one-light-fg))))
  `(helm-grep-finish ((t (:foreground ,cmd-atom-one-light-green))))
  `(helm-grep-lineno ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(helm-grep-finish ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))

  ;; git-commit
  `(git-commit-comment-action  ((t (:foreground ,cmd-atom-one-light-green :weight bold))))
  `(git-commit-comment-branch  ((t (:foreground ,cmd-atom-one-light-blue :weight bold))))
  `(git-commit-comment-heading ((t (:foreground ,cmd-atom-one-light-orange-2 :weight bold))))

  ;; magit
  `(magit-section-highlight ((t (:background ,cmd-atom-one-light-bg-hl))))
  `(magit-section-heading ((t (:foreground ,cmd-atom-one-light-orange-2 :weight bold))))
  `(magit-section-heading-selection ((t (:foreground ,cmd-atom-one-light-fg :weight bold))))
  `(magit-diff-file-heading ((t (:weight bold))))
  `(magit-diff-file-heading-highlight ((t (:background ,cmd-atom-one-light-gray :weight bold))))
  `(magit-diff-file-heading-selection ((t (:foreground ,cmd-atom-one-light-orange-2 :background ,cmd-atom-one-light-bg-hl :weight bold))))
  `(magit-diff-hunk-heading ((t (:foreground ,cmd-atom-one-light-mono-2 :background ,cmd-atom-one-light-gray))))
  `(magit-diff-hunk-heading-highlight ((t (:foreground ,cmd-atom-one-light-mono-1 :background ,cmd-atom-one-light-mono-3))))
  `(magit-diff-hunk-heading-selection ((t (:foreground ,cmd-atom-one-light-purple :background ,cmd-atom-one-light-mono-3))))
  `(magit-diff-context ((t (:foreground ,cmd-atom-one-light-fg))))
  `(magit-diff-context-highlight ((t (:background ,cmd-atom-one-light-bg-1 :foreground ,cmd-atom-one-light-fg))))
  `(magit-diffstat-added ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-diffstat-removed ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(magit-process-ok ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-process-ng ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(magit-log-author ((t (:foreground ,cmd-atom-one-light-orange-2))))
  `(magit-log-date ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(magit-log-graph ((t (:foreground ,cmd-atom-one-light-silver))))
  `(magit-sequence-pick ((t (:foreground ,cmd-atom-one-light-orange-2))))
  `(magit-sequence-stop ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-sequence-part ((t (:foreground ,cmd-atom-one-light-orange-1))))
  `(magit-sequence-head ((t (:foreground ,cmd-atom-one-light-blue))))
  `(magit-sequence-drop ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(magit-sequence-done ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(magit-sequence-onto ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(magit-bisect-good ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-bisect-skip ((t (:foreground ,cmd-atom-one-light-orange-1))))
  `(magit-bisect-bad ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(magit-blame-heading ((t (:background ,cmd-atom-one-light-bg-1 :foreground ,cmd-atom-one-light-mono-2))))
  `(magit-blame-hash ((t (:background ,cmd-atom-one-light-bg-1 :foreground ,cmd-atom-one-light-purple))))
  `(magit-blame-name ((t (:background ,cmd-atom-one-light-bg-1 :foreground ,cmd-atom-one-light-orange-2))))
  `(magit-blame-date ((t (:background ,cmd-atom-one-light-bg-1 :foreground ,cmd-atom-one-light-mono-3))))
  `(magit-blame-summary ((t (:background ,cmd-atom-one-light-bg-1 :foreground ,cmd-atom-one-light-mono-2))))
  `(magit-dimmed ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(magit-hash ((t (:foreground ,cmd-atom-one-light-purple))))
  `(magit-tag  ((t (:foreground ,cmd-atom-one-light-orange-1 :weight bold))))
  `(magit-branch-remote  ((t (:foreground ,cmd-atom-one-light-green :weight bold))))
  `(magit-branch-local   ((t (:foreground ,cmd-atom-one-light-blue :weight bold))))
  `(magit-branch-current ((t (:foreground ,cmd-atom-one-light-blue :weight bold :box t))))
  `(magit-head           ((t (:foreground ,cmd-atom-one-light-blue :weight bold))))
  `(magit-refname        ((t (:background ,cmd-atom-one-light-bg :foreground ,cmd-atom-one-light-fg :weight bold))))
  `(magit-refname-stash  ((t (:background ,cmd-atom-one-light-bg :foreground ,cmd-atom-one-light-fg :weight bold))))
  `(magit-refname-wip    ((t (:background ,cmd-atom-one-light-bg :foreground ,cmd-atom-one-light-fg :weight bold))))
  `(magit-signature-good      ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-signature-bad       ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(magit-signature-untrusted ((t (:foreground ,cmd-atom-one-light-orange-1))))
  `(magit-cherry-unmatched    ((t (:foreground ,cmd-atom-one-light-cyan))))
  `(magit-cherry-equivalent   ((t (:foreground ,cmd-atom-one-light-purple))))
  `(magit-reflog-commit       ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-reflog-amend        ((t (:foreground ,cmd-atom-one-light-purple))))
  `(magit-reflog-merge        ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-reflog-checkout     ((t (:foreground ,cmd-atom-one-light-blue))))
  `(magit-reflog-reset        ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(magit-reflog-rebase       ((t (:foreground ,cmd-atom-one-light-purple))))
  `(magit-reflog-cherry-pick  ((t (:foreground ,cmd-atom-one-light-green))))
  `(magit-reflog-remote       ((t (:foreground ,cmd-atom-one-light-cyan))))
  `(magit-reflog-other        ((t (:foreground ,cmd-atom-one-light-cyan))))

  ;; rainbow-delimiters
  `(rainbow-delimiters-depth-1-face ((t (:foreground ,cmd-atom-one-light-fg))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground ,cmd-atom-one-light-purple))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground ,cmd-atom-one-light-blue))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground ,cmd-atom-one-light-cyan))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground ,cmd-atom-one-light-green))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground ,cmd-atom-one-light-orange-1))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground ,cmd-atom-one-light-orange-2))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground ,cmd-atom-one-light-red-1))))
  `(rainbow-delimiters-depth-9-face ((t (:foreground ,cmd-atom-one-light-red-2))))
  `(rainbow-delimiters-depth-10-face ((t (:foreground ,cmd-atom-one-light-mono-1))))
  `(rainbow-delimiters-depth-11-face ((t (:foreground ,cmd-atom-one-light-mono-2))))
  `(rainbow-delimiters-depth-12-face ((t (:foreground ,cmd-atom-one-light-mono-3))))
  `(rainbow-delimiters-unmatched-face ((t (:foreground ,cmd-atom-one-light-black))))

  ;; rbenv
  `(rbenv-active-ruby-face ((t (:foreground ,cmd-atom-one-light-green))))

  ;; smartparens
  `(sp-show-pair-mismatch-face ((t (:foreground ,cmd-atom-one-light-red-1 :background ,cmd-atom-one-light-gray :weight bold))))
  `(sp-show-pair-match-face ((t (:background ,cmd-atom-one-light-gray :weight bold))))

  ;; web-mode
  `(web-mode-symbol-face ((t (:foreground ,cmd-atom-one-light-orange-1))))

  ;; flx-ido
  '(flx-highlight-face ((t (:inherit (link) :weight bold))))
  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'cmd-atom-one-light)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; cmd-atom-one-light-theme.el ends here
