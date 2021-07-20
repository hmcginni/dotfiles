;;; hrm-lisp-functions.el -- Emacs Init File - custom lisp functions

;;; Commentary:
;;    Custom Elisp Functions.

;;; Code:


;; ─────────────────────────────────────────────────────────
;; Variables

(defvar hrm/global-is-light-theme t
  "Whether the LIGHT theme is enabled.")

(defcustom hrm/initial-org-scratch-message nil
  "Initial message displayed in Org-scratch buffers."
  :group 'hrm
  :type '(sexp))

(defcustom hrm/weekly-org-notes-dir nil
  "Location of weekly org-notes files."
  :group 'hrm
  :type '(directory))


;; ─────────────────────────────────────────────────────────
;; General Utility Functions

(defun hrm/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))


(defun hrm/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(defun hrm/xor (a b)
  "XOR of A and B."
  (and (not (and a b))
       (or a b)))


(defun hrm/switch-to-previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defun hrm/reload-emacs-init-file ()
  "Reload Emacs."
  (interactive)
  (load-file "~/.emacs.d/init.el")
  (if (derived-mode-p 'org-mode)
	  (org-mode-restart)))


(defun hrm/scratch ()
  "Go to the *scratch* buffer."
  (interactive)
  (let ((scratch "*scratch*"))
	(if (get-buffer scratch)
		(switch-to-buffer scratch)
	  (switch-to-buffer scratch)
	  (with-current-buffer scratch
		(lisp-interaction-mode))
	  (insert initial-scratch-message))))


(defun hrm/org-scratch ()
  "Go to the Org-scratch buffer."
  (interactive)
  (hrm/org/update-scratch-message)
  (let ((org-scratch "scratch.org"))
    (if (get-buffer org-scratch)
        (switch-to-buffer org-scratch)
      (switch-to-buffer org-scratch)
	  (with-current-buffer org-scratch
		(org-mode)
		(setq-local desktop-save-buffer 't))
      (insert hrm/initial-org-scratch-message))))

(defun hrm/org/update-scratch-message ()
  "Create the initial text for a new Org-mode scratch buffer."
  (interactive)
  (custom-set-variables
   '(hrm/initial-org-scratch-message
	 (format
	  "#+options: toc:nil num:nil \\n:nil ::t -:t
#+html_head: <link rel=\"stylesheet\" href=\"/home/mcginh2/org/org.css\" />
#+title:
#+author: %s
#+date: %s

" user-full-name
(string-trim-right
 (shell-command-to-string "date -d \"monday this week\"  +%Y%m%d"))))))

(defun hrm/open-weekly-org-notes-buffer ()
  "Go to the weekly notes Org-mode buffer."
  (interactive)
  (let* ((tomorrow (org-time-string-to-time (org-read-date nil nil "+1")))
		 (mon (org-read-date nil nil "--mon" nil tomorrow))
		 (today (format-time-string "%Y%m%d"))
		 (notes-filename (concat mon "-notes.org"))
		 (notes-file (concat hrm/weekly-org-notes-dir notes-filename)))
	(message "%s" mon)
    (if (get-buffer notes-filename)
        (switch-to-buffer notes-filename)
      (find-file notes-file)
	  (with-current-buffer notes-filename
		(org-mode)
		(setq-local desktop-save-buffer 't))
      (insert hrm/initial-org-scratch-message))))


(defun hrm/count-thing-at-point ()
  "Count the number of instances of `thing-at-point' in this buffer."
  (interactive)
  (let* ((tap (or (thing-at-point 'symbol 'no-properties) ""))
		 (matches (count-matches tap (point-min) (point-max))))
	(if (string= tap "")
		(message "No symbol at point.")
	  (message "%d matches for \"%s\" in this buffer" matches tap))))


(defun hrm/occur-thing-at-point ()
  "Run `occur' on `thing-at-point'."
  (interactive)
  (let* ((tap (or (thing-at-point 'symbol 'no-properties) ""))
		 (matches (count-matches tap (point-min) (point-max))))
	(if (string= tap "")
		(message "No symbol at point.")
	  (occur tap)
	  (other-window 1))))


;; ─────────────────────────────────────────────────────────
;; Editing Functions

(defun hrm/new-comment-section (&optional char)
  "Insert a comment section.  Optionally specify CHAR as the fill character."
  (interactive)
  (or char
	  (setq char "─"))
  (beginning-of-line)
  (open-line 1)
  (insert-char (string-to-char char))
  (let ((beg (line-beginning-position))
		(end (line-end-position))
		num-cols-to-fill-col)
	(comment-region beg end)
	(setq num-cols-to-fill-col (- fill-column 20 (current-column)))
	(insert-char (string-to-char char) num-cols-to-fill-col)
	(forward-char)))


(defun hrm/inline-code (lang)
  "Insert inline LANG snippet for Org mode."
  (interactive)
  (if (not (string-equal major-mode "org-mode"))
	  (message "Invalid action: must be in org-mode")
	(insert (format "src_%s[:exports code]{}" lang))
	(backward-char 1)))


(defun hrm/toggle-comment-region ()
  "Comment or uncomment a region/line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)))


(defun hrm/insert-date ()
  "Print date in 'yyyymmdd' form."
  (interactive)
  (insert (shell-command-to-string "printf '%s' $(date +%Y%m%d)")))


(defun hrm/insert-monday-date ()
  "Print date of last Monday in 'yyyymmdd' form."
  (interactive)
  (insert (shell-command-to-string
		   "printf '%s' $(date -dlast-monday +%Y%m%d)")))


(defun hrm/insert-date-time ()
  "Print timestamp in 'yyyymmdd_HHMMSS' form."
  (interactive)
  (if (derived-mode-p 'org-mode)
	  (org-time-stamp '(16) t)
	(insert (shell-command-to-string "printf '%s' $(date +%Y%m%d_%H%M%S)"))))


(defun hrm/file-name-on-clipboard ()
  "Copy current buffer to clipboard."
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region
         (point-min)
         (point-max)))
      (message filename))))


(defun hrm/next-comment-section ()
  "Navigate to the next comment section."
  (interactive)
  (forward-line)
  (search-forward "─\n" nil nil 1)
  (forward-line -1)
  (beginning-of-line))


(defun hrm/previous-comment-section ()
  "Navigate to the previous comment section."
  (interactive)
  (search-forward "─\n" nil nil -1)
  (beginning-of-line))


;; ─────────────────────────────────────────────────────────
;; Appearance

(defun hrm/post-theme-customizations ()
  "Fix font weirdness."
  (set-face-attribute
   'mode-line nil
   :font "Tex Gyre Heros:pixelsize=10" )
  (set-face-attribute
   'mode-line-inactive nil
   :font "Tex Gyre Heros:pixelsize=10:slant=italic" ))


(defun hrm/light-theme ()
  "Apply a light GUI theme."
  (setq hrm/global-is-light-theme t)
  (load-theme 'atom-one-light t))
  ;; (if (display-graphic-p)
  ;; 	  (load-theme 'atom-one-light t)
  ;;   (load-theme 'cmd-atom-one-light t)))


(defun hrm/dark-theme ()
  "Apply a dark GUI theme."
  (setq hrm/global-is-light-theme nil)
  (load-theme 'atom-one-dark t))
  ;; (if (display-graphic-p)
  ;;     (load-theme 'atom-one-dark t)
  ;;   (load-theme 'cmd-atom-one-dark t)))


(defun hrm/set-theme (light)
  "Customize Emacs theme; use a LIGHT color theme if `t` and dark if `nil`."
  (if light
      (hrm/light-theme)
    (hrm/dark-theme))
  (hrm/post-theme-customizations))


(defun hrm/toggle-theme ()
  "Switch between light and dark themes."
  (interactive)
  (setq hrm/global-is-light-theme (hrm/xor hrm/global-is-light-theme t))
  (hrm/set-theme hrm/global-is-light-theme))


(defun hrm/resize (size)
  "Resize Emacs to specified SIZE."
  (interactive)
  (let ((width (cond ((string= size "narrow") 674)
					 ((string= size "half") 898)
					 ((string= size "wide") 1143))))
	(set-frame-width (selected-frame) width nil t)))


(defun hrm/dpi/get-dpi ()
  "Get the DPI of the display."
  (interactive)
  (let* ((attrs (car (display-monitor-attributes-list)))
         (display-size (cdr (assoc 'mm-size attrs)))
		 (resolution (cdddr (assoc 'geometry attrs)))
         (display-width (/ (car display-size)
                           25.4))
         (x-resolution (car resolution))
         (dpi (/ x-resolution
                 display-width)))
    (message "DPI: %f" dpi)
    dpi))


(defun hrm/dpi/determine-font-scaling ()
  "Pick a font size based on the DPI."
  (let ((dpi (hrm/dpi/get-dpi)))
    (cond ((< dpi 135) 12)  ;; dpi=96 => 12-13/14
          ((< dpi 145) 14)  ;; dpi=140 => 14/16
          ((< dpi 155) 16)  ;; [145, 155) must be ?14/16?
          ((< dpi 165) 17)  ;; ?15/17?
          (t 18))))         ;; ?16?


(defun hrm/dpi/scale-font (face weight)
  "Set the scaled font specification string for FACE and WEIGHT."
  (let* ((size (hrm/dpi/determine-font-scaling))
         (font (format "%s:size=%d:weight=%s" face size weight)))
    (set-frame-font font)))


;; ─────────────────────────────────────────────────────────
;; Narrowing

(defun hrm/narrow-to-defun-indirect ()
  "Create a new indirect buffer narrowed to the current function."
  (interactive)
  (let ((new-buffer-name (generate-new-buffer-name (buffer-name)))
        (current-buffer-name (buffer-name)))
    (make-indirect-buffer current-buffer-name
                          new-buffer-name
                          t)
    (switch-to-buffer new-buffer-name)
    (narrow-to-defun)))


(defun hrm/narrow-to-eof ()
  "Narrow from (point) to end-of-file."
  (interactive)
  (save-excursion
    (narrow-to-region (- (point)
                         (current-column))
                      (point-max))))


;; ─────────────────────────────────────────────────────────
(provide 'hrm-lisp-functions)
;;; hrm-lisp-functions.el ends here
