;; lsj2-mode.el
;; Minor mode for preparing Greek set texts.
;; Main use is to enable buffer-specific keymaps.
(defvar lsj-windows-setup nil
  "Whether lsj-set-up-windows has been called yet.")


(defcustom lsj2-mode-key-prefix (kbd "C-c g")
  "The prefix key for `lsj2' mode commands.")


(defvar lsj2-mode-keymap
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "d") 'lsj-get-defs)
      (define-key prefix-map (kbd "a") 'lsj-send-to-list)
      (define-key prefix-map (kbd "c") 'lsj-remove-trailing-commas)
      (define-key prefix-map (kbd "n") 'lsj-next-entry)
      (define-key prefix-map (kbd "f") 'lsj-find-definition)
      (define-key map lsj2-mode-key-prefix prefix-map)
      map))
  "Keymap for lsj2")


(defun lsj-get-defs()
  "Look up Greek word under cursor and display lemmas
in a vertical split."
  (interactive)
  (unless lsj-windows-setup (lsj-set-up-windows))
  (setq greek-word (current-word t t))
  (lsj--react-to-output (shell-command-to-string
  ;; (lsj--filter-and-display-xml "test" (shell-command-to-string
           (concat "/home/simon/Projects/rust/diesel-glex/target/release/run_query"
                   " "
                   greek-word)))
  (select-window (get-buffer-window "*Definitions*"))
  (beginning-of-buffer)
  (when (re-search-forward "^# S" nil t)
    (forward-line 1)))


(defun lsj-send-to-list()
  "Add approved meaning to vocab list window"
  (interactive)
  (with-current-buffer (get-buffer "*Definitions*")
    (setq lsj-def (buffer-substring
                   (line-beginning-position) (line-end-position)))
    (re-search-backward "^~~~~")
    (forward-line 1)
    (setq lsj-hd-wd (buffer-substring
                     (line-beginning-position) (line-end-position))))
  (with-current-buffer (get-buffer "Vocabulary List")
    (goto-char (point-max))
    (newline)
    (insert (concat lsj-hd-wd
                    " : "
                    lsj-def)))
  (select-window (get-buffer-window lsj-text-bfr)))


(defun lsj-find-definition(n)
  "Look up definition from numbered list in side window."
  (interactive "nWhich definition? ")
  (with-current-buffer (get-buffer "*Definitions*")
    (goto-line (+ 1 n))
    (setq lsj-search-term (buffer-substring
			   (line-beginning-position) (line-end-position)))
    (setq lsj-last-id-lemmata-pair (nth 1 (split-string lsj-search-term ">" t " ")))
    (setq lsj-search-term (nth 0 (split-string lsj-last-id-lemmata-pair)))
    (lsj--react-to-output (shell-command-to-string
			   (concat "/home/simon/Projects/rust/diesel-glex/target/release/query_with_id"
				   " "
				   lsj-search-term)))
    (select-window (get-buffer-window "*Definitions*"))
    (beginning-of-buffer)
    (when (re-search-forward "^# S" nil t)
      (forward-line 1))))


(defun lsj-remove-trailing-commas()
  (interactive)
  "Remove trailing commas from the Vocabulary List window."
  (with-current-buffer "Vocabulary List"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ",$" (point-max) t)
        (let ((b (- (point) 1))
              (e (line-end-position)))
          (delete-region b e))))))


(defun lsj-next-entry()
  "Jump to next definition entry."
  (interactive)
  (re-search-forward "^# S")
  (forward-line 1))


(defun lsj--filter-and-display-xml (short-def xml)
  "Display TEI data from LSJ."
  ;; (interactive)
  (let* (
	 (root (with-temp-buffer
		 (insert xml)
		 (xml-parse-region (point-min) (point-max))))
	 (div2 (car root))
	 (attrs (xml-node-attributes div2))
	 (key (cdr (assq 'key attrs)))
	 (node (car (xml-get-children div2 'head)))
	 (head (car (xml-node-children node)))
	 (senses (xml-get-children div2 'sense))
	 (count 0)
	 (output "")
	 )
    (dolist (sense senses)
      (setq count (+ 1 count))
      (setq i-node (car (xml-get-children sense 'i)))
      (setq i-text (car (xml-node-children i-node)))
      (setq bibl-nodes (xml-get-children sense 'bibl))
      (setq output (format "%s\n# Sense %s" output count))
      (setq output (format "%s\n%s" output i-text))
      (setq output (lsj--recurse output sense 1))
      )
    (with-current-buffer (get-buffer-create "*Definitions*")
      (erase-buffer)
      (insert "~~~~~~~~~~~~~~~~~~~~~~~\n")
      (insert (format "%s\n\n# Sense (short definition)\n%s\n" head short-def))
      (insert output)))
  )


(defun lsj--recurse (output node level)
  "Check for string or node and act accordingly."
  (setq level (+ 1 level))
  (cond
   ((stringp node)
    (setq output (format "%s > %s" output node)))
   ((listp node)
    (progn
      (setq output (format "%s\n%s:%s" output (make-string level 32) (xml-node-name node)))
      (dolist (child (xml-node-children node))
	(setq output (lsj--recurse output child level))))))
  output)


(defun lsj--react-to-output (output)
  "Check first part of string and react accordingly."
  (message "lsj--react-to-output called")
  (let* ((sections (split-string output "<\\++>"))
	 (first (nth 0 sections)))
    (cond
     ((string= first "xml")
      (lsj--filter-and-display-xml (nth 1 sections) (nth 2 sections)))
     ((string= first "opt")
      (lsj--display-options (nth 2 sections)))
     )
    )
  )


(defun lsj--display-options (opts-string)
  "Display the options for the user to choose."
    (with-current-buffer (get-buffer-create "*Definitions*")
      (erase-buffer)
      (insert opts-string)))


(defun lsj-write-last-choice-db ()
  "Write the last association of lemmata no and headword id to the database
for all inflected forms with this lemmata no."
  (interactive)
  (when
      (y-or-n-p (format "Write this pair to the database: %s?" lsj-last-id-lemmata-pair))
    (progn
      (message "Writing to database.")
      (async-shell-command (concat "/home/simon/Projects/rust/diesel-glex/target/release/set_hword_id"
				   " "
				   lsj-last-id-lemmata-pair)))))


(defun lsj-set-up-windows()
  (setq lsj-text-bfr (buffer-name))
  (lsj2-mode t)
  (split-window nil 50 'left)
  (switch-to-buffer (get-buffer-create "*Definitions*"))
  (load-file "~/.simacs_dir/private/LSJ2/local/lsj2-mode/lsj2-defs-mode.el")
  (lsj2-defs-mode t)
  (evil-insert-state)
  (evil-normal-state)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer (get-buffer-create "Vocabulary List"))
  (other-window 1)
  (setq lsj-windows-setup t))

;;;###autoload
(define-minor-mode lsj2-mode
  "A temporary minor mode to be activated specific to a buffer."
  :lighter "LSJ"
  :keymap lsj2-mode-keymap
  (unless lsj2-mode
    (setq lsj-windows-setup nil)))


(provide 'lsj2-mode)
