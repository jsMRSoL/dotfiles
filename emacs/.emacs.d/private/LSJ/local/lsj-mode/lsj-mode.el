;; lsj-mode.el
;; Minor mode for preparing set texts.
;; Main use is to enable buffer-specific keymaps.

(defun lsj-get-defs()
  "Look up Greek word under cursor and display lemmas
in a vertical split."
  (interactive)
  (setq lsj-text-bfr (buffer-name)
        greek-word (current-word t t))
  (if (get-buffer-window "*Definitions*")
      (select-window (get-buffer-window "*Definitions*"))
    (progn (split-window nil 50 'left)
           (switch-to-buffer (get-buffer-create "*Definitions*"))))
  (erase-buffer)
  (insert (shell-command-to-string
           (concat "/home/simon/Projects/python/greeklexicon/query_LSJ.py"
                   " "
                   greek-word)))
  (beginning-of-buffer)
  (when (re-search-forward "^# S" nil t)
    (forward-line 1))
  (unless lsj-mode
    (lsj-mode t)))


(defun lsj-send-to-list()
  "Add approved meaning to vocab list window"
  (interactive)
  (when (string= (buffer-name) "*Definitions*")
    (lsj--send-to-list)))

(defun lsj--send-to-list()
  (setq lsj-def (buffer-substring
                       (line-beginning-position) (line-end-position)))
  ;; (goto-char (point-min))
  (re-search-backward "^~~~~")
  (forward-line 1)
  (setq lsj-hd-wd (buffer-substring
                       (line-beginning-position) (line-end-position)))
  (if (get-buffer-window "Vocabulary List")
      (select-window (get-buffer-window "Vocabulary List"))
    (progn (split-window-below-and-focus)
           (switch-to-buffer (get-buffer-create "Vocabulary List"))))
  (insert (concat lsj-hd-wd
                  " : "
                  lsj-def))
  (newline)
  (select-window (get-buffer-window lsj-text-bfr)))


(defun lsj-find-definition(n)
  "Look up definition from numbered list in side window."
  (interactive "nWhich definition? ")
  (select-window (get-buffer-window "*Definitions*"))
  (goto-line n)
  (setq lsj-search-term (buffer-substring
                         (line-beginning-position) (line-end-position)))
  (setq lsj-search-term (nth 1 (split-string lsj-search-term "\'")))
  (erase-buffer)
  (insert (shell-command-to-string
           (concat "/home/simon/Projects/python/greeklexicon/find_from_transliterated.py"
                   " "
                   (shell-quote-argument lsj-search-term))))
  (beginning-of-buffer)
  (re-search-forward "^# S")
  (forward-line 1)
  (unless lsj-mode
    (lsj-mode t))
  (shell-command
   (concat "/home/simon/Projects/python/greeklexicon/record_head_id.py"
                 " "
                 (shell-quote-argument lsj-search-term)))
  )

(defun lsj-remove-trailing-commas()
  (interactive)
  "Remove trailing commas from the Vocabulary List window."
  (save-window-excursion
  (select-window (get-buffer-window "Vocabulary List"))
  (save-excursion
  (goto-char (point-min))
  (while (re-search-forward ",$" (point-max) t)
    (let ((b (- (point) 1))
          (e (line-end-position)))
         (delete-region b e))))))

(define-minor-mode lsj-mode
  "A temporary minor mode to be activated specific to a buffer."
  :lighter "LSJ")

(provide 'lsj-mode)
