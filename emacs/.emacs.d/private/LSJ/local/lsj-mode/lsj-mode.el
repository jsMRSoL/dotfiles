;; lsj-mode.el
;; Minor mode for preparing set texts.
;; Main use is to enable buffer-specific keymaps.
(defvar lsj-windows-setup nil
  "Whether lsj-set-up-windows has been called yet.")

(defun lsj-get-defs()
  "Look up Greek word under cursor and display lemmas
in a vertical split."
  (interactive)
  (unless lsj-windows-setup (lsj-set-up-windows))
  (setq greek-word (current-word t t))
  (select-window (get-buffer-window "*Definitions*"))
  (erase-buffer)
  (insert (shell-command-to-string
           (concat "/home/simon/Projects/python/greeklexicon/query_LSJ.py"
                   " "
                   greek-word)))
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
    (forward-line 1))
  (shell-command
   (concat "/home/simon/Projects/python/greeklexicon/record_head_id.py"
           " "
           (shell-quote-argument lsj-search-term))))

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
  (re-search-forward "^~~~~")
  (forward-line 1)
  (recenter 1)
  (re-search-forward "^# S")
  (forward-line 1))

(define-minor-mode lsj-mode
  "A temporary minor mode to be activated specific to a buffer."
  :lighter "LSJ")

(defun lsj-set-up-windows()
  (setq lsj-text-bfr (buffer-name))
  (split-window nil 50 'left)
  (switch-to-buffer (get-buffer-create "*Definitions*"))
  (lsj-mode t)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer (get-buffer-create "Vocabulary List"))
  (other-window 1)
  (setq lsj-windows-setup t))

(provide 'lsj-mode)
