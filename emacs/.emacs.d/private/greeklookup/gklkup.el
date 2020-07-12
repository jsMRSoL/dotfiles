(defun glex-get-lemmas()
  "Look up Greek word under cursor and display lemmas
in a vertical split."
  (interactive)
  (setq greektext-bfr (buffer-name)
        greek-word (current-word t t))
  (if (get-buffer-window "*Greek Lemmas*")
      (select-window (get-buffer-window "*Greek Lemmas*"))
    (progn (split-window nil 40 'left)
           (switch-to-buffer (get-buffer-create "*Greek Lemmas*"))))
  (erase-buffer)
  (insert (shell-command-to-string
           (concat "getGreekLemmas2.py"
                   " "
                   greek-word)))
  (beginning-of-buffer))

(defun glex-get-defs()
  "Jump from lemma link to fuller definition"
  (interactive)
  (setq greeklemmas-bfr (buffer-name)
        greek-link (replace-regexp-in-string "^Link: " ""
                      (buffer-substring
                       (line-beginning-position) (line-end-position))))
  (switch-to-buffer (get-buffer-create "*Greek Definitions*"))
  (erase-buffer)
  (insert (shell-command-to-string
           (concat "getGreekDefinitions.py"
                   " "
                   (shell-quote-argument greek-link))))
  (beginning-of-buffer))

(defun glex-send-to-list()
  "Add approved meaning to vocab list window"
  (interactive)
  (setq greek-and-def (buffer-substring
                       (line-beginning-position) (line-end-position)))
  (if (get-buffer-window "Vocabulary List")
      (select-window (get-buffer-window "Vocabulary List"))
    (progn (split-window-below-and-focus)
           (switch-to-buffer (get-buffer-create "Vocabulary List"))))
  (insert greek-and-def)
  (newline)
  (select-window (get-buffer-window greektext-bfr)))
