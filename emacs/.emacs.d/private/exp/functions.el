(defun test-lines (start end)
      (interactive "r")
      (setq no-lines (count-lines start end))
      (message "This region has %s lines" no-lines)
      (setq selected-lines (buffer-substring start end))
      (setq lines-list (split-string selected-lines "\n" t "\s"))
      (message "The first line is %s" (car lines-list)))


(defun marry-selected-lines (start end)
  (interactive "r")
  (setq no-lines (/ (count-lines start end) 2))
  (message "Replacing with %s lines" no-lines)
  (setq selected-lines (buffer-substring start end)
        lines-list (split-string selected-lines "\n" t "\s"))
  (delete-region start end)
  (goto-char start)
  (setq x 0)
  (while (< x no-lines)
         (insert (format "%s %s\n" (nth x lines-list) (nth (+ x no-lines) lines-list)))
         (setq x (+ 1 x))))

1 2
3 4
5 6

1
3
5
2
4
6
