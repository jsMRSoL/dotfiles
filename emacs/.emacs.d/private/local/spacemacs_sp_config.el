;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;--------------------------------------------------------------------------
;; macros configuration
;;--------------------------------------------------------------------------
  (fset 'insert-line-and-paste-clipboard
        [?O escape ?m ?A ?\" ?* ?P ?0 ?\' ?A])
;;--------------------------------------------------------------------------
;; fuzzy find-file configuration
;;--------------------------------------------------------------------------
  (load-file "/home/simon/.emacs.d/private/local/helm-fzf/helm-fzf.el")
;;--------------------------------------------------------------------------
;; keybindings configuration
;;--------------------------------------------------------------------------
  (spacemacs/set-leader-keys "X" 'org-capture)
  (spacemacs/set-leader-keys ":" 'eval-expression)
  (spacemacs/set-leader-keys "rp" 'insert-line-and-paste-clipboard)
  (spacemacs/set-leader-keys "fz" 'helm-fzf)
  ;; map avy-timer to s in normal mode
  (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-timer)
;;--------------------------------------------------------------------------
;; theme configuration
;;--------------------------------------------------------------------------
  (require 'doom-themes)

;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
;;--------------------------------------------------------------------------
;; interface configuration
;;--------------------------------------------------------------------------
  ;; fix the impossibly fast esc shortcut
  (setq-default evil-escape-delay 0.2)
  ;; use cool abbreviations in code
  (global-prettify-symbols-mode t)
;;--------------------------------------------------------------------------
;; hard wrapping configuration
;;--------------------------------------------------------------------------
  (defun sp/text-mode-config ()
    (set-fill-column 80)
    (auto-fill-mode 1)
    (spacemacs/toggle-fill-column-indicator-on)
    )
  (add-hook 'text-mode-hook #'sp/text-mode-config)
;;--------------------------------------------------------------------------
;; version-control configuration
;;--------------------------------------------------------------------------
  ;; always follows symlinks
  (setq vc-follow-symlinks t)
;;--------------------------------------------------------------------------
;; orgmode configuration
;;--------------------------------------------------------------------------
  ;; cosmetics
  (setq org-ellipsis " ▾")
  ;; logging
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; agenda files
  (setq org-agenda-files
        '("~/Documents/org/tasks.org"
          "~/Documents/org/ideas.org"
          "~/Documents/org/journal.org"
          ))
  ;; todos
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "PROJ(p!)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  ;; refile settings
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 1))
          ("journal.org" . (:maxlevel . 3))
          ("archive.org" . (:maxlevel . 1))))
  ;; save org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; tags
  (setq org-tag-alist
        '((:startgroup)
          ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?e)
          ("@home" . ?h)
          ("@garage" . ?g)
          ("@work" . ?w)
          ("@family" . ?f)
          ("@note" . ?n)
          ("@fun" . ?F)
          ("@urgent" . ?u)
          ("@computing" . ?c)
          ("@idea" . ?i)))
  ;; custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  ;; templates
  ;; key can be found here: https://orgmode.org/manual/Template-expansion.html#Template-expansion
  ;; clocking and other properties here: https://orgmode.org/manual/Template-elements.html#Template-elements
  (setq org-capture-templates
        '(("t" "Tasks / Projects / Appointments")
          ("tt" "Task" entry (file+olp "~/Documents/org/tasks.org" "To organise")
           "* TODO  %^{Title}\n  :LOGBOOK:\n  - Created: %U\n   :END:\n  :SUBTASKS:\n  - [ ]  %?\n  :END:\n  %a\n  %i" :empty-lines 1)
          ("ta" "Appointment" entry
           (file+olp+datetree "~/Documents/org/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Documents/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n**  %?\n\n"
           ;; :clock-in :clock-resume
           :empty-lines 1)

          ("b" "Book log")
          ("br" "Read" entry (file+headline "~/Documents/org/Books.org" "2021")
           ;; "| %^{Title} | %^{Author} | %^{Pages} | %^{Started} |  |  | %^{Notes} |" :kill-buffer t)
           "* %^{Title}\n:PROPERTIES:\n:Title: %\\1\n:Author: %^{Author}\n:Pages: ?\n:Started: %U\n:Finished: ?\n:Sessions: ?\n:Notes: %^{Notes} %?\n:END:"
           :kill-buffer t)
          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Documents/org/metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
          ("mg" "Guitar" table-line (file+headline "~/Documents/org/metrics.org" "Guitar")
           "| %U | %^{Time spent (m)} | %^{Notes} |" :kill-buffer t)
          ("mp" "Piano" table-line (file+headline "~/Documents/org/metrics.org" "Piano")
           "| %U | %^{Time spent (m)} | %^{Notes} |" :kill-buffer t)
          ("mr" "Reading" table-line (file+headline "~/Documents/org/metrics.org" "Reading")
          "| %U | %^{Book} | %^{Time spent (m)} | %^{Notes} |" :kill-buffer t)))
;;--------------------------------------------------------------------------
;; python tweaks
;;--------------------------------------------------------------------------
  (add-hook 'python-mode-hook
	          (lambda ()
		          (setq-default tab-width 4)))
;;--------------------------------------------------------------------------
;; treemacs tweaks
;;--------------------------------------------------------------------------
  (setq treemacs-show-hidden-files nil)
;;--------------------------------------------------------------------------
;; terminal tweaks
;;--------------------------------------------------------------------------
  (setq terminal-here-terminal-command '("st"))
;;--------------------------------------------------------------------------
;; directory navigation
;;--------------------------------------------------------------------------
  (defvar sp-common-dirs
    '((?h . "/home/simon/")
      (?d . "/home/simon/Documents/")
      (?o . "/home/simon/Downloads/")
      (?r . "/home/simon/Documents/org/")
      (?f . "/home/simon/.dotfiles/")
      (?e . "/home/simon/.emacs.d/")
      (?c . "/home/simon/.config/")
      (?b . "/home/simon/.local/usr/bin/")
      (?j . "/home/simon/Projects")
      (?y . "/home/simon/Projects/python/"))
    "An alist of common-dirs to facilitate quick navigation."
    )

  (defun sp/dired-jump-dir(char)
    "Jump to a directory in my common directories list."
    (interactive "c[h]ome, [d]ocs, d[o]wnloads, [e]macs, o[r]g, dot[f]iles, [c]onfig, .[b]in, pro[j]ects, p[y]thon")
    (dired-jump nil (alist-get char sp-common-dirs))
    )

  (spacemacs/set-leader-keys "oj" #'sp/dired-jump-dir)
;;--------------------------------------------------------------------------
;; trash-cli integration
;;--------------------------------------------------------------------------
(load-file "/home/simon/.emacs.d/private/local/trash-settings.el")
  (setq system-trash-exclude-matches '("#[^/]+#$" ".*~$" "\\.emacs\\.desktop.*"))
  (setq system-trash-exclude-paths '("/tmp"))
;;--------------------------------------------------------------------------
;; end of config
;;--------------------------------------------------------------------------
