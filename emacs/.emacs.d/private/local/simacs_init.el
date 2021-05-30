(setq inhibit-startup-message t)

(if (display-graphic-p)
    (progn
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)))
(menu-bar-mode -1)
;; how to have no bells?
(setq visual-bell t)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)

;; disable line numbers for these modes:
(dolist (mode '(org-mode-hook
                 term-mode-hook
                 shell-mode-hook
                 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
    :diminish
    :bind (
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(use-package doom-modeline
    :init (doom-modeline-mode 1))

(use-package doom-themes
    :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.5))

(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

(use-package counsel
    :bind (("M-x" . counsel-M-x)))

(defun sp/evil-hook ()
  (dolist (mode '(dashboard-mode
		  ))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump t)
    :hook (evil-mode . sp/evil-hook)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    ;; Use visual line motions
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line))


(use-package evil-escape
  :init
  (evil-escape-mode 1)
  :config
  (setq-default evil-escape-key-sequence "fd")
  (setq evil-escape-delay 0.2))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer sp/leader-keys
    :keymaps '(normal insert visual emacs dashboard-mode-map)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer sp/g-keys
    :keymaps '(normal visual)
    :prefix "g"))

(sp/leader-keys
 "1" '(winum-select-window-1 :which-key "win 1")
 "2" '(winum-select-window-2 :which-key "win 2")
 ":" '(eval-expression :which-key "M-:")
 "a" '(:ignore t :which-key "apps")
 "at" '(vterm :which-key "terminal")
 "f" '(:ignore t :which-key "files")
 "ff" '(counsel-find-file :which-key "find file")
 "fr" '(counsel-recentf :which-key "find recent")
 "fs" '(save-buffer :which-key "save")
 "g" '(:ignore t :which-key "git")
 "gs" '(magit-status :which-key "git status")
 "b" '(:ignore t :which-key "buffers")
 "bb" '(counsel-switch-buffer :which-key "switch")
 "bd" '(kill-buffer-and-window :which-key "delete")
 "SPC" '(counsel-M-x :which-key "M-x")
 "q" '(:ignore t :which-key "quit")
 "qa" '(evil-quit-all :which-key "quit all")
 "qq" '(evil-quit :which-key "quit")
 "k" '(:ignore t :which-key "lisp")
 "ke" '(eval-last-sexp :which-key "evaluate")
 "ks" '(sp-forward-sexp :which-key "fwd sexp")
 "kS" '(sp-forward-symbol :which-key "fwd symbol")
 "l" '(lsp-command-map :which-key "+lsp")
 "c" '(:ignore t :which-key "code")
 "cc" '(comment-line :which-key "comment")
 "r" '(:ignore t :which-key "registers")
 "rl" '(evil-show-registers :which-key "list")
 "rp" '(insert-line-and-paste-clipboard :which-key "list")
 "o" '(:ignore t :which-key "opt")
 "oj" '(sp/dired-jump-dir :which-key "jump dir")
 "oo" '(:ignore t :which-key "org")
 "oor" '(org-refile :which-key "org refile")
 "p" '(projectile-command-map :which-key "projectile")
 "s" '(:ignore t :which-key "search")
 "sp" '(swiper :which-key "swiper")
 "ss" '(avy-goto-char-2 :which-key "char2")
 "sl" '(avy-goto-line :which-key "line")
 "w" '(:ignore t :which-key "windows")
 "wv" '(evil-window-vsplit :which-key "vsplit")
 "ws" '(evil-window-split :which-key "split")
 "wh" '(evil-window-left :which-key "go left")
 "wj" '(evil-window-down :which-key "go down")
 "wk" '(evil-window-up :which-key "go up")
 "wl" '(evil-window-right :which-key "go right")
 "wo" '(delete-other-windows :which-key "only")
 "wd" '(sp/delete-chosen-window :which-key "delete")
 "z" '(hydra-zoom/body :which-key "zoom")
 "t" '(hydra-toggles/body :which-key "toggles")
 "TAB" '(evil-buffer :which-key "last buffer")
 "X" '(org-capture :which-key "org-capture")
 )

(sp/g-keys
 "cc" '(evilnc-comment-or-uncomment-lines :which-key "comment"))

(use-package page-break-lines)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  ;; (projects . 5)
			  ;; (agenda . 5)	;
			  ;; (registers . 5)
			  )))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  ;; :bind-keymap
  ;; ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package evil-snipe
  :defer
  :config
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-repeat-scope 'visible)
  (setq evil-snipe-spillover-scope 'visible)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package avy)

;;(require 'smartparens-config)
(use-package smartparens)
(smartparens-global-mode 1)

(use-package hydra)

(defhydra hydra-zoom (:color pink
			     :hint nil)
"
 _j_: in      _k_: out      _q_: quit
"

  ("j" text-scale-increase)
  ("k" text-scale-decrease)
  ("q" (message "Done") :exit t :color blue))

(defhydra hydra-toggles nil
  "toggles"
  ("f" auto-fill-mode "fill")
  ("t" toggle-truncate-line "truncate")
  ("q" nil "cancel"))

(use-package winum
  :defer
  :init
  (winum-mode 1))

(defun sp/delete-chosen-window()
  "Enter a window no to delete."
  (interactive)
  (let ((chosen-win (read-from-minibuffer
		     "Enter window no.: "
		     "")))
  (if (equal chosen-win "")
      (message "No window entered. Cancelling...")
    ;; (message (format "Chosen window %s" chosen-win))
      (let ((current-prefix-arg (concat "-" chosen-win)))
	(call-interactively 'winum-select-window-by-number)
	))))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))  ; or lsp

(use-package company
  :defer
  :hook (lsp-mode . company-mode)
  :bind (
	:map company-active-map
           ("C-j" . #'company-select-next)
           ("C-k" . #'company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (company--idle-delay 0.1)
  :init
  (global-company-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-nerd-commenter)

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;;--------------------------------------------------------------------------
;; macros configuration
;;--------------------------------------------------------------------------
  (fset 'insert-line-and-paste-clipboard
        [?O escape ?m ?A ?\" ?* ?P ?0 ?\' ?A])
;;--------------------------------------------------------------------------
;; fuzzy find-file configuration
;;--------------------------------------------------------------------------
  ;; (load-file "/home/simon/.emacs.d/private/local/helm-fzf/helm-fzf.el")
;;--------------------------------------------------------------------------
;; keybindings configuration
;;--------------------------------------------------------------------------
;;  (spacemacs/set-leader-keys "X" 'org-capture)
;;  (spacemacs/set-leader-keys ":" 'eval-expression)
;;  (spacemacs/set-leader-keys "rp" 'insert-line-and-paste-clipboard)
;;  (spacemacs/set-leader-keys "fz" 'helm-fzf)
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
  ;; use cool abbreviations in code
  (global-prettify-symbols-mode t)
;;--------------------------------------------------------------------------
;; hard wrapping configuration
;;--------------------------------------------------------------------------
  (defun sp/text-mode-config ()
    (set-fill-column 80)
    (auto-fill-mode 1)
    ;;(spacemacs/toggle-fill-column-indicator-on)
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
  (setq org-hide-leading-stars t)
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
;; dired tweaks
;;--------------------------------------------------------------------------
  ;; (define-key dired-mode-map (kbd "j") 'dired-next-line)
  ;; (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  ;; (define-key dired-mode-map (kbd "RET") 'dired-find-file)
(defun sp/add-dired-keys ()
  (local-set-key (kbd "<return>") 'dired-find-file)
  (local-set-key (kbd "s-<return>") 'dired-find-file-other-window))

(add-hook 'dired-mode-hook 'sp/add-dired-keys)
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

  ;;(spacemacs/set-leader-keys "oj" #'sp/dired-jump-dir)
;;--------------------------------------------------------------------------
;; trash-cli integration
;;--------------------------------------------------------------------------
(load-file "/home/simon/.emacs.d/private/local/trash-settings.el")
  (setq system-trash-exclude-matches '("#[^/]+#$" ".*~$" "\\.emacs\\.desktop.*"))
  (setq system-trash-exclude-paths '("/tmp"))
;;--------------------------------------------------------------------------
;; completion tweaks
;;--------------------------------------------------------------------------
(evil-define-key 'insert global-map (kbd "C-;") 'company-yasnippet)
;;--------------------------------------------------------------------------
;; tab bar configuration
;;--------------------------------------------------------------------------
(tab-bar-mode 1)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
;;--------------------------------------------------------------------------
;; end of config
;;--------------------------------------------------------------------------
;; Don't edit below here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(dired counsel-projectile vterm evil-nerd-commenter evil-magit magit lsp-python-ms python-mode spinner gnu-elpa-keyring-update lsp-mode company winum hydra smartparens avy evil-snipe projectile dashboard page-break-lines evil-escape counsel ivy-rich which-key rainbow-delimiters doom-themes doom-modeline use-package ivy))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 113 :width normal)))))
