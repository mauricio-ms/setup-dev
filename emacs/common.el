;; -*- lexical-binding: t; -*-
(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room
(menu-bar-mode -1)      ; Disable the menu bar

(recentf-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Set up the visible bell
(setq visible-bell t)

;; Config ibuffer-formats to leverage from larger monitors on commands like counsel-ibuffer
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 60 60 :left :elide) ;; change: 60s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(load-library "~/development/setup-dev/emacs/utils.el")

;; Let the desktop background show through
;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 130)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 140)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.0)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Teaching emacsto keep your folders clean
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'cl-lib)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some nodes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		treemacs-mode-hook
		sql-interactive-mode-hook
		graphql-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package load-relative)

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
 "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-update-project-type
   'gradle
   :test-suffix "Test"
   :src-dir "src/main/"
   :test-dir "src/test/")
  (projectile-update-project-type
   'gradlew
   :test-suffix "Test"
   :src-dir "src/main/"
   :test-dir "src/test/")
  (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-create-missing-test-files t)
  (projectile-sort-order 'recentf)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/development")
    (setq projectile-project-search-path '("~/development")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge) ;; TODO The GitHub authentication needs to be configure, check at magit documentation page

;; org roam
(use-package org-roam
  :demand t
  :custom
  (org-roam-directory (file-truename "~/development/notebook/roam-notes"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . setup-dev/org-roam-find-personal)
		 ("C-c n b" . setup-dev/org-roam-capture-inbox)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n I" . org-roam-node-insert-immediate)
		 ("C-c n p" . setup-dev/org-roam-find-project)
		 ("C-c n w" . setup-dev/org-roam-find-work)
		 ("C-c n t" . setup-dev/org-roam-capture-task)
		 :map org-mode-map
		 ("C-M-i" . completion-at-point)
		 :map org-roam-dailies-map
		 ("Y" . org-roam-dailies-capture-yesterday)
		 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-setup))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun setup-dev/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun setup-dev/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
         (seq-filter
           (setup-dev/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun setup-dev/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (setup-dev/org-roam-list-notes-by-tag setup-dev-project-type)))

;; Build the agenda list the first time for the session
(setup-dev/org-roam-refresh-agenda-list)

(defun setup-dev/org-roam-find-personal ()
  (interactive)
  ;; Select a personal note file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (setup-dev/org-roam-filter-by-tag "Personal")
   nil
   :templates
   `(("p" "work" plain ""
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ,(concat "#+title: ${title}\n#+category: ${title}\n#+filetags: Personal"))
      :unnarrowed t))))

(defun setup-dev/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'setup-dev/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun setup-dev/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'setup-dev/org-roam-project-finalize-hook)

  (let ((tag (concat "Project" setup-dev-project-type))
		(org-roam-directory (setup-dev/org-roam-get-directory)))
   ;; Select a project file to open, creating it if necessary
   (org-roam-node-find
	nil
	nil
	(setup-dev/org-roam-filter-by-tag tag)
	nil
	:templates
	`(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ,(concat "#+title: ${title}\n#+category: ${title}\n#+filetags: " tag))
       :unnarrowed t)))))

(defun setup-dev/org-roam-find-work ()
  (interactive)
  ;; Select an work note file to open, creating it if necessary
  (let ((org-roam-directory (setup-dev/org-roam-get-directory-work-notes)))
	(org-roam-node-find
	 nil
	 nil
	 (setup-dev/org-roam-filter-by-tag setup-dev-project-type)
	 nil
	 :templates
	 `(("p" "work" plain ""
		:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ,(concat "#+title: ${title}\n#+category: ${title}\n#+filetags: " setup-dev-project-type))
		:unnarrowed t)))))

(defun setup-dev/org-roam-capture-inbox ()
  (interactive)
  (let ((org-roam-directory (setup-dev/org-roam-get-directory-work-notes)))
	(org-roam-capture- :node (org-roam-node-create)
                       :templates `(("i" "inbox" plain "* %?"
									 :if-new (file+head "Inbox.org" ,(concat "#+title: Inbox\n#+filetags: " setup-dev-project-type)))))))

(defun setup-dev/org-roam-get-directory ()
  (if (equal setup-dev-project-type "Personal")
	  org-roam-directory
	(setup-dev/org-roam-get-directory-work-notes)))

(defun setup-dev/org-roam-get-directory-work-notes ()
  (file-truename
   (concat "~/development/notebook/roam-notes/"
		   (s-downcase setup-dev-project-type))))

(defun setup-dev/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'setup-dev/org-roam-project-finalize-hook)

  (let ((tag (concat "Project" setup-dev-project-type))
		(org-roam-directory (setup-dev/org-roam-get-directory)))
	;; Capture the new task, creating the project file if necessary
	(org-roam-capture- :node (org-roam-node-read
                              nil
                              (setup-dev/org-roam-filter-by-tag tag))
                       :templates `(("p" "project" plain "** TODO %?"
									 :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
															,(concat "#+title: ${title}\n#+category: ${title}\n#+filetags: " tag)
															("Tasks")))))))

(defun setup-dev/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (setup-dev/org-roam-copy-todo-to-today))))

;; org mode
;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

(defun setup-dev/org-font-setup ()
  ;; Hide emphasis markers on formatted text
  (setq org-hide-emphasis-markers t)

  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

(defun setup-dev/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . setup-dev/org-mode-setup)
  :config
  (setq org-image-actual-width nil)
  
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
    
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   `(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("b" "Backlog"
     ((todo "BACKLOG"
        ((org-agenda-overriding-header "Backlog")))))

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

	,org-agenda-workflow-command))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/development/org-files/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/development/org-files/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/development/org-files/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/development/org-files/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/development/org-files/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))
  
  (setup-dev/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun setup-dev/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
		visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . setup-dev/org-mode-visual-fill))

;; org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)))
(setq org-confirm-babel-evaluate nil)

;; org mode presentation
(defun setup-dev/org-present-start ()
  ;; Tweak font sizes 
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
									 (header-line (:height 4.0) variable-pitch)
									 (org-document-title (:height 1.75) org-document-title)
									 (org-code (:height 1.55) org-code)
									 (org-verbatim (:height 1.55) org-verbatim)
									 (org-block (:height 1.25) org-block)
									 (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " "))

(defun setup-dev/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun setup-dev/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  (setq header-line-format nil))

(use-package org-present)

;; Register hooks
(add-hook 'org-present-mode-hook 'setup-dev/org-present-start)
(add-hook 'org-present-mode-quit-hook 'setup-dev/org-present-end)
(add-hook 'org-present-after-navigate-functions 'setup-dev/org-present-prepare-slide)

;; set templates for org babel
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))

;; Automatically tangle my README.org files when them are saved
(defun efs/org-babel-tangle-config ()
  (when (or (string-equal (buffer-file-name)
						  (expand-file-name "~/development/books/programming-principles-and-practice-using-c++/README.org"))
			(string-equal (buffer-file-name)
						  (expand-file-name "~/development/books/compilers-dragon-book/README.org")))
	;; Dynamic scoping to the rescue
	(let ((org-confirm-babel-evaluate nil))
	  (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

;; drag and drop images to org-mode buffers
(use-package org-download)

(use-package simple-httpd)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")               ]        ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;; dired
(use-package dired-single)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ((dired-listing-switches "-agho --group-directories-first")
   ;; For MacOS only, requires coreutils package installation
   (insert-directory-program "gls" dired-use-ls-dired t))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Configure how some kind of files shoudl be opened
(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions
		'(("png" . "feh")
		  ("jpg" . "feh")
		  ("jpeg" . "feh")
		  ("mp4" . "mpv"))))

(use-package dired-hide-dotfiles
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
	"H" 'dired-hide-dotfiles-mode))

;; ecj-sql
;; =================== The problem is in the cider package:
;; https://github.com/clojure-emacs/cider/blob/master/cider-overlays.el
;; Track this issue to see if it is fixed
;; https://github.com/clojure-emacs/cider/issues/3596
;; ===================

(use-package flx-ido)

(use-package auto-complete
  :config
  (define-key ac-mode-map (kbd "C-M-c") 'auto-complete))

(use-package ejc-sql)
(setq clomacs-httpd-default-port 8090)
(setq ejc-use-flx t)

(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (ejc-ac-setup)))
(add-hook 'ejc-sql-minor-mode-hook
		  (lambda ()
            (ejc-eldoc-setup)))
(add-hook 'sql-mode-hook
          (lambda ()
			(ejc-sql-mode)))

;; LSP
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package graphql-mode
  :mode "\\.gql\\'"
  :hook (graphql-mode . lsp-deferred)
  :config
  (setq graphql-indent-level 2))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (add-to-list 'lsp-language-id-configuration '(graphql-mode . "gql"))
  (lsp-enable-which-key-integration t)
  (dolist (ignored-dirs '("[/\\\\]build\\'"
						  "[/\\\\]bin\\'"
						  "[/\\\\]logs\\'"
						  "[/\\\\]gradle\\'"
						  ))
	(push ignored-dirs lsp-file-watch-ignored-directories)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "c s" #'lsp-ui-doc-show)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "c h" #'lsp-ui-doc-hide)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-imenu-auto-refresh t))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

;; Java
(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package yasnippet
  :config
  (yas-global-mode)
  (define-key yas-minor-mode-map (kbd "C-<tab>") #'yas-expand))
(use-package yasnippet-snippets)

(setq dependencies-dir (expand-file-name "~/development/dependencies"))
(unless (file-exists-p dependencies-dir)
  (make-directory dependencies-dir))
(setq lombok-jar-path (concat dependencies-dir "/lombok.jar"))
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  (unless (file-exists-p lombok-jar-path)
	(url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-jar-path))
  :custom
  (lsp-java-vmargs
   `("-XX:+UseParallelGC"
	 "-XX:GCTimeRatio=4"
	 "-XX:AdaptiveSizePolicyWeight=90"
	 "-Dsun.zip.disableMemoryMapping=true"
	 "-Xmx8G"
	 "-Xms100m"
	 ,(concat "-javaagent:" lombok-jar-path)
	 )))

(require 'lsp-java-boot)
;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(use-package dap-mode
  :after lsp-mode
  :custom
  (dap-auto-configure-features '(sessions locals expressions tooltip))
  :config
  (dap-auto-configure-mode)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger"))
  (define-key dap-mode-map (kbd "<f7>") #'dap-step-in)
  (define-key dap-mode-map (kbd "<f8>") #'dap-next)
  (define-key dap-mode-map (kbd "<f9>") #'dap-continue))

(use-package dap-java
  :ensure nil
  :after (lsp-java)
  :config
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "t c" #'dap-java-debug-test-class)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "t m" #'dap-java-debug-test-method))

(use-package helm)

(use-package helm-lsp
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; perfomance tuning

;; Avoid garbage collection at statup
(setq gc-cons-threshold most-positive-fixnum ;; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 300000000 ;; 300mb
          gc-cons-percentage 0.1)))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; I didn't find another way to work with large Java projects
(setq lsp-response-timeout 30)

;; Fix compile escape codes to print stuff like test outputs nicely
(defun ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
	(ansi-color-apply-on-region (point-min) (point-max))))
(use-package ansi-color
  :config
  (add-hook 'compilation-filter-hook 'ansi-colorize-buffer))

;; C++
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; REST
(use-package restclient)

(use-package axe)

(use-package deferred)
(use-package request)
(use-package request-deferred)

(add-hook 'find-file-hook 'req-buffers-hook)
(defun req-buffers-hook ()
  (when (string= (file-name-extension buffer-file-name) "req") 
    (restclient-mode)))

;; Coding settings

;; Automatically add ending brackets and braces
(electric-pair-mode 1)

;; Make sure tab-width is 4 and not 8
(setq-default tab-width 4)

;; Highlight matching brackets and braces
(show-paren-mode 1)
