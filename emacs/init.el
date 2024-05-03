;; setup-dev config
(setq setup-dev-project-type "Personal")

;; org-agenda config
(setq org-todo-keywords
	  '((sequence "BACKLOG(b)" "TODO(t)" "ACTIVE(a)" "INTERRUPT(i)" "|" "DONE(d)")))

(setq org-refile-targets
	  '(("~/development/notebook/org-files/archive.org" :maxlevel . 1)))

(setq org-agenda-workflow-command
	  '("w" "Workflow Status"
		((todo "TODO"
               ((org-agenda-overriding-header "To Do")
				(org-agenda-files org-agenda-files)))
		 (todo "ACTIVE"
               ((org-agenda-overriding-header "In Progress")
				(org-agenda-todo-list-sublevels nil)
				(org-agenda-files org-agenda-files)))
		 (todo "INTERRUPT"
               ((org-agenda-overriding-header "Interrupted")
				(org-agenda-files org-agenda-files))))))

(load-library "~/development/setup-dev/emacs/common.el")
