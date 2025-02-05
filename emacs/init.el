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

(add-hook 'find-file-hook 'org-download-blog-hook)
(defun org-download-blog-hook ()
  "Set org-download-image-dir as a local buffer variable when opening a blog post.
Set the value as the blogpost name without the org extension."
  (if (s-contains? "projects/blog/content/posts" buffer-file-name)
	  (setq-local org-download-image-dir
				  (car
				   (split-string
					(file-name-nondirectory buffer-file-name)
					"\\.")))))

(load-library "~/development/setup-dev/emacs/common.el")
