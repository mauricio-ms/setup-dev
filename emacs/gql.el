;;; gql.el --- Module to manage GQL resources.

;;; Commentary:

;;; gql.el provides functions to find and create GQL query/mutations to be used in conjunction with graphql-mode package

;;; Code:

(defconst gql-resources-dir "~/development/notebook/gql/"
  "Constant to define the directory to store the GQL resource files.")
(defconst gql-resource-extension ".gql"
  "Constant to define the extension of the resource files.")

(defun gql-resource-find ()
  "Find a GQL resource."
  (interactive)
  (consult--read (gql-resource--list)
				 :prompt "Resource: "
				 :lookup (lambda (selected candidates input narrow)
						   (if (not (gql-resource--exists? selected))
							   (gql-resource--create selected))
						   (gql-resource--open selected))))
(global-set-key (kbd "C-c r g") 'gql-resource-find)

(defun gql-resource--list ()
  "Get a list of resources."
  (mapcar
   (lambda (f) (car (s-split gql-resource-extension f)))
   (gql-resource--files)))

(defun gql-resource--files ()
  "Get a list of files contained in GQL-RESOURCES-DIR."
  (directory-files
   gql-resources-dir
   nil
   ".gql"))

(defun gql-resource--exists? (resource)
  "Check if RESOURCE exists."
  (f-exists? (gql-resource--path resource)))

(defun gql-resource--create (resource)
  "Create a new RESOURCE."
  (with-temp-file (gql-resource--path resource)
	;; write some content if needed
	))

(defun gql-resource--open (resource)
  "Open RESOURCE."
  (switch-to-buffer (find-file-noselect (gql-resource--path resource))))

(defun gql-resource--path (resource)
  "Get the path of the file for RESOURCE."
  (concat gql-resources-dir resource gql-resource-extension))

(provide 'gql)

;;; gql.el ends here
