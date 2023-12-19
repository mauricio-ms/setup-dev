;;; http.el --- Module to manage HTTP requests.

;;; Commentary:

;;; http.el provides functions to find and create HTTP requests to be used in conjunction with request.el package

;;; Code:

(defconst http-requests-dir "~/development/notebook/http/"
  "Constant to define the directory to store the request files.")
(defconst http-request-extension ".req"
  "Constant to define the extension of the request files.")

(defcustom http-request-apis nil
  "List of APIs to be used in the HTTL request creation."
  :type 'list)
(defcustom http-request-api-resolver nil
  "Function to resolve an API by his description to his address."
  :type 'function)

(defun http-request-find ()
  "Find a HTTP request."
  (interactive)
  (consult--read (http-request--list)
				 :prompt "Request: "
				 :lookup (lambda (selected candidates input narrow)
						   (if (not (http-request--exists? selected))
							   (http-request--create selected))
						   (http-request--open selected))))
(global-set-key (kbd "C-c r h") 'http-request-find)

(defun http-request--list ()
  "Get a list of requests."
  (mapcar
   (lambda (f) (car (s-split http-request-extension f)))
   (http-request--files)))

(defun http-request--files ()
  "Get a list of files contained in HTTP-REQUESTS-DIR."
  (directory-files
   http-requests-dir
   nil
   ".req"))

(defun http-request--exists? (request)
  "Check if REQUEST exists."
  (f-exists? (http-request--path request)))

(defun http-request--create (request)
  "Create a new REQUEST."
  (let ((request-file-path (http-request--path request))
		(method (http-request--select-method))
		(api-url (format "(%s \"%s\")" http-request-api-resolver (http-request--select-api))))
	(with-temp-file request-file-path
	  (insert (format ":url := %s\n" api-url))
	  (insert "GET :url\n")
	  (insert "Content-Type: application/json\n")
	  (if (http-request--needs-request-body method)
		  (insert "\n{\n}")))))

(defun http-request--select-method ()
  "Select a HTTP method."
  (consult--read '("GET" "POST" "PUT" "PATCH" "DELETE")
				 :prompt "Method: "
				 :require-match t))

(defun http-request--select-api ()
  "Select a HTTP method."
  (consult--read http-request-apis
				 :prompt "API: "
				 :require-match t))

(defun http-request--needs-request-body (method)
  "Check if a request for METHOD needs a request body."
  (member method '("POST" "PUT" "PATCH")))

(defun http-request--open (request)
  "Open REQUEST."
  (switch-to-buffer (find-file-noselect (http-request--path request))))

(defun http-request--path (request)
  "Get the path of the file for REQUEST."
  (concat http-requests-dir request http-request-extension))

(provide 'http)

;;; http.el ends here
