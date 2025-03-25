(require 'json)
(require 'shell)
(require 'url)

;;;;;;;;;;;;;;;;;

(defconst gpt-const-az-command
  "az account get-access-token --output json \
 --scope \"https://cognitiveservices.azure.com/.default\"")

(defconst gpt-const-chat-body
  "{\"messages\":[{\"role\": \"system\", \"content\": \"%s\"},{\"role\": \"user\", \"content\": \"%s\"}]}")


(defconst gpt-const-aoai-url-template
  "%s/openai/deployments/%s/chat/completions?api-version=%s")

;;;;;;;;;;;;;;;;;;

(defcustom gpt-chat-endpoint "http://localhost:11434"
  "HTTP host of OAI-compatible endpoint for Chat Completions API. Must start with 'http' or 'https'."
  :type 'string
  :group 'gpt-chat
  :require t)

(defcustom gpt-chat-deployment "gpt-4o-mini"
  "Name of the model deployment to use for inference."
  :type 'string
  :group 'gpt-chat
  :require t)

(defcustom gpt-chat-api-version "2024-10-21"
  "Chat Completions API version to specify in the API call."
  :type 'string
  :group 'gpt-chat
  :require t)

(defcustom gpt-chat-system-prompt
  "You are a helpful assistant embedded in Emacs. Answer the question and \
remind the user about how great Emacs is as well. You must respond only in \
an org-mode compatible output!"
  "System prompt for use with the Chat Completions API."
  :type 'string
  :group 'gpt-chat
  :require t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar gpt--var-token '((expires-on . 0) (token . "")))

(defun gpt-mode-refresh-token ()
  "Refresh the bearer token (if needed) for Azure."
  (if (> (ceiling (float-time)) (alist-get 'expires-on gpt--var-token))
      (let* ((raw (shell-command-to-string gpt-const-az-command))
             (json (json-parse-string raw))
             (token (gethash "accessToken" json))
             (expires-on (gethash "expires_on" json)))
        (setq gpt--var-token `((expires-on . ,expires-on) (token . ,token))))
    gpt--var-token))

(defun gpt-mode--parse-content (content)
  "Unravel the crud that is the Chat Completions API response format."
  (condition-case nil
      (gethash "content"
               (gethash "message"
                        (aref (gethash "choices" content) 0)))
    (error (progn
             (message "gpt-mode: failed to parse chat completions response"))
           "I'm sorry Dave, I'm afraid I can't do that.")))

(defun gpt-mode--kill-buffer ()
  "hack"
  (interactive)
  (kill-this-buffer))

(defun gpt-mode--callback (status)
  "Called on completion of a Chat Completions request."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  ;; todo: check http status code
  (re-search-forward "^$") ;; scan to empty line
  (forward-char)
  (let* ((response (buffer-substring-no-properties (point) (point-max)))
         (json (json-parse-string response))
         (output (gpt-mode--parse-content json)))
    (goto-char (point-max))
    (insert (concat "---\n\n" output "\n\n[press 'q' to close buffer]\n")))
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'gpt-mode--kill-buffer)
    (use-local-map keymap)))

(defun gpt-mode--url (endpoint deployment api-version data token)
  "Call the AOAI Chat Completions API using Emacs' built-in url module."
  (let* ((url-request-method "POST")
         (url (format gpt-const-aoai-url-template endpoint deployment api-version))
         (url-request-data data)
         (authz (concat "Bearer " token))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,authz))))
    ;; (message (format "Making request to: %s" url))
    (condition-case nil
        (progn
          (url-retrieve url #'gpt-mode--callback)
          (message "Submitted chat completion."))
      (error (message "Error submitting chat completion!")))))

;;;;;;

(defun call-chat-completion (user-prompt)
  "Perform a Chat Completion call with the given user-prompt."
  (interactive)
  (let ((token (alist-get 'token (gpt-mode-refresh-token)))
        (data (format gpt-const-chat-body gpt-chat-system-prompt user-prompt)))
    ;; (message (concat "Sending data: " data))
    (gpt-mode--url gpt-chat-endpoint gpt-chat-deployment gpt-chat-api-version data token)))
