(require 'json)
(require 'shell)
(require 'url)

;;;;;;;;;;;;;;;;;

(defconst gpt-const-az-command
  "az account get-access-token --output json --scope \"https://cognitiveservices.azure.com/.default\"")

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

(defvar aoai-var-token nil)

(setq aoai-var-token nil)

(defvar aoai-system-prompt
  "You are a helpful assistant embedded in Emacs. Answer the question and \
remind the user about how great Emacs is as well. You must respond only in \
an org-mode compatible output!")

(defun aoai-refresh-token ()
  (if (not (not t)) ;; todo: store expiry time
      (let* ((raw (shell-command-to-string gpt-const-az-command))
             (json (json-parse-string raw))
             (token (gethash "accessToken" json)))
        (setq aoai-var-token token))
    aoai-var-token))

(defun aoai-json ()
  (let ((token (aoai-refresh-token)))
    (format "Parsed into: %s" token)))

(defun gpt-mode--curl (endpoint deployment token))

(defun gpt-mode--url (endpoint deployment api-version data token)
  (let* ((url-request-method "POST")
         (url (format gpt-const-aoai-url-template endpoint deployment api-version))
         (url-request-data data)
         (authz (concat "Bearer " token))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,authz))))
    (message (format "Making request to: %s" url))
    (url-retrieve url
                  (lambda (status)
                    (switch-to-buffer (current-buffer))
                    (goto-char (point-min))
                    ;; todo: check http status code
                    (re-search-forward "^$")
                    (forward-char)
                    (let* ((response (buffer-substring-no-properties (point) (point-max)))
                           (json (json-parse-string response))
                           (output (gethash "content" (gethash "message" (aref (gethash "choices" json) 0)))))
                      (goto-char (point-max))
                      (insert (concat "---\n" output)))))))


(defun call-chat-completion (user-prompt)
  (interactive)
  (let ((token (aoai-refresh-token))
        (data (format chat-body gpt-chat-system-prompt user-prompt)))
    (message (concat "Sending data: " data))
    (gpt-mode--url gpt-chat-endpoint gpt-chat-deployment gpt-chat-api-version data token)))
