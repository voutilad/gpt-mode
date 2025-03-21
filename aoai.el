(require 'json)
(require 'shell)
(require 'url)

(defconst aoai-const-az-command
  "az account get-access-token --output json --scope \"https://cognitiveservices.azure.com/.default\"")

(defvar aoai-var-token nil)
(setq aoai-var-token nil)

(defvar aoai-system-prompt
  "You are a helpful assistant embedded in Emacs. Answer the question and remind the user about how great Emacs is as well. You must respond only in an org-mode compatible output!")

(defun aoai-refresh-token ()
  (if (not (not t)) ;; todo: store expiry time
      (let* ((raw (shell-command-to-string aoai-const-az-command))
             (json (json-parse-string raw))
             (token (gethash "accessToken" json)))
        (setq aoai-var-token token))
    aoai-var-token))

(defun aoai-json ()
  (let ((token (aoai-refresh-token)))
    (format "Parsed into: %s" token)))

(defconst chat-body
  "{\"messages\":[{\"role\": \"system\", \"content\": \"%s\"},{\"role\": \"user\", \"content\": \"%s\"}]}")


(defconst gpt-mode-oai-chat-template
  "https://%s/openai/deployments/%s/chat/completions?api-version=%s")

(defun gpt-mode--curl (endpoint deployment token))

(defun gpt-mode--url (endpoint deployment token api-version data)
  (let* ((url-request-method "POST")
         (url (format gpt-mode-oai-chat-template endpoint deployment api-version))
         (url-request-data data)
         (authz (concat "Bearer " token))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,authz))))
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
  (let* ((endpoint "voutila-aoai-ncus.openai.azure.com")
         (deployment "emacs-4o-mini")
         (api-version "2024-10-21")
         (url (format "https://%s/openai/deployments/%s/chat/completions?api-version=%s"
                      endpoint deployment api-version))
         (token (aoai-refresh-token))
         (url-request-body (prin1-to-string (format chat-body aoai-system-prompt user-prompt)))
         (cmd (format
               "curl -s -H \"Authorization: Bearer %s\" --json %s \"%s\""
               token url-request-body url)))
    (let* ((output (shell-command-to-string cmd))
           (result (json-parse-string output)))
      (gethash "content" (gethash "message" (aref (gethash "choices" result) 0))))))
