(defvar socket nil)
(defvar panel nil)
(defvar buffer-file (make-temp-file "eslime"))

(defun str-join (separator seq)
  (mapconcat 'identity seq " "))

(defun send-string-to-panel (s socket panel)
  (with-temp-file buffer-file
    (insert s))
  (shell-command (str-join " " (list "tmux -L" socket "load-buffer" buffer-file)))
  (shell-command (str-join " " (list "tmux -L" socket "paste-buffer -d -t" panel))))

(defun append-newline-if-missing (s)
  (replace-regexp-in-string "\n\n$" "\n$" (concat s "\n")))

(defun send-string (s)
  (unless socket
    (setq socket (read-string "tmux socket name or absolute path: " "default")))
  (unless panel
    (setq panel (read-string "tmux target pane: " "{last}")))
  (send-string-to-panel (append-newline-if-missing s) socket panel))

(defun send ()
  (interactive)
  (let ((pos (point)))
    (unless (use-region-p)
      (mark-paragraph))
    (send-string (buffer-substring-no-properties (region-beginning) (region-end)))
    (deactivate-mark)
    (goto-char pos)))

(defun reset ()
  (interactive)
  (setq socket nil)
  (setq panel nil))
