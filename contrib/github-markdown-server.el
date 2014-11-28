;;; github-markdown-server -- start a server and open a markdown file in it
;;; Created by: Alex K (wtwf.com) Thu Nov 27 02:48:00 2014

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar github-markdown-servers '() "The servers we have running.")
(defvar github-markdown-server-port 7494 "The next available server port.")

(defun git-base (&optional file-name)
  "Get the base of a git repository for FILE-NAME (or the current buffer)."
  (interactive (buffer-file-name))
  (shell-command-to-string (concat "p -t -g " (shell-quote-argument buffer-file-name))))

(defun github-markdown-serve  (&optional file-name)
  "Get the base of a git repository for FILE-NAME (or the current buffer)."
  (interactive (list (buffer-file-name)))
  (let ((base (git-base file-name)) (server nil) (server-running nil))
    ;;;
    (progn
      (if (< (length base) 1) (setq base (directory-file-name file-name)))
      ;; find a server that might still be running
      (setq server (cl-reduce (lambda (a b)
                             (if (and (string= (car b) (substring base 0 (length (car b))))
                                   (or (not (car a)) (< (length (car b)) (length (car a)))))
                               b a)) (cons nil github-markdown-servers)))
      (if (and server
            (progn
              (message (concat "Checking if server running at" (prin1-to-string server)))
              (setq server-running
                (shell-command-to-string
                  (concat "lsof -n -i4TCP:" (number-to-string (cdr server)) " | grep LISTEN")))
              (> (length server-running) 0)))
        (progn
          (message "reusing server")
          (call-process-shell-command
            (concat "open "
              (shell-quote-argument
                (concat "http://localhost:"
                  (number-to-string (cdr server))
                  (substring file-name (length (car server))))))))
        (progn
          (if server
            (progn (message (concat "re-starting server on port " (number-to-string (cdr server)))))
            (progn
              (message "starting server"))
              (setq server (cons base github-markdown-server-port))
              ))
          (call-process-shell-command (concat "github-markdown-server"
                                        " --directory=" (shell-quote-argument (car server))
                                        " --port=" (number-to-string (cdr server))
                                        " " (shell-quote-argument file-name)) nil 0)
          (if (not server)
            (progn (add-to-list 'github-markdown-servers server)
              (setq github-markdown-server-port (+ github-markdown-server-port 1))))))))

(defun view-file-in-browser (&optional file-name)
  "Open up FILE-NAME (or the current buffer) on github or via a local markdown server."
  (interactive (list (buffer-file-name)))
  (if (string-match "\.md$" file-name)
    (github-markdown-serve file-name)
    (call-process-shell-command
      (concat "open "
        (shell-quote-argument
          (shell-command-to-string (concat "p -u -t " (shell-quote-argument file-name))))) nil 0)))

(provide 'github-markdown-server)
;;; github-markdown-server ends here
