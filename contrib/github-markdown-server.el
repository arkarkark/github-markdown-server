;;; github-markdown-server -- start a server and open a markdown file in it
;;; Created by: Alex K (wtwf.com) Thu Nov 27 02:48:00 2014

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar github-markdown-servers '() "The servers we have running.")
(defvar github-markdown-server-port 7494 "The next available server port.")
(defvar github-markdown-server-browser "Google Chrome" "Application to use with osx's open -a to open files.")

(defun git-base (&optional file-name)
  "Get the base of a git repository for FILE-NAME (or the current buffer)."
  (interactive (buffer-file-name))
  (shell-command-to-string
    (concat "git -C " (shell-quote-argument (directory-file-name file-name))
      " rev-parse --show-toplevel 2>/dev/null")))

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
            (message (concat "re-starting github-markdown-server on port:" (number-to-string (cdr server))))
            (progn
              ;; find the next available port
              (while (< 0 (length (shell-command-to-string
                       (concat "lsof -n -i4TCP:" (number-to-string github-markdown-server-port) " | grep LISTEN"))))
                (setq github-markdown-server-port (+ github-markdown-server-port 1)))

              (setq server (cons base github-markdown-server-port)))
              (message (concat "starting github-markdown-server on port:" (number-to-string (cdr server)))))
          (call-process-shell-command
            (concat "github-markdown-server"
              " --directory=" (shell-quote-argument (car server))
              " --port=" (number-to-string (cdr server))
              " " (shell-quote-argument file-name))
            nil 0)
          (if (not server)
            (progn
              (add-to-list 'github-markdown-servers server)
              (setq github-markdown-server-port (+ github-markdown-server-port 1))))))
      (message ""))))

(defun view-file-in-browser (&optional file-name)
  "Open up file on github or via a local markdown server.
FILE-NAME will be current buffer if not specified.
Prefix arg \[universal-argument] to not run local server."
  (interactive (list (buffer-file-name)))
  (if (and (not current-prefix-arg) (string-match "\.md$" file-name))
    (github-markdown-serve file-name)
    (call-process-shell-command
      (concat "open -a " (shell-quote-argument github-markdown-server-browser) " \"$("
        "perl -e 'use File::Basename; $f = shift @ARGV; $d = dirname($f); $b = basename($f); "
        "$o = `git -C $d config --get remote.origin.url`; "
        "if ($o =~ qr(!git(@|://)github.com[:/]!)) { "
        "  $o =~ s!git(@|://)github.com[:/](.*).git$!https://github.com/$2!; "
        "} else { $o = \"\" }"
        "$p = `git -C $d rev-parse --show-prefix`; "
        "chomp($o, $p); "
        "if ($o) { print \"$o/blob/master/$p$b\n\"; } else { print \"file://$f\" }' "
        (shell-quote-argument (buffer-file-name)) ")\"")
    nil 0)))

(provide 'github-markdown-server)
;;; github-markdown-server ends here
