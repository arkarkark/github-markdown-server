;;; github-markdown-server -- start a server and open a markdown file in it
;;; Created by: Alex K (wtwf.com) Thu Nov 27 02:48:00 2014

;;; Commentary:

;;; view-file-in-browser will try and show the current file in the best way in the current browser.

;;; Since I use rvm you'll need rvm.el from here https://github.com/senny/rvm.el
;;; Uncommenthing this _might_ work (untested)
;;; (defun rvm-use-default () ".") (provide 'rvm)

;;; I bind some things to keys like this:
;;; (global-set-key [(control f1)]		'view-file-in-browser)
;;; (global-set-key [(shift control f1)]	'copy-file-url-to-clipboard)

;;; Code:

(require 'cl-lib)

(autoload 'rvm-use-default "rvm" nil t)

(defvar github-markdown-servers '() "The servers we have running.")
(defvar github-markdown-server-port 7494 "The next available server port.")

;; from the emacs wiki cookbook
(defun ark-chomp (str)
  "Chomp leading and trailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n"))) (: (* (any " \t\n")) eos))) "" str))

(defun git-base (&optional file-name)
  "Get the base of a git repository for FILE-NAME (or the current buffer)."
  (if (not file-name) (setq file-name (buffer-file-name)))
  (let* ((dirname (file-name-directory file-name))
          (cmd (concat "git -C " (shell-quote-argument dirname) " rev-parse --show-toplevel 2>/dev/null")))
    (ark-chomp (shell-command-to-string cmd))))

(defun github-markdown-server-start (directory port file-name)
  "Start a github-markdown-server for DIRECTORY on PORT and open FILE-NAME."
  (rvm-use-default)
  (setenv "LANG" "en_US.UTF-8")
  (let ((cmd (concat
               "github-markdown-server"
               " --directory=" (shell-quote-argument directory)
               " --port=" (number-to-string port)
               " " (shell-quote-argument file-name))))
    (call-process-shell-command cmd nil 0)))

(defun github-markdown-serve  (&optional file-name)
  "Get the base of a git repository for FILE-NAME (or the current buffer)."
  (interactive (list (buffer-file-name)))
  (ark-reuse-or-start-server file-name 'github-markdown-server-start)
  )

(defun ark-reuse-or-start-server (file-name start-command &optional path-name-fixer)
  "Reuse a server if we can, otherwise start a new server.
FILE-NAME what we are serving.
START-COMMAND the command to start a server."
  (if (not path-name-fixer) (setq path-name-fixer (lambda (file-name path-name) path-name)))
  (let ((base (git-base file-name)) (server nil) (server-running nil))
    ;;;
    (progn
      (if (< (length base) 1) (setq base (file-name-directory file-name)))
      ;; find a server that might still be running
      (setq server
        (cl-reduce (lambda (a b)
                     (if (and (string= (car b) (substring base 0 (min (length base) (length (car b)))))
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
          (message (concat "reusing server"  (prin1-to-string (funcall path-name-fixer file-name (substring file-name (length (car server)))))))
          (call-process-shell-command
            (concat "open "
              (shell-quote-argument
                (concat "http://localhost:"
                  (number-to-string (cdr server))
                  (funcall path-name-fixer file-name (substring file-name (length (car server)))))))))
        (progn
          (if server
            (message (concat "re-starting github-markdown-server on port:" (number-to-string (cdr server))))
            (progn
              ;; find the next available port
              (while (< 0 (length (shell-command-to-string
                                    (concat "lsof -n -i4TCP:" (number-to-string github-markdown-server-port) " | grep LISTEN"))))
                (setq github-markdown-server-port (+ github-markdown-server-port 1)))

              (setq server (cons base github-markdown-server-port))
              (message (concat "starting server on port:" (number-to-string (cdr server))))
              (add-to-list 'github-markdown-servers server)
              (setq github-markdown-server-port (+ github-markdown-server-port 1))))
          (funcall start-command (car server) (cdr server) file-name)))
      (message ""))))

(defun jekyll-get-permalink-from-file (file-name path-name)
  "Get a permalink from FILE-NAME or work it out from suggest PATH-NAME."
  ;;(read-string (concat file-name "   ---   " path-name))
  (if (not (string= path-name "/"))
    (with-temp-buffer
      (insert-file-contents file-name)
      (goto-char (point-min))
      (let ((start (search-forward-regexp "^---" 6 't)))
        (if (and start (= start 4))
          (progn
            (goto-char (+ (point-min) 3))
            (if (search-forward-regexp "^---" (point-max) 't)
              (let ((bound (point)))
                (goto-char (point-min))
                (if (search-forward-regexp "^permalink: \\(.*\\)$" bound 't)
                  (setq path-name (match-string 1))))))))))
  path-name)

(defun jekyll-fix-path-name (file-name path-name)
  "Work out the pathname that FILE-NAME will be served at (PATH-NAME is our best guess right now)."
  (setq path-name (file-name-sans-extension path-name))
  (jekyll-get-permalink-from-file file-name
    (cond
      ((string-match "^/_posts/\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-[0-9][0-9]-\\(.*\\)" path-name)
        (concat "/" (match-string 1 path-name) "/" (match-string 2 path-name) "/" (match-string 3 path-name)))
      ((string-match "^/[^_]" path-name) path-name)
      ('t "/"))))

(defun jekyll-server-start (directory port file-name)
  "Start a Jekyll server for DIRECTORY on PORT and open FILE-NAME."
  (rvm-use-default)
  (let* (
          (url (concat "http://localhost:" (number-to-string port)
                 (jekyll-fix-path-name file-name (substring file-name (length directory)))))
          (cmd (concat
                 "cd " (shell-quote-argument directory)
                 ";jekyll serve -q "
                 " --port " (number-to-string port)
                 ";open  " (shell-quote-argument url))))
    (call-process-shell-command cmd nil 0)))

(defun jekyll-file-p (file-name)
  "Is FILE-NAME part of a jekyll site."
  (string-match "\.github\.io/" file-name))

(defun jekyll-serve-file (&optional file-name)
  "Find or start a server to serve FILE-NAME from a jekyll site."
  (interactive (buffer-file-name))
  (ark-reuse-or-start-server file-name 'jekyll-server-start 'jekyll-fix-path-name)
  )

(defun shell-command-to-get-file-url (file-name)
  "Make a shell command will get the file url. FILE-NAME the filename to get the url for."
  (concat
    "$("
    "perl -e 'use File::Basename; $f = shift @ARGV; $d = dirname($f); $b = basename($f); "
    "$o = `git -C $d config --get remote.origin.url`; "
    "if ($o =~ qr!git(@|://)github.com[:/]!) { "
    "  $o =~ s!git(@|://)github.com[:/](.*).git$!https://github.com/$2!; "
    "} else { $o = \"\" }"
    "$p = `git -C $d rev-parse --show-prefix`; "
    "chomp($o, $p); "
    "$lc = `git log -1 --pretty=format:%h`; "
    "if ($o) { print \"$o/blob/$lc/$p$b\"; } else { print \"file://$f\" }' "
    (shell-quote-argument file-name)
    ")"
    (if (region-active-p)
      (concat "#L"
	(number-to-string (line-number-at-pos (region-beginning))) "-L"
	(number-to-string (line-number-at-pos (- (region-end) 1) )))
      (if (> (line-number-at-pos) 1) (concat "#L" (number-to-string (line-number-at-pos))) ""))
    ))

(defun view-file-in-browser (&optional file-name)
  "Open up file in one of four ways.

1) on a local Jekyll server if the path contains .github.io/ (requires rvm.el)
2) via a local https://rubygems.org/gems/github-markdown-server
3) on github (if file is in a github.com repository)
4) via a file:/// url (last resort)
FILE-NAME will be current buffer if not specified.
Prefix arg \[universal-argument] to not run local server and open on github or via file://."
  (interactive (list (buffer-file-name)))
  (cond
    ((and (not current-prefix-arg) (jekyll-file-p file-name))
      (jekyll-serve-file file-name))
    ((and (not current-prefix-arg) (string-match "\.md$" file-name))
      (github-markdown-serve file-name))
    ('t
      (call-process-shell-command
        ;; open it with the default webbrowser The Mac::InternetConfig stuff)
        ;; open either on github or a local file (the remove.origin.url stuff)
        (concat "open -a \"$(VERSIONER_PERL_PREFER_32_BIT=1 "
          "perl -MMac::InternetConfig -le 'print +(GetICHelper \"http\")[1]')\" \""
	  (shell-command-to-get-file-url file-name)
          "\"")
        nil 0))))

(defun copy-file-url-to-clipboard (&optional file-name)
  "Put the url to access FILE-NAME in the copy buffer."
  (interactive (list (buffer-file-name)))
  (call-process-shell-command (concat "echo \"" (shell-command-to-get-file-url file-name) "\" | pbcopy")))

(provide 'github-markdown-server)
;;; github-markdown-server ends here
