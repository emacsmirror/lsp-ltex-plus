;;; lsp-ltex-plus-doctor.el --- What LTeX+ is doing, in one buffer -*- lexical-binding: t; -*-

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; `lsp-ltex-plus-doctor' answers the question the logs used to be asked:
;; is this thing working, and with what?  It is an ordinary org buffer,
;; checked by the ordinary machinery, holding a report of what the client
;; and the server are doing.
;;
;; The report itself is kept out of the check with a magic comment, so
;; that paths, symbols and version strings are not offered up as prose.
;; What follows the report -- added in a later step -- is deliberately
;; wrong text in a few languages, which the server flags in front of the
;; user.
;;
;; The file is loaded when the command is first called; nothing else in
;; the package requires it.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'lisp-mnt)
(require 'lsp-ltex-plus)

(defconst lsp-ltex-plus-doctor-buffer-name "*lsp-ltex-plus doctor*"
  "Name of the buffer `lsp-ltex-plus-doctor' reports in.")

;;;; -- Facts to report ---------------------------------------------------------

(defun lsp-ltex-plus-doctor--library-version (library)
  "Return the `Version:' header of LIBRARY's source, or nil.
Read from the file: neither this package nor `jsonrpc' defines a
variable holding its version, and the header is what a release bumps."
  (let ((file (locate-library (concat library ".el"))))
    (and file
         (file-readable-p file)
         (lm-with-file file (lm-header "version")))))

(defun lsp-ltex-plus-doctor--server-line ()
  "Return what is known about the server binary and the running server."
  (let* ((executable (lsp-ltex-plus--server-executable))
         (connection (lsp-ltex-plus--live-connection))
         (info (and connection
                    (lsp-ltex-plus--connection-server-info connection)))
         (version (and info (plist-get info :version))))
    (list (cons "Executable"
                (or executable
                    (format "not found -- `%s' is not on `exec-path'; \
set `lsp-ltex-plus-ls-plus-executable'"
                            lsp-ltex-plus-ls-plus-executable)))
          (cons "Reports itself as"
                (cond ((not connection) "nothing yet -- no server is running")
                      (version (format "%s %s"
                                       (or (plist-get info :name) "ltex-ls")
                                       version))
                      (t "nothing -- a server older than 18.7.0, which is \
the first release that says")))
          (cons "Minimum version"
                (format "%s (%s)"
                        lsp-ltex-plus-minimum-server-version
                        (if lsp-ltex-plus-require-minimum-server-version
                            "enforced: an older server is stopped"
                          "not enforced: an older server is used anyway")))
          (cons "Java"
                (if lsp-ltex-plus-java-path
                    (format "JAVA_HOME=%s" lsp-ltex-plus-java-path)
                  "whatever the launcher finds (usually its bundled runtime)"))
          (cons "Heap"
                (let ((options (lsp-ltex-plus--heap-options)))
                  (if options
                      (string-join options " ")
                    "the JVM's own default"))))))

(defun lsp-ltex-plus-doctor--connection-line ()
  "Return what is known about the connection to the server."
  (let ((connection (lsp-ltex-plus--live-connection)))
    (list (cons "State"
                (cond ((not connection) "not running -- it starts with the \
first buffer that needs it")
                      ((lsp-ltex-plus--connection-ready connection) "live, \
handshake complete")
                      (t "starting -- the handshake has not finished")))
          (cons "Root"
                (if connection
                    (lsp-ltex-plus--connection-root connection)
                  "--"))
          (cons "Documents open"
                (format "%d" (hash-table-count lsp-ltex-plus--documents))))))

(defun lsp-ltex-plus-doctor--buffer-line ()
  "Return what is in force in the doctor buffer itself."
  (list (cons "Language id" (lsp-ltex-plus--language-id))
        (cons "Front-end"
              (cond ((null lsp-ltex-plus--attached-provider)
                     (format "none yet -- this buffer is not being checked (you asked for %s)" lsp-ltex-plus-diagnostics-provider))
                    ((eq lsp-ltex-plus--attached-provider
                         lsp-ltex-plus-diagnostics-provider)
                     (format "%s" lsp-ltex-plus--attached-provider))
                    (t (format "%s -- you asked for %s, which this Emacs does not have" lsp-ltex-plus--attached-provider
                       lsp-ltex-plus-diagnostics-provider))))
        (cons "Checked" (if lsp-ltex-plus-mode "yes" "no"))))

(defun lsp-ltex-plus-doctor--count (kind)
  "Return how many entries KIND holds in total, across every language.
The global list: the defcustom merged with the file, which is what the
server is told about every document.  A project can add to it, and does
not show here -- see the last section of the report."
  (cl-loop for (_language entries) on (lsp-ltex-plus--global-plist kind)
           by #'cddr
           sum (length entries)))

(defun lsp-ltex-plus-doctor--settings-line ()
  "Return the settings a check is made with."
  (list (cons "Language" lsp-ltex-plus-language)
        (cons "Change delay" (format "%s s" lsp-ltex-plus-change-delay))
        (cons "Dictionary" (format "%d word(s)"
                                   (lsp-ltex-plus-doctor--count 'dictionary)))
        (cons "Disabled rules"
              (format "%d" (lsp-ltex-plus-doctor--count 'disabled-rules)))
        (cons "Enabled rules"
              (format "%d" (lsp-ltex-plus-doctor--count 'enabled-rules)))
        (cons "Hidden false positives"
              (format "%d"
                      (lsp-ltex-plus-doctor--count 'hidden-false-positives)))))

(defun lsp-ltex-plus-doctor--logging-line ()
  "Return where each of the four records is going, if anywhere."
  (list (cons "Client log"
              (if lsp-ltex-plus-debug
                  "on, in *lsp-ltex-plus log*"
                "off (`lsp-ltex-plus-debug')"))
        (cons "Wire record"
              (cond ((eql lsp-ltex-plus-events-buffer-size 0)
                     "off (`lsp-ltex-plus-events-buffer-size')")
                    ((null lsp-ltex-plus-events-buffer-size)
                     (format "unlimited, %s, in *ltex-ls-plus events*"
                             lsp-ltex-plus-events-buffer-format))
                    (t (format "%d bytes, %s, in *ltex-ls-plus events*"
                               lsp-ltex-plus-events-buffer-size
                               lsp-ltex-plus-events-buffer-format))))
        (cons "Server log file"
              (or lsp-ltex-plus-server-log-file
                  "none (`lsp-ltex-plus-server-log-file')"))
        (cons "Server log level" lsp-ltex-plus-ltex-ls-log-level)))

(defun lsp-ltex-plus-doctor--environment-line ()
  "Return the versions a bug report should carry."
  (list (cons "Emacs" emacs-version)
        (cons "jsonrpc"
              (or (lsp-ltex-plus-doctor--library-version "jsonrpc") "unknown"))
        (cons "lsp-ltex-plus"
              (or (lsp-ltex-plus-doctor--library-version "lsp-ltex-plus")
                  "unknown"))))

;;;; -- The report --------------------------------------------------------------

(defun lsp-ltex-plus-doctor--insert-section (title rows)
  "Insert an org heading TITLE followed by ROWS as a description list."
  (insert "* " title "\n")
  (pcase-dolist (`(,label . ,value) rows)
    (insert (format "  - %-22s :: %s\n" label value)))
  (insert "\n"))

(defun lsp-ltex-plus-doctor--insert-report ()
  "Insert the report into the current buffer, at point.
Everything here is inside the region the first magic comment disables,
so none of it is offered to the server as prose."
  (insert "#+title: LTeX+ doctor\n"
          "#+options: toc:nil\n\n"
          "  g  refresh   r  restart the server   q  bury"
          "   C-c \"  suggestions at point\n\n")
  (lsp-ltex-plus-doctor--insert-section "Server"
                                        (lsp-ltex-plus-doctor--server-line))
  (lsp-ltex-plus-doctor--insert-section "Connection"
                                        (lsp-ltex-plus-doctor--connection-line))
  (lsp-ltex-plus-doctor--insert-section "This buffer"
                                        (lsp-ltex-plus-doctor--buffer-line))
  (lsp-ltex-plus-doctor--insert-section "Settings"
                                        (lsp-ltex-plus-doctor--settings-line))
  (lsp-ltex-plus-doctor--insert-section "Logging"
                                        (lsp-ltex-plus-doctor--logging-line))
  (lsp-ltex-plus-doctor--insert-section "Environment"
                                        (lsp-ltex-plus-doctor--environment-line))
  (insert "* Where these values come from\n"
          "  The settings above are your global ones.  Any buffer can be\n"
          "  checked with different values: put them in a `.dir-locals.el'\n"
          "  and they hold for that directory, because the server is\n"
          "  answered from the buffer holding the document it is asking\n"
          "  about.  So what a project's buffers are checked with may not\n"
          "  be what this page shows.\n\n"))

(defun lsp-ltex-plus-doctor--fill ()
  "Replace the contents of the current doctor buffer with a fresh report."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Line one, and it governs everything after it: the report is not
    ;; prose and must not be checked.  See the magic comment reference,
    ;; https://ltex-plus.github.io/ltex-plus/advanced-usage.html#magic-comments
    (insert "# LTeX: enabled=false\n")
    (lsp-ltex-plus-doctor--insert-report)
    (goto-char (point-min))))

;;;; -- The mode and its commands -----------------------------------------------

(defun lsp-ltex-plus-doctor-refresh (&rest _)
  "Write the report again, with what is true now."
  (interactive)
  (lsp-ltex-plus-doctor--fill))

(defun lsp-ltex-plus-doctor-restart-server ()
  "Restart `ltex-ls-plus' and report again once it is up."
  (interactive)
  (lsp-ltex-plus-restart-server)
  (lsp-ltex-plus-doctor-refresh))

(defvar-keymap lsp-ltex-plus-doctor-mode-map
  :doc "Keymap for `lsp-ltex-plus-doctor-mode'."
  "g" #'lsp-ltex-plus-doctor-refresh
  "r" #'lsp-ltex-plus-doctor-restart-server
  "q" #'bury-buffer)

(define-derived-mode lsp-ltex-plus-doctor-mode org-mode "LTeX+ doctor"
  "Major mode for the `lsp-ltex-plus-doctor' report.

Derived from `org-mode' so that the server parses the buffer as org and
reads the magic comments in it.  The language id is inherited through
`lsp-ltex-plus--mode-entry', so this mode is not listed in
`lsp-ltex-plus-major-modes' and nothing is written there on its behalf."
  (setq-local revert-buffer-function #'lsp-ltex-plus-doctor-refresh)
  ;; The buffer visits no file, and a user who switched file-less
  ;; checking off did not mean this buffer.  Buffer-local rather than a
  ;; binding around the call: it has to hold for every later check too,
  ;; after a restart or a mode toggle.
  (setq-local lsp-ltex-plus-check-fileless-buffers t))

;;;###autoload
(defun lsp-ltex-plus-doctor ()
  "Report what LTeX+ is doing, in a buffer it also checks.
Shows the server it found and what that server says about itself, the
state of the connection, the settings a check is made with, and where
each log is going."
  (interactive)
  (let ((buffer (get-buffer-create lsp-ltex-plus-doctor-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lsp-ltex-plus-doctor-mode)
        (lsp-ltex-plus-doctor-mode))
      (lsp-ltex-plus-doctor--fill))
    (pop-to-buffer buffer)))

(provide 'lsp-ltex-plus-doctor)
;;; lsp-ltex-plus-doctor.el ends here
