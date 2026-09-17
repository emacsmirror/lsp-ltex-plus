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

(defcustom lsp-ltex-plus-doctor-samples
  '(("en-US" "English"
     "Are you tired of silly spellling mistakes in you're notes? This \
sentance is wrong on purpose, so LTeX+ has something to catch.")
    ("fr-FR" "French"
     "Fatigué des fautes d'ortographe dans vos notes ? Cette phrase est \
fausse exprès, pour que LTeX+ ai quelque chose à corriger.")
    ("de-DE" "German"
     "Müde von dummen Rechtschreibfelern in Ihren Notizen? Dieser Satz ist \
absichtlick falsch, damit LTeX+ etwas zu finden hat."))
  "Sample texts the doctor has the server check, one per language.
Each entry is (LANGUAGE LABEL TEXT).  LANGUAGE is an `ltex.language\='
code, which the doctor puts in a magic comment so that one document can
be checked in several languages.

Write at least one certain mistake into every TEXT.  An empty section
means \"no answer yet\", so a sample with nothing wrong in it looks
exactly like a language the server failed to load.  Prefer plain
misspellings to errors of style: LanguageTool keeps its spelling rules
between releases and revises its style rules.

The shipped samples advertise the package and misspell it while doing
so, which is the shortest way to show a reader what the underlines are
for.

One space after a full stop, not two: French and German flag a repeated
space, and a finding nobody put there on purpose makes the count beside
the heading disagree with the mistakes a reader can see.

The first section is the language you are configured for, when a sample
matches it; the others follow in this order.  Each new language costs
the server a model load, about ten seconds the first time."
  :type '(repeat (list (string :tag "Language")
                       (string :tag "Label")
                       (string :tag "Sample text")))
  :group 'lsp-ltex-plus)

(defconst lsp-ltex-plus-doctor-timeout 30
  "Seconds after which a section with no findings says so.
Long enough for a language model to load on a cold server, short enough
that the buffer does not sit there claiming to be waiting for ever.")

(defvar-local lsp-ltex-plus-doctor--sections nil
  "The sample sections in this buffer, in the order they appear.
Each is a plist with `:language\=', `:label\=', `:beg\=' and `:end\=' markers
around the sample text, an `:overlay\=' carrying the status, and
`:found\=' once the server has flagged something in it.")

(defvar-local lsp-ltex-plus-doctor--started nil
  "When the current report was written, as a float time.
The clock each section\='s timing is measured from.")

(defvar-local lsp-ltex-plus-doctor--timer nil
  "The timer that gives up waiting, or nil.")

(defvar-local lsp-ltex-plus-doctor--overall nil
  "Overlay carrying how long the whole check took, or nil.")

(defvar-local lsp-ltex-plus-doctor--answered nil
  "Seconds the first answer took, or nil while none has come.
One number for the document, not one per section: the server checks the
whole of it and publishes once, so every section is answered at the same
moment.")

;;;; -- Facts to report ---------------------------------------------------------

(defun lsp-ltex-plus-doctor--library-version (library)
  "Return the `Version:' header of LIBRARY's source, or nil.
Read from the file: neither this package nor `jsonrpc' defines a
variable holding its version, and the header is what a release bumps."
  (let ((file (locate-library (concat library ".el"))))
    (and file
         (file-readable-p file)
         (lm-with-file file (lm-header "version")))))

(defun lsp-ltex-plus-doctor--java ()
  "Return the `java' Emacs would start the server with, or nil.
`lsp-ltex-plus-java-path' becomes JAVA_HOME, and the launcher runs the
`java' under it; with the setting unset, the launcher finds `java' the
way Emacs would.  Resolved, never run: reading the version would start
a JVM every time the report is written."
  (if lsp-ltex-plus-java-path
      (let ((java (expand-file-name
                   "bin/java" (expand-file-name lsp-ltex-plus-java-path))))
        (and (file-executable-p java) java))
    (executable-find "java")))

(defun lsp-ltex-plus-doctor--server-line ()
  "Return what is known about the server binary and the running server."
  (let* ((executable (lsp-ltex-plus--server-executable))
         (connection (lsp-ltex-plus--live-connection))
         (info (and connection
                    (lsp-ltex-plus--connection-server-info connection)))
         (version (and info (plist-get info :version))))
    (list (cons "Executable"
                (or executable
                    (format "not found -- =%s= is not on ~exec-path~; \
set ~lsp-ltex-plus-ls-plus-executable~"
                            lsp-ltex-plus-ls-plus-executable)))
          (cons "Full version label"
                (cond ((not connection) "no server is running")
                      (version (format "%s %s"
                                       (or (plist-get info :name) "ltex-ls")
                                       version))
                      (t "none: servers before 18.7.0 report no version")))
          (cons "Detected version"
                (or (lsp-ltex-plus--version-number version)
                    (if connection "none" "--")))
          (cons "Minimum version"
                (format "%s, %s%s"
                        lsp-ltex-plus-minimum-server-version
                        (if lsp-ltex-plus-require-minimum-server-version
                            "required" "not required")
                        (cond ((not connection) "")
                              ((lsp-ltex-plus--version-at-least-p
                                version lsp-ltex-plus-minimum-server-version)
                               " (met)")
                              (t " (NOT met)"))))
          (cons "JAVA_HOME"
                (if lsp-ltex-plus-java-path
                    (format "%s, from ~lsp-ltex-plus-java-path~"
                            (directory-file-name
                             (expand-file-name lsp-ltex-plus-java-path)))
                  "not set by Emacs"))
          (cons "Java"
                (format "%s -- resolved by Emacs, not guaranteed: the \
launcher script can export a JAVA_HOME of its own"
                        (or (lsp-ltex-plus-doctor--java) "none found")))
          (cons "Heap"
                (let ((options (lsp-ltex-plus--heap-options)))
                  (if options
                      (string-join options " ")
                    "the JVM's own default"))))))

(defun lsp-ltex-plus-doctor--connection-line ()
  "Return what is known about the connection to the server."
  (let ((connection (lsp-ltex-plus--live-connection)))
    (list (cons "State"
                (cond ((not connection) "not running; the first buffer that \
needs checking starts it")
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
                     (format "none: this buffer is not being checked.  \
Your setting asks for %s" lsp-ltex-plus-diagnostics-provider))
                    ((eq lsp-ltex-plus--attached-provider
                         lsp-ltex-plus-diagnostics-provider)
                     (format "%s" lsp-ltex-plus--attached-provider))
                    (t (format "%s.  Your setting asks for %s, which this \
Emacs does not have" lsp-ltex-plus--attached-provider
                       lsp-ltex-plus-diagnostics-provider))))
        (cons "Checked" (if lsp-ltex-plus-mode "yes" "no"))))

(defun lsp-ltex-plus-doctor--file-link (variable)
  "Return an org link to the file VARIABLE names, or why there is none."
  (let ((file (symbol-value variable)))
    (cond ((not file) "not specified")
          ((file-readable-p file)
           (format "[[file:%s][%s]]" (expand-file-name file)
                   (file-name-nondirectory file)))
          (t (format "%s (not written yet)" (abbreviate-file-name file))))))

(defun lsp-ltex-plus-doctor--insert-list (kind title unit)
  "Insert what KIND holds, called TITLE and counted in UNIT.
A line per language, so that a total nobody can break down -- 41 words,
in which languages? -- is not all the report has to say, and the two
files the entries come from, named and followed."
  (insert (format "  - %s\n" title))
  (let ((entries (lsp-ltex-plus--global-plist kind)))
    (if (null entries)
        (insert "    - empty\n")
      (cl-loop for (language items) on entries by #'cddr
               for count = (length items)
               do (insert (format "    - %-12s :: %d %s\n"
                                  (substring (symbol-name language) 1)
                                  count
                                  (if (= count 1) (substring unit 0 -1) unit))))))
  (insert (format "    - %-12s :: %s\n" "file"
                  (lsp-ltex-plus-doctor--file-link
                   (lsp-ltex-plus--kind-get kind :global-file))))
  (insert (format "    - %-12s :: %s\n" "project file"
                  (lsp-ltex-plus-doctor--file-link
                   (lsp-ltex-plus--kind-get kind :project-file)))))

(defun lsp-ltex-plus-doctor--settings-line ()
  "Return the settings a check is made with."
  (list (cons "Language" lsp-ltex-plus-language)
        (cons "Change delay" (format "%s s" lsp-ltex-plus-change-delay))))

(defun lsp-ltex-plus-doctor--insert-lists ()
  "Insert the four language-keyed lists, language by language."
  (insert "* Words and rules\n")
  (lsp-ltex-plus-doctor--insert-list 'dictionary "Dictionary" "words")
  (lsp-ltex-plus-doctor--insert-list 'disabled-rules "Disabled rules" "rules")
  (lsp-ltex-plus-doctor--insert-list 'enabled-rules "Enabled rules" "rules")
  (lsp-ltex-plus-doctor--insert-list 'hidden-false-positives
                                     "Hidden false positives" "patterns")
  (insert "\n"))

(defun lsp-ltex-plus-doctor--logging-line ()
  "Return where each of the four records is going, if anywhere."
  (list (cons "Client log"
              (if lsp-ltex-plus-debug
                  "on, in *lsp-ltex-plus log*"
                "off (~lsp-ltex-plus-debug~)"))
        (cons "Wire record"
              (cond ((eql lsp-ltex-plus-events-buffer-size 0)
                     "off (~lsp-ltex-plus-events-buffer-size~)")
                    ((null lsp-ltex-plus-events-buffer-size)
                     (format "unlimited, %s, in *ltex-ls-plus events*"
                             lsp-ltex-plus-events-buffer-format))
                    (t (format "%d bytes, %s, in *ltex-ls-plus events*"
                               lsp-ltex-plus-events-buffer-size
                               lsp-ltex-plus-events-buffer-format))))
        (cons "Server log file"
              (or lsp-ltex-plus-server-log-file
                  "none (~lsp-ltex-plus-server-log-file~)"))
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
          "#+options: toc:nil\n"
          "#+startup: entitiesplain\n\n"
          "  =g=  write this report again    =r=  restart the server\n"
          "  =q=  bury this buffer           =C-c \"=  fix the mistake at point\n\n")
  (lsp-ltex-plus-doctor--insert-section "Server"
                                        (lsp-ltex-plus-doctor--server-line))
  (lsp-ltex-plus-doctor--insert-section "Connection"
                                        (lsp-ltex-plus-doctor--connection-line))
  (lsp-ltex-plus-doctor--insert-section "This buffer"
                                        (lsp-ltex-plus-doctor--buffer-line))
  (lsp-ltex-plus-doctor--insert-section "Settings"
                                        (lsp-ltex-plus-doctor--settings-line))
  (lsp-ltex-plus-doctor--insert-lists)
  (lsp-ltex-plus-doctor--insert-section "Logging"
                                        (lsp-ltex-plus-doctor--logging-line))
  (lsp-ltex-plus-doctor--insert-section "Environment"
                                        (lsp-ltex-plus-doctor--environment-line))
  (insert "* Settings for one project only\n"
          "  This report shows the global settings.  To check one project\n"
          "  with different settings -- another language, a longer\n"
          "  dictionary -- write those settings into a =.dir-locals.el=\n"
          "  file in the top directory of the project.  LTeX+ answers the\n"
          "  server from the buffer being checked, so every buffer under\n"
          "  that directory is checked with the settings of the project.\n"
          "  This report never shows the settings of a project.\n\n"))

(defun lsp-ltex-plus-doctor--make-overlay ()
  "Return a new overlay, on the heading just inserted, carrying a status."
  (let ((overlay (make-overlay (1- (point)) (point) nil t nil)))
    (overlay-put overlay 'lsp-ltex-plus-doctor t)
    overlay))

(defun lsp-ltex-plus-doctor--delete-overlays ()
  "Delete the overlays this report made, and only those.
Never `remove-overlays\=': this is an org buffer with a flymake or
flycheck front-end on it, and whatever else the user runs in org.  A
blanket sweep takes their overlays too, and a mode that keeps a list of
its own -- `org-num-mode\=' does -- then fails on the next change with a
nil position, in a backtrace naming nothing of ours."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'lsp-ltex-plus-doctor)
      (delete-overlay overlay)))
  (setq lsp-ltex-plus-doctor--overall nil))

(defun lsp-ltex-plus-doctor--ordered-samples ()
  "Return `lsp-ltex-plus-doctor-samples\=', the configured language first.
A sample for exactly `lsp-ltex-plus-language\=' leads; failing that, one
for the same language in another variant does, and it is checked under
the configured code rather than its own -- the text is the same
language, and what the user wants to see working is their setting.
With no sample for it at all, the shipped order is kept and the report
says so."
  (let* ((configured lsp-ltex-plus-language)
         (family (car (split-string configured "-")))
         (match (or (seq-find (lambda (sample) (equal (nth 0 sample) configured))
                              lsp-ltex-plus-doctor-samples)
                    (seq-find (lambda (sample)
                                (equal (car (split-string (nth 0 sample) "-"))
                                       family))
                              lsp-ltex-plus-doctor-samples))))
    (if (not match)
        lsp-ltex-plus-doctor-samples
      (cons (list configured
                  (format "%s (%s, your language)" (nth 1 match) configured)
                  (nth 2 match))
            (remq match lsp-ltex-plus-doctor-samples)))))

(defun lsp-ltex-plus-doctor--insert-sample (sample first)
  "Insert SAMPLE as a checked section and return its plist.
FIRST says this is the first one, which has to switch checking back on
after the magic comment that disabled it for the report."
  (pcase-let ((`(,language ,label ,text) sample))
    ;; `dictionary+=' per section, not once at the top: the dictionary
    ;; is kept per language, so the package's own name has to be
    ;; accepted again in each of them or the doctor flags it three
    ;; times over.
    (insert (format "# LTeX: %slanguage=%s dictionary+=LTeX\n"
                    (if first "enabled=true " "") language))
    (insert "* " label "\n")
    (let ((overlay (lsp-ltex-plus-doctor--make-overlay))
          (beg (point-marker)))
      (insert "  " text "\n")
      ;; Filled, so that the sample reads in a plain window: the doctor
      ;; buffer is not one the user came to configure line wrapping for.
      (let ((fill-column 72)
            (fill-prefix "  "))
        (fill-region beg (point)))
      (insert "\n")
      ;; Insertion type nil, both markers: the sections after this one
      ;; are inserted at exactly this point, and an end marker that
      ;; advanced with them would swallow their findings.
      (let ((end (point-marker)))
        (list :language language :label label
              :beg beg :end end :overlay overlay :found nil)))))

(defun lsp-ltex-plus-doctor--overall-status ()
  "Return what to show beside the heading of the samples."
  (cond (lsp-ltex-plus-doctor--answered
         (propertize (format "  checked in %.1f s"
                             lsp-ltex-plus-doctor--answered)
                     'face 'success))
        ((not lsp-ltex-plus-mode)
         (propertize "  LTeX+ sent nothing" 'face 'error))
        (t (propertize "  waiting for the first answer" 'face 'shadow))))

(defun lsp-ltex-plus-doctor--status (section)
  "Return the status string SECTION should be showing."
  (let ((found (plist-get section :found)))
    (cond
     (found (propertize (format "  %s" found) 'face 'success))
     ((not lsp-ltex-plus-mode)
      (propertize "  not checked: LTeX+ sent nothing.  See the Server section \
above." 'face 'error))
     ((plist-get section :timed-out)
      (propertize (format "  no answer after %d seconds: the server may have \
run out of memory while loading this language.  Raise \
~lsp-ltex-plus-java-max-heap~." lsp-ltex-plus-doctor-timeout)
                  'face 'warning))
     (t (propertize "  waiting for LTeX+ to answer.  Loading a language \
model takes a few seconds." 'face 'shadow)))))

(defun lsp-ltex-plus-doctor--show-status ()
  "Put each section\='s status on its heading.
The status is an overlay, not text: the document the server holds must
not change every time an answer arrives, or each answer would provoke
another check."
  (dolist (section lsp-ltex-plus-doctor--sections)
    (overlay-put (plist-get section :overlay)
                 'after-string (lsp-ltex-plus-doctor--status section)))
  (when lsp-ltex-plus-doctor--overall
    (overlay-put lsp-ltex-plus-doctor--overall
                 'after-string (lsp-ltex-plus-doctor--overall-status))))

(defun lsp-ltex-plus-doctor--count-in (section)
  "Return how many diagnostics fall inside SECTION\='s sample text."
  (let ((beg (plist-get section :beg))
        (end (plist-get section :end)))
    (seq-count (lambda (diagnostic)
                 (let ((region (lsp-ltex-plus--diagnostic-region diagnostic)))
                   (and (>= (car region) (marker-position beg))
                        (<= (cdr region) (marker-position end)))))
               lsp-ltex-plus--diagnostics)))

(defun lsp-ltex-plus-doctor--on-diagnostics (buffer)
  "Note what the server found for BUFFER, section by section.
On `lsp-ltex-plus--diagnostics-functions\='.  A section keeps the first
answer it got: the timing is how long that language took to arrive, and
a later publish saying the same thing must not reset it."
  (when (eq buffer (current-buffer))
    (when (and lsp-ltex-plus--diagnostics
               (not lsp-ltex-plus-doctor--answered))
      (setq lsp-ltex-plus-doctor--answered
            (- (float-time) lsp-ltex-plus-doctor--started)))
    (dolist (section lsp-ltex-plus-doctor--sections)
      (let ((count (lsp-ltex-plus-doctor--count-in section)))
        (when (and (> count 0) (not (plist-get section :found)))
          (plist-put section :found
                     (format "%d finding%s" count (if (= count 1) "" "s"))))))
    (lsp-ltex-plus-doctor--show-status)))

(defun lsp-ltex-plus-doctor--give-up (buffer)
  "Say, in BUFFER, that the sections still empty may never fill."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (dolist (section lsp-ltex-plus-doctor--sections)
        (unless (plist-get section :found)
          (plist-put section :timed-out t)))
      (lsp-ltex-plus-doctor--show-status))))

(defun lsp-ltex-plus-doctor--cancel-timer ()
  "Stop waiting for an answer that is not coming."
  (when (timerp lsp-ltex-plus-doctor--timer)
    (cancel-timer lsp-ltex-plus-doctor--timer))
  (setq lsp-ltex-plus-doctor--timer nil))

(defun lsp-ltex-plus-doctor--fill ()
  "Replace the contents of the current doctor buffer with a fresh report.
The report first, kept out of the check by a magic comment, then one
section per sample, each switching the language for what follows it."
  (let ((inhibit-read-only t)
        (samples (lsp-ltex-plus-doctor--ordered-samples)))
    (lsp-ltex-plus-doctor--cancel-timer)
    (lsp-ltex-plus-doctor--delete-overlays)
    (setq lsp-ltex-plus-doctor--sections nil)
    (erase-buffer)
    ;; Line one, and it governs everything after it: the report is not
    ;; prose and must not be checked.  See the magic comment reference,
    ;; https://ltex-plus.github.io/ltex-plus/advanced-usage.html#magic-comments
    (insert "# LTeX: enabled=false\n")
    (lsp-ltex-plus-doctor--insert-report)
    (insert "* Is it working?")
    (setq lsp-ltex-plus-doctor--overall
          (lsp-ltex-plus-doctor--make-overlay))
    (insert "\n"
            "  Every sample below contains deliberate mistakes, and each\n"
            "  sample is checked in the language named in the comment above\n"
            "  the sample.  Each heading says how many mistakes LTeX+ found.\n"
            "  LTeX+ checks the whole document in one go, so a language used\n"
            "  for the first time keeps every heading waiting while the\n"
            "  server loads a language model, which takes a few seconds.\n\n")
    (setq lsp-ltex-plus-doctor--answered nil)
    (setq lsp-ltex-plus-doctor--started (float-time))
    (let ((first t))
      (dolist (sample samples)
        (push (lsp-ltex-plus-doctor--insert-sample sample first)
              lsp-ltex-plus-doctor--sections)
        (setq first nil)))
    (setq lsp-ltex-plus-doctor--sections
          (nreverse lsp-ltex-plus-doctor--sections))
    (lsp-ltex-plus-doctor--show-status)
    (setq lsp-ltex-plus-doctor--timer
          (run-at-time lsp-ltex-plus-doctor-timeout nil
                       #'lsp-ltex-plus-doctor--give-up (current-buffer)))
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
  ;; The buffer says `#+startup: entitiesplain', which org reads when a
  ;; file is visited -- and this buffer visits none, and is written
  ;; after the mode has started.  Set the variable too, so that the
  ;; report shows the text it was given.
  (setq-local org-pretty-entities nil)
  ;; The buffer visits no file, and a user who switched file-less
  ;; checking off did not mean this buffer.  Buffer-local rather than a
  ;; binding around the call: it has to hold for every later check too,
  ;; after a restart or a mode toggle.
  (setq-local lsp-ltex-plus-check-fileless-buffers t)
  (add-hook 'lsp-ltex-plus--diagnostics-functions
            #'lsp-ltex-plus-doctor--on-diagnostics nil t)
  (add-hook 'kill-buffer-hook #'lsp-ltex-plus-doctor--cancel-timer nil t))

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
      (lsp-ltex-plus-doctor--fill)
      ;; Explicitly, not through the dispatcher: this buffer is checked
      ;; whatever set of modes the user enabled the package for.
      (unless lsp-ltex-plus-mode
        (lsp-ltex-plus-mode 1))
      ;; The mode may have declined -- no server, an old one -- and the
      ;; sections must say that rather than claim to be waiting.
      (lsp-ltex-plus-doctor--show-status)
      ;; The report was written before the handshake, so it could only
      ;; say that no server was running.  Write it again once one is,
      ;; or the version the user came here to read is the one thing
      ;; missing from it.
      (let ((connection (lsp-ltex-plus--live-connection))
            (buffer (current-buffer)))
        (when (and connection
                   (not (lsp-ltex-plus--connection-ready connection)))
          (lsp-ltex-plus--when-ready
           connection
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (lsp-ltex-plus-doctor--fill))))))))
    (pop-to-buffer buffer)))

(provide 'lsp-ltex-plus-doctor)
;;; lsp-ltex-plus-doctor.el ends here
