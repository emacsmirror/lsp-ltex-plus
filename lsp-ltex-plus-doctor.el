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
     "Are you tired of silly spellling mistakes in you're notes? LTeX+ \
finds them before your reviewer does, and this very sentance proves it.")
    ("fr-FR" "French"
     "Fatigué des fautes d'ortographe dans vos notes ? LTeX+ les trouve \
avant votre relecteur, et cette phrase, avec tout ses fautes, le prouve.")
    ("de-DE" "German"
     "Müde von dummen Rechtschreibfelern in Ihren Notizen? LTeX+ findet \
sie vor Ihrem Korrektor, und dieser Satz ist absichtlick falsch."))
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

(defvar-local lsp-ltex-plus-doctor--options-hidden t
  "Whether the setting behind each value is hidden.  Toggled by `v\='.")

(defun lsp-ltex-plus-doctor--options (&rest names)
  "Return NAMES, the settings behind a value, as text that `v\=' can hide.
Carries `lsp-ltex-plus-doctor-options\=', which the insertion turns into
an overlay: a reader wants the value, and only sometimes the name of
the thing that sets it."
  (propertize (format " (%s)"
                      (mapconcat (lambda (name) (format "~%s~" name))
                                 names ", "))
              'lsp-ltex-plus-doctor-options t))

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

(defun lsp-ltex-plus-doctor--requirement (connection version)
  "Say whether VERSION of the server on CONNECTION clears the minimum.
Coloured like the verdicts under the samples: green for a requirement
that is satisfied, red for one that is not and is enforced, amber for
one that is not and was waived, since a server kept in spite of the
warning is the user\='s decision rather than a fault."
  (let ((enforced lsp-ltex-plus-require-minimum-server-version))
    (cond
     ((not connection)
      (if enforced "required" "not required: an older server is used anyway"))
     ((lsp-ltex-plus--version-at-least-p
       version lsp-ltex-plus-minimum-server-version)
      (propertize "requirement satisfied" 'face 'success))
     (enforced
      (propertize "requirement not satisfied: the server was stopped"
                  'face 'error))
     (t (propertize "requirement not satisfied: the server is used anyway"
                    'face 'warning)))))

(defun lsp-ltex-plus-doctor--server-line ()
  "Return what is known about the server binary and the running server."
  (let* ((executable (lsp-ltex-plus--server-executable))
         (connection (lsp-ltex-plus--live-connection))
         (info (and connection
                    (lsp-ltex-plus--connection-server-info connection)))
         (version (and info (plist-get info :version))))
    (list (cons "Executable"
                (concat (if executable
                            (lsp-ltex-plus-doctor--path executable)
                          (format "not found -- =%s= is not on ~exec-path~"
                                  lsp-ltex-plus-ls-plus-executable))
                        (lsp-ltex-plus-doctor--options
                         "lsp-ltex-plus-ls-plus-executable"
                         "lsp-ltex-plus-ltex-ls-path")))
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
                (concat (format "%s -- %s"
                                lsp-ltex-plus-minimum-server-version
                                (lsp-ltex-plus-doctor--requirement
                                 connection version))
                        (lsp-ltex-plus-doctor--options
                         "lsp-ltex-plus-minimum-server-version"
                         "lsp-ltex-plus-require-minimum-server-version")))
          (cons "JAVA_HOME"
                (concat (if lsp-ltex-plus-java-path
                            (lsp-ltex-plus-doctor--path
                             (directory-file-name
                              (expand-file-name lsp-ltex-plus-java-path)))
                          "not set by Emacs")
                        (lsp-ltex-plus-doctor--options
                         "lsp-ltex-plus-java-path")))
          (cons "Java"
                (format "%s [fn:java]"
                        (if (lsp-ltex-plus-doctor--java)
                            (lsp-ltex-plus-doctor--path
                             (lsp-ltex-plus-doctor--java))
                          "none found")))
          (cons "Heap"
                (concat (let ((options (lsp-ltex-plus--heap-options)))
                          (if options
                              (string-join options " ")
                            "the JVM's own default"))
                        (lsp-ltex-plus-doctor--options
                         "lsp-ltex-plus-java-initial-heap"
                         "lsp-ltex-plus-java-max-heap"))))))

(defconst lsp-ltex-plus-doctor--documents-shown 5
  "How many open documents the report names before counting the rest.")

(defun lsp-ltex-plus-doctor--follow-buffer (name _)
  "Show the buffer called NAME.  Follows an `ltex-buffer:\=' link."
  (let ((buffer (get-buffer name)))
    (if buffer
        (pop-to-buffer-same-window buffer)
      (message "[lsp-ltex-plus] There is no buffer named %s any more" name))))

;; Org has no link type for a buffer, and `elisp:\=' asks the user to
;; confirm every time it is followed, which is worse than a plain name.
;; This one is ours, and named so.
(org-link-set-parameters "ltex-buffer"
                         :follow #'lsp-ltex-plus-doctor--follow-buffer)

(defun lsp-ltex-plus-doctor--document-link (buffer)
  "Return BUFFER as an org link, marking the report the reader is in.
A buffer with a file is linked through the file; one without -- this
report, a capture, a shell -- through `ltex-buffer:\=', the link type
above.  Built with `org-link-make-string\=', which escapes a buffer name
holding the brackets org would otherwise read as the end of the link."
  (let* ((file (buffer-file-name buffer))
         (name (buffer-name buffer))
         (link (org-link-make-string
                (concat (if file "file:" "ltex-buffer:") (or file name))
                name)))
    (if (eq buffer (current-buffer))
        (concat link " (this buffer)")
      link)))

(defun lsp-ltex-plus-doctor--documents ()
  "Return the live buffers the server is holding documents for.
Sorted by name, so that writing the report again does not shuffle the
list under the reader."
  (let (buffers)
    (maphash (lambda (_uri buffer)
               (when (buffer-live-p buffer) (push buffer buffers)))
             lsp-ltex-plus--documents)
    (sort buffers (lambda (a b) (string< (buffer-name a) (buffer-name b))))))

(defun lsp-ltex-plus-doctor--documents-value ()
  "Return the documents open on the server, counted and named.
The names, not only the count: the question a reader brings to this row
is whether the file they are writing is being checked, and a number
cannot answer it."
  (let* ((buffers (lsp-ltex-plus-doctor--documents))
         (shown (seq-take buffers lsp-ltex-plus-doctor--documents-shown))
         (rest (- (length buffers) (length shown))))
    (concat (format "%d" (length buffers))
            (mapconcat (lambda (buffer)
                         (concat "\n    - "
                                 (lsp-ltex-plus-doctor--document-link buffer)))
                       shown "")
            (if (> rest 0) (format "\n    - and %d more" rest) ""))))

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
                (lsp-ltex-plus-doctor--path
                 (and connection (lsp-ltex-plus--connection-root connection))))
          ;; Not a count until there is a server to hold them.  With no
          ;; connection the honest answer is the same "--" as the root
          ;; beside it: a plain 0 reads as "the server has nothing open",
          ;; which is a different statement from "there is no server".
          (cons "Documents open"
                (if connection
                    (lsp-ltex-plus-doctor--documents-value)
                  "--")))))

(defun lsp-ltex-plus-doctor--buffer-line ()
  "Return what is in force in the doctor buffer itself."
  (list (cons "Directory"
              (concat (lsp-ltex-plus-doctor--path
                       (abbreviate-file-name default-directory))
                      (if (dir-locals-find-file default-directory)
                          " -- its =.dir-locals.el= is in force below"
                        " -- no =.dir-locals.el= applies")))
        (cons "Language id"
              (concat (lsp-ltex-plus--language-id)
                      (lsp-ltex-plus-doctor--options
                       "lsp-ltex-plus-major-modes")))
        (cons "Front-end"
              (concat
               (cond ((null lsp-ltex-plus--attached-provider)
                      (format "none: this buffer is not being checked.  \
Your setting asks for %s" lsp-ltex-plus-diagnostics-provider))
                     ((eq lsp-ltex-plus--attached-provider
                          lsp-ltex-plus-diagnostics-provider)
                      (format "%s" lsp-ltex-plus--attached-provider))
                     (t (format "%s.  Your setting asks for %s, which this \
Emacs does not have" lsp-ltex-plus--attached-provider
                        lsp-ltex-plus-diagnostics-provider)))
               (lsp-ltex-plus-doctor--options
                "lsp-ltex-plus-diagnostics-provider")))
        (cons "Checked" (if lsp-ltex-plus-mode "yes" "no"))))

(defun lsp-ltex-plus-doctor--file-link (file)
  "Return an org link to FILE, or a note that FILE is nil.
A link even when the file is not there yet -- the link says which file
is meant and opens it, and a path written out instead would start with
the `~\=' of a home directory, which org reads as the start of inline
code and renders to the next tilde in the line."
  (if (not file)
      "not specified"
    (format "[[file:%s][%s]]%s"
            (expand-file-name file)
            (file-name-nondirectory file)
            (if (file-readable-p file) "" " (not written yet)"))))

(defun lsp-ltex-plus-doctor--path (path)
  "Return PATH as org verbatim, so that no part of PATH is read as markup."
  (if path (format "=%s=" path) "--"))

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
  (dolist (which '((:global-file . "file") (:project-file . "project file")))
    (let ((variable (lsp-ltex-plus--kind-get kind (car which))))
      (insert (format "    - %-12s :: " (cdr which)))
      (lsp-ltex-plus-doctor--insert-faced
       (concat (lsp-ltex-plus-doctor--file-link (symbol-value variable))
               (lsp-ltex-plus-doctor--options (symbol-name variable))))
      (insert "\n"))))

(defun lsp-ltex-plus-doctor--settings-line ()
  "Return the settings a check is made with."
  (list (cons "Language"
              (concat lsp-ltex-plus-language
                      (lsp-ltex-plus-doctor--options "lsp-ltex-plus-language")))
        (cons "Idle delay"
              (concat (format "%s s" lsp-ltex-plus-idle-delay)
                      (lsp-ltex-plus-doctor--options
                       "lsp-ltex-plus-idle-delay")))))

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
              (concat (if lsp-ltex-plus-debug
                          "on, in *lsp-ltex-plus log*"
                        "off")
                      (lsp-ltex-plus-doctor--options "lsp-ltex-plus-debug")))
        (cons "Wire record"
              (concat
               (cond ((eql lsp-ltex-plus-events-buffer-size 0) "off")
                     ((null lsp-ltex-plus-events-buffer-size)
                      (format "unlimited, %s, in *ltex-ls-plus events*"
                              lsp-ltex-plus-events-buffer-format))
                     (t (format "%d bytes, %s, in *ltex-ls-plus events*"
                                lsp-ltex-plus-events-buffer-size
                                lsp-ltex-plus-events-buffer-format)))
               (lsp-ltex-plus-doctor--options
                "lsp-ltex-plus-events-buffer-size"
                "lsp-ltex-plus-events-buffer-format")))
        (cons "Server log file"
              (concat (if lsp-ltex-plus-server-log-file
                          ;; Verbatim, not a link: the name may hold the
                          ;; server's `${PID}', which is no file yet.
                          (lsp-ltex-plus-doctor--path
                           lsp-ltex-plus-server-log-file)
                        "none")
                      (lsp-ltex-plus-doctor--options
                       "lsp-ltex-plus-server-log-file")))
        (cons "Server log level"
              (concat lsp-ltex-plus-ltex-ls-log-level
                      (lsp-ltex-plus-doctor--options
                       "lsp-ltex-plus-ltex-ls-log-level")))))

(defun lsp-ltex-plus-doctor--environment-line ()
  "Return the versions a bug report should carry."
  (list (cons "Emacs" emacs-version)
        (cons "jsonrpc"
              (or (lsp-ltex-plus-doctor--library-version "jsonrpc") "unknown"))
        (cons "lsp-ltex-plus"
              (or (lsp-ltex-plus-doctor--library-version "lsp-ltex-plus")
                  "unknown"))))

;;;; -- The report --------------------------------------------------------------

(defun lsp-ltex-plus-doctor--insert-faced (string)
  "Insert STRING at point, keeping the faces STRING carries.
As overlays, not as text: this buffer is fontified by org, and
font-lock removes the `face\=' property from the text it fontifies, so a
value coloured with `propertize\=' goes back to plain the moment the
window is redisplayed.  An overlay is left alone."
  (let ((start (point)))
    (insert string)
    (dolist (property '(face lsp-ltex-plus-doctor-options))
      (let ((pos 0))
        (while (< pos (length string))
          (let ((next (or (next-single-property-change pos property string)
                          (length string)))
                (value (get-text-property pos property string)))
            (when value
              (let ((overlay (make-overlay (+ start pos) (+ start next))))
                (overlay-put overlay 'lsp-ltex-plus-doctor t)
                (if (eq property 'face)
                    (overlay-put overlay 'face value)
                  (overlay-put overlay 'invisible
                               'lsp-ltex-plus-doctor-options))))
            (setq pos next)))))))

(defun lsp-ltex-plus-doctor--insert-section (title rows &optional notes)
  "Insert an org heading TITLE followed by ROWS as a description list.
NOTES are (LABEL . TEXT) org footnote definitions, written after the
rows: a caveat that has to be said once belongs under the section, not
in the middle of a line the reader is scanning."
  (insert "* " title "\n")
  (pcase-dolist (`(,label . ,value) rows)
    (insert (format "  - %-22s :: " label))
    (lsp-ltex-plus-doctor--insert-faced value)
    (insert "\n"))
  (when notes
    (insert "\n")
    ;; Column zero, which is where org looks for a definition.
    (pcase-dolist (`(,label . ,text) notes)
      (insert (format "[fn:%s] %s\n" label text))))
  (insert "\n"))

(defun lsp-ltex-plus-doctor--insert-report ()
  "Insert the report into the current buffer, at point.
Everything here is inside the region the first magic comment disables,
so none of it is offered to the server as prose."
  (insert "#+title: LTeX+ doctor\n"
          "#+options: toc:nil\n"
          "#+startup: entitiesplain\n\n"
          "  =g=  write this report again\n"
          "  =r=  restart the server\n"
          "  =v=  show the setting behind each value\n"
          "  =C-c \"=  fix the mistake at point (in the examples below)\n"
          "  =q=  bury this buffer\n\n"
          "  The report is read-only, and those letters are its keys.  The\n"
          "  examples at the end are yours: type in them, break them\n"
          "  further, and watch what comes back.\n\n")
  (lsp-ltex-plus-doctor--insert-section
   "Server" (lsp-ltex-plus-doctor--server-line)
   '(("java" . "The java Emacs resolves, from JAVA_HOME when that is set
and otherwise from ~exec-path~.  Not a guarantee: the executable above
may be a launcher script rather than the language server itself, and
such a script can export a JAVA_HOME of its own.")))
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
          "  The settings above are the ones in force for the directory this\n"
          "  report was called from.  If a project needs different settings --\n"
          "  another language, a dictionary of words that belong to it, a rule\n"
          "  switched off -- write those settings into a =.dir-locals.el= file\n"
          "  in the top directory of the project.  Most settings are read in\n"
          "  the buffer being checked ([[https://github.com/ltex-plus/emacs-\
ltex-plus/blob/main/README.md][the README]] says which), so every buffer\n"
          "  under that directory is checked with the project's settings, and\n"
          "  so is this report when you call it from there.\n\n"))

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
    (insert "** " label "\n")
    (let ((beg (point-marker)))
      (insert "  " text "\n")
      ;; Filled, so that the sample reads in a plain window: the doctor
      ;; buffer is not one the user came to configure line wrapping for.
      (let ((fill-column 72)
            (fill-prefix "  "))
        (fill-region beg (point)))
      ;; Insertion type nil on both markers: the sections after this one
      ;; are inserted at exactly this point, and an end marker that
      ;; advanced with them would swallow their findings.  The status
      ;; overlay hangs off the paragraph's last newline, so the sentence
      ;; about the paragraph is displayed under the paragraph.
      (let ((end (point-marker))
            (overlay (lsp-ltex-plus-doctor--make-overlay)))
        (insert "\n")
        (list :language language :label label
              :beg beg :end end :overlay overlay :found nil)))))

(defun lsp-ltex-plus-doctor--overall-status ()
  "Return what to show beside the heading of the samples."
  (cond (lsp-ltex-plus-doctor--answered
         (propertize (format "  checked in %.1f s"
                             lsp-ltex-plus-doctor--answered)
                     'face 'success))
        ((not lsp-ltex-plus-mode)
         (propertize "  nothing was sent to the server" 'face 'error))
        (t (propertize "  waiting for the first answer" 'face 'warning))))

(defun lsp-ltex-plus-doctor--status (section)
  "Return the line to show under SECTION\='s paragraph.
Three states, read at a glance: `success\=' for an answer that came
back, `warning\=' while an answer is still out -- which is the normal
state of a cold server for a few seconds -- and `error\=' for a check
that failed or was never sent.  Named faces, never colours, so the
user\='s theme decides what each one looks like.
A sentence below the text the sentence is about, rather than a tag
beside the heading: the reader has just read the paragraph and is
looking at the end of it.  Set off by a blank line, so that the verdict
is not taken for another line of the sample."
  (concat "\n  "
          (cond
           ((plist-get section :found)
            (propertize "Success: spelling mistakes were detected in this \
paragraph." 'face 'success))
           ((not lsp-ltex-plus-mode)
            (propertize "Not checked: nothing was sent to the server.  See \
the Server section above." 'face 'error))
           ((plist-get section :timed-out)
            (propertize (format "No answer after %d seconds: the server may \
have run out of memory while loading this language.  Raise \
~lsp-ltex-plus-java-max-heap~." lsp-ltex-plus-doctor-timeout)
                        'face 'error))
           (t (propertize "Waiting for the server to answer.  Loading a \
language model takes a few seconds." 'face 'warning)))
          "\n"))

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
          ;; What came back, not how much of it: how many mistakes
          ;; LanguageTool reports depends on the account behind the
          ;; server, so a number here would be a number to argue with.
          (plist-put section :found t))))
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

(defun lsp-ltex-plus-doctor--protect-report (end)
  "Make the report, everything before END, read-only and command-bound.
The report is generated and the next refresh throws it away, so an edit
there is a mistake; the examples after END are prose the reader is
invited to change.  The same property tells `lsp-ltex-plus-doctor-key\='
where a letter is a command and where it is a letter."
  (add-text-properties (point-min) end
                       '(read-only t lsp-ltex-plus-doctor-report t))
  ;; Without this, a character typed at the very start of the first
  ;; example would inherit the read-only property and be refused.
  (put-text-property (1- end) end 'rear-nonsticky '(read-only)))

(defun lsp-ltex-plus-doctor--project-settings ()
  "Apply the directory-local settings of `default-directory\=' here.
This buffer visits no file, so Emacs applies none by itself.  The
doctor is called from somewhere, though, and a reader who calls it
inside a project is asking about that project: its language, its
dictionary, its rules.  Read again at every refresh, so that an edited
=.dir-locals.el= takes effect on `g\='."
  (hack-dir-local-variables-non-file-buffer))

(defun lsp-ltex-plus-doctor--fill ()
  "Replace the contents of the current doctor buffer with a fresh report.
The report first, kept out of the check by a magic comment, then one
section per sample, each switching the language for what follows it."
  (let ((inhibit-read-only t)
        report-end
        samples)
    (lsp-ltex-plus-doctor--project-settings)
    (setq samples (lsp-ltex-plus-doctor--ordered-samples))
    (lsp-ltex-plus-doctor--cancel-timer)
    (lsp-ltex-plus-doctor--delete-overlays)
    (setq lsp-ltex-plus-doctor--sections nil)
    (erase-buffer)
    ;; Line one, and it governs everything after it: the report is not
    ;; prose and must not be checked.  See the magic comment reference,
    ;; https://ltex-plus.github.io/ltex-plus/advanced-usage.html#magic-comments
    (insert "# LTeX: enabled=false\n")
    (lsp-ltex-plus-doctor--insert-report)
    (insert "* Examples")
    (setq lsp-ltex-plus-doctor--overall
          (lsp-ltex-plus-doctor--make-overlay))
    (insert "\n"
            "  Three paragraphs follow, each one containing grammatical and\n"
            "  spelling mistakes on purpose, and each checked in its own\n"
            "  language.  The line above each paragraph is a\n"
            "  [[https://ltex-plus.github.io/ltex-plus/advanced-usage.html\
#magic-comments][magic comment]]: it sets the language for the text below\n"
            "  it, and adds the word LTeX to the dictionary of that language\n"
            "  so that the word is not underlined as a misspelling.  A\n"
            "  dictionary is kept per language, which is why the word is added\n"
            "  again in each comment.  Both settings are worth copying into\n"
            "  documents of your own.\n\n"
            "  The server checks the whole buffer in one go, so a language\n"
            "  used for the first time keeps every paragraph waiting while a\n"
            "  language model loads, which takes a few seconds.\n\n")
    (setq lsp-ltex-plus-doctor--answered nil)
    (setq lsp-ltex-plus-doctor--started (float-time))
    (setq report-end (point-marker))
    (let ((first t))
      (dolist (sample samples)
        (push (lsp-ltex-plus-doctor--insert-sample sample first)
              lsp-ltex-plus-doctor--sections)
        (setq first nil)))
    (setq lsp-ltex-plus-doctor--sections
          (nreverse lsp-ltex-plus-doctor--sections))
    (lsp-ltex-plus-doctor--protect-report report-end)
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

(defun lsp-ltex-plus-doctor-toggle-options ()
  "Show or hide the setting that stands behind each value."
  (interactive)
  (setq lsp-ltex-plus-doctor--options-hidden
        (not lsp-ltex-plus-doctor--options-hidden))
  (if lsp-ltex-plus-doctor--options-hidden
      (add-to-invisibility-spec 'lsp-ltex-plus-doctor-options)
    (remove-from-invisibility-spec 'lsp-ltex-plus-doctor-options))
  (message "[lsp-ltex-plus] %s"
           (if lsp-ltex-plus-doctor--options-hidden
               "Settings hidden; press v to see which setting holds a value"
             "Each value now names the setting behind it")))

(defconst lsp-ltex-plus-doctor--keys
  '((?g . lsp-ltex-plus-doctor-refresh)
    (?r . lsp-ltex-plus-doctor-restart-server)
    (?v . lsp-ltex-plus-doctor-toggle-options)
    (?q . bury-buffer))
  "What each letter does while point is in the report.")

(defun lsp-ltex-plus-doctor-key ()
  "Act on the report, or type the key in the examples.
The examples are prose to try the checker on -- edit one, watch the
underlines follow, fix it with `lsp-ltex-plus-actions' -- so a letter
typed there has to arrive as a letter.  The report above is written by
this package and replaced whole by the next refresh, so a letter there
is free to mean something."
  (interactive)
  (let ((command (and (get-text-property (point) 'lsp-ltex-plus-doctor-report)
                      (alist-get last-command-event
                                 lsp-ltex-plus-doctor--keys))))
    (if command
        (call-interactively command)
      (call-interactively #'self-insert-command))))

(defvar-keymap lsp-ltex-plus-doctor-mode-map
  :doc "Keymap for `lsp-ltex-plus-doctor-mode'."
  "g" #'lsp-ltex-plus-doctor-key
  "r" #'lsp-ltex-plus-doctor-key
  "v" #'lsp-ltex-plus-doctor-key
  "q" #'lsp-ltex-plus-doctor-key)

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
  ;; The settings behind the values are there to be asked for, not read
  ;; every time; `v' asks.
  (add-to-invisibility-spec 'lsp-ltex-plus-doctor-options)
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
      ;; The report was written before this buffer was opened on the
      ;; server -- and, with no server running, before there was one to
      ;; report.  Write it again once the document is open: the queue is
      ;; first in, first out, so the `didOpen' above has gone by then,
      ;; and the report counts itself.
      (let ((connection (lsp-ltex-plus--live-connection))
            (buffer (current-buffer)))
        (when connection
          (lsp-ltex-plus--when-ready
           connection
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (lsp-ltex-plus-doctor--fill))))))))
    ;; In the window the command was called from: the report is the
    ;; thing to read now, and a reader who wants it beside their work
    ;; can say so in `display-buffer-alist', which this still obeys.
    (pop-to-buffer-same-window buffer)))

(provide 'lsp-ltex-plus-doctor)
;;; lsp-ltex-plus-doctor.el ends here
