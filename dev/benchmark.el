;;; benchmark.el --- Time the round trip to ltex-ls-plus -*- lexical-binding: t; -*-

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; The numbers the README's Performance section quotes, and how to get
;; them again.  0.6.0 had `lsp-ltex-plus-show-latency' for this; 1.0.0
;; retired it, and for one release the README quoted figures nobody
;; could reproduce.  This file is the replacement, kept out of the
;; package because it is a maintainer's instrument.
;;
;; What is measured is one thing only: `textDocument/didChange' out,
;; `textDocument/publishDiagnostics' back.  `lsp-ltex-plus-idle-delay'
;; is deliberately *not* in it -- every send here is made by hand, with
;; the debounce parked far enough out that it never fires on its own --
;; because the delay is a number the user chooses and the round trip is
;; the one the server decides.  What a writer waits for is the sum.
;;
;; Two documents, after the two the README talks about: a page of Org
;; prose and a 15 KB LaTeX document.  Each is opened (the cold check),
;; then edited and re-sent seven times (the warm ones).  Every edit adds
;; a sentence carrying the run number, so the paragraph it lands in is
;; text the server has never seen and neither its paragraph cache nor
;; LanguageTool's sentence cache can answer for it.  Without that the
;; measurement is worthless: re-sending the page of Org prose unchanged
;; comes back in 11 ms against 35, which is the cache being timed and
;; not the check.  The same comparison on the LaTeX document is 60 ms
;; against 70-85, and the difference between those two is the honest
;; reading of where a large document's time goes -- almost all of it is
;; the whole text being sent and parsed again, not the paragraph that
;; changed.
;;
;; Two ways to run it, and they measure different things:
;;
;;   make bench
;;       A batch Emacs, against whatever `ltex-ls-plus' is on PATH with
;;       stock settings -- the bundled LanguageTool, offline.  This is
;;       where the README's local numbers come from.
;;
;;   emacsclient -e '(progn (load "…/dev/benchmark.el") \
;;                          (lsp-ltex-plus-benchmark))'
;;       Your own running Emacs, with your own configuration: a remote
;;       LanguageTool server, an API key, a language, whatever you have
;;       set.  This is the only way to get the remote figure, since the
;;       credentials for it are not in this repository.  It blocks that
;;       Emacs for as long as it runs -- a minute or so against the
;;       hosted service -- and leaves the session as it found it: the
;;       benchmark's buffers are killed, and a server that was already
;;       running stays running.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'lsp-ltex-plus)

(defvar lsp-ltex-plus-benchmark-runs 7
  "How many warm round trips to time per document.")

(defvar lsp-ltex-plus-benchmark-timeout 120
  "Seconds to wait for one answer before giving up on it.")

;;;; -- The documents -----------------------------------------------------------

;; Prose with mistakes in it, so that the server has findings to report
;; and the measurement covers the work a real check does.

(defconst lsp-ltex-plus-benchmark--paragraphs
  '("The committee has discussed the proposal at length, and it have decided to postpone a final vote until the next meeting. Several members argued that the figures presented in the annex was incomplete, while others felt that the timetable were too ambitious for a department of this size."
    "In the second part of the report we turn to the question of funding. Their are three sources under consideration, each with its own conditions, and the choice between them will effect the schedule of the whole project. The first is the least generous but by far the simplest to administer."
    "A short note on terminology. Throughout this document the word instrument refers to the apparatus itself and not to the software that drives it; where the distinction matters we say so explicitly. Readers who are familiar with the earlier reports will recognise most of the vocabulary used here."
    "Finally, we record our thanks to the technical staff, whose patience during the long series of calibration runs was remarkable. Without their help the measurements reported in section four could not of been completed before the deadline, and the conclusions would have rested on a much thinner basis.")
  "Paragraphs of English prose, each with a few mistakes in it.")

(defun lsp-ltex-plus-benchmark--prose (target)
  "Return at least TARGET characters of prose, in paragraphs."
  (let ((text "") (n 0))
    (while (< (length text) target)
      (setq text (concat text
                         (nth (mod n (length lsp-ltex-plus-benchmark--paragraphs))
                              lsp-ltex-plus-benchmark--paragraphs)
                         "\n\n"))
      (cl-incf n))
    text))

(defun lsp-ltex-plus-benchmark--org-document ()
  "Return a page of Org prose."
  (concat "#+title: A page of prose\n\n* Introduction\n\n"
          (lsp-ltex-plus-benchmark--prose 2600)))

(defun lsp-ltex-plus-benchmark--latex-document ()
  "Return a LaTeX document of about 15 KB."
  (let ((body "") (section 0) (prose (lsp-ltex-plus-benchmark--prose 1200)))
    (while (< (length body) 14000)
      (setq body (concat body
                         (format "\\section{Section %d}\n\n" (cl-incf section))
                         prose)))
    (concat "\\documentclass{article}\n\\usepackage[utf8]{inputenc}\n"
            "\\title{A longer document}\n\\begin{document}\n\\maketitle\n\n"
            body "\n\\end{document}\n")))

;;;; -- Waiting for the server --------------------------------------------------

(defvar-local lsp-ltex-plus-benchmark--publishes 0
  "How many times the server has published for this buffer.")

(defun lsp-ltex-plus-benchmark--note (buffer)
  "Record that the server published for BUFFER.
On `lsp-ltex-plus--diagnostics-functions', which also runs when a buffer
leaves the server and its diagnostics are cleared; that is not a
publish, and the document is closed by then."
  (when (and (buffer-live-p buffer) (lsp-ltex-plus--document-open-p buffer))
    (with-current-buffer buffer (cl-incf lsp-ltex-plus-benchmark--publishes))))

(defun lsp-ltex-plus-benchmark--time (buffer thunk what)
  "Call THUNK and return the seconds until the server publishes for BUFFER.
WHAT names the step in the error signalled if it never answers.  Pumps
`accept-process-output', so a batch Emacs runs its filters and timers
and a running one stays responsive to the process."
  (let ((before (buffer-local-value 'lsp-ltex-plus-benchmark--publishes buffer))
        (started (float-time)))
    (funcall thunk)
    (while (and (= before (buffer-local-value 'lsp-ltex-plus-benchmark--publishes
                                              buffer))
                (< (- (float-time) started) lsp-ltex-plus-benchmark-timeout))
      (accept-process-output nil 0.01))
    (when (= before (buffer-local-value 'lsp-ltex-plus-benchmark--publishes buffer))
      (error "No answer for %s after %d s" what lsp-ltex-plus-benchmark-timeout))
    (- (float-time) started)))

;;;; -- One document ------------------------------------------------------------

(defun lsp-ltex-plus-benchmark--open (name mode text)
  "Open a buffer NAME on TEXT in MODE and return it, checked once.
A buffer visiting no file, so that nothing is written anywhere and the
measurement does not depend on a temporary directory."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert text)
      (goto-char (point-min))
      (funcall mode))
    buffer))

(defun lsp-ltex-plus-benchmark--edit (buffer counter)
  "Add a sentence numbered COUNTER to the last paragraph of BUFFER.
A different document every time, so that the server's paragraph cache
cannot answer with what it found before.  The sentence is appended to a
paragraph that is already there, rather than set apart as one of its
own: what a writer does is add to the paragraph being written, which
dirties a paragraph the server had cached.  A new paragraph between
blank lines would leave every existing one cached and measure a lighter
check than typing produces."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (while (and (not (bobp))
                  (progn (forward-line -1)
                         (let ((line (buffer-substring (line-beginning-position)
                                                       (line-end-position))))
                           (or (string-blank-p line)
                               ;; Markup, not prose: \section{...},
                               ;; \end{document}, an Org heading or keyword.
                               (string-match-p "\\`[\\\\#*]" line))))))
      (end-of-line)
      (insert (format " Run number %d of the measurement." counter)))))

(defun lsp-ltex-plus-benchmark--document (label mode text)
  "Time the checks of a document LABEL, in MODE, holding TEXT.
Returns a plist of the label, the size, the cold check and the warm
ones, all times in seconds."
  (let ((buffer (lsp-ltex-plus-benchmark--open (format "*bench %s*" label)
                                               mode text))
        cold warm)
    (unwind-protect
        (progn
          (setq cold (lsp-ltex-plus-benchmark--time
                      buffer
                      (lambda ()
                        (with-current-buffer buffer
                          (let ((inhibit-message t)) (lsp-ltex-plus-mode 1))))
                      (format "the first check of the %s document" label)))
          (dotimes (run lsp-ltex-plus-benchmark-runs)
            ;; The edit schedules a send one delay out; make it now, so
            ;; that what is timed starts at the notification.
            (lsp-ltex-plus-benchmark--edit buffer (1+ run))
            (push (lsp-ltex-plus-benchmark--time
                   buffer
                   (lambda () (lsp-ltex-plus--send-changes buffer))
                   (format "a re-check of the %s document" label))
                  warm))
          (list :label label
                :size (buffer-size buffer)
                :cold cold
                :warm (nreverse warm)))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

;;;; -- The report --------------------------------------------------------------

(defun lsp-ltex-plus-benchmark--median (numbers)
  "Return the median of NUMBERS."
  (let ((sorted (sort (copy-sequence numbers) #'<)))
    (nth (/ (length sorted) 2) sorted)))

(defun lsp-ltex-plus-benchmark--line (result)
  "Return one line of report for RESULT."
  (format "%-22s %6d chars   cold %7.0f ms   warm median %6.0f ms   (%s)"
          (plist-get result :label)
          (plist-get result :size)
          (* 1000 (plist-get result :cold))
          (* 1000 (lsp-ltex-plus-benchmark--median (plist-get result :warm)))
          (mapconcat (lambda (seconds) (format "%.0f" (* 1000 seconds)))
                     (plist-get result :warm) " ")))

(defun lsp-ltex-plus-benchmark--backend ()
  "Say which LanguageTool the numbers were taken against."
  (if lsp-ltex-plus-lt-server-uri
      (format "%s%s" lsp-ltex-plus-lt-server-uri
              (if lsp-ltex-plus-lt-api-key " (with an API key)" ""))
    "local, the LanguageTool bundled with the server"))

;;;###autoload
(defun lsp-ltex-plus-benchmark ()
  "Time the server's answers and return the report as a string.
Interactively, show it in a buffer.  Leaves the session as it found it:
the documents it opens are killed, and a server that was already running
is left running.

Beware what it does not measure: `lsp-ltex-plus-idle-delay' seconds pass
before a real edit is sent at all.  See the commentary."
  (interactive)
  (add-hook 'lsp-ltex-plus--diagnostics-functions #'lsp-ltex-plus-benchmark--note)
  (let ((report
         (unwind-protect
             ;; Far enough out that no timer fires on its own; every
             ;; send below is made by hand.
             (let ((lsp-ltex-plus-idle-delay 3600))
               (concat
                (format "ltex-ls-plus via %s\nEmacs %s on %s\nLanguageTool: %s\n\n"
                        (or (lsp-ltex-plus--server-executable) "nothing found")
                        emacs-version system-configuration
                        (lsp-ltex-plus-benchmark--backend))
                (mapconcat
                 #'lsp-ltex-plus-benchmark--line
                 (list (lsp-ltex-plus-benchmark--document
                        "Org, one page" #'org-mode
                        (lsp-ltex-plus-benchmark--org-document))
                       (lsp-ltex-plus-benchmark--document
                        "LaTeX, ~15 KB" #'latex-mode
                        (lsp-ltex-plus-benchmark--latex-document)))
                 "\n")
                "\n"))
           (remove-hook 'lsp-ltex-plus--diagnostics-functions
                        #'lsp-ltex-plus-benchmark--note))))
    (if (called-interactively-p 'interactive)
        (with-current-buffer (get-buffer-create "*lsp-ltex-plus benchmark*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert report))
          (display-buffer (current-buffer)))
      report)))

(defun lsp-ltex-plus-benchmark-batch ()
  "Print the report on stdout, for `make bench'.
Shuts the server down afterwards: a batch Emacs that leaves a JVM behind
never exits."
  (princ (lsp-ltex-plus-benchmark))
  (when (lsp-ltex-plus--live-connection)
    (lsp-ltex-plus--shutdown-connection)))

(provide 'lsp-ltex-plus-benchmark)
;;; benchmark.el ends here
