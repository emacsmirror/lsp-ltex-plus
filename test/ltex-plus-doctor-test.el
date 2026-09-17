;;; ltex-plus-doctor-test.el --- The doctor buffer -*- lexical-binding: t; -*-

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; The doctor is a buffer the package checks like any other, so most of
;; what it does is already covered elsewhere.  What is asserted here is
;; what is particular to it: that the report is disabled and the samples
;; are not, that a finding is counted against the section it lands in
;; and no other, and -- the point of the whole design -- that a section
;; with nothing in it never reads as a section with nothing wrong.
;;
;; The real server is not needed for any of that; the one test that
;; wants a document opened uses the fake.

;;; Code:

(require 'ltex-plus-test-helper)
(require 'ltex-plus-fake-server)
(require 'lsp-ltex-plus-doctor)

(defmacro ltex-plus-doctor-test--with-report (&rest body)
  "Run BODY in a filled doctor buffer, with no server and no checking."
  (declare (indent 0) (debug t))
  `(let ((buffer (generate-new-buffer "*ltex-plus-doctor-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (lsp-ltex-plus-doctor-mode)
           (lsp-ltex-plus-doctor--fill)
           ,@body)
       (with-current-buffer buffer
         (lsp-ltex-plus-doctor--cancel-timer))
       (kill-buffer buffer))))

(defun ltex-plus-doctor-test--pretend-checked ()
  "Say the buffer is being checked, without a server to check it.
The status of a section depends on it: with the mode off the honest
answer is that nothing was sent, which is a different test."
  (setq-local lsp-ltex-plus-mode t)
  (lsp-ltex-plus-doctor--show-status))

(defun ltex-plus-doctor-test--status-string (label)
  "Return the status of the section called LABEL, faces and all."
  (let ((section (seq-find (lambda (s) (equal (plist-get s :label) label))
                           lsp-ltex-plus-doctor--sections)))
    (or (overlay-get (plist-get section :overlay) 'after-string) "")))

(defun ltex-plus-doctor-test--status (label)
  "Return the status shown for the section called LABEL."
  (let ((section (seq-find (lambda (s) (equal (plist-get s :label) label))
                           lsp-ltex-plus-doctor--sections)))
    (substring-no-properties
     (or (overlay-get (plist-get section :overlay) 'after-string) ""))))

(defun ltex-plus-doctor-test--diagnostic-on (text)
  "Return a diagnostic covering the first occurrence of TEXT in the buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (list :message (format "%s is wrong" text)
          :severity 1
          :range (list :start (lsp-ltex-plus--point-to-position (match-beginning 0))
                       :end (lsp-ltex-plus--point-to-position (match-end 0))))))

;;;; -- What the server is and is not asked to check ----------------------------

(ert-deftest ltex-plus-doctor-test-the-report-is-not-checked ()
  "The report sits under a magic comment that disables checking.
It is paths, symbols and version strings; offering it as prose would
flag the package's own name and teach the user nothing."
  (ltex-plus-doctor-test--with-report
    (goto-char (point-min))
    (should (looking-at-p "# LTeX: enabled=false$"))
    ;; And exactly one comment turns it back on, on the first sample.
    (should (= 1 (count-matches "^# LTeX: enabled=true")))
    (should (< (save-excursion (goto-char (point-min))
                               (search-forward "* Environment"))
               (save-excursion (goto-char (point-min))
                               (search-forward "# LTeX: enabled=true"))))))

(ert-deftest ltex-plus-doctor-test-each-sample-names-its-language ()
  "Every sample is preceded by a magic comment naming its language."
  (ltex-plus-doctor-test--with-report
    (dolist (section lsp-ltex-plus-doctor--sections)
      (goto-char (point-min))
      (should (search-forward (format "language=%s" (plist-get section :language))
                              nil t)))))

(ert-deftest ltex-plus-doctor-test-the-configured-language-comes-first ()
  "The language the user is configured for leads, and says so.
Its model is the one already loaded, so it is the section that can
answer first; and it is the one the user actually wants to see work."
  (let ((lsp-ltex-plus-language "de-DE"))
    (ltex-plus-doctor-test--with-report
      (let ((first (car lsp-ltex-plus-doctor--sections)))
        (should (equal (plist-get first :language) "de-DE"))
        (should (string-match-p "your language" (plist-get first :label))))))
  ;; A variant of the same language is close enough to lead, and is
  ;; checked under the code the user set rather than the sample's.
  (let ((lsp-ltex-plus-language "en-GB"))
    (ltex-plus-doctor-test--with-report
      (should (equal (plist-get (car lsp-ltex-plus-doctor--sections) :language)
                     "en-GB"))))
  ;; With no sample for it, the shipped order stands.
  (let ((lsp-ltex-plus-language "nl-NL"))
    (ltex-plus-doctor-test--with-report
      (should (equal (plist-get (car lsp-ltex-plus-doctor--sections) :language)
                     (car (car lsp-ltex-plus-doctor-samples)))))))

;;;; -- Reporting what came back ------------------------------------------------

(ert-deftest ltex-plus-doctor-test-a-finding-counts-for-its-own-section ()
  "A finding is counted against the section it falls in, and no other.
The sections are inserted one after another, so an end marker that
moved with later insertions would let the first section swallow every
finding in the buffer."
  (ltex-plus-doctor-test--with-report
    (ltex-plus-doctor-test--pretend-checked)
    (setq lsp-ltex-plus--diagnostics
          (list (ltex-plus-doctor-test--diagnostic-on "spellling")
                (ltex-plus-doctor-test--diagnostic-on "Rechtschreibfelern")))
    (lsp-ltex-plus-doctor--on-diagnostics (current-buffer))
    (should (equal (ltex-plus-doctor-test--status "English (en-US, your language)")
                   "\n  Success: spelling mistakes were detected in this \
paragraph.\n"))
    (should (string-match-p "Success" (ltex-plus-doctor-test--status "German")))
    (should (string-match-p "waiting"
                            (ltex-plus-doctor-test--status "French")))))

(ert-deftest ltex-plus-doctor-test-nothing-yet-never-reads-as-nothing-wrong ()
  "A section with no findings says why, and never that it is clean.
Every sample is wrong on purpose, so an empty section means the answer
has not come -- or is not coming.  Saying nothing would read as a pass
and send the user looking for a problem that is not there."
  (ltex-plus-doctor-test--with-report
    (ltex-plus-doctor-test--pretend-checked)
    (should (string-match-p "Waiting" (ltex-plus-doctor-test--status "German")))
    (lsp-ltex-plus-doctor--give-up (current-buffer))
    (let ((status (ltex-plus-doctor-test--status "German")))
      (should (string-match-p "No answer" status))
      (should (string-match-p "lsp-ltex-plus-java-max-heap" status)))))

(ert-deftest ltex-plus-doctor-test-an-unchecked-buffer-says-so ()
  "With the mode off, the sections say nothing was sent.
The mode declines when no server can be found; a buffer that then said
it was waiting would be waiting for something nobody sent."
  (ltex-plus-doctor-test--with-report
    (should-not lsp-ltex-plus-mode)
    (should (string-match-p "LTeX\\+ sent nothing"
                            (substring-no-properties
                             (overlay-get lsp-ltex-plus-doctor--overall
                                          'after-string))))
    (should (string-match-p "Not checked"
                            (ltex-plus-doctor-test--status "French")))))

(ert-deftest ltex-plus-doctor-test-the-timing-is-for-the-whole-check ()
  "One timing, on the samples' heading, not one per section.
The server checks the whole document and publishes once, so a
per-section time would be the same number repeated.  No count anywhere:
how many mistakes LanguageTool reports depends on the account behind
the server."
  (ltex-plus-doctor-test--with-report
    (ltex-plus-doctor-test--pretend-checked)
    (setq lsp-ltex-plus--diagnostics
          (list (ltex-plus-doctor-test--diagnostic-on "spellling")))
    (lsp-ltex-plus-doctor--on-diagnostics (current-buffer))
    (should (string-match-p
             "checked in [0-9.]+ s"
             (substring-no-properties
              (overlay-get lsp-ltex-plus-doctor--overall 'after-string))))
    (should (string-match-p "Success" (ltex-plus-doctor-test--status
                                      "English (en-US, your language)")))))

(ert-deftest ltex-plus-doctor-test-a-refresh-leaves-other-overlays-alone ()
  "Writing the report again deletes the doctor's overlays and no others.
The buffer is an org buffer with a diagnostics front-end on it and
whatever else the user runs in org.  `org-num-mode' stands for all of
them here: it keeps a list of the overlays it made, and a blanket
`remove-overlays' leaves that list full of dead ones, after which the
next change to the buffer fails with a nil position -- in a backtrace
that names nothing of ours."
  (require 'org-num)
  (ltex-plus-doctor-test--with-report
    (org-num-mode 1)
    (should (> (length org-num--overlays) 0))
    (let ((theirs (length org-num--overlays)))
      (lsp-ltex-plus-doctor--fill)
      (should (= theirs (seq-count #'overlay-start
                                   (append org-num--overlays nil))))
      ;; And ours are replaced, not accumulated: one per sample and one
      ;; for the check as a whole.
      (should (= (1+ (length lsp-ltex-plus-doctor--sections))
                 (seq-count (lambda (overlay)
                              (overlay-get overlay 'lsp-ltex-plus-doctor))
                            (overlays-in (point-min) (point-max))))))))

(ert-deftest ltex-plus-doctor-test-the-states-are-a-traffic-light ()
  "Answered is `success\=', still waiting is `warning\=', failed is `error\='.
Named faces, so a theme decides the colours; what is pinned here is
which of the three a state belongs to -- a failed check must not be
painted the same as one that is merely slow."
  (cl-flet ((face-of (label)
              ;; The status opens with an unpropertized blank line, so
              ;; the face starts where the sentence does.
              (let ((status (ltex-plus-doctor-test--status-string label)))
                (get-text-property
                 (or (next-single-property-change 0 'face status) 0)
                 'face status))))
    (ltex-plus-doctor-test--with-report
      ;; Never sent: red.
      (should (eq (face-of "German") 'error))
      (ltex-plus-doctor-test--pretend-checked)
      ;; Sent, no answer yet: amber, and the normal state of a cold
      ;; server for a few seconds.
      (should (eq (face-of "German") 'warning))
      ;; Given up on: red, because the heap it names needs raising.
      (lsp-ltex-plus-doctor--give-up (current-buffer))
      (should (eq (face-of "German") 'error))
      ;; Answered: green.
      (setq lsp-ltex-plus--diagnostics
            (list (ltex-plus-doctor-test--diagnostic-on "Rechtschreibfelern")))
      (lsp-ltex-plus-doctor--on-diagnostics (current-buffer))
      (should (eq (face-of "German") 'success)))))

;;;; -- On a server -------------------------------------------------------------

(ert-deftest ltex-plus-doctor-test-the-document-is-opened-as-org ()
  "The doctor buffer is opened on the server, once, as an org document.
The mode derives from `org-mode' and is not in the mode table, so this
is also the end-to-end check that the id is inherited rather than
registered as plain text."
  (ltex-plus-fake-with-connection
    (let ((inhibit-message t)
          (lsp-ltex-plus-idle-delay 0.1)
          (table (copy-sequence lsp-ltex-plus-major-modes)))
      (unwind-protect
          (progn
            (lsp-ltex-plus-doctor)
            (ltex-plus-fake-wait-for
             (lambda () (ltex-plus-fake-received 'textDocument/didOpen)))
            (let ((params (car (ltex-plus-fake-received 'textDocument/didOpen))))
              (should (equal (plist-get (plist-get params :textDocument) :languageId)
                             "org")))
            (should (equal table lsp-ltex-plus-major-modes))
            (with-current-buffer lsp-ltex-plus-doctor-buffer-name
              (should lsp-ltex-plus-mode)
              (should lsp-ltex-plus-check-fileless-buffers)
              (should (local-variable-p 'lsp-ltex-plus-check-fileless-buffers))
              ;; Writing the report again is an edit, not a second
              ;; document: the server is told the text changed.
              (lsp-ltex-plus-doctor-refresh)
              (should (= 1 (hash-table-count lsp-ltex-plus--documents)))))
        (when (get-buffer lsp-ltex-plus-doctor-buffer-name)
          (kill-buffer lsp-ltex-plus-doctor-buffer-name))))))

(provide 'ltex-plus-doctor-test)
;;; ltex-plus-doctor-test.el ends here
