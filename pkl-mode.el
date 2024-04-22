;;; pkl-mode.el --- Major mode for editing Pkl files -*- lexical-binding: t; -*-

;; Copyright (C) 2024 sin-ack
;; Author: sin-ack <sin-ack@protonmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages, pkl
;; URL: https://github.com/sin-ack/pkl-mode
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing Pkl files. Pkl is a configuration language
;; made by Apple: https://pkl-lang.org/

;;; Code:

(eval-when-compile (require 'rx))
(require 'font-lock)

;; Customization

(defgroup pkl nil
  "Major mode for editing Pkl files."
  :group 'languages
  :prefix "pkl-")

(defcustom pkl-enable-copilot :auto
  "Whether to enable Copilot integration in Pkl mode.
The integration requires copilot.el to be installed.

If set to `:auto', the integration will be enabled if copilot.el is
available, and silently disabled otherwise."
  :type '(choice (const :tag "Yes" t)
          (const :tag "No" nil)
          (const :tag "Auto" :auto))
  :group 'pkl)

;; Internal variables

(defvar pkl--feature-copilot nil
  "Whether `copilot-mode' is available.")

;; Optional features

(defmacro pkl--optionally-enable (feature flag &rest body)
  "Enable FEATURE if FLAG is t, and execute BODY."
  (declare (indent 1))
  `(cond ((eq ,feature :auto) (when ,flag ,@body))
         ((eq ,feature t)
          (if ,flag ,@body
            (warn "%s is t, but the relevant package is not available.  Please install it or change the option." ',feature)))))

(when (require 'copilot nil t) (setq pkl--feature-copilot t))
;; Variables defined by copilot.el
(defvaralias 'pkl--copilot-indentation-alist 'copilot-indentation-alist)

(defun pkl--integrate-copilot ()
  "Integrate Copilot with Pkl mode."
  (add-to-list 'pkl--copilot-indentation-alist '(pkl-mode tab-width)))

;; Indentation

(defun pkl-indent-line ()
  "Indent current line as Pkl code."
  (interactive)
  (let ((start-of-current-line (save-excursion (beginning-of-line) (point)))
        (level 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) start-of-current-line)
        (cond ((looking-at "\\((\\|\\[\\|{\\)") (setq level (+ level 1)))
              ((looking-at "\\()\\|\\]\\|}\\)") (setq level (- level 1))))
        (forward-char))
      ;; Handle the case of current line ending with a closing paren
      (goto-char start-of-current-line)
      (if (looking-at "^\\s-*\\()\\|\\]\\|}\\)")
          (setq level (- level 1))))
    (if (< level 0) (setq level 0))
    (indent-line-to (* level tab-width))))

;; Syntax highlighting via font-lock

(defconst pkl--identifier-rx
  (rx (any alpha ?$ ?_) (0+ (or (syntax word) ?$ ?_))))
(defconst pkl--single-identifier-rx
  (rx symbol-start (regexp pkl--identifier-rx) symbol-end))
(defconst pkl--type-name-rx
  (rx symbol-start upper (0+ (or (syntax word) ?_)) symbol-end))

(defconst pkl--binary-integer-rx
  (rx "0b" (any "01") (0+ (? ?_) (any "01"))))
(defconst pkl--octal-integer-rx
  (rx "0o" (any "0-7") (0+ (? ?_) (any "0-7"))))
(defconst pkl--hex-integer-rx
  (rx "0x" hex-digit (0+ (? ?_) hex-digit)))
(defconst pkl--decimal-integer-rx
  (rx digit (0+ (? ?_) digit)))
(defconst pkl--integer-rx
  (rx symbol-start
      (? (any "+-"))
      (or (regexp pkl--binary-integer-rx)
          (regexp pkl--octal-integer-rx)
          (regexp pkl--hex-integer-rx)
          (regexp pkl--decimal-integer-rx)))
  symbol-end)

(defconst pkl--float-rx
  (rx symbol-start
      (? (any "+-"))
      (? (regexp pkl--decimal-integer-rx)) ?.
      (regexp pkl--decimal-integer-rx)
      (? (any "eE") (any "+-") (regexp pkl--decimal-integer-rx))
      symbol-end))

(defconst pkl--doc-comment-rx
  (rx "///" (0+ (not (any "\n")))))

(defconst pkl--declaration-keyword-rx
  (rx (or "local" "hidden" "fixed" "const")))
(defconst pkl--assignment-rx
  (rx (? (regexp pkl--declaration-keyword-rx) (0+ (syntax whitespace)))
      (group (regexp pkl--single-identifier-rx))
      (0+ (syntax whitespace)) ?=))
(defconst pkl--object-assignment-rx
  (rx line-start (0+ (syntax whitespace))
      (? (regexp pkl--declaration-keyword-rx) (0+ (syntax whitespace)))
      (group (regexp pkl--single-identifier-rx))
      (0+ (syntax whitespace)) ?{))
(defconst pkl--type-annotation-rx
  (rx (group (regexp pkl--single-identifier-rx))
      (0+ (syntax whitespace)) ?:))
(defconst pkl--function-call-rx
  (rx (group (regexp pkl--single-identifier-rx)) ?\())
(defconst pkl--member-access-rx
  (rx ?. (group (regexp pkl--single-identifier-rx))))
(defconst pkl--module-definition-rx
  (rx line-start (0+ (syntax whitespace))
      "module" (1+ (syntax whitespace))
      (group (regexp pkl--identifier-rx)
             (0+ ?. (regexp pkl--identifier-rx)))))
(defconst pkl--import-call-rx
  (rx symbol-start
      (group "import" (? ?*)) (0+ (syntax whitespace)) ?\())
(defconst pkl--throw-call-rx
  (rx symbol-start
      (group "throw") (0+ (syntax whitespace)) ?\())
(defconst pkl--trace-call-rx
  (rx symbol-start
      (group "trace") (0+ (syntax whitespace)) ?\())
(defconst pkl--read-call-rx
  (rx symbol-start
      (group "read" (? (any "?*"))) (0+ (syntax whitespace)) ?\())

(defconst pkl--annotation-rx
  (rx ?@ (regexp pkl--single-identifier-rx)))

(defconst pkl--literal-rx
  (rx (or "NaN" "Infinity" "true" "false" "null")))

(defconst pkl--predefined-type-rx
  (rx (or "nothing" "unknown")))

(defconst pkl--keyword-rx
  (rx symbol-start
      (or "function" "local" "open class" "class" "hidden" "abstract"
          "extends" "new" "module" "amends" "import" "import*" "as" "is"
          "if" "else" "for" "in" "when" "fixed" "const" "throw" "read"
          "read?" "read*" "let" "this" "super" "outer")
      symbol-end))

(defconst pkl-font-lock-keywords
  `((,pkl--module-definition-rx . (1 font-lock-type-face))
    (,pkl--import-call-rx . (1 font-lock-keyword-face))
    (,pkl--throw-call-rx . (1 font-lock-keyword-face))
    (,pkl--trace-call-rx . (1 font-lock-keyword-face))
    (,pkl--read-call-rx . (1 font-lock-keyword-face))
    (,pkl--object-assignment-rx . (1 font-lock-variable-name-face))
    (,pkl--annotation-rx . font-lock-preprocessor-face)
    (,pkl--type-name-rx . font-lock-type-face)
    (,pkl--function-call-rx . (1 font-lock-function-name-face))
    (,pkl--member-access-rx . (1 font-lock-variable-name-face))
    (,pkl--literal-rx . font-lock-constant-face)
    (,pkl--keyword-rx . font-lock-keyword-face)
    (,pkl--predefined-type-rx . font-lock-type-face)
    (,pkl--assignment-rx . (1 font-lock-variable-name-face))
    (,pkl--type-annotation-rx . (1 font-lock-variable-name-face))
    (,pkl--doc-comment-rx . (0 font-lock-doc-face t))
    (,pkl--float-rx . font-lock-constant-face)
    (,pkl--integer-rx . font-lock-constant-face)))

;; Syntax table

(defconst pkl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?` "'" table)
    ;; TODO: Support paired <> delimiters. I tried to figure out how cc-mode
    ;;       does it but it looks like it does some cursed shit with manual
    ;;       syntax class assignment.
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?. "." table)
    table))

;; Major mode definition

;;;###autoload
(define-derived-mode pkl-mode prog-mode "Pkl"
  "Major mode for editing Pkl files."
  :syntax-table pkl-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local font-lock-defaults '(pkl-font-lock-keywords))
  (setq-local indent-line-function #'pkl-indent-line)

  (pkl--optionally-enable pkl-enable-copilot pkl--feature-copilot (pkl--integrate-copilot)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pkl\\'" . pkl-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("PklProject\\'" . pkl-mode))

(provide 'pkl-mode)
;;; pkl-mode.el ends here
