;;; org-outline-numbering.el --- Show outline numbering as overlays in org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author: John Kitchin, Anders Johansson
;; Maintainer: Anders Johansson
;; Created: 2018-02-16
;; Updated: 2018-02-16
;; Keywords: wp, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines a command and a mode for displaying the headline numbering
;; in org mode buffers as overlays.
;;
;; Adapted from code posted by John Kitchin at:
;; https://emacs.stackexchange.com/a/32422

;;; Code
(require 'org)
(require 'cl-lib)
(require 'ov)

(defcustom org-outline-numbering-ignored-tags '("noexport" "ARCHIVE")
  "List of tags for which subtrees will be not be given numbers"
  :type '(repeat string)
  :group 'org)

;;;###autoload
(define-minor-mode org-outline-numbering-mode
  "Minor mode to number org headings."
  :init-value nil
  (if org-outline-numbering-mode
      (org-outline-numbering-overlay)
    (org-outline-numbering-clear-overlays)))

;;;###autoload
(defun org-outline-numbering-overlay ()
  "Put numbered overlays on the headings."
  (interactive)
  (cl-loop for (p lv) in
           (let ((counters (cl-copy-list '(0 0 0 0 0 0 0 0 0 0)))
                 (current-level 1)
                 last-level)
             (save-excursion
               (org-with-wide-buffer
                (goto-char (point-min))
                (org-scan-tags
                 (lambda ()
                   (let* ((hl (org-element-context))
                          (level (org-element-property :level hl)))
                     (setq last-level current-level
                           current-level level) 
                     (cond
                      ;; no level change or increase, increment level counter
                      ((or (= last-level current-level)
                           (> current-level last-level))
                       (cl-incf (nth (1- current-level) counters)))

                      ;; decrease in level
                      (t
                       (cl-loop for i from (+ 1 current-level) below (length counters)
                                do
                                (setf (nth (1- i) counters) 0))
                       (cl-incf (nth (1- current-level) counters))))

                     (list (point)
                           (cl-subseq counters 0
                                      (cl-position 0 counters)))))
                 (lambda (_todo tags-list _level)
                   (setq org-cached-props nil)
                   (and (not (org-in-commented-heading-p))
                        (not (cl-intersection
                              org-outline-numbering-ignored-tags
                              tags-list :test
                              #'string=))))
                 nil))))
           do
           (let ((ov (make-overlay p (+ 1 p))))
             (overlay-put ov 'display (concat (mapconcat 'number-to-string lv ".") ". "))
             (overlay-put ov 'numbered-heading t)
             (overlay-put ov 'face 'default))))

;;;###autoload
(defun org-outline-numbering-clear-overlays ()
  "Clear outline numbering overlays in widened buffer"
  (interactive)
  (save-restriction
    (widen)
    (ov-clear 'numbered-heading)))

(provide 'org-outline-numbering)
;;; org-outline-numbering.el ends here
