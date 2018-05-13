;;; org-outline-numbering.el --- Show outline numbering as overlays in org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author: Anders Johansson
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
(require 'ox)
(require 'cl-lib)
(require 'ov)

(defcustom org-outline-numbering-ignored-tags '()
  "List of extra tags for which subtrees will be not be given numbers.
There is no need to add the tags from ‘org-export-exlude-tags’,
the ARCHIVE tag or similar here, since the default export
settings which excludes these are used"
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
           (let* ((info (org-combine-plists
                         (org-export--get-export-attributes)
		                 (org-export--get-buffer-attributes)
                         (org-export-get-environment)
                         '(:section-numbers t)))
                  (info (plist-put info :exclude-tags
                                   (append
                                    (plist-get info :exclude-tags)
                                    org-outline-numbering-ignored-tags)))
                  (tree (org-element-parse-buffer))
                  numberlist)
             (org-export--prune-tree tree info)
             (setq numberlist
                   (org-export--collect-headline-numbering tree info))
             (cl-loop for hl in numberlist
                      collect (cons
                               (org-element-property :begin (car hl))
                               (list (cdr hl)))))
           do
           (let ((ov (make-overlay p (+ (length lv) p))))
             (overlay-put ov 'display
                          (concat (mapconcat 'number-to-string lv ".") ". "))
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
