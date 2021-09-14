;;; hideshow-tree-sitter.el --- add tree sitter based code folding  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeetaditya Chatterjee

;; Author: Jeetaditya Chatterjee <jeetelongname@gmail.com>
;; Keywords: languages, faces, tree-sitter
;;; Commentary:
;;
;;; Code:
(require 'tree-sitter)
(require 'hideshow)
(require 'seq)
(require 'cl-lib)

(defgroup hideshow-tree-sitter nil
  "Tree sitter based code folding leveraging hideshow."
  :group 'tree-sitter
  :prefix "hideshow-tree-sitter-")

(defconst hideshow-tree-sitter-language-alist
  '((ruby-mode . "ruby"))
  "All of the langauges mode supports.") ;; TODO: Fix doc string

(defconst hideshow-tree-sitter--query-dir
  (let ((lib-dir (file-name-directory (locate-library "hideshow-tree-sitter.el"))))
    (file-name-as-directory (concat lib-dir "queries"))))

(defun hideshow-tree-sitter--get-nodes ()
  "Get all the folable nodes in a buffer."
  (let* ((tree (tsc-root-node tree-sitter-tree))
         (lang (cdr (assoc major-mode hideshow-tree-sitter-language-alist)))
         (query-string (with-temp-buffer
                         (insert-file-contents
                          (concat hideshow-tree-sitter--query-dir
                                  lang "/folds.scm"))
                         (buffer-string)))
         (query-object (tsc-make-query tree-sitter-language query-string))
         (captures (tsc-query-captures query-object tree #'tsc--buffer-substring-no-properties))
         (nodes (seq-map #'cdr captures)))
    nodes))

(defun hideshow-tree-sitter--get-range ()
  "Get the range of the node under point."
  (let* ((loc (point))
         (byte-loc (position-bytes loc))
         (nodes (hideshow-tree-sitter--get-nodes))
         (ranges-bytepos (seq-map #'tsc-node-byte-range nodes))
         (range (seq-filter (lambda (cell)
                              (and (> byte-loc (car cell))
                                   (< byte-loc (cdr cell))))
                            ranges-bytepos)))

    (message "hello %s %d" range (length range))

    (if (not (= 1 (length range)))
        (error "Something terrible has happened")
      (let ((range (car range)))
        (cons (byte-to-position (car range))
              (byte-to-position (cdr range)))))))

(defun hideshow-tree-sitter-hide-block ()
  "Hide a block."
  (message "%s" (hideshow-tree-sitter--get-range)))

(defun hideshow-tree-sitter-show-block ()
  "Unhide (show) a block.")

(defun hideshow-tree-sitter-hide-all ()
  "Hide all blocks.")

(defun hideshow-tree-sitter-show-all ()
  "Unhide (show) all blocks.")

(defun hideshow-treesitter-toggle-hiding (&optional e)
  "Toggle hiding/showing of a block.
See `hideshow-tree-sitter-hide-block' and `hideshow-tree-sitter-show-block'.
Argument E should be the event that triggered this action."
  (interactive)
  (hs-life-goes-on
   (posn-set-point (event-end e))
   (if (hs-already-hidden-p)
       (hideshow-tree-sitter-show-block)
     (hideshow-tree-sitter-hide-block))))

(provide 'hideshow-tree-sitter)
;;; hideshow-tree-sitter.el ends here
