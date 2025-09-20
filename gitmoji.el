;;; gitmoji.el --- Insert Gitmoji -*- lexical-binding: t; -*-

;; Copyright (C) 2024 4shen0ne
;; Author: 4shen0ne <https://github.com/zrquan>
;; Maintainer: 4shen0ne <4shen.01@gmail.com>

;;; Commentary:
;; Simple helper to insert a Gitmoji with a descriptive completion.

;;; Code:

(defconst gitmoji-alist
  '(("Improve structure / format of the code" . "🎨")
    ("Improve performance" . "⚡")
    ("Remove code or files" . "🔥")
    ("Fix a bug" . "🐛")
    ("Introduce new features" . "✨")
    ("Write documentation" . "📝")
    ("Deploy stuff" . "🚀")
    ("Update the UI and style files" . "💄")
    ("Initial commit" . "🎉")
    ("Add tests" . "✅")
    ("Fix security issues" . "🔒")
    ("Release / Version tags" . "🔖")
    ("Remove linter warnings" . "🚨")
    ("Work in progress" . "🚧")
    ("Fix CI build issues" . "💚")
    ("Downgrade dependencies" . "⬇️")
    ("Upgrade dependencies" . "⬆️")
    ("Pin dependencies to specific versions" . "📌")
    ("Add CI build system" . "👷")
    ("Add analytics or track code" . "📈")
    ("Refactor code" . "♻️")
    ("Add a dependency" . "➕")
    ("Remove a dependency" . "➖")
    ("Modify configuration files" . "🔧")
    ("Major refactoring" . "🔨")
    ("Internationalization and localization" . "🌐")
    ("Fix typos" . "✏️")
    ("Write bad code that needs to be improved" . "💩")
    ("Revert changes" . "⏪")
    ("Merge branches" . "🔀")
    ("Update compiled files or packages" . "📦")
    ("Update code due to external API changes" . "👽")
    ("Move or rename files" . "🚚")
    ("Add or update license" . "📄")
    ("Introduce breaking changes" . "💥")
    ("Add or update assets" . "🍱")
    ("Improve accessibility" . "♿")
    ("Add or update comments in source code" . "💡")
    ("Write code drunk" . "🍻")
    ("Add or update text and literals" . "💬")
    ("Perform database related changes" . "🗃️")
    ("Add or update logs" . "🔊")
    ("Remove logs or comments" . "🔇")
    ("Add contributor(s)" . "👥")
    ("Improve user experience / usability" . "🚸")
    ("Make architectural changes" . "🏗️")
    ("Work on responsive design" . "📱")
    ("Mock things" . "🤡")
    ("Add or update an easter egg" . "🥚")
    ("Add or update a .gitignore file" . "🙈")
    ("Add or update snapshots" . "📸")
    ("Experiment new things" . "⚗️")
    ("Improve SEO" . "🔍")
    ("Add a failing test" . "🧪")
    ("Add or update business logic" . "👔")
    ("Simple fix for a non-critical issue" . "🩹")
    ("Data exploration/inspection" . "🧐")
    ("Remove dead code" . "⚰️")
    ("Improve developer experience" . "💻")
    ("Add or update security" . "🦺"))
  "Alist mapping Gitmoji descriptions to emoji characters.")

(defun gitmoji--format-candidate (pair)
  "Return display string \"EMOJI DESC\" for PAIR."
  (concat (cdr pair) " " (car pair)))

(defun gitmoji--display->desc (display)
  "Extract description from DISPLAY string of form \"EMOJI DESC\"."
  ;; remove leading emoji and space; assume first char(s) before first space is emoji
  (string-trim (substring display (1+ (cl-position ?\s display)))))

(defun gitmoji--completion-table ()
  "Completion table whose candidates are \"EMOJI DESC\" strings."
  (let ((cand (mapcar #'gitmoji--format-candidate gitmoji-alist)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (category . gitmoji))
        (complete-with-action action cand string pred)))))

;;;###autoload
(defun gitmoji-insert ()
  "Prompt with emoji + description and insert the chosen emoji and a space."
  (interactive)
  (let* ((disp (completing-read "Gitmoji: " (gitmoji--completion-table)))
         (desc (gitmoji--display->desc disp))
         (emoji (cdr (assoc desc gitmoji-alist))))
    (when emoji
      (insert emoji " "))))

(provide 'gitmoji)
;;; gitmoji.el ends here
