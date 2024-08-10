;;; gitmoji.el --- Insert Gitmoji -*- lexical-binding: t; -*-

;; Copyright (C) 2024 4shen0ne
;;
;; Author: 4shen0ne <https://github.com/zrquan>
;; Maintainer: 4shen0ne <4shen.01@gmail.com>
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; A simple package to help you insert Gitmoji.

;;; Code:

(defvar gitmoji-alist
  '(("🎨 => Improve structure / format of the code" . "🎨")
    ("⚡ => Improve performance" . "⚡")
    ("🔥 => Remove code or files" . "🔥")
    ("🐛 => Fix a bug" . "🐛")
    ("✨ => Introduce new features" . "✨")
    ("📝 => Write documentation" . "📝")
    ("🚀 => Deploy stuff" . "🚀")
    ("💄 => Update the UI and style files" . "💄")
    ("🎉 => Initial commit" . "🎉")
    ("✅ => Add tests" . "✅")
    ("🔒 => Fix security issues" . "🔒")
    ("🔖 => Release / Version tags" . "🔖")
    ("🚨 => Remove linter warnings" . "🚨")
    ("🚧 => Work in progress" . "🚧")
    ("💚 => Fix CI build issues" . "💚")
    ("⬇️ => Downgrade dependencies" . "⬇️")
    ("⬆️ => Upgrade dependencies" . "⬆️")
    ("📌 => Pin dependencies to specific versions" . "📌")
    ("👷 => Add CI build system" . "👷")
    ("📈 => Add analytics or track code" . "📈")
    ("♻️ => Refactor code" . "♻️")
    ("➕ => Add a dependency" . "➕")
    ("➖ => Remove a dependency" . "➖")
    ("🔧 => Modify configuration files" . "🔧")
    ("🔨 => Major refactoring" . "🔨")
    ("🌐 => Internationalization and localization" . "🌐")
    ("✏️ => Fix typos" . "✏️")
    ("💩 => Write bad code that needs to be improved" . "💩")
    ("⏪ => Revert changes" . "⏪")
    ("🔀 => Merge branches" . "🔀")
    ("📦 => Update compiled files or packages" . "📦")
    ("👽 => Update code due to external API changes" . "👽")
    ("🚚 => Move or rename files" . "🚚")
    ("📄 => Add or update license" . "📄")
    ("💥 => Introduce breaking changes" . "💥")
    ("🍱 => Add or update assets" . "🍱")
    ("♿ => Improve accessibility" . "♿")
    ("💡 => Add or update comments in source code" . "💡")
    ("🍻 => Write code drunk" . "🍻")
    ("💬 => Add or update text and literals" . "💬")
    ("🗃️ => Perform database related changes" . "🗃️")
    ("🔊 => Add or update logs" . "🔊")
    ("🔇 => Remove logs or comments" . "🔇")
    ("👥 => Add contributor(s)" . "👥")
    ("🚸 => Improve user experience / usability" . "🚸")
    ("🏗️ => Make architectural changes" . "🏗️")
    ("📱 => Work on responsive design" . "📱")
    ("🤡 => Mock things" . "🤡")
    ("🥚 => Add or update an easter egg" . "🥚")
    ("🙈 => Add or update a .gitignore file" . "🙈")
    ("📸 => Add or update snapshots" . "📸")
    ("⚗️ => Experiment new things" . "⚗️")
    ("🔍 => Improve SEO" . "🔍")
    ("🧪 => Add a failing test" . "🧪")
    ("👔 => Add or update business logic" . "👔")
    ("🩹 => Simple fix for a non-critical issue" . "🩹")
    ("🧐 => Data exploration/inspection" . "🧐")
    ("⚰️ => Remove dead code" . "⚰️")
    ("💻 => Improve developer experience" . "💻")
    ("🦺 => Add or update security" . "🦺"))
  "A list of Gitmoji with descriptions for commit messages.")

(defun gitmoji-insert ()
  "Prompt the user to select and insert a Gitmoji."
  (interactive)
  (let* ((gitmoji (completing-read "Select Gitmoji: " (mapcar #'car gitmoji-alist)))
         (emoji (cdr (assoc gitmoji gitmoji-alist))))
    (insert emoji " ")))

(provide 'gitmoji)
;;; gitmoji.el ends here
