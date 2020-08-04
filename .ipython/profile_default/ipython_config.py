c.TerminalInteractiveShell.editing_mode = 'vi'

# v to open editor
c.TerminalInteractiveShell.extra_open_editor_shortcuts = True

c.TerminalInteractiveShell.highlight_matching_brackets = True

# Automatically set the terminal title
c.TerminalInteractiveShell.term_title = True

# autoreload imports
c.InteractiveShellApp.extensions = ['autoreload']
c.InteractiveShellApp.exec_lines = ['%autoreload 2']

