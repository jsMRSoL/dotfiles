# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# A simple command for demonstration purposes follows.
# -----------------------------------------------------------------------------

from __future__ import (absolute_import, division, print_function)

# You can import any python module as needed.
import os

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command


# Any class that is a subclass of "Command" will be integrated into ranger as a
# command.  Try typing ":my_edit<ENTER>" in ranger!
class my_edit(Command):
    # The so-called doc-string of the class will be visible in the built-in
    # help that is accessible by typing "?c" inside ranger.
    """:my_edit <filename>

    A sample command for demonstration purposes that opens a file in an editor.
    """

    # The execute method is called when you run this command in ranger.
    def execute(self):
        # self.arg(1) is the first (space-separated) argument to the function.
        # This way you can write ":my_edit somefilename<ENTER>".
        if self.arg(1):
            # self.rest(1) contains self.arg(1) and everything that follows
            target_filename = self.rest(1)
        else:
            # self.fm is a ranger.core.filemanager.FileManager object and gives
            # you access to internals of ranger.
            # self.fm.thisfile is a ranger.container.file.File object and is a
            # reference to the currently selected file.
            target_filename = self.fm.thisfile.path

        # This is a generic function to print text in ranger.
        self.fm.notify("Let's edit the file " + target_filename + "!")

        # Using bad=True in fm.notify allows you to print error messages:
        if not os.path.exists(target_filename):
            self.fm.notify("The given file does not exist!", bad=True)
            return

        # This executes a function from ranger.core.acitons, a module with a
        # variety of subroutines that can help you construct commands.
        # Check out the source, or run "pydoc ranger.core.actions" for a list.
        self.fm.edit_file(target_filename)

    # The tab method is called when you press tab, and should return a list of
    # suggestions that the user will tab through.
    # tabnum is 1 for <TAB> and -1 for <S-TAB> by default
    def tab(self, tabnum):
        # This is a generic tab-completion function that iterates through the
        # content of the current directory.
        return self._tab_directory_content()


class fzf_select(Command):
    """
    :fzf_select

    Find a file using fzf.

    With a prefix argument select only directories.

    See: https://github.com/junegunn/fzf
    """
    def execute(self):
        import subprocess
        import os.path
        if self.quantifier:
            # match only directories
            command=r"find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            -o -type d -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"
        else:
            # match files and directories
            command=r"find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            -o -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"
        fzf = self.fm.execute_command(command, stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.decode('utf-8').rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)

# fzf_fasd - Fasd + Fzf + Ranger (Interactive Style)
class fzf_fasd(Command):
    """
    :fzf_fasd

    Jump to a file or folder using Fasd and fzf

    URL: https://github.com/clvv/fasd
    URL: https://github.com/junegunn/fzf
    """
    def execute(self):
        import subprocess
        if self.quantifier:
            command="fasd | fzf -e -i --tac --no-sort | awk '{print $2}'"
        else:
            command="fasd | fzf -e -i --tac --no-sort | awk '{print $2}'"
        fzf = self.fm.execute_command(command, stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.decode('utf-8').rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)

# Fasd with ranger (Command Line Style)
# https://github.com/ranger/ranger/wiki/Commands
class fasd(Command):
    """
    :fasd

    Jump to directory using fasd
    URL: https://github.com/clvv/fasd
    """
    def execute(self):
        import subprocess
        arg = self.rest(1)
        if arg:
            directory = subprocess.check_output(["fasd", "-d"]+arg.split(), universal_newlines=True).strip()
            self.fm.cd(directory)

class clip(Command):
    """
    :clip

    Create command like 'shell ffmpeg -i "filename" -ss 00:00:00 -t 00:00:10 output.mp4'
    """
    def execute(self):
        self.fm.open_console('shell ffmpeg -i ' + self.fm.thisfile.relative_path + ' -ss 00:00:00 -t 00:00:10 output.mp4'.replace("%", "%%"), position=28 + len(self.fm.thisfile.relative_path))


class killspaces(Command):
    """:killspaces

    Changes the name of the currently highlighted file to version with 
    underscores instead of spaces
    """

    def execute(self):
        from ranger.container.file import File
        from os import access

        new_name = self.fm.thisfile.relative_path.replace(' ', '_')

        if new_name == self.fm.thisfile.relative_path:
            return None

        if access(new_name, os.F_OK):
            return self.fm.notify("Can't rename: file already exists!", bad=True)

        if self.fm.rename(self.fm.thisfile, new_name):
            file_new = File(new_name)
            self.fm.bookmarks.update_path(self.fm.thisfile.path, file_new)
            self.fm.tags.update_path(self.fm.thisfile.path, file_new.path)
            self.fm.thisdir.pointed_obj = file_new
            self.fm.thisfile = file_new

        return None

class gif(Command):
    """:gif

    Converts the highlighted file to a gif.
    """

    def execute(self):
        from os import access

        filename = self.fm.thisfile.basename
        ext = os.path.splitext(filename)[1]
        new_name = filename.replace(ext, ".gif")

        if access(new_name, os.F_OK):
            return self.fm.notify("Can't rename: file already exists!", bad=True)

        command = f'convert {filename} {new_name}'
        self.fm.execute_command(command)

        return None


class tablify(Command):
    """:tablify

    Converts a tab-separated csv to a tabulated text file using tabulate."""

    def execute(self):
        filename = self.fm.thisfile.basename
        ext = os.path.splitext(filename)[1]
        new_name = filename.replace(ext, "-table.txt")

        command = f'tabulate -1 -s, -f simple -o {new_name} {filename}'
        self.fm.execute_command(command)

        return None
