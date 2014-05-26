# -*- coding: utf-8 -*-
# Copyright (C) 2014 Sebastian Wiesner <lunaryorn@gmail.com>

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

"""
Cask documentation build configuration file
"""

import re
import os

SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))

needs_sphinx = '1.2'
extensions = ['sphinxcontrib.emacs', 'sphinx.ext.intersphinx']

default_role = 'code'
primary_domain = 'el'

source_suffix = '.rst'
master_doc = 'index'

def cask_version():
    version_re = re.compile('^;; Version: (?P<version>.*)$')
    doc_directory = os.path.abspath(os.path.dirname(__file__))
    cask = os.path.join(doc_directory, os.pardir, 'cask.el')
    with open(cask) as source:
        for line in source:
            match = version_re.match(line)
            if match:
                return match.group('version')
    raise ValueError('Failed to extract the version')


project = u'Cask'
copyright = u'2014, Johan Andersson'
release = cask_version()
# Just the major and minor version
version = '.'.join(release.split('.')[:2])

pygments_style = 'emacs'

nitpicky = True
nitpick_ignore = [
    # Ignore references to built-in Emacs functions
    ('el:variable', 'emacs-version'),
    ('el:function', 'version-to-list'),
    # Ignore references to standard environment variables
    ('std:envvar', 'PATH'),
]

html_title = '{0} {1}'.format(project, release)

# Restore standard formatting of emphasis, as by
# http://sphinx-doc.org/faq.html#notes
texinfo_elements = {
    'preamble': """
@definfoenclose strong,*,*
@definfoenclose emph,_,_
""",
    'copying': """\
This manual is for Cask version {release}.

Copyright @copyright{{}} {copyright}

@quotation
This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see @uref{{http://www.gnu.org/licenses/}}.
@end quotation
""".format(release=release, copyright=copyright)}

emacs_lisp_load_path = [os.path.abspath(os.path.join(SOURCE_DIR, os.pardir))]
