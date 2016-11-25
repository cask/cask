#!/usr/bin/env python
# -*- coding: utf-8; -*-

# Copyright (C) 2012, 2013, 2014 Johan Andersson
# Copyright (C) 2013, 2014 Sebastian Wiesner

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

"""
Cask bootstrap script
"""

from __future__ import unicode_literals, print_function

import sys
import errno

from sys import stdout, stderr, exit
from os.path import isdir, expanduser, join as pjoin
from subprocess import CalledProcessError, check_call


#-----------------------------------------------------------------------------
REPOSITORY = 'https://github.com/cask/cask.git'
ISSUE_TRACKER = 'https://github.com/cask/cask/issues'
TARGET_DIRECTORY = pjoin(expanduser('~'), '.cask')

#-----------------------------------------------------------------------------
# utility functions
ok_fmt  = '\033[32m{0}\033[0m' if stdout.isatty() else '{0}'
nok_fmt = '\033[31m{0}\033[0m' if stderr.isatty() else '{0}'

info  = lambda x: print(ok_fmt.format(x))

def error(msg):
    print(nok_fmt.format(msg), file=stderr)
    exit(1)

#-----------------------------------------------------------------------------
def install(installdir, source):
    if isdir(installdir):
        error('Directory %r exists. Has cask already been installed?' % installdir)

    try:
        cmd = ['git', 'clone', source, installdir]
        check_call(cmd)
    except CalledProcessError:
        msg = 'Cask could not be installed. Try again later or report the issue at %s'
        error(msg % ISSUE_TRACKER)
    except OSError as e:
        if e.errno == errno.ENOENT:
            error('Error installing cask - please install \'git\' first.')

#-----------------------------------------------------------------------------
def bootstrap(installdir):
    cask = pjoin(installdir, 'bin', 'cask')

    try:
        cmd = [sys.executable, cask, 'upgrade-cask']
        check_call(cmd)
    except CalledProcessError:
        msg = 'Cask could not be bootstrapped. Try again later, or report an issue at %s'
        error(msg % ISSUE_TRACKER)

#-----------------------------------------------------------------------------
def main(installdir, source):
    info('Installing cask into %r ...' % installdir)
    install(installdir, source)

    info('\nBootstrapping installation ...')
    bootstrap(installdir)

    info('\nSuccess! You may now add cask to your $PATH with:')
    print('  export PATH="%s/bin:$PATH"' % installdir)
    exit(0)


if __name__ == '__main__':
    main(TARGET_DIRECTORY, REPOSITORY)
