
import datetime
import subprocess

from .exceptions import GyokoException
from .log import message, info, debug, verbosity


working_dir = None


def set_working_dir(directory):
    '''
    Sets the destination directory to generate content into.
    '''
    debug('gyoko working_dir: {0}', directory)
    global working_dir, git
    working_dir = directory


def git(command, *args):
    '''
    Runs a git subcommand. *args is a list of parameters to give to git.
    '''
    args = ('git', command,) + tuple(str(x) for x in args)
    if verbosity > 1:
        args = tuple(x for x in args if x != '--quiet')
    try:
        debug('Executing git command: {0}'.format(' '.join(args)))
        proc = subprocess.Popen(args, universal_newlines=True, cwd=working_dir)
        stdout, stderr = proc.communicate(timeout=60)
    except subprocess.TimeoutExpired:
        proc.kill()
        stdout, stderr = proc.communicate()

    result = proc.returncode
    if result != 0:
        message('Git command failed:\n{0}'.format(' '.join(args)))
        if stdout:
            debug(stdout)
        if stderr:
            info(stderr)
        raise GyokoException()
    if stdout:
        debug(stdout)


def checkout_site(repo, branch):
    '''
    Prepares the git repository for site generation.
    '''
    debug('checking out {0} to {1}', branch, working_dir)
    git('clone', '--quiet', '--depth', 5, '--branch', branch, repo, '.')


def commit_site():
    '''
    Commits current changes to the repository.
    '''
    debug('committing site in {0}', working_dir)
    git('add', '--all')
    commit_msg = 'Site generated at {0}'.format(
        datetime.datetime.now().isoformat())
    git('commit', '--quiet', "--message={0}".format(commit_msg))


def push_site(remote, branch):
    '''
    Pushes changes to the repository to the given remote branch.
    '''
    debug('pushing site in {0} to {1} {2}',
          working_dir, remote, branch)
    git('push', '--quiet', remote, branch)
