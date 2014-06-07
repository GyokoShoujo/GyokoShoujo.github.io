
import datetime
import pathlib
import os
import os.path
import subprocess

from .exceptions import GyokoException
from .log import message, info, debug
import gyoko.log as log


working_dir = None
local_repo = os.path.join(os.path.dirname(__file__), '..')


def set_working_dir(directory):
    '''
    Sets the destination directory to generate content into.
    '''
    debug('gyoko working_dir: {0}', directory)
    global working_dir, git
    working_dir = directory


def git(command, *args, **kwargs):
    '''
    Runs a git subcommand. *args is a list of parameters to give to git.
    Certain meta-options can be passed via keyword arguments. Currently
    supported are:
      cwd: Directory to operate within
    '''
    args = ('git', command,) + tuple(str(x) for x in args)
    env = os.environ
    if log.verbosity > 1:
        args = tuple(x for x in args if x != '--quiet')
    if log.verbosity > 2:
        env['GIT_TRACE'] = '1'
    cwd = kwargs.get('cwd', working_dir)
    try:
        debug('Executing git command: {0}'.format(' '.join(args)))
        proc = subprocess.Popen(args, universal_newlines=True, cwd=cwd,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                env=env)
        stdout, stderr = proc.communicate(timeout=60)
    except subprocess.TimeoutExpired:
        proc.kill()
        stdout, stderr = proc.communicate()

    result = proc.returncode
    if result != 0:
        message('Git command failed:\n{0}'.format(' '.join(args)))
        if stdout:
            debug('stdout from git {0}:\n{1}', command, stdout)
        if stderr:
            info('stderr from git {0}:\n{1}', command, stderr)
        raise GyokoException()
    if stdout:
        debug('Output of git {0}:\n{1}', command, stdout)
    return stdout, stderr


def checkout_site(branch):
    '''
    Prepares the git repository for site generation.
    '''
    debug('testing that repositories are in sync')
    git('remote', 'update', cwd=local_repo)
    local, _ = git('rev-parse', branch, cwd=local_repo)
    remote, _ = git('rev-parse', 'origin/{0}'.format(branch), cwd=local_repo)
    if local != remote:
        message("Local repository doesn't match remote. Fix that first.")
        raise GyokoException()
    debug('cloning {0} to {1}', branch, working_dir)
    git('clone', '--quiet', '--branch', branch, local_repo, '.')
    output = pathlib.Path(working_dir)

    def rm_tree(d):
        keepers = ('.git',)
        for item in d.iterdir():
            if item.name in keepers:
                continue
            if item.is_dir():
                rm_tree(item)
                item.rmdir()
            else:
                item.unlink()
    rm_tree(output)


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
    debug('pushing site in {0} to {1} {2} (through local repo)',
          working_dir, remote, branch)
    git('push', '--quiet', 'origin', branch)
    git('push', '--quiet', remote, '{b}:{b}'.format(b=branch), cwd=local_repo)


def did_site_change():
    '''
    Tests whether there are any changes that need to be committed.
    '''
    stdout, _ = git('status', '--porcelain')
    # There will be nothing in stdout if there is nothing to commit.
    return bool(stdout)
