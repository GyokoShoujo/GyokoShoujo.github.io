
from .log import message, info, debug

working_dir = None


def set_working_dir(directory):
    '''
    Sets the destination directory to generate content into.
    '''
    debug('gyoko working_dir: {0}', directory)
    global working_dir
    working_dir = directory


def checkout_site(branch):
    '''
    Prepares the git repository for site generation.
    '''
    debug('checking out {0} to {1}', branch, working_dir)


def gen_site():
    '''
    Generates the site.
    '''
    debug('generating site in {0}', working_dir)


def commit_site():
    '''
    Commits current changes to the repository.
    '''
    debug('committing site in {0}', working_dir)


def push_site(remote):
    '''
    Pushes changes to the repository to the given remote.
    '''
    debug('committing site in {0} to git remote {0}', working_dir, remote)
