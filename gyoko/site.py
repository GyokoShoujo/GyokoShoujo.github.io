import base
from .exceptions import GyokoException
from .log import message, info, debug, verbosity


def gen_site():
    '''
    Generates the site.
    '''
    debug('generating site in {0}', base.working_dir)
