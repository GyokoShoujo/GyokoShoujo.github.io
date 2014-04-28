
verbosity = 0


def set_log_verbosity(v):
    '''
    Sets the logging verbosity: 0 <= verbosity <= 2
        Verbosity 0: only calls to message are displayed
        Verbosity 1: message and info calls are displayed
        Verbosity 2: message, info, and debug calls are displayed
    '''
    global verbosity
    verbosity = v


def _make_logger(allowed_verbosity):
    def logger(msg, *args, **kwargs):
        if verbosity >= allowed_verbosity:
            print(msg.format(*args, **kwargs))
    return logger


message = _make_logger(0)
info = _make_logger(1)
debug = _make_logger(2)
