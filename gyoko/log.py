
def log(msg, *args, **kwargs):
    print(msg.format(*args, **kwargs))


def no_log(msg, *args, **kwargs):
    pass


loggers = [(log, no_log, no_log), (log, log, no_log), (log, log, log), ]

message, info, debug = loggers[0]


def set_log_verbosity(verbosity):
    ''' Sets the logging verbosity: 0 <= verbosity <= 2
    Verbosity 0: only calls to message are displayed
    Verbosity 1: message and info calls are displayed
    Verbosity 2: message, info, and debug calls are displayed'''
    global message, info, debug
    message, info, debug = loggers[verbosity]
