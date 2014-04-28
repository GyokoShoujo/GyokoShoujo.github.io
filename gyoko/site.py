import pathlib

from jinja2 import Environment, TemplateNotFound
import markdown

from . import base
from .exceptions import GyokoException
from .loader import TemplateLoader
from .log import message, info, debug, verbosity

env = None


def gen_site(source_dir):
    '''
    Generates the site.
    '''
    debug('generating site in {0} using source {1}',
          base.working_dir, source_dir)
    source = pathlib.Path(source_dir)
    dest = pathlib.Path(base.working_dir)
    if not (source.exists() and source.is_dir()):
        message('Source directory {0} does not exist', source)

    tmpl_dir = source / "templates"

    global env
    env = Environment(loader=TemplateLoader(str(tmpl_dir.resolve())))

    gen_dir_from_markdown(source / "content", dest, ignore=['index', 'toc', ])
    # gen_dir_from_markdown(source / "blog", dest, generic='blog.jinja',
    #                       ignore=['index', ])


def gen_dir_from_markdown(source, output, generic='content.jinja',
                          ignore=None):
    '''
    Looks for all of the markdown files in the source directory,
    generates html files from them, and puts them in the output
    directory. 'generic' is the template to use when a template with
    the markdown file's name can't be found. Specific filenames can be
    ignored by passing a list of filename into 'ignore'.
    '''
    files_converted = 0
    for md_file in source.glob('*.md'):
        if md_file.name in ignore:
            continue
        gen_html_from_md(md_file, output, generic=generic)
        files_converted += 1

    if files_converted == 0:
        info("No html files generated in {0}".format(source))


def gen_html_from_md(md_file, output,  generic=None, extra_context={}):
    '''
    Generates an html file from the given markdown file.
    '''
    debug("Converting {0}", md_file)
    with md_file.open() as f:
        md = f.read()
    content = markdown.markdown(md)

    context = extra_context.copy()
    context['content_markdown'] = content

    file_title = md_file.stem
    if 'page_title' not in context:
        context['page_title'] = file_title

    try:
        template = env.get_template(file_title)
    except TemplateNotFound:
        template = env.get_template(generic)

    html = template.render(**context)

    output_path = output / (file_title + ".html")
    with output_path.open('w') as f:
        f.write(html)
