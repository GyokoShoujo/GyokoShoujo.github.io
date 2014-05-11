import pathlib
import shutil

from jinja2 import Environment, TemplateNotFound
import markdown
from slugify import slugify

from . import base
from .exceptions import GyokoException
from .images import make_thumbnail
from .loader import TemplateLoader
from .log import message, info, debug

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
    content_dir = source / "content"

    global env
    env = Environment(loader=TemplateLoader(str(tmpl_dir.resolve())))

    # Copy static files
    static_source = source / 'static'
    static_dest = dest / 'static'
    info("Copying static files from {0} to {1}", static_source, static_dest)
    shutil.copytree(str(static_source.resolve()), str(static_dest))
    gen_dir_from_markdown(content_dir, dest, ignore_set={'index', 'toc', })
    gen_images_dirs(content_dir, dest)

    # TODO: generate the blog
    # gen_dir_from_markdown(source / "blog", dest, generic_tmpl_name='blog.jinja',
    #                       ignore=['index', ])


def gen_images_dirs(source, output):
    for d in source.iterdir():
        if not d.is_dir():
            continue
        parts = d.name.split('-', 1)
        if len(parts) == 1:
            info("Found non-matching directory {0}", source)
            continue
        chapter_num, title = (x.strip() for x in parts)
        slug = slugify(title)
        gen_dir_from_images(d, output, slug, title)
        # TODO: gather the chapters up and make the core index.


def gen_dir_from_images(source, output, slug, chapter_title):
    '''
    Generates a "chapter" of images from the given source directory.
    '''
    html_dir = output / slug
    img_dir = output / 'static' / slug / 'page-images'
    thumbnail_dir = output / 'static' / slug / 'page-thumbnails'

    for dir in (html_dir, img_dir, thumbnail_dir):
        dir.mkdir(parents=True)
    page_title = chapter_title
    page_count = 0
    pages = {}
    url_base = '//static/' + slug + '/page-images'
    thumb_base = '//static/' + slug + '/page-thumbnails'
    for img in source.glob('*.png'):
        parts = img.name.split('-', 1)
        if len(parts) == 2:
            img_num, page_title = (x.strip() for x in parts)
        else:
            img_num = parts[0].strip()
        page_base = 'page-{0}'.format(img_num)
        img_name = page_base + '.png'
        thumbnail = thumbnail_dir / img_name
        img_dest = img_dir / img_name
        shutil.copy2(str(img.resolve()), str(img_dest))
        make_thumbnail(img, thumbnail)
        md_file = img.parent / (img.stem + '.md')
        page_count = max(int(img_num), page_count)
        context = {
            'page_title': page_title,
            'page_image': url_base + '/' + img_name
        }
        pages[int(img_num)] = (page_base, md_file, context)

    for img_num in sorted(pages.keys()):
        page_base, markdown_file, context = pages[img_num]
        context['has_previous'] = img_num > 0
        context['has_next'] = img_num < (len(pages) - 1)
        context['page_previous'] = url_base + '/page-' + str(img_num - 1) + '.html'
        context['page_next'] = url_base + '/page-' + str(img_num + 1) + '.html'
        gen_html(page_base, html_dir, markdown_file, generic_tmpl_name='page',
                 extra_context=context)

    if page_count == 0:
        info('No images found in {0}', source)

    context = {
        'page_title': chapter_title,
        'page_count': page_count,
        'file_base': 'page',
        'page_base': url_base,
        'thumbnail_base': thumb_base,
    }
    index_md = source / 'index.md'
    gen_html('index', html_dir, index_md, generic_tmpl_name='chapter',
             extra_context=context)


def gen_dir_from_markdown(source, output, generic_tmpl_name='content.jinja',
                          ignore_set=None):
    '''
    Looks for all of the markdown files in the source directory,
    generates html files from them, and puts them in the output
    directory. 'generic_tmpl_name' is the template to use when a template with
    the markdown file's name can't be found. Specific filenames can be
    ignored by passing a list of filename into 'ignore_set'.
    '''
    files_converted = 0
    for md_file in source.glob('*.md'):
        if md_file.name in ignore_set:
            continue
        gen_html(md_file.stem, output, md_file,
                 generic_tmpl_name=generic_tmpl_name)
        files_converted += 1

    if files_converted == 0:
        info("No html files generated in {0}".format(source))


def gen_html(file_name, output_dir, markdown_file=None,
             generic_tmpl_name=None, extra_context={}):
    '''
    Generates an html file from the given inputs.
      'file_name':     Name to give the file. '.html' will be appended if
                       it isn't already there.
      'output_dir':    Directory to put the output file in.
      'markdown_file': Source markdown to render text from. (optional)
      'generic_tmpl_name': By default, we look for a template named
                       'file_name'. If we can't find it, we will fall back
                       to the template specified by this parameter.
      'extra_context': A dictionary of extra context to pass to the template.
    '''
    debug("Converting {0}", markdown_file)
    if markdown_file.exists():
        with markdown_file.open() as f:
            md = f.read()
        content = markdown.markdown(md)
    else:
        content = ''

    context = extra_context.copy()
    context['content_markdown'] = content

    if 'page_title' not in context:
        context['page_title'] = file_name

    try:
        template = env.get_template(file_name)
    except TemplateNotFound:
        try:
            template = env.get_template(generic_tmpl_name)
        except TemplateNotFound:
            message('Could not find template for {0} (tried "{1}")',
                    str(output_dir), generic_tmpl_name)
            raise GyokoException()

    html = template.render(**context)

    output_path = output_dir / (file_name + ".html")
    with output_path.open('w') as f:
        f.write(html)
