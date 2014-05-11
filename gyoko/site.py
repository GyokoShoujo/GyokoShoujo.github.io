from collections import namedtuple
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

Chapter = namedtuple('Chapter', ['dir', 'pages', 'number', 'title',
                                 'slug', 'markdown_file', ])
Page = namedtuple('Page', ['source', 'number', 'title', 'markdown_file', ])


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

    chapters = build_chapters(content_dir)
    info("Found {0} chapters.", len(chapters))

    # Copy static files
    static_source = source / 'static'
    static_dest = dest / 'static'
    info("Copying static files from {0} to {1}", static_source, static_dest)
    shutil.copytree(str(static_source.resolve()), str(static_dest))
    gen_dir_from_markdown(content_dir, dest, ignore_set={'index', 'toc', })
    gen_chapters(chapters, dest)
    # TODO: Make the core index.

    # TODO: generate the blog
    # gen_dir_from_markdown(source / "blog", dest, generic_tmpl_name='blog.jinja',
    #                       ignore=['index', ])


def build_chapters(source):
    '''
    Runs through the source directory and generates a map of all of
    the pages. We return a list of chapters with all of the pages
    each chapter contains. See the definitions of the namedtuples Page
    and Chapter above.
    '''
    chapters = []
    chapter_titles = set()
    for chapter_dir in source.iterdir():
        if not chapter_dir.is_dir():
            continue
        parts = chapter_dir.name.split('-', 1)
        if len(parts) == 1:
            info("Found non-matching directory {0}", source)
            continue
        chapter_num, title = (x.strip() for x in parts)
        if title in chapter_titles:
            message('Duplicate chapter title: {0}.'
                    .format(title))
            message('Chapter title must be unique. Exiting.')
            raise GyokoException()
        chapter_titles.add(title)
        md_file = chapter_dir.parent / 'index.md'
        chapter = Chapter(chapter_dir, [], int(chapter_num),
                          title, slugify(title), md_file)
        page_title = title
        for img in chapter_dir.glob('*.png'):
            parts = img.name.split('-', 1)
            if len(parts) == 2:
                img_num, page_title = (x.strip() for x in parts)
            else:
                img_num = parts[0].strip()
            md_file = img.parent / (img.stem + '.md')
            chapter.pages.append(Page(img, int(img_num), page_title, md_file))
        if len(chapter.pages) == 0:
            info('No images found for chapter {0}', chapter.title)

        chapter.pages.sort(key=lambda p: p.number)
        chapters.append(chapter)

    chapters.sort(key=lambda c: c.number)
    return chapters


def gen_chapters(chapters, output):
    '''
    Outputs the HTML for each chapters and the pages they contain.
    '''
    page_count = sum([len(c.pages) for c in chapters])
    page_num = 1
    prev_url = None
    for chapter_idx, chapter in enumerate(chapters):
        html_dir = output / chapter.slug
        img_dir = output / 'static' / chapter.slug / 'page-images'
        thumbnail_dir = output / 'static' / chapter.slug / 'page-thumbnails'

        for dir in (html_dir, img_dir, thumbnail_dir):
            dir.mkdir(parents=True)

        img_base = '//static/' + chapter.slug + '/page-images'
        thumb_base = '//static/' + chapter.slug + '/page-thumbnails'
        for page_idx, page in enumerate(chapter.pages):
            page_base = 'page-{0}'.format(page.number)
            img_name = page_base + '.png'
            thumbnail = thumbnail_dir / img_name
            img_dest = img_dir / img_name
            shutil.copy2(str(page.source.resolve()), str(img_dest))
            make_thumbnail(page.source, thumbnail)
            page_url = '//{0}/{1}.html'.format(chapter.slug, page_base)

            next_page = None
            if page_idx < len(chapter.pages):
                next_page = '//{0}/page-{1}.html'.format(chapter.slug, page_idx+1)
            elif (chapter_idx < len(chapters) and
                  len(chapters[chapter_idx+1].pages) > 0):
                next_page = '//{0}/page-0.html'.format(chapters[chapter_idx+1].slug)
            context = {
                'page': page,
                'page_title': page.title,
                'page_image': img_base + '/' + img_name,
                'has_previous': prev_url is not None,
                'page_previous': prev_url,
                'has_next': next_page is not None,
                'page_next': next_page,
                'page_number': page_num,
                'page_count': page_count,
            }
            gen_html(page_base, html_dir, page.markdown_file,
                     generic_tmpl_name='page', extra_context=context)
            prev_url = page_url
            page_num += 1

        context = {
            'chapter': chapter,
            'page_title': chapter.title,
            'file_base': 'page',
            'thumbnail_base': thumb_base,
        }
        gen_html('index', html_dir, chapter.markdown_file,
                 generic_tmpl_name='chapter', extra_context=context)


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
