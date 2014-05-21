from functools import lru_cache
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

CACHE_SIZE = 256
THUMBNAIL_DIR = 'page-thumbnails'
IMAGE_DIR = 'page-images'


class Chapter:

    __slots__ = ('dir', 'number', 'title', 'cover', 'pages', 'slug', 'markdown_file',
                 'cover_name', 'image_uri', 'cover_image_uri', 'cover_thumbnail_uri',
                 'html_uri', 'thumbnail_uri', '_base')

    def __init__(self, dir, number, title):
        self.dir = dir
        self.number = number
        self.title = title
        self.cover = None
        self.pages = []
        self.slug = slugify(self.title)
        self.markdown_file = self.dir.parent / 'index.md'
        self.cover_name = '{0}-cover.png'.format(self.slug)
        self.image_uri = '/static/{0}/{1}'.format(self.slug, IMAGE_DIR)
        self.cover_image_uri = '{0}/{1}'.format(self.image_uri, self.cover_name)
        self.thumbnail_uri = '/static/{0}/{1}'.format(self.slug, THUMBNAIL_DIR)
        self.cover_thumbnail_uri = '{0}/{1}'.format(self.thumbnail_uri, self.cover_name)
        self.html_uri = '/{0}'.format(self.slug)

    def prepare_for_output(self, base):
        self._base = base
        for dir in (self.html_dir, self.image_dir, self.thumbnail_dir):
            dir.mkdir(parents=True)

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def html_file(self):
        return self.html_dir(self._base) / 'index.html'

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def html_dir(self):
        return self._base / self.slug

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def thumbnail_dir(self):
        return self._base / 'static' / self.slug / THUMBNAIL_DIR

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def image_dir(self):
        return self._base / 'static' / self.slug / IMAGE_DIR

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def cover_image(self):
        return self.image_dir / self.cover_name

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def cover_thumbnail(self):
        return self.thumbnail_dir / self.cover_name


class Page:

    __slots__ = ('chapter', 'source', 'number', 'title', 'stem', 'html_uri',
                 'markdown_file', 'thumbnail_uri', 'image_uri', )

    def __init__(self, chapter, source, number, title):
        self.chapter = chapter
        self.source = source
        self.number = number
        self.title = title
        self.stem = 'page-{0}'.format(self.number)
        self.html_uri = '{0}/{1}.html'.format(self.chapter.html_uri, self.stem)
        self.markdown_file = self.source.parent / (self.source.stem + '.md')
        self.thumbnail_uri = '{0}/{1}.png'.format(self.chapter.thumbnail_uri, self.stem)
        self.image_uri = '{0}/{1}.png'.format(self.chapter.image_uri, self.stem)

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def html_file(self):
        return self.chapter.html_dir / ('{0}.html'.format(self.stem))

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def output_thumbnail(self):
        return self.chapter.thumbnail_dir / ('{0}.png'.format(self.stem))

    @property
    @lru_cache(maxsize=CACHE_SIZE)
    def output_image(self):
        return self.chapter.image_dir / ('{0}.png'.format(self.stem))


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

    with open(str(dest / 'CNAME'), 'w') as f:
        f.write('www.gyokoshoujo.com\n')

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
    gen_index(chapters, source, dest)

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
        page_title = title
        cover = None
        chapter = Chapter(chapter_dir, int(chapter_num), title)

        for img in chapter_dir.glob('*.png'):
            parts = img.stem.split('-', 1)
            if len(parts) == 2:
                img_num, page_title = (x.strip() for x in parts)
            elif parts[0].lower() == 'cover':
                cover = img
                continue
            else:
                img_num = parts[0].strip()
            chapter.pages.append(Page(chapter, img, int(img_num), page_title))

        chapter.pages.sort(key=lambda p: p.number)
        if len(chapter.pages) == 0:
            message('No images found for chapter {0}', chapter.title)
            raise GyokoException()
        if cover is None:
            message('No cover found for chapter {0}, using first page.',
                    chapter.title)
            cover = chapter.pages[0].source
        chapter.cover = cover
        chapters.append(chapter)

    chapters.sort(key=lambda c: c.number)
    return chapters


def gen_index(chapters, source, output):
    '''
    Creates the book's primary index page, which is a list of all
    chapters.
    '''
    context = {
        'page_title': 'Gyoko Shoujo',
        'chapters': chapters,
    }
    gen_html('index', output, source / 'index.md', extra_context=context)


def gen_chapters(chapters, output):
    '''
    Outputs the HTML for each chapters and the pages they contain.
    '''
    page_count = sum([len(c.pages) for c in chapters])
    page_num = 1
    prev_uri = None
    for chapter_idx, chapter in enumerate(chapters):
        chapter.prepare_for_output(output)

        shutil.copy2(str(chapter.cover.resolve()), str(chapter.cover_image))
        make_thumbnail(chapter.cover, chapter.cover_thumbnail)

        for page_idx, page in enumerate(chapter.pages):
            shutil.copy2(str(page.source.resolve()), str(page.output_image))
            make_thumbnail(page.source, page.output_thumbnail)

            next_uri = None
            if page_idx < len(chapter.pages) - 1:
                next_uri = chapter.pages[page_idx+1].html_uri
            elif chapter_idx + 1 < len(chapters):
                # Link to the cover of the next chapter
                next_uri = chapters[chapter_idx+1].html_uri

            context = {
                'page': page,
                'page_title': page.title,
                'has_previous': prev_uri is not None,
                'previous_uri': prev_uri,
                'has_next': next_uri is not None,
                'next_uri': next_uri,
                'page_number': page_num,
                'page_count': page_count,
            }
            gen_html(page.stem, page.chapter.html_dir, page.markdown_file,
                     template_name='page', extra_context=context)
            prev_uri = page.html_uri
            page_num += 1

        context = {
            'chapter': chapter,
            'page_title': chapter.title,
        }
        gen_html('index', chapter.html_dir, chapter.markdown_file,
                 template_name='chapter', extra_context=context)


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
             template_name=None, generic_tmpl_name=None,
             extra_context={}):
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
        if template_name:
            template = env.get_template(template_name)
        else:
            template = env.get_template(file_name)
    except TemplateNotFound:
        try:
            if generic_tmpl_name:
                template = env.get_template(generic_tmpl_name)
            else:
                message("Could not find template to use for {0} in {1}",
                        file_name, output_dir)
                raise GyokoException()
        except TemplateNotFound:
            message('Could not find template for {0} (tried "{1}")',
                    str(output_dir), generic_tmpl_name)
            raise GyokoException()

    html = template.render(**context)

    output_path = output_dir / (file_name + ".html")
    with output_path.open('w') as f:
        f.write(html)
