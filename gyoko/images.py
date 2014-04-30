from PIL import Image

DEFAULT_THUMBNAIL_WIDTH = 200


def make_thumbnail(src, dest, width=200):
    '''
    Creates a thumbnail of the given source image and puts it in
    dest.
    '''
    src_img = Image.open(str(src.resolve()))
    src_size = src_img.size
    thumb_size = (width, int(src_size[1] * (float(width)/src_size[0])))
    thumbnail = src_img.resize(thumb_size, Image.ANTIALIAS)
    thumbnail.save(str(dest))
