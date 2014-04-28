
from jinja2 import FileSystemLoader


class TemplateLoader(FileSystemLoader):
    fname_extn = '.jinja'

    def load_template(self, template_name, template_dirs=None):
        if not template_name.endswith(self.fname_extn):
            template_name = template_name + self.fname_extn
        return super(TemplateLoader, self).load_template(template_name,
                                                         template_dirs)
