
from jinja2 import FileSystemLoader


class TemplateLoader(FileSystemLoader):
    template_extn = '.jinja'

    def load(self, env, template_name, globals=None):
        if not template_name.endswith(self.template_extn):
            template_name = template_name + self.template_extn
        return super(TemplateLoader, self).load(
            env, template_name, globals)
