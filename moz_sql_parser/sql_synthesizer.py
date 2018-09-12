import re

from jinja2 import Environment, PackageLoader

filters = {
    'is_list': lambda x: isinstance(x, list),
    'is_dict': lambda x: isinstance(x, dict)
}

template_env = Environment(loader=PackageLoader('moz_sql_parser', 'templates'))
template_env.filters.update(filters)
template = template_env.get_template('macros.jinja2')

def cleanup_string(res):
    res = re.sub(
        r'[\n\t]', ' ',
        res
    )
    res = re.sub(
        r'[ ]+', ' ',
        res
    )
    res = re.sub(
        r'^ *', '',
        res
    )
    res = re.sub(
        r' *$', '',
        res
    )
    return res


def generate_sql(args_in):

    res = template.render(json=args_in)

    return cleanup_string(res)
