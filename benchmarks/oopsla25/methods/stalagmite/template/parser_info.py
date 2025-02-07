parser_functions = {
        "__SUBJECT__.c": ["f__SUBJECT__"]
}

parser_entry_point = ("__SUBJECT__.c", "f__SUBJECT__")
assert parser_entry_point[1] in parser_functions[parser_entry_point[0]]