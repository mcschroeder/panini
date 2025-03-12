parser_functions = {
        "__SUBJECT__.c": ["__SUBJECT_FUNC__"]
}

parser_entry_point = ("__SUBJECT__.c", "__SUBJECT_FUNC__")
assert parser_entry_point[1] in parser_functions[parser_entry_point[0]]