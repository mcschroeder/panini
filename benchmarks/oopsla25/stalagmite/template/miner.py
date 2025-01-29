import config
import re

from mine import Miner

class ParserMiner(Miner):
    def get_name(self):
        return "__SUBJECT__"
    
    @staticmethod
    def generate_parse_proxy_functions_c():
        with open("parser.c", "r") as f:
            content = f.read()
            # Extract function signature
            re_function_header = r"(.+ (f__SUBJECT__\_.+)\(.*\)\s*\{)"
            pattern = re.compile(re_function_header, re.MULTILINE) # '{' is in next line
            matches = pattern.finditer(content)

        ppfsc = []
        ppfsc.append('#include "klee/klee.h"')
        ppfsc.append('')
        for m in matches:
            function_signature_with_brace = m.group(1)
            function_name = m.group(2)
            ppfsc.append('__attribute__((used))') # make sure compiler does not delete the function because it is not called
            ppfsc.append(function_signature_with_brace.replace(function_name, f"sym_{function_name}"))
            ppfsc.append(f'  (*pexp)++;') # Now we increment byte-cursor like we do for token cursor
            ppfsc.append(f'  return klee_int("_{function_name}");') ## was: return 1, but I this is more general and should work too.
            ppfsc.append('}')
            ppfsc.append('')

        for i in range(len(ppfsc)):
            ppfsc[i] += "\n"

        with open(config.proxy_parse_functions_c, "w") as f:
            f.writelines(ppfsc)
        
        print(f"serialized {config.proxy_parse_functions_c}")

    def resolve_arg(self, arg_type: str, defined_ints: list):
        if arg_type == "i8*":
            return "inp"
        else:
            assert False, f"unhandled argument type: {arg_type}"

    def get_harness_template(self):
        return '''
            #include "common.h"

            int kw_{fua}(int argc, char* argv[]) {{
                // generic byte cursor setup
                char* inp = __setup_input_byte_cursor(argc, argv);

                char** cursor = &inp;

                {fua}({args});

                // no oracle required: parser calls exit(1) on syntax error
                return 0;
            }}'''

def get_miner():
    return ParserMiner(is_token_cursor = False)

def main():
    miner = get_miner()
    miner.process_args()

if __name__ == "__main__":
    main()