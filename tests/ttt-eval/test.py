import importlib.util
import sys
import inspect

module_name = "TestModule"
spec = importlib.util.spec_from_file_location(module_name, sys.argv[1])
module = importlib.util.module_from_spec(spec)
sys.modules[module_name] = module
spec.loader.exec_module(module)
f = inspect.getmembers(module, inspect.isfunction)[0][1]
print(f(sys.argv[2]))
