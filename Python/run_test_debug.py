import sys
import os
import unittest
import importlib

# --- Get test spec from command-line ---
if len(sys.argv) < 2:
    print("Usage: python run_test_debug.py <TestClass.test_method>")
    sys.exit(1)
    
test_spec = sys.argv[1] # e.g., "TestMyDelphi.test_hello"

# --- Add Delphi Debug folder to sys.path ---
debug_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "Win64", "Debug"))
if debug_dir not in sys.path:
    sys.path.insert(0, debug_dir)

# --- Pause to attach Delphi debugger ---
pid = os.getpid()
input(f"Attach Delphi debugger to python.exe (PID: {pid}), then press Enter to continue...")

# --- Import your Delphi module ---
import pandapy as nda

# --- Dynamically find test class in tests/ folder ---
found = False
for file in os.listdir(os.path.join(os.path.dirname(__file__), "pyunittests")):
    if file.startswith("test") and file.endswith(".py"):
        module_name = file[:-3]  # strip .py
        print(module_name)
        module = importlib.import_module(f"pyunittests.{module_name}")
        if hasattr(module, test_spec.split('.')[0]):
            test_class = getattr(module, test_spec.split('.')[0])
            method_name = test_spec.split('.')[1]
            found = True
            break

if not found:
    print(f"Error: test class '{test_spec.split('.')[0]}' not found in pyunittests/")
    sys.exit(1)

# --- Build and run the suite ---
suite = unittest.TestSuite()
suite.addTest(test_class(method_name))
runner = unittest.TextTestRunner(verbosity=2)
runner.run(suite)
