import sys
import os
import unittest

# Add Delphi Debug directory once
debug_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "Win64", "Debug"))
if debug_dir not in sys.path:
    sys.path.insert(0, debug_dir)

print("DEBUG: debug_dir =", debug_dir)

# Discover and run tests in pyunittests/ folder
loader = unittest.TestLoader()
suite = loader.discover('pyunittests')  # looks for test_*.py files

runner = unittest.TextTestRunner(verbosity=2)
runner.run(suite)