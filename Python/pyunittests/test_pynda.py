import unittest
import pandapy as nda

class PaNDATests(unittest.TestCase):
  def test_len(self):
    a = nda.array([1, 2, 3])
    self.assertEqual(len(a), 3)
    
  def test_shape(self):
    a = nda.array([[1, 2], [3, 4], [5, 6]])
    s = a.shape
    self.assertEqual(len(s), 2)
    self.assertEqual(s[0], 3)
    self.assertEqual(s[1], 2)

if __name__ == "__main__":
  unittest.main()