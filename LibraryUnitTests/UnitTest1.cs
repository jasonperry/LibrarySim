using System;
using LibraryCode;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LibraryUnitTests
{
    [TestClass]
    public class TestBookCase
    {
        string[] goodDeweyCallNums = { "001", "815.1", "313.627", "510 Boof",
                                       "551.82 Smit", "001.525 C Shar"};
        string[] badDeweyCallNums = { "foo", "001.343Faar", "HQ75.34 B222" };

        [TestMethod]
        public void TestMakeBookcase()
        {
            var bookcase1 = new Bookcase<DeweyCallNumber>(4, 1000, 300, 400);
        }

        [TestMethod]
        public void DeweyCompare()
        {
            // Compare a bunch of Dewey call numbers.


        }

        [TestMethod]
        public void DeweyInitBad()
        {
            // Compare a bunch of Dewey call numbers.


        }
    }

    [TestClass]
    public class TestCallNumbers
    {
        [TestMethod]
        public void TestAlphaCallNum() {
            var cn1 = new AlphaStringsCallNumber("Bio Smi");
            var cn2 = new AlphaStringsCallNumber("Bio Smi J");
            int comp = cn1.CompareTo(cn2);
            Assert.AreEqual(comp, -1, "AlphaStringsCallNumber comparison fail")
        }
    }
}
