using System;
using LibraryCode;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LibraryUnitTests
{
    [TestClass]
    public class TestBookCase
    {
        [TestMethod]
        public void TestMakeBookcase()
        {
            var bookcase1 = new Bookcase<DeweyCallNumber>(4, 1000, 300, 400);
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
