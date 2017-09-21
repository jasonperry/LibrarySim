using System;
using LibraryCode;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LibraryUnitTests
{
    [TestClass]
    public class UnitTest1
    {
        string[] goodDeweyCallNums = { "001", "815.1", "313.627", "510 Boof",
                                       "551.82 Smit", "001.525 C Shar"};
        string[] badDeweyCallNums = { "foo", "001.343Faar", "HQ75.34 B222" };

        [TestMethod]
        public void TestMakeBookshelf()
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
}
