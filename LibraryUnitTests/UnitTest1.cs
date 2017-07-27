using System;
using LibraryCode;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LibraryUnitTests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMakeBookshelf()
        {
            var bookcase1 = new Bookcase<DeweyCallNumber>(4, 1000, 300, 400);
        }
    }
}
