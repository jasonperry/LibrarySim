﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LibraryCode;

namespace LibraryCmd
{
    class Program
    {

        static void bookshelfTest1()
        {
            Console.WriteLine("Testing bookshelf creation");
            Bookshelf<DeweyCallNumber> bs1 = new Bookshelf<DeweyCallNumber>(5, 1000, 200, 325);
            Console.WriteLine("Created shelf with {0} shelves, {1} mm of linear space.",
                              bs1.numShelves, bs1.freeSpace);
        }
        static void Main(string[] args)
        {
            Decimal d1 = new Decimal(697.3);
            Decimal d2 = new Decimal(Convert.ToDouble("697.313"));
            Console.WriteLine("Hello C# world!");
            Console.WriteLine("Decimal comparison: {0}", d1 < d2);
            DeweyCallNumber dcn = new DeweyCallNumber("510.377");
            // this returns a list!
            Console.WriteLine(dcn.subject()[0]);
            try {
                DeweyCallNumber dnbad = new DeweyCallNumber("SP243.33A34");
            } catch (CallNumberException e) {
                Console.WriteLine("Exception caught: {0}", e);
            }
            DeweyCallNumber dcn2 = new DeweyCallNumber("600.3");
            Console.WriteLine(dcn2.subject()[0]);
            Console.WriteLine("Comparing dd numbers: {0}, {1}", dcn.cnStr, dcn2.cnStr);
            Console.WriteLine(dcn.CompareTo(dcn2));
            Console.WriteLine(dcn2.CompareTo(dcn));

            /* Console.WriteLine(string.Compare(" ", "a"));
            Console.WriteLine(string.Compare("a", "b"));
            Console.WriteLine(string.Compare("a", "a"));
            Console.WriteLine(string.Compare("a", " ")); */

            // test out string indexing
            string mystr = "hello there";
            Console.WriteLine(mystr[2]);

            bookshelfTest1();

            Console.Read(); // stop it from closing.
        }
    }
}
